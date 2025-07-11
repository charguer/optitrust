open Ast
open Trm
open Typ

let max_nb_dims = 4

let toplevel_var_with_dim name_pattern =
  let vars = Array.init (max_nb_dims + 1) (fun n -> toplevel_var (sprintf name_pattern n)) in
  fun n ->
    try vars.(n)
    with Invalid_argument _ -> failwith "%s" (sprintf (name_pattern ^^ " is not defined (too many dimensions)") n)

let toplevel_var_with_dim_inv getter v =
  let exception Found of int in
  try
    for i = 0 to max_nb_dims do
      if var_eq v (getter i) then raise (Found i)
    done;
    None
  with Found k -> Some k

let mindex_var = toplevel_var_with_dim "MINDEX%d"
let mindex_var_inv = toplevel_var_with_dim_inv mindex_var

(** [mindex dims indices]: builds a call to the macro MINDEX(dims, indices)
    [dims] - dimensions of the matrix access,
    [indices ] - indices of the matrix access.

     Example:
     MINDEXN(N1,N2,N3,i1,i2,i3) = i1 * N2 * N3 + i2 * N3 + i3
     Here, dims = [N1, N2, N3] and indices = [i1, i2, i3]. *)
let mindex (dims : trms) (indices : trms) : trm =
  if List.length dims <> List.length indices then
    failwith "Matrix_core.mindex: the number of dimensions (%d) should correspond to the number of indices (%d)" (List.length dims) (List.length indices);
  let n = List.length dims in
  trm_apps (trm_var (mindex_var n)) (dims @ indices)

(** [mindex_inv t]: returns the list of dimensions and indices from the call to MINDEX [t] *)
let mindex_inv (t : trm) : (trms * trms) option =
  match t.desc with
  | Trm_apps (f, dims_and_indices, _, _) ->
    let n = List.length dims_and_indices in
    if (n mod 2 = 0 && n/2 <= max_nb_dims) then
      begin match f.desc with
      | Trm_var fv when var_eq (mindex_var (n/2)) fv ->
          Some (List.split_at (n/2) dims_and_indices)
      | _ -> None
      end
    else None
  | _ -> None

let msize_var = toplevel_var_with_dim "MSIZE%d"
let msize_var_inv = toplevel_var_with_dim_inv msize_var

(** [msize dims]: Build a call to MSIZE(dims) *)
let msize (dims: trms): trm =
  let n = List.length dims in
  trm_apps (trm_var (msize_var n)) dims

let msize_inv (t: trm): trms option =
  match trm_apps_inv t with
  | Some (f, dims) ->
    let n = List.length dims in
    if n <= max_nb_dims then
      begin match trm_var_inv f with
      | Some f when var_eq f (msize_var n) -> Some dims
      | _ -> None
      end
    else None
  | _ -> None

let typ_matrix (basetyp: typ) (dims: trms) =
  Typ.typ_array basetyp ~size:(msize dims)

let typ_matrix_inv (ty: typ): (typ * trms) option =
  match Typ.typ_array_inv ty with
  | Some (base, Some size) ->
    begin match msize_inv size with
    | Some dims -> Some (base, dims)
    | None -> None
    end
  | _ -> None

(** [access t dims indices]: builds the a matrix access with the index defined by macro [MINDEX], see [mindex] function.
    Ex: x[MINDEX(N1,N2,N3, i1, i2, i3)]. *)
let access ?(annot : trm_annot = trm_annot_default) ?(elem_typ: typ option) (t : trm) (dims : trms) (indices : trms) : trm =
  let mindex_trm = mindex dims indices in
  trm_array_access ~annot ?elem_typ t mindex_trm

(** [access_inv t]: returns the array access base, the list of dimensions and indices used as args at matrix access [t]. *)
let access_inv (t : trm) : (trm * trms * trms) option=
  match trm_array_access_inv t with
  | Some (base, index) ->
    begin match mindex_inv index with
    | Some (dm, ind) -> Some (base, dm, ind)
    | None -> None
    end
  | None -> None

(** [get base dims indices]: takes the trm built from access function and puts it into a get operation. *)
let get ?(typ: typ option) (base : trm) (dims : trms) (indices : trms) : trm =
  trm_get ?typ (access ?elem_typ:typ base dims indices)

(** [get_inv t]: gets the trm inside a get operation on an access. *)
let get_inv (t : trm) : (trm * trms * trms) option =
  match t.desc with
  | Trm_apps (_f,[base], _, _) when is_get_operation t -> access_inv base
  | _ -> None

(** [set base dims indices arg]: creates a set operation on which the address where the write is done
    is an access trm built with function accesses and [arg] is the value which is written to that
    that address. *)
let set (base : trm) (dims : trms) (indices : trms) (arg : trm) : trm =
  let write_trm = access base dims indices in
  trm_set write_trm arg

(** [set_inv t]: returns the arguments used in the function [set]. *)
let set_inv (t : trm) : (trm * trms * trms * trm) option =
  match t.desc with
  | Trm_apps (_f, [addr;v], _, _) when is_set_operation t ->
    begin match access_inv addr with
    | Some (base, dims, indices) -> Some (base, dims, indices, v)
    | None -> None
    end
  | _ -> None

(** [alloc_uninit basetyp dims]: create a term that allocates an uninitialized matrix *)
let alloc_uninit ?(annot = trm_annot_default) (basetyp: typ) (dims : trms) : trm =
  trm_new_uninit ~annot (typ_matrix basetyp dims)

(** [alloc_zero basetyp dims]: create a term that allocates a zero-initialized matrix *)
let alloc_zero ?(annot = trm_annot_default) (basetyp: typ) (dims : trms) : trm =
  let ty = typ_matrix basetyp dims in
  trm_new ~annot ty (trm_null ty)

(** [alloc basetyp dims]: create a term that allocates a matrix *)
let alloc ?(annot = trm_annot_default) ?(zero_init = false) (basetyp: typ) (dims : trms) : trm =
  if zero_init then alloc_zero ~annot basetyp dims else alloc_uninit ~annot basetyp dims

(** [alloc_uninit_inv t]:  returns all the args used in function [alloc_uninit]. *)
let alloc_uninit_inv (t : trm) : (typ * trms) option =
  match trm_new_uninit_inv t with
  | Some ty ->
    begin match typ_matrix_inv ty with
    | Some (basetyp, dims) -> Some (basetyp, dims)
    | _ -> None
    end
  | _ -> None

(** [alloc_zero_inv t]:  returns all the args used in function [alloc_zero]. *)
let alloc_zero_inv (t : trm) : (typ * trms) option =
  match trm_new_inv t with
  | Some (ty, init) when is_trm_null init ->
    begin match typ_matrix_inv ty with
    | Some (basetyp, dims) -> Some (basetyp, dims)
    | _ -> None
    end
  | _ -> None

(** [alloc_inv t]:  returns all the args used in function [alloc]. *)
let alloc_inv (t : trm) : (typ * trms * bool) option =
  match alloc_uninit_inv t with
  | Some (ty, dims) -> Some (ty, dims, false)
  | None ->
    match alloc_zero_inv t with
    | Some (ty, dims) -> Some (ty, dims, true)
    | None -> None

(**[let_alloc_inv] : returns :
   - var corresponding to an array and its type
   - all arguments used in fonction [alloc] *)
let let_alloc_inv (t : trm) : (var * trm * trm * trms * bool) option =
  match trm_let_inv t with
  | Some (array_var, typ_array, alloc_trm) -> (
      match alloc_inv alloc_trm with
      | Some (typ_alloc, trms, init) ->
          Some (array_var, typ_array, typ_alloc, trms, init)
      | None -> None)
  | None -> None

    (* TODO : Combination var_def alloc_inv : let_alloc_inv *)
let free (t : trm) : trm = trm_delete t
let free_inv (t : trm) : trm option = trm_delete_inv t

(** [access_map ?indepth f x t] applies a transformation function [f] to all
    trms that represent an access to the array [x] within the term [t].
    - [f] : a function applied to each array access. It takes a triple (base,
      dims, indices) and returns a transformed triple (base', dims', indices').
    - [x] : the variable representing the array.
    - [t] : term to inspect and transform.
    - [indepth] (optional): if [true] (default), the function is applied
      recursively to all subterms of [t]. Returns a new term where all accesses
      to [x] have been replaced using [f]. *)
let rec access_map ?(indepth = true)
    (f : typ * trms * trms -> typ * trms * trms) (x : var) (t : trm) : trm =
  match access_inv t with
  | Some (base, dims, indices) -> (
      match trm_var_inv base with
      | Some y when var_eq y x ->
          let base', dims', indices' = f (base, dims, indices) in
          access base' dims' indices'
      | _ -> trm_map (access_map f x) t)
  | _ when indepth = true -> trm_map (access_map f x) t
  | _ -> t

let ghost_mindex_unfold matrix dims res_pattern =
  ghost_call (toplevel_var (sprintf "mindex%d_unfold" (List.length dims))) (("H", res_pattern) :: ("matrix", matrix) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims)

let ghost_mindex_fold matrix dims res_pattern =
  ghost_call (toplevel_var (sprintf "mindex%d_fold" (List.length dims))) (("H", res_pattern) :: ("matrix", matrix) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims)

let ghost_ro_mindex_unfold matrix dims res_pattern =
  ghost_call (toplevel_var (sprintf "ro_mindex%d_unfold" (List.length dims))) (("H", res_pattern) :: ("matrix", matrix) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims)

let ghost_ro_mindex_fold matrix dims res_pattern =
  ghost_call (toplevel_var (sprintf "ro_mindex%d_fold" (List.length dims))) (("H", res_pattern) :: ("matrix", matrix) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims)
