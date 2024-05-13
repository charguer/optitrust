open Ast
open Trm

let max_nb_dims = 4

let toplevel_var_with_dim name_pattern =
  let vars = Array.init (max_nb_dims + 1) (fun n -> toplevel_var (sprintf name_pattern n)) in
  fun n ->
    try vars.(n)
    with Invalid_argument _ -> failwith (sprintf (name_pattern ^^ " is not defined (too many dimensions)") n)

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

(* [mindex dims indices]: builds a call to the macro MINDEX(dims, indices)
    [dims] - dimensions of the matrix access,
    [indices ] - indices of the matrix access.

     Example:
     MINDEXN(N1,N2,N3,i1,i2,i3) = i1 * N2 * N3 + i2 * N3 + i3
     Here, dims = [N1, N2, N3] and indices = [i1, i2, i3]. *)
let mindex (dims : trms) (indices : trms) : trm =
  if List.length dims <> List.length indices then failwith "Matrix_core.mindex: the number of
      dimension should correspond to the number of indices";
  let n = List.length dims in
  trm_apps (trm_var (mindex_var n)) (dims @ indices)

(* [mindex_inv t]: returns the list of dimensions and indices from the call to MINDEX [t] *)
let mindex_inv (t : trm) : (trms * trms) option =
  match t.desc with
  | Trm_apps (f, dims_and_indices, _) ->
    let n = List.length dims_and_indices in
    if (n mod 2 = 0 && n/2 <= max_nb_dims) then
      begin match f.desc with
      | Trm_var fv when var_eq (mindex_var (n/2)) fv ->
          Some (Xlist.split_at (n/2) dims_and_indices)
      | _ -> None
      end
    else None
  | _ -> None

(* [access t dims indices]: builds the a matrix access with the index defined by macro [MINDEX], see [mindex] function.
    Ex: x[MINDEX(N1,N2,N3, i1, i2, i3)]. *)
let access ?(annot : trm_annot = trm_annot_default) (t : trm) (dims : trms) (indices : trms) : trm =
  let mindex_trm = mindex dims indices in
  trm_apps ~annot (trm_binop Binop_array_access) [t; mindex_trm]

(* [access_inv t]: returns the array access base, the list of dimensions and indices used as args at matrix access [t]. *)
let access_inv (t : trm) : (trm * trms * trms) option=
  match t.desc with
  | Trm_apps (f, [base;index], _) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_array_access) ->
      begin match mindex_inv index with
      | Some (dm, ind) -> Some (base, dm, ind)
      | _ -> None
      end
    | _ -> None
    end
  | _ -> None

(* [get base dims indices]: takes the trm built from access function and puts it into a get operation. *)
let get (base : trm) (dims : trms) (indices : trms) : trm =
  let access_trm = access base dims indices in
  trm_apps (trm_unop Unop_get) [access_trm]

(* [get_inv t]: gets the trm inside a get oepration on an access. *)
let get_inv (t : trm) : (trm * trms * trms) option =
  match t.desc with
  | Trm_apps (_f,[base], _) when is_get_operation t -> access_inv base
  | _ -> None

(* [set base dims indices arg]: creates a set operation on which the address where the write is done
    is an access trm built with function accesses and [arg] is the value which is written to that
    that address. *)
let set (base : trm) (dims : trms) (indices : trms) (arg : trm) : trm =
  let write_trm = access base dims indices in
  trm_apps (trm_binop (Binop_set)) [write_trm; arg]

(* [set_inv t]: returns the arguments used in the function [set]. *)
let set_inv (t : trm) : (trm * trms * trms * trm)  option =
  match t.desc with
  | Trm_apps (_f, [addr;v], _) when is_set_operation t ->
    begin match access_inv addr with
    | Some (base, dims, indices) -> Some (base, dims, indices, v)
    | None -> None
    end
  | _ -> None

let malloc_vars = Array.init 5 (fun n -> toplevel_var (sprintf "MALLOC%d" n))
let calloc_vars = Array.init 5 (fun n -> toplevel_var (sprintf "CALLOC%d" n))

let malloc_var = toplevel_var_with_dim "MALLOC%d"
let calloc_var = toplevel_var_with_dim "CALLOC%d"

(* |alloc ~init dims size]: creates a call to function the MALLOC$(N) and CALLOC$(N) where [N] is the
     number of dimensions and [size] is the size in bytes occupied by a single matrix element in
     the memeory. *)
let alloc ?(init : trm option) (dims : trms) (size : trm) : trm =
  let n = List.length dims in
  match init with
  | Some _ -> trm_apps (trm_var (calloc_var n)) (dims @ [size])
  | None -> trm_apps (trm_var (malloc_var n)) (dims @ [size])

(* [zero_initialized]: a boolean type used as flag to tell if the array cells should be initialized to zero or not. *)
type zero_initialized = bool

(* [alloc_inv t]:  returns all the args used in function alloc [t]. *)
let alloc_inv (t : trm) : (trms * trm * zero_initialized)  option=
  match t.desc with
  | Trm_apps (f, args,_) ->
    begin match f.desc with
    | Trm_var f_var ->
      let dims , size = Xlist.unlast args in
      if (Tools.pattern_matches "CALLOC" f_var.name) then Some (dims, size, true)
        else if (Tools.pattern_matches "MALLOC" f_var.name) then Some (dims, size, false)
        else None
    | _ -> None
    end
  | _ -> None

let mfree_var = toplevel_var_with_dim "MFREE%d"

let free (dims : trms) (t : trm) : trm =
  let n = List.length dims in
  trm_apps (trm_var (mfree_var n)) (dims @ [t])

let free_inv (t : trm) : trm option =
  Option.bind (trm_apps_inv t) (fun (f, args) ->
  Option.bind (trm_var_inv f) (fun f_var ->
    if Tools.pattern_matches "MFREE" f_var.name
    then begin
      let _dims, t = Xlist.unlast args in
      Some t
    end else None
  ))

let mindex_contiguous_vars = Tools.String_map.of_seq ([""; "_uninit"; "_ro"] |> List.to_seq |> Seq.map (fun suffix ->
  suffix, Array.init 5 (fun n -> toplevel_var (sprintf "mindex%d_contiguous%s" n suffix))
))

let mindex_contiguous_rev_vars = Tools.String_map.of_seq ([""; "_uninit"; "_ro"] |> List.to_seq |> Seq.map (fun suffix ->
  suffix, Array.init 5 (fun n -> toplevel_var (sprintf "mindex%d_contiguous%s_rev" n suffix))
))

let mindex_contiguous_ghost_call n suffix args =
  ghost_call (Array.get (Tools.String_map.find suffix mindex_contiguous_vars) n) args

let mindex_contiguous_rev_ghost_call n suffix args =
  ghost_call (Array.get (Tools.String_map.find suffix mindex_contiguous_rev_vars) n) args
