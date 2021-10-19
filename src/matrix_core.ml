open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)


(* [mindex dims indices] builds the term  MINDEXN(N1,N2,N3,i1,i2,i3) where [N] is the length of dims and indices
    and dims [N1;N2;N3] and indices = [i1;i2:i3]
*)
let mindex (dims : trms) (indices : trms) : trm = 
  if List.length dims <> List.length indices then fail None "mindex: the number of dimension should correspond to the number of indices";
  let n = List.length dims in
  let mindex = "MINDEX" ^ (string_of_int n) in
  trm_apps (trm_var mindex) (dims @ indices)  


(* [mindex_inv t] returns the list of dimensions and indices used as args in a function call to MINDEX *)
let mindex_inv (t : trm) : (trms * trms) option = 
  match t.desc with 
  | Trm_apps (f, dims_and_indices) -> 
    begin match f.desc with
    | Trm_var f_name when (Internal.pattern_matches f_name "mindex") -> 
      let n = List.length dims_and_indices in
      if (n mod 2 = 0) then 
        Some (Tools.split_list_at (n/2) dims_and_indices)
      else None
    | _ -> None
    end
  | _ -> None

(* [access t dims indices] builds the term  x[MINDEXN(N1,N2,N3,i1,i2,i3)] where [N] is the length of dims and indices
    and dims [N1;N2;N3] and indices = [i1;i2:i3] and x is [t]
*)
let access (t : trm) (dims : trms) (indices : trms) : trm = 
  let mindex_trm = mindex dims indices in
  trm_apps (trm_binop Binop_array_cell_addr) [t; mindex_trm]

(* [access_inv t] returns the array access base and the list of dimensions and indices used as args in an array access 
    with that base and index a function call to MIDNEX with args dimensions and indices
 *)
let access_inv (t : trm) : (trm * trms * trms) option=
  match t.desc with 
  | Trm_apps (f, [base;index]) ->
    begin match trm_prim_inv f with 
    | Some (Prim_binop Binop_array_cell_addr) ->
      begin match mindex_inv index with
      | Some (dm, ind) -> Some (base, dm, ind)
      | _ -> None      
      end
    | _ -> None
    end
  | _ -> None

(* [get base dims indices] takes the trm builded from access function and puts it into a get operation*)
let get (base : trm) (dims : trms) (indices : trms) : trm = 
  let access_trm = access base dims indices in
  trm_apps (trm_unop Unop_get) [access_trm]

(* [get_inv t] get the trm inside a get oepration on an access*)
let get_inv (t : trm) : (trm * trms * trms) option = 
  match t.desc with 
  | Trm_apps (_f,[base]) when is_get_operation t -> access_inv base
  | _ -> None

(*  *)
let set (base : trm) (dims : trms) (indices : trms) (arg : trm) : trm = 
  let write_trm = access base dims indices in
  trm_apps (trm_binop (Binop_set)) [write_trm; arg]

let set_inv (t : trm) : (trm * trms * trms * trm)  option = 
  match t.desc with 
  | Trm_apps (_f, [addr;v]) when is_set_operation t ->
    begin match access_inv addr with 
    | Some (base, dims, indices) -> Some (base, dims, indices, v)
    | None -> None
    end 
  | _ -> None



let alloc ?(init : trm option = None) (dims : trms) (size : trm) : trm = 
  let n = List.length dims in
  match init with 
  | Some _ -> 

    trm_apps (trm_var ("MMALLOC" ^  (string_of_int n))) (dims @ [size]) 
  | None -> 
    trm_apps (trm_var ("MCALLOC" ^  (string_of_int n))) (dims @ [size]) 

type zero_initialized = bool 

let alloc_inv (t : trm) : (trms * trm * zero_initialized)  option= 
  match t.desc with 
  | Trm_apps (f, args) -> 
    begin match f.desc with 
    | Trm_var f_name -> 
      let dims , size = Tools.unlast args in
      if (Internal.pattern_matches f_name "MCALLOC") then Some (dims, size, true)
        else if (Internal.pattern_matches f_name "MMALLOC") then Some (dims, size, true)
        else None
    | _ -> None
    end
  | _ -> None


let vardef_alloc ?(init : trm option = None) (x : string) (ty : typ) (dims : trms) (size : trm) : trm = 
  let alloc_trm = alloc ~init dims size in
  trm_let Var_mutable (x, typ_ptr Ptr_kind_mut ty) (trm_apps (trm_prim (Prim_new ty) ) [alloc_trm])


let vardef_alloc_inv (t : trm) : (string * typ * trms * trm * zero_initialized) option =
  match t.desc with 
  | Trm_let (_, (x, ty), _) -> 
    let init = get_init_val t in
    begin match alloc_inv  init with 
    | Some (dims, size, z_in) -> Some (x, (get_inner_ptr_type ty), dims, size, z_in)
    | _ -> None
    end
  | _ -> None

