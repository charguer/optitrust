open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)
(*************************************************************************************)
(*                        Core functions for manipulating c matrices                 *)
(*************************************************************************************)

(* [mindex dims indices] builds the term  MINDEXN(N1,N2,N3,i1,i2,i3) where [N] is the 
    length of dims and indices and dims [N1;N2;N3] and indices = [i1;i2:i3] are the 
    dimensions and indices used in the matrix access
*)
let mindex (dims : trms) (indices : trms) : trm = 
  if List.length dims <> List.length indices then fail None "mindex: the number of 
      dimension should correspond to the number of indices";
  let n = List.length dims in
  let mindex = "MINDEX" ^ (string_of_int n) in
  trm_apps (trm_var mindex) (dims @ indices)  


(* [mindex_inv t] returns the list of dimensions and indices used as args in a function 
    call to MINDEX 
*)
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

(* [access t dims indices] builds the term  x[MINDEXN(N1,N2,N3,i1,i2,i3)] where [N] is 
      the length of dims and indices and dims [N1;N2;N3] and indices = [i1;i2:i3] have 
    the same meaning as in the defnition of [mindex]  function, and x is [t]
*)
let access (t : trm) (dims : trms) (indices : trms) : trm = 
  let mindex_trm = mindex dims indices in
  trm_apps (trm_binop Binop_array_cell_addr) [t; mindex_trm]

(* [access_inv t] returns the array access base, the list of dimensions and indices used 
      as args in an array access  with that base and index a function call to MIDNEX with
       args dimensions and indices
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

(* [get base dims indices] takes the trm builded from access function and puts it into a get
     operation
*)
let get (base : trm) (dims : trms) (indices : trms) : trm = 
  let access_trm = access base dims indices in
  trm_apps (trm_unop Unop_get) [access_trm]

(* [get_inv t] get the trm inside a get oepration on an access*)
let get_inv (t : trm) : (trm * trms * trms) option = 
  match t.desc with 
  | Trm_apps (_f,[base]) when is_get_operation t -> access_inv base
  | _ -> None

(* [set base dims indices arg] creates a set operation where the address where the write is done 
    is the access trm builded with function access and [arg] is the value which is written in 
    that address
*)
let set (base : trm) (dims : trms) (indices : trms) (arg : trm) : trm = 
  let write_trm = access base dims indices in
  trm_apps (trm_binop (Binop_set)) [write_trm; arg]

(* [set_inv t] returns the arguments used in the function set*)
let set_inv (t : trm) : (trm * trms * trms * trm)  option = 
  match t.desc with 
  | Trm_apps (_f, [addr;v]) when is_set_operation t ->
    begin match access_inv addr with 
    | Some (base, dims, indices) -> Some (base, dims, indices, v)
    | None -> None
    end 
  | _ -> None


(* |alloc ~init dims size] create a call to function MMALLOC$(N) and MCALLOC$(N) where [N] is the
     number of dimensions and [size] is the size in bytes occupied by a single matrix element in 
     the memeory
*)
let alloc ?(init : trm option = None) (dims : trms) (size : trm) : trm = 
  let n = List.length dims in
  match init with 
  | Some _ -> 

    trm_apps (trm_var ("MMALLOC" ^  (string_of_int n))) (dims @ [size]) 
  | None -> 
    trm_apps (trm_var ("MCALLOC" ^  (string_of_int n))) (dims @ [size]) 

(* a boolean type used as flag to tell if the array cells should be initialized to zero or not *)
type zero_initialized = bool 


(* [alloc_inv t]  returns all the args used in function alloc *)
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


(* [vardef_alloc ~init x ty dims size] returns a term of the form T* x = ( T* ) A where A is the trm 
    created from alloc function
*)

let vardef_alloc ?(init : trm option = None) (x : string) (ty : typ) (dims : trms) (size : trm) : trm = 
  let alloc_trm = alloc ~init dims size in
  trm_let Var_mutable (x, typ_ptr Ptr_kind_mut ty) (trm_apps (trm_prim (Prim_new ty) ) [alloc_trm])

(* [vardef_alloc_inv t ] returns all the args used in vardef_alloc*)
let vardef_alloc_inv (t : trm) : (string * typ * trms * trm * zero_initialized) option =
  match t.desc with 
  | Trm_let (_, (x, ty), _) -> 
    let init = get_init_val t in
    begin match alloc_inv  init with 
    | Some (dims, size, z_in) -> Some (x, (get_inner_ptr_type ty), dims, size, z_in)
    | _ -> None
    end
  | _ -> None



(****************************************************************************************************)
(*                        Core transformations on C matrices                                        *)
(****************************************************************************************************)


let intro_mcalloc_aux (t : trm) : trm = 
  match t.desc with 
  | Trm_apps ({desc = Trm_var "mcalloc";_},[dim; size]) -> 
    alloc [dim] size  
  | _ -> fail t.loc "intro_mcalloc_aux: expected a function call to mcalloc"


let intro_mcalloc : Target.Transfo.local = 
  Target.apply_on_path (intro_mcalloc_aux)



let intro_mmalloc_aux (t : trm) : trm = 
  match t.desc with 
  | Trm_apps ({desc = Trm_var "mmalloc";_},[dim; size]) -> 
    alloc ~init:(Some (trm_int 0)) [dim] size  
  | _ -> fail t.loc "intro_mmalloc: expected a function call to mmalloc"


let intro_mmalloc : Target.Transfo.local = 
  Target.apply_on_path (intro_mmalloc_aux)

let intro_mindex_aux (dims : trms) (t : trm) : trm = 
  match t.desc with 
  | Trm_apps (f, [base;index]) -> 
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_array_cell_addr) ->
      trm_apps ~annot:t.annot ~marks:t.marks f [base; mindex dims [index]]
    | _ -> fail t.loc "intro_mindex_aux: expected a primitive array access operation"
    end
  | _ -> fail t.loc "intro_mindex_aux: expected an array access trm"

let intro_mindex (dims : trms) : Target.Transfo.local = 
  Target.apply_on_path (intro_mindex_aux dims)



let reorder_dims_aux (order : int list) (t : trm) : trm = 
  match mindex_inv t, alloc_inv t with 
  | Some (dims, indices), None -> 
    if List.length dims <> List.length order then fail t.loc "reoder_dims_aux: the entered order does not correspond to the targeted call ";
    let reordered_dims = Tools.list_reorder order dims in
    let reordered_indices = Tools.list_reorder order indices in
    mindex (reordered_dims) (reordered_indices)
  | None, Some (dims, size, zero_init) -> 
    if List.length dims <> List.length order then fail t.loc "reorder_dims_aux: the entered order does not correspond ot the targeted call";
    let reordered_dims = Tools.list_reorder order dims in
    let init = if zero_init then Some (trm_int 0 ) else None in
    alloc ~init reordered_dims size 
  | _ -> fail t.loc "reorder_dims_aux: expected  a function call to MCALLOC or MINDEX"

let reorder_dims (order : int list) : Target.Transfo.local = 
  Target.apply_on_path (reorder_dims_aux order)

let new_redundant_dim_aux (new_dim : trm) (t : trm) : trm = 
  match alloc_inv t with 
  | Some (dims, size, zero_init) -> 
    let new_dims = new_dim :: dims in
    let init = if zero_init then Some (trm_int 0) else None in
    alloc ~init new_dims size
  | None -> fail t.loc "new_redundant_dim_aux: expected a function call to MCALLOC"

let new_redundant_dim (new_dim : trm) : Target.Transfo.local =
  Target.apply_on_path (new_redundant_dim_aux new_dim)


