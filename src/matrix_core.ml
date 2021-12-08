open Ast
open Target
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
    | Trm_var (_, f_name) when (Tools.pattern_matches "MINDEX" f_name) ->
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
    trm_apps (trm_var ("MCALLOC" ^  (string_of_int n))) (dims @ [size])
  | None ->
    trm_apps (trm_var ("MMALLOC" ^  (string_of_int n))) (dims @ [size])

(* a boolean type used as flag to tell if the array cells should be initialized to zero or not *)
type zero_initialized = bool


(* [alloc_inv t]  returns all the args used in function alloc *)
let alloc_inv (t : trm) : (trms * trm * zero_initialized)  option=
  match t.desc with
  | Trm_apps (f, args) ->
    begin match f.desc with
    | Trm_var (_, f_name) ->
      let dims , size = Tools.unlast args in
      if (Tools.pattern_matches "MCALLOC" f_name) then Some (dims, size, true)
        else if (Tools.pattern_matches "MMALLOC" f_name) then Some (dims, size, false)
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
  | Trm_let (_, (x, ty), init) ->
    begin match get_init_val init with
    | Some init1 ->
      begin match alloc_inv  init1 with
      | Some (dims, size, z_in) -> Some (x, (get_inner_ptr_type ty), dims, size, z_in)
      | _ -> None
      end
    | _ -> None
    end

  | _ -> None



(****************************************************************************************************)
(*                        Core transformations on C matrices                                        *)
(****************************************************************************************************)

(* [intro_mcalloc_aux t]: replace a call to calloc with a call to MCALLOC
      params:
       [t]: ast of the call to alloc
      return:
        the updated ast of the function call
*)
let intro_mcalloc_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_var (_, "calloc");_},[dim; size]) ->
    alloc ~init:(Some (trm_int 0)) [dim] size
  | _ -> fail t.loc "intro_mcalloc_aux: expected a function call to mcalloc"


let intro_mcalloc : Target.Transfo.local =
  Target.apply_on_path (intro_mcalloc_aux)


(* [intro_mmalloc_aux t]: replace a call to calloc with a call to MMALLOC
      params:
       [t]: ast of the call to alloc
      return:
        the updated ast of the function call
*)
let intro_mmalloc_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_var (_, "malloc");_},[{desc = Trm_apps (_,[dim ;size]);_}]) ->
    alloc ~init:None [dim] size
  | _ -> fail t.loc "intro_mmalloc: expected a function call to mmalloc"


let intro_mmalloc : Target.Transfo.local =
  Target.apply_on_path (intro_mmalloc_aux)


(* [intro_mindex_aux dim t] replace an array access at index [i] with an array access at MINDEX([dim],i)
     params:
      [dim]: the size of the array accesses with [t]
      [t]: the ast of the array access
     return:
      the updated ast of the array access
*)
let intro_mindex_aux (dim : trm) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [base;index]) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_array_cell_addr) ->
      trm_apps ~annot:t.annot ~marks:t.marks f [base; mindex [dim] [index]]
    | _ -> fail t.loc "intro_mindex_aux: expected a primitive array access operation"
    end
  | _ -> fail t.loc "intro_mindex_aux: expected an array access trm got %s"

let intro_mindex (dim : trm) : Target.Transfo.local =
  Target.apply_on_path (intro_mindex_aux dim)


(* [reorder_dims_aux order t]: reorder the dimensions in a call to MCALLOC, MMALLOC or MINDEX
      params:
        [order]: a list of indices based on which the elements in dims should be ordered
        [t]: ast of the call to MCALLOC, MMALLOC, MINDEX
      return:
        the updated ast of the call with reordered args
*)
let reorder_dims_aux (rotate_n : int) (order : int list) (t : trm) : trm =
  match mindex_inv t, alloc_inv t with
  | Some (dims, indices), None ->
    let nb = List.length dims in
    let order = if rotate_n <> 0 
      then let id_perm = Tools.range 0 (nb - 1) in
           Tools.list_rotate rotate_n id_perm
      else 
        begin match order with 
      | [] -> fail t.loc "reorder_dims_aux: permuation order of indices and dims should be given or ~rotate_n argument should be used"
      | _ -> order
      end in
    begin try Tools.check_permutation nb order with | Tools.Invalid_permutation -> fail t.loc "order is not a permutation of indices" end;
    let reordered_dims = Tools.list_reorder order dims in
    let reordered_indices = Tools.list_reorder order indices in
    mindex (reordered_dims) (reordered_indices)
  | None, Some (dims, size, zero_init) ->
    let nb = List.length dims in
    let order = if rotate_n <> 0 
      then let id_perm = Tools.range 0 (nb - 1) in
           Tools.list_rotate rotate_n id_perm
      else 
        begin match order with 
      | [] -> fail t.loc "reorder_dims_aux: permuation order of indices and dims should be given or ~rotate_n argument should be used"
      | _ -> order
      end in
    begin try Tools.check_permutation nb order with | Tools.Invalid_permutation -> fail t.loc "order is not a permutation of indices" end;
    let reordered_dims = Tools.list_reorder order dims in
    let init = if zero_init then Some (trm_int 0 ) else None in
    alloc ~init reordered_dims size
  | _ -> fail t.loc "reorder_dims_aux: expected  a function call to MCALLOC or MINDEX"

let reorder_dims (rotate_n : int ) (order : int list) : Target.Transfo.local =
  Target.apply_on_path (reorder_dims_aux rotate_n order)


(* [insert_alloc_dim_aux new_dim t]: add a new dimension at the beginning of the list of dimension
      params:
        [new_dim]: the new dimension which is goin to be inserted into the list of dims in call to MCALLOC or MMALLOC
        [t]: ast of the call to ALLOC functions
      return:
        the updated ast of the call to ALLOC functions with the new arg [new_dim]
*)
let insert_alloc_dim_aux (new_dim : trm) (t : trm) : trm =
  match alloc_inv t with
  | Some (dims, size, zero_init) ->
    let new_dims = new_dim :: dims in
    let init = if zero_init then Some (trm_int 0) else None in
    alloc ~init new_dims size
  | None -> fail t.loc "insert_alloc_dim_aux: expected a function call to MCALLOC"

let insert_alloc_dim (new_dim : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_alloc_dim_aux new_dim)

(* [insert_access_dim_index_aux new_dim new_index t]: add a new dimension at the beginning of the list of dimension
        and add a new index at the begining of the list of indices in the call to MINDEX inside the
        targeted array access
      params:
        [new_dim]: the new dimension which is goin to be inserted into the list of dims in call to MINDEX
        [new_index]: the new index which is goin to be inserted into the list of indices in call to MINDEX
        [t]: ast of the array_access with the updated list of args in the call to MINDEX
      return:
        the updated ast of the
*)
let insert_access_dim_index_aux (new_dim : trm) (new_index : trm) (t : trm) : trm =
  match access_inv t with
  | Some (base, dims, indices) ->
    let new_dims = new_dim :: dims in
    let new_indices = new_index :: indices in
    access base new_dims new_indices
  | None -> fail t.loc "insert_access_dim_index_aux: expected an array access "

let insert_access_dim_index (new_dim : trm) (new_index : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_access_dim_index_aux new_dim new_index)


(* [local_name_aux mark var local_var malloc_trms var_type t] insert a local matrix declaration with name [local_var] and copy the content
      from the matrix [var] to [local_var] and replace all the accesses of that matrix inside the targeted insturction [t]
    params:
      [mark]: an optional mark at the final generated sequence
      [var]: the name of the current matrix used in instruction [t]
      [new_var]: the name of the local matrix which replaces all the current accesses of [var]
      [t]: ast of thee instuction which contains accesses to [var]
    return:
      ast of a hidden sequence which contains the updated instruction [t] and the other instructions
        used for copying the value from the current matrix to the local one
*)
let local_name_aux (mark : mark option) (var : var) (local_var : var) (malloc_trms : trms * trm * bool) (var_type : typ) (indices : (var list) ) (t : trm) : trm =
  let dims, size, zero_init = malloc_trms in
  let local_var_type = var_type in
  let init = if zero_init then Some (trm_int 0) else None in
  let fst_instr = trm_let_mut (local_var,local_var_type) (trm_cast (local_var_type) (alloc ~init dims size )) in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> "i" ^ (string_of_int (i + 1))) dims | _ as l -> l  end in
  let indices = List.map (fun ind -> trm_var ind) indices_list in
  let nested_loop_range = List.map2 (fun dim ind-> (ind, (trm_int 0), DirUp,  dim, Post_inc)) dims indices_list in
  let write_on_local_var = trm_set (trm_apps (trm_binop Binop_array_cell_addr) [trm_var local_var; mindex dims indices]) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var var; mindex dims indices]) in
  let write_on_var = trm_set (trm_apps (trm_binop Binop_array_cell_addr) [trm_var var; mindex dims indices]) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var local_var; mindex dims indices]) in
  let snd_instr = trm_fors nested_loop_range write_on_local_var in
  let new_t = Internal.subst_var var (trm_var local_var) t in 
  let thrd_instr = trm_fors nested_loop_range write_on_var in
  let last_instr = trm_apps (trm_var "MFREE") [trm_var local_var] in
  let final_trm = trm_seq_no_brace [fst_instr; snd_instr; new_t; thrd_instr; last_instr] in
  match mark with Some m -> trm_add_mark m final_trm | _ ->  final_trm

let local_name (mark : mark option) (var : var) (local_var : var) (malloc_trms :trms * trm * bool) (var_type : typ) (indices : var list ) : Target.Transfo.local =
  Target.apply_on_path (local_name_aux mark var local_var malloc_trms var_type indices)

let delocalize_aux (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (index : string) (ops : delocalize_ops) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    if Mlist.length tl <> 5 then fail t.loc "delocalize_aux: the targeted sequence does not have the correct shape";
    let decl = Mlist.nth tl 0 in
    begin match decl.desc with
    | Trm_let (_, (local_var, tx), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps (_, [alloc_trm]) ->
          begin match alloc_inv alloc_trm with
          | Some (dims, _, _) ->
            let alloc_arity = List.length dims in
            let new_alloc_trm = insert_alloc_dim_aux dim alloc_trm in
            let new_decl = trm_let_mut (local_var, (get_inner_ptr_type tx)) (trm_cast (get_inner_ptr_type tx) new_alloc_trm) in
            let tg = [nbMulti; cCellAccess ~base:[cVar local_var] ~index:[] ()] in
            let snd_instr = Mlist.nth tl 1 in
            begin match trm_fors_inv alloc_arity snd_instr with
            | Some (loop_range, body) ->
              let set_instr =
              begin match body.desc with
              | Trm_seq tl when Mlist.length tl = 1->
                Mlist.nth tl 0
              | _ -> body
              end in
              begin match set_inv set_instr with
              | Some (base, dims, indices, old_var_access) ->
                
                let new_dims = dim :: dims in
                let new_indices = (trm_var index) :: indices in
                let new_loop_range = loop_range @ [(index, trm_int 0, DirUp, dim, Post_inc)] in
                let init_val = match ops with 
                  | Delocalize_arith (li, _) -> trm_lit li
                  | Delocalize_obj (clean_f, _) -> trm_apps (trm_var clean_f) [] in

                let new_body = if init_zero 
                  then trm_seq_nomarks [set base new_dims new_indices init_val]
                  else trm_seq_nomarks [
                    set base new_dims((trm_int 0) :: indices) old_var_access;
                    trm_for index (trm_int 1) DirUp  dim (Post_inc) (set base new_dims new_indices init_val;)
                  ]
                  in
                
                let new_snd_instr = if init_zero 
                  then trm_fors new_loop_range new_body 
                  else trm_fors loop_range new_body in
                
                let thrd_instr = Mlist.nth tl 2 in
                let ps2 = resolve_target tg thrd_instr in
                let new_thrd_instr =
                  List.fold_left (fun acc p ->
                    apply_on_path (insert_access_dim_index_aux dim (trm_apps (trm_var "ANY") [dim])) acc p
                  ) thrd_instr ps2 in

                let new_access = access base new_dims new_indices in
                
                let acc, acc_provided = match acc with
                | Some s -> s, true
                | None -> "s", false in
                
                let op_fun (l_arg : trm) (r_arg : trm) : trm  = match ops with 
                  | Delocalize_arith (_, op) -> trm_set ~annot:[App_and_set] l_arg (trm_apps (trm_binop op) [l_arg; r_arg])
                  | Delocalize_obj (_, transfer_f) -> trm_apps (trm_var transfer_f) [l_arg; r_arg] 
                in

                let new_body = if acc_in_place then
                  if acc_provided then fail t.loc "delocalize_aux: if acc_in_place is set to true there is not need to provide an accumulator"
                  else 
                  begin
                    trm_seq_nomarks [
                      set (trm_var "a") dims indices (access base new_dims ((trm_int 0) :: indices));
                      trm_for index (trm_int 1) DirUp dim (Post_inc) (
                        op_fun old_var_access new_access
                      )
                    ]
                    end
                  else 
                    if not acc_provided then fail t.loc "delocalize_aux: accumulator should be provided otherwise you need to set the flag ~acc_in_place to false" else
                    (trm_seq_nomarks [
                        trm_let_mut (acc, typ_int ()) (trm_int 0);
                        trm_for index (trm_int 0) DirUp dim (Post_inc) (trm_seq_nomarks [
                            op_fun (trm_var acc) new_access]);
                        trm_set old_var_access (trm_var acc)]) in
                let new_frth_instr =
                  trm_fors loop_range new_body in
                    
                let fifth_instr = Mlist.nth tl 4 in
                  trm_seq ~annot:t.annot ~marks:t.marks (Mlist.of_list [new_decl; new_snd_instr; new_thrd_instr; new_frth_instr; fifth_instr])

              |  _ -> fail t.loc "delocalize_aux: expected a write operation on array cell"
              end

            | _ -> fail t.loc "delocalize_aux: expected a write operation on array cell"
            end

          | _ -> fail t.loc "delocalize_aux: couldn't find the call to the alloc function"
          end
        | _ -> fail t.loc "delocalize_aux: alloc functions are called inside a cast operation"
        end
      | _ ->  fail t.loc "delocalize_aux: expected an initialized variable"
      end
    | _ -> fail t.loc "delocalize_aux: expected the declaration of the local variable"
    end

  |  _ -> fail t.loc "delocalize_aux: expected sequence which contains the mandatory instructions for applying the delocalize transformation"

let delocalize (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (index : string) (ops : delocalize_ops): Target.Transfo.local =
  Target.apply_on_path (delocalize_aux dim init_zero acc_in_place acc index ops)

