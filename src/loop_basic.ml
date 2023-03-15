open Ast
open Target

(* [swap tg]: expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let swap : Transfo.t =
  apply_on_targets (Loop_core.swap)

(* [color nb_colors i_color tg]: expects the target [tg] to point at a simple for  loop,
   let's say [for (int i = start; i < stop; i += step) { body } ].
   [nb_colors] - an expression denoting the number of colors (e.g., ["2"]),
   [index] - denotes a fresh name to use as index for iterating over colors.

   In case [step = 1]:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index; i < stop; i += nb_color) { body }].

   In the general case, it produces:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index*step; i < stop; i += step*nb_color) { body }]. *)
let color (nb_colors : trm) ?(index : var option) : Transfo.t =
  apply_on_targets (Loop_core.color nb_colors index)

(* [tile tile_size index tg]: expects the target [tg] to point at a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   divides - denotes a flag to know if tile_size divides the size of the array or not
   [tile_size] - denotes the width of the tile (e.g., ["2"])
   [index] - denotes a fresh name to use as index for iterating over tiles.
   [bound] - can be one of
      - TileBoundMin: generates a constraint of the form  [i < min(X, bx+B)]
      - TileBoundAnd: generates a constraint of the form [i <  X && i < bx+B]
      - TileDivides: generates a constraint of the form [i < X], which is only true if B divides X

   It produces:
   [for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }]. *)
let tile ?(index : var = "b${id}")
         ?(bound : tile_bound = TileBoundMin) 
         (tile_size : trm) : Transfo.t =
  apply_on_targets (Loop_core.tile index bound tile_size)

(* [hoist x_step tg]: expects [tg] to point at a variable declaration inside a
    simple loop. Let's say for {int i ...} {
        int x; [tg]
        ...
        x = ..
      }
    The targeted declaration should be detached, then the transformation it's going to introduce
    an array declaration right before the for loop that contains the targeted declaration.
    The declared array will have name [name], type the same as the one targeted by [tg] and the size
    of the array it's going to be equal to the [loop_bound -1]. All the variable occurrences are
    going to be replaced with array accesses at index the index of the for loop.

    [x_step] - denotes the array name that is going to hoist all the values of the targeted variable
    for each index of the for loop. *)
(* LATER/ deprecated *)
let hoist_old ?(name : var = "${var}_step") ?(array_size : trm option = None) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_transformed_targets (Path.index_in_surrounding_loop)
     (fun t (i, p) -> Loop_core.hoist_old name i array_size t p) tg)

(* TODO: clean up code *)
let hoist_on (name : string)
             (mark : mark option)
             (arith_f : trm -> trm)
             (decl_index : int) (t : trm) : trm =
  let error = "Loop_basic.hoist_on: only simple loops are supported" in
  let (range, body) = trm_inv ~error trm_for_inv t in
  let (index, start, dir, stop, step, _par) = range in
  assert (dir = DirUp); (* TODO: other directions *)
  let (array_size, new_index) = match step with
  | Pre_inc | Post_inc ->
     let typ = Some (typ_int ()) in
     (trm_sub ~typ stop start, trm_sub ~typ  (trm_var ~typ index) start)
  | Step s ->
    let typ = Some (typ_int ()) in
    (* i = start; i < stop; i += step *)
    let trm_ceil_div a b = 
      trm_div ~typ (trm_add ~typ a (trm_sub ~typ b (trm_int 1))) b
    in
     (trm_ceil_div (trm_sub ~typ stop start) s,
      trm_div ~typ (trm_sub ~typ (trm_var ~typ index) start) s)
  | _ -> fail t.loc "Loop_basic.hoist_on: unsupported loop step"
  in
  let body_instrs = trm_inv ~error trm_seq_inv body in
  let ty = ref (typ_auto()) in
  let old_name = ref "" in
  let new_name = ref "" in
  let new_dims = ref [] in
  let with_mindex (dims : trms) : trm =
    new_dims := (arith_f array_size) :: dims;
    let partial_indices = (arith_f new_index) ::
      (List.init (List.length dims) (fun _ -> trm_lit (Lit_int 0))) in
    mindex !new_dims partial_indices
  in
  let update_decl (decl : trm) : trm =
    let error = "Loop_basic.hoist_on: expected variable declaration" in
    let (vk, x, tx, init) = trm_inv ~error trm_let_inv decl in
    old_name := x;
    new_name := Tools.string_subst "${var}" x name;
    ty := get_inner_ptr_type tx;
    begin match Matrix_core.alloc_inv_with_ty init with
    | Some (dims, elem_size) ->
      let mindex = with_mindex dims in
      (* extra reference to remove *)
      ty := Option.get (typ_ptr_inv !ty);
      (* TODO: let_immut? *)
      trm_let_mut (x, (get_inner_ptr_type tx))
        (trm_array_access (trm_var_get !new_name) mindex)
    | None ->
      fail init.loc "expected MALLOCN initialization";
      (* DEPRECATED: before MALLOC0
      if not ((is_trm_uninitialized init) || (is_trm_new_uninitialized init))
      then fail init.loc "expected uninitialized allocation";
      let mindex = with_mindex [] in
      trm_let_ref (x, (get_inner_ptr_type tx))
        (trm_array_access (trm_var_get !new_name) mindex)
      *)
    end
  in
  let body_instrs_new_decl = Mlist.update_nth decl_index update_decl body_instrs in
  (*
  Printf.printf "body_instrs_new_decl:\n%s\n" (AstC_to_c.ast_to_string (trm_seq ~annot:body.annot body_instrs_new_decl));
  *)
  let new_body_instrs = (* if (List.length !new_dims > 1)
  then *) begin
    let free_index_opt = ref None in
    Mlist.iteri (fun i instr ->
      match trm_free_inv instr with
      | Some freed ->
        begin match trm_get_inv freed with
        | Some x ->
          begin match trm_var_inv x with
          | Some (_, freed_name) when freed_name = !old_name ->
            assert (Option.is_none !free_index_opt);
            free_index_opt := Some i;
          | _ -> ()
          end
        | None -> ()
        end
      | _ -> ()
    ) body_instrs_new_decl;
    match !free_index_opt with
    | Some free_index -> Mlist.remove free_index 1 body_instrs_new_decl
    | None -> fail body.loc "Loop_basic.hoist: expected free instruction"
  end (* DEPRECATED: before MALLOC0
    else body_instrs_new_decl *)
  in 
  let new_body = trm_seq ~annot:body.annot new_body_instrs in
  trm_seq_no_brace [
    trm_may_add_mark mark (
      (* TODO: let_immut? *)
      trm_let_mut (!new_name, (typ_ptr Ptr_kind_mut !ty))
        (Matrix_core.alloc_with_ty !new_dims !ty));
    trm_for ~annot:t.annot range new_body;
    trm_free (trm_var_get !new_name);
  ]

(* TODO: document *)
let hoist ?(name : var = "${var}_step")
          ?(mark : mark option = None)
          ?(arith_f : trm -> trm = Arith_core.(simplify_aux true gather_rec))
         (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply (fun t p_instr ->
      let (i, p) = Path.index_in_surrounding_loop p_instr in
      Path.apply_on_path (hoist_on name mark arith_f i) t p
      ) tg)

(* [fission_on]: split loop [t] into two loops

    [index]: index of the splitting point
    [t]: ast of the loop
    *)
let fission_on (index : int) (t : trm) : trm =
  (* TODO: trm_for_inv_instrs => (l_range, tl) *)
  match t.desc with
  | Trm_for (l_range, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let tl1, tl2 = Mlist.split index tl in
      let b1 = trm_seq tl1 in
      let b2 = trm_seq tl2 in
      trm_seq_no_brace [
        trm_for l_range b1; (* TODO: trm_for_instrs l_range tl1 *)
        trm_for l_range b2;]
    | _ -> fail t.loc "Loop_basic.fission_on: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "Loop_basic.fission_on: only simple loops are supported"

(* [fission tg]: expects the target [tg] to point somewhere inside the body of the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.

   @correctness: Reads in new second loop need to never depend on writes on
   first loop after index i. Writes in new second loop need to never overwrite
   writes in first loop after index i. *)
let fission (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply (fun t p_before ->
      let (p_seq, split_i) = Path.last_dir_before_inv_success p_before in
      let p_loop = Path.parent_with_dir p_seq Dir_body in
      apply_on_path (fission_on split_i) t p_loop)
    tg)

(* [fission_all_instrs_on]: split loop [t] into N loops,
   one per instruction in the loop body

   [t]: ast of the loop
    *)
let fission_all_instrs_on (t : trm) : trm =
  (* TODO: trm_for_inv_instrs => (l_range, tl) *)
  match t.desc with
  | Trm_for (l_range, body) ->
    begin match body.desc with
    | Trm_seq tl ->
      let body_lists = List.map (fun t1 -> trm_seq_nomarks [t1]) (Mlist.to_list tl) in
      trm_seq_no_brace (List.map (fun t1 -> trm_for l_range t1) body_lists)
    | _ -> fail t.loc "Loop_basic.fission_all_instrs_on: expected the sequence inside the loop body"
    end
  | _ -> fail t.loc "Loop_basic.fission_all_instrs_on: only simple loops are supported"
    
(* LATER: only keep fission or fission_all_instrs,
   implementing one with the other *)
(* [fission_all_instrs]: similar to [fission],
   but splits the targeted loop into N loops,
   one per instruction in the loop body.
   *)
let fission_all_instrs (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ -> 
    Target.apply_at_target_paths fission_all_instrs_on tg)
  
(* [fusion_on_block tg]: expects the target [tg] to point at a sequence containing two loops
    with the same range, start step and bound but different body.
    Then it's going to merge the bodies of all the loops that belong to the targeted sequence. *)
let fusion_on_block ?(keep_label : bool = false) : Transfo.t =
  apply_on_targets (Loop_core.fusion_on_block keep_label)

(* [grid_enumerate index_and_bounds tg]: expects the target [tg] to point at a loop iterating over
    a grid. The grid can be of any dimension.
    Loop  [tg] then is transformed into nested loops
    where the number of nested loops is equal to the number of dimensions.
      [index_and_bounds] - is a list of pairs, where each pair denotes the index and the bound
        of the loop iterating over a specific dimension.
    Ex: Assume A = X * Y * Z, and [index_and_bounds] = [("x","X");("y","y");("z","Z")] and the result is

      for (int a = 0; a < A; a++){        for (int x = 0; x < X; x++){
        .......                       =>    for (int y = 0; y < Y; y++){
      }                                       for (int z = 0; z < Z, z++){
                                                int a = ((x * Y) + y)*Z + z
                                                ...
                                              }
                                            }
                                          } *)
let grid_enumerate (index_and_bounds : (string * trm) list) : Transfo.t =
  apply_on_targets (Loop_core.grid_enumerate index_and_bounds)

(* [unroll ~braces ~my_mark tg]: expects the target to point at a simple loop of the shape
    for (int i = a; i < a + C; i++) or for (int i = 0; i < C; i++)
      then it will move the instructions out of the loop by replacing
      the index i occurrence with a + j in and j in the second case where
      j is an integer in range from 0 to C.

    Assumption: Both a and C should be declared as constant variables. *)
let unroll ?(braces : bool = false) ?(my_mark : mark  = "")  (tg : target): unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Loop_core.unroll braces my_mark) tg)

(* [move_out tg]: expects the target [tg] to point at an instruction inside the loop
    that is not dependent on the index of the loop or any local variable.
    Then it will move it outside the loop.

    NOTE:: currently, there is no check that the transformation is legitimate.
      
    LATER: Implement a combi transformation that will check if the targeted instruction
    is dependent on any local variable or the loop index. *)
let move_out ?(mark : mark option = None) (tg : target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  apply_on_transformed_targets (Path.index_in_surrounding_loop)
    (fun t (i, p) -> Loop_core.move_out mark i t p ) tg)
    
(* [unswitch tg]:  expects the target [tg] to point at an if statement with a constant condition
     (not dependent on loop index or local variables) inside a loop.  Then it will take that
      if statment outside the loop.

   @correctness: requires that the loop is parallelizable *)
let unswitch (tg : target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  apply_on_transformed_targets(Path.index_in_surrounding_loop)
    (fun t (i, p) -> Loop_core.unswitch i t p) tg)


(* [to_unit_steps index tg]: expects target [tg] to point at a for loop
    [index] - denotes the new index for the transformed loop
        by default is an empty string. The reason for that is to check if the user
        gave the name of the new index of not. If not then [index] = unit_index
        where index is the index of the targeted loop.

    Assumption:
      The targeted loop should be of the form:
        for (int i = a; i < b; i+=B){ s += i },
        and it assumes that B divides (b-a). It then
        transforms the targeted loop into the following form:
          for (int index = 0; index < ...; index++) {
            int i = (a + (j * B));
            s += i;
           } *)
let to_unit_steps ?(index : var = "" ) : Transfo.t =
  apply_on_targets (Loop_core.to_unit_steps index)

(* [fold ~direction index start stop step tg]: expects the target [tg] to point at the first instruction in a sequence
    and it assumes that the sequence containing the target [tg] is composed of a list of instructions which
    can be expressed into a single for loop with [index] [direction] [start] [nb_instructions] and [step] as loop
    components. *)
let fold ~index:(index : var) ~start:(start : int) ~step:(step : int) : Transfo.t =
  apply_on_targets (
    Loop_core.fold index start step
)

(* [split_range nb cut tg]: expects the target [tg] to point at a simple loop
    then based on the arguments nb or cut it will split the loop into two loops. *)
let split_range ?(nb : int = 0) ?(cut : trm = trm_unit()) (tg : target) : unit =
  Internal.nobrace_remove_after( fun _ ->
    apply_on_targets (Loop_core.split_range nb cut) tg )

type shift_kind =
| ToZero
| Add of trm

(* [shift_on index kind]: shifts a loop index to start from zero or by a given amount. *)
let shift_on (index : var) (kind : shift_kind) (t : trm): trm =
  let index' = index in
  let error = "Loop_basic.shift_on: expected a target to a simple for loop" in
  let ((index, start, direction, stop, step, is_parallel), body) = trm_inv ~error trm_for_inv t in
  let body_terms = trm_inv ~error trm_seq_inv body in
  let (start', shift) = match kind with
  | ToZero -> (trm_int 0, trm_minus start)
  | Add s -> (trm_add start s, s)
  in
  let stop' = trm_add stop shift in
  (* NOTE: Option.get assuming all types are available *)
  let body' = trm_seq (Mlist.push_front (
    trm_let_immut (index, (Option.get start.typ))
      (trm_sub (trm_var index') shift)) body_terms) in
  trm_for (index', start', direction, stop', step, is_parallel) body'

(* [shift index amount]: shifts a loop index by a given amount. *)
let shift ?(reparse : bool = false) (index : var) (amount : trm) (tg : target) : unit =
  (* FIXME: having to think about reparse here is not great *)
  reparse_after ~reparse (
    Target.apply_at_target_paths (shift_on index (Add amount))) tg
    
(* [shift_to_zero index]: shifts a loop index to start from zero. *)
let shift_to_zero ?(reparse : bool = false) (index : var) (tg : target) : unit =
  (* FIXME: having to think about reparse here is not great *)
  reparse_after ~reparse (
    Target.apply_at_target_paths (shift_on index ToZero)) tg

(* [rename_index new_index]: renames the loop index variable *)
let rename_index (new_index : var) (tg : target) : unit =
  apply_on_targets (Loop_core.rename_index new_index) tg