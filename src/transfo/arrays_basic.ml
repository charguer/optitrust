open Ast
open Target

(* [to_variables new_vars tg]: expects the target [tg] to point at an array declaration.
    Then it transforms this declaration into a list of declarations.
    [new_vars] - denotes the list of variables that is going to replace the initial declaration
      the length of this list is equal to [size -1] where [size] is the size of the array.*)
let to_variables (new_vars : vars) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Arrays_core.to_variables new_vars i t p
  ) tg)


(* [tile ~block_type block_size tg]: expects the target [tg] to point at an array declaration.
   Then it takes that declaration and transforms it into a tiled array. All the accesses of the
   targeted array are handled as well.
   [block_type] - denotes the name of the array which is going to represent a tile.
   [block_size] - size of the block of tiles. *)
let tile ?(block_type : typvar = "") (block_size : var) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Arrays_core.tile block_type block_size i t p) tg)

(* [swap name x tg]: expects the target [tg] to point at an array declaration.
   It changes the declaration so that the bounds of the array are switched. Also
   all the accesses of the targeted array are handled as well.*)
let swap (tg : target) : unit =
  apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Arrays_core.swap i t p) tg


(* [aos_to_soa tv sz] finds the definition of type [tv] which should be a typedef Record.
    Then it will change its struct fields type to arrys of size [sz] with type their current type.
    All the accesses will be swapped.
    Ex:
      int const N = 100;
      typedef struct {
        int x;
        int y;
      } vect;
      vect w[N];
      int main(){
        int i;
        int c = w[i].x;
        return 0;
      }

      int const N = 100;
      typedef struct {
        int x[N];
        int y[N];
      }
      vect w
      int main(){
        int i;
        int c = w.x[i];
        return 0;
      }
*)
let aos_to_soa (tv : typvar) (sz : var) : unit =
  apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,_) ->  Arrays_core.aos_to_soa tv sz t p) [cFunDef "main"]


(* [set_explicit tg] expects the target [tg] to point at an array declaration
    then it will remove the initialization trm and a list of write operations on
    each of the cells of the targeted array.
*)
let set_explicit (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Arrays_core.set_explicit) tg)

let elim_accesses_on (decl_index : int) (t : trm) : trm =
  let array_var = ref "" in
  let array_vals = ref [] in
  Nobrace.enter ();

  let remove_decl (t : trm) : trm =
    let error = "Arrays.elim_accesses_on: expected constant array literal declaration" in
    let (_, name, typ, init) = trm_inv ~error trm_let_inv t in
    (* Printf.printf "QSJIDO:\n%s\n" (Ast_to_text.ast_to_string t); *)
    let (_elem_ty, _size) = typ_inv ~error t.loc typ_const_array_inv typ in
    let array_mlist = trm_inv ~error array_inv init in
    array_var := name;
    array_vals := Mlist.to_list array_mlist;
    trm_seq_no_brace []
  in

  let rec update_accesses (t : trm) : trm =
    match Option.bind (trm_get_inv t) array_access_inv with
    | Some (base, index) ->
      begin match trm_var_inv base with
      | Some (_, x) when x = !array_var ->
        let error = "Arrays.elim_accesses_on: array accesses must be literals" in
        (* TODO: check that moving trm evaluation here is ok *)
        begin match trm_inv ~error trm_lit_inv index with
        | Lit_int i -> List.nth !array_vals i
        | _ -> fail index.loc error
        end
      | _ -> trm_map update_accesses t
      end
    | _ -> trm_map update_accesses t
  in

  let instrs = trm_inv
   ~error:"Arrays.elim_accesses_on: expected sequence"
   trm_seq_inv t in
  let new_instrs = Mlist.update_at_index_and_fix_beyond decl_index remove_decl update_accesses instrs in

  let nobrace_id = Nobrace.exit () in
  Internal.clean_no_brace_seq nobrace_id (
    trm_seq ~annot:t.annot ?loc:t.loc new_instrs)

(* [elim_accesses] expects the target [tg] to point at an array literal declaration, and resolves all its accesses, that must be constant, to eliminate it.
  *)
let elim_accesses (tg : target) : unit =
  Target.apply (fun t p ->
    let (i, p_seq) = Path.index_in_seq p in
    Path.apply_on_path (elim_accesses_on i) t p_seq
  ) tg
