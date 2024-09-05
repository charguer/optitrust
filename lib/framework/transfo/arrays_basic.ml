open Prelude
open Target

(** [to_variables new_vars tg]: expects the target [tg] to point at an array declaration.
    Then it transforms this declaration into a list of declarations.
    [new_vars] - denotes the list of variables that is going to replace the initial declaration
      the length of this list is equal to [size -1] where [size] is the size of the array.*)
let%transfo to_variables (new_vars : string list) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths_in_seq (Arrays_core.to_variables_at new_vars) tg
  )


(** [tile ~block_type block_size tg]: expects the target [tg] to point at an array declaration.
   Then it takes that declaration and transforms it into a tiled array. All the accesses of the
   targeted array are handled as well.
   [block_type] - denotes the name of the array which is going to represent a tile.
   [block_size] - size of the block of tiles. *)
let%transfo tile ?(block_type : string = "") (block_size : var) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths_in_seq (Arrays_core.tile_at block_type block_size) tg
  )

(** [swap name x tg]: expects the target [tg] to point at an array declaration.
   It changes the declaration so that the bounds of the array are switched. Also
   all the accesses of the targeted array are handled as well.*)
let%transfo swap (tg : target) : unit =
  apply_at_target_paths_in_seq Arrays_core.swap_at tg


(** [aos_to_soa tv sz] finds the definition of type [tv] which should be a typedef Record.
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
  Trace.apply (fun t ->
    Arrays_core.aos_to_soa_rec tv sz t
  )

(** [set_explicit tg] expects the target [tg] to point at an array declaration
    then it will remove the initialization trm and a list of write operations on
    each of the cells of the targeted array.
*)
let%transfo set_explicit (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths (Arrays_core.detach_init_on) tg)

let inline_constant_on (array_var : var) (array_vals : trm list) (mark_accesses : mark) (t : trm) : trm =
  let error = "Arrays_basic.inline_constant_on: expected array access with constant index" in
  (* Show.trm "t" t; *)
  let ptr_t = trm_inv ~error trm_get_inv t in
  let (base, index) = trm_inv ~error trm_array_access_inv ptr_t in
  let var = trm_inv ~error trm_var_inv base in
  if var <> array_var then
    trm_fail base error;
  (* TODO: check that moving trm evaluation here is ok *)
  begin match trm_inv ~error trm_lit_inv index with
  | Lit_int (_, i) -> trm_add_mark mark_accesses (List.nth array_vals i)
  | _ -> trm_fail index error
  end

(** [inline_constant] expects the target [decl] to point at a constant array literal declaration, and resolves all accesses targeted by [tg], that must be at constant indices.
  *)
let%transfo inline_constant ?(mark_accesses : mark = no_mark) ~(decl : target) (tg : target) : unit =
  let decl_p = resolve_target_exactly_one_with_stringreprs_available decl (Trace.ast ()) in
  let decl_t = Path.resolve_path decl_p (Trace.ast ()) in
  let error = "Arrays_basic.inline_constant: expected constant array literal declaration" in
  let (var, typ, init) = trm_inv ~error trm_let_inv decl_t in
  let (_elem_ty, _size) = typ_inv ~error decl_t typ_array_inv typ in
  let _, array_mlist = trm_inv ~error trm_array_inv init in
  Target.apply_at_target_paths (inline_constant_on var (Mlist.to_list array_mlist) mark_accesses) tg

let elim_on (decl_index : int) (t : trm) : trm =
  let remove_decl (t : trm) : trm =
    let error = "Arrays.elim_constant_on: expected constant array literal declaration" in
    let (name, typ, init) = trm_inv ~error trm_let_inv t in
    (* Tools.debug "QSJIDO:\n%s" (Ast_to_text.ast_to_string t); *)
    let (_elem_ty, _size) = typ_inv ~error t typ_array_inv typ in
    let _array_mlist = trm_inv ~error trm_array_inv init in
    trm_seq_nobrace_nomarks []
  in
  (* TODO: check that its not used anywhere *)

  let instrs = trm_inv
   ~error:"Arrays.elim_constant_on: expected sequence"
   trm_seq_inv t in
  let new_instrs = Mlist.update_nth decl_index remove_decl instrs in
  trm_seq ~annot:t.annot ?loc:t.loc new_instrs

(** [elim] expects the target [tg] to point at a constant array literal declaration, and eliminates it if it is not accessed anymore.
  *)
let%transfo elim (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths_in_seq elim_on tg)
