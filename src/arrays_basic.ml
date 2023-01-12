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
