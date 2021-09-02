open Ast
open Target

(* [to_variables new_vars tg] expects the target [tg] pointing to an array declaration.
    It then transforms this declaration into a list of declarations.
    [new_vars] - denotes the list of variables which is going to replace the initial declaration
      the length of this list is equal to one less than this size of the array.
*)
let to_variables (new_vars : var list) (tg : target) : unit = 
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.to_variables new_vars i t p 
  ) tg

(*[tile name block_name b x tg] expects the target [tg] to point to an array declaration.
  It then takes this declaration and transforms it into a tiled array.
  [block_type] is the size the block.
  [block_size] the name of the array which is going to represent a tile.

  Ex:
     int const N = 40;            int contst B = 8;
     int const B = 8;             typedef int X[B];
     typedef int *T;              typedef X * T;
     T t;                         T t;
     int main() {  =>=>=>         int main(){
      int i;                        int i;
      int x = t[i];                 int x = t[i / B][i % B];
      return 0;                     return 0;  
    }                             }
*)
let tile ?(block_type :typvar = "") (block_size : var) (tg : target) : unit =
  Internal.nobrace_enter();
  Target.apply_on_transformed_targets(Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.tile block_type block_size i t p) tg;
  Internal.nobrace_remove_and_exit ()

(* [swap name x tg] expects the target [tg] to point to an array delcaratio.
   It changes the declaration so that the bounds of the array ar switched. Also 
   all the in all the occurrences of the array the accesses are swaped.
*)
let swap (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.swap i t p) tg


(* [aos_to_soa tv sz] finds the definition of type [tv] which should be a typedef struct.
    Then it will change its struct fields type to arrys of size [sz] with type their current type.
    All the accesses will be swapped.
    Ex:
      int const N = 100;    int const N = 100;
      typedef struct {      typedef struct {
        int x;                int x[N];
        int y;                int y[N];
      } vect;               } vect;
      vect w[N];            vect w:
      int main(){           int main(){
        int i;                int i;
        int c = w[i].x;       int c = w.x[i];
        return 0;             return 0;
      }                     }
*)
let aos_to_soa (tv : typvar) (sz : var) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,_) t ->  Arrays_core.aos_to_soa tv sz t p) [Target.cFunDef "main"]
  (* Target.apply_on_target(Arrays_core.aos_to_soa tv sz) [Target.dRoot] *)
  


