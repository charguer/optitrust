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
  [block_name] the name of the array which is going to represent a tile.
  [b] is the size the block.
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
let tile (block_name : typvar) (b : var) (tg : target) : unit =
  Target.apply_on_transformed_targets(Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.tile block_name b i t p) tg

(* [swap name x tg] expects the target [tg] to point to an array delcaratio.
   It changes the declaration so that the bounds of the array ar switched. Also 
   all the in all the occurrences of the array the accesses are swaped.
*)
let swap (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.swap i t p) tg


(* [aos_to_soa name x tg] expects the target [tg] to point to an array of structures declaration.
    then it will change this declaration into a single variable and the underlying struct type will 
    become a structure of arrays. All the accesse will be swapped.
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
let aos_to_soa (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t ->  Arrays_core.aos_to_soa i t p) tg



