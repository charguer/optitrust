open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 * All the follwing transformations expects target to point to a simple loop,         *
 * say [for (int i = start; i < stop; i += step) { body } ].
 *)

(* [interchange tg] expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let interchange : Target.Transfo.t =
  Target.apply_on_target (Loop_core.interchange)

(* [color nb_colors i_color tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   [nb_colors] - denotes the number of colors (e.g., ["2"]),
   [index] - denotes a fresh name to use as index for iterating over colors.
   In case [step = 1]:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index; i < stop; i += nb_color) { body }].
   In the general case, it produces:
   [for (int index = 0; index < nb_color; index++) {
      for (int i = index*step; i < stop; i += step*nb_color) { body }].
*)
let color (nb_colors : string_trm) ?(index : var = "") : Target.Transfo.t =
  Target.apply_on_target (Loop_core.color nb_colors index)


(* [tile tile_size index tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   divides - denotes a flag to know if tile_size divides the size of the array or not
   [tile_size] - denotes the width of the tile (e.g., ["2"])
   [index] - denotes a fresh name to use as index for iterating over tiles.
   [bound] - denotes the bound type for the produced loop
   It produces:
   [for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }].
*)
let tile ?(index : var = "") ?(bound : tile_bound = TileBoundMin) (tile_size : string_trm) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile index bound tile_size)

(* [hoist x_step tg]: expects [tg] to point to simple loop.
    [x_step] - denotes the variable going to be hoisted outside the loop
    Ex:
      int *t;                                 int *t;
      int *u;                                 int *u;
      int main() {                            int main(){
        for (int i = 0; (i < 10); i++) {        int x_steo[10];
          int x;                                for (int i = 0; i < 10; i++){
          x = t[i];                               x_step[i] = t[i];
          u[i] = x;                               u[i] = x_step[i];
        }                                       }
        return 0;                               return 0;
      }                                       }
*)
let hoist (x_step : var) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_transformed_targets(Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.hoist x_step i t p) tg) 

(* [fission tg]: expects [tg] to point somewhere inside the body ot the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.
*)
let fission (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ ->
    Target.apply_on_transformed_target_between (Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.fission i t p) tg )

(* [fusion_on_block tg] expects [tg] to point to a sequence containing two loops
    with the same range, start step and bound but different body.
    Then it's going to take the body of the second loop and append
    it to the body ot the first loop.
*)
let fusion_on_block ?(keep_label : bool = true) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.fusion_on_block keep_label)

(* [extract_variable tg] expects tg to point to an uninitialized variable
   declaration inside a for loop. The idea is similar to loop hoist
*)
let extract_variable (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ ->
    Target.apply_on_transformed_targets (Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.extract_variable i t p) tg)

(* [grid_enumerate index_and_bounds tg] expects tg to point to loop iterating over
    a grid. The grid can be of any dimension. This loop is transformed into nested loops
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
                                          }
*)
let grid_enumerate (index_and_bounds : (string * string) list) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.grid_enumerate index_and_bounds)


(* [unroll] expects the target to point to a loop. It the checks if teh loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed.
    Assumption: C should be a literal, this is needed to compute the number 
    of sequences to generate.
*)
let unroll ?(label : var = "") (tg : Target.target): unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_target (Loop_core.unroll label) tg)



(* [invariant] expects the target [tg] to point to an instruction inside the loop
    which is not dependent on the index of the loop or any local variable.
    Then it will take it outside the loop.
*)
let invariant (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_transformed_targets (Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.invariant i t p ) tg)

(* [unswitch tg] expects the target [tg] to point to an if statement inside the loop
     with a constant condition (not dependent on loop index or local variables)
     Then it will take the if statment outside the loop.
*)
let unswitch (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_transformed_targets(Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.unswitch i t p) tg)


(* [to_unit_steps index tg] expects target [tg] to point to a for loop
    [index] - denotes the new index for the transformed loop
        by default is an empty string. The reason for that is to check if the user
        gave the name of the new index of not. If not then [index] = unit_index
        where index is the index of the targeted loop.
    Assumption:
      The targeted loop should be of the form:
        for (int i = a; i < b; i+=B){...}, and it assumes that B divides (b-a). It then
        transforms the targeted loop into the following form:
          for (int index = 0;)
*)
let to_unit_steps ?(index : var = "" ) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.to_unit_steps index)

