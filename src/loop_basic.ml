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
  Target.apply_on_targets (Loop_core.interchange)

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
  Target.apply_on_targets (Loop_core.color nb_colors index)


(* [tile tile_size index tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   divides - denotes a flag to know if tile_size divides the size of the array or not
   [tile_size] - denotes the width of the tile (e.g., ["2"])
   [index] - denotes a fresh name to use as index for iterating over tiles.
   [bound] - can be one of
      - TileBoundMin: generates a constraint of the form  [i < min(X, bx+B)]
      - TileBoundAnd: generates a constraint of the form [i <  X && i < bx+B]
      - TileBoundDivides: generates a constraint of the form [i < X], which is only true if B divides X

   It produces:
   [for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }].
*)
let tile ?(index : var = "b${id}") ?(bound : tile_bound = TileBoundMin) (tile_size : string_trm) : Target.Transfo.t =
  Target.apply_on_targets (Loop_core.tile index bound tile_size)

(* [hoist x_step tg]: expects [tg] to point to a variable declaration inside a
    simple loop. Let's say for {int i ...} {
        int x; [tg]
        ...
        x = ..
      }
    The declaration should be detached. Then it will add an array  declaration
    with name [name] right before the loop ex. int name[N] when N is the upper bound
    of the loop and make the targeted variable point to the array [name] at index i.
    ex. int x = name[i].
    [x_step] - denotes the array name which is going to hoist all the values of the targetd variable
    for each index of the for loop.
*)
let hoist ? (name : var = "${var}_step") (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_transformed_targets (Internal.get_trm_in_surrounding_loop)
     (fun t (p, i) -> Loop_core.hoist name i t p) tg)

(* [fission tg]: expects [tg] to point somewhere inside the body ot the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.
*)
let fission (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ ->
    Target.apply_on_transformed_targets_between (fun (p,i) -> Internal.get_trm_in_surrounding_loop (p @ [Dir_seq_nth i]))
    (fun t (p, i) -> Loop_core.fission i t p) tg )

(* [fusion_on_block tg] expects [tg] to point to a sequence containing two loops
    with the same range, start step and bound but different body.
    Then it's going to append the body of to the body ot the first loop and of course
    remove the second loop from the ast.
*)
let fusion_on_block ?(keep_label : bool = false) : Target.Transfo.t =
  Target.apply_on_targets (Loop_core.fusion_on_block keep_label)

(* [grid_enumerate index_and_bounds tg] expects tg to point to loop iterating over
    a grid. The grid can be of any dimension. Loop  [tg] is transformed into nested loops
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
  Target.apply_on_targets (Loop_core.grid_enumerate index_and_bounds)


(* [unroll] expects the target to point to a simple loop of the shape
    for (int i = a; i < a + C; i++) or for (int i = 0; i < C; i++)
      then it will move the instructions out of the loop by replacing
      the index i occurrence with a + j in and j in the second case where
      j is an integer in rance from 0 to C.

    Assumption: Both a and C should be declared as constant variables.
*)
let unroll ?(braces : bool = false) ?(my_mark : mark  = "")  (tg : Target.target): unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets (Loop_core.unroll braces my_mark) tg)


(* [invariant] expects the target [tg] to point to an instruction inside the loop
    which is not dependent on the index of the loop or any local variable.
    Then it will take it outside the loop.
*)
let invariant (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_transformed_targets (Internal.get_trm_in_surrounding_loop)
    (fun t (p, i) -> Loop_core.invariant i t p ) tg)

(* [unswitch tg] expects the target [tg] to point to an if statement inside the loop
     with a constant condition (not dependent on loop index or local variables)
     Then it will take the if statment outside the loop.
*)
let unswitch (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_transformed_targets(Internal.get_trm_in_surrounding_loop)
    (fun t (p, i) -> Loop_core.unswitch i t p) tg)


(* [to_unit_steps index tg] expects target [tg] to point to a for loop
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
           }
*)
let to_unit_steps ?(index : var = "" ) : Target.Transfo.t =
  Target.apply_on_targets (Loop_core.to_unit_steps index)

(* [fold ~direction index start stop step tg] expects the target [tg] to point to the first instruction in a sequence
    and it assumes that the sequence containing the target [tg] is composed of a list of instructions which 
    can be expressed into a single for loop with [index] [direction] [start] [nb_instructions] and [step] as loop 
    components.
*)
let fold ?(direction : loop_dir = DirUp)(index : var) (start : int) (step : int) : Target.Transfo.t =
  Target.apply_on_targets (
    Loop_core.fold index direction start step
)

