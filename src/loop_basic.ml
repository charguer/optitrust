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
      - TileBoundDivides: generates a constraint of the form [i < X], which is only true if B divides X

   It produces:
   [for (int index = 0; index < stop; index += tile_size) {
      for (int i = index; i < min(X, bx+B); i++) { body }]. *)
let tile ?(index : var = "b${id}") ?(bound : tile_bound = TileBoundMin) (tile_size : trm) : Transfo.t =
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
let hoist ?(name : var = "${var}_step") ?(array_size : trm option = None) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_transformed_targets (Path.index_in_surrounding_loop)
     (fun t (i, p) -> Loop_core.hoist name i array_size t p) tg)

(* [fission tg]: expects the target [tg] to point somewhere inside the body of the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.

   @correctness: Reads in new second loop need to never depend on writes on
   first loop after index i. Writes in new second loop need to never overwrite
   writes in first loop after index i. *)
let fission (tg : target) : unit =
  Internal.nobrace_remove_after( fun _ ->
    apply_on_transformed_targets_between (fun (p,i) -> Path.index_in_surrounding_loop (p @ [Dir_seq_nth i]))
    (fun t (i, p) -> Loop_core.fission i t p) tg )

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
let move_out (tg : target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  apply_on_transformed_targets (Path.index_in_surrounding_loop)
    (fun t (i, p) -> Loop_core.move_out i t p ) tg)

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

(* [shift index amount]: shifts a loop index by a given amount. *)
let shift (index : var) (amount : trm) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Loop_core.shift index (Loop_core.Add amount)) tg)
    
(* [shift_to_zero index]: shifts a loop index to start from zero. *)
let shift_to_zero (index : var) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Loop_core.shift index Loop_core.ToZero) tg)

(* [rename_index new_index]: renames the loop index variable *)
let rename_index (new_index : var) (tg : target) : unit =
  apply_on_targets (Loop_core.rename_index new_index) tg