open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 * All the follwing transformations expects target to point to a simple loop,         *
 * say [for (int i = start; i < stop; i += step) { body } ]. 
 *)

(* [swap tg] expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let swap : Target.Transfo.t =
  Target.apply_on_target (Loop_core.swap)

(* [color nb_colors i_color tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   [nb_colors] - denotes the number of colors (e.g., ["2"]),
   [color_index] - denotes a fresh name to use as index for iterating over colors.
   In case [step = 1]:
   [for (int color_index = 0; color_index < nb_color; color_index++) {
      for (int i = color_index; i < stop; i += nb_color) { body }].
   In the general case, it produces:
   [for (int color_index = 0; color_index < nb_color; color_index++) {
      for (int i = color_index*step; i < stop; i += step*nb_color) { body }]. 
*)
let color (nb_colors : string_trm) (color_index : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.color nb_colors color_index)


(* [tile tile_width tile_index tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ]. 
   divides - denotes a flag to know if tile_width divides the size of the array or not 
   [tile_width] - denotes the width of the tile (e.g., ["2"])
   [tile_index] - denotes a fresh name to use as index for iterating over tiles.
   It produces:
   [for (int tile_index = 0; tile_index < stop; tile_index += tile_width) {
      for (int i = tile_index; i < min(X, bx+B); i++) { body }].
*)
let tile ?(divides : bool = true) (tile_width : string_trm) (tile_index : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile divides tile_width tile_index)

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
let hoist (x_step : var) : Target.Transfo.t =
  Target.apply_on_transformed_targets(Internal.get_decl_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.hoist x_step i t p)

(* [split tg]: expects [tg] to point somewhere insie the body ot the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.
*)
let split : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.get_decl_in_surrounding_loop)
    (fun (p,i) t -> Loop_core.split i t p )

(* [fusion tg] expects [tg] to point to a sequence containing two loops 
    with the same range, start step and bound but different body.
    Then it's going to take the body of the second loop and append 
    it to the body ot the first loop. 
*)
let fusion : Target.Transfo.t =
  Target.apply_on_target (Loop_core.fusion)

(* [extract_variable tg] expects tg to point to an uninitialized variable  
   declaration inside a for loop. The idea is similar to loop hoist
*)
let extract_variable : Target.Transfo.t =
  Target.apply_on_transformed_targets(Internal.get_decl_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.extract_variable i t p)

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
  

(* [move before after loop_to_move] move one loop before or after another loop in 
    a "sequence"(not in the context of Optitrust) of nested loops.
    [before] - a default argument given as empty string, if the user wants to move 
      [loop_to_move] before another loop then it should use this default argument with the 
      value the the quoted loop intex
    [after] - similar to [after] but now is the index of the loop after whom 
      we want to move [loop_to_move]
*)
let move ?(before : string = "") ?(after : string = "") (loop_to_move : string) : unit = 
  let t = Trace.get_ast() in
  let move_where, target_loop = match before, after with 
  | "", _ -> "after", [Target.cFor loop_to_move]
  | _, "" -> "before", [Target.cFor before]
  | _ -> fail None "move: make sure you specify where to move the loop, don't give both before and after directives" in
  let exp = Constr.resolve_target_exactly_one target_loop t in
  let (loop, _) = Path.resolve_path exp t in
  let indices_list = Internal.get_loop_nest_indices loop in
  match move_where with 
  | "after" -> 
    let indices_list = Tools.chop_list_after after indices_list in
    Tools.printf "%s\n" (Tools.list_to_string indices_list);
    let counter = ref (List.length indices_list) in
    while (!counter <> 0) do
      counter := !counter - 1;
      swap [Target.cFor loop_to_move];
      Tools.printf "%s\n" "Swap done";
    done
  | "before" ->
    let indices_list = Tools.chop_list_after loop_to_move indices_list in
    List.iter (fun x -> swap [Target.cFor x]) (List.rev indices_list)
  | _ -> fail t.loc "move: something went wrong"


(* [unroll] expects the target to point to a loop. It the checks if teh loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the 
    the instructions out of the loop and the loop will be removed.
*)
let unroll : Target.Transfo.t =
  Target.apply_on_transformed_targets(Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Loop_core.unroll i t p)