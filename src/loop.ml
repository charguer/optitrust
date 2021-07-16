open Ast

(* All the follwing transformations expects target to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ]. 
*)


(* [swap tg] expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let swap : Target.Transfo.t =
  Target.apply_on_target (Loop_core.swap)


(* Description of a term as a string (convenient for the user)
   LATER: this type might become generalized in the future. *)
type string_trm = string

(* [color nb_colors i_color tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ].
   It takes as arguments:
   - [nb_colors] denotes the number of colors (e.g., ["2"]),
   - [color_index] denotes a fresh name to use as index for iterating over colors.
   In case [step = 1], it transforms the loop into the nested loops.
   [for (int color_index = 0; color_index < nb_color; color_index++) {
      for (int i = color_index; i < stop; i += nb_color) { body }].
   In the general case, it produces:
   [for (int color_index = 0; color_index < nb_color; color_index++) {
      for (int i = color_index*step; i < stop; i += step*nb_color) { body }]. *)
let color (nb_colors : string_trm) (color_index : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.color nb_colors color_index)


(* [tile b i_bloc tg]: expects [tg] to point to a simple loop,
   say [for (int i = start; i < stop; i += step) { body } ]. 
   It takes as arguments:
    - [tile_width] denotes the width of the tile (e.g., ["2"])
    - [tile_index] denotes a fresh name to use as index for iterating over tiles.
   It produces:
   [for (int tile_index = 0; tile_index < stop; tile_index += tile_width) {
      for (int i = tile_index; i < min(X, bx+B); i++) { body }].
*)
let tile ?(divides : bool = true) (tile_width : string_trm) (tile_index : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile divides tile_width tile_index)

(* [hoist x_step tg] *)
let hoist (x_step : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.hoist x_step)

(* [split tg]: expects [tg] to point somewhere insie the body ot the simple loop
   It splits the loop in two loops, the spliting point is trm matched by the relative target.
*)
let split : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.get_decl_in_surrounding_loop)
    (fun (p,i) t -> Loop_core.split i t p )

(* [fusion tg] *)
let fusion : Target.Transfo.t =
  Target.apply_on_target (Loop_core.fusion)

(* [extract_variable tg] *)
let extract_variable : Target.Transfo.t =
  Target.apply_on_transformed_targets(Generic_core.get_decl_in_surrounding_loop)
    (fun (p, i) t -> Loop_core.extract_variable i t p)

(* [grid_enumerate index_and_bounds tg] *)
let grid_enumerate (index_and_bounds : (string * string) list) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.grid_enumerate index_and_bounds)
  
let rec get_loop_nest_indices (t : trm) : 'a list = 
  match t.desc with 
  | Trm_for (index, _, _, _, _, body) ->
    begin match body.desc with 
    | Trm_seq [f_loop] ->
      index :: get_loop_nest_indices f_loop
      
    | _ -> 
      (* Ast_to_text.print_ast ~only_desc:true stdout body; *)
      index :: []
    
    end
  | Trm_for_c (_, _, _, body) ->
    let index = for_loop_index t in 
    begin match body.desc with 
    | Trm_seq [f_loop] ->
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | _ -> []


(* [move before after loop_to_move] *)
let move ?(before : string = "") ?(after : string = "") (loop_to_move : string) : unit = 
  let t = Trace.get_ast() in
  let move_where, target_loop = match before, after with 
  | "", _ -> "after", [Target.cFor loop_to_move]
  | _, "" -> "before", [Target.cFor before]
  | _ -> fail None "move: make sure you specify where to move the loop, don't give both before and after directives" in
  let exp = Constr.resolve_target_exactly_one target_loop t in
  let (loop, _) = Path.resolve_path exp t in
  let indices_list = get_loop_nest_indices loop in
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

let unroll : Target.Transfo.t =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Loop_core.unroll i t p)