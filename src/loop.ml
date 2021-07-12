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
   In produces:
   [for (int tile_index = 0; tile_index < stop; tile_index += tile_width) {
      for (int i = tile_index; i < min(X, bx+B); i++) { body }].
*)
let tile (tile_width : string_trm) (tile_index : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile tile_width tile_index)

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
  
(* TODO: move to old folder in old_tiling.ml *)
(*
  -----------DEPRECATED-----------------
  transform a pre-tiled loop of the form
  optional_label:
  for i = 0; i < N; i++
    int i1 = i / block_size
    int i2 = i % block_size
    body
  into a loop of the form
  optional_label:
  for i1 = 0; i1 < N / block_size; i1++
    for i2 = 0; i2 < block_size; i2++
      i = i1 * block_size + i2 // only if i is used in body
      body
 *)


(* [tile_old tg] *)
let tile_old : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile_old)

(* get_loop_nest_indices -- currently omiting the last one

*)
(* for a { for b {} {  for j {}   ;  for k {} } } -- >  a::b::[]
  the function should check that it is a loop nest :
      aux t =
         if t is a for (i , body) then  i::(aux body)
         if t is a seq[(for(..)) as t1] and nothing else then  aux t1
         else []

*)

(* let rec get_loop_nest_indices (t : trm) : 'a list =
    match t.desc with
    | Trm_labelled (_, t_loop) -> get_loop_nest_indices t_loop
    | Trm_seq [t_loop;_] -> get_loop_nest_indices t_loop
    | Trm_for_c (_,_,_,body) ->
      let loop_index = for_loop_index t in
      begin match body.desc with
      | Trm_seq ({desc = Trm_seq (f_loop :: _);_} :: _) ->
        loop_index :: get_loop_nest_indices f_loop
      | _ -> loop_index :: []
      end
    | _ -> [] *)


(* let move_loop_before_aux (clog : out_channel) (loop_index : var) (t : trm) : trm =
    let log : string =
      let loc : string =
        match t.loc with
        | None -> ""
        | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in
      Printf.sprintf
      ("  - expression\n%s\n" ^^
          "    %sis a (labelled) loop\n"
      )
      (Ast_to_c.ast_to_string t) loc
      in
      write_log clog log;
      (* Get the path from the outer loop to the one we want to swap with


      let path_list = List.rev (get_path clog t) in

        *)
      let path_list = List.rev (get_loop_nest_indices t)  in
      (* do a list rev at the end of get_loop_nest_indices
         let rec chop_list_before x xs =
            | [] -> error "did not find x"
            | y::tl -> if y = x then [] else y:: chop_list_before x tl      *)
      let rec clean_path (xl : 'a list) : 'a list = match xl with
        | [] -> []
        | hd :: tl ->
          if hd = loop_index then tl
          else clean_path tl
      in
      let _check_last = List.mem loop_index path_list in
      (*
      let path_list = if not check_last then path_list
          else clean_path path_list
      *)
      let path_list = clean_path path_list
      in
      (* List.fold_right (fun i acc  -> loop swap t i) path_list acc t
         --checkout the documentation of fold_right *)
      let rec multi_swap (xl : 'a list) (t : trm) : trm = match xl with
      | [] -> t
      | hd :: tl ->
        let t = loop_swap  [cFor_chd] in

        multi_swap tl t
     (* in *)
     multi_swap path_list t *)

(* let move_loop_before (clog : out_channel) (tr : target)(loop_index : var) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
    print_info t.loc "move_loop_before: no matching subterm";
    t
  | _ ->
    List.fold_left
      (fun t dl ->
        apply_on_path (move_loop_before_aux clog loop_index) t dl)
      t
      epl *)

(* let move_loop_after_aux (clog : out_channel) (loop_index : var) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
    ("  - expression\n%s\n" ^^
          "    %sis a (labelled) loop\n"
    )
    (Ast_to_c.ast_to_string t) loc
    in
    write_log clog log;
    let path_list = get_loop_nest_indices t in
    let rec clean_path (xl : 'a list) : 'a list = match xl with
      | [] -> []
      | hd :: tl ->
        if hd = loop_index then tl
        else clean_path tl
      in
    let l_index = List.hd path_list in
    let _check_last = List.mem loop_index path_list in
    let path_list = if false then path_list
      else clean_path (List.rev path_list)
      in
    let path_length = List.length path_list in
    (*if (path_list = []) then error ---try to check the error in case  move_before "i" "i" *)
    (* List.fold_left (fun _i acc -> swap l_index) (List.tl path_list) *)
    let rec multi_swap (count : int) (t : trm) : trm = match count with
      | 0 ->  t
      | _ -> let pl = [cFor_cl_index ] in
           let t = loop_swap clog pl t in
           multi_swap (count-1) t
      in
    multi_swap path_length t *)

(* let move_loop_after (clog : out_channel) (tr : target)(loop_index : var) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
    print_info t.loc "move_loop_before: no matching subterm";
    t
  | _ ->
    List.fold_left
      (fun t dl ->
        apply_on_path (move_loop_after_aux clog loop_index) t dl)
      t
      epl

let move_loop (clog : out_channel)  ?(move_before : string = "") ?(move_after : string = "") (loop_index : string) (t : trm) : trm =
  let log : string =
      let loc : string =
        match t.loc with
        | None -> ""
        | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
          ("  - expression\n%s\n" ^^
          "    %sis a struct type\n"
          )
      (Ast_to_c.ast_to_string t) loc
    in
  write_log clog log;
  match move_before, move_after with
  | "",_ -> move_loop_after clog [cFor_cloop_index] move_after t
  | _,"" -> move_loop_before clog [cFor_cmove_before] loop_index t
  | _,_ -> fail t.loc "move_loop: only one of move_before or move_after should be specified" *)



