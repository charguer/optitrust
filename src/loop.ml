open Ast
(* [swap tg] *)
let swap : Target.Transfo.t =
  Target.apply_on_target (Loop_core.swap)

(* [color c i_color tg] *)
let color (c : var) (i_color : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.color c i_color )

(* [tile b i_bloc tg] *)
let tile (b : var)(i_block : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.tile b i_block)


(* [hoist x_step tg] *)
let hoist (x_step : var) : Target.Transfo.t =
  Target.apply_on_target (Loop_core.hoist x_step)

(* [split tg] *)
let split (index : int) : Target.Transfo.t = 
  Target.apply_on_target(Loop_core.split index)

(* [fusion tg] *)
let fusion : Target.Transfo.t =
  Target.apply_on_target (Loop_core.fusion )


(* TODO: Ask Arthur, if this should still be used or not *)
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
  Target.apply_on_target(Loop_core.tile_old )

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
    | Trm_for (_,_,_,body) ->
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
        let t = loop_swap  [cFor hd] in
        
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
      | _ -> let pl = [cFor l_index ] in
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
  | "",_ -> move_loop_after clog [cFor loop_index] move_after t
  | _,"" -> move_loop_before clog [cFor move_before] loop_index t
  | _,_ -> fail t.loc "move_loop: only one of move_before or move_after should be specified" *)


 
