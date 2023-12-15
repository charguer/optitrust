open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Resources.show ();
  let x = find_var_in_current_ast "x" in
  let z = find_var_in_current_ast "z" in
  let y = find_var_in_current_ast "y" in
  !! Matrix_basic.intro_malloc0 x [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 z [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 y [cFunBody "f"; cFor "n"; dBody];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "x"];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "z"];
  (*
  !! Ast.assert_transfo_error "expected uninitialized allocation" (fun _ ->
    Loop_basic.hoist [cVarDef "w"]);
  *)

  !! Loop_basic.hoist ~name:"yn" [cFunBody "f"; cVarDef "y"];
  !! Loop_basic.hoist ~name:"ym" [cFunBody "f"; cVarDef "yn"];
  !! Loop_basic.hoist ~name:"yl" [cFunBody "f"; cVarDef "ym"];

  let y = find_var_in_current_ast "sum" in
  !! Matrix_basic.intro_malloc0 y [cFunBody "f2"; cFor "j"; dBody];
  !! Loop_basic.hoist [cFunBody "f2"; cVarDef "sum"];

  (* TODO: Factorize simplifications of array accesses *)
  !! step_backtrack (fun () ->
    let mark = Mark.next () in
    Variable_basic.inline ~mark [nbMulti; cVarDefs ["x"; "z"; "yn"; "ym"; "y"; "sum"]];
    let paths = Target.resolve_target_current_ast [nbMulti; cMark mark] in
    let paths = Xlist.remove_duplicates (List.map (fun p -> Path.find_surrounding_expr p (Trace.ast ())) paths) in
    List.iter (fun path ->
      Target.apply_at_path (trm_bottom_up (fun t ->
        try Matrix.simpl_access_of_access_on t
        with Contextualized_error _ -> t)) path
      ) paths;
    List.iter (fun path ->
      Target.apply_at_path (trm_bottom_up (fun t ->
        try Matrix.simpl_index_add_on t
        with Contextualized_error _ -> t)) path
      ) paths;
    Resources.show ();
  )
)
