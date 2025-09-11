open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  let (x, _) = find_var "x" [] in
  let (z, _) = find_var "z" [] in
  let (y, _) = find_var "y" [] in
  !! Matrix_basic.intro_malloc0 x [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 z [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 y [cFunBody "f"; cFor "n"; dBody];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "x"];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "z"];
  (*
  !! Ast.assert_transfo_error "expected uninitialized allocation" (fun _ ->
    Loop_basic.hoist [cVarDef "w"]);
  *)

  (* without_substep_validity_checks (fun () -> *)
  (* FIXME: handle div operator
  !! Loop_basic.hoist ~name:"yn" [cFunBody "f"; cVarDef "y"];
  !! Loop_basic.hoist ~name:"ym" [cFunBody "f"; cVarDef "yn"];
  !! Loop_basic.hoist ~name:"yl" [cFunBody "f"; cVarDef "ym"];
  *)

  let (y, _) = find_var "sum" [] in
  !! Matrix_basic.intro_malloc0 y [cFunBody "f2"; cFor "j"; dBody];
  !! Loop_basic.hoist [cFunBody "f2"; cVarDef "sum"];

  (* TODO: Factorize simplifications of array accesses *)
  !! step_backtrack (fun () ->
    let mark = Mark.next () in
    (* FIXME: div op, "yn"; "ym"; "y"; *)
    without_substep_validity_checks (fun () ->
      (* FIXME: better inline justifs *)
      Variable_basic.inline ~mark [nbMulti; cVarDefs ["x"; "z"; "sum"]];
    );
    let paths = Target.resolve_target [nbMulti; cMark mark] in
    let paths = List.remove_duplicates (List.map (fun p -> (Path.parent p)) paths) in
    List.iter (fun path ->
      Target.apply_at_path (trm_bottom_up_try
        Matrix.simpl_access_of_access_on
       ) path
      ) paths;
    List.iter (fun path ->
      Target.apply_at_path (trm_bottom_up_try
        Matrix.simpl_index_add_on
        ) path
      ) paths;
  )
)
