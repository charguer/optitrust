open Optitrust
open Prelude

let _ = Flags.check_validity := true (* FIXME: false *)
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_all (*Steps_important*)

let int = trm_int

let _ = Run.script_cpp (fun () ->
  (* !! Function.elim_infix_ops ~indepth:true []; *)
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];

  (* LATER: !! Variable.local_name ~var:"s" ~local_var:"t" [tSpanSeq [cForBody "bi"]]; *)
  !! (
    Sequence.intro ~mark:"t_scope" ~start:[tFirst; cForBody "bi"] ~stop:[tLast; cForBody "bi"] ();
    Variable.local_name ~var:"s" ~local_var:"t" [cMark "t_scope"];
    Sequence.elim [cMark "t_scope"];
  );

  (* DEPRECATED? !! Sequence_basic.insert (trm_let (new_var "d", typ_f32) (trm_get (trm_find_var "s" []))) [tFirst; cForBody "bi"]; *)
  !! (
    Variable.insert ~name:"d" ~typ:typ_f32 ~value:(trm_get (trm_find_var "s" [])) [cForBody "bi"; tFirst];
    Accesses.shift_var ~inv:true ~factor:(trm_find_var "d" []) [cFor "bi"; cVarDef "t"];
    Variable.inline [cVarDef "d"];
    Arith.simpl_surrounding_expr Arith.gather_rec [nbMulti; cVar "s"];
  );

  !! Loop.hoist [cVarDef "t"];
  !! Loop.fission [tBefore; cFor "bi"; cWriteVar "s"];
  !! Loop.parallel [cFor "bi" ~body:[cFor "i"]];
  !! Cleanup.std();
  (* includes: !! Function.use_infix_ops ~indepth:true []; *)
)
