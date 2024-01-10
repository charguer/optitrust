open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: add function calls. *)
  !! Resources.show ();

  (* 1. effects/bindings are unused *)
  (* TODO: ~shadowed:true / ~unused:true ? *)
  !! Sequence_basic.delete [cFunBody "dead_code"; sInstr "a++"];
  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; cOr [[cVarDef "a"](* FIXME: ; [cVarDef "v"]*)]];
  (* FIXME:  !! Sequence_basic.delete [nbMulti; cFunBody "dead_code"; sInstr "u."]; *)
  !! Sequence_basic.delete [cFunBody "dead_code"; cVarDef "y"];
  !! Sequence_basic.delete [cFunBody "dead_code"; sInstr "z = i + j"];
  !! Sequence_basic.delete [cFunBody "dead_code"; cFor "j"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [cFunBody "dead_code"; sInstr "z = i"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [cFunBody "dead_code"; sInstr "x = z"]);

  (* 2. effects are shadowed  s.step_style_before <- style_normal_code();
  s.step_style_after <- style_resources() *)
  (* TODO: ~shadowed:true ? *)
  !! Sequence_basic.delete [cFunBody "shadowed"; sInstr "x = 3"];
  !! Sequence_basic.delete [occFirst; cFunBody "shadowed"; sInstr "*y = x"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [cFunBody "shadowed"; sInstr "x = 5"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [occFirst; cFunBody "shadowed"; sInstr "*y = x"]);

  (* 3. effects are redundant *)
  (* TODO: ~redundant_with:tg ? *)
  !! Sequence_basic.delete [cFunBody "redundant"; occLast; sInstr "*x = 2"];
  !! Sequence_basic.delete [cFunBody "redundant"; occLast; sInstr "*x = 3"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [cFunBody "redundant"; occLast; sInstr "*x = 3"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.delete [cFunBody "redundant"; occLast; sInstr "*x = v"]);

  (* can't delete expression, must be an instruction. *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
       Sequence_basic.delete [nbMulti; cFunBody "wrong_target"; cInt 8]);

)
