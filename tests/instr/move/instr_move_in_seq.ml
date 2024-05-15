open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  !! Instr.move_in_seq ~dest:[tFirst] [cFunBody "pair"; tSpan [cVarDef "k2"; tBefore] [cVarDef "k3"; tAfter]];
  !! Instr.move_in_seq ~dest:[tFirst] [cFunBody "pair"; tSpan [cVarDef "k2"; tBefore] [cVarDef "k3"; tAfter]];
  !! Instr.move_in_seq ~dest:[tLast] [cFunBody "pair"; cVarDef "k1"];

  (* TODO: Would be better to preserve the mark around the moved instructions *)
  !! Marks.add "m" [cFunBody "pure_facts"; tSpan [cCall "req_triv"; occFirst; tBefore] [cCall "req_triv"; occLast; tAfter]];
  !! Instr.move_in_seq ~dest:[cVarDef "k"; tAfter] [cMarkSpan "m"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Instr.move_in_seq ~dest:[cVarDef "k"; tBefore] [cFunBody "pure_facts"; tSpan [cCall "req_triv"; occFirst; tBefore] [cCall "req_triv"; occLast; tAfter]]
  );

  !! Instr.move_in_seq ~dest:[cVarDef "z"; tAfter] [cFunBody "pure_facts"; cCall "req_triv"; occLast];

  !! Instr.move_in_seq ~dest:[tFirst] [cFunBody "pure_noop"; cCall "req_triv"];

  !! Instr.move_in_seq ~dest:[tLast] [cFunBody "with_assert_alias"; cVarDef "a"];
)
