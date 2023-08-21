open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->

  !! Sequence_basic.elim [cLabel "toinline"];

)

"
int main() {
  int a = 0;
  toinline:{
    int b = 1;
    int c = 2;
  }
  a++;
}
"

let _ = Run.script_cpp ( fun _ ->

  !! Sequence_basic.elim [cSeq ~args:[[cVarDef "u"]] ()];
  !! Sequence_basic.elim [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "z"]) ()];

  (* Shadowing behaviour (1). *)
  !! Loop_basic.unroll [cFor "i"];
  !! Trace.failure_expected (fun () ->
    Sequence_basic.elim [nbMulti; cSeq ~args:[[cVarDef "s0"]] ()]);

  (* Shadowing behaviour (2). *)
  !! Trace.failure_expected (fun () ->
    Sequence_basic.elim [cSeq ~args:[[cVarDef "s1"]] ()]);
)
