open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cVarDef "a"];
  !! Sequence_basic.intro_between [tBefore; cVarDef "d1"] [tAfter; cVarDef "d3"];
  !! Sequence_basic.intro_after [cVarDef "f"];

)

"
int main() {
  int a = 0;
  int b1 = 0;
  int b2 = 0;
  int c = 0;
  int d1 = 0;
  int d2 = 0;
  int d3 = 0;
  int e = 0;
  int f = 0;
  int g = 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor "i"];
  !! Sequence_basic.intro_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence_basic.intro_after [cVarDef "u"];
  !! Trace.alternative (fun _ ->
      Sequence_basic.intro_before [cVarDef "u"];
      !! ();
  );

  !! Trace.failure_expected (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Trace.failure_expected (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);

  !! Trace.failure_expected (fun () ->
    Sequence_basic.intro 1 [cFor "k"; cVarDef "a"]);
  !! Sequence_basic.intro 2 [cFor "k"; cVarDef "b"]
)
