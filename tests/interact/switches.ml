(* Usage: see interact.ml;
   make switches.out ; ls *_out.cpp *)

open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Label.add "r0" [cVarDef "a"];
  Trace.switch [
    (fun () ->
        !! Label.add "m1" [cVarDef "b"]);
    (fun () ->
        !! Label.add "m1" [cVarDef "b"];
           Label.add "m2" [cVarDef "b"];
        !! Label.add "m3" [cVarDef "b"];);
    (fun () ->
        Trace.dump ~prefix:"switch_aux"()); ];

   show [cVarDef "a"];
   !! Label.add "r1" [cVarDef "c"];
  Trace.switch [
    (fun () ->
        !! Label.add "k1" [cVarDef "d"]);
    (fun () ->
        !! Label.add "k2" [cVarDef "d"])];
)

