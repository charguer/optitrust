(* make optitrust; make switches.out; ls *_out.cpp
   each file corresponds to only branch.
   currently no support for F6, unless
   setting nonzero values for the [only_branch] argument of every [switch],
   in order to indicate which branch one is interested in vizualizing. *)

open Optitrust
open Target

(* DEPRECATED

let _ = Run.script_cpp (fun () ->

  !! Label.add "r0" [cVarDef "a"];

  (* NOTE: manually comment/uncomment the ~only_branch argument
     - use only_branch to use the interactive mode
     - comment it out for generating all output programs *)
  Trace.sm1"witch ~only_branch:1 [
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


*)
