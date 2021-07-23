open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* TODO: generalize to replace expressiosn,
     not necessarily instructions
        !! Generic.replace "b" [nbMulti; cVar "a"];
     implem does not need to look at surrounding sequence. *)
  !! Generic.change_occurrence "b" [nbMulti; cVar "a"];
  !! Generic.change_occurrence "f1" [cFun "f"; cVar "f"];
)
(* TODO: try
      !! Generic.replace "z" [nbMulti; cFunDef "f"; cArg "x"];
      ==> rename an argument and all of its occurences
*)

(* This is an instance of "replace"
    Generic.replace "b" [nbMulti; cVar "a"]
  *)

(* Variable.rename ~list:[("a","b")] [cTopFun "main"] *)

(* TODO:

  Variable.rename ~func:(fun s -> s ^ "1") [cSeq()]
  Variable.rename ~list:[("a","a1")] [cSeq()]  ==> implemented in terms for ~func
   { int a = x+0; f(a); }
   -->
   { int a1 = x+0; f(a1); }


  Sequence.iter (fun i tg_i -> (* tg_i is an explicit path of type target *)
    Variable.rename ~func:(fun s -> s ^ string_of_int i) tg_i
    ) [cLabel "foo"; dBody] // targeting the cSeq

  foo: {
   { int a = x+0; f(a); }
   { int a = x+1; f(a); }
  }
  -->
  foo: {
   { int a1 = x+0; f(a1); }
   { int a2 = x+1; f(a2); }
  }

*)