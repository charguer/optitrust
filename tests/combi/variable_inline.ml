open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

   show [cVarDef "p"];
   !! Variable.inline [cVarDef "p"];
)


(*
  Documentation at basic level of Variable.unfold
    Applies to a definition [T a = e;]
    Fails if the definition is not a const or a reference.
    Replaces [a] with [e] at the desired targets (or everywhere).


  Documentation at combi level of Variable.unfold

  1)   T& a = e;
       Replace all occurences of a with e  (in the scope).
       (This transformation is always correct, no matter
       if T is const or not.)

  2)   T const a = e;
       Replace all occurences of a with e  (in the scope).
       (This transformation is correct if e makes no side effects,
        else it's more complicated; it's user responsability.)

  3)   T a = e;
       We call Variable.to_const beforehand, to make it "T const a = e"
       (This works if variable a is not modified after its definition,
       else conversion to const would fail.)

  Documentation at combi level of Variable.inline
    It is Variable.unfold + delete declaration
*)

(* TODO For the unit tests

    int a = 3;
    int& b = a;  // unfold or inline b
    int r = a + b;

    int a = 3; // basic.unfold should fail, combi.unfold should succeed
    int r = a + a;

    int a = 3; // combi.unfold should fail (because to_const fails)
    a = 3;
    int r = a + a;

*)