
open Optitrust
open Target

(* Note: In C11, any parameter of function type is adjusted to the corresponding pointer type. Eg:
    int f(char g(double)); // declares int f(char ( *g )(double))
    int h(int(void)); // declares int h(int ( * )(void))
   We favor the lightweight notation on the left-hand side, to improve readability;
   in the future, we may want to introduce an annotation to allow preserving the presentation
  used by the original code in case it involves a star. *)

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  let tLabelSpan label = tSpan [tAfter; cLabel (label ^ "_start")] [tBefore; cLabel (label ^ "_end")] in
  !! Function_basic.uninline ~f:[cFunDef "gtwice"] [tLabelSpan "gtwice"];
  !! Function_basic.uninline ~f:[cFunDef "f"] [nbMulti; tLabelSpan "f"];
  !! Variable_basic.inline [nbMulti; cVarDef "r_pure"];

  !! Function_basic.uninline ~f:[cFunDef "loop_with_ret"] [tLabelSpan "loop_with_ret"];
  (* FIXME: validation for higher-order functions
    Flags.check_validity := false;
    Flags.recompute_resources_between_steps := false;
    !! Function_basic.uninline ~fct:[cFunDef "iter_nat_for"] [cLabel "hobody"];

    !! Function_basic.uninline ~fct:[cFunDef "iter_bag2"] [cLabel "bagbody2"];
    (* Test to undo the action of the unlining: *)
      !! Function_basic.inline [cCall "iter_bag2"];
      !! Function_basic.beta [cTopFunDef "test_bag2"; cFor_c ""; dBody; cFun""];


    !! Function_basic.uninline ~fct:[cFunDef "iter_bag"] [cLabel "bagbody"];
    (* LATER: bug if iter_bag uses variable name "it" instead of "iter", the variable
       is considered as non-const; maybe this will be fixed when encodings are reimplemented *)
    (* ARTHUR: the issue is related to the fact that the matching function does
       not currently check that the types in the pattern match the types in the term;
       thus, a "const int x" can match against an "int x", and this is an issue it seems *)
    (* Test to undo the action of the unlining: *)
      !! Function_basic.inline [cCall "iter_bag"];
      !! Function_basic.beta [cTopFunDef "test_bag"; cFor_c ""; dBody; cFun""];
      *)

)

(*
   LATER: we will improve the rule_match function so that it is not needed to
   introduce a sequence for matching "body(i)" against a list of instructions. *)

(* ==> notes, already implemented
----------------
UNIT TEST 1

Function_basic.uninline ~fct:[cFunDef "f"] [cLabel "test"]

// before:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  test:{
    int b = (r+2)+1;
    g(b, r+2);
  }
}

// after:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  test: {
    f(r+2);
  }
}

----------------
Implementation:

  essentially, we are calling [rule_match], with pattern the body of the function f:

    int a = x+1;
    g(a, x);

  and with term the contents of the targeted sequence:

    int b = (r+2)+1;
    g(b, r+2);

  and with the list of pattern variables ["x"], corresponding to the arguments of f.

  the list of pattern variables gets increased when we have a trm_let;
  for example, here we have "int a = ..." to match against "int b = ...",
  so we need to add "a" as additional pattern variables, and recall that it maps to "b";
  this way, we can later match occurences of "a" in the pattern with occurences of "b"
  in the processed term.

  At the end of rule_match, we have the following instantiation:
    x -> r+2
  // note that the binding (a -> b) is no longer visible when we exit the scope of the trm_seq.

  then, we look at the list of arguments of "f", namely the list ["x"],
  and we generate the function call [f(r+2)], because x is bound to [r+2].

  For all this to work, the current implementation of rule_match needs to be augmented
  with cases for trm_seq, trm_for, trm_let, etc.


----------------
UNIT TEST 2

Function_basic.uninline ~fct:[cFunDef "iter_nat_for"] [cFor "j"]

// before:

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

int main() {
  int s = 0;
  int m = 3;
  for (int j = 0; j < m; j++) {
    { // with a nested sequence for the body
      s += 2*j;
      s -= j;
    }
  }
}

// after: // note that this is not valid C syntax

int main() {
  int s = 0;
  int m = 3;
  iter_nat_for(m, void body(int j) {
      s += 2*j;
      s -= j;
    });
}


/// little exercise:
 // Function.inline iter_nat_for

int main() {
  int s = 0;
  int m = 3;
  for (int i = 0; i < m; i++) {
    (void body(int j) {
      s += 2*j;
      s -= j;
    })) (i);
  }



----------------
Implementation:

  To support the example above, we need one more feature: the resolution of the
  matching of "body(i)" against the sequence {s += 2*j; s -= j;}, where body
  is a pattern variable, and its arguments are (by hypothesis) a list of variables.

  We have a flag in rule_match to indicate that we are willing to instantiate
  a variable such as "body" with a function that we synthesis on the fly.

  This synthesized function is a trm_let_fun, with name "body",
  with arguments ["j"], and with body {s += 2*j; s -= j;}.

  The argument is ["j"] because we are matching "body(i)" in a context where "i"
  is bound to "j" (from the time we entered the scope of the for loop and matched
  "for i" against "for j").


----------------
// Note in passing (for LATER)
// here we may want to call  Function.bind_arg ["","?"] [cCall "iter_nat_for"]
//   where the "?" means that we should guess a good name to use for the
//   fresh variable; in case the argument is a trm_let_fun, we take the
//   existing name from that function. we get:

int main() {
  int s = 0;
  void body(int j) {
      s += 2*j;
      s -= j;
    });
  iter_nat_for(n, body);
}

*)


(*
----------------
At the combi level, the uninline operation should be able to automatically
perform a Sequence_intro, when given the target of the first instruction
of the body of the function

----------------
COMBI UNIT TEST 1

  Function.uninline ~fct:[cVarDef "f"] [cVarDef "b"]

// before:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  int b = (r+2)+1;
  g(b, r+2);
  int s = r;
}

// after:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  f(r+2);
  int s = r;
}

// The idea is that by looking at the body of the function [f], we see that we
// need 2 instructions, thus we can call  Sequence.intro   on the target [cVarDef "b"]
// with ~nb:2 and ~mark:m, for some fresh mark [m].
// Once this is done, we can call  Function_basic.uninline with the same function
// and with the target [cMark "m"].
// After this call, we can eliminate the sequence introduced.


*)
