
open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Function_basic.uninline ~fct:[cFunDef "f"] [cLabelBody "body"];
  )
"
void foo(int x);

void f(int x) {
  foo(x);
  foo(x);
}

int main() {
  body: {
    foo(3);
    foo(3);
  }
}
"


(* LATER: add a "compute" transformation to simplify
    - products of int
    - sums and products of doubles

   LATER: simplification recursively in atoms, see example of [w];
   to implement using trm_map. *)

let _ = Run.script_cpp (fun _ ->


    !! Function_basic.uninline ~fct:[cFunDef "gtwice"] [cLabelBody "gtwice_body"];
    !! Function_basic.uninline ~fct:[cFunDef "f"] [cLabelBody "fbody"];
    !! Function_basic.uninline ~fct:[cFunDef "iter_nat_for"] [cLabelBody "hobody"];
    !! Function_basic.uninline ~fct:[cFunDef "iter_bag"] [cLabelBody "bagbody"];

)

(* TODO: implement the combi version, see at the bottom of this file;
   LATER: we will improve the new_rule_match function so that it is not needed to
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
Pseudo code for rule_match




----------------
// Note in passing (for LATER)
// here we may want to call  Function.bind_arg ["","?"] [cFun "iter_nat_for"]
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


(* TODO:
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