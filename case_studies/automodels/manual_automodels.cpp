#include <optitrust_models.h>

//---------------------------------------
// auxiliary function "incr"

__DECL(Fincr, "int -> int");
// not revealed: __DEF(Fincr, "fun (i:int) -> i+1");

void incr(int* p) {
  __consumes("p ~~> n");
  __produces("p ~~> Fincr(n)");
  __admitted();
  p = *p + 1;
}

//---------------------------------------
// function to typecheck

void f_raw(int* p, int n) {
  for (int i = 0; i < n; i++) {
    int a = *p;
    p = a + 1;
  }
  incr(p);
}

//---------------------------------------
// function with permissions

void f_raw(int* p, int n) {
  __modifies("p ~> Cell");
  for (int i = 0; i < n; i++) {
    __modifies("p ~> Cell");
    const int a = *p;
    p = a + 1;
  }
  incr(p);
}

//---------------------------------------
// function with automodels, written by hand

__DECL(Fp, "int -> int"); // output contents of "p" in terms of the input models of "f"

void f(int* p, int n) {
  __requires("P0: int"); // auto-quantify input contents of "p"
  __consumes("p ~~> P0"); // refines __consumes("p ~> Cell");
  __produces("p ~~> Fp(P0)"); // refines __produces("p ~> Cell");
                              // and auto-specify output contents of "p" as "Fp" of its input contents

  __DECL(Gp, "int -> int"); // Gp describes the "evolution" of the contents of "p" at each iteration
  __DECL(P1, "int -> int"); // P1(i) describes the contents of "p" at iteration i

  for (int i = 0; i < n; i++) {
    // loop invariant, resources:
    __spreserves("p ~~> P1(i)");
    // loop invariant, pure part:
    __spreserves("E0: P0 = P1(0)");
    __spreserves("ES: forall j in 0..i -> P1(j+1) = Gp(P1(j))");

    const int a = *p;  // yields the equality "a = P1(i)", which we keep track of
    __DEF(A, "P1(i)"); // by defining "A := P1(i)" and subsequently using "A" instead of "a"

    p = a + 1; // now "p" has contents "A + 1", which we name P2.
    __DEF(P2, "A + 1");  // here we thus have: p ~~> P2

    // Now reaching the end of the loop. We include two equalities, to reflect
    // the fact that the current contents of "p" (which is P2, and also equal to P1(i) + 1)
    //  is described both by "P1(i+1)" and "Gp(P1(i))".
    __ghost(assume, "P1(i+1) = P2");
    __ghost(assume, " Gp(P1(i)) = P2");
    // by transitivity, we can deduce P1(i+1) = Gp(P1(i)), thereby stepping the invariant
    // TODO: __ghost(eq_trans, "Gp(P1(i)) = P2 = Gp(P1(i))");


    // Note: the loop body could be transformed by any code that preserves Gp(P1(i)) = P1(i) + 1

  }
  __DEF(P3, "P1(n)"); // contents of "p" after exiting the loop
   // in other words, we here have:   p ~~> P3

   // Note: one could replace the whole loop with "p += n", and prove the optimization
   // correct by means of an induction using "for j in 0..n -> P1(j+1) = P1(j) + 1"
   // and the base case "P1(0) = P0", thereby proving "P3 = P1(n) = P1(0) + n = P0 + n".
   // This optimization can be carried out even though the original code did not include
   // a function correctness proof.

  __ghost(assume, "Fp(P0) = P3"); // this "assume" effectively defines "Fp".
}





(
