#include <optitrust_models.h>

//---------------------------------------
// auxiliary function "incr"

__DECL(Fincr, "int -> int");
// not revealed: __DEF(Fincr, "fun (i:int) -> i+1");

void incr(int* p) {
  __consumes("p ~~> n");
  __produces("p ~~> Fincr(n)");
  __admitted();
  *p = *p + 1;
}

//---------------------------------------
// function to typecheck

void f_raw(int* p, int n) {
  for (int i = 0; i < n; i++) {
    int a = *p;
    *p = a + 1;
  }
  incr(p);
}

//---------------------------------------
// function with permissions

void f_raw(int* p, int n) {
  __modifies("p ~> Cell");
  for (int i = 0; i < n; i++) {
    // __smodifies("p ~> Cell"); // optionnel
    const int a = *p;
    *p = a + 1;
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

  __DECL(Gp, "int -> int -> int"); // Gp describes the "evolution" of the contents of "p" at each iteration

  __DECL(P1, "int -> int"); // P1(i) describes the contents of "p" at iteration i
  __AXIOM(P1(0) = P0);
  __ghost(axiom, "P1(i+1) = P1(i) + 1"); // voir ces axiomes comme des defs
   // habité car il y a un code terminant qui exhibe
   // cas général: forall j in .., exists x y, tq.
    //    F(P1(j), x,p2 ) /\ G(x,y) /\  H(y,i,p2,P1(j+1))

      // p ~~> P1(j)
    const int x = f(p)
      // p ~~> p2  * [F(P1(j), x, p2)]
    const int y = g(x)
      // p~~> p2  * [ G(x,y)]
    h(y,p)
      // p~~> P1(j+1) * [H(y,p2,P1(j+1))]

  for (int i = 0; i < n; i++) {
    // pour l'optim:
     //   rajouter l'invariant : forall j in 0..i, P1(j) = P0+j

    // loop invariant, resources:
    __spreserves("p ~~> P1(i)");
    // loop invariant, pure part:

      // TODO: essayer sans ça:
    __spreserves("ES: forall j in 0..i -> P1(j+1) = Gp(j, P1(j))");

     // forme générale: A(P1(j+1), x )  | B(x,y)  |  C(y,i,P1(j))

    // __xensures("ES: P1(i+1) = Gp(i, P1(i))"); // probablement équivalent

    const int a = *p;  // yields the equality "a = P1(i)", which we keep track of
    // on aura gratuitement
        __DEF(A, "P1(i)"); // by defining "A := P1(i)" and subsequently using "A"   instead of "a"
        __axiom("a = P1(i)"); // ou bien :

    *p = a + 1; // now "p" has contents "A + 1", which we name P2.
    // on aura peut être gratuitement  p ~~> a + 1   ou (via alias) p ~~> P1(i) + 1
        __DEF(P2, "A + 1");  // here we thus have: p ~~> P2

    // Now reaching the end of the loop. We include two equalities, to reflect
    // the fact that the current contents of "p" (which is P2, and also equal to P1(i) + 1)
    //  is described both by "P1(i+1)" and "Gp(P1(i))".
    __ghost(assume, "P1(i+1) = P1(i) + 1");

    // deprecated __ghost(assume, "P1(i+1) = P2");
    // deprecated __ghost(assume, " Gp(P1(i)) = P2");

    // by transitivity, we can deduce P1(i+1) = Gp(P1(i)), thereby stepping the invariant
    // TODO: __ghost(eq_trans, "Gp(P1(i)) = P2 = Gp(P1(i))");


    // Note: the loop body could be transformed by any code that preserves Gp(P1(i)) = P1(i) + 1

  }
  // après avoir renforcé l'invariant de boucle, tu peux ajouter un assume
  //  car on a le droit d'ajouter des assumes/assert dérivables des autres
  //   assume (P1(n) = P0+n)
   // je remplace la boucle par
   // *p = p+3
   // ça me donne un P3 égal.

  __DEF(P3, "P1(n)"); // contents of "p" after exiting the loop
   // in other words, we here have:   p ~~> P3

   incr(p);
   __DEF(P4, "Fincr(P3)");

  __ghost(assume, "Fp(P0) = P4"); // this "assume" effectively defines "Fp".
}

(
  // Note: one could replace the whole loop with "p += n", and prove the optimization
  // correct by means of an induction using "for j in 0..n -> P1(j+1) = P1(j) + 1"
  // and the base case "P1(0) = P0", thereby proving "P3 = P1(n) = P1(0) + n = P0 + n".
  // This optimization can be carried out even though the original code did not include
  // a function correctness proof.

//---------------------------------------
// function to typecheck

void f_opt(int* p, int n) {
  *p += n;
  incr(p);
}


f1() {
  assume(P)
}
--> g()


 proof(H: P->Q)


f2() {
  assume(Q)
}
--> g()

--------
f_shape() {
  spec_shapes
}
--
f_automodel() {
   spec
   assume(P)
}
--
f2() {
  spec
  assume(Q)
  instr1
}
--
f3() {
  spec
  assume(Q)
  instr2
}


---------------
mq: le code de f2() est une compilation valide du code de f_shape

nouveau: les étapes de "assume dérivables" font maintenant partie
de la base de confiance

espoir: que le code des transfos ne soit pas dans la base de confiance
seulement le typeur et les étapes de changement de assume

---------------

---------------

f()  =>  F()

g()  =>  G()

--


f1()  =>  F1()
f2()  =>  F2()
f3()  =>  F3()

---------------