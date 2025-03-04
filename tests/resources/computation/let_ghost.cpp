#include <optitrust.h>

__DEF(three_plus, "fun (x: int) -> 3 + x");
__DEF_TYPED(plus_two, "int -> int", "fun x -> x + 2");

int five() {
  __ensures("_Res = three_plus(2)");
  __ensures("_Res = plus_two(3)");
  return 3 + 2;
}

__ASSERT(eq_refl_0, "0 = 0");
__DECL(P, "int -> Prop");
__AXIOM(P0, "P(0)");
__AXIOM(PS, "forall (n: int) (proof: P(n)) -> P(n+1)");
__PROOF(P0_1, "PS(0, P0)");
__PROOF_OF(P0_1_1, "P(0+1+1)", "PS(0+1, P0_1)");
// LATER: __PROOF_OF(P1, "P(1)", "rewrite(0+1, 1, P, _, P0_1)");

void local_ghosts() {
  __ensures("P(4+1)");
  __DEF(Q, "fun (n: int) -> P(n+1)");
  __AXIOM(Q4, "Q(4)");
}

// Should fail: Q not in environment
//__AXIOM(Q6, "Q(6)");
