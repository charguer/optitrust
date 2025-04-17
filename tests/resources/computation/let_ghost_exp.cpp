#include <optitrust.h>

__ghost(define, "x := fun (x: int) -> 3 + x", "three_plus <- x");

__ghost(define, "T := int -> int, x := fun x -> x + 2", "plus_two <- x");

int five() {
  __ensures("__is_true(_Res == three_plus(2))");
  __ensures("__is_true(_Res == plus_two(3))");
  return 3 + 2;
}

__ghost(assert_prop, "P := __is_true(0 == 0)", "eq_refl_0 <- proof");

__ghost(assert_inhabited, "x := arbitrary(int -> Prop)", "P <- x");

__ghost(assert_prop, "proof := admit(P(0))", "P0 <- proof");

__ghost(assert_prop, "proof := admit(forall (n: int) (_: P(n)) -> P(n + 1))",
        "PS <- proof");

__ghost(assert_prop, "proof := PS(0, P0)", "P0_1 <- proof");

__ghost(assert_prop, "P := P(0 + 1 + 1), proof := PS(0 + 1, P0_1)",
        "P0_1_1 <- proof");

void local_ghosts() {
  __ensures("P(4 + 1)");
  __ghost(define, "x := fun (n: int) -> P(n + 1)", "Q <- x");
  __ghost(assert_prop, "proof := admit(Q(4))", "Q4 <- proof");
}
