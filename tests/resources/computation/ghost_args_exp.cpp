#include <optitrust.h>

int div_exact(int a, int b) {
  __requires("q: int");
  __requires("proof: __is_true(a == b * q)");
  __ensures("__is_true(_Res == q)");
  __admitted();
  return a / b;
}

void f() { __requires("k: int"); }

int g(int x) {
  __pure();
  __admitted();
  return x;
}

__ghost_ret any_proof() {
  __requires("P: Prop");
  __requires("proof: P");
}

void caller() {
  __pure();
  int y = div_exact(6, 3);
  __with("q := 2, proof := __admitted");
  f();
  __with("k := 6");
  __ghost(any_proof, "P := __is_true(2 + 2 == 4)");
  int z = g(__call_with(div_exact(12, 2), "q := 6, proof := __admitted"));
}

__ghost_ret dependant_proof() {
  __requires("P: int -> Prop");
  __requires("proof: forall (x: int) -> P(x)");
}

void dependant_test() {
  __requires("H: forall (x: int) (y: int) -> __is_true(x + y == y + x)");
  __ghost(any_proof, "P := __is_true(2 + 3 == 3 + 2), proof := H(2, 3)");
  __ghost(dependant_proof,
          "P := fun x -> __is_true(x + 0 == 0 + x), proof := fun x -> H(x, 0)");
}
