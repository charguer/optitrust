#include <optitrust.h>

int div_exact(int a, int b) {
  __requires("q: int");
  __requires("proof: a = b * q");
  __ensures("_Res = q");
  __admitted();
  return a / b;
}

void f() {
  __requires("k: int");
}

int g(int x) {
  __pure();
  __admitted();
  return x;
}

__GHOST(any_proof) {
  __requires("P: Prop, proof: P");
}

void caller() {
  __pure();
  // int x = div_exact(6, 3); // Typing fails when uncommented
  int y = div_exact(6, 3); __with("q := 2, proof := __admitted");
  f(); __with("6");
  __ghost(any_proof, "P := 2+2 = 4");

  int z = g(__call_with(div_exact(12, 2), "q := 6, proof := __admitted"));
}

__GHOST(dependant_proof) {
  __requires("P: int -> Prop, proof: forall (x:int) -> P(x)");
}

void dependant_test() {
  __requires("H: forall (x:int) (y:int) -> x + y = y + x");
  __ghost(any_proof, "P := 2+3 = 3+2, proof := H(2, 3)");
  __ghost(dependant_proof, "P := fun x -> x + 0 = 0 + x, proof := fun (x:int) -> H(x, 0)");
  //should fail : __ghost(dependant_proof, "P := fun x -> x + 0 = 0 + x, proof := fun (x: int) -> H(0, x)");
}
