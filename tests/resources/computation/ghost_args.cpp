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
