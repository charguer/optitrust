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
  return x;
}

__GHOST(exist_opposite) {
  __requires("x: int");
  __ensures("y: int");
  __ensures("x + y = 0");
  __admitted();
}

int gen_pythagorean() {
  __ensures("y: int, z: int");
  __ensures("_Res * _Res = y * y + z * z");
  __ghost(assert_prop, "P := 5 * 5 = 4 * 4 + 3 * 3");
  return 5;
}

void caller() {
  __pure();
  // int x = div_exact(6, 3); // Typing fails when uncommented
  const int y = div_exact(6, 3); __with("q := 2");
  f(); __with("6");
  __ghost(assert_prop, "P := 2+2 = 4");

  const int z = g(__call_with(div_exact(12, 2), "q := 6"));

  __ghost(exist_opposite, "x := 5", "a <- y");
  __ghost(assert_prop, "P := 5 + a = 0");

  const int k = gen_pythagorean(); __bind("i, j");
  __ghost(assert_prop, "P := k * k = i * i + j * j");
}

__GHOST(dependant_proof) {
  __requires("P: int -> Prop, proof: forall (x:int) -> P(x)");
}

void dependant_test() {
  __requires("H: forall (x:int) (y:int) -> x + y = y + x");
  __ghost(assert_prop, "P := 2+3 = 3+2, proof := H(2, 3)");
  __ghost(dependant_proof, "P := fun x -> x + 0 = 0 + x, proof := fun (x:int) -> H(x, 0)");
  //should fail : __ghost(dependant_proof, "P := fun x -> x + 0 = 0 + x, proof := fun (x: int) -> H(0, x)");
}
