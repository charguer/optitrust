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
  return x;
}

__ghost_ret exist_opposite() {
  __requires("x: int");
  __ensures("y: int");
  __ensures("__is_true(x + y == 0)");
  __admitted();
}

int gen_pythagorean() {
  __ensures("y: int");
  __ensures("z: int");
  __ensures("__is_true(_Res * _Res == y * y + z * z)");
  __ghost(assert_prop, "P := __is_true(5 * 5 == 4 * 4 + 3 * 3)");
  return 5;
}

void caller() {
  __pure();
  const int y = div_exact(6, 3);
  __with("q := 2");
  f();
  __with("k := 6");
  __ghost(assert_prop, "P := __is_true(2 + 2 == 4)");
  const int z = g(__call_with(div_exact(12, 2), "q := 6"));
  __ghost(exist_opposite, "x := 5", "a <- y");
  __ghost(assert_prop, "P := __is_true(5 + a == 0)");
  const int k = gen_pythagorean();
  __bind("i <- y, j <- z");
  __ghost(assert_prop, "P := __is_true(k * k == i * i + j * j)");
}

__ghost_ret dependant_proof() {
  __requires("P: int -> Prop");
  __requires("proof: forall (x: int) -> P(x)");
}

void dependant_test() {
  __requires("H: forall (x: int) (y: int) -> __is_true(x + y == y + x)");
  __ghost(assert_prop, "P := __is_true(2 + 3 == 3 + 2), proof := H(2, 3)");
  __ghost(dependant_proof,
          "P := fun x -> __is_true(x + 0 == 0 + x), proof := fun x -> H(x, 0)");
}
