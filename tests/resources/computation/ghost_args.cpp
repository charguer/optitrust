#include <optitrust.h>

/*
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
*/

// Demo of promotion of equalities such as "g = v" into aliases "g := v" in postconditions.
// LATER: we would like the syntax "g := v" to be supported in pre/postconditions.
// CURRENT HACK: the function "compute_produced_resources" promotes equalities of the form "_Res = v";
// we only do it for _Res because if we do it for other equalities, we might introduce cycles in aliases.

__GHOST(eq_refl_int) {
  __pure();
  __requires("x:int");
  __ensures("proof: x = x");
  __admitted();
}

int test_alias_post(int r) {
  __requires("x : int");
  __requires("y : int");
  __requires("E: r = x + y - y");
  //__ensures("m : float");
  __ensures("_Res = x");
  __admitted(); // TODO: needs ghost rewrite
  //__DEF(m, "1.");
  return r;
}

int test_alias_post_call() {
  __ensures("2 = _Res"); // can be proved upon return if "a := 2" is in context.
  __ghost(eq_refl_int, "x := 2 + 3 - 3", "E <- proof");
  const int a = test_alias_post(2 + 3 - 3); __with("x := 2, y := 3, E := E"); //__bind("y");
  return a;
}
