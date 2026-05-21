#include <optitrust_models.h>

__DECL(fold_float, "float * (float * float -> float) * int * int * int * (int -> float) -> float");
__AXIOM(fold_float_empty, "forall (neutral: float) (op: float * float -> float) (f: int -> float) -> neutral =. fold_float(neutral, op, 0, 0, 1, f)");
// TODO: check inequality constraints a >= b ? b % s != 0 ?
__AXIOM(fold_float_next, "forall (neutral: float) (op: float * float -> float) (a b s: int) (f: int -> float) -> fold_float(neutral, op, a, b, s, f) +. f(b) =. fold_float(neutral, op, a, b + s, s, f)");

__DEF(l_vmult, "fun (A B : int -> float) (n: int) -> fun (i: int) -> A(i) *. B(i)");
__DEF(l_sum, "fun (A : int -> float) (n: int) -> fold_float(0.f, (fun a b -> a +. b), 0, n, 1, A)");
// __DEF(logical_dotp, "fun (A B : int -> float) -> fold_float(0, 0, 1000, 1, fun i acc -> acc + A(i) * B(i))");
__DEF(l_dotp, "fun (A B : int -> float) -> l_sum(l_vmult(A, B, 1000), 1000)");


float dotp(float* a, float* b) {
  __requires("A: int -> float, B: int -> float");
  __reads("a ~> Matrix1(1000, A), b ~> Matrix1(1000, B)");
  __ensures("_Res =. l_dotp(A, B)");

  /* float f2;
  float f3;
  float f4; */
  int i2;
  float f5;
  float f6;
  float f7;
  /* float f8;
  float f9;
  int i3;
  int iarr; */
  float f10;

  f10 = 0.f;
  // &f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, 0, 1, fun k -> A(k) *. B(k))
  __ghost(rewrite_float_linear, "inside := fun v -> &f10 ~~> v, by := fold_float_empty(0.f, (fun a b -> a +. b), fun k -> A(k) *. B(k))");
  for (int i3 = 0; i3 < 1000; i3++) {
    __spreserves("&f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, i3, 1, fun k -> A(k) *. B(k))");

    i2 = MINDEX1(1000, i3);
    __GHOST_BEGIN(focusB, ro_matrix1_focus, "b, i3");
    f7 = b[i2];
    __GHOST_END(focusB);
    __GHOST_BEGIN(focusA, ro_matrix1_focus, "a, i3");
    f6 = a[i2];
    __GHOST_END(focusA);
    f5 = f6 * f7;
    // f10 = f5 + f10;
    f10 = f10 + f5;

    __ghost(rewrite_float_linear, "inside := fun v -> &f10 ~~> v, by := fold_float_next(0.f, (fun a b -> a +. b), 0, i3, 1, fun k -> A(k) *. B(k))");
  }
  // &f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, 1000, 1, fun k -> A(k) *. B(k))

  __ghost(eq_refl_float, "fold_float(0.f, (fun a b -> a +. b), 0, 1000, 1, fun k -> A(k) *. B(k))");
  return f10;
}

__DEF(l_crazy_vmult, "fun (A B : int -> float) (n: int) -> fun (i: int) -> A(i) *. B(0)");
__DEF(l_crazy_dotp, "fun (A B : int -> float) -> l_sum(l_crazy_vmult(A, B, 1000), 1000)");

float crazy_dotp(float* a, float* b) {
  __requires("A: int -> float, B: int -> float");
  __reads("a ~> Matrix1(1000, A), b ~> Matrix1(1000, B)");
  __ensures("_Res =. l_crazy_dotp(A, B)");

  /* float f2;
  float f3;
  float f4; */
  int i2;
  float f5;
  float f6;
  float f7;
  /* float f8;
  float f9;
  int i3;
  int iarr; */
  float f10;

  f10 = 0.f;

  __GHOST_BEGIN(focusB, ro_matrix1_focus, "b, 0");
  f7 = b[MINDEX1(1000, 0)];
  __GHOST_END(focusB);
  // &f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, 0, 1, fun k -> A(k) *. B(k))
  __ghost(rewrite_float_linear, "inside := fun v -> &f10 ~~> v, by := fold_float_empty(0.f, (fun a b -> a +. b), fun k -> A(k) *. B(0))");
  for (int i3 = 0; i3 < 1000; i3++) {
    __spreserves("&f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, i3, 1, fun k -> A(k) *. B(0))");

    i2 = MINDEX1(1000, i3);
    __GHOST_BEGIN(focusA, ro_matrix1_focus, "a, i3");
    f6 = a[i2];
    __GHOST_END(focusA);
    f5 = f6 * f7;
    // f10 = f5 + f10;
    f10 = f10 + f5;

    __ghost(rewrite_float_linear, "inside := fun v -> &f10 ~~> v, by := fold_float_next(0.f, (fun a b -> a +. b), 0, i3, 1, fun k -> A(k) *. B(0))");
  }
  // &f10 ~~> fold_float(0.f, (fun a b -> a +. b), 0, 1000, 1, fun k -> A(k) *. B(k))

  __ghost(eq_refl_float, "fold_float(0.f, (fun a b -> a +. b), 0, 1000, 1, fun k -> A(k) *. B(0))");
  return f10;
}
