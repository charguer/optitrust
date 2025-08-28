#ifndef OPTITRUST_COMMON_H
#define OPTITRUST_COMMON_H

#include <optitrust_intrinsics.h>

/* ---- Primitive for ghost pairs ---- */

inline __ghost_fn __ghost_begin(__ghost_fn, __ghost_args = "", __ghost_bind = "") { return __admitted; }
inline void __ghost_end(__ghost_fn) {}

#define __GHOST_BEGIN(rev_ghost, ...) const __ghost_fn rev_ghost = __ghost_begin(__VA_ARGS__)
#define __GHOST_END(rev_ghost) __ghost_end(rev_ghost)

inline __ghost_fn __with_reverse(__ghost_fn g, __ghost_fn g_rev) { return g; }
inline void __reverts(__ghost_fn) {}

#define __GHOST_BEGIN_CUSTOM(rev_ghost, forward_ghost, backward_ghost) __GHOST_BEGIN(rev_ghost, __with_reverse(forward_ghost, backward_ghost))

inline __ghost_ret __clear(__ghost_args) {} // primitive to remove a pure fact from the context

/* ---- Adding resources to context and checking assertions ---- */

__GHOST(assert_inhabited) {
  /* Both names x are shared with the define intrinsic */
  __requires("T: Type, x: T");
  __ensures("x: T");
}
#define __DECL(name, type) __ghost(assert_inhabited, "x:=arbitrary(" type ")", #name "<-x")

inline __ghost_ret define() {} // need to be builtin until we handle aliases in post-conditions
#define __DEF(name, def) __ghost(define, "x:=" def, #name "<-x")
#define __DEF_TYPED(name, type, def) __ghost(define, "T:=" type ", x:=" def, #name "<-x")

// asserts that a property is true to generate a new pure proposition
__GHOST(assert_prop) {
  __requires("P: Prop, proof: P");
  __ensures("proof: P");
}

#define __ASSERT(name, prop) __ghost(assert_prop, "P:=" prop, #name "<-proof")
#define __PROOF_OF(name, prop, proof) __ghost(assert_prop, "P:=" prop ", proof:=" proof, #name "<-proof")
#define __PROOF(name, proof) __ghost(assert_prop, "proof:=" proof, #name "<-proof")
#define __AXIOM(name, prop) __PROOF(name, "admit(" prop ")")
// LATER: classify admit: user_admit(P), trust_transfo(P), tactic("tac"), interactive(P)

__GHOST(assert_eq) {
  /* Names x and y are used in assert_alias code */
  __requires("x: int, y: int, eq: x = y");
  // __ensures("eq: x = y"); // FIXME: We should probably remember the equality fact for later, but this currently breaks the ability to join after a pure fission.
}

inline __ghost_ret assert_alias() {} // need to be builtin until we handle aliases in post-conditions

__GHOST(rewrite_prop) {
  __requires("from: int, to: int");
  __requires("inside: int -> Prop");
  __requires("by: from = to");
  __requires("inside(from)");
  __ensures("inside(to)");
  __admitted();
}

__GHOST(rewrite_linear) {
  __requires("from: int, to: int");
  __requires("inside: int -> HProp");
  __requires("by: from = to");
  __consumes("inside(from)");
  __produces("inside(to)");
  __admitted();
}

__GHOST(rewrite_float_linear) {
  __requires("from: float, to: float");
  __requires("inside: float -> HProp");
  __requires("by: from =. to");
  __consumes("inside(from)");
  __produces("inside(to)");
  __admitted();
}

__GHOST(eq_refl_float) {
  __requires("x:float");
  __ensures("x =. x");
  __admitted();
}



// FIXME: Explain here why this is useful and can it be used in formulas ?
// assumes a formula with no need to prove it later
__GHOST(assume) {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

// FIXME: Why not simply using assert_prop with proof left to be inferred ?
// defers proving a formula to later
__GHOST(to_prove) {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

/* ---- Pure matrix functions ---- */

inline int MINDEX0() {
  return 0;
}

inline int MINDEX1(int N1, int i1) {
  return i1;
}

inline int MINDEX2(int N1, int N2, int i1, int i2) {
  return i1 * N2 + i2;
}

inline int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N3 + i3;
}

inline int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

inline size_t MSIZE0() {
  return 1;
}

inline size_t MSIZE1(int N1) {
  return (size_t)N1;
}

inline size_t MSIZE2(int N1, int N2) {
  return (size_t)N1 * (size_t)N2;
}

inline size_t MSIZE3(int N1, int N2, int N3) {
  return (size_t)N1 * (size_t)N2 * (size_t)N3;
}

inline size_t MSIZE4(int N1, int N2, int N3, int N4) {
  return (size_t)N1 * (size_t)N2 * (size_t)N3 * (size_t)N4;
}

#define MALLOC(T) (T*) malloc(sizeof(T))
#define MALLOC0(T) (T*) malloc(MSIZE0() * sizeof(T))
#define MALLOC1(T, N1) (T*) malloc(MSIZE1(N1) * sizeof(T))
#define MALLOC2(T, N1, N2) (T*) malloc(MSIZE2(N1, N2) * sizeof(T))
#define MALLOC3(T, N1, N2, N3) (T*) malloc(MSIZE3(N1, N2, N3) * sizeof(T))
#define MALLOC4(T, N1, N2, N3, N4) (T*) malloc(MSIZE4(N1, N2, N3, N4) * sizeof(T))

#define CALLOC0(T) (T*) calloc(MSIZE0(), sizeof(T))
#define CALLOC1(T, N1) (T*) calloc(MSIZE1(N1), sizeof(T))
#define CALLOC2(T, N1, N2) (T*) calloc(MSIZE2(N1, N2), sizeof(T))
#define CALLOC3(T, N1, N2, N3) (T*) calloc(MSIZE3(N1, N2, N3), sizeof(T))
#define CALLOC4(T, N1, N2, N3, N4) (T*) calloc(MSIZE4(N1, N2, N3, N4), sizeof(T))


/* ---- Arithmetic Functions ---- */

inline int exact_div(int n, int b) {
  __pure();
  __admitted();
  return n / b;
}

inline int min(int a, int b) {
  __pure();
  __admitted();
  return a < b ? a : b;
}

inline int max(int a, int b) {
  __pure();
  __admitted();
  return a > b ? a : b;
}

/* ---- Other Functions ---- */

inline int ANY(int maxValue) { return 0; }

/* ---- Ghosts to manipulate heap propositions ---- */

__DECL(Wand, "HProp * HProp -> HProp");

__GHOST(close_wand) {
  /* LATER: Replace that id with a generated name on the respective open */
  __requires("H1: HProp, H2: HProp");
  __consumes("Wand(H1, H2), H1");
  __produces("H2");
  __admitted();
}

__GHOST(hide) {
  __requires("H: HProp");
  __consumes("H");
  __ensures("H2: HProp");
  __produces("Wand(H2, H), H2");
  __admitted();
}

__GHOST(hide_rev) {
  __reverts(hide);
  __ghost(close_wand);
}

__GHOST(wand_simplify) {
  __requires("H1: HProp, H2: HProp, H3: HProp");
  __consumes("Wand(H1, H2), Wand(H2, H3)");
  __produces("Wand(H1, H3)");
  __admitted();
}

__GHOST(assert_hprop) {
  /* Name H is used in forget_init code */
  __requires("H: HProp");
  __consumes("H");
  __produces("H");
}

inline __ghost_ret forget_init() {} // need to be builtin to rewrite H into its uninit counterpart

/* ---- In Range ---- */

__DECL(in_range, "int * Range -> Prop");
__DECL(is_subrange, "Range * Range -> Prop");
__DECL(range_count, "Range -> int");

__GHOST(in_range_extend) {
  __requires("x: int, r1: Range, r2: Range");
  __requires("in_range(x, r1), is_subrange(r1, r2)");
  __ensures("in_range(x, r2)");
  __admitted();
}

__GHOST(in_range_shift) {
  __requires("x: int, k: int, a: int, b: int, s: int");
  __requires("in_range(x, range(a, b, s))");
  __ensures("in_range(x+k, range(a+k, b+k, s))");
  __admitted();
}

__GHOST(in_range_shift_extend) {
  __requires("x: int, k: int, r: Range, a: int, b: int, s: int");
  __requires("in_range(x, range(a, b, s))");
  __requires("is_subrange(range(a+k, b+k, s), r)");
  __ensures("in_range(x+k, r)");
  __admitted(); // for efficiency
  __ghost(in_range_shift, "x, k, a, b, s");
  __ghost(in_range_extend, "x+k, range(a+k, b+k, s), r");
}

__GHOST(in_range_bounds) {
  __requires("x: int, a: int, b: int, s: int");
  __requires("in_range(x, range(a, b, s)), s >= 0");
  __ensures("lower_bound: x >= a, upper_bound: x <= b");
  __admitted();
}

/* ---- Manually split RO resources ---- */

__GHOST(ro_split2) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f/2, H), _RO(f/2, H)");
  __admitted();
}

__GHOST(ro_split3) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f/3, H), _RO(f/3, H), _RO(f/3, H)");
  __admitted();
}

__GHOST(ro_split4) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f/4, H), _RO(f/4, H), _RO(f/4, H), _RO(f/4, H)");
  __admitted();
}

__GHOST(ro_allow_join2) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f/2, H)");
  __produces("_RO(f - f/2, H)");
  __admitted();
}

__GHOST(ro_allow_join3) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f/3, H)");
  __produces("_RO(f - f/3 - f/3, H)");
  __admitted();
}

__GHOST(ro_allow_join4) {
  __requires("f: _Fraction, H: HProp");
  __consumes("_RO(f/4, H)");
  __produces("_RO(f - f/4 - f/4 - f/4, H)");
  __admitted();
}

/* ---- Group Ghosts ---- */

__GHOST(ro_fork_group) {
  __requires("f: _Fraction, H: HProp, r: Range");
  __consumes("_RO(f, H)");
  // TODO: can we write: for _ in r -> _RO(f / range_count(r), H) ?
  __produces("_RO(f / range_count(r), for _ in r -> H)");
  __admitted();
}

__GHOST(ro_join_group) {
  __reverts(ro_fork_group);
  __admitted();
}

__GHOST(swap_groups) {
  __requires("items: int * int -> HProp, inner_range: Range, outer_range: Range");
  __consumes("for i in outer_range -> for j in inner_range -> items(i,j)");
  __produces("for j in inner_range -> for i in outer_range -> items(i,j)");
  __admitted();
}

__GHOST(swap_groups_rev) {
  __reverts(swap_groups);
  __admitted();
  // Buggy because reusing var ids...
  //__ghost(swap_groups, "items := fun i,j -> items(j,i)");
}

__GHOST(ro_swap_groups) {
  __requires("items: int * int -> HProp, inner_range: Range, outer_range: Range, f: _Fraction");
  __consumes("_RO(f, for i in outer_range -> for j in inner_range -> items(i,j))");
  __produces("_RO(f, for j in inner_range -> for i in outer_range -> items(i,j))");
  __admitted();
}

__GHOST(ro_swap_groups_rev) {
  __reverts(ro_swap_groups);
  __admitted();
}

__GHOST(tiled_index_in_range) {
  __requires("tile_index: int, index: int");
  __requires("tile_count: int, tile_size: int, size: int");
  __requires("div_check: size = tile_count * tile_size");
  __requires("in_range(tile_index, 0..tile_count)");
  __requires("in_range(index, 0..tile_size)");
  __ensures("in_range(tile_index * tile_size + index, 0..size)");
  __admitted();
}

__AXIOM(eq_sym, "forall (m n: int) (eq: m = n) -> n = m");
__AXIOM(zero_mul_intro, "forall (n: int) -> 0 = 0 * n");
__AXIOM(plus_zero_intro, "forall (n: int) -> n = n + 0");
__AXIOM(add_assoc_right, "forall (m n p: int) -> (m + n) + p = m + (n + p)");
__AXIOM(mul_add_factor, "forall (m n: int) -> m * n + n = (m + 1) * n");

__GHOST(tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> HProp,"
    "div_check: size = tile_count * tile_size,"
    "positive_tile_size: tile_size >= 0"
  );
  __consumes("Group(0..size, items)");
  __produces("for bi in 0..tile_count ->"
               "for i in 0..tile_size -> items(bi * tile_size + i)");
  __admitted();
}

__GHOST(untile_divides) {
  __reverts(tile_divides);
  __admitted();
}

__GHOST(ro_tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> HProp,"
    "div_check: size = tile_count * tile_size,"
    "positive_tile_size: tile_size >= 0,"
    "f: _Fraction"
  );
  __consumes("_RO(f, Group(0..size, items))");
  __produces("_RO(f, for bi in 0..tile_count ->"
               "for i in 0..tile_size -> items(bi * tile_size + i))");
  __admitted();
}

__GHOST(ro_untile_divides) {
  __reverts(ro_tile_divides);
  __admitted();
}

__GHOST(group_collapse) {
  __requires("n: int, m: int, items: (int * int) -> HProp");
  __consumes("for i in 0..n -> for j in 0..m -> items(i, j)");
  __produces("for ij in 0..(n * m) -> items(ij / m, ij % m)");
  __admitted();
}

__GHOST(group_uncollapse) {
  __reverts(group_collapse);
  __admitted();
}

__GHOST(ro_group_collapse) {
  __requires("n: int, m: int, items: (int * int) -> HProp, "
             "f: _Fraction");
  __consumes("_RO(f, for i in 0..n -> for j in 0..m -> items(i, j))");
  __produces("_RO(f, for ij in 0..(n * m) -> items(ij / m, ij % m))");
  __admitted();
}

__GHOST(ro_group_uncollapse) {
  __reverts(ro_group_collapse);
  __admitted();
}

__GHOST(group_focus) {
  __requires("i: int, range: Range, items: int -> HProp");
  __requires("bound_check: in_range(i, range)");
  __consumes("Group(range, items)");
  __produces("Wand(items(i), Group(range, items)), items(i)");
  __admitted();
}

__GHOST(group_unfocus) {
  __reverts(group_focus);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

__GHOST(ro_group_focus) {
  __requires("i: int, range: Range, items: int -> HProp, f: _Fraction");
  __requires("bound_check: in_range(i, range)");
  __consumes("_RO(f, Group(range, items))");
  __produces("Wand(_RO(f, items(i)), _RO(f, Group(range, items))), _RO(f, items(i))");
  __admitted();
}

__GHOST(ro_group_unfocus) {
  __reverts(ro_group_focus);
  __admitted(); // for performance
  __ghost(close_wand);

}

__GHOST(ro_group2_focus) {
  __requires("i: int, r: Range, r2: Range, items: int * int -> HProp, f: _Fraction");
  __requires("bound_check: in_range(i, r)");
  __consumes("_RO(f, for i2 in r2 -> for i in r -> items(i2, i))");
  __produces("Wand(_RO(f, for i2 in r2 -> items(i2, i)),"
                  "_RO(f, for i2 in r2 -> for i in r -> items(i2, i)))");
  __produces("_RO(f, for i2 in r2 -> items(i2, i))");
  __admitted();
}

__GHOST(ro_group2_unfocus) {
  __reverts(ro_group2_focus);

  __ghost(close_wand);
}

__GHOST(group_focus_subrange) {
  __requires("sub_range: Range, big_range: Range, items: int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("Group(big_range, items)");
  __produces("Wand(Group(sub_range, items), Group(big_range, items)), Group(sub_range, items)");
  __admitted();
}

__GHOST(group_unfocus_subrange) {
  __reverts(group_focus_subrange);
  __ghost(close_wand);
}

__GHOST(ro_group_focus_subrange) {
  __requires("sub_range: Range, big_range: Range, items: int -> HProp, f: _Fraction");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_RO(f, Group(big_range, items))");
  __produces("Wand(_RO(f, Group(sub_range, items)), _RO(f, Group(big_range, items)))");
  __produces("_RO(f, Group(sub_range, items))");
  __admitted();
}

__GHOST(ro_group_unfocus_subrange) {
  __reverts(ro_group_focus_subrange);
  __ghost(close_wand);
}

__GHOST(group2_focus_subrange) {
  __requires("outer_range: Range, sub_range: Range, big_range: Range, items: int -> int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("for i in outer_range -> Group(big_range, items(i))");
  __produces("Wand("
      "for i in outer_range -> Group(sub_range, items(i)),"
      "for i in outer_range -> Group(big_range, items(i))"
    "),"
    "for i in outer_range -> Group(sub_range, items(i))");

  __admitted();
  /* FIXME: This ghost should not be needed but to circumvent it we would have to write:
  __with_reverse(
    [&]() {
      __consumes("for i in 0..10 -> for j in 0..10 -> for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
      __produces("for i in 0..10 -> for j in 2..10 -> for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
      __produces("for i in 0..10 -> Wand("
                 "  for j in 2..10 -> for k in 0..4 ->"
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell,"
                 "  for j in 0..10 -> for k in 0..4 ->"
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell"
                 ")");
      for (int i = 0; i < 10; i++) {
        __xconsumes("for j in 0..10 -> for k in 0..4 ->"
                    "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
        __xproduces("for j in 2..10 -> for k in 0..4 ->"
                    "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
        __xproduces("Wand("
                    "  for j in 2..10 -> for k in 0..4 ->"
                    "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell,"
                    "  for j in 0..10 -> for k in 0..4 ->"
                    "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell"
                    ")");
        __ghost(group_focus_subrange,
          "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell, "
          "start := 2, stop := 10, step := 1");
      }
    },
    [&]() {
      for (int i = 0; i < 10; i++) {
        __xproduces("for j in 0..10 -> for k in 0..4 ->"
                    "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
        __xconsumes("for j in 2..10 -> for k in 0..4 ->"
                    "  &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
        __xconsumes("Wand("
                    "  for j in 2..10 -> for k in 0..4 ->"
                    "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell,"
                    "  for j in 0..10 -> for k in 0..4 ->"
                    "    &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell"
                    ")");
        __ghost(group_unfocus_subrange, "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell");
      }
    }
  )
  */
}

__GHOST(group2_unfocus_subrange) {
  __reverts(group2_focus_subrange);
  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_unfocus_subrange, ...);
  } */
}

/* --- group_shift and nested variants: */

__GHOST(group_shift) {
  __requires("start: int, stop: int, step: int, items: int -> HProp");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __consumes("for i in range(start, stop, step) -> items(i)");
  __produces("for i in range(new_start, new_stop, step) -> items(i - shift)");
  __admitted();
}

__GHOST(group_unshift) {
  __reverts(group_shift);
  __admitted();
}

__GHOST(ro_group_shift) {
  __requires("start: int, stop: int, step: int, items: int -> HProp");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__GHOST(ro_group_unshift) {
  __reverts(ro_group_shift);
  __admitted();
}

/* --- group_scale and nested variants: */

__GHOST(group_scale) {
  __requires("stop: int, step: int, items: int -> HProp");
  __requires("factor: int, new_step: int, new_stop: int");
  __requires("check_stop: new_stop = factor * stop, check_step: new_step = factor * step");
  __requires("check_factor: factor <> 0");
  __consumes("for i in range(0, stop, step) -> items(i)");
  __produces("for i in range(0, new_stop, new_step) -> items(exact_div(i, factor))");
  __admitted();
}

__GHOST(group_unscale) {
  __reverts(group_scale);
  __admitted();
}

__GHOST(ro_group_scale) {
  __requires("stop: int, step: int, items: int -> HProp");
  __requires("factor: int, new_step: int, new_stop: int");
  __requires("check_stop: new_stop = factor * stop, check_step: new_step = factor * step");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(0, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(0, new_stop, new_step) -> items(exact_div(i, factor)))");
  __admitted();
}

__GHOST(ro_group_unscale) {
  __reverts(ro_group_scale);
  __admitted();
}

/* --- group split/join and nested variants: */

__GHOST(group_split) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __consumes("for i in range(start, stop, step) -> items(i)");
  __produces("for i in range(start, split, step) -> items(i)");
  __produces("for i in range(split, stop, step) -> items(i)");
  __admitted();
}

__GHOST(group_join) {
  __reverts(group_split);
  __admitted();
}

__GHOST(ro_group_split) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(start, split, step) -> items(i))");
  __produces("_RO(f, for i in range(split, stop, step) -> items(i))");
  __admitted();
}

__GHOST(ro_group_join) {
  __reverts(ro_group_split);
  __admitted();
}

__GHOST(pure_group_split) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> Prop");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires("forall i in range(start, stop, step) -> items(i)");
  __ensures("forall i in range(start, split, step) -> items(i)");
  __ensures("forall i in range(split, stop, step) -> items(i)");
  __admitted();
}

__GHOST(pure_group_join) {
  __reverts(pure_group_split);
  __admitted();
}

/* MINDEX (un)folding */

__GHOST(mindex2_unfold) {
  __requires("T: Type, H: (int * int -> ptr(T)) -> HProp, matrix: ptr(T), n1: int, n2: int");
  __consumes("H(fun i1 i2 -> &matrix[MINDEX2(n1, n2, i1, i2)])");
  __produces("H(fun i1 i2 -> &(&matrix[i1*n2])[MINDEX1(n2, i2)])");
  __admitted();
}

__GHOST(mindex2_fold) {
  __reverts(mindex2_unfold);
  __admitted();
}

__GHOST(mindex3_unfold) {
  __requires("T: Type, H: (int * int * int -> ptr(T)) -> HProp, matrix: ptr(T), n1: int, n2: int, n3: int");
  __consumes("H(fun i1 i2 i3 -> &matrix[MINDEX3(n1, n2, n3, i1, i2, i3)])");
  __produces("H(fun i1 i2 i3 -> &(&matrix[i1*n2*n3])[MINDEX2(n2, n3, i2, i3)])");
  __admitted();
}

__GHOST(mindex3_fold) {
  __reverts(mindex3_unfold);
  __admitted();
}

__GHOST(ro_mindex2_unfold) {
  __requires("T: Type, H: (int * int -> ptr(T)) -> HProp, matrix: ptr(T), n1: int, n2: int, f: _Fraction");
  __consumes("_RO(f, H(fun i1 i2 -> &matrix[MINDEX2(n1, n2, i1, i2)]))");
  __produces("_RO(f, H(fun i1 i2 -> &(&matrix[i1*n2])[MINDEX1(n2, i2)]))");
  __admitted();
}

__GHOST(ro_mindex2_fold) {
  __reverts(ro_mindex2_unfold);
  __admitted();
}

__GHOST(ro_mindex3_unfold) {
  __requires("T: Type, H: (int * int * int -> ptr(T)) -> HProp, matrix: ptr(T), n1: int, n2: int, n3: int, f: _Fraction");
  __consumes("_RO(f, H(fun i1 i2 i3 -> &matrix[MINDEX3(n1, n2, n3, i1, i2, i3)]))");
  __produces("_RO(f, H(fun i1 i2 i3 -> &(&matrix[i1*n2*n3])[MINDEX2(n2, n3, i2, i3)]))");
  __admitted();
}

__GHOST(ro_mindex3_fold) {
  __reverts(ro_mindex3_unfold);
  __admitted();
}

#endif
