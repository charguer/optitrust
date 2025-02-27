#ifndef OPTITRUST_H
#define OPTITRUST_H

#include <optitrust_intrinsics.h>

inline __ghost_fn __ghost_begin(__ghost_fn, __ghost_args = "", __ghost_bind = "") { return __admitted; }
inline void __ghost_end(__ghost_fn) {}

#define __GHOST_BEGIN(rev_ghost, ...) const __ghost_fn rev_ghost = __ghost_begin(__VA_ARGS__)
#define __GHOST_END(rev_ghost) __ghost_end(rev_ghost)

inline __ghost_fn __with_reverse(__ghost_fn g, __ghost_fn g_rev) { return g; }
inline void __reverts(__ghost_fn) {}

#define __GHOST_BEGIN_CUSTOM(rev_ghost, forward_ghost, backward_ghost) __GHOST_BEGIN(rev_ghost, __with_reverse(forward_ghost, backward_ghost))

/* ---- Matrix Functions ---- */

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

#define DEFINE_MMEMCPY(T) \
  inline void MMEMCPY_##T(T* dest, int d_offset, T* src, int s_offset, int length) { \
    __requires("d_end: int, s_end: int, d_all: int, s_all: int"); \
    __requires("check_d_size: d_end = d_offset + length"); \
    __requires("check_s_size: s_end = s_offset + length"); \
    __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~> Cell"); \
    __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~> Cell"); \
    __admitted(); \
    memcpy(&dest[d_offset], &src[s_offset], length * sizeof(T)); \
  }

DEFINE_MMEMCPY(int)
DEFINE_MMEMCPY(float)
DEFINE_MMEMCPY(double)

/* ---- Ghosts ---- */

// asserts that a property is true to generate a new pure proposition
__GHOST(assert_prop) {
  __requires("P: Prop");
  __requires("proof: P");
  __ensures("proof: P");
}

// assumes a formula with no need to prove it later
__GHOST(assume) {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

// defers proving a formula to later
__GHOST(to_prove) {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

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

// NOTE: calling forget_init on _RO or _Uninit is possible but useless
__GHOST(forget_init) {
  __requires("H: HProp");
  __consumes("H");
  __produces("_Uninit(H)");
  __admitted();
}

/* ---- In Range ---- */

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

// DEPRECATED? we have in_range_extend and can make loop
/* __GHOST(subrange_to_group_in_range) {
  __requires("r1: range, r2: range");
  __requires("is_subrange(r1, r2)");
  __ensures("for i in r1 -> in_range(i, r2)");
  __admitted(); // TODO
} */

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

/* identities due to normal form:

__GHOST(ro_distribute_group) {
  __requires("range: Range, items: int -> HProp, f: _Fraction");
  __consumes("_RO(f, Group(range, items))");
  __produces("for i in range -> _RO(f, items(i))");
  __admitted();
}

__GHOST(ro_factorize_group) {
  __reverts(ro_distribute_group)
  __admitted();
}
*/

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

__GHOST(uninit_swap_groups) {
  __requires("items: int * int -> HProp, inner_range: Range, outer_range: Range");
  __consumes("_Uninit(for i in outer_range -> for j in inner_range -> items(i,j))");
  __produces("_Uninit(for j in inner_range -> for i in outer_range -> items(i,j))");
  __admitted();
}

__GHOST(uninit_swap_groups_rev) {
  __reverts(uninit_swap_groups);
  __admitted();
}

__GHOST(tiled_index_in_range) {
  __requires("tile_index: int, index: int");
  __requires("tile_count: int, tile_size: int, size: int");
  __requires("size = tile_size * tile_count");
  __requires("in_range(tile_index, 0..tile_count)");
  __requires("in_range(index, 0..tile_size)");
  __ensures("in_range(tile_index * tile_size + index, 0..size)");
  __admitted();
}

__GHOST(tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> HProp,"
    "bound_check: size = tile_size * tile_count"
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
    "bound_check: size = tile_size * tile_count,"
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

__GHOST(group_collapse_ro) {
  __requires("n: int, m: int, items: (int * int) -> HProp, "
             "f: _Fraction");
  __consumes("_RO(f, for i in 0..n -> for j in 0..m -> items(i, j))");
  __produces("_RO(f, for ij in 0..(n * m) -> items(ij / m, ij % m))");
  __admitted();
}

__GHOST(group_uncollapse_ro) {
  __reverts(group_collapse_ro);
  __admitted();
}

__GHOST(group_collapse_uninit) {
  __requires("n: int, m: int, items: (int * int) -> HProp");
  __consumes("_Uninit(for i in 0..n -> for j in 0..m -> items(i, j))");
  __produces("_Uninit(for ij in 0..(n * m) -> items(ij / m, ij % m))");
  __admitted();
}

__GHOST(group_uncollapse_uninit) {
  __reverts(group_collapse_uninit);
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

__GHOST(group_ro_focus) {
  __requires("i: int, range: Range, items: int -> HProp, f: _Fraction");
  __requires("bound_check: in_range(i, range)");
  __consumes("_RO(f, Group(range, items))");
  __produces("Wand(_RO(f, items(i)), _RO(f, Group(range, items))), _RO(f, items(i))");
  __admitted();
}

__GHOST(group_ro_unfocus) {
  __reverts(group_ro_focus);
  __admitted(); // for performance
  __ghost(close_wand);

}

__GHOST(group_uninit_focus) {
  __requires("i: int, range: Range, items: int -> HProp");
  __requires("bound_check: in_range(i, range)");
  __consumes("_Uninit(Group(range, items))");
  __produces("Wand(_Uninit(items(i)), _Uninit(Group(range, items))), _Uninit(items(i))");
  __admitted();
}

__GHOST(group_uninit_unfocus) {
  __reverts(group_uninit_focus);

  __ghost(close_wand);
}

__GHOST(group2_ro_focus) {
  __requires("i: int, r: Range, r2: Range, items: int * int -> HProp, f: _Fraction");
  __requires("bound_check: in_range(i, r)");
  __consumes("_RO(f, for i2 in r2 -> for i in r -> items(i2, i))");
  __produces("Wand(_RO(f, for i2 in r2 -> items(i2, i)),"
                  "_RO(f, for i2 in r2 -> for i in r -> items(i2, i)))");
  __produces("_RO(f, for i2 in r2 -> items(i2, i))");
  __admitted();
}

__GHOST(group2_ro_unfocus) {
  __reverts(group2_ro_focus);

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

__GHOST(group_focus_subrange_ro) {
  __requires("sub_range: Range, big_range: Range, items: int -> HProp, f: _Fraction");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_RO(f, Group(big_range, items))");
  __produces("Wand(_RO(f, Group(sub_range, items)), _RO(f, Group(big_range, items)))");
  __produces("_RO(f, Group(sub_range, items))");
  __admitted();
}

__GHOST(group_unfocus_subrange_ro) {
  __reverts(group_focus_subrange_ro);
  __ghost(close_wand);
}

__GHOST(group_focus_subrange_uninit) {
  __requires("sub_range: Range, big_range: Range, items: int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(Group(big_range, items))");
  __produces(
    "Wand(_Uninit(Group(sub_range, items)),_Uninit(Group(big_range, items))),"
    "_Uninit(Group(sub_range, items))");
  __admitted();
}

__GHOST(group_unfocus_subrange_uninit) {
  __reverts(group_focus_subrange_uninit);
  __ghost(close_wand);
}

__GHOST(group2_focus_subrange_uninit) {
  __requires("outer_range: Range, sub_range: Range, big_range: Range, items: int -> int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(for i in outer_range -> Group(big_range, items(i)))");
  __produces("Wand("
      "_Uninit(for i in outer_range -> Group(sub_range, items(i))),"
      "_Uninit(for i in outer_range -> Group(big_range, items(i)))"
    "),"
    "_Uninit(for i in outer_range -> Group(sub_range, items(i)))");

  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_focus_subrange_uninit, ...);
  } */

  /* FIXME: This ghost should not be needed but to circumvent it we would have to write:
  __with_reverse(
    [&]() {
      __consumes("_Uninit(Group(0..10, fun i -> "
                 "Group(0..10, fun j -> "
                 "for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
      __produces("_Uninit(Group(0..10, fun i -> "
                 "Group(2..10, fun j -> "
                 "for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                 ", "
                 "Group(0..10, fun i -> Wand("
                 "  _Uninit("
                 "  Group(2..10, fun j -> "
                 "  Group(0..4
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                 "  _Uninit("
                 "  Group(0..10, fun j -> "
                 "  for k in 0..4 -> "
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                 "))");
      for (int i = 0; i < 10; i++) {
        __xconsumes("_Uninit("
                  "Group(0..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");
        __xproduces("_Uninit("
                  "Group(2..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(2..10, fun j -> "
                  "  Group(0..4, fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(0..10, fun j -> "
                  "  for k in 0..4 -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ")");
        __ghost(group_focus_subrange_uninit,
          "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
          "start := 2, stop := 10, step := 1");
      }
    },
    [&]() {
      for (int i = 0; i < 10; i++) {
        __xproduces("_Uninit("
                  "Group(0..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");
        __xconsumes("_Uninit("
                  "Group(2..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(2..10, fun j -> "
                  "  Group(0..4, fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(0..10, fun j -> "
                  "  for k in 0..4 -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ")");
        __ghost(group_unfocus_subrange_uninit, "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
      }
    }
  )
  */
}

__GHOST(group2_unfocus_subrange_uninit) {
  __reverts(group2_focus_subrange_uninit);
  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_unfocus_subrange_uninit, ...);
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

__GHOST(group_shift_uninit) {
  __requires("start: int, stop: int, step: int, items: int -> HProp");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __consumes("_Uninit(for i in range(start, stop, step) -> items(i))");
  __produces("_Uninit(for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__GHOST(group_unshift_uninit) {
  __reverts(group_shift_uninit);
  __admitted();
}

__GHOST(group_shift_ro) {
  __requires("start: int, stop: int, step: int, items: int -> HProp");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__GHOST(group_unshift_ro) {
  __reverts(group_shift_ro);
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

__GHOST(group_scale_uninit) {
  __requires("stop: int, step: int, items: int -> HProp");
  __requires("factor: int, new_step: int, new_stop: int");
  __requires("check_stop: new_stop = factor * stop, check_step: new_step = factor * step");
  __consumes("_Uninit(for i in range(0, stop, step) -> items(i))");
  __produces("_Uninit(for i in range(0, new_stop, new_step) -> items(exact_div(i, factor)))");
  __admitted();
}

__GHOST(group_unscale_uninit) {
  __reverts(group_scale_uninit);
  __admitted();
}

__GHOST(group_scale_ro) {
  __requires("stop: int, step: int, items: int -> HProp");
  __requires("factor: int, new_step: int, new_stop: int");
  __requires("check_stop: new_stop = factor * stop, check_step: new_step = factor * step");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(0, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(0, new_stop, new_step) -> items(exact_div(i, factor)))");
  __admitted();
}

__GHOST(group_unscale_ro) {
  __reverts(group_scale_ro);
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

__GHOST(group_split_uninit) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __consumes("_Uninit(for i in range(start, stop, step) -> items(i))");
  __produces("_Uninit(for i in range(start, split, step) -> items(i))");
  __produces("_Uninit(for i in range(split, stop, step) -> items(i))");
  __admitted();
}

__GHOST(group_join_uninit) {
  __reverts(group_split_uninit);
  __admitted();
}

__GHOST(group_split_ro) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(start, split, step) -> items(i))");
  __produces("_RO(f, for i in range(split, stop, step) -> items(i))");
  __admitted();
}

__GHOST(group_join_ro) {
  __reverts(group_split_ro);
  __admitted();
}

__GHOST(group_split_pure) {
  __requires("start: int, stop: int, step: int, split: int, items: int -> Prop");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires("forall i in range(start, stop, step) -> items(i)");
  __ensures("forall i in range(start, split, step) -> items(i)");
  __ensures("forall i in range(split, stop, step) -> items(i)");
  __admitted();
}

__GHOST(group_join_pure) {
  __reverts(group_split_pure);
  __admitted();
}

/* ---- Matrix Ghosts ---- */

__GHOST(matrix2_focus)  {
  __requires("T: Type, M: ptr(T), i: int, j: int, m: int, n: int");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("M ~> Matrix2(m, n)");
  __produces("Wand(&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> Matrix2(m, n)), &M[MINDEX2(m, n, i, j)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check := bound_check_i");
  __ghost(group_focus, "i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__GHOST(matrix2_unfocus) {
  __reverts(matrix2_focus);

  __ghost(close_wand);
}

__GHOST(matrix1_focus) {
  __requires("T: Type, M: ptr(T), i: int, n: int");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("M ~> Matrix1(n)");
  __produces("Wand(&M[MINDEX1(n, i)] ~> Cell, M ~> Matrix1(n)), &M[MINDEX1(n, i)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check := bound_check");
}

__GHOST(matrix1_unfocus) {
  __reverts(matrix1_focus);

  __ghost(close_wand);
}

__GHOST(matrix1_ro_focus) {
  __requires("T: Type, M: ptr(T), i: int, n: int, f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, M ~> Matrix1(n))");
  __produces("Wand(_RO(f, &M[MINDEX1(n, i)] ~> Cell), _RO(f, M ~> Matrix1(n))), _RO(f, &M[MINDEX1(n, i)] ~> Cell)");
  __admitted(); // for efficiency
  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check");
}

__GHOST(matrix1_ro_unfocus) {
  __reverts(matrix1_ro_focus);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

__GHOST(matrix2_ro_focus) {
  __requires("T: Type, M: ptr(T), i: int, j: int, m: int, n: int, f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, M ~> Matrix2(m, n))");
  __produces("Wand(_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> Matrix2(m,n))), _RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell)");

  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(group_ro_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__GHOST(matrix2_ro_unfocus) {
  __reverts(matrix2_ro_focus);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

// matrix*_contiguous

__GHOST(matrix2_contiguous) {
  __requires("T: Type, M: ptr(T), a: int, b: int, n2: int, n1: int");
  __consumes("for i in a..b -> for j in 0..n1 -> "
               "&M[MINDEX2(n2, n1, i, j)] ~> Cell");
  __produces("for k in a*n1..b*n1 -> &M[MINDEX1(n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(matrix3_contiguous) {
  __requires("T: Type, M: ptr(T), a: int, b: int, n3: int, n2: int, n1: int");
  __consumes("for i3 in a..b -> for i2 in 0..n2 -> "
             "for i1 in 0..n1 -> &M[MINDEX3(n3, n2, n1, i1, i2, i3)] ~> Cell");
  __produces("for k in a*n2*n1..b*n2*n1 -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(mindex2_contiguous) {
  __requires("T: Type, M: ptr(T), n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell");
  __produces("for k in i2*n1 + a..i2*n1 + b -> &M[MINDEX1(n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(mindex2_contiguous_rev) {
  __reverts(mindex2_contiguous);
  __admitted();
}

__GHOST(mindex3_contiguous) {
  __requires("T: Type, M: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell");
  __produces("for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
               "&M[MINDEX1(n3*n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(mindex3_contiguous_rev) {
  __reverts(mindex3_contiguous);
  __admitted();
}

__GHOST(mindex2_contiguous_uninit) {
  __requires("T: Type, M: ptr(T), n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces("_Uninit(for k in (i2*n1 + a)..(i2*n1 + b) -> "
               "&M[MINDEX1(n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex2_contiguous_uninit_rev) {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__GHOST(mindex3_contiguous_uninit) {
  __requires("T: Type, M: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(for i1 in a..b -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces("_Uninit(for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
               "&M[MINDEX1(n3*n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex3_contiguous_uninit_rev) {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__GHOST(mindex2_contiguous_ro) {
  __requires("T: Type, M: ptr(T), n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces("_RO(f, for k in (i2*n1 + a)..(i2*n1 + b) -> "
               "&M[MINDEX1(n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex2_contiguous_ro_rev) {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__GHOST(mindex3_contiguous_ro) {
  __requires("T: Type, M: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces("_RO(f, for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
                "&M[MINDEX1(n3*n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex3_contiguous_ro_rev) {
  __reverts(mindex3_contiguous_ro);
  __admitted();
}

/* ---- Ghosts to check assertions ---- */

__GHOST(assert_eq) {
  /* Names x and y are used in assert_alias code */
  __requires("x: int, y: int, eq: x = y");
}

inline __ghost_ret assert_alias() {}


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

/* ---- Algorithmic Functions ---- */

/* TODO: generalize to any monoid/group, see doc/sliding-window.md */
// template<typename T, typename ST>
inline uint16_t reduce_spe1(int start, int stop, uint8_t* input, int n, int m, int j) {
  __requires("check_range: is_subrange(start..stop, 0..n)");
  __requires("bound_check: in_range(j, 0..m)");
  __reads("input ~> Matrix2(n, m)");
  __admitted(); // NOT NECESSARY, BUT FASTER
  // __reads("for k in 0..n -> &input[MINDEX2(n, m, k, j)] ~> Cell");

  uint16_t s = (uint16_t)0;
  for (int i = start; i < stop; i++) {
    __smodifies("&s ~> Cell");
    __sreads("input ~> Matrix2(n, m)");

    __ghost(in_range_extend, "i, start..stop, 0..n");
    __GHOST_BEGIN(focus, matrix2_ro_focus, "input, i, j");
    s += (uint16_t)input[MINDEX2(n, m, i, j)];
    __GHOST_END(focus);
  }
  return s;
}

#endif
