#include <optitrust_intrinsics.h>

__ghost_fn __ghost_begin(__ghost_fn, __ghost_args, __ghost_bind) {
  return __admitted;
}

void __ghost_end(__ghost_fn) {}

__ghost_fn __with_reverse(__ghost_fn g, __ghost_fn g_rev) { return g; }

void __reverts(__ghost_fn) {}

__ghost_ret __clear(__ghost_args) {}

__ghost_ret assert_inhabited() {
  __requires("T: Type");
  __requires("x: T");
  __ensures("x: T");
}

__ghost_ret define() {}

__ghost_ret assert_prop() {
  __requires("P: Prop");
  __requires("proof: P");
  __ensures("proof: P");
}

__ghost_ret assert_eq() {
  __requires("x: int");
  __requires("y: int");
  __requires("eq: __is_true(x == y)");
}

__ghost_ret assert_alias() {}

__ghost_ret assume() {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

__ghost_ret to_prove() {
  __requires("P: Prop");
  __ensures("H: P");
  __admitted();
}

int MINDEX0() { return 0; }

int MINDEX1(int N1, int i1) { return i1; }

int MINDEX2(int N1, int N2, int i1, int i2) { return i1 * N2 + i2; }

int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N3 + i3;
}

int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

size_t MSIZE0() { return 1; }

size_t MSIZE1(int N1) { return (size_t)N1; }

size_t MSIZE2(int N1, int N2) { return (size_t)N1 * (size_t)N2; }

size_t MSIZE3(int N1, int N2, int N3) {
  return (size_t)N1 * (size_t)N2 * (size_t)N3;
}

size_t MSIZE4(int N1, int N2, int N3, int N4) {
  return (size_t)N1 * (size_t)N2 * (size_t)N3 * (size_t)N4;
}

void MMEMCPY_int(int* dest, int d_offset, int* src, int s_offset, int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~> Cell");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~> Cell");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(int));
}

void MMEMCPY_float(float* dest, int d_offset, float* src, int s_offset,
                   int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~> Cell");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~> Cell");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(float));
}

void MMEMCPY_double(double* dest, int d_offset, double* src, int s_offset,
                    int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~> Cell");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~> Cell");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(double));
}

__ghost(assert_inhabited, "x := arbitrary(HProp * HProp -> HProp)",
        "Wand <- x");

__ghost_ret close_wand() {
  __requires("H1: HProp");
  __requires("H2: HProp");
  __consumes("Wand(H1, H2)");
  __consumes("H1");
  __produces("H2");
  __admitted();
}

__ghost_ret hide() {
  __requires("H: HProp");
  __consumes("H");
  __ensures("H2: HProp");
  __produces("Wand(H2, H)");
  __produces("H2");
  __admitted();
}

__ghost_ret hide_rev() {
  __reverts(hide);
  __ghost(close_wand);
}

__ghost_ret wand_simplify() {
  __requires("H1: HProp");
  __requires("H2: HProp");
  __requires("H3: HProp");
  __consumes("Wand(H1, H2)");
  __consumes("Wand(H2, H3)");
  __produces("Wand(H1, H3)");
  __admitted();
}

__ghost_ret forget_init() {
  __requires("H: HProp");
  __consumes("H");
  __produces("_Uninit(H)");
  __admitted();
}

__ghost(assert_inhabited, "x := arbitrary(int * Range -> Prop)",
        "in_range <- x");

__ghost(assert_inhabited, "x := arbitrary(Range * Range -> Prop)",
        "is_subrange <- x");

__ghost(assert_inhabited, "x := arbitrary(Range -> int)", "range_count <- x");

__ghost_ret in_range_extend() {
  __requires("x: int");
  __requires("r1: Range");
  __requires("r2: Range");
  __requires("in_range(x, r1)");
  __requires("is_subrange(r1, r2)");
  __ensures("in_range(x, r2)");
  __admitted();
}

__ghost_ret in_range_shift() {
  __requires("x: int");
  __requires("k: int");
  __requires("a: int");
  __requires("b: int");
  __requires("s: int");
  __requires("in_range(x, range(a, b, s))");
  __ensures("in_range(x + k, range(a + k, b + k, s))");
  __admitted();
}

__ghost_ret in_range_shift_extend() {
  __requires("x: int");
  __requires("k: int");
  __requires("r: Range");
  __requires("a: int");
  __requires("b: int");
  __requires("s: int");
  __requires("in_range(x, range(a, b, s))");
  __requires("is_subrange(range(a + k, b + k, s), r)");
  __ensures("in_range(x + k, r)");
  __admitted();
  __ghost(in_range_shift, "x := x, k := k, a := a, b := b, s := s");
  __ghost(in_range_extend, "x := x + k, r1 := range(a + k, b + k, s), r2 := r");
}

__ghost_ret ro_split2() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 2, H)");
  __produces("_RO(f / 2, H)");
  __admitted();
}

__ghost_ret ro_split3() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 3, H)");
  __produces("_RO(f / 3, H)");
  __produces("_RO(f / 3, H)");
  __admitted();
}

__ghost_ret ro_split4() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 4, H)");
  __produces("_RO(f / 4, H)");
  __produces("_RO(f / 4, H)");
  __produces("_RO(f / 4, H)");
  __admitted();
}

__ghost_ret ro_allow_join2() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f / 2, H)");
  __produces("_RO(f - f / 2, H)");
  __admitted();
}

__ghost_ret ro_allow_join3() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f / 3, H)");
  __produces("_RO(f - f / 3 - f / 3, H)");
  __admitted();
}

__ghost_ret ro_allow_join4() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __consumes("_RO(f / 4, H)");
  __produces("_RO(f - f / 4 - f / 4 - f / 4, H)");
  __admitted();
}

__ghost_ret ro_fork_group() {
  __requires("f: _Fraction");
  __requires("H: HProp");
  __requires("r: Range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / range_count(r), for #_1 in r -> H)");
  __admitted();
}

__ghost_ret ro_join_group() {
  __reverts(ro_fork_group);
  __admitted();
}

__ghost_ret swap_groups() {
  __requires("items: int * int -> HProp");
  __requires("inner_range: Range");
  __requires("outer_range: Range");
  __consumes("for i in outer_range -> for j in inner_range -> items(i, j)");
  __produces("for j in inner_range -> for i in outer_range -> items(i, j)");
  __admitted();
}

__ghost_ret swap_groups_rev() {
  __reverts(swap_groups);
  __admitted();
}

__ghost_ret ro_swap_groups() {
  __requires("items: int * int -> HProp");
  __requires("inner_range: Range");
  __requires("outer_range: Range");
  __requires("f: _Fraction");
  __consumes(
      "_RO(f, for i in outer_range -> for j in inner_range -> items(i, j))");
  __produces(
      "_RO(f, for j in inner_range -> for i in outer_range -> items(i, j))");
  __admitted();
}

__ghost_ret ro_swap_groups_rev() {
  __reverts(ro_swap_groups);
  __admitted();
}

__ghost_ret uninit_swap_groups() {
  __requires("items: int * int -> HProp");
  __requires("inner_range: Range");
  __requires("outer_range: Range");
  __consumes(
      "_Uninit(for i in outer_range -> for j in inner_range -> items(i, j))");
  __produces(
      "_Uninit(for j in inner_range -> for i in outer_range -> items(i, j))");
  __admitted();
}

__ghost_ret uninit_swap_groups_rev() {
  __reverts(uninit_swap_groups);
  __admitted();
}

__ghost_ret tiled_index_in_range() {
  __requires("tile_index: int");
  __requires("index: int");
  __requires("tile_count: int");
  __requires("tile_size: int");
  __requires("size: int");
  __requires("__is_true(size == tile_size * tile_count)");
  __requires("in_range(tile_index, 0..tile_count)");
  __requires("in_range(index, 0..tile_size)");
  __ensures("in_range(tile_index * tile_size + index, 0..size)");
  __admitted();
}

__ghost_ret tile_divides() {
  __requires("tile_count: int");
  __requires("tile_size: int");
  __requires("size: int");
  __requires("items: int -> HProp");
  __requires("bound_check: __is_true(size == tile_size * tile_count)");
  __consumes("Group(0..size, items)");
  __produces(
      "for bi in 0..tile_count -> for i in 0..tile_size -> items(bi * "
      "tile_size + i)");
  __admitted();
}

__ghost_ret untile_divides() {
  __reverts(tile_divides);
  __admitted();
}

__ghost_ret ro_tile_divides() {
  __requires("tile_count: int");
  __requires("tile_size: int");
  __requires("size: int");
  __requires("items: int -> HProp");
  __requires("bound_check: __is_true(size == tile_size * tile_count)");
  __requires("f: _Fraction");
  __consumes("_RO(f, Group(0..size, items))");
  __produces(
      "_RO(f, for bi in 0..tile_count -> for i in 0..tile_size -> items(bi * "
      "tile_size + i))");
  __admitted();
}

__ghost_ret ro_untile_divides() {
  __reverts(ro_tile_divides);
  __admitted();
}

__ghost_ret group_collapse() {
  __requires("n: int");
  __requires("m: int");
  __requires("items: int * int -> HProp");
  __consumes("for i in 0..n -> for j in 0..m -> items(i, j)");
  __produces("for ij in 0..n * m -> items(ij / m, ij % m)");
  __admitted();
}

__ghost_ret group_uncollapse() {
  __reverts(group_collapse);
  __admitted();
}

__ghost_ret group_collapse_ro() {
  __requires("n: int");
  __requires("m: int");
  __requires("items: int * int -> HProp");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in 0..n -> for j in 0..m -> items(i, j))");
  __produces("_RO(f, for ij in 0..n * m -> items(ij / m, ij % m))");
  __admitted();
}

__ghost_ret group_uncollapse_ro() {
  __reverts(group_collapse_ro);
  __admitted();
}

__ghost_ret group_collapse_uninit() {
  __requires("n: int");
  __requires("m: int");
  __requires("items: int * int -> HProp");
  __consumes("_Uninit(for i in 0..n -> for j in 0..m -> items(i, j))");
  __produces("_Uninit(for ij in 0..n * m -> items(ij / m, ij % m))");
  __admitted();
}

__ghost_ret group_uncollapse_uninit() {
  __reverts(group_collapse_uninit);
  __admitted();
}

__ghost_ret group_focus() {
  __requires("i: int");
  __requires("range: Range");
  __requires("items: int -> HProp");
  __requires("bound_check: in_range(i, range)");
  __consumes("Group(range, items)");
  __produces("Wand(items(i), Group(range, items))");
  __produces("items(i)");
  __admitted();
}

__ghost_ret group_unfocus() {
  __reverts(group_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret group_ro_focus() {
  __requires("i: int");
  __requires("range: Range");
  __requires("items: int -> HProp");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, range)");
  __consumes("_RO(f, Group(range, items))");
  __produces("Wand(_RO(f, items(i)), _RO(f, Group(range, items)))");
  __produces("_RO(f, items(i))");
  __admitted();
}

__ghost_ret group_ro_unfocus() {
  __reverts(group_ro_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret group_uninit_focus() {
  __requires("i: int");
  __requires("range: Range");
  __requires("items: int -> HProp");
  __requires("bound_check: in_range(i, range)");
  __consumes("_Uninit(Group(range, items))");
  __produces("Wand(_Uninit(items(i)), _Uninit(Group(range, items)))");
  __produces("_Uninit(items(i))");
  __admitted();
}

__ghost_ret group_uninit_unfocus() {
  __reverts(group_uninit_focus);
  __ghost(close_wand);
}

__ghost_ret group2_ro_focus() {
  __requires("i: int");
  __requires("r: Range");
  __requires("r2: Range");
  __requires("items: int * int -> HProp");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, r)");
  __consumes("_RO(f, for i2 in r2 -> for i in r -> items(i2, i))");
  __produces(
      "Wand(_RO(f, for i2 in r2 -> items(i2, i)), _RO(f, for i2 in r2 -> for i "
      "in r -> items(i2, i)))");
  __produces("_RO(f, for i2 in r2 -> items(i2, i))");
  __admitted();
}

__ghost_ret group2_ro_unfocus() {
  __reverts(group2_ro_focus);
  __ghost(close_wand);
}

__ghost_ret group_focus_subrange() {
  __requires("sub_range: Range");
  __requires("big_range: Range");
  __requires("items: int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("Group(big_range, items)");
  __produces("Wand(Group(sub_range, items), Group(big_range, items))");
  __produces("Group(sub_range, items)");
  __admitted();
}

__ghost_ret group_unfocus_subrange() {
  __reverts(group_focus_subrange);
  __ghost(close_wand);
}

__ghost_ret group_focus_subrange_ro() {
  __requires("sub_range: Range");
  __requires("big_range: Range");
  __requires("items: int -> HProp");
  __requires("f: _Fraction");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_RO(f, Group(big_range, items))");
  __produces(
      "Wand(_RO(f, Group(sub_range, items)), _RO(f, Group(big_range, items)))");
  __produces("_RO(f, Group(sub_range, items))");
  __admitted();
}

__ghost_ret group_unfocus_subrange_ro() {
  __reverts(group_focus_subrange_ro);
  __ghost(close_wand);
}

__ghost_ret group_focus_subrange_uninit() {
  __requires("sub_range: Range");
  __requires("big_range: Range");
  __requires("items: int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(Group(big_range, items))");
  __produces(
      "Wand(_Uninit(Group(sub_range, items)), _Uninit(Group(big_range, "
      "items)))");
  __produces("_Uninit(Group(sub_range, items))");
  __admitted();
}

__ghost_ret group_unfocus_subrange_uninit() {
  __reverts(group_focus_subrange_uninit);
  __ghost(close_wand);
}

__ghost_ret group2_focus_subrange_uninit() {
  __requires("outer_range: Range");
  __requires("sub_range: Range");
  __requires("big_range: Range");
  __requires("items: int -> int -> HProp");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(for i in outer_range -> Group(big_range, items(i)))");
  __produces(
      "Wand(_Uninit(for i in outer_range -> Group(sub_range, items(i))), "
      "_Uninit(for i in outer_range -> Group(big_range, items(i))))");
  __produces("_Uninit(for i in outer_range -> Group(sub_range, items(i)))");
  __admitted();
}

__ghost_ret group2_unfocus_subrange_uninit() {
  __reverts(group2_focus_subrange_uninit);
  __admitted();
}

__ghost_ret group_shift() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("shift: int");
  __requires("new_start: int");
  __requires("new_stop: int");
  __requires("check_start: __is_true(new_start == start + shift)");
  __requires("check_stop: __is_true(new_stop == stop + shift)");
  __consumes("for i in range(start, stop, step) -> items(i)");
  __produces("for i in range(new_start, new_stop, step) -> items(i - shift)");
  __admitted();
}

__ghost_ret group_unshift() {
  __reverts(group_shift);
  __admitted();
}

__ghost_ret group_shift_uninit() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("shift: int");
  __requires("new_start: int");
  __requires("new_stop: int");
  __requires("check_start: __is_true(new_start == start + shift)");
  __requires("check_stop: __is_true(new_stop == stop + shift)");
  __consumes("_Uninit(for i in range(start, stop, step) -> items(i))");
  __produces(
      "_Uninit(for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__ghost_ret group_unshift_uninit() {
  __reverts(group_shift_uninit);
  __admitted();
}

__ghost_ret group_shift_ro() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("shift: int");
  __requires("new_start: int");
  __requires("new_stop: int");
  __requires("check_start: __is_true(new_start == start + shift)");
  __requires("check_stop: __is_true(new_stop == stop + shift)");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces(
      "_RO(f, for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__ghost_ret group_unshift_ro() {
  __reverts(group_shift_ro);
  __admitted();
}

__ghost_ret group_scale() {
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("factor: int");
  __requires("new_step: int");
  __requires("new_stop: int");
  __requires("check_stop: __is_true(new_stop == factor * stop)");
  __requires("check_step: __is_true(new_step == factor * step)");
  __requires("check_factor: __is_true(factor != 0)");
  __consumes("for i in range(0, stop, step) -> items(i)");
  __produces(
      "for i in range(0, new_stop, new_step) -> items(exact_div(i, factor))");
  __admitted();
}

__ghost_ret group_unscale() {
  __reverts(group_scale);
  __admitted();
}

__ghost_ret group_scale_uninit() {
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("factor: int");
  __requires("new_step: int");
  __requires("new_stop: int");
  __requires("check_stop: __is_true(new_stop == factor * stop)");
  __requires("check_step: __is_true(new_step == factor * step)");
  __consumes("_Uninit(for i in range(0, stop, step) -> items(i))");
  __produces(
      "_Uninit(for i in range(0, new_stop, new_step) -> items(exact_div(i, "
      "factor)))");
  __admitted();
}

__ghost_ret group_unscale_uninit() {
  __reverts(group_scale_uninit);
  __admitted();
}

__ghost_ret group_scale_ro() {
  __requires("stop: int");
  __requires("step: int");
  __requires("items: int -> HProp");
  __requires("factor: int");
  __requires("new_step: int");
  __requires("new_stop: int");
  __requires("check_stop: __is_true(new_stop == factor * stop)");
  __requires("check_step: __is_true(new_step == factor * step)");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(0, stop, step) -> items(i))");
  __produces(
      "_RO(f, for i in range(0, new_stop, new_step) -> items(exact_div(i, "
      "factor)))");
  __admitted();
}

__ghost_ret group_unscale_ro() {
  __reverts(group_scale_ro);
  __admitted();
}

__ghost_ret group_split() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("split: int");
  __requires("items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __consumes("for i in range(start, stop, step) -> items(i)");
  __produces("for i in range(start, split, step) -> items(i)");
  __produces("for i in range(split, stop, step) -> items(i)");
  __admitted();
}

__ghost_ret group_join() {
  __reverts(group_split);
  __admitted();
}

__ghost_ret group_split_uninit() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("split: int");
  __requires("items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __consumes("_Uninit(for i in range(start, stop, step) -> items(i))");
  __produces("_Uninit(for i in range(start, split, step) -> items(i))");
  __produces("_Uninit(for i in range(split, stop, step) -> items(i))");
  __admitted();
}

__ghost_ret group_join_uninit() {
  __reverts(group_split_uninit);
  __admitted();
}

__ghost_ret group_split_ro() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("split: int");
  __requires("items: int -> HProp");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(start, split, step) -> items(i))");
  __produces("_RO(f, for i in range(split, stop, step) -> items(i))");
  __admitted();
}

__ghost_ret group_join_ro() {
  __reverts(group_split_ro);
  __admitted();
}

__ghost_ret group_split_pure() {
  __requires("start: int");
  __requires("stop: int");
  __requires("step: int");
  __requires("split: int");
  __requires("items: int -> Prop");
  __requires("bound_check: in_range(split, range(start, stop, step))");
  __requires(
      "forall (i: int) (_: in_range(i, range(start, stop, step))) -> items(i)");
  __ensures(
      "forall (i: int) (_: in_range(i, range(start, split, step))) -> "
      "items(i)");
  __ensures(
      "forall (i: int) (_: in_range(i, range(split, stop, step))) -> items(i)");
  __admitted();
}

__ghost_ret group_join_pure() {
  __reverts(group_split_pure);
  __admitted();
}

__ghost_ret matrix2_focus() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("m: int");
  __requires("n: int");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("M ~> Matrix2(m, n)");
  __produces("Wand(&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> Matrix2(m, n))");
  __produces("&M[MINDEX2(m, n, i, j)] ~> Cell");
  __ghost(group_focus, "i := i, bound_check := bound_check_i");
  __ghost(group_focus, "i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret matrix2_unfocus() {
  __reverts(matrix2_focus);
  __ghost(close_wand);
}

__ghost_ret matrix1_focus() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("M ~> Matrix1(n)");
  __produces("Wand(&M[MINDEX1(n, i)] ~> Cell, M ~> Matrix1(n))");
  __produces("&M[MINDEX1(n, i)] ~> Cell");
  __ghost(group_focus, "i := i, bound_check := bound_check");
}

__ghost_ret matrix1_unfocus() {
  __reverts(matrix1_focus);
  __ghost(close_wand);
}

__ghost_ret matrix1_ro_focus() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, M ~> Matrix1(n))");
  __produces(
      "Wand(_RO(f, &M[MINDEX1(n, i)] ~> Cell), _RO(f, M ~> Matrix1(n)))");
  __produces("_RO(f, &M[MINDEX1(n, i)] ~> Cell)");
  __admitted();
  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check");
}

__ghost_ret matrix1_ro_unfocus() {
  __reverts(matrix1_ro_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret matrix2_ro_focus() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("m: int");
  __requires("n: int");
  __requires("f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, M ~> Matrix2(m, n))");
  __produces(
      "Wand(_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> Matrix2(m, "
      "n)))");
  __produces("_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell)");
  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(group_ro_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret matrix2_ro_unfocus() {
  __reverts(matrix2_ro_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret matrix2_contiguous() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("a: int");
  __requires("b: int");
  __requires("n2: int");
  __requires("n1: int");
  __consumes(
      "for i in a..b -> for j in 0..n1 -> &M[MINDEX2(n2, n1, i, j)] ~> Cell");
  __produces("for k in a * n1..b * n1 -> &M[MINDEX1(n2 * n1, k)] ~> Cell");
  __admitted();
}

__ghost_ret matrix3_contiguous() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("a: int");
  __requires("b: int");
  __requires("n3: int");
  __requires("n2: int");
  __requires("n1: int");
  __consumes(
      "for i3 in a..b -> for i2 in 0..n2 -> for i1 in 0..n1 -> &M[MINDEX3(n3, "
      "n2, n1, i1, i2, i3)] ~> Cell");
  __produces(
      "for k in a * n2 * n1..b * n2 * n1 -> &M[MINDEX1(n3 * n2 * n1, k)] ~> "
      "Cell");
  __admitted();
}

__ghost_ret mindex2_contiguous() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes("for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell");
  __produces(
      "for k in i2 * n1 + a..i2 * n1 + b -> &M[MINDEX1(n2 * n1, k)] ~> Cell");
  __admitted();
}

__ghost_ret mindex2_contiguous_rev() {
  __reverts(mindex2_contiguous);
  __admitted();
}

__ghost_ret mindex3_contiguous() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes("for i1 in a..b -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell");
  __produces(
      "for k in i3 * n2 * n1 + i2 * n1 + a..i3 * n2 * n1 + i2 * n1 + b -> "
      "&M[MINDEX1(n3 * n2 * n1, k)] ~> Cell");
  __admitted();
}

__ghost_ret mindex3_contiguous_rev() {
  __reverts(mindex3_contiguous);
  __admitted();
}

__ghost_ret mindex2_contiguous_uninit() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes("_Uninit(for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces(
      "_Uninit(for k in i2 * n1 + a..i2 * n1 + b -> &M[MINDEX1(n2 * n1, k)] ~> "
      "Cell)");
  __admitted();
}

__ghost_ret mindex2_contiguous_uninit_rev() {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__ghost_ret mindex3_contiguous_uninit() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes(
      "_Uninit(for i1 in a..b -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces(
      "_Uninit(for k in i3 * n2 * n1 + i2 * n1 + a..i3 * n2 * n1 + i2 * n1 + b "
      "-> &M[MINDEX1(n3 * n2 * n1, k)] ~> Cell)");
  __admitted();
}

__ghost_ret mindex3_contiguous_uninit_rev() {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__ghost_ret mindex2_contiguous_ro() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces(
      "_RO(f, for k in i2 * n1 + a..i2 * n1 + b -> &M[MINDEX1(n2 * n1, k)] ~> "
      "Cell)");
  __admitted();
}

__ghost_ret mindex2_contiguous_ro_rev() {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__ghost_ret mindex3_contiguous_ro() {
  __requires("T: Type");
  __requires("M: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("f: _Fraction");
  __consumes(
      "_RO(f, for i1 in a..b -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces(
      "_RO(f, for k in i3 * n2 * n1 + i2 * n1 + a..i3 * n2 * n1 + i2 * n1 + b "
      "-> &M[MINDEX1(n3 * n2 * n1, k)] ~> Cell)");
  __admitted();
}

__ghost_ret mindex3_contiguous_ro_rev() {
  __reverts(mindex3_contiguous_ro);
  __admitted();
}

int exact_div(int n, int b) {
  __pure();
  __admitted();
  return n / b;
}

int min(int a, int b) {
  __pure();
  __admitted();
  return a < b ? a : b;
}

int max(int a, int b) {
  __pure();
  __admitted();
  return a > b ? a : b;
}

int ANY(int maxValue) { return 0; }

uint16_t reduce_spe1(int start, int stop, uint8_t* input, int n, int m, int j) {
  __requires("check_range: is_subrange(start..stop, 0..n)");
  __requires("bound_check: in_range(j, 0..m)");
  __reads("input ~> Matrix2(n, m)");
  __admitted();
  uint16_t s = (uint16_t)0;
  for (int i = start; i < stop; i++) {
    __smodifies("&s ~> Cell");
    __sreads("input ~> Matrix2(n, m)");
    __ghost(in_range_extend, "x := i, r1 := start..stop, r2 := 0..n");
    const __ghost_fn focus =
        __ghost_begin(matrix2_ro_focus, "M := input, i := i, j := j");
    s += (uint16_t)input[MINDEX2(n, m, i, j)];
    __ghost_end(focus);
  }
  return s;
}
