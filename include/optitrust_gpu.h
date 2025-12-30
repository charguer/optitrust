#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

extern const int __threadfor;

__DECL(GMem, "MemType");
__DECL(KernelParams, "int * int * int -> HProp");
__DECL(SMemAlloc, "int -> HProp");

__DECL(chunk_range, "Range * int * int -> Range");
__DECL(range_eq, "Range * Range -> Prop");

/* --- Kernel launches ---- */

__DECL(HostCtx, "HProp");

void kernel_start(int tpb, int bpg, int smem_sz) {
  __requires("r: Range");
  __requires("by: range_eq(range_plus(MINDEX1(bpg * tpb, 0), bpg * tpb), r)");
  __consumes("HostCtx");
  __produces("ThreadsCtx(r)");
  __produces("KernelParams(tpb, bpg, smem_sz)");
  __produces("desync_for(r) b in ..bpg -> SMemAlloc(smem_sz)");
  __admitted();
}

void kernel_end() {
  __requires("tpb: int, bpg: int, smem_sz: int, r: Range");
  __requires("by: range_eq(range_plus(MINDEX1(bpg * tpb, 0), bpg * tpb), r)");
  __consumes("ThreadsCtx(r)");
  __consumes("KernelParams(tpb, bpg, smem_sz)");
  __consumes("desync_for(r) b in ..bpg -> SMemAlloc(smem_sz)");
  __produces("HostCtx");
  __admitted();
}

/* --- Synchronization ---- */

__DECL(block_sync_mem, "MemType -> Prop");
__AXIOM(gmem_block_sync_mem, "block_sync_mem(GMem)");

void blocksync() {
  __requires("H: HProp, tpb: int, bpg: int, smem_sz: int, t: int");
  __reads("KernelParams(tpb, bpg, smem_sz)");
  __preserves("ThreadsCtx(range_plus(t, tpb))");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

__GHOST(kernel_end_sync) {
  __requires("H: HProp, tpb: int, bpg: int, smem_sz: int, t: int, N: int");
  __requires("by: bpg * tpb = N");
  __preserves("KernelParams(tpb, bpg, smem_sz)");
  __preserves("ThreadsCtx(range_plus(t, N))");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

/* --- Memory management ---- */

template <typename T> T __GMEM_GET_IMPL(T* p);
template <typename T> T __GMEM_GET(T* p) {
  __requires("v: T, t: int");
  __preserves("ThreadsCtx(range_plus(t, MSIZE0()))");
  __reads("p ~~>[GMem] v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __GMEM_GET_IMPL(p);
}

template <typename T> void __GMEM_SET_IMPL(T* p, T v);
template <typename T> void __GMEM_SET(T* p, T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(range_plus(t, MSIZE0()))");
  __writes("p ~~>[GMem] v");
  __ensures("__spec_override_noret()");
  __admitted();
  __GMEM_SET_IMPL(p, v);
}

// TODO: integrate into optitrust_models (possibly requires reworking existing case_studies/testbenches)
// TODO: syntax sugar for matrix1 with memory types
__GHOST(ro_matrix1_focus_generic) {
  __requires("T: Type, matrix: ptr(T), i: int, n: int, MT: MemType, M: int -> T, f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, for i in 0..n -> &matrix[MINDEX1(n,i)] ~~>[MT] M(i))");
  __produces("Wand(_RO(f, &matrix[MINDEX1(n, i)] ~~>[MT] M(i)), _RO(f, for i in 0..n -> &matrix[MINDEX1(n,i)] ~~>[MT] M(i))), _RO(f, &matrix[MINDEX1(n, i)] ~~>[MT] M(i))");
  __admitted(); // for efficiency
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check");
}

__GHOST(ro_matrix1_unfocus_generic) {
  __reverts(ro_matrix1_focus_generic);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

template <typename T> void gmem_free(T* p) {
  __requires("H: HProp");
  __preserves("HostCtx");
  __consumes("Free(p, H)");
  __consumes("H");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> T* __gmem_malloc1_impl(int N1);
template <typename T> T* __gmem_malloc1(int N1) {
  __preserves("HostCtx");
  __produces("for i in 0..N1 -> &_Res[MINDEX1(N1,i)] ~> UninitCellOf(GMem)");
  __produces("Free(_Res, for i in 0..N1 -> &_Res[MINDEX1(N1,i)] ~> UninitCellOf(GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __gmem_malloc1_impl<T>(N1);
}
#define gmem_malloc1(T, N1) __call_with(__gmem_malloc1<T>(N1), "T := "#T);

template <typename T> void memcpy_host_to_device1_impl(T* dest, T* src, int N1);
template <typename T> void memcpy_host_to_device1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __reads("for i in 0..N1 -> &src[MINDEX1(N1,i)] ~~> A(i)");
  __writes("for i in 0..N1 -> &dest[MINDEX1(N1,i)] ~~>[GMem] A(i)");
  __ensures("__spec_override_noret()");
  __admitted();
  return memcpy_host_to_device1_impl<T>(dest, src, N1);
}

template <typename T> void memcpy_device_to_host1_impl(T* dest, T* src, int N1);
template <typename T> void memcpy_device_to_host1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __reads("for i in 0..N1 -> &src[MINDEX1(N1,i)] ~~>[GMem] A(i)");
  __writes("for i in 0..N1 -> &dest[MINDEX1(N1,i)] ~~> A(i)");
  __ensures("__spec_override_noret()");
  __admitted();
  return memcpy_device_to_host1_impl<T>(dest, src, N1);
}

template <typename T> T* __gmem_malloc2_impl(int N1, int N2);
template <typename T> T* __gmem_malloc2(int N1, int N2) {
  __preserves("HostCtx");
  __produces("_Res ~> UninitMatrixOf2(N1, N2, GMem)");
  __produces("Free(_Res, _Res ~> UninitMatrixOf2(N1, N2, GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __gmem_malloc2_impl<T>(N1);
}
#define gmem_malloc2(T, N1, N2) __call_with(__gmem_malloc2<T>(N1,N2), "T := "#T);

template <typename T> void memcpy_host_to_device2_impl(T* dest, T* src, int N1, int N2);
template <typename T> void memcpy_host_to_device2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __reads("src ~> Matrix2(N1,N2, A)");
  __writes("dest ~> MatrixOf2(N1,N2, GMem, A)");
  __ensures("__spec_override_noret()");
  __admitted();
  return memcpy_host_to_device2_impl<T>(dest, src, N1, N2);
}

template <typename T> void memcpy_device_to_host2_impl(T* dest, T* src, int N1, int N2);
template <typename T> void memcpy_device_to_host2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __reads("src ~> MatrixOf2(N1,N2, GMem, A)");
  __writes("dest ~> Matrix2(N1,N2, A)");
  __ensures("__spec_override_noret()");
  __admitted();
  return memcpy_device_to_host2_impl<T>(dest, src, N1, N2);
}

/* ---- DesyncGroup ghosts ---- */

__GHOST(rewrite_range) {
  __requires("rf: int -> Range");
  __requires("from: int, to: int");
  __requires("by: from = to");
  __ensures("range_eq(rf(from),rf(to))");
  __admitted();
}

__GHOST(rewrite_linear_range) {
  __requires("inside: Range -> HProp");
  __requires("from: Range, to: Range");
  __requires("by: range_eq(from, to)");
  __consumes("inside(from)");
  __produces("inside(to)");
  __admitted();
}

// TODO: handle > 2 cases where we have > MINDEX1
__GHOST(chunk_range_plus2) {
  __requires("D1: int, D2: int");
  __ensures("forall (i: int) -> range_eq( chunk_range(range_plus(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)), D2, i), range_plus(MINDEX2(D2,MSIZE1(D1),i,0), MSIZE1(D1)) )");
  __admitted();
}


__GHOST(group_to_desyncgroup1) {
  __requires("D1: int, items: int -> HProp");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE1(D1),0), MSIZE1(D1)))");
  __consumes("for i in 0..D1 -> items(i)");
  __produces("DesyncGroup(range_plus(MINDEX1(MSIZE1(D1),0), MSIZE1(D1)), D1, fun i -> items(i))");
  __admitted();
}

__GHOST(group_to_desyncgroup2) {
  __requires("D1: int, D2: int, items: int*int -> HProp");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)))");
  __consumes("for i in 0..D2 -> for j in 0..D1 -> items(i,j)");
  __produces("DesyncGroup(range_plus(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)), D2, fun i -> DesyncGroup(range_plus(MINDEX2(D2,MSIZE1(D1),i,0), MSIZE1(D1)), D1, fun j -> items(i,j) ) )");
  __admitted();
}

__GHOST(desyncgroup_tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "items: int -> HProp,"
    "br: Range, r: int -> Range,"
    "inner_range_check: forall (i: int) -> range_eq(chunk_range(br, tile_count, i), r(i)),"
    "positive_tile_size: tile_size >= 0"
  );
  __consumes("DesyncGroup(br, MSIZE2(tile_count,tile_size), items)");
  __produces("DesyncGroup(br, tile_count, fun bi -> "
               "DesyncGroup(r(bi), tile_size, fun i -> items(MINDEX2(tile_count, tile_size, bi, i) ) ) )");
  __admitted();
}

__GHOST(desyncgroup_untile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> HProp,"
    "rf: Range, br: Range, r: int -> Range,"
    "div_check: MSIZE2(tile_count,tile_size) = size,"
    "outer_range_check: range_eq(br, rf),"
    "positive_tile_size: tile_size >= 0"
  );
  __consumes("DesyncGroup(br, tile_count, fun bi -> "
    "DesyncGroup(r(bi), tile_size, fun i -> items(MINDEX2(tile_count, tile_size, bi, i)) ))");
  __produces("DesyncGroup(rf, size, items)");
  __admitted();
}

#endif // OPTITRUST_GPU_H
