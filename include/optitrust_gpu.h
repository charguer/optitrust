#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

extern const int __threadfor;

__DECL(GMem, "MemType");
__DECL(SMem, "MemType");
__DECL(KernelParams, "int * int * int -> HProp");
__DECL(SMemAlloc, "int -> HProp");
__DECL(DeadKernelCtx, "HProp");

__DECL(chunk_range, "Range * int * int -> Range");
__DECL(range_eq, "Range * Range -> Prop");

/* --- Kernel launches ---- */

__DECL(HostCtx, "HProp");

void kernel_start(int tpb, int bpg, int smem_sz) {
  __requires("r: Range");
  __requires("by: range_eq(counted_range(MINDEX1(bpg * tpb, 0), bpg * tpb), r)");
  __consumes("HostCtx");
  __produces("ThreadsCtx(r)");
  __produces("KernelParams(tpb, bpg, smem_sz)");
  __admitted();
}

void kernel_end() {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __consumes("DeadKernelCtx");
  __consumes("KernelParams(tpb, bpg, smem_sz)");
  __produces("HostCtx");
  __admitted();
}

__GHOST(kill_threads) {
  __requires("r: Range");
  __requires("tpb: int, bpg: int, smem_sz: int");
  __preserves("KernelParams(tpb, bpg, smem_sz)");
  __requires("by: range_eq(counted_range(MINDEX1(bpg * tpb, 0), bpg * tpb), r)");
  __consumes("ThreadsCtx(r)");
  __produces("DeadKernelCtx");
  __admitted();
}

/* --- Synchronization ---- */

__DECL(block_sync_mem, "MemType -> Prop");
__AXIOM(gmem_block_sync_mem, "block_sync_mem(GMem)");
__AXIOM(smem_block_sync_mem, "block_sync_mem(SMem)");

void blocksync() {
  __requires("H: HProp, tpb: int, bpg: int, smem_sz: int, t: int");
  __reads("KernelParams(tpb, bpg, smem_sz)");
  __preserves("ThreadsCtx(counted_range(t, tpb))");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

__GHOST(kernel_end_sync) {
  __requires("H: HProp");
  __reads("DeadKernelCtx");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

/* --- Memory management ---- */

// To appease C++ typechecker
template <typename T> T __get_sig_generic(T* p);
template <typename T> T* __alloc_sig_generic();

template <typename T> T __gmem_get(T* p) {
  __requires("v: T, t: int");
  __preserves("ThreadsCtx(counted_range(t, MSIZE0()))");
  __reads("p ~~>[GMem] v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __get_sig_generic<T>(p);
}

template <typename T> void __gmem_set(T* p, T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(counted_range(t, MSIZE0()))");
  __writes("p ~~>[GMem] v");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> void gmem_free(T* p) {
  __requires("H: HProp");
  __preserves("HostCtx");
  __consumes("Free(p, H)");
  __consumes("H");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> T* __gmem_malloc1(int N1) {
  __preserves("HostCtx");
  __produces("_Res ~> UninitMatrixOf1(N1, GMem)");
  __produces("Free(_Res, _Res ~> UninitMatrixOf1(N1, GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define GMEM_MALLOC1(T, N1) __call_with(__gmem_malloc1<T>(N1), "T := "#T)

template <typename T> T* __gmem_malloc2(int N1, int N2) {
  __preserves("HostCtx");
  __produces("_Res ~> UninitMatrixOf2(N1, N2, GMem)");
  __produces("Free(_Res, _Res ~> UninitMatrixOf2(N1, N2, GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define GMEM_MALLOC2(T, N1, N2) __call_with(__gmem_malloc2<T>(N1,N2), "T := "#T)

template <typename T> void memcpy_host_to_device1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix1(N1, A)");
  __writes("dest ~> MatrixOf1(N1, GMem, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}
template <typename T> void memcpy_host_to_device2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix2(N1,N2, A)");
  __writes("dest ~> MatrixOf2(N1,N2, GMem, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> void memcpy_device_to_host1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __preserves("HostCtx");
  __reads("src ~> MatrixOf1(N1, GMem, A)");
  __writes("dest ~> Matrix1(N1, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}
template <typename T> void memcpy_device_to_host2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __preserves("HostCtx");
  __reads("src ~> MatrixOf2(N1,N2, GMem, A)");
  __writes("dest ~> Matrix2(N1,N2, A)");
  __ensures("__spec_override_noret()");
  __admitted();
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

// TODO: Add more of these? Or change the thread for typechecker rule to produce the chunk_range equality automatically?
__GHOST(chunk_counted_range2) {
  __requires("D1: int, D2: int");
  __ensures("P: forall (i: int) -> range_eq( chunk_range(counted_range(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)), D2, i), counted_range(MINDEX2(D2,MSIZE1(D1),i,0), MSIZE1(D1)) )");
  __admitted();
}
__GHOST(chunk_counted_range4) {
  __requires("D1: int, D2: int, D3: int, D4: int");
  __ensures("P: forall (i: int) -> range_eq( chunk_range(counted_range(MINDEX1(MSIZE4(D4,D3,D2,D1),0), MSIZE4(D4,D3,D2,D1)), D4, i), counted_range(MINDEX2(D4,MSIZE3(D3,D2,D1),i,0), MSIZE3(D3,D2,D1)) )");
  __admitted();
}

__GHOST(group_to_desyncgroup) {
  __requires("N: int, items: int -> HProp, r: Range");
  __preserves("ThreadsCtx(r)");
  __consumes("for i in 0..N -> items(i)");
  __produces("desync_for(r) i in ..N -> items(i)");
  __admitted();
}

// TODO: Consider removing?
// have just one standard way of doing the conversion to avoid confusion (group_to_desyncgroup alone is equally expressive)
__GHOST(group_to_desyncgroup2) {
  __requires("D1: int, D2: int, items: int*int -> HProp");
  __preserves("ThreadsCtx(counted_range(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)))");
  __consumes("for i in 0..D2 -> for j in 0..D1 -> items(i,j)");
  __produces("DesyncGroup(counted_range(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)), D2, fun i -> DesyncGroup(counted_range(MINDEX2(D2,MSIZE1(D1),i,0), MSIZE1(D1)), D1, fun j -> items(i,j) ) )");
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
