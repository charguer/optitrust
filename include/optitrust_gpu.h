#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

extern const int __threadfor;
#define __device extern void __device__()
extern const int __device_call;

__DECL(GMem, "MemType");
__DECL(SMem, "MemType");
__DECL(KernelParams, "int * int * int -> HProp");
__DECL(SMemAlloc, "int -> HProp");
__DECL(KernelSetupCtx, "HProp");
__DECL(KernelTeardownCtx, "HProp");

__DECL(range_eq, "Range * Range -> Prop"); // TODO move this outside this file

__GHOST(rewrite_threadsctx_sz) {
  __requires("from: int, to: int, start: int");
  __requires("by: from = to");
  __consumes("ThreadsCtx(start ..+ from)");
  __produces("ThreadsCtx(start ..+ to)");
  __ghost(rewrite_linear, "inside := fun i -> ThreadsCtx(start ..+ i), by := by");
}

/* --- Kernel launches ---- */

__DECL(HostCtx, "HProp");

void kernel_launch(int bpg, int tpb, int smem_sz) {
  __consumes("HostCtx");
  __produces("KernelSetupCtx");
  __produces("KernelParams(bpg, tpb, smem_sz)");
  __admitted();
}

void kernel_setup_end() {
  __requires("bpg: int, tpb: int, smem_sz: int, tctx_sz: int");
  __requires("by: bpg * tpb = tctx_sz");
  __consumes("KernelSetupCtx");
  __produces("ThreadsCtx(MINDEX1(0, 0) ..+ tctx_sz)");
  __preserves("KernelParams(bpg, tpb, smem_sz)");
  __admitted();
}

void kernel_teardown_begin() {
  __requires("bpg: int, tpb: int, smem_sz: int, tctx_sz: int");
  __requires("by: bpg * tpb = tctx_sz");
  __consumes("ThreadsCtx(MINDEX1(0, 0) ..+ tctx_sz)");
  __produces("KernelTeardownCtx");
  __preserves("KernelParams(bpg, tpb, smem_sz)");
  __admitted();
}

void kernel_kill() {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __consumes("KernelParams(bpg, tpb, smem_sz)");
  __consumes("KernelTeardownCtx");
  __produces("HostCtx");
  __admitted();
}

/* --- Synchronization ---- */

__DECL(block_sync_mem, "MemType -> Prop");
__AXIOM(gmem_block_sync_mem, "block_sync_mem(GMem)");
__AXIOM(smem_block_sync_mem, "block_sync_mem(SMem)");

// TODO should take a list of HPROP
void blocksync() {
  __requires("H: HProp, tpb: int, bpg: int, smem_sz: int, t: int");
  __reads("KernelParams(bpg, tpb, smem_sz)");
  __preserves("ThreadsCtx(counted_range(t, tpb))");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

// TODO could be merged with kernel_device_end when a list of HProps is supported
__GHOST(kernel_teardown_sync) {
  __requires("H: HProp");
  __reads("KernelTeardownCtx");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

__DECL(is_mem_any, "MemType -> Prop");
__AXIOM(any_is_mem_any, "is_mem_any(Any)");

void magicsync() {
  __requires("H: HProp");
  __consumes("H");
  __produces("Sync(is_mem_any, H)");
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
  // TODO: Free(p,H) needs to store a memory type as well.
  // Don't know that `p` is a pointer to GMem, or that H is a permission on GMem cells.
  __consumes("Free(p, H)");
  __consumes("H");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> T* __gmem_malloc1(int N1) {
  __preserves("HostCtx");
  __produces("_Res ~> UninitMatrix1Of(N1, GMem)");
  __produces("Free(_Res, _Res ~> UninitMatrix1Of(N1, GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define GMEM_MALLOC1(T, N1) __call_with(__gmem_malloc1<T>(N1), "T := "#T)

template <typename T> T* __gmem_malloc2(int N1, int N2) {
  __preserves("HostCtx");
  __produces("_Res ~> UninitMatrix2Of(N1, N2, GMem)");
  __produces("Free(_Res, _Res ~> UninitMatrix2Of(N1, N2, GMem))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define GMEM_MALLOC2(T, N1, N2) __call_with(__gmem_malloc2<T>(N1,N2), "T := "#T)

template <typename T> void memcpy_host_to_device1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix1(N1, A)");
  __writes("dest ~> Matrix1Of(N1, GMem, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}
template <typename T> void memcpy_host_to_device2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix2(N1,N2, A)");
  __writes("dest ~> Matrix2Of(N1,N2, GMem, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> void memcpy_device_to_host1(T* dest, T* src, int N1) {
  __requires("A: int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix1Of(N1, GMem, A)");
  __writes("dest ~> Matrix1(N1, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}
template <typename T> void memcpy_device_to_host2(T* dest, T* src, int N1, int N2) {
  __requires("A: int * int -> T");
  __preserves("HostCtx");
  __reads("src ~> Matrix2Of(N1,N2, GMem, A)");
  __writes("dest ~> Matrix2(N1,N2, A)");
  __ensures("__spec_override_noret()");
  __admitted();
}

/* ---- DesyncGroup ghosts ---- */

__GHOST(group_to_desyncgroup) {
  __requires("N: int, items: int -> HProp, r: Range");
  __preserves("ThreadsCtx(r)");
  __consumes("for i in 0..N -> items(i)");
  __produces("desync_for i in ..N -> items(i)");
  __admitted();
}

__GHOST(desync_tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> HProp,"
    "div_check: size = tile_count * tile_size,"
    "positive_tile_size: tile_size >= 0"
  );
  __consumes("DesyncGroup(size, items)");
  __produces("desync_for bi in ..tile_count ->"
               "desync_for i in ..tile_size -> items(bi * tile_size + i)");
  __admitted();
}

__GHOST(desync_untile_divides) {
  __reverts(desync_tile_divides);
  __admitted();
}


// TODO move these range ghosts

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

#endif // OPTITRUST_GPU_H
