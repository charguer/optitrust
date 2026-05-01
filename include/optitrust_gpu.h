#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

extern const int __threadfor;
extern const int __magic_threadfor;
#define __device extern void __device__()
extern const int __device_call;
extern const int __barrier_sequence;

__DECL(GMem, "MemType");
__DECL(SMem, "MemType");
__DECL(TReg, "MemType");
__DECL(KernelParams, "int * int * int -> HProp");
__DECL(SMemAllowance, "int -> HProp");
__DECL(SMemToken, "int -> HProp");
__DECL(KernelSetupCtx, "HProp");
__DECL(KernelTeardownCtx, "HProp");

// LATER: make the info about size and memory cell part of the normal "Free" token,
// or add support for polymorphic pure functions to make this work
// for now we add size variants of smem_free
// __DECL(SMemFree, "T: Type -> ptr(T) -> int -> MemType -> HProp");

__GHOST(rewrite_threadsctx_sz) {
  __requires("from: int, to: int, start: int");
  __requires("by: from = to");
  __consumes("ThreadsCtx(start ..+ from)");
  __produces("ThreadsCtx(start ..+ to)");
  __ghost(rewrite_linear, "inside := fun i -> ThreadsCtx(start ..+ i), by := by");
}

__GHOST(rewrite_threadsctx_sz1) {
  __requires("from: int, to: int, start: int");
  __requires("by: from = to");
  __consumes("ThreadsCtx(start ..+ MSIZE1(from))");
  __produces("ThreadsCtx(start ..+ MSIZE1(to))");
  __ghost(rewrite_linear, "inside := fun i -> ThreadsCtx(start ..+ MSIZE1(i)), by := by");
}

/* --- Kernel launches ---- */

__DECL(HostCtx, "HProp");

void kernel_launch(int bpg, int tpb, int smem_sz) {
  __consumes("HostCtx");
  __produces("KernelSetupCtx");
  __produces("KernelParams(bpg, tpb, smem_sz)");
  __produces("SMemAllowance(smem_sz)");
  __admitted();
}

void kernel_setup_end() {
  __requires("bpg: int, tpb: int, smem_sz: int, grid_sz: int");
  __requires("by: bpg * tpb = grid_sz");
  __consumes("KernelSetupCtx");
  __produces("ThreadsCtx(MINDEX1(0, 0) ..+ grid_sz)");
  __preserves("KernelParams(bpg, tpb, smem_sz)");
  __preserves("SMemAllowance(0)"); // Must allocate all shared memory requested
  __admitted();
}

void kernel_teardown_begin() {
  __requires("bpg: int, tpb: int, smem_sz: int, grid_sz: int");
  __requires("by: bpg * tpb = grid_sz");
  __consumes("ThreadsCtx(MINDEX1(0, 0) ..+ grid_sz)");
  __produces("KernelTeardownCtx");
  __preserves("KernelParams(bpg, tpb, smem_sz)");
  __preserves("SMemAllowance(0)"); // Shared memory is still allocated when teardown begins
  __admitted();
}

void kernel_kill() {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __consumes("KernelParams(bpg, tpb, smem_sz)");
  __consumes("KernelTeardownCtx");
  __consumes("SMemAllowance(smem_sz)"); // Must give back all shared memory
  __produces("HostCtx");
  __admitted();
}

__GHOST(take_smem_token) {
  __requires("tok_sz: int, smem_sz_rem: int");
  __consumes("SMemAllowance(tok_sz + smem_sz_rem)");
  __produces("SMemAllowance(smem_sz_rem)");
  __produces("SMemToken(tok_sz)");
  __admitted();
}

__GHOST(give_smem_token) {
  __reverts(take_smem_token);
  __admitted();
}

/* --- Synchronization ---- */

inline void magic_barrier() {}

__DECL(block_sync_mem, "MemType -> Prop");
__AXIOM(gmem_block_sync_mem, "block_sync_mem(GMem)");
__AXIOM(smem_block_sync_mem, "block_sync_mem(SMem)");

// LATER: should take a list of HPROP
void blocksync() {
  __requires("H: HProp, tpb: int, bpg: int, smem_sz: int, t: int");
  __reads("KernelParams(bpg, tpb, smem_sz)");
  __preserves("ThreadsCtx(t ..+ tpb)");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

// LATER: should take a list of HPROP (and could be merged with kernel_device_end at that point)
__GHOST(kernel_teardown_sync) {
  __requires("H: HProp");
  __reads("KernelTeardownCtx");
  __consumes("H");
  __produces("Sync(block_sync_mem, H)");
  __admitted();
}

/* --- Memory management ---- */

// To appease C++ typechecker
template <typename T> T __get_sig_generic(T* p);
template <typename T> T* __alloc_sig_generic();

// Global memory

template <typename T> T __gmem_get(T* p) {
  __requires("v: T, t: int");
  __reads("ThreadsCtx(t ..+ MSIZE0())");
  __reads("p ~~>[GMem] v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __get_sig_generic<T>(p);
}

template <typename T> void __gmem_set(T* p, T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(t ..+ MSIZE0())");
  __writes("p ~~>[GMem] v");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> void gmem_free(T* p) {
  __requires("H: HProp");
  __preserves("HostCtx");
  // LATER: Free(p,H) needs to store a memory type as well.
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

// Shared memory

template <typename T> T __smem_get(T* p) {
  __requires("v: T, t: int");
  __reads("ThreadsCtx(t ..+ MSIZE0())");
  __reads("p ~~>[SMem] v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __get_sig_generic<T>(p);
}

template <typename T> void __smem_set(T* p, T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(t ..+ MSIZE0())");
  __writes("p ~~>[SMem] v");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> T* __smem_malloc1(int N1) {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __preserves("KernelSetupCtx");
  __reads("KernelParams(bpg,tpb,smem_sz)");
  // LATER: matrix sugar for this
  __produces("desync_for i in ..bpg -> for j1 in 0..N1 -> &_Res[MINDEX2(bpg, N1, DMINDEX1(bpg, i), j1)] ~> UninitCellOf(SMem)");
  // expect a permission of groups, not desyncgroups. Should sync (end kernel) first before freeing.
  __produces("Free(_Res, for i in 0..bpg -> for j1 in 0..N1 -> &_Res[MINDEX2(bpg, N1, DMINDEX1(bpg, i), j1)] ~> UninitCellOf(SMem))");
  __consumes("SMemToken(sizeof(T)*N1)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define SMEM_MALLOC1(T, N1) __call_with(__smem_malloc1<T>(N1), "T := "#T)

template <typename T> T* __smem_malloc2(int N1, int N2) {
  __requires("tpb: int, bpg: int, smem_sz: int, smem_sz_rem: int");
  __preserves("KernelSetupCtx");
  __reads("KernelParams(bpg,tpb,smem_sz)");
  __produces("desync_for i in ..bpg -> for j1 in 0..N1 -> for j2 in 0..N2 -> &_Res[MINDEX3(bpg, N1, N2, DMINDEX1(bpg, i), j1, j2)] ~> UninitCellOf(SMem)");
  __produces("Free(_Res, for i in 0..bpg -> for j1 in 0..N1 -> for j2 in 0..N2 -> &_Res[MINDEX3(bpg, N1, N2, DMINDEX1(bpg, i), j1, j2)] ~> UninitCellOf(SMem))");
  __consumes("SMemToken(sizeof(T)*(N1*N2))");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define SMEM_MALLOC2(T, N1, N2) __call_with(__smem_malloc2<T>(N1, N2), "T := "#T)

// LATER: should be able to get away with just one smem_free, but since the Free token
// doesn't store size, we can't.
template <typename T> void __smem_free1(T* p, int N1) {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __preserves("KernelTeardownCtx");
  __reads("KernelParams(bpg,tpb,smem_sz)");
  __consumes("for i in 0..bpg -> for j1 in 0..N1 -> &p[MINDEX2(bpg, N1, DMINDEX1(bpg, i), j1)] ~> UninitCellOf(SMem)");
  __consumes("Free(p, for i in 0..bpg -> for j1 in 0..N1 -> &p[MINDEX2(bpg, N1, DMINDEX1(bpg, i), j1)] ~> UninitCellOf(SMem))");
  __produces("SMemToken(sizeof(T)*N1)");
  __ensures("__spec_override_noret()");
  __admitted();
}

template <typename T> void __smem_free2(T* p, int N1, int N2) {
  __requires("tpb: int, bpg: int, smem_sz: int");
  __preserves("KernelTeardownCtx");
  __reads("KernelParams(bpg,tpb,smem_sz)");
  __consumes("for i in 0..bpg -> for j1 in 0..N1 -> for j2 in 0..N2 -> &p[MINDEX3(bpg, N1, N2, DMINDEX1(bpg, i), j1, j2)] ~> UninitCellOf(SMem)");
  __consumes("Free(p, for i in 0..bpg -> for j1 in 0..N1 -> for j2 in 0..N2 -> &p[MINDEX3(bpg, N1, N2, DMINDEX1(bpg, i), j1, j2)] ~> UninitCellOf(SMem))");
  __produces("SMemToken(sizeof(T)*(N1*N2))");
  __ensures("__spec_override_noret()");
  __admitted();
}

// Thread registers

template <typename T> T* __treg_ref(T v) {
  __requires("t: int, sz: int");
  __preserves("ThreadsCtx(t..+sz)");
  __produces("desync_for i in ..sz -> &_Res[MINDEX1(sz, DMINDEX1(sz, i))] ~~>[TReg] v");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define TREG_REF(T, v) __call_with(__treg_ref<T>(v), "T := "#T)

// specialized version of above with singleton ghost built in
template <typename T> T* __treg_ref_s(T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(t..+MSIZE0())");
  __produces("_Res ~~>[TReg] v");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  // admitted for now because proper autofree/typechecking for TReg is not implemented
  __admitted();
  // but these steps should be correct
  T* const p = TREG_REF(T, v);
  __ghost(unwrap_singleton_desyncgroup, "H := fun i -> &p[MINDEX1(MSIZE0(), DMINDEX1(MSIZE0(), i))] ~~>[TReg] v");
  __ghost(singleton_mindex_simplify, "H := fun (g: ptr(T)) -> g ~~>[TReg] v, p := p");
  return p;
}
#define TREG_REF_S(T,v) __call_with(__treg_ref_s<T>(v), "T := "#T)

template <typename T> T* __treg_ref_uninit0() {
  __requires("t: int, sz: int");
  __preserves("ThreadsCtx(t..+sz)");
  __produces("desync_for i in ..sz -> &_Res[MINDEX1(sz, DMINDEX1(sz, i))] ~> UninitCellOf(TReg)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define TREG_REF_UNINIT0(T) __call_with(__treg_ref_uninit0<T>(), "T := "#T)

// specialized version of above with singleton ghost built in
template <typename T> T* __treg_ref_uninit0_s() {
  __requires("t: int");
  __preserves("ThreadsCtx(t..+MSIZE0())");
  __produces("_Res ~> UninitCellOf(TReg)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  // admitted for now because proper autofree/typechecking for TReg is not implemented
  __admitted();
  // but these steps should be correct
  T* const p = TREG_REF_UNINIT0(T);
  __ghost(unwrap_singleton_desyncgroup, "H := fun i -> &p[MINDEX1(MSIZE0(), DMINDEX1(MSIZE0(), i))] ~> UninitCellOf(TReg)");
  __ghost(singleton_mindex_simplify, "H := fun p -> p ~> UninitCellOf(TReg), p := p");
  return p;
}
#define TREG_REF_UNINIT0_S(T) __call_with(__treg_ref_uninit0_s<T>(), "T := "#T)

template <typename T> T* __treg_ref_uninit1(int N1) {
  __requires("t: int, sz: int");
  __preserves("ThreadsCtx(t..+sz)");
  __produces("desync_for i in ..sz -> for j1 in 0..N1 -> &_Res[MINDEX2(sz, N1, DMINDEX1(sz, i), j1)] ~> UninitCellOf(TReg)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define TREG_REF_UNINIT1(T, N1) __call_with(__treg_ref_uninit1<T>(N1), "T := "#T)

template <typename T> T* __treg_ref_uninit2(int N1, int N2) {
  __requires("t: int, sz: int");
  __preserves("ThreadsCtx(t..+sz)");
  __produces("desync_for i in ..sz -> for j1 in 0..N1 -> for j2 in 0..N2 -> &_Res[MINDEX3(sz, N1, N2, DMINDEX1(sz, i), j1, j2)] ~> UninitCellOf(TReg)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __alloc_sig_generic<T>();
}
#define TREG_REF_UNINIT2(T, N1, N2) __call_with(__treg_ref_uninit2<T>(N1, N2), "T := "#T)

template <typename T> T __treg_get(T* p) {
  __requires("v: T, t: int");
  __reads("ThreadsCtx(t ..+ MSIZE0())");
  __reads("p ~~>[TReg] v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __get_sig_generic<T>(p);
}

template <typename T> void __treg_set(T* p, T v) {
  __requires("t: int");
  __preserves("ThreadsCtx(t ..+ MSIZE0())");
  __writes("p ~~>[TReg] v");
  __ensures("__spec_override_noret()");
  __admitted();
}

#endif // OPTITRUST_GPU_H
