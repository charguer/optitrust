#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

extern const int __threadfor;

__DECL(GMem, "MemType");
__DECL(KernelParams, "int * int * int -> HProp");

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

/* ---- DesyncGroup ghosts ---- */


__DECL(chunk_range, "Range * int * int -> Range");
__DECL(range_eq, "Range * Range -> Prop");

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
