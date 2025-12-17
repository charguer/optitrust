#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

#include <optitrust_models.h>

__DECL(GMem, "MemType");

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

// TODO: These need to be written properly using the "Free" resource & etc.
// can't just free arbitrary resources
template <typename T> T* __GMEM_ALLOC_FN_IMPL();
template <typename T> T* __GMEM_ALLOC_FN() {
  __requires("t: int");
  __preserves("ThreadsCtx(range_plus(t, MSIZE0()))");
  __produces("_Res ~> UninitCellOf(GMem)");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __GMEM_ALLOC_FN_IMPL<T>();
}
#define __GMEM_ALLOC(T) __call_with(__GMEM_ALLOC_FN<T>(), "T := "#T)

template <typename T> void __GMEM_FREE_IMPL(T* p);
template <typename T> void __GMEM_FREE(T* p) {
  __requires("t: int");
  __preserves("ThreadsCtx(range_plus(t, MSIZE0()))");
  __consumes("p ~> UninitCellOf(GMem)");
  __ensures("__spec_override_noret()");
  __admitted();
  __GMEM_FREE_IMPL(p);
}


#endif // OPTITRUST_GPU_H
