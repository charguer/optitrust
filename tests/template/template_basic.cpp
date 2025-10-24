#include "optitrust_common.h"
#include "optitrust_intrinsics.h"
#include <optitrust_models.h>

template <typename T> T __GET_TEST_IMPL(T* p);
template <typename T> T __GET_TEST(T* p) {
  __requires("v: T");
  __reads("p ~~> v");
  __ensures("__spec_override_ret(T, v)");
  __admitted();
  return __GET_TEST_IMPL(p);
}

template <typename T> void __SET_TEST_IMPL(T* p, T v);
template <typename T> void __SET_TEST(T* p, T v) {
  __writes("p ~~> v");
  __ensures("__spec_override_noret()");
  __admitted();
  __SET_TEST_IMPL(p, v);
}

// not the right alloc definition (needs to give the Free resource & etc.)
// but doesn't matter for this unit test
template <typename T> T* __ALLOC_TEST_IMPL();
template <typename T> T* __ALLOC_TEST() {
  __produces("_Res ~> UninitCell");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __admitted();
  return __ALLOC_TEST_IMPL<T>();
}
#define __ALLOC_TEST_MACRO(T) (T*)__ALLOC_TEST<T>();__with("T := "#T)

template <typename T> void __FREE_TEST_IMPL(T* p);
template <typename T> void __FREE_TEST(T* p) {
  __consumes("p ~> UninitCell");
  __ensures("__spec_override_noret()");
  __admitted();
  __FREE_TEST_IMPL(p);
}

void test(int *a, int *b) {
  __consumes("b ~> UninitCell");
  __writes("a ~~> 100");

  // TODO: not possible to do this just yet: complains with error
  //     Failure("Could not find a prototype for trm at location Unknown location")
  // because of the "__with" binding, and
  // without the with binding, just pulls a random T from context
  // int *b = (int*)__GMEM_ALLOC_FN<int>(); __with("T := int");

  __SET_TEST(b, 100);

  __SET_TEST(a, __GET_TEST(b));

  __FREE_TEST(b);

}
