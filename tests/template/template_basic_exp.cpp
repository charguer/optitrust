#include <optitrust_models.h>

#include "optitrust_common.h"
#include "optitrust_intrinsics.h"

template <typename T>
T __GET_TEST_IMPL(T*)

    T __GET_TEST(T* p) {
  __requires("T: Type");
  __requires("p: T*");
  __requires("v: T");
  __ensures("__spec_override_ret(T, v)");
  __reads("p ~~> v");
  __admitted();
  return __GET_TEST_IMPL(p);
}

template <typename T>
void __SET_TEST_IMPL(T*, T)

    void __SET_TEST(T* p, T v) {
  __requires("T: Type");
  __requires("v: T");
  __requires("p: T*");
  __ensures("__spec_override_noret()");
  __writes("p ~~> v");
  __admitted();
  __SET_TEST_IMPL(p, v);
}

template <typename T>
T* __ALLOC_TEST_IMPL()

    T* __ALLOC_TEST() {
  __requires("T: Type");
  __ensures("__spec_override_ret_implicit(ptr(T))");
  __produces("_Res ~> UninitCell");
  __admitted();
  return __ALLOC_TEST_IMPL();
}

template <typename T>
void __FREE_TEST_IMPL(T*)

    void __FREE_TEST(T* p) {
  __requires("T: Type");
  __requires("p: T*");
  __consumes("p ~> UninitCell");
  __ensures("__spec_override_noret()");
  __admitted();
  __FREE_TEST_IMPL(p);
}

void test(int* a, int* b) {
  __consumes("b ~> UninitCell");
  __writes("a ~~> 100");
  __SET_TEST(b, 100);
  __SET_TEST(a, __GET_TEST(b));
  __FREE_TEST(b);
}
