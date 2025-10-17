#include "optitrust_common.h"
#include "optitrust_intrinsics.h"
#include <optitrust_models.h>

struct GPUThing{};

int ExecCtx = 0;

__DEF(add_x, "fun (x: float) (A: (int * int) -> float) -> fun (i j: int) -> A(i,j) +. x");

#define VECTORADD_CONTRACT\
  __requires("A: int * int -> float");\
  __consumes("arr ~> Matrix2(N, 32, A)");\
  __produces("arr ~> Matrix2(N, 32, add_x(5.f, A))");\

#define KERNEL_DECL(CONTRACT, KERNEL_NAME, ARGS, ...) \
void kernel_##KERNEL_NAME (__VA_ARGS__){ \
  __requires("f: _Fraction"); \
  __consumes("_RO(f, &ExecCtx ~~> 0)"); \
  __produces("_RO(f, &ExecCtx ~~> 0)"); \
  CONTRACT \
  __ghost([&]() { \
    __consumes("_RO(f, &ExecCtx ~~> 0)"); \
    __produces("_RO(f, &ExecCtx ~~> 1)"); \
    __admitted(); \
  });\
  KERNEL_NAME ARGS; \
  __ghost([&]() { \
    __consumes("_RO(f, &ExecCtx ~~> 1)"); \
    __produces("_RO(f, &ExecCtx ~~> 0)"); \
    __admitted(); \
  });\
  __admitted(); \
}

#define BLOCK_LOOP(IND, DIM, FRAC, BODY)\
  __ghost([&]() {\
    __consumes("_RO(" FRAC ", &ExecCtx ~~> 1)");\
    __produces("_RO(" FRAC ", &ExecCtx ~~> 2)");\
    __admitted();\
  });\
  for (int IND = 0; IND < DIM; IND ++) { \
    __xconsumes("_RO(" FRAC " / , &ExecCtx ~~> 2)");\
    BODY \
  }\
  __ghost([&]() { \
    __consumes("_RO(" FRAC ", &ExecCtx ~~> 2)"); \
    __produces("_RO(" FRAC ", &ExecCtx ~~> 1)"); \
    __admitted(); \
  });
/*
  __xrequires(FRAC#IND ": _Fraction, " FRAC#IND " = " FRAC "/" #DIM);\
    __xconsumes("_RO(" FRAC#IND ", &ExecCtx ~~> 3)");\
    __xproduces("_RO(" FRAC#IND ", &ExecCtx ~~> 3)");\
*/
#define THREAD_LOOP(IND, DIM, FRAC, BODY)\
  __ghost([&]() {\
    __consumes("_RO(" FRAC ", &ExecCtx ~~> 2)");\
    __produces("_RO(" FRAC ", &ExecCtx ~~> 3)");\
    __admitted();\
  });\
  for (int IND = 0; IND < DIM; IND ++) { \
    __sreads("&ExecCtx ~~> 3");\
    __xrequires(FRAC#IND": _Fraction");\
    BODY \
  }\
  __ghost([&]() { \
    __consumes("_RO(" FRAC ", &ExecCtx ~~> 3)"); \
    __produces("_RO(" FRAC ", &ExecCtx ~~> 2)"); \
    __admitted(); \
  });

void gpu_kernel(float *arr, int N) {
  __requires("f: _Fraction");
  __consumes("_RO(f, &ExecCtx ~~> 1)"); \
  __produces("_RO(f, &ExecCtx ~~> 1)"); \
  VECTORADD_CONTRACT

  BLOCK_LOOP(bx, N, "f",
    __xconsumes("for j in 0..32 -> &arr[MINDEX2(N,32,bx,j)] ~~> A(bx,j)");
    __xproduces("for j in 0..32 -> &arr[MINDEX2(N,32,bx,j)] ~~> add_x(5.f, A)(bx,j)");
    THREAD_LOOP(tx, 32, "fbx",
      __xconsumes("&arr[MINDEX2(N,32,bx,tx)] ~~> A(bx,tx)");
      __xproduces("&arr[MINDEX2(N,32,bx,tx)] ~~> add_x(5.f, A)(bx,tx)");
      arr[MINDEX2(N,32, bx, tx)] = arr[MINDEX2(N,32,bx,tx)] + 5.f;
    )
  )
}

KERNEL_DECL(
  VECTORADD_CONTRACT,
  gpu_kernel,
  (arr, N),
  float *arr, int N
)

void kernel_cpu(float *arr, int N) {
  VECTORADD_CONTRACT
  __reads("&ExecCtx ~~> 0");
  kernel_gpu_kernel(arr, N);
}

/*void please_call_me_with_one(int *x, int *res) {
  __reads("x ~~> 1");
  __writes("res ~~> 0");
  *res = (*x-1)/(*x);
  __ghost(assume, "P := (1-1)/1 = 0", "stuff <- H");
  __ghost(rewrite_linear, "inside := (fun v -> res ~~> v), by := stuff");
}

void um(int *x, int *res) {
  __reads("x ~~> 1");
  __reads("x ~~> 0");
  __writes("res ~~> 0");
  please_call_me_with_one(x, res);
}*/
