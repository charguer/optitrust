from ctypes import *
import numpy
from numpy.ctypeslib import ndpointer
import timeit
from statistics import median
import sys
import os
import tvm

implementation_given = len(sys.argv) > 1
if implementation_given:
  implementation_name, _ = os.path.splitext(os.path.basename(sys.argv[1]))
  so_functions = CDLL(sys.argv[1])
  mm = so_functions.mm
  mm.restype = None
  mm.argtypes = [
    ndpointer(c_float, flags="C_CONTIGUOUS"),
    ndpointer(c_float, flags="C_CONTIGUOUS"),
    ndpointer(c_float, flags="C_CONTIGUOUS"),
    c_int,
    c_int,
    c_int,
  ]

M = 1024
N = 1024
P = 1024
dt = "float32"

C = numpy.zeros((M, N), dtype=dt)
A = numpy.random.rand(M, P).astype(dt)
B = numpy.random.rand(P, N).astype(dt)

C_ref = [None]
def run_mm_ref():
  C_ref[0] = numpy.matmul(A, B)

custom_batches = {
  "matmul 'matmul'": 1,
  "matmul 'matmul0'": 1, # 1.42
  "matmul 'matmul1'": 2, # 0.47  x3
  "matmul 'matmul2'": 4, # 0.055 x8.5
  "matmul 'matmul3'": 8, # 0.020 x2.75, total x71
                   # Rise  0.012      , total x118, 1.6x over OptiTrust
                   # TVM   0.011      , total x129, 1.8x over OptiTrust
                   # numpy 0.007      , total x202, 2.8x over OptiTrust
}

def benchmark(msg, f):
  n_repeat = 10
  n_batch = custom_batches[msg] if msg in custom_batches else 10
  durations = timeit.repeat(f, repeat=n_repeat, number=n_batch)
  print("{:<25}: {:.4f}s median, range [{:.4f}; {:.4f}]s over {}x{} runs".format(
    msg, median(durations) / n_batch, min(durations) / n_batch, max(durations) / n_batch, n_repeat, n_batch))

if implementation_given:
  def run_mm():
    mm(C, A, B, M, N, P)

  benchmark("matmul '{}'".format(implementation_name), run_mm)

  run_mm_ref()
  numpy.testing.assert_allclose(C, C_ref[0], rtol=1e-5, equal_nan=False)
else:
  # -mcpu=core-avx2
  target = "llvm -mcpu=core-avx2"
  dev = tvm.device(target, 0)
  c = tvm.nd.array(C, dev)
  a = tvm.nd.array(A, dev)
  b = tvm.nd.array(B, dev)

  def build_mm_tvm():
    bn = 32
    kfactor = 4

    # Algorithm
    k = tvm.te.reduce_axis((0, P), "k")
    A = tvm.te.placeholder((M, P), name="A")
    B = tvm.te.placeholder((P, N), name="B")
    # C = te.compute((M, N), lambda m, n: te.sum(A[m, k] * B[k, n], axis=k), name="C")
    # needs to be rewritten:
    packedB = tvm.te.compute(
      (N / bn, P, bn), lambda bigN, k, littleN: B[k, bigN * bn + littleN], name="packedB"
    )
    C = tvm.te.compute(
      (M, N),
      lambda m, n: tvm.te.sum(A[m, k] * packedB[n // bn, k, tvm.tir.indexmod(n, bn)], axis=k),
      name="C",
    )

    # Schedule
    s = tvm.te.create_schedule(C.op)
    CC = s.cache_write(C, "global")
    mo, no, mi, ni = s[C].tile(C.op.axis[0], C.op.axis[1], bn, bn)
    s[CC].compute_at(s[C], no)
    mc, nc = s[CC].op.axis
    (kaxis,) = s[CC].op.reduce_axis
    ko, ki = s[CC].split(kaxis, factor=kfactor)
    s[CC].reorder(ko, mc, ki, nc)
    s[CC].vectorize(nc)
    s[CC].unroll(ki)
    s[C].parallel(mo)
    bigN, _, littleN = s[packedB].op.axis
    s[packedB].vectorize(littleN)
    s[packedB].parallel(bigN)

    # print(tvm.lower(s, [A, B, C], simple_mode=True))

    return tvm.build(s, [A, B, C], target=target, name="mm")

  mm_tvm = build_mm_tvm()
  def run_mm_tvm():
    mm_tvm(a, b, c)

  benchmark("matmul numpy", run_mm_ref)
  benchmark("matmul TVM", run_mm_tvm)
  numpy.testing.assert_allclose(c.numpy(), C_ref[0], rtol=1e-5, equal_nan=False)
