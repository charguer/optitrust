from ctypes import *
import numpy
from numpy.ctypeslib import ndpointer
from statistics import median
import sys
import time
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

# rotate through 10 arrays to mitigate cache interference
# numpy.random.seed()
n_bench = 20
n_allocs = n_bench
C = [ numpy.zeros((M, N), dtype=dt) for i in range(0, n_allocs) ]
A = [ numpy.random.rand(M, P).astype(dt) for i in range(0, n_allocs) ]
B = [ numpy.random.rand(P, N).astype(dt) for i in range(0, n_allocs) ]

C_ref = [ numpy.zeros((M, N), dtype=dt) for i in range(0, n_allocs) ]
def run_mm_ref(i):
  numpy.matmul(A[i], B[i], out=C_ref[i])

def benchmark(msg, f):
  durations_s = []
  for i in range(0, n_bench):
    start = time.time_ns()
    f(i)
    stop = time.time_ns()
    durations_s.append((stop - start) / (10 ** 9))
  print("{:<40}: {:.4f} s median, range [{:.4f}; {:.4f}] s over {} runs".format(
    msg, median(durations_s), min(durations_s), max(durations_s), n_bench))

if implementation_given:
  def run_mm(i):
    mm(C[i], A[i], B[i], M, N, P)

  benchmark("matmul '{}'".format(implementation_name), run_mm)

  for i in range(0, n_bench):
    run_mm_ref(i)
    numpy.testing.assert_allclose(C[i], C_ref[i], rtol=1e-5, equal_nan=False)
else:
  # -mcpu=core-avx2
  target = "llvm -mcpu=core-avx2"

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

  dev = tvm.device(target, 0)
  to_dev = lambda x: tvm.nd.array(x, dev)
  c = list(map(to_dev, C))
  a = list(map(to_dev, A))
  b = list(map(to_dev, B))
  def run_mm_tvm(i):
    mm_tvm(a[i], b[i], c[i])

  benchmark("matmul numpy", run_mm_ref)
  benchmark("matmul TVM", run_mm_tvm)
  for (a, b) in zip(c, C_ref):
    numpy.testing.assert_allclose(a.numpy(), b, rtol=1e-5, equal_nan=False)
