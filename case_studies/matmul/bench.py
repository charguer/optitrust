from ctypes import *
import numpy
from numpy.ctypeslib import ndpointer
#from statistics import median, stdev
import sys
import time
import os
import tvm

implementation_given = len(sys.argv) > 1
implementation_name = None
if implementation_given:
  implementation_name, _ = os.path.splitext(os.path.basename(sys.argv[1]))
  so_functions = CDLL(sys.argv[1])
  # mm = so_functions.mm
  # mm.restype = None
  # mm.argtypes = [
  #   ndpointer(c_float, flags="C_CONTIGUOUS"),
  #   ndpointer(c_float, flags="C_CONTIGUOUS"),
  #   ndpointer(c_float, flags="C_CONTIGUOUS"),
  #   c_int,
  #   c_int,
  #   c_int,
  # ]
  mm1024 = so_functions.mm1024
  mm1024.restype = None
  mm1024.argtypes = [
    ndpointer(c_float, flags="C_CONTIGUOUS"),
    ndpointer(c_float, flags="C_CONTIGUOUS"),
    ndpointer(c_float, flags="C_CONTIGUOUS"),
  ]

M = 1024
N = 1024
P = 1024
dt = "float32"

# rotate through 10 arrays to mitigate cache interference
# LATER: numpy.random.seed()
n_allocs = 10
# TODO: adapt n_bench to confidence intervals / percentiles
n_bench = 200 if implementation_name != "matmul" else 10
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
    f(i % n_allocs) # TODO: is this OK if n_bench % n_allocs != 0 ?
    stop = time.time_ns()
    durations_s.append((stop - start) / (10 ** 9))
  qs = [1, 5, 10, 25, 50, 75, 90, 95, 99]
  percentiles = numpy.percentile(durations_s, qs) # , method='median_unbiased'
  print("{:<40}: {:.4f} s median, range [{:.4f}; {:.4f}], over {} runs".format(
    msg, numpy.median(durations_s), numpy.min(durations_s), numpy.max(durations_s), n_bench))
  for (q, p) in zip(qs, percentiles):
    print("{}th percentile: {:.4f}".format(q, p))
  # print("raw: {}".format(durations_s))
  print()

if implementation_given:
  def run_mm(i):
    mm1024(C[i], A[i], B[i])

  benchmark("matmul '{}'".format(implementation_name), run_mm)

  for i in range(0, n_allocs):
    run_mm_ref(i)
    numpy.testing.assert_allclose(C[i], C_ref[i], rtol=1e-5, equal_nan=False)
else:
  # -mcpu=core-avx2
  target = "llvm -mcpu=core-avx2"

  def dump(make_s, f):
    if False: # set to True to dump
      f = open(f, "w")
      f.write(make_s())
      f.close()

  def build_mm_tvm():
    bn = 32
    kfactor = 4

    # Algorithm
    # k = tvm.te.reduce_axis((0, P), "k")
    # A = tvm.te.placeholder((M, P), name="A")
    # B = tvm.te.placeholder((P, N), name="B")
    # C = te.compute((M, N), lambda m, n: te.sum(A[m, k] * B[k, n], axis=k), name="C")

    # needs to be rewritten:

    # Algorithm 2
    k = tvm.te.reduce_axis((0, P), "k")
    A = tvm.te.placeholder((M, P), name="A")
    B = tvm.te.placeholder((P, N), name="B")
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

    dump(lambda: str(tvm.lower(s, [A, B, C], name="mm")), "tvm_ir")

    return tvm.build(s, [A, B, C], target=target, name="mm")

  mm_tvm = build_mm_tvm()

  dump(mm_tvm.get_source, "tvm_code")

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
