#!/usr/bin/env python3
"""Post-process mm_bench CSVs into relative-performance statistics,
figures, and a similar-vs-different verdict.

Reads:  samples.csv, correctness.csv, meta.csv   (in the results dir)
Writes: summary.csv, speedup.csv, verdict.txt,
        speedup.{pdf,png}, tflops.{pdf,png}

Dependency: numpy required; scipy and matplotlib optional.
"""
import argparse
import csv
import math
import os
import sys
from collections import defaultdict

import numpy as np

BASELINE = "tiled32"  # pick your reference; change to "optitrust" etc.


def load_samples(path):
    """Group by (m, n, p, impl) -> {(draw, sample): (time_us, tflops)}."""
    g = defaultdict(dict)
    with open(path) as f:
        for row in csv.DictReader(f):
            key = (int(row["m"]), int(row["n"]), int(row["p"]), row["impl"])
            ds = (int(row["draw"]), int(row["sample"]))
            g[key][ds] = (float(row["time_us"]), float(row["tflops"]))
    return g


def times(g, m, n, p, impl):
    return np.array([v[0] for v in g.get((m, n, p, impl), {}).values()])


def tflops_arr(g, m, n, p, impl):
    return np.array([v[1] for v in g.get((m, n, p, impl), {}).values()])


def paired_ratios(g, m, n, p, base, other):
    a = g.get((m, n, p, base), {})
    b = g.get((m, n, p, other), {})
    common = sorted(set(a) & set(b))
    if not common:
        return np.array([])
    return np.array([a[ds][0] / b[ds][0] for ds in common])


def bootstrap_ci(x, stat=np.median, n_boot=2000, ci=0.95, seed=0):
    x = np.asarray(x, dtype=float)
    if len(x) == 0:
        return (np.nan, np.nan, np.nan)
    rng = np.random.default_rng(seed)
    est = stat(x)
    boots = np.empty(n_boot)
    for i in range(n_boot):
        idx = rng.integers(0, len(x), len(x))
        boots[i] = stat(x[idx])
    lo, hi = (1 - ci) / 2, 1 - (1 - ci) / 2
    return (est, np.percentile(boots, 100 * lo), np.percentile(boots, 100 * hi))


def geomean_speedup(g, shapes, base, other, ci=0.95, seed=0):
    per_shape = []
    for (m, n, p) in shapes:
        r = paired_ratios(g, m, n, p, base, other)
        if len(r):
            per_shape.append(r)
    if not per_shape:
        return (np.nan, np.nan, np.nan)
    point = math.exp(np.mean([np.log(np.median(r)) for r in per_shape]))
    rng = np.random.default_rng(seed)
    boots = []
    for _ in range(2000):
        chosen = [rng.choice(per_shape) for _ in range(len(per_shape))]
        boots.append(math.exp(np.mean([np.log(np.median(r)) for r in chosen])))
    lo, hi = (1 - ci) / 2, 1 - (1 - ci) / 2
    return (point, np.percentile(boots, 100 * lo), np.percentile(boots, 100 * hi))
