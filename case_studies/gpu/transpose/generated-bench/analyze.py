#!/usr/bin/env python3
"""Post-process a transpose_bench output directory.

Reads samples.csv / correctness.csv, prints per-size summary statistics
(median, quartiles, IQR) for each kernel, runs pairwise two-sided
Mann-Whitney U tests between kernels per size (to decide whether two
implementations have similar performance), writes stats.txt, and draws
box plots (boxplot_HxW.png) if matplotlib is available.

Usage: python3 analyze.py [results_dir]
"""

import csv
import math
import os
import sys
from collections import defaultdict


def quantile(sorted_vals, q):
    if not sorted_vals:
        return float("nan")
    if len(sorted_vals) == 1:
        return sorted_vals[0]
    pos = q * (len(sorted_vals) - 1)
    lo = int(pos)
    hi = min(lo + 1, len(sorted_vals) - 1)
    frac = pos - lo
    return sorted_vals[lo] * (1.0 - frac) + sorted_vals[hi] * frac


def summarize(vals):
    if not vals:
        return None
    s = sorted(vals)
    n = len(s)
    mean = sum(s) / n
    var = sum((x - mean) ** 2 for x in s) / (n - 1) if n > 1 else 0.0
    return {
        "n": n,
        "mean": mean,
        "std": math.sqrt(var),
        "min": s[0],
        "q1": quantile(s, 0.25),
        "median": quantile(s, 0.50),
        "q3": quantile(s, 0.75),
        "max": s[-1],
    }


def norm_cdf(z):
    return 0.5 * (1.0 + math.erf(z / math.sqrt(2.0)))


def mann_whitney_u(x, y):
    """Two-sided Mann-Whitney U test with tie correction and continuity correction."""
    n1, n2 = len(x), len(y)
    if n1 == 0 or n2 == 0:
        return float("nan"), float("nan")
    combined = sorted([(v, 0) for v in x] + [(v, 1) for v in y])
    ranks = [0.0] * len(combined)
    ties = []
    i = 0
    while i < len(combined):
        j = i
        while j + 1 < len(combined) and combined[j + 1][0] == combined[j][0]:
            j += 1
        avg_rank = (i + j) / 2.0 + 1.0
        for k in range(i, j + 1):
            ranks[k] = avg_rank
        if j > i:
            ties.append(j - i + 1)
        i = j + 1
    r1 = sum(r for r, (_, g) in zip(ranks, combined) if g == 0)
    u1 = r1 - n1 * (n1 + 1) / 2.0
    n = n1 + n2
    tie_term = sum(t ** 3 - t for t in ties)
    var = (n1 * n2 / 12.0) * ((n + 1) - tie_term / (n * (n - 1)))
    mean = n1 * n2 / 2.0
    if var <= 0:
        return u1, 1.0
    z = (u1 - mean - (0.5 if u1 > mean else -0.5)) / math.sqrt(var)
    p = 2.0 * (1.0 - norm_cdf(abs(z)))
    return u1, p


def main():
    outdir = sys.argv[1] if len(sys.argv) > 1 else "results"
    samples_path = os.path.join(outdir, "samples.csv")
    if not os.path.exists(samples_path):
        print(f"error: {samples_path} not found; run the benchmark first", file=sys.stderr)
        return 1

    gbs = defaultdict(list)
    ms = defaultdict(list)
    with open(samples_path, newline="") as f:
        for row in csv.DictReader(f):
            key = (row["kernel"], int(row["H"]), int(row["W"]))
            ms[key].append(float(row["time_ms"]))
            gbs[key].append(float(row["bandwidth_GBs"]))

    failures = []
    corr_path = os.path.join(outdir, "correctness.csv")
    if os.path.exists(corr_path):
        with open(corr_path, newline="") as f:
            for row in csv.DictReader(f):
                if row["status"] != "OK":
                    failures.append(row)
    if failures:
        print("CORRECTNESS FAILURES (kernel,H,W,input,mismatches):")
        for r in failures:
            print("  ", r["kernel"], r["H"], r["W"], r["input_id"], r["mismatches"])

    sizes = sorted({(h, w) for (_, h, w) in gbs})
    kernels = sorted({k for (k, _, _) in gbs})

    lines = []
    for (h, w) in sizes:
        lines.append(f"=== H={h} W={w} ===")
        for k in kernels:
            sg = summarize(gbs[(k, h, w)])
            sm = summarize(ms[(k, h, w)])
            if sg is None:
                continue
            lines.append(
                f"  {k:<12} n={sg['n']:<6} median={sg['median']:9.2f} GB/s  "
                f"IQR=[{sg['q1']:8.2f}, {sg['q3']:8.2f}]  min={sg['min']:8.2f}  "
                f"max={sg['max']:8.2f}  median_ms={sm['median']:.6f}"
            )
        for i in range(len(kernels)):
            for j in range(i + 1, len(kernels)):
                u, p = mann_whitney_u(gbs[(kernels[i], h, w)], gbs[(kernels[j], h, w)])
                verdict = (
                    "similar (not significantly different, p>=0.05)"
                    if p >= 0.05
                    else "significantly different (p<0.05)"
                )
                lines.append(
                    f"  MWU {kernels[i]} vs {kernels[j]}: U={u:.1f} p={p:.4g} -> {verdict}"
                )

    text = "\n".join(lines)
    print(text)
    with open(os.path.join(outdir, "stats.txt"), "w") as f:
        f.write(text + "\n")

    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        print("matplotlib not available; wrote stats.txt only (no box plots)")
        return 0

    for (h, w) in sizes:
        fig, ax = plt.subplots(figsize=(max(4.0, 1.4 * len(kernels)), 4.0))
        ax.boxplot([gbs[(k, h, w)] for k in kernels], showfliers=True)
        ax.set_xticklabels(kernels)
        ax.set_ylabel("bandwidth (GB/s)")
        ax.set_title(f"matrix transpose H={h} W={w}")
        fig.tight_layout()
        path = os.path.join(outdir, f"boxplot_{h}x{w}.png")
        fig.savefig(path, dpi=150)
        plt.close(fig)
        print(f"wrote {path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
