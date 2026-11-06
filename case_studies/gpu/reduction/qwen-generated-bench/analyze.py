#!/usr/bin/env python3
"""Post-process reduction_bench CSVs into relative-performance statistics,
figures, and a similar-vs-different verdict.

Reads:  samples.csv, correctness.csv, meta.csv   (in the results dir)
Writes: summary.csv, speedup.csv, verdict.txt,
        speedup.{pdf,png}, bandwidth.{pdf,png}

Dependency: numpy required; scipy and matplotlib optional.
"""
import argparse
import csv
import math
import os
import sys
from collections import defaultdict

import numpy as np

BASELINE = "reduce3"  # kernel that speedups are measured against


def load_samples(path):
    """Group by (N, impl) -> {(draw, sample): (time_us, gbps)}."""
    g = defaultdict(dict)
    with open(path) as f:
        for row in csv.DictReader(f):
            key = (int(row["N"]), row["impl"])
            ds = (int(row["draw"]), int(row["sample"]))
            g[key][ds] = (float(row["time_us"]), float(row["gbps"]))
    return g


def times(g, N, impl):
    return np.array([v[0] for v in g.get((N, impl), {}).values()])


def gbps_arr(g, N, impl):
    return np.array([v[1] for v in g.get((N, impl), {}).values()])


def paired_ratios(g, N, base, other):
    a = g.get((N, base), {})
    b = g.get((N, other), {})
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


def geomean_speedup(g, sizes, base, other, ci=0.95, seed=0):
    per_size = []
    for N in sizes:
        r = paired_ratios(g, N, base, other)
        if len(r):
            per_size.append(r)
    if not per_size:
        return (np.nan, np.nan, np.nan)
    point = math.exp(np.mean([np.log(np.median(r)) for r in per_size]))
    rng = np.random.default_rng(seed)
    boots = []
    for _ in range(2000):
        chosen = [rng.choice(per_size) for _ in range(len(per_size))]
        boots.append(math.exp(np.mean([np.log(np.median(r)) for r in chosen])))
    lo, hi = (1 - ci) / 2, 1 - (1 - ci) / 2
    return (point, np.percentile(boots, 100 * lo), np.percentile(boots, 100 * hi))


def paired_pvalue(g, N, base, other):
    a = g.get((N, base), {})
    b = g.get((N, other), {})
    common = sorted(set(a) & set(b))
    if len(common) < 8:
        return float("nan")
    try:
        from scipy.stats import wilcoxon
        return wilcoxon([a[ds][0] for ds in common],
                        [b[ds][0] for ds in common]).pvalue
    except Exception:
        return float("nan")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default="results")
    ap.add_argument("--ci", type=float, default=0.95)
    args = ap.parse_args()

    g = load_samples(os.path.join(args.dir, "samples.csv"))
    meta = {}
    mpath = os.path.join(args.dir, "meta.csv")
    if os.path.exists(mpath):
        with open(mpath) as f:
            for row in csv.reader(f):
                if len(row) == 2 and row[0] != "key":
                    meta[row[0]] = row[1]

    sizes = sorted({n for (n, _) in g})
    impls = sorted({i for (_, i) in g})
    base = BASELINE if BASELINE in impls else impls[0]

    cpath = os.path.join(args.dir, "correctness.csv")
    all_ok = True
    if os.path.exists(cpath):
        with open(cpath) as f:
            for row in csv.DictReader(f):
                all_ok = all_ok and row["ok"] == "1"

    # ---- summary.csv ----
    with open(os.path.join(args.dir, "summary.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["N", "impl", "n_samples", "min_us", "q05_us",
                    "q25_us", "med_us", "q75_us", "q95_us", "max_us",
                    "std_us", "med_gbps"])
        for N in sizes:
            for impl in impls:
                t = times(g, N, impl)
                if len(t) == 0:
                    continue
                b = gbps_arr(g, N, impl)
                q = np.percentile(t, [5, 25, 50, 75, 95])
                w.writerow([N, impl, len(t), t.min(), q[0], q[1],
                            q[2], q[3], q[4], t.max(), t.std(ddof=1),
                            float(np.median(b))])

    # ---- speedup.csv ----
    speedup_rows = []
    for N in sizes:
        for impl in impls:
            if impl == base:
                continue
            r = paired_ratios(g, N, base, impl)
            if len(r) == 0:
                continue
            est, lo, hi = bootstrap_ci(r, np.median, ci=args.ci)
            p = paired_pvalue(g, N, base, impl)
            verdict = "SIMILAR"
            if (lo > 1.0 or hi < 1.0) or (p != float("nan") and p < (1 - args.ci)):
                verdict = "DIFFERENT"
            speedup_rows.append(dict(
                N=N, baseline=base, impl=impl,
                med_us_base=float(np.median(times(g, N, base))),
                med_us_impl=float(np.median(times(g, N, impl))),
                med_gbps_impl=float(np.median(gbps_arr(g, N, impl))),
                med_speedup=est, ci_lo=lo, ci_hi=hi,
                p=p, verdict=verdict))

    with open(os.path.join(args.dir, "speedup.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["N", "baseline", "impl", "med_us_baseline",
                    "med_us_impl", "med_gbps_impl", "med_speedup",
                    "speedup_ci_lo", "speedup_ci_hi", "p", "verdict"])
        for r in speedup_rows:
            w.writerow([r["N"], r["baseline"], r["impl"],
                        r["med_us_base"], r["med_us_impl"], r["med_gbps_impl"],
                        r["med_speedup"], r["ci_lo"], r["ci_hi"], r["p"],
                        r["verdict"]])

    # ---- verdict.txt ----
    if len(impls) >= 2:
        geom = {}
        for impl in impls:
            if impl == base:
                continue
            geom[impl] = geomean_speedup(g, sizes, base, impl, ci=args.ci)

        with open(os.path.join(args.dir, "verdict.txt"), "w") as f:
            f.write(f"Device: {meta.get('device','?')} "
                    f"({meta.get('compute_capability','?')})\n"
                    f"Baseline: {base}\n"
                    f"All correctness checks passed: {all_ok}\n\n")
            for impl in impls:
                if impl == base:
                    continue
                gm, glo, ghi = geom[impl]
                f.write(f"Geometric-mean speedup of '{impl}' over '{base}': "
                        f"{gm:.3f} ({100*args.ci:.0f}% CI {glo:.3f}..{ghi:.3f})\n")
            f.write("\n")
            f.write(f"{'N':>12} {'base_us':>10} {'impl_us':>10} "
                    f"{'speedup':>8} {'CI':>16} {'p':>7}  verdict\n")
            for r in speedup_rows:
                f.write(f"{r['N']:>12} {r['med_us_base']:>10.2f} "
                        f"{r['med_us_impl']:>10.2f} {r['med_speedup']:>8.3f} "
                        f"{f'({r['ci_lo']:.3f},{r['ci_hi']:.3f})':>16} "
                        f"{r['p']:>7.4f}  {r['verdict']}\n")

        print(f"Baseline: {base}   all_ok: {all_ok}")
        for impl in impls:
            if impl == base:
                continue
            gm, glo, ghi = geom[impl]
            print(f"  geomean speedup {impl} vs {base}: "
                  f"{gm:.3f} ({glo:.3f}..{ghi:.3f})")

    # ---- figures ----
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        # (A) speedup vs N
        fig, ax = plt.subplots(figsize=(8, 5))
        for impl in impls:
            if impl == base:
                continue
            xs, ys, los, his = [], [], [], []
            for r in speedup_rows:
                if r["impl"] != impl:
                    continue
                xs.append(math.log10(r["N"]))
                ys.append(r["med_speedup"])
                los.append(r["med_speedup"] - r["ci_lo"])
                his.append(r["ci_hi"] - r["med_speedup"])
            if xs:
                ax.errorbar(xs, ys, yerr=[los, his], fmt="o-",
                           capsize=4, label=impl)
        ax.axhline(1.0, color="grey", ls="--", lw=1, label="no difference")
        ax.set_yscale("log")
        ax.set_xlabel("log10(N)")
        ax.set_ylabel(f"speedup vs {base} (×, log)")
        ax.set_title("Reduction: median speedup vs problem size")
        ax.legend()
        fig.tight_layout()
        for ext in ("pdf", "png"):
            fig.savefig(os.path.join(args.dir, f"speedup.{ext}"), dpi=150)
        plt.close(fig)

        # (B) bandwidth vs N
        fig, ax = plt.subplots(figsize=(8, 5))
        for impl in impls:
            xs, ys = [], []
            for N in sizes:
                b = gbps_arr(g, N, impl)
                if len(b):
                    xs.append(math.log10(N))
                    ys.append(float(np.median(b)))
            if xs:
                ax.plot(xs, ys, marker="o", label=impl)
        ax.set_xlabel("log10(N)")
        ax.set_ylabel("effective bandwidth (GB/s)")
        ax.set_title("Reduction bandwidth vs problem size")
        ax.grid(alpha=0.3)
        ax.legend()
        fig.tight_layout()
        for ext in ("pdf", "png"):
            fig.savefig(os.path.join(args.dir, f"bandwidth.{ext}"), dpi=150)
        plt.close(fig)
        print("wrote speedup.{pdf,png}, bandwidth.{pdf,png}")
    except Exception as e:
        print(f"(skipped figures: {e})")

    print(f"wrote summary.csv, speedup.csv, verdict.txt; impls={impls}")


if __name__ == "__main__":
    sys.exit(main())
