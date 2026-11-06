#!/usr/bin/env python3
"""Post-process transpose_bench CSVs into relative-performance statistics,
box/speedup/bandwidth figures, and a similar-vs-different verdict.

Reads:  samples.csv, correctness.csv, meta.csv   (in the results dir)
Writes: summary.csv, speedup.csv, verdict.txt,
        boxplot.{pdf,png}, speedup.{pdf,png}, bandwidth.{pdf,png}

Dependency: numpy required; scipy and matplotlib optional.
"""
import argparse
import csv
import math
import os
import sys
from collections import defaultdict

import numpy as np

BASELINE = "ref"  # kernel that speedups are measured against


def load_samples(path):
    """Group samples by (H, W, impl) -> {(draw, sample): (time_us, gbps)}."""
    g = defaultdict(dict)
    with open(path) as f:
        for row in csv.DictReader(f):
            key = (int(row["H"]), int(row["W"]), row["impl"])
            ds = (int(row["draw"]), int(row["sample"]))
            g[key][ds] = (float(row["time_us"]), float(row["gbps"]))
    return g


def times(g, H, W, impl):
    return np.array([v[0] for v in g.get((H, W, impl), {}).values()])


def gbps(g, H, W, impl):
    return np.array([v[1] for v in g.get((H, W, impl), {}).values()])


def paired_ratios(g, H, W, base, other):
    """Per (draw,sample) ratio base_us/other_us on the intersection of trials.
    Values > 1 mean `other` is faster than `base`."""
    a = g.get((H, W, base), {})
    b = g.get((H, W, other), {})
    common = sorted(set(a) & set(b))
    if not common:
        return np.array([])
    return np.array([a[ds][0] / b[ds][0] for ds in common])


def bootstrap_ci(x, stat=np.median, n_boot=2000, ci=0.95, seed=0):
    """Bootstrap CI on `stat(x)` plus the point estimate."""
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


def geomean_speedup_across_shapes(g, shapes, base, other, ci=0.95, seed=0):
    """Geometric mean of per-shape median speedups, with a shape-bootstrap CI."""
    per_shape = []
    for (H, W) in shapes:
        r = paired_ratios(g, H, W, base, other)
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


def paired_pvalue(g, H, W, base, other):
    """Wilcoxon signed-rank on the paired trials (needs scipy); else nan."""
    a = g.get((H, W, base), {})
    b = g.get((H, W, other), {})
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

    shapes = sorted({(h, w) for (h, w, _) in g})
    impls = sorted({i for (_, _, i) in g})
    base = BASELINE if BASELINE in impls else impls[0]

    cpath = os.path.join(args.dir, "correctness.csv")
    all_ok = True
    if os.path.exists(cpath):
        with open(cpath) as f:
            for row in csv.DictReader(f):
                all_ok = all_ok and row["ok"] == "1"

    # ---- summary.csv: per (shape, impl) absolute stats + median bandwidth ---
    with open(os.path.join(args.dir, "summary.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["H", "W", "n", "impl", "n_samples", "min_us", "q05_us",
                    "q25_us", "med_us", "q75_us", "q95_us", "max_us",
                    "std_us", "med_gbps"])
        for (H, W) in shapes:
            for impl in impls:
                t = times(g, H, W, impl)
                if len(t) == 0:
                    continue
                b = gbps(g, H, W, impl)
                q = np.percentile(t, [5, 25, 50, 75, 95])
                w.writerow([H, W, H * W, impl, len(t), t.min(), q[0], q[1],
                            q[2], q[3], q[4], t.max(), t.std(ddof=1),
                            float(np.median(b))])

    # ---- speedup.csv: per shape, per non-baseline kernel vs baseline -------
    speedup_rows = []
    for (H, W) in shapes:
        for impl in impls:
            if impl == base:
                continue
            r = paired_ratios(g, H, W, base, impl)
            if len(r) == 0:
                continue
            est, lo, hi = bootstrap_ci(r, np.median, ci=args.ci)
            p = paired_pvalue(g, H, W, base, impl)
            verdict = "SIMILAR"
            if (lo > 1.0 or hi < 1.0) or (p != float("nan") and p < (1 - args.ci)):
                verdict = "DIFFERENT"
            speedup_rows.append(dict(
                H=H, W=W, n=H * W, baseline=base, impl=impl,
                med_us_base=float(np.median(times(g, H, W, base))),
                med_us_impl=float(np.median(times(g, H, W, impl))),
                med_gbps_impl=float(np.median(gbps(g, H, W, impl))),
                med_speedup=est, ci_lo=lo, ci_hi=hi,
                p=p, verdict=verdict))

    with open(os.path.join(args.dir, "speedup.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["H", "W", "n", "baseline", "impl", "med_us_baseline",
                    "med_us_impl", "med_gbps_impl", "med_speedup",
                    "speedup_ci_lo", "speedup_ci_hi", "p", "verdict"])
        for r in speedup_rows:
            w.writerow([r["H"], r["W"], r["n"], r["baseline"], r["impl"],
                        r["med_us_base"], r["med_us_impl"], r["med_gbps_impl"],
                        r["med_speedup"], r["ci_lo"], r["ci_hi"], r["p"],
                        r["verdict"]])

    # ---- verdict.txt + stdout ---------------------------------------------
    verdict_lines = []
    geom = {}
    for impl in impls:
        if impl == base:
            continue
        gm, glo, ghi = geomean_speedup_across_shapes(g, shapes, base, impl,
                                                     ci=args.ci)
        geom[impl] = (gm, glo, ghi)

    if len(impls) >= 2:
        with open(os.path.join(args.dir, "verdict.txt"), "w") as f:
            f.write(f"Device: {meta.get('device','?')} "
                    f"({meta.get('compute_capability','?')})\n"
                    f"Baseline: {base}\n"
                    f"All correctness checks passed: {all_ok}\n\n")
            for impl in impls:
                if impl == base:
                    continue
                gm, glo, ghi = geom[impl]
                f.write(f"Geometric-mean speedup of '{impl}' over '{base}' "
                        f"across all shapes: {gm:.3f} "
                        f"({100*args.ci:.0f}% CI {glo:.3f}..{ghi:.3f})\n\n")
            f.write(f"{'shape':>12} {'base_us':>10} {'impl_us':>10} "
                    f"{'speedup':>8} {'CI':>16} {'p':>7}  verdict\n")
            for r in speedup_rows:
                f.write(f"{str(r['H'])+'x'+str(r['W']):>12} "
                        f"{r['med_us_base']:>10.2f} {r['med_us_impl']:>10.2f} "
                        f"{r['med_speedup']:>8.3f} "
                        f"{str(f'({r['ci_lo']:.3f},{r['ci_hi']:.3f})'):>16} "
                        f"{r['p']:>7.4f}  {r['verdict']}\n")
        # mirror to stdout
        print(f"Baseline: {base}   all_ok: {all_ok}")
        for impl in impls:
            if impl == base:
                continue
            gm, glo, ghi = geom[impl]
            print(f"  geomean speedup {impl} vs {base}: "
                  f"{gm:.3f} ({glo:.3f}..{ghi:.3f})")

    # ---- figures -----------------------------------------------------------
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        # (A) speedup vs problem size (the "similar or not" figure)
        fig, ax = plt.subplots(figsize=(8, 5))
        for impl in impls:
            if impl == base:
                continue
            xs, ys, los, his = [], [], [], []
            for r in speedup_rows:
                if r["impl"] != impl:
                    continue
                xs.append(math.log10(r["n"]))
                ys.append(r["med_speedup"])
                los.append(r["med_speedup"] - r["ci_lo"])
                his.append(r["ci_hi"] - r["med_speedup"])
            ax.errorbar(xs, ys, yerr=[los, his], fmt="o-", capsize=4, label=impl)
        ax.axhline(1.0, color="grey", ls="--", lw=1, label="no difference")
        ax.set_yscale("log")
        ax.set_xlabel("log10(#elements)")
        ax.set_ylabel(f"speedup vs {base} (x, log)")
        ax.set_title("Median speedup per shape (paired, same input)")
        ax.legend()
        fig.tight_layout()
        for ext in ("pdf", "png"):
            fig.savefig(os.path.join(args.dir, f"speedup.{ext}"), dpi=150)
        plt.close(fig)

        # (B) effective bandwidth vs problem size (canonical memory figure)
        fig, ax = plt.subplots(figsize=(8, 5))
        for impl in impls:
            xs, ys = [], []
            for (H, W) in shapes:
                b = gbps(g, H, W, impl)
                if len(b):
                    xs.append(math.log10(H * W))
                    ys.append(float(np.median(b)))
            if xs:
                ax.plot(xs, ys, marker="o", label=impl)
        ax.set_xlabel("log10(#elements)")
        ax.set_ylabel("effective bandwidth (GB/s)")
        ax.set_title("Transpose bandwidth vs problem size")
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
