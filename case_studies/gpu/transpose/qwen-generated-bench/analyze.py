#!/usr/bin/env python3
"""Post-process transpose_bench CSVs into relative-performance statistics,
figures, and a similar-vs-different verdict.

Reads:  samples.csv, correctness.csv, meta.csv   (in the results dir)
Writes: summary.csv, speedup.csv, verdict.txt,
        speedup.{pdf,png}, bandwidth.{pdf,png},
        focused_speedup.{pdf,png}, focused_runtime.{pdf,png},
        focused_bandwidth.{pdf,png}

Method: per-pair median of base_time/other_time, with a bootstrap CI
(distribution-free, no normality assumption). Verdict is SIMILAR if the
CI includes 1.0, DIFFERENT otherwise. Geometric-mean speedup across all
shapes uses a shape-level bootstrap.

All-kernels figures use median lines / CI errorbars on a categorical
shape axis. The focused figures compare one kernel (focus) against the
baseline: speedup as a CI errorbar, runtime and bandwidth as grouped
box plots. Each is a separate file.

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


def gbps_arr(g, H, W, impl):
    return np.array([v[1] for v in g.get((H, W, impl), {}).values()])


def paired_ratios(g, H, W, base, other):
    """Per (draw, sample) ratio base_us / other_us.
    >1 means other is faster; <1 means base is faster."""
    a = g.get((H, W, base), {})
    b = g.get((H, W, other), {})
    common = sorted(set(a) & set(b))
    if not common:
        return np.array([])
    return np.array([a[ds][0] / b[ds][0] for ds in common])


def bootstrap_ci(x, stat=np.median, n_boot=2000, ci=0.95, seed=0):
    """Bootstrap CI on stat(x). Distribution-free (no normality assumption)."""
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
    """Geometric mean of per-shape median speedups, with a shape-bootstrap CI.
    Distribution-free: resamples shapes, recomputes geomean each time."""
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


def shape_label(H, W):
    """Compact label for a transpose shape."""
    if H == W:
        return f"{H}\u00b2"          # e.g. "1024²"
    return f"{H}x{W}"               # e.g. "256x2048"


def set_shape_ticks(ax, shapes, pos):
    """Set categorical x-ticks at each shape's integer position."""
    ax.set_xticks(sorted(pos.values()))
    ax.set_xticklabels([shape_label(H, W) for (H, W) in shapes],
                       rotation=45, ha="right", fontsize=8)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default="results")
    ap.add_argument("--ci", type=float, default=0.95,
                    help="confidence level for bootstrap CI (default 0.95)")
    ap.add_argument("--focus", default="OptiTrust-noconflicts",
                    help="kernel to highlight in the focused figures "
                         "(default: OptiTrust-noconflicts); pass 'none' to skip")
    args = ap.parse_args()

    g = load_samples(os.path.join(args.dir, "samples.csv"))
    meta = {}
    mpath = os.path.join(args.dir, "meta.csv")
    if os.path.exists(mpath):
        with open(mpath) as f:
            for row in csv.reader(f):
                if len(row) == 2 and row[0] != "key":
                    meta[row[0]] = row[1]

    shapes = sorted({(H, W) for (H, W, _) in g})
    impls = sorted({i for (_, _, i) in g})
    base = BASELINE if BASELINE in impls else impls[0]

    # Categorical x-position per shape (avoids H*W collisions, e.g. 256x2048 = 2048x256).
    pos = {s: i + 1 for i, s in enumerate(shapes)}

    cpath = os.path.join(args.dir, "correctness.csv")
    all_ok = True
    if os.path.exists(cpath):
        with open(cpath) as f:
            for row in csv.DictReader(f):
                all_ok = all_ok and row["ok"] == "1"

    # ---- summary.csv ----
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
                b = gbps_arr(g, H, W, impl)
                q = np.percentile(t, [5, 25, 50, 75, 95])
                w.writerow([H, W, H * W, impl, len(t), t.min(), q[0], q[1],
                            q[2], q[3], q[4], t.max(), t.std(ddof=1),
                            float(np.median(b))])

    # ---- speedup.csv ----
    speedup_rows = []
    for (H, W) in shapes:
        for impl in impls:
            if impl == base:
                continue
            r = paired_ratios(g, H, W, base, impl)
            if len(r) == 0:
                continue
            est, lo, hi = bootstrap_ci(r, np.median, ci=args.ci)
            verdict = "SIMILAR" if (lo <= 1.0 <= hi) else "DIFFERENT"
            speedup_rows.append(dict(
                H=H, W=W, n=H * W, baseline=base, impl=impl,
                med_us_base=float(np.median(times(g, H, W, base))),
                med_us_impl=float(np.median(times(g, H, W, impl))),
                med_gbps_impl=float(np.median(gbps_arr(g, H, W, impl))),
                med_speedup=est, ci_lo=lo, ci_hi=hi,
                verdict=verdict))

    with open(os.path.join(args.dir, "speedup.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["H", "W", "n", "baseline", "impl", "med_us_baseline",
                    "med_us_impl", "med_gbps_impl", "med_speedup",
                    "speedup_ci_lo", "speedup_ci_hi", "verdict"])
        for r in speedup_rows:
            w.writerow([r["H"], r["W"], r["n"], r["baseline"], r["impl"],
                        r["med_us_base"], r["med_us_impl"], r["med_gbps_impl"],
                        r["med_speedup"], r["ci_lo"], r["ci_hi"],
                        r["verdict"]])

    # ---- verdict.txt ----
    if len(impls) >= 2:
        geom = {}
        for impl in impls:
            if impl == base:
                continue
            geom[impl] = geomean_speedup(g, shapes, base, impl, ci=args.ci)

        with open(os.path.join(args.dir, "verdict.txt"), "w") as f:
            f.write(f"Device: {meta.get('device', '?')} "
                    f"({meta.get('compute_capability', '?')})\n")
            f.write(f"Baseline: {base}\n")
            f.write(f"All correctness checks passed: {all_ok}\n")
            f.write(f"Criterion: bootstrap CI ({100*args.ci:.0f}%) on "
                    f"paired median ratio; SIMILAR iff CI includes 1.0\n\n")
            for impl in impls:
                if impl == base:
                    continue
                gm, glo, ghi = geom[impl]
                verdict = "SIMILAR" if (glo <= 1.0 <= ghi) else "DIFFERENT"
                f.write(f"Geomean speedup '{impl}' vs '{base}': "
                        f"{gm:.3f} ({100*args.ci:.0f}% CI {glo:.3f}..{ghi:.3f}) "
                        f"-> {verdict}\n")
            f.write("\n")
            hdr = (f"{'shape':>12} {'base_us':>10} {'impl_us':>10} "
                   f"{'speedup':>8} {'CI':>16}  {'verdict':>9}")
            f.write(hdr + "\n")
            f.write("-" * len(hdr) + "\n")
            for r in speedup_rows:
                label = shape_label(r["H"], r["W"])
                f.write(f"{label:>12} {r['med_us_base']:>10.2f} "
                        f"{r['med_us_impl']:>10.2f} {r['med_speedup']:>8.3f} "
                        f"({r['ci_lo']:.3f},{r['ci_hi']:.3f})"
                        f"  {r['verdict']:>9}\n")

        print(f"Baseline: {base}   all_ok: {all_ok}")
        for impl in impls:
            if impl == base:
                continue
            gm, glo, ghi = geom[impl]
            verdict = "SIMILAR" if (glo <= 1.0 <= ghi) else "DIFFERENT"
            print(f"  geomean speedup {impl} vs {base}: "
                  f"{gm:.3f} ({glo:.3f}..{ghi:.3f}) -> {verdict}")

    # ---- figures ----
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        import matplotlib.lines as mlines

        # (A) speedup vs shape (paired median ratio + bootstrap CI, errorbar)
        fig, ax = plt.subplots(figsize=(9, 5))
        for impl in impls:
            if impl == base:
                continue
            xs, ys, los, his = [], [], [], []
            for r in speedup_rows:
                if r["impl"] != impl:
                    continue
                xs.append(pos[(r["H"], r["W"])])
                ys.append(r["med_speedup"])
                los.append(r["med_speedup"] - r["ci_lo"])
                his.append(r["ci_hi"] - r["med_speedup"])
            if xs:
                ax.errorbar(xs, ys, yerr=[los, his], fmt="o-",
                           capsize=3, label=impl)
        ax.axhline(1.0, color="grey", ls="--", lw=1, label="no difference")
        set_shape_ticks(ax, shapes, pos)
        ax.set_xlabel("shape (H×W)")
        ax.set_ylabel(f"speedup vs {base}")
        ax.set_title("Transpose: median speedup per shape (paired, bootstrap CI)")
        ax.legend(fontsize=8)
        fig.tight_layout()
        for ext in ("pdf", "png"):
            fig.savefig(os.path.join(args.dir, f"speedup.{ext}"), dpi=150)
        plt.close(fig)

        # (B) bandwidth vs shape (median lines)
        fig, ax = plt.subplots(figsize=(9, 5))
        for impl in impls:
            xs, ys = [], []
            for (H, W) in shapes:
                b = gbps_arr(g, H, W, impl)
                if len(b):
                    xs.append(pos[(H, W)])
                    ys.append(float(np.median(b)))
            if xs:
                ax.plot(xs, ys, marker="o", label=impl)
        set_shape_ticks(ax, shapes, pos)
        ax.set_xlabel("shape (H×W)")
        ax.set_ylabel("effective bandwidth (GB/s)")
        ax.set_title("Transpose bandwidth per shape (median)")
        ax.grid(alpha=0.3, axis="y")
        ax.legend(fontsize=8)
        fig.tight_layout()
        for ext in ("pdf", "png"):
            fig.savefig(os.path.join(args.dir, f"bandwidth.{ext}"), dpi=150)
        plt.close(fig)

        # (C) Focused figures: one kernel vs baseline, three SEPARATE plots
        focus = args.focus
        if focus in impls:
            n = len(shapes)
            fp = [i - 0.18 for i in range(1, n + 1)]
            bp = [i + 0.18 for i in range(1, n + 1)]
            legend = [
                mlines.Line2D([], [], marker="s", color="none",
                             markerfacecolor="steelblue", label=focus),
                mlines.Line2D([], [], marker="s", color="none",
                             markerfacecolor="lightgrey", label=base),
            ]

            def shape_xticks(ax):
                ax.set_xticks(range(1, n + 1))
                ax.set_xticklabels([shape_label(H, W) for (H, W) in shapes],
                                   rotation=45, ha="right", fontsize=8)

            # --- C1: focused speedup (CI errorbar) ---
            fig, ax = plt.subplots(figsize=(9, 5))
            xs, ys, los, his = [], [], [], []
            for r in speedup_rows:
                if r["impl"] != focus:
                    continue
                xs.append(pos[(r["H"], r["W"])])
                ys.append(r["med_speedup"])
                los.append(r["med_speedup"] - r["ci_lo"])
                his.append(r["ci_hi"] - r["med_speedup"])
            if xs:
                ax.errorbar(xs, ys, yerr=[los, his], fmt="o-", capsize=3,
                           color="steelblue", label=focus)
            ax.axhline(1.0, color="grey", ls="--", lw=1, label="no difference")
            shape_xticks(ax)
            ax.set_xlabel("shape (H×W)")
            ax.set_ylabel(f"speedup vs {base}")
            ax.set_title(f"{focus} vs {base}: median speedup (bootstrap CI)")
            ax.legend(loc="upper right", fontsize=9)
            fig.tight_layout()
            for ext in ("pdf", "png"):
                fig.savefig(os.path.join(args.dir, f"focused_speedup.{ext}"),
                            dpi=150)
            plt.close(fig)

            # --- C2: focused runtime (grouped box plots) ---
            fig, ax = plt.subplots(figsize=(9, 5))
            fdata = [times(g, H, W, focus) for (H, W) in shapes
                     if len(times(g, H, W, focus))]
            bdata = [times(g, H, W, base) for (H, W) in shapes
                     if len(times(g, H, W, base))]
            if fdata:
                ax.boxplot(fdata, positions=fp, widths=0.34,
                          patch_artist=True, showfliers=True,
                          flierprops=dict(marker=".", ms=2, alpha=0.4))
                ax.boxplot(bdata, positions=bp, widths=0.34,
                          patch_artist=True, showfliers=True,
                          flierprops=dict(marker=".", ms=2, alpha=0.4),
                          medianprops=dict(color="black"))
                shape_xticks(ax)
                ax.legend(handles=legend, loc="upper right")
            ax.set_xlabel("shape (H×W)")
            ax.set_ylabel("runtime (µs)")
            ax.set_title(f"{focus} vs {base}: runtime (box plot)")
            fig.tight_layout()
            for ext in ("pdf", "png"):
                fig.savefig(os.path.join(args.dir, f"focused_runtime.{ext}"),
                            dpi=150)
            plt.close(fig)

            # --- C3: focused bandwidth (grouped box plots) ---
            fig, ax = plt.subplots(figsize=(9, 5))
            fdata = [gbps_arr(g, H, W, focus) for (H, W) in shapes
                     if len(gbps_arr(g, H, W, focus))]
            bdata = [gbps_arr(g, H, W, base) for (H, W) in shapes
                     if len(gbps_arr(g, H, W, base))]
            if fdata:
                ax.boxplot(fdata, positions=fp, widths=0.34,
                          patch_artist=True, showfliers=True,
                          flierprops=dict(marker=".", ms=2, alpha=0.4))
                ax.boxplot(bdata, positions=bp, widths=0.34,
                          patch_artist=True, showfliers=True,
                          flierprops=dict(marker=".", ms=2, alpha=0.4),
                          medianprops=dict(color="black"))
                shape_xticks(ax)
                ax.legend(handles=legend, loc="upper right")
            ax.set_xlabel("shape (H×W)")
            ax.set_ylabel("bandwidth (GB/s)")
            ax.set_title(f"{focus} vs {base}: bandwidth (box plot)")
            fig.tight_layout()
            for ext in ("pdf", "png"):
                fig.savefig(os.path.join(args.dir, f"focused_bandwidth.{ext}"),
                            dpi=150)
            plt.close(fig)

            print("wrote focused_speedup/runtime/bandwidth.{pdf,png}")

        print("wrote speedup.{pdf,png}, bandwidth.{pdf,png}")
    except Exception as e:
        print(f"(skipped figures: {e})")

    print(f"wrote summary.csv, speedup.csv, verdict.txt; impls={impls}")


if __name__ == "__main__":
    sys.exit(main())
