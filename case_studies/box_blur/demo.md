open noisy.png
open out_OptiTrust.png

# Halide

(x then y)
reference runtime: 17.3911ms
reference fps: 57

(y then x)
reference runtime: 18.3192ms
reference fps: 54

# Unoptimized

OptiTrust runtime: 641.035ms
OptiTrust fps: 1

# Loop reorder

OptiTrust runtime: 101.051ms
OptiTrust fps: 9

`./view_result.sh /home/thomas/verified_transfo/src/case_studies/box_blur box_blur 20 view_diff recompile_optitrust_no`

# Loop fusion

OptiTrust runtime: 93.3751ms
OptiTrust fps: 10

# Loop tile + simd

OptiTrust runtime: 79.8796ms
OptiTrust fps: 12

# Loop tile + threads

OptiTrust runtime: 24.2745ms
OptiTrust fps: 41

(no vec)
OptiTrust runtime: 25.984ms
OptiTrust fps: 38