# Plan

1. Intro de l'example: flux vidéo bruité de 24 images par seconde, on souhaite réduire le bruit sur chaque image avec un flou.
2. Implémentation d'un flou naïf en C: le bruit est bien enlevé, mais programme trop lent pour le flux.
3. Objectif d'optimisation: dépasser les 24 images par secondes. A la main? (encore souvent fait comme ça)
  1. Permutation de boucles. (data locality)
  2. tuilage pour vectorization: introduire bug. (parallelism)
4. Difficulté optimisation manuelle (prends du temps, facile d'introduire des bugs). OptiTrust outil interactif pour la transformation de programmes. gagner du temps / plus productif, éviter les bugs.
  1. rattrapage Permutation pour localité des données
  2. tuilage + vectorization pour parallélisme
  3. fusion + tuilage + threads pour parallélisme
5. Conclusion & Questions
  - OptiTrust, outil interactif pour la transformation de programmes. gagner du temps / plus productif, éviter les bugs.
  - pas seulement des codes de traitement d'image
  - pas seulement pour optimiser la vitesse d'exécution (e.g. utilisation mémoire, énergie)

- 4. rewrite program by hand, see output and perf
  - change 'box_blur_manual' + run './test_handwritten'
- 5. rewrite program with tool, see output and perf
  - start 'optw'?
  - 'box_blur_after' changes on F6 / Alt+F6 + run './test_optitrust' ?
  - script doing all above?

# Useful

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

FIXME: `3.` vs `3.0f`

# Loop tile + simd

OptiTrust runtime: 79.8796ms
OptiTrust fps: 12

# Loop fusion + tile + threads

OptiTrust runtime: 22.6ms
OptiTrust fps: 44