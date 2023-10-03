(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash" "coreutils" "gcc-toolchain" "ocaml-graph"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
