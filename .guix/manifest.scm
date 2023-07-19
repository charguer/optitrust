(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash" "coreutils" "gcc-toolchain"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
