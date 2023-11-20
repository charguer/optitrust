(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash" "coreutils" "gcc-toolchain" "ocaml-merlin" "emacs"
         "emacs-tuareg" "emacs-projectile" "emacs-spacemacs-theme" "git"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
