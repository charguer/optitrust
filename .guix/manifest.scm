(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash"
         "coreutils"
         "gcc-toolchain"
         "nano"
	 "graphviz"
         "ocaml-merlin"
         "ocaml-graph"
         "emacs"
         "emacs-org"
         "emacs-org-ref"
         "emacs-tuareg"
         "emacs-projectile"
         "emacs-spacemacs-theme"
         "git"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
