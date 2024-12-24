(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash"
         "coreutils"
         "time"
         "gdb"
         "valgrind"
         "clang-toolchain@13"
         "python"
         "python-apac-modelizer"
         "nano"
         "graphviz"
         "xdotool"
         "ocaml-merlin"
         "ocaml-dot-merlin-reader"
         "emacs"
         "emacs-org"
         "emacs-org-ref"
         "emacs-tuareg"
         "emacs-projectile"
         "emacs-treemacs"
         "emacs-spacemacs-theme"
         "git"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
