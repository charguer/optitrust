(concatenate-manifests
 (list
  (specifications->manifest
   (list "bash"
         "coreutils"
         "time"
         "gdb"
         "valgrind"
         "gcc-toolchain@11.3"
         "libomp@13"
         "python"
         "python-apac-modelizer"
         "nano"
         "graphviz"
         "xdotool"
         "ocaml-merlin"
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
