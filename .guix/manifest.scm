(use-modules (guix packages) (guix git-download) (gnu packages ocaml)
             (guix build-system dune))

(define-public ocaml-graph-mf
  (package
   (inherit ocaml-graph)
   (name "ocaml-graph-mf")
   (version "2.1.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/felsocim/ocamlgraph")
       (commit "052d5b9f363ccc5bf9cea5227d7895d2ea9eb41f")))
     (sha256
      (base32
       "1izd7zg0jcskbc41dpqn7iz097nnrj0wr5yjcmkj84zc7f9lz6kk"))))
   (build-system dune-build-system)
   (arguments
    `(#:install-target "install"))
   (inputs (list lablgtk ocaml-graphics ocaml-stdlib-shims))))

(concatenate-manifests
 (list
  (packages->manifest (list ocaml-graph-mf))
  (specifications->manifest
   (list "bash"
         "coreutils"
         "time"
         "gcc-toolchain@11.3"
         "libomp@13"
         "nano"
	 "graphviz"
         "ocaml-merlin"
         "emacs"
         "emacs-org"
         "emacs-org-ref"
         "emacs-tuareg"
         "emacs-projectile"
         "emacs-spacemacs-theme"
         "git"))
  (package->development-manifest
   (specification->package "ocaml-optitrust"))))
