{
  description = "OCaml dev environment with custom LLVM binary + opam switch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # ---- Custom LLVM binary derivation ----
      customLLVM = pkgs.stdenv.mkDerivation {
        pname = "optitrust_llvm";
        version = "15.0.0";

        src = pkgs.fetchurl {
          url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-15.0.0/clang+llvm-15.0.0-x86_64-linux-gnu-rhel-8.4.tar.xz";
          sha256 = "20b17fabc97b93791098e771adf18013c50eae2e45407f8bfa772883b6027d30";
        };

        phases = [ "unpackPhase" "installPhase" ];

        unpackPhase = ''
          mkdir source
          tar -xf $src -C source --strip-components=1
        '';

        installPhase = ''
          mkdir -p $out
          cp -r source/* $out/

          # Add version-suffixed symlink for llvm-config
          ln -sf $out/bin/llvm-config $out/bin/llvm-config-15
        '';
      };

      dev = pkgs.mkShell {
        buildInputs = [
          pkgs.opam
          pkgs.ocaml
          pkgs.pkg-config
          pkgs.autoconf
          pkgs.curl
          pkgs.libev
	        pkgs.ncurses
          customLLVM
	        pkgs.gcc.cc.lib
        ];

        # ---- Ensure custom LLVM is used ----
        LLVM_HOME = customLLVM;

        # ---- Where opam root lives (inside repo) ----
        OPAMROOT = ".opam";

        shellHook = ''
          export PATH="$LLVM_HOME/bin:$PATH"
          export LD_LIBRARY_PATH="$LLVM_HOME/lib:${pkgs.gcc.cc.lib}/lib:$LD_LIBRARY_PATH"
          export CPATH="$LLVM_HOME/include:$CPATH"

          # Initialize isolated opam root (project local)
          if [ ! -d "$OPAMROOT" ]; then
            echo "[nix-shell] initializing opam root"
            opam init --bare --disable-sandboxing -n

            # Create switch with your exact compiler variant
            opam switch create . --no-install \
              --packages=ocaml-variants.4.14.2+options,ocaml-option-flambda
          fi

          eval "$(opam env --switch=.)"

          # ---- Apply your pins (idempotent) ----
      	  # quit first if everything is already installed
          [ "$(printf '%s\n' dune menhirLib pprint conf-libclang clangml refl menhir base64 ocamlbuild ocaml-lsp-server ppx_deriving dream odoc lambdasoup ocaml-lsp-server dot-merlin-reader | grep -xF -f <(opam list --short --installed) | wc -l)" -eq 16 ] && exit 0

          opam pin add -y dune 3.18.2
          opam pin add -y menhirLib 20210419
          opam pin add -y pprint 20220103
          opam pin add -y conf-libclang 15 --no-depexts
          opam pin add -y clangml 4.8.0 --yes --no-action || true   # continue anyway

          # ---- Install your required packages ----
          opam install -y ocaml-lsp-server dot-merlin-reader clangml dune refl pprint menhir menhirLib base64 ocamlbuild \
            ocaml-lsp-server ppx_deriving
          opam install -y odoc lambdasoup dream

          opam init --no
        '';
      };

    in {
      devShells.${system}.default = dev;

      ## ---- Add this so that `nix shell` works ----
      packages.${system}.default = dev;
    };
}

