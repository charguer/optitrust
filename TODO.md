
- prendre le répo actuel, le cloner sur https://github.com/charguer/optitrust, garder uniquement src/
  /Splitting a subfolder out into a new repository/

  puis sur  git@gitlab.inria.fr:charguer/verified_transfo.git  garder uniquement les papiers/talks/biblio
  et renommer src en deprecated_src


- README.md => reprendre sc_artifact.md qui est à jour et intégrer ce qui manque de l'ancien README.md
  + opam pin add clangml 4.8.0

- LICENSE: gnu gpl 3

- loop.ml

   // function on subterm (ast local)
   let swap_on (index : int) (t : trm) : trm =
      // only these functions produce new pieces of ast

   // function on paths (ast global)
   let swap_at (index : int) : Target.Transfo.local = // trm -> path -> trm
     Target.apply_on_path (swap_on index)

   // function for end-user (ast global)
   let swap (tg : target) : unit =
     apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
       (fun t (p,i) -> swap_at_path i t p) tg

   // Variable.inline  ==> Variable.inline_basic

- /tests/
  combi & basic à plat

- mettre un fichier driver.ml pour les tests

- think about parsing and `(expr "2")` vs `(lit "2")` (e.g. loop shift transformation):
  ```ocaml
  let lit l =
       try trm_int (int_of_string l)
       with _ ->
       try trm_double (float_of_string l)
       with _ ->
       code (Lit l)
  ```

- rajouter le dossier 'case_studies' dans un 'make test' à la racine

- mettre en place un raccourci pour ouvrir les fichiers tests associés à une transfo

- lire la doc de merlin pour lui faire pointer vers les sources et pas l'installation

- rewrite rules: comment retrouver l'expressivé de ton système


- déplacer les fonctions sur les paths de internal vers path.ml, en renommant



LATER

- raffiner le système des marks between pour avoir une affinité "vers le haut ou vers le bas".

- type reconstruction

- add a mechanism for computing tests twice, once with reparsing in-betweeen every small steps

- option si besoin: placer les fichiers générés par les tests dans un sous dossier