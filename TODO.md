
- prendre le répo actuel, le cloner sur https://github.com/charguer/optitrust, garder uniquement src/
  /Splitting a subfolder out into a new repository/

  puis sur  git@gitlab.inria.fr:charguer/verified_transfo.git  garder uniquement les papiers/talks/biblio
  et renommer src en deprecated_src


- README.md => reprendre sc_artifact.md qui est à jour et intégrer ce qui manque de l'ancien README.md

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


- option si besoin: placer les fichiers générés par les tests dans un sous dossier

- mettre en place un raccourci pour ouvrir les fichiers tests associés à une transfo

- lire la doc de merlin pour lui faire pointer vers les sources et pas l'installation

- arthur: enlever le système des traces

- rewrite rules: comment retrouver l'expressivé de ton système


- diff for tests composed with removal of \n

- déplacer les fonctions sur les paths de internal vers path.ml, en renommant

- raffiner le système des marks between pour avoir une affinité "vers le haut ou vers le bas".

LATER

- type reconstruction

