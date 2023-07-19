
If you want to skip this step in future use of VScode, type
`gnome-session-properties`, add an entry with the name
`optitrust watcher` and the path to the watch script
(e.g. `~/optitrust/src/.vscode/watch.sh`)





### For future use
```
  // LATER: FOR FUTURE USE ONLY
    {
    "key":"ctrl+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View documentation for unit test",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },


  {
    "key":"shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "Recompile and view diff",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },

  {
    "key":"f6",
    "command": "workbench.action.tasks.runTask",
    "args": "Open unit test ML and CPP files",
    "when": "config.workspaceKeybindings.OptiTrust.enabled && resourceFilename == Makefile && resourceDirname =~ /^.*\/verified_transfo\/src\/tests\/.*$/"
  },



-
```
 sudo mkdir -p /usr/local/lib/compcert; cp ~/verified_transfo/src/src/cparser/include/* /usr/local/lib/compcert"
```




- prendre le répo actuel, le cloner sur https://github.com/charguer/optitrust, garder uniquement src/
  /Splitting a subfolder out into a new repository/

  puis sur  git@gitlab.inria.fr:charguer/verified_transfo.git  garder uniquement les papiers/talks/biblio
  et renommer src en deprecated_src


- README.md => reprendre sc_artifact.md qui est à jour et intégrer ce qui manque de l'ancien README.md
  + ```
    opam init
    opam switch create optitrust 4.14.1
    opam pin add menhirLib 20210419
    opam pin add pprint 20220103
    opam pin add clangml 4.8.0
    opam install dune clangml pprint menhir menhirLib base64 ocamlbuild
    eval $(opam env)
    ```
TODO: une commande avec les -y  pour répondre yes et des ;\  pour pouvoir exécuter en une seule fois
=> mieux encore, make setup, voir comme fait françois pottier dans
  https://gitlab.inria.fr/fpottier/sek/-/blob/master/Makefile

  + `ocamllsp --fallback-read-dot-merlin` for LSP (VSCode) .merlin support

- update keybindings and templates
  + "Maj+F6" (dune), + recover fast "F6" (dune without hanging, don't check dependencies)
    + test if script in other project uses installed optitrust and is fast
  + display tree of details for specific steps

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
  - essayé, Ctrl+click ne follow plus

- rewrite rules: comment retrouver l'expressivé de ton système


- déplacer les fonctions sur les paths de internal vers path.ml, en renommant



LATER

- raffiner le système des marks between pour avoir une affinité "vers le haut ou vers le bas".

- type reconstruction

- add a mechanism for computing tests twice, once with reparsing in-betweeen every small steps

- option si besoin: placer les fichiers générés par les tests dans un sous dossier





   // deactivate existing binding
  {
    "key":"f5",
    "command": "-workbench.action.debug.start",
    "when": "debuggersAvailable && debugState != 'initializing'"
  },

  {
    "key":"f5",
    "command": "workbench.action.tasks.runTask",
    "args": "Compile the last-tried test(s)",
    "when": "resourceDirname =~ /^.*\/verified_transfo\/src\/src\/.*$/ || resourceDirname =~ /^.*\/verified_transfo\/src\/src\/transfo\/.*$/"
  },
  {
    "key":"f5",
    "command": "workbench.action.tasks.runTask",
    "args": "Compile the current test",
    // "when": "resourceDirname =~ /^.*\/verified_transfo\/src\/tests\/.*$/"
  },
    {
    "key":"shift+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace",
     // "args": "Compile the current test with -dump-trace",
  },
    {
    "key":"alt+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace with -trace-details-only-for-line",
  },





    {
      "label": "Compile the current test",
      "type": "shell",
      "command": "./tester",
      "args": [
        "${relativeFile}"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },
    {
      "label": "Compile the current test with -dump-trace",
      "type": "shell",
      "command": "./tester",
      "args": [
        "${relativeFile}",
        "-dump-trace"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },
    {
      "label": "Compile the last-tried test(s)",
      "type": "shell",
      "command": "./tester",
      "args": [
        "__last"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },




# autostart watch
gnome-terminal --geometry 100x30+0+0 -e "bash -c \"${OPTITRUST}/.vscode/watch.sh\""

e.g.
gnome-terminal --geometry 100x30+0+0 -e "bash -c \"~/shared/verified_transfo/src/.vscode/watch.sh\""


(* *************************************************************************************************************
  Note: to see a diff at the level of the OptiTrust AST, use:
    -dump-ast-details
  and the shortcut "ctrl+shift+f6" for opening the diff between [*_before_enc.cpp] and [*_after_enc.cpp]
***************************************************************************************************************)



prevent generation of .ml files in folders by shortcuts


todate=$(date -d 2013-07-18 +%s)
cond=$(date -d 2014-08-19 +%s)

if [ $todate -ge $cond ];
then
    break
fi

#DATE_MODIF_LIB=`find ${OPTITRUST_FOLDER}/src -name "*.ml" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f1-1 -d" "`
#DATE_MODIF_SRC=`find . -name "${FILEBASE}.ml" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f1-1 -d" "`
#DATE_MODIF_CMX=`find . -name "${PROG}" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f1-1 -d" "`

# Explainations:
#   %T@ gives you the modification time like a unix timestamp
#   sort -n sorts numerically,
#   tail -1 takes the last line (highest timestamp),
#   cut -f1 -d" " cuts away the second field (the filename) from the output


#if (( $(echo "${DATE_MODIF_CMX} > ${DATE_MODIF_LIB}" |bc -l) )); then
#  echo "cmx more recent than lib"
#fi




Useful entries for `keybindings.json`
```
{
    "key": "ctrl+shift+alt+t",
    "command": "workbench.action.tasks.terminate"
}
```


# Usage

A transformation script is a `.ml` file. See [`SCRIPT.md`](SCRIPT.md) for
instructions on how to write scripts.

To edit a transformation script with the possibility of (partially) executing
it:
- Copy the `.vscode` directory into the directory that contains the script.
- In a terminal, go to the script directory and open Visual Studio Code by
  executing `code .`.
- Select the script file in the Visual Studio Code interface.
- To execute the script up to a given instruction, put the cursor on the line of
  this instruction and use the shortcut chosen during the setup phase.
  Assumption: the instruction holds on one line.
- To fully execute the script, just put the cursor on the line of the last
  instruction and use the shortcut.

When a script is (partially) executed, a diff for the source code before and
after the last transformation step is displayed with Meld.


# Source codes

Source codes in C or in C++ are allowed, but only a subset of these languages is
dealt with.

Constraints on switch statements:
- Cases must end with a break instruction.
- Nested cases must share their entire body. We call them case groups.

Constraint on `const` variables: it is forbidden to use the "address of"
operator on them.

Variables that are not `const` are heap allocated: the corresponding AST use
`const` pointers to such variables. This should be transparent for the user.


# Optitrust flags

You can create a file called `optitrust_flags.sh` in the working directory,
to define the variable `FLAGS`. Examples include:

```
# generation of stats.log
FLAGS="-analyse-stats"
FLAGS="-analyse-stats-details"

# generation of _enc.cpp files
FLAGS="-dump-ast-details"

# reports the lines at which reparse operations are performed
FLAGS="-debug-reparse"

# add a reparse operation at every !^ symbol in the script
FLAGS="-reparse-at-big-steps"

```

In case of missing opam packages, or incorrect opam switch loaded:
```
  Reason: /home/charguer/.opam/4.09.1+flambda/lib/ocaml/stublibs/dllunix.so: undefined symbol: caml_local_roots
```
