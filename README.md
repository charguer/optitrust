# Requirements

- OCaml (tested with version 4.12.0)
- dune (tested with version 2.9.1)
- [clangml](https://gitlab.inria.fr/tmartine/clangml) (tested with version
  4.5.0)
- [pprint](https://github.com/fpottier/pprint) (tested with version 20220103)
- [menhir] and [menhirLib] (tested with version 20210419)

- gcc (tested with version 5.5.0)
- clang-format (tested with version 3.8.0)
- jemalloc (libjemalloc system distribution suffices)

- Visual Studio Code (tested with version 1.63.2)
- Meld (tested with version 3.18.0)


# Installation details

```
   # Installation of system packages
   sudo apt-get install clang-format meld libclang-dev llvm-dev libomp-dev pkg-config zlib1g-dev libjemalloc-dev
   # optional
   sudo apt-get valgrind

   # Installation of opam: https://opam.ocaml.org/doc/Install.html
   sudo apt-get install opam

   opam init
   opam switch create 4.12.0
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam install dune clangml pprint menhir menhirLib base64 ocamlbuild
   # (optional but recommended for vscode)
   opam install merlin ocp-indent user-setup
   # (includes ocaml-lsp-server)

   # Installation of vscode: https://code.visualstudio.com/download
   # ...download the .deb package and install it

   # Installation of header files
   cd src
   sudo make install_compcert_stdlib

```

# Path to optitrust libraries

Either: (make sure to adapt the paths)

```
export OPTITRUST=~/verified_transfo/src/
```

or

```
 sudo mkdir -p /usr/local/lib/compcert; cp ~/verified_transfo/src/src/cparser/include/* /usr/local/lib/compcert"
```


# VScode customization

```
   # Recommended extensions for VSCode (type CTRL+SHIFT+X)
     - GitLens
     - OCaml and Reason IDE (see below for details)

   # Disable option   C_Cpp: Dim Inactive Regions   in the settings

   # OCaml syntax highlighting
   # New plugins to highlight the code "OCaml and Reason IDE"
   # Type CTRL+P, then paste and execute the commande:
   #    ext install freebroccolo.reasonml
   # Alternative? ocamllabs.ocaml-platform

   # As explained in https://www.cosmiccode.blog/blog/vscode-for-ocaml/
   # for merlin to work well you need to update settings.json (global VS code settings)
   # go to file/ preferences / settings, type "settings.json", then at the very bottom click "edit".
  # with (make sure to get the path right depending on opam's version)
```
       {

         // ... other stuff


        "reason.path.ocamlmerlin": "bash -ic ~/.opam/4.12.0/bin/ocamlmerlin",
        "reason.path.ocamlfind": "bash -ic ~/.opam/4.12.0/bin/ocamlfind",
        "reason.path.ocpindent": "bash -ic ~/.opam/4.12.0/bin/ocp-indent",
        "reason.diagnostics.tools": [
            "merlin"
          ],
       }
```

In that same file, include:
```
    "files.associations": {
      "*.ml": "ocaml",
      "*.mli": "ocaml",
      "*.json": "jsonc"
	},
``

Useful entries for `keybindings.json`
```
{
    "key": "ctrl+shift+alt+t",
    "command": "workbench.action.tasks.terminate"
}
```

   #
   # (optional) Disable minimap: menu "View" / uncheck "Show Minimap".

   # (optional) Install VSCode C++ extension
   #     ext install ms-vscode.cpptools
   #
   # (optional) Install GitLens extension
   #     ext install eamodio.gitlens
```


# Build and install

Execute `make && make install` at the root of the project.

# Example

```
  code . &
  # click on test_suite/aosoa.ml in the list of files
  # press F6 to execute the comparison
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


# Shortcuts


The transformation script execution is based on a Visual Studio Code task. To
execute this task, one may use a keyboard shortcut. This shortcut triggers a
task defined in `.vscode/tasks.json`, which is part of the repository, unlike
`keybindings.json`, which is a user-specific configuration file.

Run `code` to open VSCode. To edit the `keybindings.json` file from Visual Studio
Code, type `Ctrl + Shift + P` to access the command panel and then choose
"Preferences: Open Keyboard Shortcuts (JSON)". There, replace the empty square
braces with the following contents:

```
  {
    "key": "f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff",
  },
  {
    "key": "alt+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View big step diff",
  },
  {
    "key":"shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "Recompile and view diff",
  },
  {
    "key":"ctrl+shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff for ast encoding",
  },
  {
    "key": "shift+alt+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace",
  },
  {
    "key": "f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute from intermediate state",
  },
  {
    "key": "alt+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute a big step from intermediate state",
  },
  {
    "key": "shift+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Recompile and execute from intermediate state",
  },
  {
    "key": "ctrl+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Save intermediate state",
  },

  {
    "key":"f6",
    "command": "workbench.action.tasks.runTask",
    "args": "Open unit test ML and CPP files",
    "when": "resourceFilename == Makefile && resourceDirname =~ /^.*\/verified_transfo\/src\/tests\/.*$/"
  },
  {
    "key":"ctrl+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View documentation for unit test",
  },

```

Note: on Ubuntu, alt+F6 is bound to a window moving operation,
it needs to be deactivated in the settings panel, shortcut tab,
'window' group of shortcuts.


# Optitrust flags

You can create a file called `optitrust_flags.sh` in the working directory,
to define the variable `FLAGS`. Examples include:

```
# generation of timing.log
FLAGS="-analyse-stats"
FLAGS="-analyse-stats-details"

# generation of _enc.cpp files
FLAGS="-dump-ast-details"

# reports the lines at which reparse operations are performed
FLAGS="-debug-reparse"

# add a reparse operation at every !^ symbol in the script
FLAGS="-reparse-at-big-steps"

```

Multiple flags can be passed at once in the string.


# Troubleshouting

If the watch script is not launched in a background terminal, then the execution of a task simply hangs.
Start the watch, press the shortcut again, click on "terminate/restart the task".

In case of missing opam packages, or incorrect opam switch loaded:
```
  Reason: /home/charguer/.opam/4.09.1+flambda/lib/ocaml/stublibs/dllunix.so: undefined symbol: caml_local_roots
```

# JEmalloc

Notes:
```
- JEMALLOC:
   export LD_PRELOAD=$LD_PRELOAD:/path/to/jemalloc-build/lib/jemalloc.so.1
  cc app.c -o app -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`
```
