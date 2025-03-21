
OptiTrust is a tool for user-guided, source-to-source transformations.

It is described in this draft paper:

http://www.chargueraud.org/research/2024/optitrust/optitrust.pdf

The project webpage is:

http://optitrust.inria.fr

OptiTrust is a research prototype, under active development.

Warning: in the current version, generating fully detailed HTML reports (with information about every substep) for our case studies requires a very large amount of memory, possibly more than available on your machine. We are working on making typechecking incremental to avoid this problem.

If you are interested in a demo, please get in touch with @charguer.


# OptiTrust Installation

--------------------------------------------------------------------------------
## Installation

### Install OCaml and system packages

It takes about 30 minutes to install the required OCaml software.

Installation of system packages:

```sh
   sudo apt-get install libomp-dev pkg-config zlib1g-dev
   # for C++ headers support:
   sudo apt-get install libc++-dev
   # optional, only if you prefer using `meld` over `code -d` for viewing diffs:
   sudo apt-get install meld
```

Install Clang 17. IMPORTANT: later versions are not supported by the Clangml package that OptiTrust depends upon. (Thus, don't use `sudo apt-get install clang clang-format libclang-dev llvm-dev`). You can try this procedure:

```
  wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
  sudo add-apt-repository "deb http://apt.llvm.org/oracular/ llvm-toolchain-oracular-17 main"
  sudo apt install clang-17 clang-tools-17 clang-17-doc libclang-common-17-dev libclang-17-dev libclang1-17 clang-format-17 clangd-17 libc++-17-dev libfuzzer-17-dev lldb-17 lld-17 libc++abi-17-dev
```

 Alternatively, you can try the automatic installation script, described on `https://ubuntuhandbook.org/index.php/2023/09/how-to-install-clang-17-or-16-in-ubuntu-22-04-20-04/`.

```
  wget https://apt.llvm.org/llvm.sh
  chmod u+x llvm.sh
  sudo ./llvm.sh 17
  # check:   clang-17 --version
  # removal: sudo apt remove --autoremove clang-17 lldb-17 lld-17 clangd-17
```

Installation of Opam, the OCaml package manager (don't use `sudo apt-get install opam` as it might give you an out of date version).
The following one-liner is advertised on `https://opam.ocaml.org/doc/Install.html`.

```
  bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
```

Installation of the opam switch with relevant packages:

```sh
   opam init
   opam switch create 4.14.1
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam pin add clangml 4.8.0
   opam install dune refl clangml pprint menhir menhirLib base64 ocamlbuild ocaml-lsp-server ppx_deriving
   # next line used only for generating the documentation of OptiTrust:
   opam install odoc lambdasoup
   # fancy traces
   opam install dream
   # then in any case execute the last line below
   eval $(opam env)
```

 Note: clangml 4.8.0 is from Sept 2022.

### Install precommit hooks

This command configures git to automatically run unit tests between commits. It can be ignored if you just want to try OptiTrust without contributing, and you have not downloaded the source files through git.

```sh
  make install_git_hooks
```

### Precompiling headers

For faster compilation, we precompile header files.

```sh
  make precompile
```

### Install libraries for parsing

Then, you need to either export the environment variable OPTITRUST by
executing "export OPTITRUST=`pwd`", from the optitrust folder,
or more conveniently execute the command:

```sh
  sudo make install_compcert_stdlib"
```

which essentially performs a `sudo install src/c/compcert_parser/include/*.h usr/local/lib/compcert`

### Test your installation from the command line

Checking your installation of OptiTrust is working:
```sh
   make tests
```

### Configure VScode (or VSCodium) for interactive usage

You can install either VSCode or VSCodium (more open).

Installation of VSCode (see https://code.visualstudio.com/download for alternatives)
```sh
  sudo snap install --classic code
```

Installation of VSCodium:

```sh
  sudo snap install codium --classic.
```

Alternative without use of snap:

```sh
   wget -qO - https://gitlab.com/paulcarroty/vscodium-deb-rpm-repo/raw/master/pub.gpg \
    | gpg --dearmor | sudo dd of=/usr/share/keyrings/vscodium-archive-keyring.gpg
   echo 'deb [ signed-by=/usr/share/keyrings/vscodium-archive-keyring.gpg ] https://download.vscodium.com/debs vscodium main' \
    | sudo tee /etc/apt/sources.list.d/vscodium.list
   sudo apt update
   sudo apt install codium
```

In the rest of this tutorial, we assume VSCode. If you are using VSCodium,
you probably have an existing alias from 'code' to 'codium'. If not, you
may want to add into your `~/.bashrc` the line:
```bash
   alias code='codium'
```
(or use `sudo ln -s /usr/bin/codium /usr/bin/code`).

Recommended: installation of Chromium browser, which is very fast for
opening up the "diff" pages generated by OptiTrust:

```sh
   sudo snap install chromium
```

--------------------------------------------------------------------------------
## Configuration of VSCode

The point is to set up keybindings for interactive development of tranformation
scripts. For example, F6 shows the diff associated with the transformation
covering the cursor position in the VSCode editor.

### Open VSCode

From the folder that contains present README file, open VSCode using:

```sh

   code . &
   # or
   codium . &

```

### Install the OCaml platform:

Open the extension pannel (`ctrl+shift+x`) and look for the "Ocaml platform" extension.
Alternatively, use the quick open prompt (`ctrl+p`), then paste `ext install ocamllabs.ocaml-platform`.

### Install the OptiTrust shortcuts for VSCode

In VSCode, open the file `~/.config/Code/User/keybindings.json`.
For VSCodium, this file is located at `~/.config/VSCodium/User/keybindings.json`.
If you have an empty file, paste the following contents.
If you have a nonempty file, copy the bindings into your file.

```jsonc
[
  // OptiTrust keybindings
  {
    "key": "f5",
    "command": "workbench.action.tasks.runTask",
    "args": "Redo last view command",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "shift+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+shift+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace light",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace for one step",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff only code",
    "when": "config.optitrust.enableKeybindings"
  },  
  {
    "key": "ctrl+shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff using internal syntax",
    "when": "config.optitrust.enableKeybindings"
  },
  // For working with unit tests
  {
    "key": "f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Rerun the last-tried test(s)",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Run the current test",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+shift+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Run and open diff for the current test",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "shift+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Run all the tests",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "alt+shift+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Open unit test ML and CPP files",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "alt+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Open unit test ML and CPP files and documentation",
    "when": "config.optitrust.enableKeybindings"
  },
  // For working with long transformation scripts
  {
    "key": "f7",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff from intermediate state",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "shift+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace from intermediate state",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Save intermediate state",
    "when": "config.optitrust.enableKeybindings"
  },
  // To open documentation
  {
    "key": "f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Open doc for current source file in browser",
    "when": "config.optitrust.enableKeybindings && resourceExtname == .ml"
  },
  // For killing a task, type 'ctrl+k' twice, then 'enter'
  {
     "key": "ctrl+k ctrl+k",
     "command": "workbench.action.tasks.terminate",
     "when": "config.optitrust.enableKeybindings"
  },
  // Unused "alt+f6", "alt+f7", "ctrl+shift+f6",..
  // End of OptiTrust keybindings
]  
```

Note: the shortcuts refer to tasks that are defined in `.vscode/tasks.json`,
which can be opened by pressing `ctrl+p` then typing `tasks.json`.

You can then adjust the keybindings to match your personal preferences,
by changing the key parameter.


Note: the intermediate state shortcuts (`F7`) are useful for saving checkpoints.
Press `ctrl+F7` to compute the result of a transformation at a given line
of the script. Then, type `F7` on a line further in the file.
The result should be the same as with typing `F6`, except that it runs
faster because the execution proceeds not from the beginning of the script
but instead from the line of the snapshot taken using `ctrl+F7`.

### Deactivate possibly conficting bindings

On desktop managers such as Ubuntu's, certain shortcuts such as
`Alt+F6`, `Alt+F7` etc. are bound to window manipulation operations,
e.g. window resize. If you want to use these shortcuts, you may wish
to deactivate the Ubuntu bindings. To that end, use the following commands:

```sh
  sudo apt-get install dconf-editor
  gsettings set org.gnome.desktop.wm.keybindings begin-move []
  gsettings set org.gnome.desktop.wm.keybindings begin-resize []
  gsettings set org.gnome.desktop.wm.keybindings cycle-group []
  gsettings set org.gnome.desktop.wm.keybindings cycle-group-backward []
  gsettings set org.gnome.desktop.wm.keybindings toggle-maximized []
```

Alternatively, you can open the settings panel, the keyboard menu, the
shortcut submenu, then in the search bar type `Alt+F` (or go to the
'window' group), then select an action, and type `Backspace` to disable
the shortcut, then click on the `save` button.


--------------------------------------------------------------------------------
## Using OptiTrust in VSCode


### Test your installation

We are at last ready to test the installation on a case study.

In VScode, open `case_studies/matmul/matmul.ml` (using e.g. `ctrl+p`).
Place your cursor on the line starting with `!! List.iter tile`.
Type `F6`.
You should see a diff opening up in a browser.

Another shortcut to try is `shift+F5`, on any line of the `matmul.ml` file
After a dozen seconds, you should see the full transformation trace for the
matrix-multiply case study.

### Viewing keyboard shortcuts

It may not be easy at first to recall all the shortcuts. Besides using a sticker
at the bottom of your screen, you can use the command `./shortcuts.sh` to display
the shortcuts, and in VSCode use menu File / Preferences / Keyboard Shortcuts,
then type "optitrust" in the query bar.

### Troubleshooting

If you don't see a diff, possible issues include:
   - Your shortcut is not set up correctly; when the shortcut is pressed,
   a terminal should open to show the output of the task.
     If needed, to investigate whether key bindings are set up properly,
     in VScode type `ctrl+K` immediately followed by `ctrl+s`, then type `alt+k`,
     then type `F6` and see whether you see "Tasks: run tasks" a entry.
   - the compilation failed due to incorrect setup; you should see error
     messages in the terminal.


--------------------------------------------------------------------------------
## Documentation

The documentation for OptiTrust is generated using the OCaml 'odoc' tool.

https://ocaml.github.io/odoc/odoc_for_authors.html

The top-level command `make doc` calls 'dune build @doc', which invokes 'odoc'.
Then, the generated documentation is copied into the folder '_doc'.
There, the documentation is patched in order to add the diffs associated
with the documentation unit tests (files tests/.../*_doc.ml).

The patch operation is implemented in "doc/add_tests_into_doc.ml".

The top-level command `make viewdoc` compiles the doc and opens it.


--------------------------------------------------------------------------------
## Tester

For running the test suite, execute:

```sh
  cd ~/optitrust
  ~/tester run
```

See the header of the file `tester.ml` for the full documentation.

Optional: in your `~/.bashrc` you can add an alias to make it easier to invoke the tester.

```sh
  alias t='~/optitrust/tester'
```


--------------------------------------------------------------------------------
## Optional tools

See `INSTALL_EXTRA.md` for a list of additional useful tools for program optimization.

See `VSCODE_CUSTOMIZE.md` for useful tips for using VScode or VScodium.



