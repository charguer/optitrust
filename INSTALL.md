
# OptiTrust Installation

--------------------------------------------------------------------------------

The system installation instructions below are provided for systems running Ubuntu. There is an experimental option to install using a Nix flake, which contains the required opam environment and LLVM dependencies in an isolated environment, but it is known to have issues on systems with a newer libc (such as Arch Linux), and does not integrate well with VSCode/ocaml LSP. Instructions for the nix flake [are also provided below](#nix-flake-installation-experimental).

## System Installation

### Install system packages

It takes about 30 minutes to install the required OCaml software.

Installation of system packages:

```sh
   # lib for OpenMP, for dev files, and coreutils for nohup
   sudo apt-get install libomp-dev pkg-config zlib1g-dev coreutils
   # for interactive task watcher support:
   sudo apt install inotify-tools
   # optional, only if you prefer using `meld` over `code -d` for viewing diffs:
   sudo apt-get install meld
```

### Install Clang 15.0.0 (exactly)

Next, we need to install exactly version 15.0.0 of Clang. Other versions 15.0.x are not supported by the Clangml package that OptiTrust depends upon. (In particular, don't use `sudo apt-get install clang libclang-dev llvm-dev`, and don't use `sudo ./llvm.sh 15` to install Clang-15). The following procedure install 15.0.0 from binaries:

```sh
# Download and install binaries from: https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.0
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-15.0.0/clang+llvm-15.0.0-x86_64-linux-gnu-rhel-8.4.tar.xz
tar -xf clang+llvm-15.0.0-x86_64-linux-gnu-rhel-8.4.tar.xz
sudo mv clang+llvm-15.0.0-x86_64-linux-gnu-rhel-8.4 /opt/clang-15.0.0
```

Because LLVM does not ship C++ headers, you need to install them separately:
```sh
# Install for C++ headers support:
sudo apt-get install libc++abi-15-dev
```

Depending on your prior installation, you might need to add the newly installed version of clang/llvm-config to the path, then select it among all of your versions :

```bash
  sudo update-alternatives --install /usr/bin/clang clang /opt/clang-15.0.0/bin/clang 100
  sudo update-alternatives --config clang
  # then type the number that matches the /opt path
```

```bash
  sudo update-alternatives --install /usr/bin/llvm-config llvm-config /opt/clang-15.0.0/bin/llvm-config 100
  sudo update-alternatives --config llvm-config
  # then type the number that matches the /opt path
```

Finally, update your `$PATH` to include the newly installed clang/LLVM at the front of the `$PATH`:

```bash
export PATH="/opt/clang-15.0.0/bin/:$PATH"
```

This step is important in addition to the update-alternatives step because the `clangml` package looks for the `llvm-config-15` binary in particular, and if this points to an old installation, it will use that version of clang instead.

If everything goes well `clang --version` and `llvm-config-15 --version` both report 15.0.0.
The command `clang -v -E -stdlib=libc++ - < /dev/null` shows the path to the include folders.
One of these folders should include the `functional` C++ library, e.g. `ls /usr/include/c++/11/functional`.


### Install OCaml packages

LATER: ideally, we would not require a global switch, as it is bad for benchmarking.

Installation of Opam, the OCaml package manager (don't use `sudo apt-get install opam` as it might give you an out of date version).
The following one-liner is advertised on `https://opam.ocaml.org/doc/Install.html`.

```
  bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
```

Installation of the opam switch with relevant packages (it seems that it must be done after `clang 15.0.0` is installed):

```sh
   opam init
   opam switch create 4.14.2+options --packages=ocaml-variants.4.14.2+options,ocaml-option-flambda
   opam pin add dune 3.18.2
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam pin add conf-libclang 15
   opam pin add clangml 4.8.0  # -> continueanyway
   opam install dune refl pprint menhir menhirLib base64 ocamlbuild ocaml-lsp-server ppx_deriving
```

For clangml in particular, use the `--verbose` flag to double check that clangml is using the right version (15.0.0):

```bash
opam install --verbose clangml
# Should see a line in the output of the configure script ran by opam that looks like:
# - checking for llvm-config... /usr/bin/llvm-config
# - checking llvm-config version... 15.0.0
```

Install remaining packages:

```bash
   # next line used only for generating the documentation of OptiTrust:
   opam install odoc lambdasoup
   # fancy traces
   opam install dream
   # then in any case execute the last line below
   eval $(opam env)
```

Note: dune 3.19.0 is known to have an issue, when executing the tester script use 3.18.2 instead

Note: clangml 4.8.0 is from Sept 2022.

Note: graphics is used by the pview tool only.

Next, jump to the [Optitrust Setup](#optitrust-setup) section.

## Nix flake installation (experimental)

### Install Nix

First make sure Nix is installed on your system. Follow [the instructions for a multi-user Nix installation](https://nixos.org/download/). We recommend using this install script instead of whatever package your distro may provide for Nix.

### Enable Nix flakes

We use flakes, which are still an experimental Nix feature. Add this line to `~/.config/nix/nix.conf` (create the directory if it doesn't exist) to permanently enable the feature:

```
experimental-features = nix-command flakes
```

### Test that it works

In the root of the OptiTrust repo, run `nix develop`. If it works, this will drop you into a shell with the special OptiTrust Nix environment. Note that unlike e.g. a Docker container, the binaries and libraries of your system are still exposed to the environment. The environment simply shadows all of the packages you have installed on your system. In some cases, this will break things because something on your system may for example depend on a newer version of `libc` than the Nix environment has. Depending on your system's libc, you may be able to run the system VSCode inside this environment, otherwise you have to run nix develop inside the VS Code terminal.

The first time running `nix develop` will take a while as it is setting up all the system packages for the environment, as well as installing OCaml and all of OptiTrust's OCaml dependencies. If it is working, you should not see any errors from the output of all the `opam` commands that were run to set up the environment for the first time.

Note that any time you want to run commands related to OptiTrust (opam clang etc.), you will need to drop into this shell again with `nix develop`. After the first time, everything is cached so it is much faster. 

Follow to the next section to test that your OptiTrust installation works.

## OptiTrust setup

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

If you ever see an error about "PCH files", run `make clean` to remove compiled headers.

### Test your installation from the command line

Checking your installation of OptiTrust is working:

```sh
  ./tester run tile
```
If this works, try :

```sh
   make tests
```

### Configure VScode (or VSCodium) for interactive usage

You can install either VSCode or VSCodium (more open). You should install this on the system even if you are using the Nix environment.
To install VSCode, visit: https://code.visualstudio.com/docs/?dv=linux64_deb

Download the `.deb` package, then open it using "Software Install".

Alternative installation procedure for Codium via apt-get:

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


### Direnv setup for automatic Nix shell activation in VSCode (experimental)

If vscode works within the nix shell on your system, there is a vscode extension which can activate the shell automatically whenever you open the repo. That way you don't have to launch a terminal, do `nix develop`, and then run `code .` As with the nix shell your mileage may vary.

Note: all of this installation happens on your system.

1. [Install direnv](https://direnv.net/docs/installation.html). You don't have to add the shell hook as written on the website. Just do step 1.

2. [Install nix-direnv](https://github.com/nix-community/nix-direnv?tab=readme-ov-file#from-source). You are following the "from source" instructions here.

3. [Install direnv extension for VSCode](https://marketplace.visualstudio.com/items?itemName=mkhl.direnv).

Once installed, when you open the OptiTrust repo in VScode, you should first be prompted by the `direnv` extension in the bottom right to allow the `.envrc` in the repo to be run. Hit `Allow`. It should then ask you if you want to reload the window to activate the Nix environment. Once reloaded, the OCaml extension should also now be able to see the opam environment belonging to your Nix. Terminals you open in the editor will also now inherit the nix environment, so you don't have to type `nix develop` in them.

--------------------------------------------------------------------------------
## Browser installation

NOTE: If you're using the Nix shell, don't try to install a browser in the Nix environment, install it on the system as you would normally.

Recommended: installation of Chromium browser, which is very fast for
opening up the "diff" pages generated by OptiTrust:

```sh
   sudo snap install chromium
```

Note that chromium may not work with snap on some ubuntu versions, if needed use firefox instead. If you want to use a specific browser, include a line in your ~/.bashrc such as:

```sh
export OPTITRUST_BROWSER=firefox
```

Firefox can also be used, and can also be installed using snap, however you
may encountered issues with GTK libs conflicting or not being found.
To install Firefox via apt:

```
sudo install -d -m 0755 /etc/apt/keyrings  # might not be needed
wget -q https://packages.mozilla.org/apt/repo-signing-key.gpg -O- | sudo tee /etc/apt/keyrings/packages.mozilla.org.asc > /dev/null
gpg -n -q --import --import-options import-show /etc/apt/keyrings/packages.mozilla.org.asc | awk '/pub/{getline; gsub(/^ +| +$/,""); if($0 == "35BAA0B33E9EB396F59CA838C0BA5CE6DC6315A3") print "\nSuccess.\n"; else print "\nFailure.\n"}'
echo "deb [signed-by=/etc/apt/keyrings/packages.mozilla.org.asc] https://packages.mozilla.org/apt mozilla main" | sudo tee -a /etc/apt/sources.list.d/mozilla.list > /dev/null
# Give priority:
echo '
Package: *
Pin: origin packages.mozilla.org
Pin-Priority: 1000
' | sudo tee /etc/apt/preferences.d/mozilla
sudo apt update && sudo apt install firefox
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
    "key": "ctrl+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace -save-steps script",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+shift+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View standalone trace -save-steps script",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "alt+ctrl+shift+f5", // experimental, only for advanced users, heavier trace
    "command": "workbench.action.tasks.runTask",
    "args": "View standalone trace -save-steps important",
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
  {
    "key": "alt+ctrl+shift+f10",
    "command": "workbench.action.tasks.runTask",
    "args": "Compile with gcc", // LATER: might want to use clang instead
    "when": "config.optitrust.enableKeybindings"
  },
  // For working with long transformation scripts (might not be maintained)
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
  // For viewing documentation
  {
    "key": "f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Open doc for current source file in browser",
    "when": "config.optitrust.enableKeybindings && (resourceExtname == .ml || resourceExtname == .cpp)"
  },
  {
    "key": "ctrl+f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute make viewdoc",
    "when": "config.optitrust.enableKeybindings"
  },
  {
    "key": "ctrl+shift+f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute make doc viewdoc",
    "when": "config.optitrust.enableKeybindings"
  },
  // For testing OptiTrust shortcuts
  {
    "key": "alt+shift+f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Test OptiTrust Shortcuts",
    "when": "config.optitrust.enableKeybindings"
  },
  // For working with pview (experimental for one case study)
  {
    "key": "shift+f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute Pview Makefile",
    "when": "config.optitrust.enableKeybindings && resourceDirname =~ /^.*\/pview\/.*$/"
  },
  {
    "key": "shift+f11",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute Pview Demo",
    "when": "config.optitrust.enableKeybindings && resourceDirname =~ /^.*\/demo_pview\/.*$/"
  },
  // For killing a task, type 'ctrl+k' twice, then 'enter'
  {
     "key": "ctrl+k ctrl+k",
     "command": "workbench.action.tasks.terminate",
     // "args": "Kill the current task",
     "when": "config.optitrust.enableKeybindings"
  }
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

### Troubleshooting GUI

If you don't see a diff, possible issues include:
   - Your shortcut is not set up correctly; when the shortcut is pressed,
   a terminal should open to show the output of the task.
     If needed, to investigate whether key bindings are set up properly,
     in VScode type `ctrl+K` immediately followed by `ctrl+s`, then type `alt+k`,
     then type `F6` and see whether you see "Tasks: run tasks" a entry.
   - the compilation failed due to incorrect setup; you should see error
     messages in the terminal.

### Troubleshooting Compilation

If you see the error saying that pch files have been generated by a newer version of clang,
execute: `cd ~/optitrust; rm include/*.pch; touch disable_precompile.txt`
to disable the pch generation feature.

If you see the error 'functional.h' not found, then check the existence of C++ header files
in the included folders listed by `clang -v -E -stdlib=libc++ - < /dev/null`.
In particular: `ls /usr/include/c++/v1/iostream; ls /usr/lib/llvm-15/lib/libc++.so`
should work. These files are provided by `sudo apt-get install libc++-dev libc++abi-15-dev`.


--------------------------------------------------------------------------------
## Documentation

The documentation for OptiTrust is generated using the OCaml 'odoc' tool.

https://ocaml.github.io/odoc/odoc/odoc_for_authors.html

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

