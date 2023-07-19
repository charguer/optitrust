# OptiTrust Installation

--------------------------------------------------------------------------------
## Installation

It takes about 30 minutes to install the required OCaml software.

Installation of system packages:

```
   sudo apt-get install opam clang-format meld libclang-dev llvm-dev libomp-dev pkg-config zlib1g-dev
```

Installation of OCaml ecosystem:
```
   opam init
   opam switch create 4.14.1
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam pin add clangml 4.8.0
   opam install dune clangml pprint menhir menhirLib base64 ocamlbuild ocaml-lsp-server
   eval $(opam env)
```

(TODO: check whether ocamlbuild is still needed.)


Checking your installation of OptiTrust is working:
```
   make tests
```

Installation of VSCode:

```
   sudo snap install --classic code
```

Recommended: installation of Chromium browser, which opens up
the generated "diff" pages faster than other browsers.

```
   sudo apt-get install chromium-browser
```



--------------------------------------------------------------------------------
## Configuration of VsCode

The point is to set up keybindings for interactive development of tranformation
scripts. For example, F6 shows the diff associated with the transformation
covering the cursor position in the VSCode editor.

### Install VScode

From the folder that contains present README file, open VScode using:

```
   code . &
```

### Install the OCaml platform:

Type the shortcut `ctrl+p`, then paste `ext install ocamllabs.ocaml-platform`.
(Alternatively, type `ctrl+shift+x` and look for the "Ocaml platform" extension.)

### Install the OptiTrust shortcuts for Vscode

In VScode, type `ctrl+p`, then type `keybindings.json`.
If you have an empty file, paste the following contents.
If you have a nonempty file, copy the inner contents of the outermost braces,
and merge that contents just before the final closing brace of the existing file.


```
{
  {
    "key": "shift+f5",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key": "f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key": "alt+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View big step diff",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key":"ctrl+shift+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff for ast encoding",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  // For working with long transformation scripts
  {
    "key": "shift+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Recompile and execute from intermediate state",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key": "f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute from intermediate state",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key": "alt+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute a big step from intermediate state",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
  {
    "key": "ctrl+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Save intermediate state",
    "when": "config.workspaceKeybindings.OptiTrust.enabled"
  },
}
```

Note: the shortcuts refer to tasks that are defined in `.vscode/tasks.json`,
which can be opened by pressing `ctrl+p` then typing `tasks.json`.

(TODO: create a task and a shortcut F5 for calling _last_view_result.sh)

Note: the F7 shortcuts are useful for saving checkpoints.
Press `ctrl+F7` to compute the result of a transformation at a given line
of the script. Then, type `F7` on a line further in the file.
The result should be the same as with typing `F6`, except that it runs
faster because the execution proceeds not from the beginning of the script
but instead from the line of the snapshot taken using `ctrl+F7`.



### Deactivate conficting Ubuntu binding

IMPORTANT: on Ubuntu, `Alt+F6` is bound to a window moving operation;
you can either modify the shortcut, or simpler deactivate the Ubuntu binding.
To that end, open the settings panel, the keyboard menu, the shortcut submenu,
then in the 'window' group, type on the `Alt+F6` shortcut, type `Backspace`
to disable the shortcut.


--------------------------------------------------------------------------------
## Using OptiTrust in VsCode

### Launch the watch script

For technical reasons (related to VSCode sandboxing of terminals executing scripts),
we need to launch a background script for handling the OptiTrust keyshortcuts.

Open a fresh terminal, reach the folder containing the present README file,
then type:
```
   .vscode/watch.sh
```
and leave the terminal opened in the background with the task running.

### Test your installation

We are at last ready to test the installation on a case study.

In VScode, type `ctrl+p`, type `matmul.ml`.
Place your cursor on the line starting with `!! List.iter tile`.
Type `F6`.
You should see a diff opening up in a browser.

Another shortcut to try is `shift+F5`, on any line of the `matmul.ml` file
After a dozen seconds, you should see the full transformation trace for the
matrix-multiply case study.

### Troubleshooting

If you don't see a diff, possible issues include:
   - Your shortcut is not set up correctly; when the shortcut is pressed,
     a line should be printed in the terminal with the watch.sh script;
     If needed, to investigate whether key bindings are set up properly,
     in VScode type `ctrl+K` immediately followed by `ctrl+s`, then type `alt+k`,
     then type `F6` and see whether you see "Tasks: run tasks" a entry.
   - The watch.sh script was not launched; in this case, terminate the
     task by clicking the trash icon in the bottom-right corner of the screen,
     then launch the watch script in a terminal, then try again.
   - the compilation failed due to incorrect setup; you should see error
     messages in the terminal.



--------------------------------------------------------------------------------
## OPTIONAL: Installation of additional useful tools for program optimization

### Installation of hwloc

The package `hwloc` provides the command `lstopo` used for viewing your
machine topology, and gather the identifiers of the cores to use for
multicore executions.

```
   sudo apt install -y hwloc
```

### Installation of ICC via Intel oneAPI HPC

# Download the Intel key, add it to the system keyring, then install the relevant package

```
   wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
   | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

   echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
     https://apt.repos.intel.com/oneapi all main" \
     | sudo tee /etc/apt/sources.list.d/oneAPI.list

   sudo apt update && sudo apt install -y intel-hpckit
```

### Installation of Valgrind

To install the Valgrind tool for detecting memory errors.

```
   sudo apt-get valgrind
```

### Installation of OpenMPI

To use OpenMPI with the GCC toolchain for distributed computations:

```
   sudo apt-get install libopenmpi-dev openmpi-bin
```


### Installation of jemalloc

Jemalloc is an alternative to standard malloc, for fast multi-thread allocation.
It may be activated in the PIC case study, in particular.

```
   sudo apt install -y libjemalloc-dev
```

To use it:

```
  export LD_PRELOAD=$LD_PRELOAD:/path/to/jemalloc-build/lib/jemalloc.so.1
  cc myprog.c -o myprog -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`
```


### Installation of FFTW

The FFTW (fast fourier transform) library is used by the Poison solver in the PIC case study.

```
   sudo apt-get install -y libfftw3-dev
```


--------------------------------------------------------------------------------
## OPTIONAL: general-purpose customization for VScode


### Customization of VScode

Optional: install the VSCode C++ extension:
`ctrl+P` then `ext install ms-vscode.cpptools`

Optional: install the GitLens extension:
`ctrl+P` then `ext install eamodio.gitlens`

In your `~/.config/Code/User/settings.json` file, add the lines:

```
   // Whitespace trimming is already activated in OptiTrust's local settings.json file
	"files.trimTrailingWhitespace": true,

   // To allow reading C++ code in ifdef sections
   "C_Cpp.dimInactiveRegions": false,

   // To enlarge the editing space
   "editor.minimap.enabled": false,

   // View long lines in a reasonable way
   "editor.wordWrap": "on",
   "editor.wordWrapColumn": 100,

```

In case you wish to use a specific opam switch for VScode, e.g.:
```
  "ocaml.sandbox": {
    "kind": "opam",
    "switch": "4.14.1"
  }
```


### Installation of extensions for bookmarks in files

The "Bookmarks" extension is useful to place anonymous marks throughout a given file.
The "Numbered Bookmarks" extension is useful to place numbered marks across different files.

Type the shortcut `ctrl+p`, then paste `ext install alefragnani.Bookmarks`.
Type the shortcut `ctrl+p`, then paste `ext install alefragnani.numbered-bookmarks`.

In your `~/.config/Code/User/settings.json` file, add the lines:

```
  "numberedBookmarks.navigateThroughAllFiles": "replace",
  "bookmarks.navigateThroughAllFiles": false,
```

Default keybindings for Bookmarks:

Command name | JSON Command | Default
:-:|:-:|:-:
Bookmarks: Toggle | `bookmarks.toggle` | `Ctrl+Alt+K`
Bookmarks: Jump To Next | `bookmarks.jumpToNext` | `Ctrl+Alt+L`
Bookmarks: Jump To Previous | `bookmarks.jumpToPrevious` | `Ctrl+Alt+J`
Bookmarks: Clear | `bookmarks.clear` |
Bookmarks: Clear From All Files | `bookmarks.clearFromAllFiles`
Bookmarks: List | `bookmarks.list` |
Bookmarks: List From All Files | `bookmarks.listFromAllFiles`

Alternative keybindings, merge this list into the `keybindings.json` file
(before the last closing brace).
```
  {
    "key":"f9",
    "command": "bookmarks.toggle",
    "when": "editorTextFocus"
  },
  {
    "key":"f4",
    "command": "bookmarks.jumpToNext",
    "when": "editorTextFocus"
  },
  {
    "key":"shift+f4",
    "command": "bookmarks.jumpToPrevious",
    "when": "editorTextFocus"
  },
```

Default keybindings for Numbered Bookmarks:

Command name | JSON Command | Default
:-:|:-:|:-:
Numbered Bookmarks: Toggle Bookmark 0 | `numberedBookmarks.toggleBookmark0` |
Numbered Bookmarks: Toggle Bookmark 1 | `numberedBookmarks.toggleBookmark1` | `Ctrl+Shift+1`
... | ... | ...
Numbered Bookmarks: Toggle Bookmark 9 | `numberedBookmarks.toggleBookmark9` | `Ctrl+Shift+9`
Numbered Bookmarks: Jump To Bookmark 0 | `numberedBookmarks.jumpToBookmark0` |
Numbered Bookmarks: Jump To Bookmark 1 | `numberedBookmarks.jumpToBookmark1` | `Ctrl+1`
... | ... | ...
Numbered Bookmarks: Jump To Bookmark 9 | `numberedBookmarks.jumpToBookmark9` | `Ctrl+9`
Numbered Bookmarks: Clear | `numberedBookmarks.clear` |
Numbered Bookmarks: Clear From All Files | `numberedBookmarks.clearFromAllFiles`
Numbered Bookmarks: List | `numberedBookmarks.list` |
Numbered Bookmarks: List From All Files | `numberedBookmarks.listFromAllFiles`

Suggested additional keybindings:
```
  {
    "key":"ctrl+shift+[Backquote]",
    "command": "numberedBookmarks.clearFromAllFiles"
  },
  {
    "key": "ctrl+[Backquote]",
    "command": "numberedBookmarks.listFromAllFiles"
  },
```

### Turning off of distracting programming assistants

In case you don't like automatic inserts and popups while you are typing code:

```
  "editor.codeLens": false,
  "[ocaml]": {
    "editor.definitionLinkOpensInPeek": false,
    "editor.inlayHints.enabled": "off",
    "editor.hover.enabled": false,
    "editor.inlineSuggest.enabled": false,
    "editor.tabSize": 2,
    //"editor.autoIndent": true,
    //"editor.indent
    "editor.codeLens": false,"editor.lightbulb.enabled": false,
    "editor.quickSuggestions": {
			"strings": false
		},
	},
  "editor.inlayHints.enabled": "off",
	"editor.acceptSuggestionOnCommitCharacter": false,
	"editor.suggestOnTriggerCharacters": false,
	"editor.quickSuggestions": {
        "other": false,
        "comments": false,
        "strings": false
    },
	"editor.autoClosingQuotes": "never",
	"editor.autoClosingBrackets": "never",
	"editor.autoSurround": "never",
   "gitlens.currentLine.enabled": false,
```

