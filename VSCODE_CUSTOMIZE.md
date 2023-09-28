

# general-purpose customization for VScode (OPTIONAL)


### Customization of VScode

Optional: install the VSCode C++ extension:
`ctrl+P` then `ext install ms-vscode.cpptools`
For Codium: install instead `llvm-vs-code-extensions.vscode-clangd`

Optional: install the GitLens extension:
`ctrl+P` then `ext install eamodio.gitlens`
Variant: install the GitGraph extension:
`ctrl+P` then `ext install mhutchie.git-graph`

In your `~/.config/Code/User/settings.json` file, add the lines:

```jsonc
  // Whitespace trimming is already activated in OptiTrust's local settings.json file
  "files.trimTrailingWhitespace": true,

  "files.insertFinalNewline": true,

  // To allow reading C++ code in ifdef sections
  "C_Cpp.dimInactiveRegions": false,

  // To enlarge the editing space
  "editor.minimap.enabled": false,

  "editor.mouseWheelZoom": true,

  "workbench.list.smoothScrolling": true,

  "files.simpleDialog.enable": true,

  // View long lines in a reasonable way
  "editor.wordWrap": "on",
  "editor.wordWrapColumn": 100,
  // Alternative:
  "editor.wordWrap": "on",
  "editor.wrappingIndent": "indent",
```

In case you wish to use a specific opam switch for VScode, e.g.:
```json
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

```json
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
```json
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
```json
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

```jsonc
  "editor.definitionLinkOpensInPeek": false,
  "editor.hover.enabled": false,
  "editor.inlineSuggest.enabled": false,
  "editor.lightbulb.enabled": false,
  "editor.codeLens": false,
  "editor.inlayHints.enabled": "offUnlessPressed", // or "off"
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
  "workbench.editor.decorations.badges": false,

  "gitblame.inlineMessageFormat": "${author.name} (${time.ago})",
  "gitblame.statusBarMessageClickAction": "Open git show",
  "gitblame.statusBarMessageFormat": "${author.name} (${time.ago})",
```
