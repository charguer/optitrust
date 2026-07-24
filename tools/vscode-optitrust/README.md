# OptiTrust VS Code Extension

VS Code support for the in-tree OptiTrust development workflow.

The extension does not reimplement OptiTrust. It provides an editor interface
over the existing project tools, especially `tools/view_result.sh`,
`tools/_last_view_result.sh`, the trace server, and `./tester`. Normal OCaml
editing remains owned by the standard OCaml extension and OCaml-LSP.

## Features

- Detects whether the opened workspace is an OptiTrust repository.
- Runs step diffs, full traces, and step traces from the current cursor line.
- Opens one universal HTML step-diff view for C/C++ and OptiLambda output.
- Uses OptiLambda Surface as the default step-diff representation.
- Switches generated diff and trace panels between C/C++, Surface, Internal,
  and Fully-Typed OptiLambda.
- Lazily generates missing OptiLambda diff representations when a panel switch
  requests them.
- Keeps generated diff and trace text searchable inside the webview.
- Groups ghost and contract blocks behind compact `G*` and `C*` buttons.
- Shows context diffs inside ghost and contract popups in step diff views.
- Shows Surface OptiLambda hover details for variables and supported syntax
  nodes.
- Reuses an attached live diff/trace panel and lets you detach a panel when you
  want to keep it.
- Applies VS Code-styled popup, hover, diff, and syntax-highlight colors in
  generated views.
- Registers `.opti` as the OptiLambda file extension.
- Adds OptiLambda bracket matching, `//` comments, and TextMate syntax
  highlighting for opened `.opti` files.
- Opens generated, expected, and other associated files from the active file.
- Runs current tests and reruns the last OptiTrust test selection.
- Displays command output and exact backend commands in the `OptiTrust` output
  panel.
- Provides a health check for the local OptiTrust installation.
- Provides OptiNLP commands and a native VS Code Chat participant named
  `@optinlp`.

## Requirements

- VS Code or VSCodium.
- Node.js 20 or newer for building and packaging the extension.
- An in-tree OptiTrust checkout.
- The normal OptiTrust development dependencies, including:
  - `opam`
  - `dune`
  - `clang`
  - `llvm-config-15`
- The standard OCaml VS Code extension is recommended for `.ml` editing.

## Setup

Start from a working OptiTrust checkout and open the repository root:

```bash
cd /path/to/optitrust
code .
```

Build the extension from its own folder:

```bash
cd tools/vscode-optitrust
npm install
npm run compile
```

Run it in a development Extension Host:

```bash
npm run dev:extension
```

This opens a separate VS Code window with the local extension loaded. In that
new window, open the OptiTrust repository root if it is not already open.

To install a local `.vsix` instead:

```bash
cd tools/vscode-optitrust
npm install
npm run compile
npm run package
code --install-extension optitrust-0.0.1.vsix
```

For VSCodium:

```bash
codium --install-extension optitrust-0.0.1.vsix
```

After installing the `.vsix`, reload VS Code and open the OptiTrust repository
root. Run this command to confirm that the extension activated:

```text
OptiTrust: Verify Extension Loaded
```

Then run:

```text
OptiTrust: Health Check
```

The health check verifies workspace detection, required tools, the backend build
for the runner and trace server, and trace-server reachability.

## Workspace Detection

The extension detects the OptiTrust root using repository-level files such as:

- `dune-project`
- `optitrust.opam`
- `tester`
- `tools/view_result.sh`
- `lib/optitrust.ml`

The current extension targets in-tree OptiTrust development. External or
off-tree OptiTrust installations are not supported yet.

If automatic detection is not enough, set:

```json
"optitrust.rootOverride": "/absolute/path/to/optitrust"
```

## Main Workflows

### View Step Diff

Open an OptiTrust `.ml` transformation script, place the cursor on or near a
transformation line, then run:

```text
OptiTrust: View Step Diff
```

Default keybinding:

```text
F6
```

The step diff opens in the universal OptiTrust HTML diff view. This is the only
user-facing step-diff view; the old native `vscode.diff` path is not used for
step diffs because it cannot host interactive HTML controls.

The step diff starts in OptiLambda Surface mode by default. Use the selector in
the diff toolbar to switch between:

- `C/C++`
- `OptiLambda Surface`
- `OptiLambda Internal`
- `OptiLambda Fully-Typed`

When a representation is not already available, the panel asks the backend to
generate it on demand and updates in place.

The diff view supports:

- side-by-side code diff rendering,
- searchable generated text,
- ghost and contract group buttons,
- contract and ghost popup diffs,
- Surface variable and syntax-node hover popups,
- the toolbar `Detach` button.

### View Full Trace

Run:

```text
OptiTrust: View Full Trace
```

Default keybinding:

```text
Shift+F5
```

The full trace opens in an OptiTrust webview. The trace tree, step navigation,
syntax selector, grouped ghost/contract controls, hovers, and search all remain
inside the same panel.

### View Step Trace

Run:

```text
OptiTrust: View Step Trace
```

Default keybinding:

```text
Shift+F6
```

This opens a trace focused on the transformation step at the cursor line.

### Trace With Saved Step Script

Run:

```text
OptiTrust: View Trace Save Steps Script
```

Default keybinding:

```text
Ctrl+F5
```

This uses the existing OptiTrust `-save-steps script` trace mode.

### Redo The Last View Command

Run:

```text
OptiTrust: Redo Last View Command
```

Default keybinding:

```text
F5
```

This reruns the last diff or trace command with the latest cursor and file
context.

### Live View Reuse And Detach

Diff and trace commands share one attached OptiTrust view slot. Re-running
`F6`, `Shift+F5`, `Shift+F6`, or `F5` updates that attached slot instead of
leaving many old views open.

Use `OptiTrust: Detach View` or the panel `Detach` button to keep the current
view. Once detached, the button changes to `Detached`, becomes disabled, and
later diff/trace commands update a new attached panel instead.

### Search In Diff And Trace Views

Use `Ctrl+F` inside generated diff and trace webviews. The search operates on
real DOM text, so generated code, diff lines, visible popup content, and trace
text remain searchable.

### Ghost And Contract Groups

Generated OptiLambda views compact consecutive ghost and contract blocks:

- square `G1`, `G2`, ... buttons represent ghost groups,
- round `C1`, `C2`, ... buttons represent contract groups.

Click a group button to open its popup. Click the same button again to close it.
In step diffs, group popups can show a context diff between the old and new
group content, highlighting removed lines in red and added lines in green.

### Surface Hover Details

Surface OptiLambda views attach VS Code-styled hover popups to supported
variables and syntax nodes. Use them for type and skeleton information without
displaying every type inline.

Internal and Fully-Typed representations currently get ghost and contract
grouping only.

### Select Default Diff/Trace Syntax

Run:

```text
OptiTrust: Select Diff/Trace Syntax
```

This writes workspace settings for commands that need to request one syntax from
the backend up front. Available modes are:

- `C/C++`
- `OptiLambda Surface`
- `OptiLambda Internal`
- `OptiLambda Fully-Typed`

The `F6` step-diff workflow intentionally opens Surface by default and then
lets the toolbar switch representations in the panel.

### Run Tests

Run the current OptiTrust test:

```text
OptiTrust: Run Current Test
```

Default keybinding:

```text
Ctrl+F10
```

The extension runs:

```bash
./tester run -with-ignored <file>
```

Rerun the last tried tests:

```text
OptiTrust: Rerun Last-Tried Tests
```

Default keybinding:

```text
F10
```

Run the current test through the OptiTrust diff workflow:

```text
OptiTrust: Run Current Test And Open Diff
```

Default keybinding:

```text
Ctrl+Shift+F10
```

### Associated Files

Use the editor-title button in the top-right of supported files, or run:

```text
OptiTrust: Open Associated Files
```

The QuickPick can open one associated file or all associated files. Supported
related file types include `.ml`, `.cpp`, `.c`, `.opti`, `.html`, `.js`, and
`.trace`.

You can also run:

```text
OptiTrust: Open Generated Output
OptiTrust: Open Expected Output
OptiTrust: Open Unit Test ML And CPP Files
```

`Open Unit Test ML And CPP Files` has this default keybinding:

```text
Alt+Shift+F10
```

## Commands

| Command | Description |
| --- | --- |
| `OptiTrust: Verify Extension Loaded` | Checks that the extension activates. |
| `OptiTrust: View Step Diff` | Shows the universal HTML diff for the transformation at the cursor line. |
| `OptiTrust: Detach View` | Keeps the current OptiTrust view open and removes it from future live updates. |
| `OptiTrust: View Full Trace` | Generates and opens a full trace in the attached OptiTrust view. |
| `OptiTrust: View Trace Save Steps Script` | Generates a full trace with `-save-steps script`. |
| `OptiTrust: View Step Trace` | Generates and opens a trace for the current step. |
| `OptiTrust: Redo Last View Command` | Re-runs the last extension view command. |
| `OptiTrust: Run Current Test` | Runs the current OptiTrust test. |
| `OptiTrust: Rerun Last-Tried Tests` | Re-runs the last test selection. |
| `OptiTrust: Run Current Test And Open Diff` | Runs the current test through `tester rundiff`. |
| `OptiTrust: Open Generated Output` | Opens generated output related to the current file. |
| `OptiTrust: Open Expected Output` | Opens expected output related to the current file. |
| `OptiTrust: Open Associated Files` | Opens the associated-files QuickPick menu. |
| `OptiTrust: Open Unit Test ML And CPP Files` | Opens the `.ml` and `.cpp` files for a unit test. |
| `OptiTrust: Select Diff/Trace Syntax` | Selects the default backend-requested view syntax. |
| `OptiTrust: Health Check` | Runs installation and backend checks. |
| `OptiTrust: Show Shortcuts` | Shows the extension shortcuts from inside VS Code. |
| `OptiTrust: Open OptiNLP Chat` | Opens native VS Code Chat for `@optinlp`. |
| `OptiTrust: OptiNLP Generate Target` | Generates a target for the active selection or file. |
| `OptiTrust: OptiNLP Generate Script` | Generates a transformation script from a command. |
| `OptiTrust: OptiNLP Generate Full Transformation` | Generates a complete transformation script for the active file. |
| `OptiTrust: OptiNLP Suggest Target At Cursor` | Runs the F7 target-at-cursor workflow for `.ml` scripts. |
| `OptiTrust: OptiNLP Set Gemini API Key` | Stores the Gemini API key used by OptiNLP. |
| `OptiTrust: OptiNLP Set OpenAI API Key` | Stores the OpenAI API key used by OptiNLP. |
| `OptiTrust: OptiNLP Set API Key` | Stores an API key for the currently selected OptiNLP provider. |
| `OptiTrust: OptiNLP Select Provider` | Selects the OptiNLP provider. |
| `OptiTrust: OptiNLP Set Model` | Sets an optional OptiNLP model override. |
| `OptiTrust: OptiNLP Clear Session` | Clears OptiNLP in-memory session context. |

## Default Keybindings

| Keybinding | Command |
| --- | --- |
| `F6` | View step diff |
| `Shift+F5` | View full trace |
| `Ctrl+F5` | View trace with `-save-steps script` |
| `Shift+F6` | View step trace |
| `F5` | Redo last view command |
| `F7` | OptiNLP suggest target at cursor |
| `F10` | Rerun last-tried tests |
| `Ctrl+F10` | Run current test |
| `Ctrl+Shift+F10` | Run current test and open diff |
| `Alt+Shift+F10` | Open unit test ML and CPP files |

On macOS, `Ctrl+F10` and `Ctrl+Shift+F10` are contributed as `Cmd+F10` and
`Cmd+Shift+F10`.

Disable all contributed keybindings with:

```json
"optitrust.enableKeybindings": false
```

## Settings

```json
"optitrust.enableKeybindings": true,
"optitrust.rootOverride": "",
"optitrust.scriptFolders": [],
"optitrust.viewSyntax": "cpp",
"optitrust.optilambdaRepresentation": "surface",
"optitrust.syntaxHighlightThemePath": "",
"optitrust.optinlpProvider": "gemini",
"optitrust.optinlpModel": "",
"optitrust.optinlpUseProviderSession": true
```

`optitrust.scriptFolders` accepts workspace-relative folders for user-created
transformation scripts.

`optitrust.viewSyntax` accepts:

- `cpp`
- `optilambda`

`optitrust.optilambdaRepresentation` accepts:

- `surface`
- `internal`
- `typed`

`optitrust.syntaxHighlightThemePath` accepts an absolute or workspace-relative
path to a VS Code color theme JSON file. Leave it empty to let the extension
try to resolve the active VS Code theme automatically.

## OptiLambda Support

The extension registers `.opti` as the OptiLambda file extension. Surface,
Internal, and Fully-Typed are representations of the same OptiLambda language;
they are not separate VS Code languages and do not use separate language ids.

It provides:

- line comments with `//`,
- bracket matching,
- TextMate syntax highlighting for opened `.opti` files,
- representation-specific `.opti` artifact discovery,
- OptiLambda display in diff and trace panels,
- grouped ghost and contract rendering in generated HTML views,
- Surface hover details in generated HTML views.

The current representation model is:

- Surface: the readable display syntax used for human-facing OptiLambda output.
- Internal: explicit internal operations such as `get`, `set`, and `ref`.
- Fully-Typed: explicit internal operations with type parameters when available.

The extension is display-oriented. Parsing OptiLambda back into the OptiTrust
AST is a future backend milestone.

## OptiNLP Native Chat

The extension contributes a native VS Code Chat participant named `@optinlp`.
This is the only OptiNLP chat UI; voice input belongs to VS Code Chat through
VS Code Speech.

Examples:

```text
@optinlp target the second loop named i
@optinlp /target target the y loop that writes to out
@optinlp /script unroll the loop i
@optinlp /full generate a full transformation script for this file
@optinlp /config
@optinlp /clear
@optinlp /help
```

When `/target` or auto mode resolves to target generation from an active `.ml`
script, OptiNLP sends the matching same-basename `.cpp` or `.c` source file as
the model context.

`F7` prepares richer target-at-cursor context by executing the current `.ml`
script through the line before the cursor, opening the generated `_after.opti`
state, and focusing native VS Code Chat. To avoid creating a new chat session,
the prepared `@optinlp /target ...` prompt is copied to the clipboard; paste it
into the existing Chat input and send it. The pending context is short-lived and
is consumed by that request.

OptiNLP source context such as `.ml`, `.cpp`, and `.opti` files is refreshed on
each request because those files may change while you work. Stable OptiNLP
prompt, knowledge, and eval files under `tools/optiNLP/` are tracked by session
hash. With a stateful provider such as OpenAI, stable prompt-kit context is sent
once per session and later requests continue from the previous provider
response. Stateless providers such as Gemini keep receiving stable context on
each request so the model has the necessary context. Disable
`optitrust.optinlpUseProviderSession` to force every request to send full
context.

For voice input, install Microsoft's `VS Code Speech` extension, open VS Code
Chat, focus the chat input, choose `@optinlp`, and use the microphone button
provided by VS Code Chat.

## Health Check

Run:

```text
OptiTrust: Health Check
```

The health check reports:

- workspace root detection,
- required external tools,
- OCaml/dune backend build status,
- trace server reachability,
- known skipped checks.

It checks tools such as:

- `opam`
- `dune`
- `clang`
- `llvm-config-15`

It also builds:

```bash
dune build tools/runner/optitrust_runner.exe tools/trace_server/trace_server.exe
```

## Development Commands

From `tools/vscode-optitrust`:

```bash
npm install
npm run compile
npm run dev:extension
npm run package
```

Useful scripts:

- `npm run compile`: rebuilds the webview highlighter bundle and TypeScript
  extension output.
- `npm run dev:extension`: starts a local VS Code Extension Host.
- `npm run watch`: watches TypeScript changes.
- `npm run package`: creates `optitrust-0.0.1.vsix`.
- `npm run test:optinlp`: compiles and runs OptiNLP tests.

After changing webview highlighting code, run `npm run compile` so
`tools/web_view/optitrust_syntax_highlight.js` is regenerated.

## Notes On Webviews And Theme Colors

Generated diff and trace views are opened inside VS Code webviews. The extension
rewrites local resources and inlines local scripts/styles so generated OptiTrust
HTML can run under VS Code webview security rules.

The panels use VS Code theme variables for backgrounds, borders, fonts, line
numbers, popups, and diff colors. Syntax highlighting is produced by the
webview highlighter bundle, which tries to resolve the active VS Code theme and
falls back to Shiki built-in themes when the active theme cannot be loaded.

VS Code webviews do not expose the exact TextMate token colors of every
installed editor theme. If the automatic theme lookup is not good enough, set
`optitrust.syntaxHighlightThemePath` to the JSON file for the theme you want the
webviews to use.

The `OptiTrust` output panel reports theme-resolution information when
generated views are opened. In the webview DOM, highlighted containers also
include diagnostic `data-optitrust-*` attributes such as the highlighter,
resolved theme, and theme source.

## Troubleshooting

If the extension does not activate:

- open the OptiTrust repository root, not only `tools/vscode-optitrust`,
- run `OptiTrust: Verify Extension Loaded`,
- check that VS Code is version 1.90 or newer.

If workspace detection fails:

- run `OptiTrust: Health Check`,
- confirm that `dune-project`, `optitrust.opam`, `tester`,
  `tools/view_result.sh`, and `lib/optitrust.ml` exist under the root,
- set `optitrust.rootOverride` to the absolute OptiTrust path if needed.

If generated views open blank:

- run `OptiTrust: Health Check`,
- inspect the `OptiTrust` output panel,
- regenerate the diff or trace,
- open the generated `_diff.html` or `_trace.html` file directly if needed.

If syntax switching in a diff panel gets stuck:

- inspect the `OptiTrust` output panel for the backend command and error,
- make sure the backend can generate OptiLambda output,
- rebuild the backend and regenerate the diff.

If colors in generated views look wrong:

- inspect the `OptiTrust` output panel for the requested and resolved theme,
- set `optitrust.syntaxHighlightThemePath` to the desired VS Code theme JSON
  file,
- run `npm run compile` after changing highlighter source files.

If `npm install` fails on WSL paths:

- run `npm install` inside WSL/Linux,
- avoid running Windows `npm` from a `\\wsl.localhost\...` path.

If npm reports unsupported Node versions:

- install Node.js 20 or newer in the environment where you build the extension.

If OptiLambda fields are missing in a trace:

- regenerate the trace after rebuilding the backend,
- old generated trace files do not contain the newer `*_optilambda_*` fields.

If a large trace stays on `Loading the trace ...`:

- reload the Extension Development Host or reinstall the rebuilt `.vsix`,
- regenerate the trace,
- check that the generated `_trace.js` file exists next to the trace HTML.

## Known Limitations

- The extension supports in-tree OptiTrust development only.
- `.ml` files remain OCaml files; this extension does not replace OCaml-LSP.
- Diff and trace generation still depends on existing OptiTrust scripts.
- Exact syntax colors in webviews may differ from custom editor themes.
- Internal and Fully-Typed OptiLambda currently have ghost and contract grouping
  but not the full Surface hover feature set.
- OptiLambda parsing is not implemented in this extension pass.
- Fully-Typed output quality depends on type information available in the AST.
