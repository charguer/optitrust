# OptiTrust VS Code Extension

VS Code support for the in-tree OptiTrust development workflow.

The extension does not reimplement OptiTrust. It provides an editor interface over
the existing project tools, especially `tools/view_result.sh`, `tools/_last_view_result.sh`,
and `./tester`. Normal OCaml editing remains owned by the standard OCaml extension
and OCaml-LSP.

## Features

- Detects whether the opened workspace is an OptiTrust repository.
- Remembers the detected OptiTrust project root.
- Warns clearly when the workspace is not supported.
- Runs a step diff for the transformation at the current cursor line.
- Runs a full standalone transformation trace for the current script.
- Runs a step trace for the transformation at the current cursor line.
- Re-runs the last OptiTrust view command.
- Runs the current OptiTrust test.
- Displays command output in a dedicated `OptiTrust` output panel.
- Shows the exact command executed by the extension.
- Shows progress notifications for long-running commands.
- Reports command failures with readable VS Code messages.
- Detects OptiTrust transformation scripts before running transformation commands.
- Passes the current 1-based cursor line to OptiTrust commands.
- Supports scripts in `tests`, `case_studies`, and configured user script folders.
- Opens generated output files for the current script or test.
- Opens expected output files for the current test.
- Compares generated output against expected output.
- Discovers associated files using suffix and extension rules.
- Provides a top-right editor button for associated files.
- Supports related `.ml`, `.cpp`, `.c`, `.opti`, `.html`, `.js`, and `.trace` files.
- Provides default OptiTrust keybindings that can be disabled.
- Removes the need to copy OptiTrust shortcuts into `keybindings.json`.
- Registers `.opti` as the OptiLambda file extension.
- Adds OptiLambda bracket matching and `//` comments.
- Adds OptiLambda syntax highlighting with theme-friendly TextMate scopes.
- Shows generated diff and trace views inside VS Code webviews.
- Reuses an existing diff/trace panel for the same file and view metadata.
- Lets generated diffs switch between C/C++ and OptiLambda representations in the panel.
- Lets standalone traces switch between C/C++ and OptiLambda representations in the panel.
- Supports server-backed trace requests with `syntax=cpp` or `syntax=optilambda&repr=...`.
- Provides a health check for the OptiTrust installation.
- Supports local `.vsix` packaging and manual installation.

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

## Install From `.vsix`

From `tools/vscode-optitrust`:

```bash
npm install
npm run compile
npm run package
```

Then install the generated package:

```bash
code --install-extension optitrust-0.0.1.vsix
```

For VSCodium:

```bash
codium --install-extension optitrust-0.0.1.vsix
```

After installing, reload VS Code and open the OptiTrust repository root.

## Workspace Detection

The extension detects the OptiTrust root using repository-level files such as:

- `dune-project`
- `optitrust.opam`
- `tester`
- `tools/view_result.sh`
- `lib/optitrust.ml`

The current MVP targets in-tree OptiTrust development. External or off-tree
OptiTrust installations are not supported yet.

If automatic detection is not enough, set:

```json
"optitrust.rootOverride": "/absolute/path/to/optitrust"
```

## Main Workflows

### View A Step Diff

Open an OptiTrust script, place the cursor on or near a transformation line, then run:

```text
OptiTrust: View Step Diff
```

Default keybinding:

```text
F6
```

The generated diff opens inside VS Code. When OptiLambda payloads are available,
the panel can switch in place between:

- C/C++,
- OptiLambda Surface,
- OptiLambda Internal,
- OptiLambda Fully-Typed.

### View A Full Trace

Run:

```text
OptiTrust: View Full Trace
```

Default keybinding:

```text
Shift+F5
```

The trace opens in the standard OptiTrust trace viewer inside VS Code. The tree,
step navigation, and controls are preserved. The representation selector switches
the displayed code and diff content without replacing the trace UI.

### View A Step Trace

Run:

```text
OptiTrust: View Step Trace
```

Default keybinding:

```text
Shift+F6
```

For server-backed traces, the selected syntax is sent to the trace server as:

```text
syntax=cpp
syntax=optilambda&repr=surface
syntax=optilambda&repr=internal
syntax=optilambda&repr=typed
```

### Run The Current Test

Run:

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

Output appears in the `OptiTrust` output panel.

### Open Associated Files

Use the editor-title button in the top-right of supported files, or run:

```text
OptiTrust: Open Associated Files
```

The QuickPick menu can:

- open all associated files,
- open one associated file,
- compare generated and expected outputs when a pair exists.

## Commands

| Command | Description |
| --- | --- |
| `OptiTrust: Verify Extension Loaded` | Checks that the extension activates. |
| `OptiTrust: View Step Diff` | Shows the diff for the transformation at the cursor line. |
| `OptiTrust: View Diff Only Code` | Shows a reduced code-only diff. |
| `OptiTrust: View Diff Using Internal Syntax` | Shows the legacy internal syntax diff mode. |
| `OptiTrust: View Full Trace` | Generates and opens a full standalone trace. |
| `OptiTrust: View Trace Save Steps Script` | Generates a full trace with `-save-steps script`. |
| `OptiTrust: View Step Trace` | Generates and opens a trace for the current step. |
| `OptiTrust: Redo Last View Command` | Runs `tools/_last_view_result.sh`. |
| `OptiTrust: Run Current Test` | Runs the current OptiTrust test. |
| `OptiTrust: Rerun Last-Tried Tests` | Re-runs the last test selection. |
| `OptiTrust: Run Current Test And Open Diff` | Runs the current test, then opens the associated diff. |
| `OptiTrust: Open Generated Output` | Opens generated output related to the current file. |
| `OptiTrust: Open Expected Output` | Opens expected output related to the current file. |
| `OptiTrust: Compare Output With Expected` | Opens a VS Code diff for generated vs expected output. |
| `OptiTrust: Open Associated Files` | Opens the associated-files QuickPick menu. |
| `OptiTrust: Open Unit Test ML And CPP Files` | Opens the `.ml` and `.cpp` files for a unit test. |
| `OptiTrust: Select Diff/Trace Syntax` | Selects the default server-backed view syntax. |
| `OptiTrust: Health Check` | Runs installation and backend checks. |

## Default Keybindings

| Keybinding | Command |
| --- | --- |
| `F6` | View step diff |
| `Ctrl+F6` | View diff only code |
| `Ctrl+Shift+F6` | View diff using internal syntax |
| `Shift+F5` | View full trace |
| `Ctrl+F5` | View trace with `-save-steps script` |
| `Shift+F6` | View step trace |
| `F5` | Redo last view command |
| `F10` | Rerun last-tried tests |
| `Ctrl+F10` | Run current test |
| `Ctrl+Shift+F10` | Run current test and open diff |
| `Alt+Shift+F10` | Open unit test ML and CPP files |

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
"optitrust.optilambdaRepresentation": "surface"
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

Generated step diffs and standalone full traces keep the normal OptiTrust UI and
provide an in-panel selector for C/C++ and the three OptiLambda representations.
The settings are mainly used for server-backed views and commands that need to
request one syntax up front.

## OptiLambda Support

The extension registers `.opti` as the OptiLambda file extension. Surface,
Internal, and Fully-Typed are representations of the same OptiLambda language;
they are not separate VS Code languages and do not use separate language ids.

It provides:

- line comments with `//`,
- bracket matching,
- basic syntax highlighting,
- theme-friendly TextMate scopes,
- generated/expected `.opti` comparison support,
- representation-specific `.opti` artifact discovery,
- OptiLambda display in diff and trace panels.

The current representation model is:

- Surface: the readable display syntax used for human-facing OptiLambda output.
- Internal: explicit internal operations such as `get`, `set`, and `ref`.
- Fully-Typed: explicit internal operations with type parameters when available.

The extension is display-oriented. Parsing OptiLambda back into the OptiTrust AST
is a future backend milestone.

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

## Notes On Webviews And Theme Colors

Generated diff and trace views are opened inside VS Code webviews. The extension
rewrites local resources and inlines local scripts/styles so generated OptiTrust
HTML can run under VS Code webview security rules.

The panels use VS Code theme variables for backgrounds, borders, fonts, line
numbers, and diff colors. C/C++ syntax highlighting is still produced by the
existing Highlight.js/diff2html pipeline, with dark/light webview overrides so it
matches VS Code themes more closely.

VS Code webviews do not expose the exact TextMate token colors of every installed
editor theme. For that reason, syntax colors are theme-compatible but may not be
pixel-identical to a custom editor theme.

## Troubleshooting

If generated views open blank:

- run `OptiTrust: Health Check`,
- inspect the `OptiTrust` output panel,
- regenerate the diff or trace,
- open the generated `_diff.html` or `_trace.html` file directly if needed.

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

- The MVP supports in-tree OptiTrust development only.
- `.ml` files remain OCaml files; this extension does not replace OCaml-LSP.
- Diff and trace generation still depends on existing OptiTrust scripts.
- The exact syntax colors in webviews may differ from custom editor themes.
- OptiLambda parsing is not implemented in this extension pass.
- Fully-Typed output quality depends on type information available in the AST.
