# OptiNLP

OptiNLP helps an AI assistant generate OptiTrust targets and transformation
scripts from natural-language requests. It combines prompt assets in this
directory with the VS Code extension and a small CLI.

OptiNLP currently supports three workflows:

1. generate an OptiTrust target;
2. generate a transformation script from an explicit command;
3. generate a full transformation script from the active source file.

Private internship notes under `practice/` may be used as background while
designing prompts, but generated OptiNLP artifacts must not be written there or
copy private text from there.

## Directory Guide

```text
tools/optiNLP/
  README.md
  prompts/
    01_target_generator.md
    02_command_to_script.md
    03_code_to_full_script.md
  knowledge/
    target_description.md
    targets.md
    script_patterns.md
    transformations.md
    optilambda.md
  eval/
    target_cases.md
    command_to_script_cases.md
    code_to_script_cases.md
    target_prompt_smoke.md
```

- `prompts/`: mode-specific instructions sent to the AI provider.
- `knowledge/`: stable OptiTrust context loaded with the prompts.
- `eval/`: manual test cases for checking whether prompt outputs are good.

The VS Code and CLI implementation lives under:

```text
tools/vscode-optitrust/src/optinlp/
tools/vscode-optitrust/src/commands/optinlp*.ts
```

## Setup

OptiNLP is part of the OptiTrust VS Code extension. For the general extension
requirements, `.vsix` packaging, installation commands, and workspace detection
rules, read:

```text
tools/vscode-optitrust/README.md
```

After the extension setup is complete, compile it from the extension directory:

```bash
cd tools/vscode-optitrust
npm run compile
```

For local OptiNLP or extension development:

```bash
npm run dev:extension
```

The CLI uses the compiled extension output, so run `npm run compile` again after
changing TypeScript files.

## Provider Setup

OptiNLP supports these providers:

- `gemini`, the default provider;
- `openai`;
- `mock`, for deterministic local testing without an API key.

In VS Code, use the command palette:

```text
OptiTrust: OptiNLP Select Provider
OptiTrust: OptiNLP Set Model
OptiTrust: OptiNLP Set API Key
```

Provider settings are:

```json
"optitrust.optinlpProvider": "gemini",
"optitrust.optinlpModel": "",
"optitrust.optinlpUseProviderSession": true
```

For CLI usage, API keys are read from the environment:

```bash
export GEMINI_API_KEY=...
export OPENAI_API_KEY=...
```

To test without a remote provider:

```bash
export OPTINLP_PROVIDER=mock
```

## VS Code Usage

Open the OptiTrust repository in VS Code, then open a C/C++ source file or an
OptiTrust transformation script.

Main commands:

```text
OptiTrust: Open OptiNLP Chat
OptiTrust: OptiNLP Generate Target
OptiTrust: OptiNLP Generate Script
OptiTrust: OptiNLP Generate Full Transformation
OptiTrust: OptiNLP Suggest Target At Cursor
OptiTrust: OptiNLP Clear Session
```

Native chat participant:

```text
@optinlp /target target the second loop named i
@optinlp /script unroll the loop i
@optinlp /full generate a complete transformation script for this file
@optinlp /config
@optinlp /clear
@optinlp /help
```

When possible, OptiNLP uses the active editor and associated source files as
context. Generated target results can be inserted into the editor. Generated
scripts can be opened as new editor documents.

## CLI Usage

Compile the extension first:

```bash
cd tools/vscode-optitrust
npm run compile
```

Then run:

```bash
npm run optinlp -- target --file ../../tests/loop/unroll/loop_unroll.cpp --request "target the loop i"
npm run optinlp -- script --file ../../tests/loop/unroll/loop_unroll.cpp --request "unroll the loop i"
npm run optinlp -- full --file ../../tests/loop/unroll/loop_unroll.cpp --request "generate a complete transformation script for this file"
```

Useful options:

```text
--json
--provider gemini|mock|openai
--model MODEL_NAME
--root /path/to/optitrust
--session-summary "..."
```

Example with the mock provider:

```bash
npm run optinlp -- target \
  --provider mock \
  --file ../../tests/loop/unroll/loop_unroll.cpp \
  --request "target the loop i"
```

## Modes

### Target Generation

Input:

```text
target the second loop named i
```

Expected kind of output:

```ocaml
[occIndex 1; cFor "i"]
```

Prompt:

```text
prompts/01_target_generator.md
```

### Command To Script

Input:

```text
unroll the loop i
```

Expected kind of output:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unroll [cFor "i"];
)
```

Prompt:

```text
prompts/02_command_to_script.md
```

### Full Script Generation

Input:

```text
generate a complete transformation script for this file
```

Expected output:

- code summary;
- candidate transformations;
- full generated script;
- assumptions;
- validation steps.

Prompt:

```text
prompts/03_code_to_full_script.md
```

## Testing

Run OptiNLP TypeScript tests:

```bash
cd tools/vscode-optitrust
npm run test:optinlp
```

Check prompt-kit formatting:

```bash
git diff --check -- tools/optiNLP
```

Manual prompt evaluation:

1. Choose a prompt from `prompts/`.
2. Include the relevant files from `knowledge/`.
3. Run one case from `eval/`.
4. Compare the AI output with the expected target, script, or candidate result.

The evaluation files are intentionally small and readable so prompt failures can
be diagnosed by hand.

## Updating Prompts

When changing a prompt:

1. update the matching file in `prompts/`;
2. update or add examples in `eval/`;
3. update `knowledge/` if the prompt depends on new OptiTrust APIs;
4. run `git diff --check -- tools/optiNLP`;
5. run `npm run test:optinlp` from `tools/vscode-optitrust` if the output schema
   or prompt sections changed.

Keep prompt section names stable when possible. The VS Code integration parses
known Markdown sections to offer editor actions such as inserting targets or
opening generated scripts.

## Current Limitations

- OptiNLP is only as reliable as its prompts, knowledge files, and provider
  output.
- Generated scripts still need normal OptiTrust validation.
- OptiLambda is currently a printer-oriented representation; do not generate
  `Run.script_opti` unless parser support is added later.
- The `ollama` provider is listed as a possible provider id in code but is not
  implemented yet.
