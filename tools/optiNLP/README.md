# OptiNLP Prompt Kit

OptiNLP is a prompt-engineering layer for helping an AI assistant produce
OptiTrust transformation scripts. It is not an NLP model, parser, or runtime
integration yet. The first goal is to build reliable prompts that understand
OptiTrust targets, transformation commands, and the script style used in the
repository.

This directory is the tool-facing home for OptiNLP prompt assets and possible
future integrations. Private internship notes and reports under `practice/` may
be used as background while designing the prompts, but generated OptiNLP
artifacts must not be written there or copy private text from there.

## Milestones

1. Target generation: convert natural-language references to program locations
   into valid OptiTrust target syntax.
2. Command-to-script generation: convert explicit user commands into OptiTrust
   scripts using known transformation APIs.
3. Code-to-candidate-script generation: inspect input C/C++ or OptiLambda text
   and propose candidate transformations with assumptions and validation steps.

## Current Scope

Implemented in this first pass:

- knowledge notes for target syntax, script patterns, transformations, and
  OptiLambda context;
- three engineered prompts:
  - `prompts/01_target_generator.md`;
  - `prompts/02_command_to_script.md`;
  - `prompts/03_code_to_full_script.md`;
- separate manual evaluation cases for each prompt.

Not implemented yet:

- a CLI;
- VS Code UI integration;
- calls to an AI provider;
- automatic script validation;
- `.opti` parsing. OptiLambda is currently a printer-oriented textual view;
  prompt outputs must not assume that `Run.script_opti` exists.

## How To Use

Give an AI assistant:

1. the relevant prompt from `prompts/`;
2. the knowledge files in `knowledge/`;
3. the user request;
4. the source C/C++ code or printed OptiLambda text;
5. any existing script, trace, diff, or error output if available.

The assistant should return structured reasoning, valid target or script syntax,
and validation steps. When it cannot disambiguate a target, it should ask a
focused clarification instead of guessing.

## Prompt And Evaluation Split

Prompts define assistant behavior. Evaluation files test whether that behavior
works on concrete cases.

```text
prompts/01_target_generator.md          target request -> target syntax
prompts/02_command_to_script.md         user command -> transformation script
prompts/03_code_to_full_script.md       input code -> full transformation script

eval/target_cases.md
eval/command_to_script_cases.md
eval/code_to_script_cases.md
```
