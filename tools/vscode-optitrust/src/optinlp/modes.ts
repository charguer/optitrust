// Central registry for OptiNLP workflows. Add or change workflows here first so
// prompt loading, CLI routing, UI labels, and auto-routing stay in sync.
import { OptiNlpMode } from "./providerTypes";

export type OptiNlpCliCommand = "target" | "script" | "full";
export type OptiNlpUiMode = OptiNlpMode | "auto";

export interface OptiNlpModeDefinition {
  readonly id: OptiNlpMode;
  readonly cliCommand: OptiNlpCliCommand;
  readonly label: string;
  readonly shortLabel: string;
  readonly placeholder: string;
  readonly promptFile: string;
  readonly knowledgeFiles: readonly string[];
}

export const OPTINLP_MODE_DEFINITIONS: readonly OptiNlpModeDefinition[] = [
  {
    id: "target",
    cliCommand: "target",
    label: "Generate Target",
    shortLabel: "Target",
    placeholder: "target the second loop named i",
    promptFile: "01_target_generator.md",
    knowledgeFiles: ["target_description.md", "targets.md"]
  },
  {
    id: "command_to_script",
    cliCommand: "script",
    label: "Generate Script",
    shortLabel: "Script",
    placeholder: "unroll the loop i",
    promptFile: "02_command_to_script.md",
    knowledgeFiles: ["target_description.md", "targets.md", "script_patterns.md", "transformations.md", "optilambda.md"]
  },
  {
    id: "code_to_full_script",
    cliCommand: "full",
    label: "Generate Full Transformation",
    shortLabel: "Full Transformation",
    placeholder: "generate a complete transformation script for the whole file",
    promptFile: "03_code_to_full_script.md",
    knowledgeFiles: ["target_description.md", "targets.md", "script_patterns.md", "transformations.md", "optilambda.md"]
  }
] as const;

export const OPTINLP_MODE_BY_ID = new Map(OPTINLP_MODE_DEFINITIONS.map(definition => [definition.id, definition]));
export const OPTINLP_MODE_BY_CLI_COMMAND = new Map(OPTINLP_MODE_DEFINITIONS.map(definition => [definition.cliCommand, definition]));

export function modeDefinition(mode: OptiNlpMode): OptiNlpModeDefinition {
  const definition = OPTINLP_MODE_BY_ID.get(mode);
  if (!definition) {
    throw new Error(`Unknown OptiNLP mode '${mode}'.`);
  }
  return definition;
}

export function modeFromCliCommand(command: string): OptiNlpMode | undefined {
  return OPTINLP_MODE_BY_CLI_COMMAND.get(command as OptiNlpCliCommand)?.id;
}

export function isOptiNlpCliCommand(command: string): command is OptiNlpCliCommand {
  return OPTINLP_MODE_BY_CLI_COMMAND.has(command as OptiNlpCliCommand);
}

export function resolveAutoMode(mode: OptiNlpUiMode, request: string): OptiNlpMode {
  return resolveRequestedMode(mode, request);
}

export function resolveRequestedMode(mode: OptiNlpUiMode, request: string): OptiNlpMode {
  if (mode === "target") {
    return "target";
  }
  if (isFullFileScriptRequest(request)) {
    return "code_to_full_script";
  }
  if (mode !== "auto") {
    return mode;
  }
  const text = request.toLowerCase();
  if (/\b(target|position|before|after|loop|call|variable)\b/u.test(text) && !/\b(unroll|inline|insert|tile|fuse|split|parallel|transform)\b/u.test(text)) {
    return "target";
  }
  if (/\b(suggest|candidate|optimi[sz]e|opportunity|what can)\b/u.test(text)) {
    return "code_to_full_script";
  }
  return "command_to_script";
}

function isFullFileScriptRequest(request: string): boolean {
  const text = request.toLowerCase();
  const wantsScript = /\b(generate|create|write|make|produce|build)\b[\s\S]*\b(script|transformation|transformations|optimi[sz]ation)\b/u.test(text);
  const wantsFullScope = /\b(whole|entire|full|complete)\b[\s\S]*\b(file|source|code|script|transformation|transformations)\b/u.test(text);
  const namesMlScript = /\b[a-z0-9_-]+\.ml\b/u.test(text);
  return (wantsScript && wantsFullScope) || namesMlScript;
}
