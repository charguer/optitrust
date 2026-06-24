// Central registry for OptiNLP workflows. Add or change workflows here first so
// prompt loading, CLI routing, UI labels, and auto-routing stay in sync.
import { OptiNlpMode } from "./providerTypes";

export type OptiNlpCliCommand = "target" | "script" | "candidates";
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
    knowledgeFiles: ["targets.md"]
  },
  {
    id: "command_to_script",
    cliCommand: "script",
    label: "Generate Script",
    shortLabel: "Script",
    placeholder: "unroll the loop i",
    promptFile: "02_command_to_script.md",
    knowledgeFiles: ["targets.md", "script_patterns.md", "transformations.md", "optilambda.md"]
  },
  {
    id: "code_to_candidate_script",
    cliCommand: "candidates",
    label: "Suggest Candidate Script",
    shortLabel: "Candidates",
    placeholder: "suggest a first transformation",
    promptFile: "03_code_to_candidate_script.md",
    knowledgeFiles: ["targets.md", "script_patterns.md", "transformations.md", "optilambda.md"]
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
  if (mode !== "auto") {
    return mode;
  }
  const text = request.toLowerCase();
  if (/\b(target|position|before|after|loop|call|variable)\b/u.test(text) && !/\b(unroll|inline|insert|tile|fuse|split|parallel|transform)\b/u.test(text)) {
    return "target";
  }
  if (/\b(suggest|candidate|optimi[sz]e|opportunity|what can)\b/u.test(text)) {
    return "code_to_candidate_script";
  }
  return "command_to_script";
}
