// Structured OptiNLP result types and markdown parsers for the three prompt
// outputs. This is the bridge between prompt-facing markdown and UI actions.
import type { OptiNlpMode } from "./providerTypes";

export interface TargetResult {
  readonly kind: "target";
  readonly intent: string;
  readonly candidateNodes: readonly string[];
  readonly recommendedTarget?: string;
  readonly whyThisTarget?: string;
  readonly ambiguities?: string;
  readonly alternatives: readonly string[];
  readonly validation?: string;
  readonly missingInformation?: string;
}

export interface ScriptResult {
  readonly kind: "command_to_script";
  readonly intent: string;
  readonly transformationApi: string;
  readonly target: string;
  readonly generatedScript: string;
  readonly assumptions: readonly string[];
  readonly validation: string;
}

export interface CandidateTransformation {
  readonly rank: string;
  readonly transformation: string;
  readonly target: string;
  readonly whyItMayApply: string;
  readonly risk: string;
}

export interface FullScriptResult {
  readonly kind: "code_to_full_script";
  readonly codeSummary: string;
  readonly candidateTransformations: readonly CandidateTransformation[];
  readonly recommendedFirstCandidate: string;
  readonly fullScript: string;
  readonly validation: string;
  readonly missingInformation?: string;
}

export type OptiNlpStructuredResult = TargetResult | ScriptResult | FullScriptResult;

export const targetResultSchema = {
  type: "object",
  required: ["kind", "intent", "candidateNodes", "alternatives"],
  properties: {
    kind: { const: "target" },
    intent: { type: "string" },
    candidateNodes: { type: "array", items: { type: "string" } },
    recommendedTarget: { type: "string" },
    whyThisTarget: { type: "string" },
    ambiguities: { type: "string" },
    alternatives: { type: "array", items: { type: "string" } },
    validation: { type: "string" },
    missingInformation: { type: "string" }
  }
} as const;

export const scriptResultSchema = {
  type: "object",
  required: ["kind", "intent", "transformationApi", "target", "generatedScript", "assumptions", "validation"],
  properties: {
    kind: { const: "command_to_script" },
    intent: { type: "string" },
    transformationApi: { type: "string" },
    target: { type: "string" },
    generatedScript: { type: "string" },
    assumptions: { type: "array", items: { type: "string" } },
    validation: { type: "string" }
  }
} as const;

export const fullScriptResultSchema = {
  type: "object",
  required: ["kind", "codeSummary", "candidateTransformations", "recommendedFirstCandidate", "fullScript", "validation"],
  properties: {
    kind: { const: "code_to_full_script" },
    codeSummary: { type: "string" },
    candidateTransformations: {
      type: "array",
      items: {
        type: "object",
        required: ["rank", "transformation", "target", "whyItMayApply", "risk"],
        properties: {
          rank: { type: "string" },
          transformation: { type: "string" },
          target: { type: "string" },
          whyItMayApply: { type: "string" },
          risk: { type: "string" }
        }
      }
    },
    recommendedFirstCandidate: { type: "string" },
    fullScript: { type: "string" },
    validation: { type: "string" },
    missingInformation: { type: "string" }
  }
} as const;

export class OptiNlpSchemaError extends Error {
  readonly mode: OptiNlpMode;

  constructor(mode: OptiNlpMode, message: string) {
    super(message);
    this.name = "OptiNlpSchemaError";
    this.mode = mode;
  }
}

export function parseOptiNlpMarkdownResult(mode: OptiNlpMode, markdown: string): OptiNlpStructuredResult {
  switch (mode) {
    case "target":
      return parseTargetResult(markdown);
    case "command_to_script":
      return parseScriptResult(markdown);
    case "code_to_full_script":
      return parseFullScriptResult(markdown);
  }
}

export function parseOptiNlpMarkdownResultSafely(mode: OptiNlpMode, markdown: string): OptiNlpStructuredResult | undefined {
  try {
    return parseOptiNlpMarkdownResult(mode, markdown);
  } catch (error) {
    if (error instanceof OptiNlpSchemaError) {
      return undefined;
    }
    throw error;
  }
}

function parseTargetResult(markdown: string): TargetResult {
  const missingInformation = section(markdown, "Missing Information");
  if (missingInformation) {
    return {
      kind: "target",
      intent: "",
      candidateNodes: [],
      alternatives: [],
      missingInformation
    };
  }

  const intent = requiredSection(markdown, "Intent", "target");
  const recommendedTarget = firstCodeBlock(requiredSection(markdown, "Recommended Target", "target"));
  const candidateNodes = bulletLines(section(markdown, "Candidate Nodes") ?? "");
  const alternatives = codeBlocks(section(markdown, "Alternatives") ?? "");

  if (!recommendedTarget) {
    throw new OptiNlpSchemaError("target", "Target result is missing a Recommended Target code block.");
  }

  return {
    kind: "target",
    intent,
    candidateNodes,
    recommendedTarget,
    whyThisTarget: section(markdown, "Why This Target"),
    ambiguities: section(markdown, "Ambiguities"),
    alternatives,
    validation: firstCodeBlock(section(markdown, "Validation") ?? "") ?? section(markdown, "Validation")
  };
}

function parseScriptResult(markdown: string): ScriptResult {
  return {
    kind: "command_to_script",
    intent: requiredSection(markdown, "Intent", "command_to_script"),
    transformationApi: requiredSection(markdown, "Transformation API", "command_to_script"),
    target: requiredCodeBlock(markdown, "Target", "command_to_script"),
    generatedScript: requiredCodeBlock(markdown, "Generated Script", "command_to_script"),
    assumptions: linesOrNone(requiredSection(markdown, "Assumptions", "command_to_script")),
    validation: requiredCodeBlock(markdown, "Validation", "command_to_script")
  };
}

function parseFullScriptResult(markdown: string): FullScriptResult {
  return {
    kind: "code_to_full_script",
    codeSummary: requiredSection(markdown, "Code Summary", "code_to_full_script"),
    candidateTransformations: parseCandidateTable(requiredSection(markdown, "Candidate Transformations", "code_to_full_script")),
    recommendedFirstCandidate: requiredSection(markdown, "Recommended First Candidate", "code_to_full_script"),
    fullScript: requiredCodeBlock(markdown, "Full Transformation Script", "code_to_full_script"),
    validation: requiredCodeBlock(markdown, "Validation", "code_to_full_script"),
    missingInformation: section(markdown, "Missing Information")
  };
}

function section(markdown: string, title: string): string | undefined {
  const escaped = title.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const regex = new RegExp(`^##\\s+${escaped}\\s*$([\\s\\S]*?)(?=^##\\s+|(?![\\s\\S]))`, "im");
  const match = regex.exec(markdown);
  const value = match?.[1]?.trim();
  return value && value.length > 0 ? value : undefined;
}

function requiredSection(markdown: string, title: string, mode: OptiNlpMode): string {
  const value = section(markdown, title);
  if (!value) {
    throw new OptiNlpSchemaError(mode, `${mode} result is missing required section '${title}'.`);
  }
  return value;
}

function codeBlocks(markdown: string): string[] {
  const blocks: string[] = [];
  const regex = /```[A-Za-z0-9_-]*\n([\s\S]*?)```/g;
  let match: RegExpExecArray | null;
  while ((match = regex.exec(markdown)) !== null) {
    blocks.push(match[1].trim());
  }
  return blocks;
}

function firstCodeBlock(markdown: string): string | undefined {
  return codeBlocks(markdown)[0];
}

function requiredCodeBlock(markdown: string, title: string, mode: OptiNlpMode): string {
  const block = firstCodeBlock(requiredSection(markdown, title, mode));
  if (!block) {
    throw new OptiNlpSchemaError(mode, `${mode} result section '${title}' is missing a code block.`);
  }
  return block;
}

function bulletLines(markdown: string): string[] {
  return markdown
    .split(/\r?\n/u)
    .map(line => line.trim())
    .filter(line => /^[-*]\s+/u.test(line))
    .map(line => line.replace(/^[-*]\s+/u, "").trim());
}

function linesOrNone(markdown: string): string[] {
  const bullets = bulletLines(markdown);
  if (bullets.length > 0) {
    return bullets;
  }
  const trimmed = markdown.trim();
  return trimmed.length > 0 && trimmed !== "None." ? [trimmed] : [];
}

function parseCandidateTable(markdown: string): CandidateTransformation[] {
  return markdown
    .split(/\r?\n/u)
    .map(line => line.trim())
    .filter(line => line.startsWith("|") && !/^\|\s*-+/u.test(line))
    .slice(1)
    .map(line => line.split("|").slice(1, -1).map(cell => cell.trim()))
    .filter(cells => cells.length >= 5)
    .map(cells => ({
      rank: cells[0],
      transformation: cells[1],
      target: cells[2],
      whyItMayApply: cells[3],
      risk: cells[4]
    }));
}
