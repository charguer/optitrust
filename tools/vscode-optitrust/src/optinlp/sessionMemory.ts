// Session-only memory for OptiNLP. It stores compact summaries of prior turns
// and validation status, never full source files or prompt/knowledge text.
import { OptiNlpProviderRequest, OptiNlpProviderResult } from "./providerTypes";
import { OptiNlpStructuredResult } from "./resultSchemas";

export interface OptiNlpValidationRecord {
  readonly command: string;
  readonly ok: boolean;
  readonly detail?: string;
}

export interface OptiNlpSessionTurn {
  readonly mode: OptiNlpProviderRequest["mode"];
  readonly userRequest: string;
  readonly filePath: string;
  readonly language: string;
  readonly provider: string;
  readonly model: string;
  readonly target?: string;
  readonly script?: string;
  readonly candidateScript?: string;
  readonly assumptions: readonly string[];
  readonly validation?: string;
}

export interface OptiNlpSessionSnapshot {
  readonly turns: readonly OptiNlpSessionTurn[];
  readonly acceptedAssumptions: readonly string[];
  readonly lastValidation?: OptiNlpValidationRecord;
}

export interface OptiNlpSessionMemoryOptions {
  readonly maxTurns?: number;
}

export class OptiNlpSessionMemory {
  private readonly maxTurns: number;
  private turns: OptiNlpSessionTurn[] = [];
  private acceptedAssumptions: string[] = [];
  private lastValidation: OptiNlpValidationRecord | undefined;

  constructor(options: OptiNlpSessionMemoryOptions = {}) {
    this.maxTurns = Math.max(1, options.maxTurns ?? 8);
  }

  recordGeneration(request: OptiNlpProviderRequest, result: OptiNlpProviderResult): void {
    const turn = turnFromResult(request, result);
    this.turns = [...this.turns, turn].slice(-this.maxTurns);
  }

  recordValidation(record: OptiNlpValidationRecord): void {
    this.lastValidation = record;
  }

  acceptAssumptions(assumptions: readonly string[]): void {
    const seen = new Set(this.acceptedAssumptions);
    for (const assumption of assumptions.map(value => value.trim()).filter(Boolean)) {
      if (!seen.has(assumption)) {
        seen.add(assumption);
        this.acceptedAssumptions.push(assumption);
      }
    }
  }

  snapshot(): OptiNlpSessionSnapshot {
    return {
      turns: [...this.turns],
      acceptedAssumptions: [...this.acceptedAssumptions],
      lastValidation: this.lastValidation
    };
  }

  clear(): void {
    this.turns = [];
    this.acceptedAssumptions = [];
    this.lastValidation = undefined;
  }

  summary(maxChars = 2000): string | undefined {
    const lines: string[] = [];
    const lastTurn = this.turns.at(-1);
    if (lastTurn) {
      lines.push(`Previous request: ${lastTurn.userRequest}`);
      lines.push(`Previous mode: ${lastTurn.mode}`);
      lines.push(`Previous file: ${lastTurn.filePath}`);
      if (lastTurn.target) {
        lines.push(`Previous target: ${lastTurn.target}`);
      }
      if (lastTurn.script) {
        lines.push(`Previous script: ${truncateOneLine(lastTurn.script, 500)}`);
      }
      if (lastTurn.candidateScript) {
        lines.push(`Previous candidate script: ${truncateOneLine(lastTurn.candidateScript, 500)}`);
      }
      if (lastTurn.assumptions.length > 0) {
        lines.push(`Previous assumptions: ${lastTurn.assumptions.join("; ")}`);
      }
      if (lastTurn.validation) {
        lines.push(`Previous validation suggestion: ${truncateOneLine(lastTurn.validation, 300)}`);
      }
    }

    if (this.acceptedAssumptions.length > 0) {
      lines.push(`Accepted assumptions: ${this.acceptedAssumptions.join("; ")}`);
    }

    if (this.lastValidation) {
      const status = this.lastValidation.ok ? "passed" : "failed";
      const detail = this.lastValidation.detail ? ` (${truncateOneLine(this.lastValidation.detail, 300)})` : "";
      lines.push(`Last validation: ${status}: ${this.lastValidation.command}${detail}`);
    }

    if (lines.length === 0) {
      return undefined;
    }
    return truncateMultiline(lines.join("\n"), maxChars);
  }
}

function turnFromResult(request: OptiNlpProviderRequest, result: OptiNlpProviderResult): OptiNlpSessionTurn {
  const structured = result.structured;
  return {
    mode: request.mode,
    userRequest: request.userRequest,
    filePath: request.filePath,
    language: request.language,
    provider: result.provider,
    model: result.model,
    target: targetFrom(structured),
    script: scriptFrom(structured),
    candidateScript: candidateScriptFrom(structured),
    assumptions: assumptionsFrom(structured),
    validation: validationFrom(structured)
  };
}

function targetFrom(result: OptiNlpStructuredResult | undefined): string | undefined {
  if (!result) {
    return undefined;
  }
  switch (result.kind) {
    case "target":
      return result.recommendedTarget;
    case "command_to_script":
      return result.target;
    case "code_to_candidate_script":
      return result.candidateTransformations[0]?.target;
  }
}

function scriptFrom(result: OptiNlpStructuredResult | undefined): string | undefined {
  return result?.kind === "command_to_script" ? result.generatedScript : undefined;
}

function candidateScriptFrom(result: OptiNlpStructuredResult | undefined): string | undefined {
  return result?.kind === "code_to_candidate_script" ? result.candidateScript : undefined;
}

function assumptionsFrom(result: OptiNlpStructuredResult | undefined): readonly string[] {
  return result?.kind === "command_to_script" ? result.assumptions : [];
}

function validationFrom(result: OptiNlpStructuredResult | undefined): string | undefined {
  if (!result) {
    return undefined;
  }
  return result.kind === "target" ? result.validation : result.validation;
}

function truncateOneLine(value: string, maxChars: number): string {
  return truncateMultiline(value.replace(/\s+/gu, " ").trim(), maxChars);
}

function truncateMultiline(value: string, maxChars: number): string {
  if (value.length <= maxChars) {
    return value;
  }
  return `${value.slice(0, Math.max(0, maxChars - 3))}...`;
}
