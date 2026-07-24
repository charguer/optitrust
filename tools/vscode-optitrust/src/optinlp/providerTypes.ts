// Provider-neutral OptiNLP request/result contracts. CLI, VS Code commands,
// UI commands, native chat, and provider implementations communicate through these types.
import type { OptiNlpStructuredResult } from "./resultSchemas";

export type OptiNlpMode = "target" | "command_to_script" | "code_to_full_script";

export interface OptiNlpProviderRequest {
  readonly mode: OptiNlpMode;
  readonly userRequest: string;
  readonly sourceText: string;
  readonly filePath: string;
  readonly language: string;
  readonly promptText: string;
  readonly knowledgeText: string;
  readonly stableContextKey?: string;
  readonly stableContextLabel?: string;
  readonly stableContextOmitted?: boolean;
  readonly stableSourceContextKey?: string;
  readonly stableSourceContextLabel?: string;
  readonly stableSourceContextOmitted?: boolean;
  readonly providerSessionEnabled?: boolean;
  readonly previousProviderResponseId?: string;
  readonly sessionSummary?: string;
  readonly abortSignal?: AbortSignal;
}

export interface OptiNlpProviderResult {
  readonly provider: string;
  readonly model: string;
  readonly markdownOutput: string;
  readonly providerResponseId?: string;
  // Best-effort parsed fields for editor actions; raw markdown is still valid
  // output when a provider does not follow the exact section schema.
  readonly structured?: OptiNlpStructuredResult;
  readonly rawResponse?: unknown;
}

export interface OptiNlpProvider {
  readonly name: string;
  readonly model: string;
  readonly supportsProviderSession?: boolean;

  generateTarget(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult>;
  generateScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult>;
  generateFullScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult>;
}

export function requestWithMode(request: OptiNlpProviderRequest, mode: OptiNlpMode): OptiNlpProviderRequest {
  return { ...request, mode };
}
