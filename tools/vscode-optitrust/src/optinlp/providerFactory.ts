// Factory for OptiNLP model providers. New providers should be registered here
// without leaking provider-specific options into the rest of the OptiNLP code.
import { GeminiProvider, GeminiProviderOptions } from "./geminiProvider";
import { MockProvider, MockProviderOptions } from "./mockProvider";
import { OpenAiProvider, OpenAiProviderOptions } from "./openaiProvider";
import { OptiNlpProvider } from "./providerTypes";

export type OptiNlpProviderId = "gemini" | "mock" | "openai" | "ollama";
export const DEFAULT_OPTINLP_PROVIDER: OptiNlpProviderId = "gemini";
export const OPTINLP_PROVIDER_IDS: readonly OptiNlpProviderId[] = ["gemini", "mock", "openai", "ollama"];
export const IMPLEMENTED_OPTINLP_PROVIDER_IDS: readonly OptiNlpProviderId[] = ["gemini", "mock", "openai"];

export interface OptiNlpProviderFactoryOptions {
  readonly provider?: OptiNlpProviderId;
  readonly gemini?: GeminiProviderOptions;
  readonly mock?: MockProviderOptions;
  readonly openai?: OpenAiProviderOptions;
}

export function createOptiNlpProvider(options: OptiNlpProviderFactoryOptions = {}): OptiNlpProvider {
  const provider = options.provider ?? DEFAULT_OPTINLP_PROVIDER;
  switch (provider) {
    case "gemini":
      return new GeminiProvider(options.gemini);
    case "mock":
      return new MockProvider(options.mock);
    case "openai":
      return new OpenAiProvider(options.openai);
    case "ollama":
      throw new Error(`OptiNLP provider '${provider}' is not implemented yet.`);
  }
}

export function parseOptiNlpProviderId(value: string): OptiNlpProviderId | undefined {
  return OPTINLP_PROVIDER_IDS.includes(value as OptiNlpProviderId) ? (value as OptiNlpProviderId) : undefined;
}
