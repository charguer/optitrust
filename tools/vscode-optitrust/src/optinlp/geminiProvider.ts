// Gemini implementation of the OptiNLP provider interface. This file owns the
// Gemini wire format and converts responses back into provider-neutral results.
import { OptiNlpProviderError, technicalDetailFrom } from "./providerErrors";
import { OptiNlpProvider, OptiNlpProviderRequest, OptiNlpProviderResult, requestWithMode } from "./providerTypes";
import { parseOptiNlpMarkdownResultSafely } from "./resultSchemas";

export const DEFAULT_GEMINI_MODEL = "gemini-3.5-flash";

export interface GeminiProviderOptions {
  readonly model?: string;
  readonly apiKey?: string;
  readonly apiKeyProvider?: () => Promise<string | undefined> | string | undefined;
  readonly endpointBase?: string;
  readonly fetchImpl?: typeof fetch;
}

interface GeminiTextPart {
  readonly text?: string;
}

interface GeminiResponse {
  readonly candidates?: readonly {
    readonly content?: {
      readonly parts?: readonly GeminiTextPart[];
    };
  }[];
  readonly error?: {
    readonly message?: string;
  };
}

export class GeminiProvider implements OptiNlpProvider {
  readonly name = "gemini";
  readonly model: string;
  private readonly apiKey?: string;
  private readonly apiKeyProvider?: GeminiProviderOptions["apiKeyProvider"];
  private readonly endpointBase: string;
  private readonly fetchImpl: typeof fetch;

  constructor(options: GeminiProviderOptions = {}) {
    this.model = options.model ?? DEFAULT_GEMINI_MODEL;
    this.apiKey = options.apiKey;
    this.apiKeyProvider = options.apiKeyProvider;
    this.endpointBase = options.endpointBase ?? "https://generativelanguage.googleapis.com/v1beta";
    this.fetchImpl = options.fetchImpl ?? fetch;
  }

  async generateTarget(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "target"));
  }

  async generateScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "command_to_script"));
  }

  async generateFullScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "code_to_full_script"));
  }

  buildPromptForTest(request: OptiNlpProviderRequest): string {
    return buildGeminiPrompt(request);
  }

  private async generate(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    const apiKey = await this.resolveApiKey();
    if (!apiKey) {
      throw new OptiNlpProviderError(this.name, "Set Gemini API key before using OptiNLP.", "Missing Gemini API key.");
    }

    const url = `${this.endpointBase}/models/${encodeURIComponent(this.model)}:generateContent?key=${encodeURIComponent(apiKey)}`;
    const body = {
      contents: [
        {
          role: "user",
          parts: [{ text: buildGeminiPrompt(request) }]
        }
      ],
      generationConfig: {
        temperature: 0.2
      }
    };

    let response: Response;
    try {
      response = await this.fetchImpl(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body)
      });
    } catch (error) {
      throw new OptiNlpProviderError(
        this.name,
        "Gemini request failed. Check your network connection and API key.",
        technicalDetailFrom(error),
        error
      );
    }

    let rawResponse: GeminiResponse;
    try {
      rawResponse = (await response.json()) as GeminiResponse;
    } catch (error) {
      throw new OptiNlpProviderError(this.name, "Gemini returned an unreadable response.", technicalDetailFrom(error), error);
    }

    if (!response.ok) {
      const detail = rawResponse.error?.message ?? `HTTP ${response.status}`;
      throw new OptiNlpProviderError(this.name, "Gemini request failed. Check your network connection and API key.", detail);
    }

    const markdownOutput = extractGeminiText(rawResponse);
    if (markdownOutput.trim().length === 0) {
      throw new OptiNlpProviderError(this.name, "Gemini returned an empty OptiNLP response.", "No candidate text parts found.");
    }

    const structured = parseOptiNlpMarkdownResultSafely(request.mode, markdownOutput);

    return {
      provider: this.name,
      model: this.model,
      markdownOutput,
      structured,
      rawResponse
    };
  }

  private async resolveApiKey(): Promise<string | undefined> {
    if (this.apiKey && this.apiKey.trim().length > 0) {
      return this.apiKey.trim();
    }
    const provided = await this.apiKeyProvider?.();
    if (provided && provided.trim().length > 0) {
      return provided.trim();
    }
    if (process.env.GEMINI_API_KEY && process.env.GEMINI_API_KEY.trim().length > 0) {
      return process.env.GEMINI_API_KEY.trim();
    }
    return undefined;
  }
}

function buildGeminiPrompt(request: OptiNlpProviderRequest): string {
  const session = request.sessionSummary?.trim();
  return [
    "# OptiNLP Task",
    `Mode: ${request.mode}`,
    "",
    "# System Prompt",
    request.promptText.trim(),
    "",
    "# Knowledge",
    request.knowledgeText.trim(),
    "",
    "# Context",
    `File: ${request.filePath}`,
    `Language: ${request.language}`,
    session ? `Session summary:\n${session}` : "Session summary: None.",
    "",
    "# Source",
    "```",
    request.sourceText,
    "```",
    "",
    "# User Request",
    request.userRequest,
    "",
    "# Output Contract",
    "Return only the markdown format required by the selected OptiNLP prompt.",
    "Include every required section from that prompt.",
    "Do not add provider notes, apologies, or extra sections."
  ].join("\n");
}

function extractGeminiText(response: GeminiResponse): string {
  return (
    response.candidates?.[0]?.content?.parts
      ?.map(part => part.text ?? "")
      .join("")
      .trim() ?? ""
  );
}
