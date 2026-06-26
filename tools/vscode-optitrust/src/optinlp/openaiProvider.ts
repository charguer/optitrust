// OpenAI implementation of the OptiNLP provider interface. This file owns the
// Responses API wire format and keeps OpenAI-specific fields out of callers.
import { OptiNlpProviderError, technicalDetailFrom } from "./providerErrors";
import { OptiNlpProvider, OptiNlpProviderRequest, OptiNlpProviderResult, requestWithMode } from "./providerTypes";
import { parseOptiNlpMarkdownResultSafely } from "./resultSchemas";

export const DEFAULT_OPENAI_MODEL = "gpt-5.5";

export interface OpenAiProviderOptions {
  readonly model?: string;
  readonly apiKey?: string;
  readonly apiKeyProvider?: () => Promise<string | undefined> | string | undefined;
  readonly endpoint?: string;
  readonly fetchImpl?: typeof fetch;
}

interface OpenAiTextPart {
  readonly type?: string;
  readonly text?: string;
}

interface OpenAiOutputItem {
  readonly type?: string;
  readonly content?: readonly OpenAiTextPart[];
}

interface OpenAiResponse {
  readonly output?: readonly OpenAiOutputItem[];
  readonly output_text?: string;
  readonly error?: {
    readonly message?: string;
  };
}

export class OpenAiProvider implements OptiNlpProvider {
  readonly name = "openai";
  readonly model: string;
  private readonly apiKey?: string;
  private readonly apiKeyProvider?: OpenAiProviderOptions["apiKeyProvider"];
  private readonly endpoint: string;
  private readonly fetchImpl: typeof fetch;

  constructor(options: OpenAiProviderOptions = {}) {
    this.model = options.model ?? DEFAULT_OPENAI_MODEL;
    this.apiKey = options.apiKey;
    this.apiKeyProvider = options.apiKeyProvider;
    this.endpoint = options.endpoint ?? "https://api.openai.com/v1/responses";
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
    return buildOpenAiPrompt(request);
  }

  private async generate(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    const apiKey = await this.resolveApiKey();
    if (!apiKey) {
      throw new OptiNlpProviderError(this.name, "Set OpenAI API key before using OptiNLP.", "Missing OpenAI API key.");
    }

    const body = {
      model: this.model,
      instructions: [
        "You are an OptiNLP provider.",
        "Return only the markdown format required by the selected OptiNLP prompt.",
        "Include every required section from that prompt.",
        "Do not add provider notes, apologies, or extra sections."
      ].join("\n"),
      input: buildOpenAiPrompt(request),
      store: false
    };

    let response: Response;
    try {
      response = await this.fetchImpl(this.endpoint, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${apiKey}`
        },
        body: JSON.stringify(body)
      });
    } catch (error) {
      throw new OptiNlpProviderError(this.name, "OpenAI request failed. Check your network connection and API key.", technicalDetailFrom(error), error);
    }

    let rawResponse: OpenAiResponse;
    try {
      rawResponse = (await response.json()) as OpenAiResponse;
    } catch (error) {
      throw new OptiNlpProviderError(this.name, "OpenAI returned an unreadable response.", technicalDetailFrom(error), error);
    }

    if (!response.ok) {
      const detail = rawResponse.error?.message ?? `HTTP ${response.status}`;
      throw new OptiNlpProviderError(this.name, "OpenAI request failed. Check your network connection and API key.", detail);
    }

    const markdownOutput = extractOpenAiText(rawResponse);
    if (markdownOutput.trim().length === 0) {
      throw new OptiNlpProviderError(this.name, "OpenAI returned an empty OptiNLP response.", "No output text found.");
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
    if (process.env.OPENAI_API_KEY && process.env.OPENAI_API_KEY.trim().length > 0) {
      return process.env.OPENAI_API_KEY.trim();
    }
    return undefined;
  }
}

function buildOpenAiPrompt(request: OptiNlpProviderRequest): string {
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
    request.userRequest
  ].join("\n");
}

function extractOpenAiText(response: OpenAiResponse): string {
  if (response.output_text && response.output_text.trim().length > 0) {
    return response.output_text.trim();
  }

  return (
    response.output
      ?.flatMap(item => item.content ?? [])
      .filter(part => part.type === "output_text" || part.text !== undefined)
      .map(part => part.text ?? "")
      .join("")
      .trim() ?? ""
  );
}
