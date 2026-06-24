import * as assert from "assert";
import { GeminiProvider } from "./geminiProvider";
import { MockProvider } from "./mockProvider";
import { OpenAiProvider } from "./openaiProvider";
import { OptiNlpProviderError } from "./providerErrors";
import { createOptiNlpProvider } from "./providerFactory";
import { OptiNlpProviderRequest } from "./providerTypes";
import { parseOptiNlpMarkdownResult } from "./resultSchemas";

const sampleRequest: OptiNlpProviderRequest = {
  mode: "target",
  userRequest: "target the loop i",
  sourceText: "void f(int n) { for (int i = 0; i < n; i++) work(i); }",
  filePath: "tests/demo.cpp",
  language: "cpp",
  promptText: "# Prompt\nReturn target output.",
  knowledgeText: "# Knowledge\nUse cFor for loops.",
  sessionSummary: "Previous target was [cFor \"j\"]."
};

async function testProviderFactory(): Promise<void> {
  assert.strictEqual(createOptiNlpProvider().name, "gemini");
  assert.strictEqual(createOptiNlpProvider({ provider: "mock" }).name, "mock");
  assert.strictEqual(createOptiNlpProvider({ provider: "openai" }).name, "openai");
}

async function testMockProvider(): Promise<void> {
  const provider = new MockProvider();
  const result = await provider.generateTarget(sampleRequest);

  assert.strictEqual(result.provider, "mock");
  assert.strictEqual(result.model, "mock-model");
  assert.strictEqual(result.structured?.kind, "target");
  assert.strictEqual(result.structured?.recommendedTarget, "[cFor \"i\"]");

  const scriptResult = await provider.generateScript(sampleRequest);
  assert.strictEqual(scriptResult.structured?.kind, "command_to_script");

  const candidateResult = await provider.generateCandidateScript(sampleRequest);
  assert.strictEqual(candidateResult.structured?.kind, "code_to_candidate_script");
}

async function testGeminiPromptConstruction(): Promise<void> {
  const provider = new GeminiProvider({ apiKey: "test-key", model: "test-model" });
  const prompt = provider.buildPromptForTest(sampleRequest);

  assert.match(prompt, /Mode: target/);
  assert.match(prompt, /# System Prompt/);
  assert.match(prompt, /# Knowledge/);
  assert.match(prompt, /File: tests\/demo.cpp/);
  assert.match(prompt, /target the loop i/);
  assert.match(prompt, /Return only the markdown format required/);
  assert.match(prompt, /Include every required section/);
}

async function testGeminiMissingApiKey(): Promise<void> {
  const previous = process.env.GEMINI_API_KEY;
  delete process.env.GEMINI_API_KEY;

  try {
    const provider = new GeminiProvider({ apiKeyProvider: () => undefined });
    await assert.rejects(
      () => provider.generateTarget(sampleRequest),
      (error: unknown) =>
        error instanceof OptiNlpProviderError &&
        error.provider === "gemini" &&
        error.userMessage === "Set Gemini API key before using OptiNLP."
    );
  } finally {
    if (previous !== undefined) {
      process.env.GEMINI_API_KEY = previous;
    }
  }
}

async function testGeminiEmptyResponse(): Promise<void> {
  const fetchImpl: typeof fetch = async () =>
    new Response(JSON.stringify({ candidates: [{ content: { parts: [{ text: "   " }] } }] }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });

  const provider = new GeminiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateTarget(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError &&
      error.provider === "gemini" &&
      error.userMessage === "Gemini returned an empty OptiNLP response."
  );
}

async function testGeminiProviderException(): Promise<void> {
  const fetchImpl: typeof fetch = async () => {
    throw new Error("network unavailable");
  };

  const provider = new GeminiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateScript(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError &&
      error.provider === "gemini" &&
      error.userMessage === "Gemini request failed. Check your network connection and API key." &&
      error.technicalDetail === "network unavailable"
  );
}

async function testGeminiSuccessfulResponse(): Promise<void> {
  const fetchImpl: typeof fetch = async () =>
    new Response(JSON.stringify({ candidates: [{ content: { parts: [{ text: validCandidateMarkdown }] } }] }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });

  const provider = new GeminiProvider({ apiKey: "test-key", model: "gemini-test", fetchImpl });
  const result = await provider.generateCandidateScript(sampleRequest);

  assert.strictEqual(result.provider, "gemini");
  assert.strictEqual(result.model, "gemini-test");
  assert.strictEqual(result.markdownOutput, validCandidateMarkdown);
  assert.strictEqual(result.structured?.kind, "code_to_candidate_script");
  assert.ok(result.rawResponse);
}

async function testGeminiInvalidStructuredResponse(): Promise<void> {
  const fetchImpl: typeof fetch = async () =>
    new Response(JSON.stringify({ candidates: [{ content: { parts: [{ text: "## Intent\nGenerated." }] } }] }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });

  const provider = new GeminiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateTarget(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError &&
      error.provider === "gemini" &&
      error.userMessage === "Gemini returned an invalid OptiNLP response." &&
      /Recommended Target|Candidate Nodes|Ambiguities|Intent/u.test(error.technicalDetail ?? "")
  );
}

async function testOpenAiPromptConstruction(): Promise<void> {
  const provider = new OpenAiProvider({ apiKey: "test-key", model: "test-model" });
  const prompt = provider.buildPromptForTest(sampleRequest);

  assert.match(prompt, /Mode: target/);
  assert.match(prompt, /# System Prompt/);
  assert.match(prompt, /# Knowledge/);
  assert.match(prompt, /File: tests\/demo.cpp/);
  assert.match(prompt, /target the loop i/);
}

async function testOpenAiMissingApiKey(): Promise<void> {
  const previous = process.env.OPENAI_API_KEY;
  delete process.env.OPENAI_API_KEY;

  try {
    const provider = new OpenAiProvider({ apiKeyProvider: () => undefined });
    await assert.rejects(
      () => provider.generateTarget(sampleRequest),
      (error: unknown) =>
        error instanceof OptiNlpProviderError &&
        error.provider === "openai" &&
        error.userMessage === "Set OpenAI API key before using OptiNLP."
    );
  } finally {
    if (previous !== undefined) {
      process.env.OPENAI_API_KEY = previous;
    }
  }
}

async function testOpenAiEmptyResponse(): Promise<void> {
  const fetchImpl: typeof fetch = async () =>
    new Response(JSON.stringify({ output: [{ content: [{ type: "output_text", text: "   " }] }] }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });

  const provider = new OpenAiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateTarget(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError && error.provider === "openai" && error.userMessage === "OpenAI returned an empty OptiNLP response."
  );
}

async function testOpenAiProviderException(): Promise<void> {
  const fetchImpl: typeof fetch = async () => {
    throw new Error("network unavailable");
  };

  const provider = new OpenAiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateScript(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError &&
      error.provider === "openai" &&
      error.userMessage === "OpenAI request failed. Check your network connection and API key." &&
      error.technicalDetail === "network unavailable"
  );
}

async function testOpenAiSuccessfulResponse(): Promise<void> {
  let requestBody: { model?: string; store?: boolean; input?: string } | undefined;
  const fetchImpl: typeof fetch = async (_url, init) => {
    requestBody = JSON.parse(String(init?.body)) as typeof requestBody;
    return new Response(JSON.stringify({ output: [{ content: [{ type: "output_text", text: validScriptMarkdown }] }] }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });
  };

  const provider = new OpenAiProvider({ apiKey: "test-key", model: "openai-test", fetchImpl });
  const result = await provider.generateScript(sampleRequest);

  assert.strictEqual(result.provider, "openai");
  assert.strictEqual(result.model, "openai-test");
  assert.strictEqual(result.markdownOutput, validScriptMarkdown);
  assert.strictEqual(result.structured?.kind, "command_to_script");
  assert.strictEqual(requestBody?.model, "openai-test");
  assert.strictEqual(requestBody?.store, false);
  assert.match(requestBody?.input ?? "", /# User Request/);
}

async function testOpenAiInvalidStructuredResponse(): Promise<void> {
  const fetchImpl: typeof fetch = async () =>
    new Response(JSON.stringify({ output_text: "## Intent\nGenerated." }), {
      status: 200,
      headers: { "Content-Type": "application/json" }
    });

  const provider = new OpenAiProvider({ apiKey: "test-key", fetchImpl });
  await assert.rejects(
    () => provider.generateTarget(sampleRequest),
    (error: unknown) =>
      error instanceof OptiNlpProviderError &&
      error.provider === "openai" &&
      error.userMessage === "OpenAI returned an invalid OptiNLP response." &&
      /Recommended Target|Candidate Nodes|Ambiguities|Intent/u.test(error.technicalDetail ?? "")
  );
}

async function testMarkdownSchemaParsing(): Promise<void> {
  const parsedTarget = parseOptiNlpMarkdownResult("target", validTargetMarkdown);
  assert.strictEqual(parsedTarget.kind, "target");
  assert.strictEqual(parsedTarget.recommendedTarget, "[occIndex 1; cFor \"i\"]");

  const parsedScript = parseOptiNlpMarkdownResult("command_to_script", validScriptMarkdown);
  assert.strictEqual(parsedScript.kind, "command_to_script");
  assert.match(parsedScript.generatedScript, /Loop\.unroll/u);

  const parsedCandidate = parseOptiNlpMarkdownResult("code_to_candidate_script", validCandidateMarkdown);
  assert.strictEqual(parsedCandidate.kind, "code_to_candidate_script");
  assert.strictEqual(parsedCandidate.candidateTransformations.length, 1);
}

const validTargetMarkdown = [
  "## Intent",
  "Target the second loop named `i`.",
  "",
  "## Candidate Nodes",
  "- Candidate 1: first loop",
  "- Candidate 2: second loop",
  "",
  "## Recommended Target",
  "```ocaml",
  "[occIndex 1; cFor \"i\"]",
  "```",
  "",
  "## Why This Target",
  "`cFor \"i\"` matches both loops, and `occIndex 1` selects the second.",
  "",
  "## Ambiguities",
  "None.",
  "",
  "## Alternatives",
  "```ocaml",
  "[cFunBody \"f\"; occIndex 1; cFor \"i\"]",
  "```",
  "",
  "## Validation",
  "```ocaml",
  "!! Show.target [occIndex 1; cFor \"i\"];",
  "```"
].join("\n");

const validScriptMarkdown = [
  "## Intent",
  "Unroll loop `i`.",
  "",
  "## Transformation API",
  "`Loop.unroll` applies to loop targets.",
  "",
  "## Target",
  "```ocaml",
  "[cFor \"i\"]",
  "```",
  "",
  "## Generated Script",
  "```ocaml",
  "open Optitrust",
  "open Target",
  "",
  "let _ = Run.script_cpp (fun _ ->",
  "  !! Loop.unroll [cFor \"i\"];",
  ")",
  "```",
  "",
  "## Assumptions",
  "None.",
  "",
  "## Validation",
  "```bash",
  "dune exec -- ./mock.exe",
  "```"
].join("\n");

const validCandidateMarkdown = [
  "## Code Summary",
  "One loop over `i` calls `work`.",
  "",
  "## Candidate Transformations",
  "| Rank | Transformation | Target | Why it may apply | Risk |",
  "| --- | --- | --- | --- | --- |",
  "| High | Loop.unroll | `[cFor \"i\"]` | User asked for a local loop transform | Needs valid unroll factor/default |",
  "",
  "## Recommended First Candidate",
  "Try unrolling the visible loop first.",
  "",
  "## Candidate Script",
  "```ocaml",
  "open Optitrust",
  "open Target",
  "",
  "let _ = Run.script_cpp (fun _ ->",
  "  !! Loop.unroll [cFor \"i\"];",
  ")",
  "```",
  "",
  "## Validation",
  "```bash",
  "dune exec -- ./mock.exe",
  "```",
  "",
  "## Missing Information",
  "None."
].join("\n");

async function main(): Promise<void> {
  await testProviderFactory();
  await testMockProvider();
  await testGeminiPromptConstruction();
  await testGeminiMissingApiKey();
  await testGeminiEmptyResponse();
  await testGeminiProviderException();
  await testGeminiSuccessfulResponse();
  await testGeminiInvalidStructuredResponse();
  await testOpenAiPromptConstruction();
  await testOpenAiMissingApiKey();
  await testOpenAiEmptyResponse();
  await testOpenAiProviderException();
  await testOpenAiSuccessfulResponse();
  await testOpenAiInvalidStructuredResponse();
  await testMarkdownSchemaParsing();
  console.log("OptiNLP provider tests passed.");
}

void main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
