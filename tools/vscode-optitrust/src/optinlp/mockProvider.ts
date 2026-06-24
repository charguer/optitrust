// Deterministic provider used for tests and UI development without network
// access or API keys.
import { OptiNlpProvider, OptiNlpProviderRequest, OptiNlpProviderResult, requestWithMode } from "./providerTypes";
import { parseOptiNlpMarkdownResult } from "./resultSchemas";

export interface MockProviderOptions {
  readonly model?: string;
  readonly markdownOutput?: string;
}

export class MockProvider implements OptiNlpProvider {
  readonly name = "mock";
  readonly model: string;
  private readonly markdownOutput: string;

  constructor(options: MockProviderOptions = {}) {
    this.model = options.model ?? "mock-model";
    this.markdownOutput = options.markdownOutput ?? DEFAULT_MOCK_OUTPUT;
  }

  async generateTarget(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "target"));
  }

  async generateScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "command_to_script"));
  }

  async generateCandidateScript(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    return this.generate(requestWithMode(request, "code_to_candidate_script"));
  }

  private async generate(request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
    const markdownOutput = this.markdownOutput === DEFAULT_MOCK_OUTPUT ? mockOutputForMode(request.mode) : this.markdownOutput;
    return {
      provider: this.name,
      model: this.model,
      markdownOutput,
      structured: parseOptiNlpMarkdownResult(request.mode, markdownOutput),
      rawResponse: { provider: this.name, mode: request.mode }
    };
  }
}

const DEFAULT_MOCK_OUTPUT = "__default__";

function mockOutputForMode(mode: OptiNlpProviderRequest["mode"]): string {
  switch (mode) {
    case "target":
      return [
        "## Intent",
        "Mock target intent.",
        "",
        "## Candidate Nodes",
        "- Candidate 1: mock node",
        "",
        "## Recommended Target",
        "```ocaml",
        "[cFor \"i\"]",
        "```",
        "",
        "## Why This Target",
        "Mock explanation.",
        "",
        "## Ambiguities",
        "None.",
        "",
        "## Alternatives",
        "```ocaml",
        "[cFunBody \"f\"; cFor \"i\"]",
        "```",
        "",
        "## Validation",
        "```ocaml",
        "!! Show.target [cFor \"i\"];",
        "```"
      ].join("\n");
    case "command_to_script":
      return [
        "## Intent",
        "Mock script intent.",
        "",
        "## Transformation API",
        "Loop.unroll fits the mock command.",
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
    case "code_to_candidate_script":
      return [
        "## Code Summary",
        "Mock code summary.",
        "",
        "## Candidate Transformations",
        "| Rank | Transformation | Target | Why it may apply | Risk |",
        "| --- | --- | --- | --- | --- |",
        "| High | Loop.unroll | `[cFor \"i\"]` | Mock reason | Mock risk |",
        "",
        "## Recommended First Candidate",
        "Try the high-confidence mock candidate first.",
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
  }
}
