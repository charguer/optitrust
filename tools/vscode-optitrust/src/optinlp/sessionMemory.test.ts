import * as assert from "assert";
import { OptiNlpProviderRequest, OptiNlpProviderResult } from "./providerTypes";
import { OptiNlpSessionMemory } from "./sessionMemory";

const baseRequest: OptiNlpProviderRequest = {
  mode: "target",
  userRequest: "target the loop i",
  sourceText: "void f() {}",
  filePath: "tests/demo.cpp",
  language: "cpp",
  promptText: "prompt",
  knowledgeText: "knowledge"
};

function targetResult(target: string): OptiNlpProviderResult {
  return {
    provider: "mock",
    model: "mock-model",
    markdownOutput: "markdown",
    structured: {
      kind: "target",
      intent: "Target a loop.",
      candidateNodes: ["Candidate 1"],
      recommendedTarget: target,
      alternatives: [],
      validation: `!! Show.target ${target};`
    }
  };
}

function scriptResult(): OptiNlpProviderResult {
  return {
    provider: "mock",
    model: "mock-model",
    markdownOutput: "markdown",
    structured: {
      kind: "command_to_script",
      intent: "Unroll a loop.",
      transformationApi: "Loop.unroll",
      target: "[cFor \"i\"]",
      generatedScript: "open Optitrust\nopen Target\nlet _ = Run.script_cpp (fun _ ->\n  !! Loop.unroll [cFor \"i\"];\n)",
      assumptions: ["Loop `i` is unique."],
      validation: "dune exec -- ./mock.exe"
    }
  };
}

function fullScriptResult(): OptiNlpProviderResult {
  return {
    provider: "mock",
    model: "mock-model",
    markdownOutput: "markdown",
    structured: {
      kind: "code_to_full_script",
      codeSummary: "A loop.",
      candidateTransformations: [
        {
          rank: "High",
          transformation: "Loop.unroll",
          target: "[cFor \"i\"]",
          whyItMayApply: "Local loop.",
          risk: "Needs validation."
        }
      ],
      recommendedFirstCandidate: "Try unrolling.",
      fullScript: "open Optitrust\nopen Target\nlet _ = Run.script_cpp (fun _ ->\n  !! Loop.unroll [cFor \"i\"];\n)",
      validation: "dune exec -- ./mock.exe",
      missingInformation: "None."
    }
  };
}

function testRecordsCompactGenerationState(): void {
  const memory = new OptiNlpSessionMemory();
  memory.recordGeneration(baseRequest, targetResult("[cFor \"i\"]"));

  const snapshot = memory.snapshot();
  assert.strictEqual(snapshot.turns.length, 1);
  assert.strictEqual(snapshot.turns[0].target, "[cFor \"i\"]");
  assert.strictEqual(snapshot.turns[0].userRequest, "target the loop i");
  assert.strictEqual(snapshot.turns[0].provider, "mock");
  assert.ok(!JSON.stringify(snapshot).includes("void f"));
  assert.ok(!JSON.stringify(snapshot).includes("knowledge"));
}

function testSummaryIncludesLatestScriptAssumptionsAndValidation(): void {
  const memory = new OptiNlpSessionMemory();
  memory.recordGeneration({ ...baseRequest, mode: "command_to_script", userRequest: "unroll the loop i" }, scriptResult());
  memory.acceptAssumptions(["Loop `i` is unique.", "Loop `i` is unique.", "No side effects."]);
  memory.recordValidation({ command: "dune exec -- ./mock.exe", ok: false, detail: "exit code 1" });

  const summary = memory.summary();
  assert.ok(summary);
  assert.match(summary, /Previous request: unroll the loop i/u);
  assert.match(summary, /Previous target: \[cFor "i"\]/u);
  assert.match(summary, /Previous script:/u);
  assert.match(summary, /Accepted assumptions: Loop `i` is unique.; No side effects./u);
  assert.match(summary, /Last validation: failed/u);
}

function testKeepsOnlyMaxTurns(): void {
  const memory = new OptiNlpSessionMemory({ maxTurns: 2 });
  memory.recordGeneration({ ...baseRequest, userRequest: "first" }, targetResult("[cFor \"i\"]"));
  memory.recordGeneration({ ...baseRequest, userRequest: "second" }, targetResult("[cFor \"j\"]"));
  memory.recordGeneration({ ...baseRequest, userRequest: "third" }, targetResult("[cFor \"k\"]"));

  const snapshot = memory.snapshot();
  assert.deepStrictEqual(
    snapshot.turns.map(turn => turn.userRequest),
    ["second", "third"]
  );
}

function testCandidateSummaryAndClear(): void {
  const memory = new OptiNlpSessionMemory();
  memory.recordGeneration({ ...baseRequest, mode: "code_to_full_script", userRequest: "generate full transformation" }, fullScriptResult());
  assert.match(memory.summary() ?? "", /Previous full script:/u);

  memory.clear();
  assert.strictEqual(memory.summary(), undefined);
  assert.strictEqual(memory.snapshot().turns.length, 0);
}

function main(): void {
  testRecordsCompactGenerationState();
  testSummaryIncludesLatestScriptAssumptionsAndValidation();
  testKeepsOnlyMaxTurns();
  testCandidateSummaryAndClear();
  console.log("OptiNLP session memory tests passed.");
}

main();
