import * as assert from "assert";
import * as fs from "fs/promises";
import * as os from "os";
import * as path from "path";
import { Writable } from "stream";
import { inferLanguage, loadOptiNlpAssets } from "./assets";
import { runOptiNlpCli } from "./cli";
import { resolveRequestedMode } from "./modes";

class MemoryWritable extends Writable {
  chunks: string[] = [];

  _write(chunk: string | Buffer, _encoding: BufferEncoding, callback: (error?: Error | null) => void): void {
    this.chunks.push(chunk.toString());
    callback();
  }

  text(): string {
    return this.chunks.join("");
  }
}

async function withTempSource(test: (filePath: string) => Promise<void>): Promise<void> {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "optinlp-cli-"));
  const filePath = path.join(dir, "input.cpp");
  await fs.writeFile(filePath, "void f(int n) { for (int i = 0; i < n; i++) work(i); }\n", "utf8");
  await test(filePath);
}

async function testLanguageInference(): Promise<void> {
  assert.strictEqual(inferLanguage("x.c"), "c");
  assert.strictEqual(inferLanguage("x.cpp"), "cpp");
  assert.strictEqual(inferLanguage("x.ml"), "ocaml");
  assert.strictEqual(inferLanguage("x.opti"), "optilambda");
}

async function testAssetLoading(): Promise<void> {
  const assets = await loadOptiNlpAssets(path.resolve(__dirname, "../../../.."), "target");
  assert.match(assets.promptText, /OptiTrust Target Generator/u);
  assert.match(assets.knowledgeText, /OptiTrust Target Knowledge/u);
}

async function testWholeFileScriptRouting(): Promise<void> {
  assert.strictEqual(resolveRequestedMode("command_to_script", "generate a complete transformation script for the whole file"), "code_to_candidate_script");
  assert.strictEqual(resolveRequestedMode("auto", "write matmul.ml for this source"), "code_to_candidate_script");
}

async function testCliTargetMarkdown(): Promise<void> {
  await withTempSource(async filePath => {
    const stdout = new MemoryWritable();
    const stderr = new MemoryWritable();
    const exitCode = await runOptiNlpCli(
      ["target", "--file", filePath, "--request", "target the loop i", "--provider", "mock"],
      stdout,
      stderr
    );

    assert.strictEqual(exitCode, 0);
    assert.match(stdout.text(), /## Recommended Target/u);
    assert.match(stdout.text(), /\[cFor "i"\]/u);
    assert.strictEqual(stderr.text(), "");
  });
}

async function testCliScriptJson(): Promise<void> {
  await withTempSource(async filePath => {
    const stdout = new MemoryWritable();
    const stderr = new MemoryWritable();
    const exitCode = await runOptiNlpCli(
      ["script", "--file", filePath, "--request", "unroll the loop i", "--provider", "mock", "--json"],
      stdout,
      stderr
    );

    assert.strictEqual(exitCode, 0);
    const parsed = JSON.parse(stdout.text()) as { structured: { kind: string; generatedScript: string } };
    assert.strictEqual(parsed.structured.kind, "command_to_script");
    assert.match(parsed.structured.generatedScript, /Loop\.unroll/u);
    assert.strictEqual(stderr.text(), "");
  });
}

async function testCliCandidatesGoal(): Promise<void> {
  await withTempSource(async filePath => {
    const stdout = new MemoryWritable();
    const stderr = new MemoryWritable();
    const exitCode = await runOptiNlpCli(
      ["candidates", "--file", filePath, "--goal", "suggest a first transformation", "--provider", "mock", "--json"],
      stdout,
      stderr
    );

    assert.strictEqual(exitCode, 0);
    const parsed = JSON.parse(stdout.text()) as { structured: { kind: string; candidateTransformations: unknown[] } };
    assert.strictEqual(parsed.structured.kind, "code_to_candidate_script");
    assert.strictEqual(parsed.structured.candidateTransformations.length, 1);
    assert.strictEqual(stderr.text(), "");
  });
}

async function testCliMissingRequest(): Promise<void> {
  const stdout = new MemoryWritable();
  const stderr = new MemoryWritable();
  const exitCode = await runOptiNlpCli(["target", "--file", "missing.cpp", "--provider", "mock"], stdout, stderr);

  assert.strictEqual(exitCode, 2);
  assert.match(stderr.text(), /Missing required --request option/u);
}

async function main(): Promise<void> {
  await testLanguageInference();
  await testAssetLoading();
  await testWholeFileScriptRouting();
  await testCliTargetMarkdown();
  await testCliScriptJson();
  await testCliCandidatesGoal();
  await testCliMissingRequest();
  console.log("OptiNLP CLI tests passed.");
}

void main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
