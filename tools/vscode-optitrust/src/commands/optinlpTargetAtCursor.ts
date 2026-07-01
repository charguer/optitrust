// F7 OptiNLP workflow: execute an OptiTrust script up to the line before the
// cursor, open the generated OptiLambda state, and ask the configured provider
// for robust target suggestions for the current transformation line.
import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { OptiNlpPanel } from "./optinlpPanel";
import { runOptiNlpGeneration } from "./optinlpCommands";
import { getActiveEditorContext } from "../optitrust/editor";
import { markExecutedLine } from "../optitrust/decorations";
import { runCommand } from "../optitrust/runner";
import { validateTransformationScript } from "../optitrust/scripts";
import { findAssociatedCSourceFile } from "../optitrust/files";
import { openFileOrHtml } from "../optitrust/views";
import { OptitrustWorkspace, relativeToRoot } from "../optitrust/workspace";
import { OptiNlpSessionMemory } from "../optinlp/sessionMemory";

interface TargetAtCursorContext {
  readonly scriptPath: string;
  readonly scriptRelativePath: string;
  readonly sourcePath: string;
  readonly sourceRelativePath: string;
  readonly afterOptiPath: string;
  readonly transformationLine: number;
  readonly executionLine: number;
  readonly transformationText: string;
  readonly scriptPrefix: string;
  readonly sourceText: string;
  readonly afterOptiText: string;
}

interface PrefixScript {
  readonly filePath: string;
  readonly relativePath: string;
  readonly noOpLine: number;
}

async function exists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function readText(filePath: string): Promise<string> {
  return fs.readFile(filePath, "utf8");
}

async function writeText(filePath: string, text: string): Promise<void> {
  await fs.writeFile(filePath, text, "utf8");
}

async function findAfterOptiFile(scriptPath: string): Promise<string | undefined> {
  const parsed = path.parse(scriptPath);
  const candidates = [
    path.join(parsed.dir, `${parsed.name}_after_surface.opti`),
    path.join(parsed.dir, `${parsed.name}_after.opti`),
    path.join(parsed.dir, `${parsed.name}_after_internal.opti`),
    path.join(parsed.dir, `${parsed.name}_after_typed.opti`)
  ];
  for (const candidate of candidates) {
    if (await exists(candidate)) {
      return candidate;
    }
  }
  return undefined;
}

function scriptPrefix(document: vscode.TextDocument, stopBeforeLine: number): string {
  const end = new vscode.Position(Math.max(0, stopBeforeLine - 1), 0);
  return document.getText(new vscode.Range(new vscode.Position(0, 0), end));
}

function lineText(document: vscode.TextDocument, line: number): string {
  const index = Math.max(0, Math.min(document.lineCount - 1, line - 1));
  return document.lineAt(index).text;
}

function currentTransformationStartLine(document: vscode.TextDocument, cursorLine: number): number {
  for (let line = Math.max(1, cursorLine); line >= 1; line -= 1) {
    const text = lineText(document, line);
    if (/^\s*!!!?/u.test(text)) {
      return line;
    }
  }
  return cursorLine;
}

function currentTransformationText(document: vscode.TextDocument, startLine: number, cursorLine: number): string {
  const stopLine = Math.min(document.lineCount, Math.max(cursorLine, startLine) + 4);
  const lines: string[] = [];
  for (let line = startLine; line <= stopLine; line += 1) {
    const text = lineText(document, line);
    lines.push(text);
    if (line > cursorLine && /^\s*\);?\s*$/u.test(text)) {
      break;
    }
  }
  return lines.join("\n").trim();
}

function patchScriptCppCall(scriptText: string, sourceFileName: string, outputPrefix: string): string {
  const withExistingFilename = scriptText.replace(
    /\bRun\.script_cpp\s+~filename\s*:\s*"[^"]*"/u,
    `Run.script_cpp ~filename:"${sourceFileName}" ~prefix:"${outputPrefix}"`
  );
  if (withExistingFilename !== scriptText) {
    return withExistingFilename;
  }
  return scriptText.replace(/\bRun\.script_cpp\b/u, `Run.script_cpp ~filename:"${sourceFileName}" ~prefix:"${outputPrefix}"`);
}

async function createPrefixScript(
  workspace: OptitrustWorkspace,
  editorContext: ReturnType<typeof getActiveEditorContext>,
  sourcePath: string,
  stopBeforeLine: number
): Promise<PrefixScript> {
  const parsed = path.parse(editorContext.filePath);
  const tempBase = `${editorContext.fileBase}_optinlp_prefix`;
  const tempPath = path.join(parsed.dir, `${tempBase}.ml`);
  const prefix = scriptPrefix(editorContext.document, stopBeforeLine);
  const patchedPrefix = patchScriptCppCall(prefix, path.basename(sourcePath), editorContext.fileBase);
  const trimmedPrefix = patchedPrefix.trimEnd();
  const noOpLine = trimmedPrefix.length === 0 ? 1 : trimmedPrefix.split(/\r?\n/u).length + 1;
  const tempText = [
    trimmedPrefix,
    "  !!();",
    ")",
    ""
  ].join("\n");

  await writeText(tempPath, tempText);
  return {
    filePath: tempPath,
    relativePath: relativeToRoot(workspace.root, tempPath),
    noOpLine
  };
}

async function cleanupPrefixScript(prefixScript: PrefixScript): Promise<void> {
  const parsed = path.parse(prefixScript.filePath);
  const candidates = [
    prefixScript.filePath,
    path.join(parsed.dir, `${parsed.name}.cmxs`)
  ];
  await Promise.all(candidates.map(async candidate => {
    try {
      await fs.unlink(candidate);
    } catch {
      // Best-effort cleanup only. The generated after-state is kept.
    }
  }));
}

function targetRequest(context: TargetAtCursorContext): string {
  return [
    `Generate robust OptiTrust target suggestions for ${context.scriptRelativePath}:${context.transformationLine}.`,
    `The OptiTrust script has been executed through line ${context.executionLine}, immediately before the current line.`,
    "Use the matching C/C++ source and the generated OptiLambda after-state to infer the target for the current transformation line.",
    "Prefer concise semantic targets. Avoid sExpr, sInstr, and other exact-text selectors unless no semantic target can work.",
    "",
    "Current transformation line:",
    "```ocaml",
    context.transformationText,
    "```"
  ].join("\n");
}

function targetSourceContext(context: TargetAtCursorContext): string {
  return [
    `# Matching C/C++ Source: ${context.sourceRelativePath}`,
    "```cpp",
    context.sourceText.trimEnd(),
    "```",
    "",
    `# OptiTrust Script Prefix Through Line ${context.executionLine}: ${context.scriptRelativePath}`,
    "```ocaml",
    context.scriptPrefix.trimEnd(),
    "```",
    "",
    `# Current Transformation Line ${context.transformationLine}`,
    "```ocaml",
    context.transformationText,
    "```",
    "",
    `# Generated OptiLambda State Before Current Line: ${relativeToRoot(path.dirname(context.scriptPath), context.afterOptiPath)}`,
    "```optilambda",
    context.afterOptiText.trimEnd(),
    "```"
  ].join("\n");
}

async function collectTargetAtCursorContext(workspace: OptitrustWorkspace, editorContext: ReturnType<typeof getActiveEditorContext>): Promise<TargetAtCursorContext | undefined> {
  const source = await findAssociatedCSourceFile(editorContext.filePath);
  if (!source) {
    vscode.window.showWarningMessage(`OptiNLP: no matching C/C++ source file found for ${editorContext.fileBase}.ml.`);
    return undefined;
  }
  const sourcePath = source.path;

  const transformationStartLine = currentTransformationStartLine(editorContext.document, editorContext.line);
  const executionLine = Math.max(1, transformationStartLine - 1);
  const prefixScript = await createPrefixScript(workspace, editorContext, sourcePath, transformationStartLine);
  markExecutedLine(editorContext.editor, executionLine);

  try {
    await runCommand({
      cwd: workspace.root,
      command: path.join(workspace.root, "tools", "view_result.sh"),
      args: ["step_diff", prefixScript.relativePath, String(prefixScript.noOpLine)],
      title: "OptiTrust: Prepare OptiNLP Target Context",
      env: {
        NODIFFDISPLAY: "1",
        OPTITRUST_NO_BROWSER: "1"
      }
    });
  } catch {
    return undefined;
  } finally {
    await cleanupPrefixScript(prefixScript);
  }

  const afterOptiPath = await findAfterOptiFile(editorContext.filePath);
  if (!afterOptiPath) {
    vscode.window.showWarningMessage(`OptiNLP: script ran, but no ${editorContext.fileBase}_after.opti file was found.`);
    return undefined;
  }

  return {
    scriptPath: editorContext.filePath,
    scriptRelativePath: editorContext.relativePath,
    sourcePath,
    sourceRelativePath: relativeToRoot(workspace.root, sourcePath),
    afterOptiPath,
    transformationLine: editorContext.line,
    executionLine,
    transformationText: currentTransformationText(editorContext.document, transformationStartLine, editorContext.line),
    scriptPrefix: scriptPrefix(editorContext.document, transformationStartLine),
    sourceText: await readText(sourcePath),
    afterOptiText: await readText(afterOptiPath)
  };
}

export async function suggestOptiNlpTargetAtCursor(
  extensionContext: vscode.ExtensionContext,
  workspace: OptitrustWorkspace,
  memory: OptiNlpSessionMemory
): Promise<void> {
  const editorContext = getActiveEditorContext(workspace.root);
  const validation = validateTransformationScript(editorContext);
  if (!validation.ok) {
    vscode.window.showWarningMessage(validation.reason ?? "Unsupported OptiTrust script.");
    return;
  }

  const collected = await collectTargetAtCursorContext(workspace, editorContext);
  if (!collected) {
    return;
  }

  await vscode.window.showTextDocument(editorContext.document, { viewColumn: vscode.ViewColumn.One, preserveFocus: false });
  await openFileOrHtml(workspace.root, collected.afterOptiPath, path.basename(collected.afterOptiPath));

  const panel = OptiNlpPanel.show(extensionContext, workspace, memory);
  panel.setSourceEditor(editorContext.editor, true);

  const request = targetRequest(collected);
  panel.postUserRequest("target", request, `Target · ${collected.scriptRelativePath}:${collected.transformationLine}`);

  const outcome = await runOptiNlpGeneration(extensionContext, workspace, memory, "target", request, {
    renderToOutput: false,
    editor: editorContext.editor,
    sourceContext: {
      text: targetSourceContext(collected),
      label: "target-at-cursor context"
    },
    filePath: collected.sourceRelativePath,
    language: "cpp+optilambda+ocaml"
  });

  if (outcome) {
    panel.postGenerationOutcome(outcome);
  }
}
