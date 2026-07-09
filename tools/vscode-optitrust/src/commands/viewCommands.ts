import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { getActiveEditorContext } from "../optitrust/editor";
import { markExecutedLine } from "../optitrust/decorations";
import { fileExists } from "../optitrust/fileSystem";
import { appendLine } from "../optitrust/output";
import { runCommand } from "../optitrust/runner";
import { validateTransformationScript } from "../optitrust/scripts";
import { backendFlagsForViewMode, getSelectedViewMode, VIEW_MODES, ViewModeDefinition } from "../optitrust/viewMode";
import { openHtmlView } from "../optitrust/views";
import { OptitrustWorkspace } from "../optitrust/workspace";

type ViewMode = "step_diff" | "full_trace" | "step_trace";
type ViewOption = "trace-save-steps-script";

interface ViewCommandSpec {
  readonly scriptMode: "step_diff" | "full_trace" | "step_trace";
  readonly title: string;
  readonly viewKind: "diff" | "trace" | "step-trace";
  readonly htmlSuffix: "_diff.html" | "_trace.html";
}

const VIEW_COMMANDS: Record<ViewMode, ViewCommandSpec> = {
  step_diff: {
    scriptMode: "step_diff",
    title: "OptiTrust: View Step Diff",
    viewKind: "diff",
    htmlSuffix: "_diff.html"
  },
  full_trace: {
    scriptMode: "full_trace",
    title: "OptiTrust: View Full Trace",
    viewKind: "trace",
    htmlSuffix: "_trace.html"
  },
  step_trace: {
    scriptMode: "step_trace",
    title: "OptiTrust: View Step Trace",
    viewKind: "step-trace",
    htmlSuffix: "_trace.html"
  }
};

const CPP_VIEW_MODE = VIEW_MODES.find(mode => mode.id === "cpp") ?? VIEW_MODES[0];
const DEFAULT_STEP_DIFF_VIEW_MODE = VIEW_MODES.find(mode => mode.id === "optilambda.surface") ?? VIEW_MODES[1];

interface StoredViewContext {
  readonly root: string;
  readonly relativePath: string;
  readonly line: number;
  readonly fileDir: string;
  readonly fileBase: string;
}

interface StoredViewRequest {
  readonly mode: ViewMode;
  readonly option?: ViewOption;
  readonly context: StoredViewContext;
  readonly viewMode: ViewModeDefinition;
}

let lastViewRequest: StoredViewRequest | undefined;

export async function runViewCommand(workspace: OptitrustWorkspace, mode: ViewMode, option?: ViewOption): Promise<void> {
  const context = getActiveEditorContext(workspace.root);
  const validation = validateTransformationScript(context);
  if (!validation.ok) {
    vscode.window.showWarningMessage(validation.reason ?? "Unsupported OptiTrust script.");
    return;
  }

  markExecutedLine(context.editor, context.line);

  const selectedViewMode = getSelectedViewMode();
  const initialDiffViewMode = option === undefined ? DEFAULT_STEP_DIFF_VIEW_MODE : selectedViewMode;
  const commandViewMode = mode === "step_diff" ? initialDiffViewMode : selectedViewMode;

  await executeViewRequest(workspace, {
    mode,
    option,
    context: {
      root: workspace.root,
      relativePath: context.relativePath,
      line: context.line,
      fileDir: context.fileDir,
      fileBase: context.fileBase
    },
    viewMode: commandViewMode
  });
}

function viewArgs(mode: ViewMode, selectedViewMode: ViewModeDefinition, option?: ViewOption): string[] {
  if (option === "trace-save-steps-script") {
    return ["-save-steps", "script"];
  }

  // Full traces use serialized, server-backed data for in-window switching.
  // Step diffs generate the selected syntax first; the HTML diff view requests
  // other syntaxes lazily when the user switches representation.
  if (mode === "full_trace") {
    return [];
  }
  return backendFlagsForViewMode(selectedViewMode);
}

export function runViewTraceSaveStepsScript(workspace: OptitrustWorkspace): Promise<void> {
  return runViewCommand(workspace, "full_trace", "trace-save-steps-script");
}

export async function redoLastViewCommand(workspace: OptitrustWorkspace): Promise<void> {
  const request = lastViewRequest ?? (await readLastViewRequest(workspace));
  if (request) {
    await executeViewRequest(workspace, request, "OptiTrust: Redo Last View Command");
    return;
  }

  const redoScript = path.join(workspace.root, "tools", "_last_view_result.sh");
  try {
    await runCommand({
      cwd: workspace.root,
      command: redoScript,
      title: "OptiTrust: Redo Last View Command",
      env: {
        OPTITRUST_NO_BROWSER: "1"
      }
    });
  } catch {
    return;
  }
  vscode.window.showWarningMessage("Redo finished, but no extension view context was available. Run View Step Diff or View Full Trace once from the extension.");
}

async function executeViewRequest(workspace: OptitrustWorkspace, request: StoredViewRequest, titleOverride?: string): Promise<void> {
  const spec = VIEW_COMMANDS[request.mode];
  const args = [
    spec.scriptMode,
    request.context.relativePath,
    String(request.context.line),
    ...viewArgs(request.mode, request.viewMode, request.option)
  ];

  try {
    await runCommand({
      cwd: workspace.root,
      command: path.join(workspace.root, "tools", "view_result.sh"),
      args,
      title: titleOverride ?? spec.title,
      env: {
        OPTITRUST_NO_BROWSER: "1"
      }
    });
  } catch {
    return;
  }

  lastViewRequest = request;
  await openViewResult(request);
}

async function openViewResult(request: StoredViewRequest): Promise<void> {
  const spec = VIEW_COMMANDS[request.mode];
  const htmlFile = path.join(request.context.fileDir, `${request.context.fileBase}${spec.htmlSuffix}`);
  if (await fileExists(htmlFile)) {
    await openHtmlView(
      request.context.root,
      htmlFile,
      spec.viewKind,
      `${request.viewMode.id}:${request.option ?? "default"}:${request.context.relativePath}`,
      `${request.context.fileBase} ${spec.viewKind}`,
      {
        useLiveView: true,
        lazyDiff:
          request.mode === "step_diff"
            ? {
                relativePath: request.context.relativePath,
                line: request.context.line
              }
            : undefined,
        initialDiffRepresentation:
          request.mode === "step_diff"
            ? request.viewMode.optilambdaRepresentation ?? "cpp"
            : undefined
      }
    );
  } else {
    appendLine(`Generated view was not found: ${htmlFile}`);
    vscode.window.showWarningMessage(`OptiTrust command finished, but generated view was not found: ${path.basename(htmlFile)}`);
  }
}

async function readLastViewRequest(workspace: OptitrustWorkspace): Promise<StoredViewRequest | undefined> {
  const redoScript = path.join(workspace.root, "tools", "_last_view_result.sh");
  let content: string;
  try {
    content = await fs.readFile(redoScript, "utf8");
  } catch {
    return undefined;
  }

  const args = parseLastViewResultArgs(content);
  if (args.length < 3) {
    return undefined;
  }

  const mode = modeFromScriptMode(args[0]);
  const line = Number(args[2]);
  if (!mode || !Number.isInteger(line)) {
    return undefined;
  }

  const filePath = path.resolve(workspace.root, args[1]);
  return {
    mode,
    context: {
      root: workspace.root,
      relativePath: path.relative(workspace.root, filePath),
      line,
      fileDir: path.dirname(filePath),
      fileBase: path.basename(filePath, path.extname(filePath))
    },
    viewMode: viewModeFromArgs(args.slice(3), mode)
  };
}

function parseLastViewResultArgs(content: string): string[] {
  const tokens = splitShellWords(content.trim());
  const scriptIndex = tokens.findIndex(token => token.endsWith("view_result.sh"));
  return scriptIndex >= 0 ? tokens.slice(scriptIndex + 1) : [];
}

function splitShellWords(text: string): string[] {
  const words: string[] = [];
  const pattern = /"([^"\\]*(?:\\.[^"\\]*)*)"|'([^']*)'|(\S+)/gu;
  for (const match of text.matchAll(pattern)) {
    words.push((match[1] ?? match[2] ?? match[3] ?? "").replace(/\\(["\\])/gu, "$1"));
  }
  return words;
}

function modeFromScriptMode(scriptMode: string): ViewMode | undefined {
  if (scriptMode === "step_diff" || scriptMode === "step_diff_from_inter") {
    return "step_diff";
  }
  if (scriptMode === "full_trace" || scriptMode === "standalone_full_trace" || scriptMode === "full_trace_from_inter") {
    return "full_trace";
  }
  if (scriptMode === "step_trace") {
    return "step_trace";
  }
  return undefined;
}

function viewModeFromArgs(args: string[], mode: ViewMode): ViewModeDefinition {
  if (args.includes("-print-optilambda-syntax")) {
    const representation = optionValue(args, "-optilambda-repr") ?? "surface";
    return VIEW_MODES.find(viewMode => viewMode.optilambdaRepresentation === representation) ?? DEFAULT_STEP_DIFF_VIEW_MODE;
  }

  return mode === "step_diff" ? CPP_VIEW_MODE : getSelectedViewMode();
}

function optionValue(args: string[], option: string): string | undefined {
  const index = args.indexOf(option);
  return index >= 0 ? args[index + 1] : undefined;
}
