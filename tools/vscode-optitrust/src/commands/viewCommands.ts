import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { getActiveEditorContext } from "../optitrust/editor";
import { markExecutedLine } from "../optitrust/decorations";
import { appendLine } from "../optitrust/output";
import { runCommand } from "../optitrust/runner";
import { validateTransformationScript } from "../optitrust/scripts";
import { backendFlagsForViewMode, getSelectedViewMode, ViewModeDefinition } from "../optitrust/viewMode";
import { openHtmlView } from "../optitrust/views";
import { OptitrustWorkspace } from "../optitrust/workspace";

type ViewMode = "step_diff" | "full_trace" | "step_trace";
type ViewOption = "diff-only-code" | "diff-internal-syntax" | "trace-save-steps-script";

interface ViewCommandSpec {
  readonly mode: ViewMode;
  readonly scriptMode: "step_diff" | "full_trace" | "step_trace" | "standalone_full_trace";
  readonly title: string;
  readonly viewKind: "diff" | "trace" | "step-trace";
  readonly htmlSuffix: "_diff.html" | "_trace.html" | "_standalone_trace.html";
}

const VIEW_COMMANDS: Record<ViewMode, ViewCommandSpec> = {
  step_diff: {
    mode: "step_diff",
    scriptMode: "step_diff",
    title: "OptiTrust: View Step Diff",
    viewKind: "diff",
    htmlSuffix: "_diff.html"
  },
  full_trace: {
    mode: "full_trace",
    scriptMode: "standalone_full_trace",
    title: "OptiTrust: View Full Trace",
    viewKind: "trace",
    htmlSuffix: "_standalone_trace.html"
  },
  step_trace: {
    mode: "step_trace",
    scriptMode: "step_trace",
    title: "OptiTrust: View Step Trace",
    viewKind: "step-trace",
    htmlSuffix: "_trace.html"
  }
};

async function exists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

export async function runViewCommand(workspace: OptitrustWorkspace, mode: ViewMode, option?: ViewOption): Promise<void> {
  const spec = VIEW_COMMANDS[mode];
  const context = getActiveEditorContext(workspace.root);
  const validation = validateTransformationScript(context);
  if (!validation.ok) {
    vscode.window.showWarningMessage(validation.reason ?? "Unsupported OptiTrust script.");
    return;
  }

  markExecutedLine(context.editor, context.line);

  const selectedViewMode = getSelectedViewMode();
  const extraArgs = viewArgs(spec.mode, selectedViewMode, option);
  const args = [spec.scriptMode, context.relativePath, String(context.line), ...extraArgs];

  try {
    await runCommand({
      cwd: workspace.root,
      command: path.join(workspace.root, "tools", "view_result.sh"),
      args,
      title: spec.title,
      env: {
        OPTITRUST_NO_BROWSER: "1"
      }
    });
  } catch {
    return;
  }

  const htmlFile = path.join(context.fileDir, `${context.fileBase}${spec.htmlSuffix}`);
  if (await exists(htmlFile)) {
    await openHtmlView(workspace.root, htmlFile, spec.viewKind, `${selectedViewMode.id}:${option ?? "default"}:${context.relativePath}`, `${context.fileBase} ${spec.viewKind}`);
  } else {
    appendLine(`Generated view was not found: ${htmlFile}`);
    vscode.window.showWarningMessage(`OptiTrust command finished, but generated view was not found: ${path.basename(htmlFile)}`);
  }
}

function viewArgs(mode: ViewMode, selectedViewMode: ViewModeDefinition, option?: ViewOption): string[] {
  if (option === "diff-only-code") {
    return ["-print-only-code"];
  }
  if (option === "diff-internal-syntax") {
    return ["-print-optitrust-syntax"];
  }
  if (option === "trace-save-steps-script") {
    return ["-save-steps", "script"];
  }

  // Full standalone traces and step diffs generate both C/C++ and OptiLambda
  // payloads when supported, then switch syntax inside the webview. Passing the
  // global syntax flag here would collapse that dual-view behavior into a single
  // backend output.
  if (mode === "full_trace" || mode === "step_diff") {
    return [];
  }
  return backendFlagsForViewMode(selectedViewMode);
}

export function runViewDiffOnlyCode(workspace: OptitrustWorkspace): Promise<void> {
  return runViewCommand(workspace, "step_diff", "diff-only-code");
}

export function runViewDiffInternalSyntax(workspace: OptitrustWorkspace): Promise<void> {
  return runViewCommand(workspace, "step_diff", "diff-internal-syntax");
}

export function runViewTraceSaveStepsScript(workspace: OptitrustWorkspace): Promise<void> {
  return runViewCommand(workspace, "full_trace", "trace-save-steps-script");
}

export async function redoLastViewCommand(workspace: OptitrustWorkspace): Promise<void> {
  const redoScript = path.join(workspace.root, "tools", "_last_view_result.sh");
  try {
    await runCommand({
      cwd: workspace.root,
      command: redoScript,
      title: "OptiTrust: Redo Last View Command"
    });
  } catch {
    return;
  }
}
