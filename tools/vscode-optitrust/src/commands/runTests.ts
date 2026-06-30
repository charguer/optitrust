import * as path from "path";
import * as vscode from "vscode";
import { getActiveEditorContext } from "../optitrust/editor";
import { runCommand } from "../optitrust/runner";
import { validateTransformationScript } from "../optitrust/scripts";
import { OptitrustWorkspace } from "../optitrust/workspace";

export async function runCurrentTest(workspace: OptitrustWorkspace): Promise<void> {
  const context = getActiveEditorContext(workspace.root);
  const validation = validateTransformationScript(context);
  if (!validation.ok) {
    vscode.window.showWarningMessage(validation.reason ?? "Unsupported OptiTrust test.");
    return;
  }

  await runCommand({
    cwd: workspace.root,
    command: path.join(workspace.root, "tester"),
    args: ["run", "-with-ignored", context.relativePath],
    title: "OptiTrust: Run Current Test"
  }).catch(() => undefined);
}

export async function rerunLastTests(workspace: OptitrustWorkspace): Promise<void> {
  await runCommand({
    cwd: workspace.root,
    command: path.join(workspace.root, "tester_last.sh"),
    title: "OptiTrust: Rerun Last-Tried Tests"
  }).catch(() => undefined);
}

export async function runCurrentTestAndOpenDiff(workspace: OptitrustWorkspace): Promise<void> {
  const context = getActiveEditorContext(workspace.root);
  const validation = validateTransformationScript(context);
  if (!validation.ok) {
    vscode.window.showWarningMessage(validation.reason ?? "Unsupported OptiTrust test.");
    return;
  }

  await runCommand({
    cwd: workspace.root,
    command: path.join(workspace.root, "tester"),
    args: ["rundiff", "-with-ignored", context.relativePath],
    title: "OptiTrust: Run Current Test And Open Diff"
  }).catch(() => undefined);
}
