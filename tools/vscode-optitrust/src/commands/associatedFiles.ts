import * as path from "path";
import * as fs from "fs/promises";
import * as vscode from "vscode";
import { getActiveEditorContext } from "../optitrust/editor";
import { AssociatedFile, findAssociatedFiles, outputPairs, pickAssociatedFile } from "../optitrust/files";
import { openFileOrHtml } from "../optitrust/views";
import { OptitrustWorkspace } from "../optitrust/workspace";

function activePathOrThrow(): string {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.uri.scheme !== "file") {
    throw new Error("No local file is active.");
  }
  return editor.document.uri.fsPath;
}

async function exists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function openAssociated(workspace: OptitrustWorkspace, candidates: AssociatedFile[], message: string): Promise<void> {
  const file = await pickAssociatedFile(candidates, message);
  if (!file) {
    vscode.window.showInformationMessage(message);
    return;
  }
  await openFileOrHtml(workspace.root, file.path, file.label);
}

export async function openGeneratedOutput(workspace: OptitrustWorkspace): Promise<void> {
  const files = (await findAssociatedFiles(activePathOrThrow())).filter(file => file.kind === "generated");
  await openAssociated(workspace, files, "No generated output file found for the current file.");
}

export async function openExpectedOutput(workspace: OptitrustWorkspace): Promise<void> {
  const files = (await findAssociatedFiles(activePathOrThrow())).filter(file => file.kind === "expected");
  await openAssociated(workspace, files, "No expected output file found for the current file.");
}

export async function compareOutputExpected(): Promise<void> {
  const pairs = await outputPairs(activePathOrThrow());
  if (pairs.length === 0) {
    vscode.window.showInformationMessage("No generated/expected output pair found for the current file.");
    return;
  }

  const selected =
    pairs.length === 1
      ? pairs[0]
      : (
          await vscode.window.showQuickPick(
            pairs.map(pair => ({
              label: pair.label,
              description: `${path.basename(pair.out)} <-> ${path.basename(pair.exp)}`,
              pair
            })),
            { placeHolder: "Select output pair to compare" }
          )
        )?.pair;

  if (!selected) {
    return;
  }

  await vscode.commands.executeCommand(
    "vscode.diff",
    vscode.Uri.file(selected.out),
    vscode.Uri.file(selected.exp),
    `${path.basename(selected.out)} <-> ${path.basename(selected.exp)}`
  );
}

export async function openAssociatedFiles(workspace: OptitrustWorkspace): Promise<void> {
  const context = getActiveEditorContext(workspace.root);
  const files = await findAssociatedFiles(context.filePath);
  const pairs = await outputPairs(context.filePath);
  if (files.length === 0) {
    vscode.window.showInformationMessage("No associated files found for the current file.");
    return;
  }

  // Keep the editor-title button compact: one command opens a QuickPick that
  // exposes bulk open, pair comparison, and individual file navigation.
  const picked = await vscode.window.showQuickPick(
    [
      {
        label: "Open all associated files",
        description: `${files.length} file(s)`,
        all: true
      },
      ...pairs.map(pair => ({
        label: `Compare ${pair.label}`,
        description: `${path.basename(pair.out)} <-> ${path.basename(pair.exp)}`,
        pair
      })),
      ...files.map(file => ({
        label: file.label,
        description: file.kind,
        detail: file.path,
        file
      }))
    ],
    { placeHolder: "Select associated OptiTrust file" }
  );

  if (!picked) {
    return;
  }

  if ("all" in picked) {
    for (const file of files) {
      await openFileOrHtml(workspace.root, file.path, file.label);
    }
    return;
  }

  if ("pair" in picked && picked.pair) {
    await vscode.commands.executeCommand(
      "vscode.diff",
      vscode.Uri.file(picked.pair.out),
      vscode.Uri.file(picked.pair.exp),
      `${path.basename(picked.pair.out)} <-> ${path.basename(picked.pair.exp)}`
    );
    return;
  }

  if ("file" in picked && picked.file) {
    await openFileOrHtml(workspace.root, picked.file.path, picked.file.label);
  }
}

export async function openUnitTestMlCppFiles(workspace: OptitrustWorkspace): Promise<void> {
  const currentPath = activePathOrThrow();
  const parsed = path.parse(currentPath);
  const base = path.join(parsed.dir, parsed.name.replace(/_doc$/u, "").replace(/_exp$/u, "").replace(/_out$/u, ""));
  const mlFile = `${base}.ml`;
  const cppFile = `${base}.cpp`;

  if (!(await exists(mlFile))) {
    vscode.window.showWarningMessage(`No unit test script found: ${path.basename(mlFile)}`);
    return;
  }

  await openFileOrHtml(workspace.root, mlFile, path.basename(mlFile));
  if (await exists(cppFile)) {
    await openFileOrHtml(workspace.root, cppFile, path.basename(cppFile));
  }
}
