import * as path from "path";
import * as vscode from "vscode";
import { getActiveEditorContext } from "../optitrust/editor";
import { AssociatedFile, findAssociatedFiles, OPTITRUST_C_SOURCE_EXTENSIONS, pickAssociatedFile } from "../optitrust/files";
import { fileExists } from "../optitrust/fileSystem";
import { openFileOrHtml } from "../optitrust/views";
import { OptitrustWorkspace } from "../optitrust/workspace";

type AssociatedQuickPickItem = vscode.QuickPickItem & {
  readonly all?: true;
  readonly file?: AssociatedFile;
};

function activePathOrThrow(): string {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.uri.scheme !== "file") {
    throw new Error("No local file is active.");
  }
  return editor.document.uri.fsPath;
}

async function openAssociated(workspace: OptitrustWorkspace, candidates: AssociatedFile[], message: string): Promise<void> {
  const file = await pickAssociatedFile(candidates, message);
  if (!file) {
    vscode.window.showInformationMessage(message);
    return;
  }
  await openFileOrHtml(workspace.root, file.path, file.label);
}

function stripOptilambdaRepresentationSuffix(name: string): string {
  return name.replace(/_(surface|internal|typed)$/u, "");
}

function isFrequentAssociatedFile(file: AssociatedFile): boolean {
  const parsed = path.parse(file.label);
  const name = stripOptilambdaRepresentationSuffix(parsed.name);
  return (
    parsed.ext === ".ml" ||
    (OPTITRUST_C_SOURCE_EXTENSIONS as readonly string[]).includes(parsed.ext) ||
    /_(out|exp|after)$/u.test(name)
  );
}

function associatedFileItem(file: AssociatedFile): AssociatedQuickPickItem {
  return {
    label: file.label,
    description: file.kind,
    detail: file.path,
    file
  };
}

function associatedFileGroup(label: string, files: AssociatedFile[]): AssociatedQuickPickItem[] {
  if (files.length === 0) {
    return [];
  }
  return [
    {
      label,
      kind: vscode.QuickPickItemKind.Separator
    },
    ...files.map(associatedFileItem)
  ];
}

export async function openGeneratedOutput(workspace: OptitrustWorkspace): Promise<void> {
  const files = (await findAssociatedFiles(activePathOrThrow())).filter(file => file.kind === "generated");
  await openAssociated(workspace, files, "No generated output file found for the current file.");
}

export async function openExpectedOutput(workspace: OptitrustWorkspace): Promise<void> {
  const files = (await findAssociatedFiles(activePathOrThrow())).filter(file => file.kind === "expected");
  await openAssociated(workspace, files, "No expected output file found for the current file.");
}

export async function openAssociatedFiles(workspace: OptitrustWorkspace): Promise<void> {
  const context = getActiveEditorContext(workspace.root);
  const files = await findAssociatedFiles(context.filePath);
  if (files.length === 0) {
    vscode.window.showInformationMessage("No associated files found for the current file.");
    return;
  }

  // Keep the editor-title button compact: one command opens a QuickPick that
  // exposes bulk open and individual file navigation.
  const frequentFiles = files.filter(isFrequentAssociatedFile);
  const otherFiles = files.filter(file => !isFrequentAssociatedFile(file));
  const items: AssociatedQuickPickItem[] = [
    {
      label: "Open all associated files",
      description: `${files.length} file(s)`,
      all: true
    },
    ...associatedFileGroup("Frequent files", frequentFiles),
    ...associatedFileGroup("Other files", otherFiles)
  ];

  const picked = await vscode.window.showQuickPick(
    items,
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

  if (!(await fileExists(mlFile))) {
    vscode.window.showWarningMessage(`No unit test script found: ${path.basename(mlFile)}`);
    return;
  }

  await openFileOrHtml(workspace.root, mlFile, path.basename(mlFile));
  if (await fileExists(cppFile)) {
    await openFileOrHtml(workspace.root, cppFile, path.basename(cppFile));
  }
}
