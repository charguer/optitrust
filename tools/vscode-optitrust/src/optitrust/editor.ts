import * as path from "path";
import * as vscode from "vscode";
import { relativeToRoot } from "./workspace";

export interface ActiveEditorContext {
  readonly editor: vscode.TextEditor;
  readonly document: vscode.TextDocument;
  readonly filePath: string;
  readonly fileDir: string;
  readonly fileBase: string;
  readonly relativePath: string;
  readonly line: number;
}

export function getActiveEditorContext(root: string, sourceEditor?: vscode.TextEditor): ActiveEditorContext {
  const editor = sourceEditor ?? vscode.window.activeTextEditor;
  if (!editor) {
    throw new Error("No active editor.");
  }

  const document = editor.document;
  if (document.uri.scheme !== "file") {
    throw new Error("The active document is not a local file.");
  }

  const filePath = document.uri.fsPath;
  const parsed = path.parse(filePath);
  return {
    editor,
    document,
    filePath,
    fileDir: parsed.dir,
    fileBase: parsed.name,
    relativePath: relativeToRoot(root, filePath),
    line: editor.selection.active.line + 1
  };
}
