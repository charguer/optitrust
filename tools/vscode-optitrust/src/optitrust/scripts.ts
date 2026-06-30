import * as path from "path";
import * as vscode from "vscode";
import { ActiveEditorContext } from "./editor";

export interface ScriptValidation {
  readonly ok: boolean;
  readonly reason?: string;
}

function normalizedRelativeDir(relativePath: string): string {
  return path.posix.dirname(relativePath.split(path.sep).join("/"));
}

function configuredScriptFolders(): string[] {
  return vscode.workspace.getConfiguration("optitrust").get<string[]>("scriptFolders", []).map(folder => folder.replace(/\\/g, "/").replace(/\/+$/, ""));
}

function isAllowedFolder(relativePath: string): boolean {
  const dir = normalizedRelativeDir(relativePath);
  if (dir === "tests" || dir.startsWith("tests/")) {
    return true;
  }
  if (dir === "case_studies" || dir.startsWith("case_studies/")) {
    return true;
  }

  return configuredScriptFolders().some(folder => dir === folder || dir.startsWith(`${folder}/`));
}

export function isOptitrustScript(document: vscode.TextDocument): boolean {
  if (document.languageId !== "ocaml" && path.extname(document.uri.fsPath) !== ".ml") {
    return false;
  }

  const text = document.getText();
  return /\bRun\.script(?:_cpp)?\b/.test(text) || /(^|\n)\s*!!/.test(text);
}

export function validateTransformationScript(context: ActiveEditorContext): ScriptValidation {
  if (path.extname(context.filePath) !== ".ml") {
    return { ok: false, reason: "OptiTrust transformation commands require an .ml script." };
  }

  if (!isAllowedFolder(context.relativePath)) {
    return {
      ok: false,
      reason: "This script is outside tests, case_studies, and configured OptiTrust script folders."
    };
  }

  if (!isOptitrustScript(context.document)) {
    return {
      ok: false,
      reason: "This .ml file does not look like an OptiTrust transformation script."
    };
  }

  return { ok: true };
}
