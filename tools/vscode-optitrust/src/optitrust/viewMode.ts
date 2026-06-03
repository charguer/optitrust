import * as vscode from "vscode";
import { appendLine } from "./output";

export type ViewSyntax = "cpp" | "optilambda";

export function getViewSyntax(): ViewSyntax {
  return vscode.workspace.getConfiguration("optitrust").get<ViewSyntax>("viewSyntax", "cpp");
}

export function backendFlagsForViewSyntax(syntax: ViewSyntax): string[] {
  if (syntax === "optilambda") {
    appendLine("Requesting OptiLambda diff/trace output.");
    return ["-print-optilambda-syntax"];
  }
  return [];
}
