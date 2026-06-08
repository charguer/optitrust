import * as vscode from "vscode";
import { appendLine } from "./output";

export type ViewSyntax = "cpp" | "optilambda";
export type OptilambdaRepresentation = "surface" | "internal" | "typed";
export type ViewModeId = "cpp" | `optilambda.${OptilambdaRepresentation}`;

export interface ViewModeDefinition {
  readonly id: ViewModeId;
  readonly label: string;
  readonly description: string;
  readonly syntax: ViewSyntax;
  readonly optilambdaRepresentation?: OptilambdaRepresentation;
}

export const VIEW_MODES: readonly ViewModeDefinition[] = [
  {
    id: "cpp",
    label: "C/C++",
    description: "Use current C/C++ diff and trace output.",
    syntax: "cpp"
  },
  {
    id: "optilambda.surface",
    label: "OptiLambda Surface",
    description: "Use readable OptiLambda syntax.",
    syntax: "optilambda",
    optilambdaRepresentation: "surface"
  },
  {
    id: "optilambda.internal",
    label: "OptiLambda Internal",
    description: "Use explicit internal OptiLambda operations.",
    syntax: "optilambda",
    optilambdaRepresentation: "internal"
  },
  {
    id: "optilambda.typed",
    label: "OptiLambda Fully-Typed",
    description: "Use explicit OptiLambda operations with type parameters.",
    syntax: "optilambda",
    optilambdaRepresentation: "typed"
  }
];

export function getViewSyntax(): ViewSyntax {
  return vscode.workspace.getConfiguration("optitrust").get<ViewSyntax>("viewSyntax", "cpp");
}

export function getOptilambdaRepresentation(): OptilambdaRepresentation {
  return vscode.workspace.getConfiguration("optitrust").get<OptilambdaRepresentation>("optilambdaRepresentation", "surface");
}

export function getSelectedViewMode(): ViewModeDefinition {
  const syntax = getViewSyntax();
  if (syntax === "cpp") {
    return VIEW_MODES[0];
  }
  const representation = getOptilambdaRepresentation();
  return VIEW_MODES.find(mode => mode.optilambdaRepresentation === representation) ?? VIEW_MODES[1];
}

export async function updateSelectedViewMode(mode: ViewModeDefinition): Promise<void> {
  const config = vscode.workspace.getConfiguration("optitrust");
  await config.update("viewSyntax", mode.syntax, vscode.ConfigurationTarget.Workspace);
  if (mode.optilambdaRepresentation) {
    await config.update("optilambdaRepresentation", mode.optilambdaRepresentation, vscode.ConfigurationTarget.Workspace);
  }
}

export function backendFlagsForViewMode(mode: ViewModeDefinition): string[] {
  if (mode.syntax === "optilambda") {
    const representation = mode.optilambdaRepresentation ?? "surface";
    appendLine(`Requesting ${mode.label} diff/trace output.`);
    return ["-print-optilambda-syntax", "-optilambda-repr", representation];
  }
  return [];
}
