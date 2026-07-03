import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { appendLine } from "./output";
import { runCommand } from "./runner";
import { backendFlagsForViewMode, ViewModeDefinition, VIEW_MODES } from "./viewMode";

const OPTITRUST_DIFF_SCHEME = "optitrust-diff";

interface DiffFilePair {
  readonly before: string;
  readonly after: string;
  readonly label: string;
}

interface NativeDiffSession {
  readonly id: string;
  readonly root: string;
  readonly scriptRelativePath: string;
  readonly line: number;
  readonly fileDir: string;
  readonly fileBase: string;
  readonly generatedModes: Set<string>;
}

export interface NativeStepDiffContext {
  readonly root: string;
  readonly scriptRelativePath: string;
  readonly line: number;
  readonly fileDir: string;
  readonly fileBase: string;
}

interface OpenNativeStepDiffOptions {
  readonly viewColumn?: vscode.ViewColumn;
  readonly markGenerated?: boolean;
  readonly generateIfMissing?: boolean;
}

const sessions = new Map<string, NativeDiffSession>();

async function exists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

function stepDiffCandidates(fileDir: string, fileBase: string, selectedViewMode: ViewModeDefinition): DiffFilePair[] {
  if (selectedViewMode.id === "optilambda.surface") {
    return [
      {
        before: path.join(fileDir, `${fileBase}_before.opti`),
        after: path.join(fileDir, `${fileBase}_after.opti`),
        label: selectedViewMode.label
      },
      {
        before: path.join(fileDir, `${fileBase}_before_surface.opti`),
        after: path.join(fileDir, `${fileBase}_after_surface.opti`),
        label: selectedViewMode.label
      }
    ];
  }

  if (selectedViewMode.id === "optilambda.internal" || selectedViewMode.id === "optilambda.typed") {
    const representation = selectedViewMode.optilambdaRepresentation ?? "surface";
    return [
      {
        before: path.join(fileDir, `${fileBase}_before_${representation}.opti`),
        after: path.join(fileDir, `${fileBase}_after_${representation}.opti`),
        label: selectedViewMode.label
      }
    ];
  }

  return [".cpp", ".c", ".cu"].map(extension => ({
    before: path.join(fileDir, `${fileBase}_before${extension}`),
    after: path.join(fileDir, `${fileBase}_after${extension}`),
    label: selectedViewMode.label
  }));
}

async function findExistingPair(candidates: DiffFilePair[]): Promise<DiffFilePair | undefined> {
  for (const candidate of candidates) {
    if ((await exists(candidate.before)) && (await exists(candidate.after))) {
      return candidate;
    }
  }
  return undefined;
}

function sessionId(fileDir: string, fileBase: string): string {
  return path.resolve(fileDir, fileBase);
}

function diffUri(filePath: string, session: NativeDiffSession): vscode.Uri {
  const query = new URLSearchParams({
    file: filePath,
    session: session.id,
    root: session.root,
    scriptRelativePath: session.scriptRelativePath,
    line: String(session.line),
    fileDir: session.fileDir,
    fileBase: session.fileBase
  });
  return vscode.Uri.from({
    scheme: OPTITRUST_DIFF_SCHEME,
    path: `/${path.basename(filePath)}`,
    query: query.toString()
  });
}

function filePathFromUri(uri: vscode.Uri): string {
  const filePath = new URLSearchParams(uri.query).get("file");
  if (!filePath) {
    throw new Error(`Missing backing file in ${uri.toString()}`);
  }
  return filePath;
}

function sessionFromUri(uri: vscode.Uri): NativeDiffSession | undefined {
  const query = new URLSearchParams(uri.query);
  const id = query.get("session");
  const existing = id ? sessions.get(id) : undefined;
  if (existing) {
    return existing;
  }

  const fileDir = query.get("fileDir");
  const fileBase = query.get("fileBase");
  const root = query.get("root");
  const scriptRelativePath = query.get("scriptRelativePath");
  const line = Number(query.get("line"));
  if (!id || !fileDir || !fileBase || !root || !scriptRelativePath || !Number.isInteger(line)) {
    return undefined;
  }
  const restored = { id, root, scriptRelativePath, line, fileDir, fileBase, generatedModes: new Set<string>() };
  sessions.set(id, restored);
  return restored;
}

function activeNativeDiffUri(): vscode.Uri | undefined {
  const activeEditorUri = vscode.window.activeTextEditor?.document.uri;
  if (activeEditorUri?.scheme === OPTITRUST_DIFF_SCHEME) {
    return activeEditorUri;
  }

  const input = vscode.window.tabGroups.activeTabGroup.activeTab?.input;
  if (input instanceof vscode.TabInputTextDiff && input.modified.scheme === OPTITRUST_DIFF_SCHEME) {
    return input.modified;
  }
  return undefined;
}

class NativeDiffContentProvider implements vscode.TextDocumentContentProvider {
  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    return fs.readFile(filePathFromUri(uri), "utf8");
  }
}

export function registerNativeDiffProvider(context: vscode.ExtensionContext): void {
  context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider(OPTITRUST_DIFF_SCHEME, new NativeDiffContentProvider()));
}

async function generateStepDiff(session: NativeDiffSession, selectedViewMode: ViewModeDefinition): Promise<boolean> {
  const args = [
    "step_diff",
    session.scriptRelativePath,
    String(session.line),
    ...backendFlagsForViewMode(selectedViewMode)
  ];

  try {
    await runCommand({
      cwd: session.root,
      command: path.join(session.root, "tools", "view_result.sh"),
      args,
      title: `OptiTrust: Generate ${selectedViewMode.label} Diff`,
      env: {
        OPTITRUST_NO_BROWSER: "1"
      }
    });
    session.generatedModes.add(selectedViewMode.id);
    return true;
  } catch {
    return false;
  }
}

export async function openNativeStepDiff(
  context: NativeStepDiffContext,
  selectedViewMode: ViewModeDefinition,
  options: OpenNativeStepDiffOptions = {}
): Promise<void> {
  const id = sessionId(context.fileDir, context.fileBase);
  const existingSession = sessions.get(id);
  const session: NativeDiffSession =
    existingSession?.scriptRelativePath === context.scriptRelativePath && existingSession.line === context.line
      ? existingSession
      : {
          id,
          root: context.root,
          scriptRelativePath: context.scriptRelativePath,
          line: context.line,
          fileDir: context.fileDir,
          fileBase: context.fileBase,
          generatedModes: new Set<string>()
        };
  sessions.set(session.id, session);

  if (options.markGenerated) {
    session.generatedModes.add(selectedViewMode.id);
  }

  if (options.generateIfMissing && !session.generatedModes.has(selectedViewMode.id)) {
    const generated = await generateStepDiff(session, selectedViewMode);
    if (!generated) {
      return;
    }
  }

  await openExistingNativeStepDiff(session, selectedViewMode, options.viewColumn ?? vscode.ViewColumn.Beside);
}

async function openExistingNativeStepDiff(
  session: NativeDiffSession,
  selectedViewMode: ViewModeDefinition,
  viewColumn: vscode.ViewColumn
): Promise<void> {
  const candidates = stepDiffCandidates(session.fileDir, session.fileBase, selectedViewMode);
  const pair = await findExistingPair(candidates);
  if (!pair) {
    appendLine(`Generated native diff files were not found for ${session.fileBase} (${selectedViewMode.label}).`);
    for (const candidate of candidates) {
      appendLine(`Missing candidate: ${candidate.before} <-> ${candidate.after}`);
    }
    vscode.window.showWarningMessage(`OptiTrust command finished, but generated ${selectedViewMode.label} diff files were not found.`);
    return;
  }

  await vscode.commands.executeCommand(
    "vscode.diff",
    diffUri(pair.before, session),
    diffUri(pair.after, session),
    `OptiTrust Diff: ${session.fileBase} (${pair.label})`,
    { preview: false, viewColumn }
  );
}

export async function switchNativeDiffSyntax(): Promise<void> {
  const activeUri = activeNativeDiffUri();
  if (!activeUri) {
    vscode.window.showWarningMessage("Open an OptiTrust native diff before switching syntax.");
    return;
  }

  const session = sessionFromUri(activeUri);
  if (!session) {
    vscode.window.showWarningMessage("This OptiTrust diff can no longer be switched. Re-run View Step Diff.");
    return;
  }

  const picked = await vscode.window.showQuickPick(
    VIEW_MODES.map(mode => ({
      label: mode.label,
      description: mode.description,
      mode
    })),
    {
      title: "OptiTrust Diff Syntax",
      placeHolder: "Select syntax for this diff"
    }
  );

  if (!picked) {
    return;
  }

  await openNativeStepDiff(session, picked.mode, {
    viewColumn: vscode.ViewColumn.Active,
    generateIfMissing: true
  });
}
