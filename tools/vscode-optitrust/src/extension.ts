import * as vscode from "vscode";
import {
  compareOutputExpected,
  openAssociatedFiles,
  openExpectedOutput,
  openGeneratedOutput,
  openUnitTestMlCppFiles
} from "./commands/associatedFiles";
import { runHealthCheck } from "./commands/healthCheck";
import {
  clearOptiNlpSession,
  generateOptiNlpScript,
  generateOptiNlpTarget,
  setOptiNlpOpenAiApiKey,
  setOptiNlpGeminiApiKey,
  suggestOptiNlpCandidateScript
} from "./commands/optinlpCommands";
import { OptiNlpPanel } from "./commands/optinlpPanel";
import { rerunLastTests, runCurrentTest, runCurrentTestAndOpenDiff } from "./commands/runTests";
import {
  redoLastViewCommand,
  runViewCommand,
  runViewDiffInternalSyntax,
  runViewDiffOnlyCode,
  runViewTraceSaveStepsScript
} from "./commands/viewCommands";
import { disposeDecorations, updateDecorations } from "./optitrust/decorations";
import { appendLine, disposeOutput } from "./optitrust/output";
import { getSelectedViewMode, updateSelectedViewMode, VIEW_MODES } from "./optitrust/viewMode";
import { findOptitrustRoot, OptitrustWorkspace } from "./optitrust/workspace";
import { OptiNlpSessionMemory } from "./optinlp/sessionMemory";

let currentWorkspace: OptitrustWorkspace | undefined;
let warnedUnsupportedWorkspace = false;
let optiNlpSession: OptiNlpSessionMemory | undefined;

async function refreshWorkspace(startPath?: string): Promise<OptitrustWorkspace | undefined> {
  const detection = await findOptitrustRoot(startPath);
  currentWorkspace = detection.workspace;

  if (!currentWorkspace && !warnedUnsupportedWorkspace) {
    warnedUnsupportedWorkspace = true;
    vscode.window.showWarningMessage(
      `OptiTrust workspace not detected. This extension currently supports in-tree OptiTrust development only. ${detection.reason ?? ""}`.trim()
    );
  }

  if (currentWorkspace) {
    appendLine(`OptiTrust root detected: ${currentWorkspace.root}`);
  }

  return currentWorkspace;
}

async function requireWorkspace(): Promise<OptitrustWorkspace | undefined> {
  const activeFile = vscode.window.activeTextEditor?.document.uri.scheme === "file" ? vscode.window.activeTextEditor.document.uri.fsPath : undefined;
  const workspace = currentWorkspace ?? (await refreshWorkspace(activeFile));
  if (!workspace) {
    vscode.window.showWarningMessage("OptiTrust workspace not detected.");
  }
  return workspace;
}

function registerCommand(context: vscode.ExtensionContext, command: string, callback: () => Promise<void> | void): void {
  context.subscriptions.push(
    vscode.commands.registerCommand(command, async () => {
      try {
        await callback();
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showErrorMessage(`OptiTrust: ${message}`);
      }
    })
  );
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  await refreshWorkspace(vscode.window.activeTextEditor?.document.uri.fsPath);
  optiNlpSession = new OptiNlpSessionMemory();

  registerCommand(context, "optitrust.hello", async () => {
    const workspace = await requireWorkspace();
    vscode.window.showInformationMessage(workspace ? `OptiTrust extension loaded: ${workspace.root}` : "OptiTrust extension loaded.");
  });

  registerCommand(context, "optitrust.viewDiff", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewCommand(workspace, "step_diff");
  });

  registerCommand(context, "optitrust.viewFullTrace", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewCommand(workspace, "full_trace");
  });

  registerCommand(context, "optitrust.viewTraceSaveStepsScript", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewTraceSaveStepsScript(workspace);
  });

  registerCommand(context, "optitrust.viewStepTrace", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewCommand(workspace, "step_trace");
  });

  registerCommand(context, "optitrust.viewDiffOnlyCode", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewDiffOnlyCode(workspace);
  });

  registerCommand(context, "optitrust.viewDiffInternalSyntax", async () => {
    const workspace = await requireWorkspace();
    if (!workspace) {
      return;
    }
    await runViewDiffInternalSyntax(workspace);
  });

  registerCommand(context, "optitrust.redoLastViewCommand", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await redoLastViewCommand(workspace);
    }
  });

  registerCommand(context, "optitrust.runCurrentTest", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await runCurrentTest(workspace);
    }
  });

  registerCommand(context, "optitrust.rerunLastTests", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await rerunLastTests(workspace);
    }
  });

  registerCommand(context, "optitrust.runCurrentTestAndOpenDiff", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await runCurrentTestAndOpenDiff(workspace);
    }
  });

  registerCommand(context, "optitrust.openGeneratedOutput", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await openGeneratedOutput(workspace);
    }
  });

  registerCommand(context, "optitrust.openExpectedOutput", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await openExpectedOutput(workspace);
    }
  });

  registerCommand(context, "optitrust.compareOutputExpected", compareOutputExpected);

  registerCommand(context, "optitrust.openAssociatedFiles", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await openAssociatedFiles(workspace);
    }
  });

  registerCommand(context, "optitrust.openUnitTestMlCppFiles", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await openUnitTestMlCppFiles(workspace);
    }
  });

  registerCommand(context, "optitrust.selectViewSyntax", async () => {
    const selected = getSelectedViewMode();
    const picked = await vscode.window.showQuickPick(
      VIEW_MODES.map(mode => ({
        label: mode.label,
        description: mode.description,
        picked: mode.id === selected.id,
        mode
      })),
      { placeHolder: "Select OptiTrust diff/trace syntax" }
    );
    if (!picked) {
      return;
    }
    await updateSelectedViewMode(picked.mode);
    vscode.window.showInformationMessage(`OptiTrust diff/trace syntax set to ${picked.label}.`);
  });

  registerCommand(context, "optitrust.healthCheck", async () => {
    const workspace = await requireWorkspace();
    if (workspace) {
      await runHealthCheck(workspace);
    }
  });

  registerCommand(context, "optitrust.optinlpChat", async () => {
    const workspace = await requireWorkspace();
    if (workspace && optiNlpSession) {
      OptiNlpPanel.show(context, workspace, optiNlpSession);
    }
  });

  registerCommand(context, "optitrust.optinlpGenerateTarget", async () => {
    const workspace = await requireWorkspace();
    if (workspace && optiNlpSession) {
      await generateOptiNlpTarget(context, workspace, optiNlpSession);
    }
  });

  registerCommand(context, "optitrust.optinlpGenerateScript", async () => {
    const workspace = await requireWorkspace();
    if (workspace && optiNlpSession) {
      await generateOptiNlpScript(context, workspace, optiNlpSession);
    }
  });

  registerCommand(context, "optitrust.optinlpSuggestCandidateScript", async () => {
    const workspace = await requireWorkspace();
    if (workspace && optiNlpSession) {
      await suggestOptiNlpCandidateScript(context, workspace, optiNlpSession);
    }
  });

  registerCommand(context, "optitrust.optinlpSetGeminiApiKey", async () => {
    await setOptiNlpGeminiApiKey(context);
  });

  registerCommand(context, "optitrust.optinlpSetOpenAiApiKey", async () => {
    await setOptiNlpOpenAiApiKey(context);
  });

  registerCommand(context, "optitrust.optinlpClearSession", async () => {
    if (optiNlpSession) {
      await clearOptiNlpSession(optiNlpSession);
    }
  });

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(editor => updateDecorations(editor)),
    vscode.workspace.onDidChangeTextDocument(event => {
      if (event.document === vscode.window.activeTextEditor?.document) {
        updateDecorations(vscode.window.activeTextEditor);
      }
    }),
    vscode.workspace.onDidChangeWorkspaceFolders(() => {
      currentWorkspace = undefined;
      warnedUnsupportedWorkspace = false;
      void refreshWorkspace(vscode.window.activeTextEditor?.document.uri.fsPath);
    }),
    {
      dispose: () => {
        disposeDecorations();
        disposeOutput();
      }
    }
  );

  updateDecorations();
}

export function deactivate(): void {
  disposeDecorations();
  disposeOutput();
  optiNlpSession = undefined;
}
