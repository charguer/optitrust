// Shortcut help command. The list mirrors the default keybinding table in the
// VS Code extension README so users can discover the built-in workflow keys
// without opening documentation.
import * as vscode from "vscode";

interface ShortcutItem extends vscode.QuickPickItem {
  readonly command: string;
}

const SHORTCUTS: readonly ShortcutItem[] = [
  {
    label: "F6",
    description: "View step diff",
    detail: "OptiTrust: View Step Diff",
    command: "optitrust.viewDiff"
  },
  {
    label: "Ctrl+F6",
    description: "View diff only code",
    detail: "OptiTrust: View Diff Only Code",
    command: "optitrust.viewDiffOnlyCode"
  },
  {
    label: "Ctrl+Shift+F6",
    description: "View diff using internal syntax",
    detail: "OptiTrust: View Diff Using Internal Syntax",
    command: "optitrust.viewDiffInternalSyntax"
  },
  {
    label: "Shift+F5",
    description: "View full trace",
    detail: "OptiTrust: View Full Trace",
    command: "optitrust.viewFullTrace"
  },
  {
    label: "Ctrl+F5",
    description: "View trace with -save-steps script",
    detail: "OptiTrust: View Trace Save Steps Script",
    command: "optitrust.viewTraceSaveStepsScript"
  },
  {
    label: "Shift+F6",
    description: "View step trace",
    detail: "OptiTrust: View Step Trace",
    command: "optitrust.viewStepTrace"
  },
  {
    label: "F5",
    description: "Redo last view command",
    detail: "OptiTrust: Redo Last View Command",
    command: "optitrust.redoLastViewCommand"
  },
  {
    label: "F7",
    description: "OptiNLP suggest target at cursor",
    detail: "OptiTrust: OptiNLP Suggest Target At Cursor",
    command: "optitrust.optinlpSuggestTargetAtCursor"
  },
  {
    label: "F10",
    description: "Rerun last-tried tests",
    detail: "OptiTrust: Rerun Last-Tried Tests",
    command: "optitrust.rerunLastTests"
  },
  {
    label: "Ctrl+F10",
    description: "Run current test",
    detail: "OptiTrust: Run Current Test",
    command: "optitrust.runCurrentTest"
  },
  {
    label: "Ctrl+Shift+F10",
    description: "Run current test and open diff",
    detail: "OptiTrust: Run Current Test And Open Diff",
    command: "optitrust.runCurrentTestAndOpenDiff"
  },
  {
    label: "Alt+Shift+F10",
    description: "Open unit test ML and CPP files",
    detail: "OptiTrust: Open Unit Test ML And CPP Files",
    command: "optitrust.openUnitTestMlCppFiles"
  }
];

export async function showShortcuts(): Promise<void> {
  const picked = await vscode.window.showQuickPick(SHORTCUTS, {
    title: "OptiTrust Shortcuts",
    placeHolder: "Select a shortcut to run its command",
    matchOnDescription: true,
    matchOnDetail: true
  });

  if (picked) {
    await vscode.commands.executeCommand(picked.command);
  }
}
