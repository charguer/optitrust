import * as vscode from "vscode";

const executedLineDecoration = vscode.window.createTextEditorDecorationType({
  isWholeLine: true,
  backgroundColor: new vscode.ThemeColor("editor.findMatchHighlightBackground"),
  overviewRulerColor: new vscode.ThemeColor("editor.findMatchHighlightBackground"),
  overviewRulerLane: vscode.OverviewRulerLane.Left
});

const transformationLineDecoration = vscode.window.createTextEditorDecorationType({
  isWholeLine: true,
  borderWidth: "0 0 0 2px",
  borderStyle: "solid",
  borderColor: new vscode.ThemeColor("editorGutter.modifiedBackground")
});

let lastExecuted: { uri: string; line: number } | undefined;

export function markExecutedLine(editor: vscode.TextEditor, line: number): void {
  lastExecuted = { uri: editor.document.uri.toString(), line };
  updateDecorations(editor);
}

export function updateDecorations(editor: vscode.TextEditor | undefined = vscode.window.activeTextEditor): void {
  if (!editor || editor.document.languageId !== "ocaml") {
    return;
  }

  const transformationRanges: vscode.Range[] = [];
  for (let index = 0; index < editor.document.lineCount; index += 1) {
    if (/^\s*!!/.test(editor.document.lineAt(index).text)) {
      transformationRanges.push(editor.document.lineAt(index).range);
    }
  }
  editor.setDecorations(transformationLineDecoration, transformationRanges);

  if (lastExecuted?.uri === editor.document.uri.toString()) {
    const line = Math.max(0, Math.min(lastExecuted.line - 1, editor.document.lineCount - 1));
    editor.setDecorations(executedLineDecoration, [editor.document.lineAt(line).range]);
  } else {
    editor.setDecorations(executedLineDecoration, []);
  }
}

export function disposeDecorations(): void {
  executedLineDecoration.dispose();
  transformationLineDecoration.dispose();
}
