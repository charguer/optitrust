import * as vscode from "vscode";

let outputChannel: vscode.OutputChannel | undefined;

export function getOutputChannel(): vscode.OutputChannel {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel("OptiTrust");
  }
  return outputChannel;
}

export function showOutput(): void {
  getOutputChannel().show(true);
}

export function appendLine(line = ""): void {
  getOutputChannel().appendLine(line);
}

export function appendHeader(title: string): void {
  const channel = getOutputChannel();
  channel.appendLine("");
  channel.appendLine(`== ${title} ==`);
}

export function disposeOutput(): void {
  outputChannel?.dispose();
  outputChannel = undefined;
}
