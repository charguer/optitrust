import { spawn } from "child_process";
import * as vscode from "vscode";
import { appendHeader, appendLine, showOutput } from "./output";

export interface CommandResult {
  readonly command: string;
  readonly cwd: string;
  readonly exitCode: number | null;
  readonly stdout: string;
  readonly stderr: string;
}

export interface RunOptions {
  readonly cwd: string;
  readonly command: string;
  readonly args?: readonly string[];
  readonly title: string;
  readonly shell?: boolean;
  readonly allowFailure?: boolean;
  readonly env?: NodeJS.ProcessEnv;
}

function shellQuote(value: string): string {
  if (/^[A-Za-z0-9_./:=+-]+$/.test(value)) {
    return value;
  }
  return `'${value.replace(/'/g, "'\\''")}'`;
}

export function formatCommand(command: string, args: readonly string[] = []): string {
  return [command, ...args].map(shellQuote).join(" ");
}

export async function runCommand(options: RunOptions): Promise<CommandResult> {
  const args = [...(options.args ?? [])];
  const renderedCommand = formatCommand(options.command, args);
  const startedAt = Date.now();
  let stdout = "";
  let stderr = "";

  appendHeader(options.title);
  appendLine(`cwd: ${options.cwd}`);
  appendLine(`command: ${renderedCommand}`);
  showOutput();

  return vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: options.title,
      cancellable: false
    },
    () =>
      new Promise<CommandResult>((resolve, reject) => {
        const child = spawn(options.command, args, {
          cwd: options.cwd,
          shell: options.shell ?? false,
          env: { ...process.env, ...(options.env ?? {}) }
        });

        child.stdout?.on("data", (chunk: Buffer) => {
          const text = chunk.toString();
          stdout += text;
          appendLine(text.trimEnd());
        });

        child.stderr?.on("data", (chunk: Buffer) => {
          const text = chunk.toString();
          stderr += text;
          appendLine(text.trimEnd());
        });

        child.on("error", error => {
          appendLine(`failed to start: ${error.message}`);
          vscode.window.showErrorMessage(`OptiTrust command failed to start: ${error.message}`);
          reject(error);
        });

        child.on("close", exitCode => {
          const duration = ((Date.now() - startedAt) / 1000).toFixed(1);
          appendLine(`exit code: ${exitCode}`);
          appendLine(`duration: ${duration}s`);

          const result: CommandResult = {
            command: renderedCommand,
            cwd: options.cwd,
            exitCode,
            stdout,
            stderr
          };

          if (exitCode === 0 || options.allowFailure) {
            resolve(result);
            return;
          }

          const message = `OptiTrust command failed with exit code ${exitCode}: ${renderedCommand}`;
          vscode.window.showErrorMessage(message);
          reject(new Error(message));
        });
      })
  );
}
