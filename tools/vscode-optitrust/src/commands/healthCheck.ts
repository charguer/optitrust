import * as http from "http";
import * as path from "path";
import { spawn } from "child_process";
import * as vscode from "vscode";
import { appendHeader, appendLine, showOutput } from "../optitrust/output";
import { runCommand } from "../optitrust/runner";
import { OptitrustWorkspace } from "../optitrust/workspace";

async function checkCommand(
  workspace: OptitrustWorkspace,
  title: string,
  command: string,
  args: string[],
  fix: string
): Promise<boolean> {
  try {
    const result = await runCommand({
      cwd: workspace.root,
      command,
      args,
      title,
      allowFailure: true
    });
    if (result.exitCode === 0) {
      appendLine(`OK: ${command}`);
      return true;
    }
    appendLine(`FAILED: ${command} exited with code ${result.exitCode}`);
    appendLine(`Fix: ${fix}`);
    return false;
  } catch (error) {
    appendLine(`${command}: failed to start (${error instanceof Error ? error.message : String(error)})`);
    appendLine(`Fix: ${fix}`);
    return false;
  }
}

function traceServerReachable(): Promise<boolean> {
  return new Promise(resolve => {
    const request = http.get("http://localhost:6775", response => {
      response.resume();
      resolve(true);
    });
    request.on("error", () => resolve(false));
    request.setTimeout(1000, () => {
      request.destroy();
      resolve(false);
    });
  });
}

async function waitForTraceServer(timeoutMs: number): Promise<boolean> {
  const startedAt = Date.now();
  while (Date.now() - startedAt < timeoutMs) {
    if (await traceServerReachable()) {
      return true;
    }
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  return false;
}

async function tryStartTraceServer(workspace: OptitrustWorkspace): Promise<boolean> {
  appendLine("trace server: attempting to start optitrust_trace_server");
  const child = spawn("dune", ["exec", "optitrust_trace_server"], {
    cwd: workspace.root,
    detached: true,
    stdio: "ignore",
    env: process.env
  });
  child.on("error", error => appendLine(`trace server: failed to start (${error.message})`));
  // The health check only needs to verify reachability; ownership/lifecycle of
  // the trace server remains with the normal OptiTrust tooling.
  child.unref();
  return waitForTraceServer(3000);
}

export async function runHealthCheck(workspace: OptitrustWorkspace): Promise<void> {
  let ok = true;

  appendHeader("OptiTrust Health Check");
  appendLine(`root: ${workspace.root}`);
  showOutput();

  appendHeader("Workspace");
  appendLine(`root: ${workspace.root}`);

  appendHeader("Required Tools");
  ok = (await checkCommand(workspace, "OptiTrust Health: opam", "opam", ["switch", "show"], "Install opam and initialize/select the OptiTrust OCaml switch.")) && ok;
  ok = (await checkCommand(workspace, "OptiTrust Health: dune", "dune", ["--version"], "Install dune in the active opam switch.")) && ok;
  ok = (await checkCommand(workspace, "OptiTrust Health: clang", "clang", ["--version"], "Install clang 15 or make clang available in PATH.")) && ok;
  ok = (await checkCommand(workspace, "OptiTrust Health: llvm-config-15", "llvm-config-15", ["--version"], "Install LLVM 15 development tools and make llvm-config-15 available in PATH.")) && ok;

  appendHeader("OCaml / Dune Build");
  ok = (await checkCommand(
    workspace,
    "OptiTrust Health: Build runner and trace server",
    "dune",
    ["build", path.join("tools", "runner", "optitrust_runner.exe"), path.join("tools", "trace_server", "trace_server.exe")],
    "Run opam env, install missing OCaml dependencies, then rebuild OptiTrust."
  )) && ok;

  appendHeader("Optional Script Execution");
  appendLine("Skipped: no stable lightweight OptiTrust script execution check is enabled yet.");

  appendHeader("Trace Server");
  let reachable = await traceServerReachable();
  if (!reachable) {
    reachable = await tryStartTraceServer(workspace);
  }
  appendLine(reachable ? "trace server: reachable at localhost:6775" : "trace server: could not be reached or started at localhost:6775");
  ok = reachable && ok;

  if (ok) {
    vscode.window.showInformationMessage("OptiTrust health check completed successfully.");
  } else {
    vscode.window.showWarningMessage("OptiTrust health check completed with issues. See the OptiTrust output panel.");
  }
}
