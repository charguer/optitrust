import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";

export interface OptitrustWorkspace {
  readonly root: string;
}

export interface WorkspaceDetection {
  readonly workspace?: OptitrustWorkspace;
  readonly reason?: string;
}

const REQUIRED_MARKERS = [
  "optitrust.opam",
  "tester",
  path.join("tools", "view_result.sh"),
  path.join("lib", "optitrust.ml")
];

async function exists(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

async function readText(filePath: string): Promise<string | undefined> {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch {
    return undefined;
  }
}

function parentDirectories(start: string): string[] {
  const dirs: string[] = [];
  let current = path.resolve(start);
  while (true) {
    dirs.push(current);
    const parent = path.dirname(current);
    if (parent === current) {
      return dirs;
    }
    current = parent;
  }
}

async function isOptitrustRoot(candidate: string): Promise<WorkspaceDetection> {
  const duneProject = path.join(candidate, "dune-project");
  if (!(await exists(duneProject))) {
    return { reason: `Missing ${duneProject}` };
  }

  const duneProjectText = await readText(duneProject);
  if (!duneProjectText || !/\(name\s+optitrust\)/.test(duneProjectText)) {
    return { reason: "dune-project does not declare project name optitrust" };
  }

  const missing: string[] = [];
  for (const marker of REQUIRED_MARKERS) {
    if (!(await exists(path.join(candidate, marker)))) {
      missing.push(marker);
    }
  }

  if (missing.length > 0) {
    return { reason: `Missing OptiTrust marker(s): ${missing.join(", ")}` };
  }

  return { workspace: { root: candidate } };
}

export async function findOptitrustRoot(startPath?: string): Promise<WorkspaceDetection> {
  const configuredRoot = vscode.workspace.getConfiguration("optitrust").get<string>("rootOverride", "").trim();
  if (configuredRoot.length > 0) {
    return isOptitrustRoot(configuredRoot);
  }

  const searchStarts: string[] = [];
  if (startPath) {
    searchStarts.push(path.dirname(startPath));
  }

  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    searchStarts.push(folder.uri.fsPath);
  }

  const seen = new Set<string>();
  let lastReason = "No workspace folder is open.";
  for (const start of searchStarts) {
    for (const dir of parentDirectories(start)) {
      if (seen.has(dir)) {
        continue;
      }
      seen.add(dir);
      const result = await isOptitrustRoot(dir);
      if (result.workspace) {
        return result;
      }
      lastReason = result.reason ?? lastReason;
    }
  }

  return { reason: lastReason };
}

export function relativeToRoot(root: string, filePath: string): string {
  return path.relative(root, filePath).split(path.sep).join("/");
}
