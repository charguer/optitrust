// Loads OptiNLP prompt-kit markdown from the repository and builds provider
// requests. Prompt/knowledge filenames come from the central mode registry.
import * as fs from "fs/promises";
import * as path from "path";
import { modeDefinition } from "./modes";
import { OptiNlpMode, OptiNlpProviderRequest } from "./providerTypes";

export interface OptiNlpAssets {
  readonly promptText: string;
  readonly knowledgeText: string;
}

export interface BuildOptiNlpRequestOptions {
  readonly root?: string;
  readonly mode: OptiNlpMode;
  readonly filePath: string;
  readonly userRequest: string;
  readonly sessionSummary?: string;
}

export async function buildOptiNlpProviderRequest(options: BuildOptiNlpRequestOptions): Promise<OptiNlpProviderRequest> {
  const root = options.root ? path.resolve(options.root) : await findOptiTrustRoot(process.cwd());
  const absoluteFilePath = path.resolve(options.filePath);
  const [sourceText, assets] = await Promise.all([fs.readFile(absoluteFilePath, "utf8"), loadOptiNlpAssets(root, options.mode)]);

  return {
    mode: options.mode,
    userRequest: options.userRequest,
    sourceText,
    filePath: path.relative(root, absoluteFilePath).split(path.sep).join("/"),
    language: inferLanguage(absoluteFilePath),
    promptText: assets.promptText,
    knowledgeText: assets.knowledgeText,
    sessionSummary: options.sessionSummary
  };
}

export async function loadOptiNlpAssets(root: string, mode: OptiNlpMode): Promise<OptiNlpAssets> {
  const definition = modeDefinition(mode);
  const optiNlpRoot = path.join(root, "tools", "optiNLP");
  const promptPath = path.join(optiNlpRoot, "prompts", definition.promptFile);
  const knowledgePaths = definition.knowledgeFiles.map(file => path.join(optiNlpRoot, "knowledge", file));

  const [promptText, ...knowledgeParts] = await Promise.all([
    fs.readFile(promptPath, "utf8"),
    ...knowledgePaths.map(filePath => fs.readFile(filePath, "utf8"))
  ]);

  return {
    promptText,
    knowledgeText: knowledgeParts
      .map((text, index) => `# Knowledge: ${definition.knowledgeFiles[index]}\n\n${text.trim()}`)
      .join("\n\n")
  };
}

export async function findOptiTrustRoot(startPath: string): Promise<string> {
  let current = path.resolve(startPath);
  const stat = await safeStat(current);
  if (stat?.isFile()) {
    current = path.dirname(current);
  }

  while (true) {
    if (await isOptiTrustRoot(current)) {
      return current;
    }
    const parent = path.dirname(current);
    if (parent === current) {
      throw new Error(`Could not find OptiTrust root from ${startPath}.`);
    }
    current = parent;
  }
}

export function inferLanguage(filePath: string): string {
  const ext = path.extname(filePath).toLowerCase();
  switch (ext) {
    case ".c":
      return "c";
    case ".cc":
    case ".cpp":
    case ".cxx":
    case ".hpp":
    case ".h":
      return "cpp";
    case ".ml":
      return "ocaml";
    case ".opti":
      return "optilambda";
    default:
      return ext.length > 0 ? ext.slice(1) : "text";
  }
}

async function isOptiTrustRoot(candidate: string): Promise<boolean> {
  const required = [
    path.join(candidate, "dune-project"),
    path.join(candidate, "tools", "optiNLP", "prompts"),
    path.join(candidate, "tools", "optiNLP", "knowledge")
  ];
  const checks = await Promise.all(required.map(filePath => safeStat(filePath)));
  return checks.every(Boolean);
}

async function safeStat(filePath: string): Promise<import("fs").Stats | undefined> {
  try {
    return await fs.stat(filePath);
  } catch {
    return undefined;
  }
}
