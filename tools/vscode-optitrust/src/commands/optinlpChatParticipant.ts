// Native VS Code Chat integration for OptiNLP. Voice input is intentionally
// delegated to VS Code's chat surface, where the VS Code Speech extension can
// provide microphone transcription without custom webview recording code.
import * as fs from "fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { inferLanguage } from "../optinlp/assets";
import { modeDefinition, modeFromCliCommand, resolveAutoMode } from "../optinlp/modes";
import { OptiNlpMode } from "../optinlp/providerTypes";
import { OptiNlpProviderError } from "../optinlp/providerErrors";
import { editorActionForResult, targetSuggestionsFromMarkdown } from "../optinlp/resultActions";
import { OptiNlpStructuredResult } from "../optinlp/resultSchemas";
import { OptiNlpSessionMemory } from "../optinlp/sessionMemory";
import { findAssociatedCSourceFile, findAssociatedFiles } from "../optitrust/files";
import { OptitrustWorkspace, relativeToRoot } from "../optitrust/workspace";
import {
  clearOptiNlpSession,
  optiNlpConfigurationSummary,
  runOptiNlpGeneration,
  SourceContext,
  sourceContextFromEditor
} from "./optinlpCommands";
import { takePendingOptiNlpChatRequest } from "./optinlpChatContext";

type WorkspaceProvider = () => Promise<OptitrustWorkspace | undefined>;

const CHAT_PARTICIPANT_ID = "optitrust.optinlp";
const COMMAND_INSERT_TARGET = "optitrust.optinlpInsertTarget";
const COMMAND_OPEN_SCRIPT = "optitrust.optinlpOpenScript";
const COMMAND_SELECT_PROVIDER = "optitrust.optinlpSelectProvider";
const COMMAND_SET_MODEL = "optitrust.optinlpSetModel";
const COMMAND_SET_API_KEY = "optitrust.optinlpSetConfiguredApiKey";

interface ChatSourceContext {
  readonly sourceContext: SourceContext;
  readonly filePath: string;
  readonly language: string;
  readonly targetInsertionFilePath?: string;
}

export function registerOptiNlpChatParticipant(
  context: vscode.ExtensionContext,
  getWorkspace: WorkspaceProvider,
  memory: OptiNlpSessionMemory
): void {
  const handler: vscode.ChatRequestHandler = async (request, _chatContext, stream, token) => {
    const prompt = request.prompt.trim();
    if (await handleUtilityCommand(request.command, memory, stream)) {
      return {};
    }

    if (!prompt) {
      stream.markdown("Describe the OptiTrust target or transformation you want, or use `/help`.");
      return {};
    }

    const workspace = await getWorkspace();
    if (!workspace) {
      stream.markdown("OptiTrust workspace not detected.");
      return {};
    }

    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.uri.scheme !== "file") {
      stream.markdown("Open an OptiTrust source or script file before asking OptiNLP.");
      return {};
    }

    const mode = modeForChatRequest(request.command, prompt);
    stream.progress(`OptiNLP: ${modeDefinition(mode).label}`);
    const pending = takePendingOptiNlpChatRequest(mode, prompt);
    const source = pending ?? (await sourceContextForChatRequest(workspace, editor, mode));
    if (!source) {
      stream.markdown(`No matching C/C++ source file found for \`${path.basename(editor.document.uri.fsPath)}\`.`);
      return {};
    }

    try {
      const outcome = await runOptiNlpGeneration(context, workspace, memory, mode, pending?.userRequest ?? prompt, {
        renderToOutput: false,
        editor,
        throwProviderErrors: true,
        sourceContext: source.sourceContext,
        filePath: source.filePath,
        language: source.language,
        cancellationToken: token
      });
      if (!outcome) {
        stream.markdown("OptiNLP did not produce a result.");
        return {};
      }

      stream.markdown(outcome.result.markdownOutput);
      renderActionButtons(stream, outcome.mode, outcome.result.structured, outcome.result.markdownOutput, source.targetInsertionFilePath);
      return {};
    } catch (error) {
      if (error instanceof OptiNlpProviderError) {
        stream.markdown(`**OptiNLP Error**\n\n${error.userMessage}`);
        if (error.technicalDetail) {
          stream.markdown(`\n\n\`\`\`text\n${error.technicalDetail}\n\`\`\``);
        }
        renderConfigurationButtons(stream);
        return {};
      }
      throw error;
    }
  };

  const participant = vscode.chat.createChatParticipant(CHAT_PARTICIPANT_ID, handler);
  participant.iconPath = new vscode.ThemeIcon("sparkle");
  participant.followupProvider = {
    provideFollowups: () => [
      { label: "Generate target", prompt: "target the loop i", command: "target" },
      { label: "Generate script", prompt: "unroll the loop i", command: "script" },
      { label: "Show config", prompt: "show configuration", command: "config" }
    ]
  };
  context.subscriptions.push(participant);
}

async function handleUtilityCommand(
  command: string | undefined,
  memory: OptiNlpSessionMemory,
  stream: vscode.ChatResponseStream
): Promise<boolean> {
  switch (command) {
    case "config":
      renderConfig(stream);
      return true;
    case "clear":
      await clearOptiNlpSession(memory);
      stream.markdown("OptiNLP session memory cleared.");
      return true;
    case "help":
      renderHelp(stream);
      return true;
    default:
      return false;
  }
}

function renderHelp(stream: vscode.ChatResponseStream): void {
  stream.markdown([
    "## OptiNLP Help",
    "",
    "Use `@optinlp` with one of these commands:",
    "",
    "- `/target`: generate robust OptiTrust targets.",
    "- `/script`: generate an OptiTrust transformation script from a command.",
    "- `/full`: generate a complete transformation script for the active file.",
    "- `/config`: show provider/model configuration.",
    "- `/clear`: clear OptiNLP session memory.",
    "",
    "Examples:",
    "",
    "```text",
    "@optinlp /target target the second loop named i",
    "@optinlp /script unroll the loop i",
    "@optinlp /full generate a full transformation script for this file",
    "```",
    "",
    "When `/target` runs from an active `.ml` script, OptiNLP uses the matching same-basename C/C++ source file as context.",
    "",
    "Source files are sent fresh each turn. Stable OptiNLP prompt/knowledge/eval files are sent once per stateful provider session when supported, and resent for stateless providers.",
    "",
    "Voice input is provided by native VS Code Chat through VS Code Speech."
  ].join("\n"));
  renderConfigurationButtons(stream);
}

function renderConfig(stream: vscode.ChatResponseStream): void {
  const config = optiNlpConfigurationSummary();
  stream.markdown([
    "## OptiNLP Configuration",
    "",
    `- Provider: \`${config.provider}\``,
    `- Model: ${config.model.length > 0 ? `\`${config.model}\`` : "provider default"}`,
    `- Provider session memory: ${config.useProviderSession ? "enabled" : "disabled"}`
  ].join("\n"));
  renderConfigurationButtons(stream);
}

function renderActionButtons(
  stream: vscode.ChatResponseStream,
  mode: OptiNlpMode,
  result: OptiNlpStructuredResult | undefined,
  markdownOutput: string,
  targetInsertionFilePath?: string
): void {
  if (!result) {
    if (mode === "target") {
      renderFallbackTargetButtons(stream, markdownOutput, targetInsertionFilePath);
    }
    return;
  }

  if (result.kind === "target") {
    const suggestions = targetSuggestions(result);
    const targets = suggestions.length > 0 ? suggestions.map(suggestion => suggestion.target) : targetSuggestionsFromMarkdown(markdownOutput);
    for (const target of targets) {
      stream.button({
        title: target,
        command: COMMAND_INSERT_TARGET,
        arguments: [target, targetInsertionFilePath]
      });
    }
    return;
  }

  const action = editorActionForResult(result);
  if (action?.kind === "open_script") {
    stream.button({
      title: "Open Script",
      command: COMMAND_OPEN_SCRIPT,
      arguments: [action.text]
    });
  }
}

function renderFallbackTargetButtons(stream: vscode.ChatResponseStream, markdownOutput: string, targetInsertionFilePath?: string): void {
  for (const target of targetSuggestionsFromMarkdown(markdownOutput)) {
    stream.button({
      title: target,
      command: COMMAND_INSERT_TARGET,
      arguments: [target, targetInsertionFilePath]
    });
  }
}

function renderConfigurationButtons(stream: vscode.ChatResponseStream): void {
  stream.button({ title: "Select Provider", command: COMMAND_SELECT_PROVIDER });
  stream.button({ title: "Set Model", command: COMMAND_SET_MODEL });
  stream.button({ title: "Set API Key", command: COMMAND_SET_API_KEY });
}

function targetSuggestions(result: Extract<OptiNlpStructuredResult, { readonly kind: "target" }>): { readonly target: string }[] {
  const seen = new Set<string>();
  const suggestions: { readonly target: string }[] = [];
  const add = (target: string | undefined): void => {
    const trimmed = target?.trim();
    if (!trimmed || seen.has(trimmed)) {
      return;
    }
    seen.add(trimmed);
    suggestions.push({ target: trimmed });
  };
  add(result.recommendedTarget);
  result.alternatives.forEach(add);
  return suggestions;
}

function modeForChatRequest(command: string | undefined, prompt: string): OptiNlpMode {
  return command ? modeFromCliCommand(command) ?? resolveAutoMode("auto", prompt) : resolveAutoMode("auto", prompt);
}

async function sourceContextForChatRequest(
  workspace: OptitrustWorkspace,
  editor: vscode.TextEditor,
  mode: OptiNlpMode
): Promise<ChatSourceContext | undefined> {
  const activePath = editor.document.uri.fsPath;
  if (mode === "target" && path.extname(activePath) === ".ml") {
    const source = await findAssociatedCSourceFile(activePath);
    if (!source) {
      return undefined;
    }
    return {
      sourceContext: {
        text: await fs.readFile(source.path, "utf8"),
        label: "associated source"
      },
      filePath: relativeToRoot(workspace.root, source.path),
      language: inferLanguage(source.path),
      targetInsertionFilePath: activePath
    };
  }

  return {
    sourceContext: sourceContextFromEditor(editor),
    filePath: relativeToRoot(workspace.root, activePath),
    language: inferLanguage(activePath),
    targetInsertionFilePath: mode === "target" ? await targetInsertionPathForActiveFile(activePath) : undefined
  };
}

async function targetInsertionPathForActiveFile(activePath: string): Promise<string | undefined> {
  if (path.extname(activePath) === ".ml") {
    return activePath;
  }
  if (path.extname(activePath) !== ".opti") {
    return undefined;
  }
  const files = await findAssociatedFiles(activePath);
  return files.find(file => file.kind === "script")?.path;
}
