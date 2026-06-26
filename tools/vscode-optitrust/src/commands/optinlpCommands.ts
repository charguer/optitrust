// VS Code command handlers for OptiNLP. This file owns editor interaction
// (quick input, selection/full-file consent, insertion/opening documents), while
// core prompt/provider behavior stays in src/optinlp.
import * as vscode from "vscode";
import { loadOptiNlpAssets, inferLanguage } from "../optinlp/assets";
import { generateOptiNlp } from "../optinlp/generation";
import { modeDefinition, resolveRequestedMode } from "../optinlp/modes";
import { OptiNlpProviderError } from "../optinlp/providerErrors";
import { createOptiNlpProvider, DEFAULT_OPTINLP_PROVIDER, OptiNlpProviderId, parseOptiNlpProviderId } from "../optinlp/providerFactory";
import { OptiNlpMode, OptiNlpProviderRequest, OptiNlpProviderResult } from "../optinlp/providerTypes";
import { editorActionForResult } from "../optinlp/resultActions";
import { OptiNlpSessionMemory } from "../optinlp/sessionMemory";
import { getActiveEditorContext } from "../optitrust/editor";
import { appendHeader, appendLine, showOutput } from "../optitrust/output";
import { OptitrustWorkspace } from "../optitrust/workspace";

const GEMINI_API_KEY_SECRET = "optinlp.geminiApiKey";
const OPENAI_API_KEY_SECRET = "optinlp.openaiApiKey";

interface SourceContext {
  readonly text: string;
  readonly label: "selection" | "full file";
}

export interface OptiNlpGenerationOutcome {
  readonly mode: OptiNlpMode;
  readonly userRequest: string;
  readonly sourceLabel: SourceContext["label"];
  readonly result: OptiNlpProviderResult;
}

interface OptiNlpGenerationOptions {
  readonly renderToOutput?: boolean;
  readonly editor?: vscode.TextEditor;
  readonly throwProviderErrors?: boolean;
}

export async function setOptiNlpGeminiApiKey(context: vscode.ExtensionContext): Promise<void> {
  await setProviderApiKey(context, "Gemini", GEMINI_API_KEY_SECRET);
}

export async function setOptiNlpOpenAiApiKey(context: vscode.ExtensionContext): Promise<void> {
  await setProviderApiKey(context, "OpenAI", OPENAI_API_KEY_SECRET);
}

export async function setOptiNlpConfiguredProviderApiKey(context: vscode.ExtensionContext): Promise<void> {
  const config = vscode.workspace.getConfiguration("optitrust");
  const configuredProvider = config.get<string>("optinlpProvider", DEFAULT_OPTINLP_PROVIDER);
  const provider = parseOptiNlpProviderId(configuredProvider) ?? DEFAULT_OPTINLP_PROVIDER;
  switch (provider) {
    case "openai":
      await setOptiNlpOpenAiApiKey(context);
      return;
    case "gemini":
      await setOptiNlpGeminiApiKey(context);
      return;
    case "mock":
      vscode.window.showInformationMessage("OptiNLP mock provider does not need an API key.");
      return;
    case "ollama":
      vscode.window.showWarningMessage("OptiNLP Ollama provider is not implemented yet.");
      return;
  }
}

async function setProviderApiKey(context: vscode.ExtensionContext, providerLabel: string, secretKey: string): Promise<void> {
  const apiKey = await vscode.window.showInputBox({
    title: `OptiNLP: Set ${providerLabel} API Key`,
    prompt: `Enter the ${providerLabel} API key used by OptiNLP.`,
    password: true,
    ignoreFocusOut: true,
    validateInput: value => (value.trim().length === 0 ? "API key cannot be empty." : undefined)
  });

  if (apiKey === undefined) {
    return;
  }

  await context.secrets.store(secretKey, apiKey.trim());
  vscode.window.showInformationMessage(`OptiNLP ${providerLabel} API key saved.`);
}

export async function clearOptiNlpSession(memory: OptiNlpSessionMemory): Promise<void> {
  memory.clear();
  vscode.window.showInformationMessage("OptiNLP session cleared.");
}

export async function generateOptiNlpTarget(context: vscode.ExtensionContext, workspace: OptitrustWorkspace, memory: OptiNlpSessionMemory): Promise<void> {
  const outcome = await runModeFromInput(context, workspace, memory, "target");
  if (outcome) {
    await applyDefaultEditorAction(outcome);
  }
}

export async function generateOptiNlpScript(context: vscode.ExtensionContext, workspace: OptitrustWorkspace, memory: OptiNlpSessionMemory): Promise<void> {
  const outcome = await runModeFromInput(context, workspace, memory, "command_to_script");
  if (outcome) {
    await applyDefaultEditorAction(outcome);
  }
}

export async function generateOptiNlpFullTransformation(context: vscode.ExtensionContext, workspace: OptitrustWorkspace, memory: OptiNlpSessionMemory): Promise<void> {
  const outcome = await runModeFromInput(context, workspace, memory, "code_to_full_script");
  if (outcome) {
    await applyDefaultEditorAction(outcome);
  }
}

async function runModeFromInput(
  context: vscode.ExtensionContext,
  workspace: OptitrustWorkspace,
  memory: OptiNlpSessionMemory,
  mode: OptiNlpMode
): Promise<OptiNlpGenerationOutcome | undefined> {
  const definition = modeDefinition(mode);
  const request = await promptForRequest(mode, `OptiNLP: ${definition.label}`);
  if (!request) {
    return undefined;
  }
  return runOptiNlpGeneration(context, workspace, memory, mode, request);
}

export async function runOptiNlpGeneration(
  context: vscode.ExtensionContext,
  workspace: OptitrustWorkspace,
  memory: OptiNlpSessionMemory,
  mode: OptiNlpMode,
  userRequest: string,
  options: OptiNlpGenerationOptions = { renderToOutput: true }
): Promise<OptiNlpGenerationOutcome | undefined> {
  const resolvedMode = resolveRequestedMode(mode, userRequest);
  const editorContext = getActiveEditorContext(workspace.root, options.editor);
  const sourceContext = await getSourceContext(editorContext.editor);
  if (!sourceContext) {
    return undefined;
  }

  const assets = await loadOptiNlpAssets(workspace.root, resolvedMode);
  const request: OptiNlpProviderRequest = {
    mode: resolvedMode,
    userRequest,
    sourceText: sourceContext.text,
    filePath: editorContext.relativePath,
    language: inferLanguage(editorContext.filePath),
    promptText: assets.promptText,
    knowledgeText: assets.knowledgeText,
    sessionSummary: memory.summary()
  };

  const provider = createConfiguredProvider(context);

  let result: OptiNlpProviderResult;
  const definition = modeDefinition(resolvedMode);
  try {
    result = await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: `OptiNLP: ${definition.label}`,
        cancellable: false
      },
      () => generateOptiNlp(provider, request)
    );
  } catch (error) {
    if (error instanceof OptiNlpProviderError) {
      if (options.throwProviderErrors) {
        throw error;
      }
      appendHeader("OptiNLP Error");
      appendLine(error.userMessage);
      if (error.technicalDetail) {
        appendLine(`Detail: ${error.technicalDetail}`);
      }
      showOutput();
      vscode.window.showErrorMessage(`OptiNLP: ${error.userMessage}`);
      return undefined;
    }
    throw error;
  }

  memory.recordGeneration(request, result);
  if (options.renderToOutput ?? true) {
    renderResult(resolvedMode, userRequest, sourceContext.label, result);
  }
  vscode.window.showInformationMessage(`OptiNLP ${definition.label.toLowerCase()} complete.`);
  return {
    mode: resolvedMode,
    userRequest,
    sourceLabel: sourceContext.label,
    result
  };
}

async function getSourceContext(editor: vscode.TextEditor): Promise<SourceContext | undefined> {
  const selectedText = editor.document.getText(editor.selection);
  if (selectedText.trim().length > 0) {
    return { text: selectedText, label: "selection" };
  }

  const sendFullFile = await vscode.window.showWarningMessage(
    "OptiNLP will send the full active file to the configured AI provider because no text is selected.",
    { modal: true },
    "Send Full File"
  );
  if (sendFullFile !== "Send Full File") {
    return undefined;
  }
  return { text: editor.document.getText(), label: "full file" };
}

async function promptForRequest(mode: OptiNlpMode, title: string): Promise<string | undefined> {
  const definition = modeDefinition(mode);
  const value = await vscode.window.showInputBox({
    title,
    prompt: "Describe the target, transformation, or optimization goal.",
    placeHolder: definition.placeholder,
    ignoreFocusOut: true,
    validateInput: input => (input.trim().length === 0 ? "Request cannot be empty." : undefined)
  });
  const trimmed = value?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : undefined;
}

function renderResult(mode: OptiNlpMode, userRequest: string, sourceLabel: SourceContext["label"], result: OptiNlpProviderResult): void {
  appendHeader(`OptiNLP: ${modeDefinition(mode).label}`);
  appendLine(`provider: ${result.provider}`);
  appendLine(`model: ${result.model}`);
  appendLine(`context: ${sourceLabel}`);
  appendLine(`request: ${userRequest}`);
  appendLine("");
  appendLine(result.markdownOutput);
  showOutput();
}

export async function applyDefaultEditorAction(outcome: OptiNlpGenerationOutcome): Promise<void> {
  const action = editorActionForResult(outcome.result.structured);
  if (!action) {
    return;
  }
  switch (action.kind) {
    case "insert_target":
      await insertTextAtCursor(action.text);
      return;
    case "open_script":
      await openOcamlDocument(action.text);
      return;
  }
}

function createConfiguredProvider(context: vscode.ExtensionContext): ReturnType<typeof createOptiNlpProvider> {
  const config = vscode.workspace.getConfiguration("optitrust");
  const configuredProvider = config.get<string>("optinlpProvider", DEFAULT_OPTINLP_PROVIDER);
  const provider = parseOptiNlpProviderId(configuredProvider) ?? DEFAULT_OPTINLP_PROVIDER;
  const model = config.get<string>("optinlpModel", "").trim() || undefined;
  return createOptiNlpProvider({
    provider,
    gemini: {
      model,
      apiKeyProvider: async () => context.secrets.get(GEMINI_API_KEY_SECRET)
    },
    openai: {
      model,
      apiKeyProvider: async () => context.secrets.get(OPENAI_API_KEY_SECRET)
    },
    mock: { model }
  });
}

export async function insertTextAtCursor(text: string, sourceEditor?: vscode.TextEditor): Promise<void> {
  const editor = sourceEditor ?? vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage("OptiNLP: No active editor for insertion.");
    return;
  }
  await editor.edit(edit => {
    edit.insert(editor.selection.active, text);
  });
}

export async function openOcamlDocument(text: string): Promise<void> {
  const document = await vscode.workspace.openTextDocument({
    content: text.endsWith("\n") ? text : `${text}\n`,
    language: "ocaml"
  });
  await vscode.window.showTextDocument(document, { preview: false, viewColumn: vscode.ViewColumn.Beside });
}
