// Webview side panel for OptiNLP. The panel is intentionally presentation-only:
// generation, provider selection, and editor actions live in shared modules.
import * as path from "path";
import * as vscode from "vscode";
import {
  clearOptiNlpSession,
  insertTargetAtCursor,
  openOcamlDocument,
  OptiNlpGenerationOutcome,
  runOptiNlpGeneration,
  setOptiNlpConfiguredProviderApiKey
} from "./optinlpCommands";
import { inferLanguage } from "../optinlp/assets";
import { modeDefinition, OPTINLP_MODE_DEFINITIONS, OptiNlpUiMode, resolveAutoMode } from "../optinlp/modes";
import { OptiNlpMode } from "../optinlp/providerTypes";
import { OptiNlpProviderError } from "../optinlp/providerErrors";
import { editorActionForResult } from "../optinlp/resultActions";
import { OptiNlpStructuredResult } from "../optinlp/resultSchemas";
import { OptiNlpSessionMemory } from "../optinlp/sessionMemory";
import { OptitrustWorkspace, relativeToRoot } from "../optitrust/workspace";

interface WebviewMessage {
  readonly type: string;
  readonly mode?: OptiNlpUiMode;
  readonly request?: string;
  readonly text?: string;
  readonly resultId?: string;
  readonly target?: string;
}

interface TargetSuggestion {
  readonly label: string;
  readonly target: string;
}

export class OptiNlpPanel {
  private static current: OptiNlpPanel | undefined;
  private readonly panel: vscode.WebviewPanel;
  private resultSeq = 0;
  private readonly results = new Map<string, OptiNlpStructuredResult>();
  private sourceEditor: vscode.TextEditor | undefined;
  private sourceEditorLocked = false;
  private disposables: vscode.Disposable[] = [];

  private constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly workspace: OptitrustWorkspace,
    private readonly memory: OptiNlpSessionMemory,
    panel: vscode.WebviewPanel
  ) {
    this.panel = panel;
    this.sourceEditor = asFileTextEditor(vscode.window.activeTextEditor);
    this.panel.webview.html = renderPanelHtml();
    this.panel.onDidDispose(() => this.dispose(), undefined, this.disposables);
    this.panel.webview.onDidReceiveMessage(message => this.handleMessage(message as WebviewMessage), undefined, this.disposables);
    vscode.window.onDidChangeActiveTextEditor(editor => {
      const fileEditor = asFileTextEditor(editor);
      if (fileEditor && !this.sourceEditorLocked) {
        this.sourceEditor = fileEditor;
      }
      void this.refreshContext();
    }, undefined, this.disposables);
  }

  static show(context: vscode.ExtensionContext, workspace: OptitrustWorkspace, memory: OptiNlpSessionMemory): OptiNlpPanel {
    if (OptiNlpPanel.current) {
      OptiNlpPanel.current.panel.reveal(vscode.ViewColumn.Beside);
      void OptiNlpPanel.current.refreshContext();
      return OptiNlpPanel.current;
    }

    const panel = vscode.window.createWebviewPanel("optitrustOptiNlp", "OptiNLP", vscode.ViewColumn.Beside, {
      enableScripts: true,
      retainContextWhenHidden: true
    });
    OptiNlpPanel.current = new OptiNlpPanel(context, workspace, memory, panel);
    void OptiNlpPanel.current.refreshContext();
    return OptiNlpPanel.current;
  }

  setSourceEditor(editor: vscode.TextEditor, locked = false): void {
    this.sourceEditor = editor;
    this.sourceEditorLocked = locked;
    void this.refreshContext();
  }

  postGenerationOutcome(outcome: OptiNlpGenerationOutcome): void {
    this.postOutcome(outcome);
  }

  postUserRequest(mode: OptiNlpUiMode, request: string, meta?: string): void {
    this.post({
      type: "externalRequest",
      mode,
      request,
      meta
    });
  }

  private async handleMessage(message: WebviewMessage): Promise<void> {
    switch (message.type) {
      case "ready":
        await this.refreshContext();
        return;
      case "generate":
        await this.generate(message.mode ?? "auto", message.request ?? "");
        return;
      case "setApiKey":
        await setOptiNlpConfiguredProviderApiKey(this.context);
        return;
      case "clearSession":
        await clearOptiNlpSession(this.memory);
        this.post({ type: "sessionCleared" });
        return;
      case "copy":
        if (message.text) {
          await vscode.env.clipboard.writeText(message.text);
          this.post({ type: "copied" });
        }
        return;
      case "insertTarget":
        await this.insertTarget(message.resultId, message.target);
        return;
      case "openScript":
        await this.openScript(message.resultId);
        return;
    }
  }

  private async generate(mode: OptiNlpUiMode, request: string): Promise<void> {
    const trimmed = request.trim();
    if (trimmed.length === 0) {
      this.post({ type: "error", message: "Request cannot be empty." });
      return;
    }

    this.sourceEditorLocked = false;
    const resolvedMode = resolveAutoMode(mode, trimmed);
    this.post({ type: "busy", busy: true });
    try {
      const outcome = await runOptiNlpGeneration(this.context, this.workspace, this.memory, resolvedMode, trimmed, {
        renderToOutput: false,
        editor: this.getSourceEditor(),
        throwProviderErrors: true
      });
      if (!outcome) {
        this.post({ type: "busy", busy: false });
        return;
      }
      this.postOutcome(outcome);
      await this.refreshContext();
    } catch (error) {
      const message =
        error instanceof OptiNlpProviderError && error.technicalDetail
          ? `${error.userMessage}\n\n${error.technicalDetail}`
          : error instanceof Error
            ? error.message
            : String(error);
      this.post({ type: "error", message });
    } finally {
      this.post({ type: "busy", busy: false });
    }
  }

  private async insertTarget(resultId: string | undefined, target: string | undefined): Promise<void> {
    if (target && target.trim().length > 0) {
      await insertTargetAtCursor(target, this.getSourceEditor());
      this.post({ type: "inserted" });
      return;
    }

    const result = resultId ? this.results.get(resultId) : undefined;
    if (!result || result.kind !== "target" || !result.recommendedTarget) {
      this.post({ type: "error", message: "No target expression is available for insertion." });
      return;
    }
    await insertTargetAtCursor(result.recommendedTarget, this.getSourceEditor());
    this.post({ type: "inserted" });
  }

  private async openScript(resultId: string | undefined): Promise<void> {
    const result = resultId ? this.results.get(resultId) : undefined;
    const action = editorActionForResult(result);
    if (!action) {
      this.post({ type: "error", message: "No generated script is available." });
      return;
    }
    if (action.kind === "open_script") {
      await openOcamlDocument(action.text);
      return;
    }
    this.post({ type: "error", message: "This result does not contain a generated script." });
  }

  private storeStructuredResult(result: OptiNlpStructuredResult | undefined): string | undefined {
    if (!result) {
      return undefined;
    }
    const id = String(++this.resultSeq);
    this.results.set(id, result);
    return id;
  }

  private postOutcome(outcome: OptiNlpGenerationOutcome): void {
    const resultId = this.storeStructuredResult(outcome.result.structured);
    this.post({
      type: "result",
      resultId,
      mode: outcome.mode,
      request: outcome.userRequest,
      sourceLabel: outcome.sourceLabel,
      provider: outcome.result.provider,
      model: outcome.result.model,
      markdown: outcome.result.markdownOutput,
      structured: outcome.result.structured,
      targetSuggestions: targetSuggestions(outcome.result.structured)
    });
  }

  private async refreshContext(): Promise<void> {
    const editor = this.getSourceEditor();
    if (!editor) {
      this.post({ type: "context", label: "No active file" });
      return;
    }
    const filePath = editor.document.uri.fsPath;
    const selectedText = editor.document.getText(editor.selection);
    const relativePath = relativeToRoot(this.workspace.root, filePath);
    const selectionLabel = selectedText.trim().length > 0 ? "selection" : "full file";
    this.post({
      type: "context",
      label: `${relativePath} · ${selectionLabel} · ${inferLanguage(filePath)}`,
      fileName: path.basename(filePath),
      hasSelection: selectedText.trim().length > 0
    });
  }

  private getSourceEditor(): vscode.TextEditor | undefined {
    if (this.sourceEditorLocked && this.sourceEditor) {
      return this.sourceEditor;
    }
    const activeEditor = asFileTextEditor(vscode.window.activeTextEditor);
    if (activeEditor) {
      this.sourceEditor = activeEditor;
      return activeEditor;
    }
    return this.sourceEditor;
  }

  private post(message: unknown): void {
    void this.panel.webview.postMessage(message);
  }

  private dispose(): void {
    OptiNlpPanel.current = undefined;
    for (const disposable of this.disposables) {
      disposable.dispose();
    }
    this.disposables = [];
  }
}

function targetSuggestions(result: OptiNlpStructuredResult | undefined): TargetSuggestion[] {
  if (!result || result.kind !== "target") {
    return [];
  }

  const seen = new Set<string>();
  const suggestions: TargetSuggestion[] = [];
  const add = (label: string, target: string | undefined): void => {
    const trimmed = target?.trim();
    if (!trimmed || seen.has(trimmed)) {
      return;
    }
    seen.add(trimmed);
    suggestions.push({ label, target: trimmed });
  };

  add("Recommended", result.recommendedTarget);
  result.alternatives.forEach((target, index) => add(`Alternative ${index + 1}`, target));
  return suggestions;
}

function asFileTextEditor(editor: vscode.TextEditor | undefined): vscode.TextEditor | undefined {
  return editor?.document.uri.scheme === "file" ? editor : undefined;
}

function renderPanelHtml(): string {
  const nonce = createNonce();
  const modeOptions = [
    '<option value="auto">Auto</option>',
    ...OPTINLP_MODE_DEFINITIONS.map(definition => `<option value="${escapeHtml(definition.id)}">${escapeHtml(definition.shortLabel)}</option>`)
  ].join("");
  const defaultPlaceholder = modeDefinition("target").placeholder;
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'nonce-${nonce}'; script-src 'nonce-${nonce}';">
  <title>OptiNLP</title>
  <style nonce="${nonce}">
    :root {
      color-scheme: light dark;
      font-family: var(--vscode-font-family);
      font-size: var(--vscode-font-size);
    }
    body {
      margin: 0;
      color: var(--vscode-foreground);
      background: var(--vscode-editor-background);
    }
    .root {
      display: grid;
      grid-template-rows: auto 1fr auto;
      height: 100vh;
      min-width: 0;
      gap: 10px;
    }
    .toolbar {
      display: grid;
      grid-template-columns: 1fr auto auto;
      gap: 8px;
      align-items: center;
      padding: 12px;
      margin: 10px 10px 0;
      border: 1px solid var(--vscode-panel-border);
      border-radius: 20px;
      background: var(--vscode-sideBar-background);
      overflow: hidden;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.12);
    }
    .context {
      min-width: 0;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      color: var(--vscode-descriptionForeground);
    }
    .messages {
      overflow-y: auto;
      padding: 18px 14px 22px;
    }
    .turn {
      display: flex;
      margin: 0 0 16px;
    }
    .turn.user {
      justify-content: flex-end;
    }
    .turn.assistant,
    .turn.error {
      justify-content: flex-start;
    }
    .bubble {
      max-width: min(760px, 88%);
      border: 1px solid var(--vscode-panel-border);
      border-radius: 14px;
      overflow: hidden;
      background: var(--vscode-editorWidget-background);
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.16);
    }
    .turn.user .bubble {
      color: var(--vscode-button-foreground);
      background: var(--vscode-button-background);
      border-color: var(--vscode-button-background);
      border-bottom-right-radius: 5px;
    }
    .turn.assistant .bubble,
    .turn.error .bubble {
      border-bottom-left-radius: 5px;
    }
    .turn.error .bubble {
      border-color: var(--vscode-inputValidation-errorBorder);
      background: var(--vscode-inputValidation-errorBackground, var(--vscode-editorWidget-background));
    }
    .message-header {
      display: grid;
      grid-template-columns: 1fr auto;
      gap: 8px;
      align-items: center;
      padding: 9px 12px;
      border-bottom: 1px solid var(--vscode-panel-border);
      color: var(--vscode-descriptionForeground);
    }
    .turn.user .message-header {
      color: var(--vscode-button-foreground);
      border-bottom-color: color-mix(in srgb, var(--vscode-button-foreground) 25%, transparent);
    }
    .meta {
      min-width: 0;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .message-body {
      margin: 0;
      padding: 12px;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      font-family: var(--vscode-editor-font-family);
      font-size: var(--vscode-editor-font-size);
      line-height: 1.45;
    }
    .turn.user .message-body {
      font-family: var(--vscode-font-family);
      font-size: var(--vscode-font-size);
    }
    .composer {
      display: grid;
      grid-template-columns: minmax(104px, 142px) 1fr auto;
      gap: 8px;
      padding: 12px;
      margin: 0 10px 10px;
      border: 1px solid var(--vscode-panel-border);
      border-radius: 20px;
      background: var(--vscode-sideBar-background);
      overflow: hidden;
      box-shadow: 0 2px 12px rgba(0, 0, 0, 0.14);
    }
    select,
    textarea,
    button {
      font: inherit;
    }
    select,
    textarea {
      color: var(--vscode-input-foreground);
      background: var(--vscode-input-background);
      border: 1px solid var(--vscode-input-border, var(--vscode-panel-border));
      border-radius: 10px;
    }
    select {
      border-radius: 999px;
      padding: 0 10px;
      min-height: 32px;
    }
    textarea {
      min-height: 32px;
      max-height: 110px;
      resize: vertical;
      padding: 9px 10px;
      line-height: 1.35;
      border-radius: 14px;
    }
    button {
      color: var(--vscode-button-foreground);
      background: var(--vscode-button-background);
      border: 0;
      border-radius: 999px;
      padding: 7px 12px;
      min-height: 32px;
      cursor: pointer;
    }
    button:hover {
      background: var(--vscode-button-hoverBackground);
    }
    button.secondary {
      color: var(--vscode-button-secondaryForeground);
      background: var(--vscode-button-secondaryBackground);
      border: 1px solid transparent;
    }
    button.secondary:hover {
      background: var(--vscode-button-secondaryHoverBackground);
    }
    button:disabled {
      opacity: 0.6;
      cursor: default;
    }
    button.icon {
      min-width: 32px;
      padding: 5px 9px;
    }
    #set-key,
    #clear {
      min-width: 48px;
      padding-inline: 12px;
    }
    #send {
      min-width: 52px;
      padding: 5px 11px;
      min-height: 30px;
      align-self: end;
    }
    .actions {
      display: flex;
      gap: 6px;
      align-items: center;
    }
    .suggestions {
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      padding: 0 12px 12px;
    }
    .suggestion {
      max-width: 100%;
      text-align: left;
      font-family: var(--vscode-editor-font-family);
      font-size: var(--vscode-editor-font-size);
      overflow-wrap: anywhere;
    }
    .status {
      min-height: 18px;
      color: var(--vscode-descriptionForeground);
      padding: 0 10px 8px;
    }
    @media (max-width: 520px) {
      .toolbar {
        grid-template-columns: 1fr;
      }
      .composer {
        grid-template-columns: 1fr;
      }
      .actions {
        justify-content: flex-start;
      }
    }
  </style>
</head>
<body>
  <div class="root">
    <div class="toolbar">
      <div class="context" id="context">OptiNLP</div>
      <button class="secondary" id="set-key" type="button">Key</button>
      <button class="secondary" id="clear" type="button">Clear</button>
    </div>
    <main class="messages" id="messages" aria-live="polite"></main>
    <form class="composer" id="form">
      <select id="mode" aria-label="Mode">
        ${modeOptions}
      </select>
      <textarea id="request" aria-label="Request" placeholder="${escapeHtml(defaultPlaceholder)}"></textarea>
      <button id="send" type="submit">Send</button>
      <div class="status" id="status"></div>
    </form>
  </div>
  <script nonce="${nonce}">
    const vscode = acquireVsCodeApi();
    const messages = document.getElementById('messages');
    const form = document.getElementById('form');
    const mode = document.getElementById('mode');
    const request = document.getElementById('request');
    const send = document.getElementById('send');
    const status = document.getElementById('status');
    const context = document.getElementById('context');
    const setKey = document.getElementById('set-key');
    const clear = document.getElementById('clear');
    const modeLabels = ${JSON.stringify(Object.fromEntries(OPTINLP_MODE_DEFINITIONS.map(definition => [definition.id, definition.shortLabel])))};
    let history = restoreHistory();
    let pendingUserId = undefined;

    renderHistory();

    form.addEventListener('submit', event => {
      event.preventDefault();
      sendRequest(request.value.trim());
    });

    function sendRequest(text) {
      if (!text) return;
      const userMessage = {
        id: createMessageId(),
        role: 'user',
        mode: mode.value,
        text,
        meta: modeTitle(mode.value)
      };
      pendingUserId = userMessage.id;
      history.push(userMessage);
      request.value = '';
      saveAndRender();
      vscode.postMessage({ type: 'generate', mode: mode.value, request: text });
    }

    setKey.addEventListener('click', () => vscode.postMessage({ type: 'setApiKey' }));
    clear.addEventListener('click', () => {
      history = [];
      pendingUserId = undefined;
      saveAndRender();
      vscode.postMessage({ type: 'clearSession' });
    });

    window.addEventListener('message', event => {
      const message = event.data;
      if (message.type === 'context') {
        context.textContent = message.label;
      } else if (message.type === 'busy') {
        send.disabled = Boolean(message.busy);
        status.textContent = message.busy ? 'Running...' : '';
      } else if (message.type === 'result') {
        appendAssistantResult(message);
      } else if (message.type === 'externalRequest') {
        appendExternalRequest(message);
      } else if (message.type === 'error') {
        appendError(message.message || 'Unknown error');
      } else if (message.type === 'copied') {
        status.textContent = 'Copied';
        setTimeout(() => { status.textContent = ''; }, 1200);
      } else if (message.type === 'inserted') {
        status.textContent = 'Inserted';
        setTimeout(() => { status.textContent = ''; }, 1200);
      } else if (message.type === 'sessionCleared') {
        status.textContent = 'Cleared';
        setTimeout(() => { status.textContent = ''; }, 1200);
      }
    });

    function appendAssistantResult(message) {
      history.push({
        id: createMessageId(),
        role: 'assistant',
        text: message.markdown || '',
        meta: modeTitle(message.mode) + ' · ' + message.sourceLabel + ' · ' + message.provider + ' · ' + message.model,
        resultId: message.resultId,
        kind: message.structured && message.structured.kind,
        targetSuggestions: Array.isArray(message.targetSuggestions) ? message.targetSuggestions : [],
        replyTo: pendingUserId
      });
      pendingUserId = undefined;
      saveAndRender();
    }

    function appendExternalRequest(message) {
      const userMessage = {
        id: createMessageId(),
        role: 'user',
        mode: message.mode || 'target',
        text: message.request || '',
        meta: message.meta || modeTitle(message.mode || 'target')
      };
      pendingUserId = userMessage.id;
      history.push(userMessage);
      saveAndRender();
    }

    function appendError(text) {
      history.push({
        id: createMessageId(),
        role: 'error',
        text,
        meta: 'Error',
        replyTo: pendingUserId
      });
      pendingUserId = undefined;
      saveAndRender();
    }

    function renderHistory() {
      messages.replaceChildren();
      for (const item of history) {
        appendMessage(item);
      }
      messages.scrollTop = messages.scrollHeight;
    }

    function appendMessage(item) {
      const turn = document.createElement('article');
      turn.className = 'turn ' + item.role;
      const bubble = document.createElement('div');
      bubble.className = 'bubble';
      const header = document.createElement('div');
      header.className = 'message-header';
      const label = document.createElement('div');
      label.className = 'meta';
      label.textContent = item.meta || roleTitle(item.role);
      const actions = document.createElement('div');
      actions.className = 'actions';

      if (item.role !== 'user') {
        const copy = document.createElement('button');
        copy.className = 'secondary icon';
        copy.type = 'button';
        copy.title = 'Copy';
        copy.textContent = 'Copy';
        copy.addEventListener('click', () => vscode.postMessage({ type: 'copy', text: item.text }));
        actions.appendChild(copy);
      }
      if (item.resultId && (item.kind === 'command_to_script' || item.kind === 'code_to_full_script')) {
        const open = document.createElement('button');
        open.className = 'secondary icon';
        open.type = 'button';
        open.title = 'Open script';
        open.textContent = 'Open';
        open.addEventListener('click', () => vscode.postMessage({ type: 'openScript', resultId: item.resultId }));
        actions.appendChild(open);
      }
      header.appendChild(label);
      header.appendChild(actions);
      const body = document.createElement('pre');
      body.className = 'message-body';
      body.textContent = item.text;
      bubble.appendChild(header);
      bubble.appendChild(body);
      if (item.targetSuggestions && item.targetSuggestions.length > 0) {
        const suggestions = document.createElement('div');
        suggestions.className = 'suggestions';
        for (const suggestion of item.targetSuggestions) {
          if (!suggestion || !suggestion.target) continue;
          const button = document.createElement('button');
          button.className = 'secondary suggestion';
          button.type = 'button';
          button.title = suggestion.label ? suggestion.label + ': insert target' : 'Insert target';
          button.textContent = suggestion.target;
          button.addEventListener('click', () => vscode.postMessage({
            type: 'insertTarget',
            resultId: item.resultId,
            target: suggestion.target
          }));
          suggestions.appendChild(button);
        }
        bubble.appendChild(suggestions);
      }
      turn.appendChild(bubble);
      messages.appendChild(turn);
    }

    function modeTitle(value) {
      if (value === 'auto') return 'Auto';
      if (modeLabels[value]) return modeLabels[value];
      return 'OptiNLP';
    }

    function roleTitle(role) {
      if (role === 'user') return 'You';
      if (role === 'error') return 'Error';
      return 'OptiNLP';
    }

    function createMessageId() {
      return String(Date.now()) + '-' + String(Math.random()).slice(2);
    }

    function restoreHistory() {
      const state = vscode.getState();
      return Array.isArray(state && state.history) ? state.history : [];
    }

    function saveAndRender() {
      vscode.setState({ history });
      renderHistory();
    }

    vscode.postMessage({ type: 'ready' });
  </script>
</body>
</html>`;
}

function createNonce(): string {
  const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let nonce = "";
  for (let index = 0; index < 32; index += 1) {
    nonce += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return nonce;
}

function escapeHtml(value: string): string {
  return value.replace(/&/gu, "&amp;").replace(/</gu, "&lt;").replace(/>/gu, "&gt;").replace(/"/gu, "&quot;");
}
