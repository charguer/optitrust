// Webview side panel for OptiNLP. The panel is intentionally presentation-only:
// generation, provider selection, and editor actions live in shared modules.
import * as path from "path";
import * as vscode from "vscode";
import { clearOptiNlpSession, insertTextAtCursor, openOcamlDocument, runOptiNlpGeneration, setOptiNlpConfiguredProviderApiKey } from "./optinlpCommands";
import { inferLanguage } from "../optinlp/assets";
import { modeDefinition, OPTINLP_MODE_DEFINITIONS, OptiNlpUiMode, resolveAutoMode } from "../optinlp/modes";
import { OptiNlpMode } from "../optinlp/providerTypes";
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
}

export class OptiNlpPanel {
  private static current: OptiNlpPanel | undefined;
  private readonly panel: vscode.WebviewPanel;
  private resultSeq = 0;
  private readonly results = new Map<string, OptiNlpStructuredResult>();
  private sourceEditor: vscode.TextEditor | undefined;
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
      if (fileEditor) {
        this.sourceEditor = fileEditor;
      }
      void this.refreshContext();
    }, undefined, this.disposables);
  }

  static show(context: vscode.ExtensionContext, workspace: OptitrustWorkspace, memory: OptiNlpSessionMemory): void {
    if (OptiNlpPanel.current) {
      OptiNlpPanel.current.panel.reveal(vscode.ViewColumn.Beside);
      void OptiNlpPanel.current.refreshContext();
      return;
    }

    const panel = vscode.window.createWebviewPanel("optitrustOptiNlp", "OptiNLP", vscode.ViewColumn.Beside, {
      enableScripts: true,
      retainContextWhenHidden: true
    });
    OptiNlpPanel.current = new OptiNlpPanel(context, workspace, memory, panel);
    void OptiNlpPanel.current.refreshContext();
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
        await this.insertTarget(message.resultId);
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

    const resolvedMode = resolveAutoMode(mode, trimmed);
    this.post({ type: "busy", busy: true });
    try {
      const outcome = await runOptiNlpGeneration(this.context, this.workspace, this.memory, resolvedMode, trimmed, {
        renderToOutput: false,
        editor: this.getSourceEditor()
      });
      if (!outcome) {
        this.post({ type: "busy", busy: false });
        return;
      }
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
        structured: outcome.result.structured
      });
      await this.refreshContext();
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      this.post({ type: "error", message });
    } finally {
      this.post({ type: "busy", busy: false });
    }
  }

  private async insertTarget(resultId: string | undefined): Promise<void> {
    const result = resultId ? this.results.get(resultId) : undefined;
    if (!result || result.kind !== "target" || !result.recommendedTarget) {
      this.post({ type: "error", message: "No target expression is available for insertion." });
      return;
    }
    await insertTextAtCursor(result.recommendedTarget, this.getSourceEditor());
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
    }
    .toolbar {
      display: grid;
      grid-template-columns: 1fr auto auto;
      gap: 8px;
      align-items: center;
      padding: 10px;
      border-bottom: 1px solid var(--vscode-panel-border);
      background: var(--vscode-sideBar-background);
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
      padding: 10px;
    }
    .message {
      border: 1px solid var(--vscode-panel-border);
      border-radius: 6px;
      margin-bottom: 10px;
      overflow: hidden;
      background: var(--vscode-editorWidget-background);
    }
    .message-header {
      display: grid;
      grid-template-columns: 1fr auto;
      gap: 8px;
      align-items: center;
      padding: 8px 10px;
      border-bottom: 1px solid var(--vscode-panel-border);
      color: var(--vscode-descriptionForeground);
    }
    .message pre {
      margin: 0;
      padding: 10px;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      font-family: var(--vscode-editor-font-family);
      font-size: var(--vscode-editor-font-size);
      line-height: 1.45;
    }
    .composer {
      display: grid;
      grid-template-columns: minmax(110px, 150px) 1fr auto;
      gap: 8px;
      padding: 10px;
      border-top: 1px solid var(--vscode-panel-border);
      background: var(--vscode-sideBar-background);
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
      border-radius: 4px;
    }
    textarea {
      min-height: 34px;
      max-height: 110px;
      resize: vertical;
      padding: 7px;
    }
    button {
      color: var(--vscode-button-foreground);
      background: var(--vscode-button-background);
      border: 0;
      border-radius: 4px;
      padding: 6px 10px;
      min-height: 32px;
      cursor: pointer;
    }
    button:hover {
      background: var(--vscode-button-hoverBackground);
    }
    button.secondary {
      color: var(--vscode-button-secondaryForeground);
      background: var(--vscode-button-secondaryBackground);
    }
    button.secondary:hover {
      background: var(--vscode-button-secondaryHoverBackground);
    }
    button:disabled {
      opacity: 0.6;
      cursor: default;
    }
    .actions {
      display: flex;
      gap: 6px;
      align-items: center;
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

    form.addEventListener('submit', event => {
      event.preventDefault();
      vscode.postMessage({ type: 'generate', mode: mode.value, request: request.value });
    });

    setKey.addEventListener('click', () => vscode.postMessage({ type: 'setApiKey' }));
    clear.addEventListener('click', () => {
      messages.replaceChildren();
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
        appendResult(message);
      } else if (message.type === 'error') {
        appendTextCard('Error', message.message || 'Unknown error');
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

    function appendResult(message) {
      const title = modeTitle(message.mode) + ' · ' + message.sourceLabel + ' · ' + message.provider;
      appendTextCard(title, message.markdown || '', message.request || '', message.resultId, message.structured && message.structured.kind);
      request.value = '';
    }

    function appendTextCard(title, text, requestText = '', resultId = undefined, kind = undefined) {
      const card = document.createElement('article');
      card.className = 'message';
      const header = document.createElement('div');
      header.className = 'message-header';
      const label = document.createElement('div');
      label.textContent = requestText ? title + ' · ' + requestText : title;
      const actions = document.createElement('div');
      actions.className = 'actions';
      const copy = document.createElement('button');
      copy.className = 'secondary';
      copy.type = 'button';
      copy.textContent = 'Copy';
      copy.addEventListener('click', () => vscode.postMessage({ type: 'copy', text }));
      actions.appendChild(copy);
      if (resultId && kind === 'target') {
        const insert = document.createElement('button');
        insert.className = 'secondary';
        insert.type = 'button';
        insert.textContent = 'Insert';
        insert.addEventListener('click', () => vscode.postMessage({ type: 'insertTarget', resultId }));
        actions.appendChild(insert);
      }
      if (resultId && (kind === 'command_to_script' || kind === 'code_to_candidate_script')) {
        const open = document.createElement('button');
        open.className = 'secondary';
        open.type = 'button';
        open.textContent = 'Open';
        open.addEventListener('click', () => vscode.postMessage({ type: 'openScript', resultId }));
        actions.appendChild(open);
      }
      header.appendChild(label);
      header.appendChild(actions);
      const pre = document.createElement('pre');
      pre.textContent = text;
      card.appendChild(header);
      card.appendChild(pre);
      messages.appendChild(card);
      messages.scrollTop = messages.scrollHeight;
    }

    function modeTitle(value) {
      const labels = ${JSON.stringify(Object.fromEntries(OPTINLP_MODE_DEFINITIONS.map(definition => [definition.id, definition.shortLabel])))};
      if (labels[value]) return labels[value];
      return 'OptiNLP';
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
