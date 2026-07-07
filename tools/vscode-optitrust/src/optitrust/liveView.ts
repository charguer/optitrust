import * as vscode from "vscode";

type LiveViewKind = "native-diff" | "html";

interface AttachedLiveView {
  readonly kind: LiveViewKind;
  readonly viewColumn: vscode.ViewColumn;
  readonly getViewColumn?: () => vscode.ViewColumn | undefined;
  readonly detach?: () => void;
  readonly dispose?: () => Thenable<unknown> | Promise<unknown> | void;
  readonly ownsUri?: (uri: vscode.Uri) => boolean;
}

interface PrepareAttachedLiveViewOptions {
  readonly replaceSameKind?: boolean;
}

let attachedLiveView: AttachedLiveView | undefined;
let liveViewSlotId = 1;

export function initializeLiveViewContext(): void {
  setLiveViewContexts(false, false);
}

export async function prepareAttachedLiveView(kind: LiveViewKind, options: PrepareAttachedLiveViewOptions = {}): Promise<vscode.ViewColumn> {
  const viewColumn = currentAttachedViewColumn();
  if (attachedLiveView?.kind === kind && !options.replaceSameKind) {
    return viewColumn;
  }

  await closeAttachedLiveView();
  return viewColumn;
}

export function currentAttachedViewColumn(fallback: vscode.ViewColumn = vscode.ViewColumn.Beside): vscode.ViewColumn {
  return attachedLiveView?.getViewColumn?.() ?? attachedLiveView?.viewColumn ?? fallback;
}

export function attachLiveView(view: AttachedLiveView): void {
  attachedLiveView = view;
  refreshLiveViewContexts();
}

export function detachLiveView(): boolean {
  if (!attachedLiveView) {
    return false;
  }
  attachedLiveView.detach?.();
  attachedLiveView = undefined;
  liveViewSlotId += 1;
  setLiveViewContexts(false, false);
  return true;
}

export function currentLiveViewSlotId(): number {
  return liveViewSlotId;
}

export function clearLiveView(view: AttachedLiveView): void {
  if (attachedLiveView === view) {
    attachedLiveView = undefined;
    setLiveViewContexts(false, false);
  }
}

export function isAttachedLiveViewUri(uri: vscode.Uri): boolean {
  return attachedLiveView?.ownsUri?.(uri) ?? false;
}

export function isAttachedLiveView(view: AttachedLiveView): boolean {
  return attachedLiveView === view;
}

export function refreshLiveViewContexts(): void {
  setLiveViewContexts(attachedLiveView !== undefined, activeEditorIsAttachedLiveView());
}

export function activeViewIsAttachedLiveView(): boolean {
  return activeEditorIsAttachedLiveView();
}

export function setActiveLiveViewContext(active: boolean): void {
  setLiveViewContexts(attachedLiveView !== undefined, active);
}

async function closeAttachedLiveView(): Promise<void> {
  const view = attachedLiveView;
  attachedLiveView = undefined;
  setLiveViewContexts(false, false);
  await view?.dispose?.();
}

function activeEditorIsAttachedLiveView(): boolean {
  if (!attachedLiveView) {
    return false;
  }

  return activeEditorUris().some(uri => attachedLiveView?.ownsUri?.(uri));
}

function activeEditorUris(): vscode.Uri[] {
  const input = vscode.window.tabGroups.activeTabGroup.activeTab?.input;
  if (input instanceof vscode.TabInputTextDiff) {
    return [input.original, input.modified];
  }
  if (input instanceof vscode.TabInputText) {
    return [input.uri];
  }

  const activeEditorUri = vscode.window.activeTextEditor?.document.uri;
  return activeEditorUri ? [activeEditorUri] : [];
}

function setLiveViewContexts(attached: boolean, active: boolean): void {
  void vscode.commands.executeCommand("setContext", "optitrust.liveViewAttached", attached);
  void vscode.commands.executeCommand("setContext", "optitrust.activeViewIsLiveView", active);
}
