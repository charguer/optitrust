// Helpers for preparing source text before it is sent to an OptiNLP provider.
// VS Code-specific code computes ranges; this file only owns provider-neutral
// source annotations.

export const SELECTED_SOURCE_START_MARKER = "<start>";
export const SELECTED_SOURCE_END_MARKER = "<end>";

export function markSelectedRangeInText(text: string, startOffset: number, endOffset: number): string {
  const start = Math.max(0, Math.min(startOffset, text.length));
  const end = Math.max(start, Math.min(endOffset, text.length));
  return `${text.slice(0, start)}${SELECTED_SOURCE_START_MARKER}${text.slice(start, end)}${SELECTED_SOURCE_END_MARKER}${text.slice(end)}`;
}
