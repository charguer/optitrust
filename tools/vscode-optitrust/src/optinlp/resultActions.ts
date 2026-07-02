// Utilities for turning structured OptiNLP results into editor actions.
// VS Code commands and native chat use these to avoid divergent behavior.
import { OptiNlpStructuredResult } from "./resultSchemas";

export type OptiNlpEditorAction =
  | { readonly kind: "insert_target"; readonly text: string }
  | { readonly kind: "open_script"; readonly text: string };

export function editorActionForResult(result: OptiNlpStructuredResult | undefined): OptiNlpEditorAction | undefined {
  if (!result) {
    return undefined;
  }
  switch (result.kind) {
    case "target":
      return result.recommendedTarget ? { kind: "insert_target", text: result.recommendedTarget } : undefined;
    case "command_to_script":
      return { kind: "open_script", text: result.generatedScript };
    case "code_to_full_script":
      return { kind: "open_script", text: result.fullScript };
  }
}

const TARGET_SELECTOR_HINT = /\b(?:nbMulti|nbAny|nbExact|occIndex|occFirst|occLast|tBefore|tAfter|tFirst|tLast|tBetweenAll|tSpan|cFor|cFor_c|cWhile|cIf|cFunDef|cTopFunDef|cFunBody|cTopFunBody|cCall|cVarDef|cVarsDef|cVar|cReadVar|cWriteVar|cArrayRead|cArrayWrite|cFieldRead|cFieldWrite|cSeq|cReturn|cLabel|cMark)\b/u;
const OCAML_CODE_BLOCK_PATTERN = /```(?:ocaml)?\s*([\s\S]*?)```/giu;
const TARGET_LIST_PATTERN = /\[[^\]\n]*(?:\][^\[\n]*)?\]/gu;

export function targetSuggestionsFromMarkdown(markdown: string): string[] {
  const suggestions: string[] = [];
  const seen = new Set<string>();
  const add = (candidate: string): void => {
    const trimmed = candidate.trim().replace(/;?\s*$/u, "");
    if (!trimmed || seen.has(trimmed) || !TARGET_SELECTOR_HINT.test(trimmed)) {
      return;
    }
    seen.add(trimmed);
    suggestions.push(trimmed);
  };

  for (const block of markdown.matchAll(OCAML_CODE_BLOCK_PATTERN)) {
    collectTargetsFromText(block[1], add);
  }
  collectTargetsFromText(markdown.replace(OCAML_CODE_BLOCK_PATTERN, ""), add);
  return suggestions;
}

function collectTargetsFromText(text: string, add: (candidate: string) => void): void {
  for (const match of text.matchAll(TARGET_LIST_PATTERN)) {
    add(match[0]);
  }
}
