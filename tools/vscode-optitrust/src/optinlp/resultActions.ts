// Utilities for turning structured OptiNLP results into editor actions.
// The VS Code commands and panel both use these to avoid divergent behavior.
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
    case "code_to_candidate_script":
      return { kind: "open_script", text: result.candidateScript };
  }
}
