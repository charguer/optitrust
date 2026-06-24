// Provider-neutral generation helpers shared by CLI, commands, and webviews.
// Keeping dispatch here avoids repeating mode-specific provider calls.
import { OptiNlpProvider, OptiNlpProviderRequest, OptiNlpProviderResult } from "./providerTypes";

export async function generateOptiNlp(provider: OptiNlpProvider, request: OptiNlpProviderRequest): Promise<OptiNlpProviderResult> {
  switch (request.mode) {
    case "target":
      return provider.generateTarget(request);
    case "command_to_script":
      return provider.generateScript(request);
    case "code_to_candidate_script":
      return provider.generateCandidateScript(request);
  }
}
