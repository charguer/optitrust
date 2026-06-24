// Normalized provider error type. UI and CLI code can show userMessage while
// keeping provider-specific details separate and non-secret.
export class OptiNlpProviderError extends Error {
  readonly provider: string;
  readonly userMessage: string;
  readonly technicalDetail?: string;

  constructor(provider: string, userMessage: string, technicalDetail?: string, cause?: unknown) {
    super(userMessage);
    this.name = "OptiNlpProviderError";
    this.provider = provider;
    this.userMessage = userMessage;
    this.technicalDetail = technicalDetail;

    if (cause !== undefined) {
      (this as Error & { cause?: unknown }).cause = cause;
    }
  }
}

export function technicalDetailFrom(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
}
