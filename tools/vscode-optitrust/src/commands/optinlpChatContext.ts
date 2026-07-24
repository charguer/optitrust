// Short-lived context handoff from editor commands to the native OptiNLP chat
// participant. F7 prepares rich source/after-state context, then chat consumes
// it when the user submits the prefilled @optinlp request.
import { randomUUID } from "crypto";
import { OptiNlpMode } from "../optinlp/providerTypes";
import { SourceContext } from "./optinlpCommands";

export interface PendingOptiNlpChatRequest {
  readonly id: string;
  readonly createdAt: number;
  readonly mode: OptiNlpMode;
  readonly chatPrompt: string;
  readonly userRequest: string;
  readonly sourceContext: SourceContext;
  readonly filePath: string;
  readonly language: string;
  readonly targetInsertionFilePath?: string;
}

export type NewPendingOptiNlpChatRequest = Omit<PendingOptiNlpChatRequest, "id" | "createdAt">;

const PENDING_REQUEST_TTL_MS = 10 * 60 * 1000;
let pendingRequest: PendingOptiNlpChatRequest | undefined;

export function setPendingOptiNlpChatRequest(request: NewPendingOptiNlpChatRequest): PendingOptiNlpChatRequest {
  pendingRequest = {
    ...request,
    id: randomUUID(),
    createdAt: Date.now()
  };
  return pendingRequest;
}

export function takePendingOptiNlpChatRequest(mode: OptiNlpMode, chatPrompt: string): PendingOptiNlpChatRequest | undefined {
  if (!pendingRequest) {
    return undefined;
  }
  if (Date.now() - pendingRequest.createdAt > PENDING_REQUEST_TTL_MS) {
    pendingRequest = undefined;
    return undefined;
  }
  const trimmedPrompt = chatPrompt.trim();
  const matchesPrompt =
    trimmedPrompt === pendingRequest.chatPrompt ||
    trimmedPrompt.startsWith(`${pendingRequest.chatPrompt} `) ||
    trimmedPrompt.includes(pendingRequest.id);
  if (pendingRequest.mode !== mode || !matchesPrompt) {
    return undefined;
  }

  const request = pendingRequest;
  pendingRequest = undefined;
  return request;
}
