#!/usr/bin/env node
// Command-line entrypoint for testing OptiNLP outside VS Code. It reuses the
// same prompt loader, provider factory, and schemas as the extension.
import { buildOptiNlpProviderRequest } from "./assets";
import { generateOptiNlp } from "./generation";
import { isOptiNlpCliCommand, modeDefinition, modeFromCliCommand, OptiNlpCliCommand } from "./modes";
import { OptiNlpProviderError } from "./providerErrors";
import { createOptiNlpProvider, DEFAULT_OPTINLP_PROVIDER, IMPLEMENTED_OPTINLP_PROVIDER_IDS, OptiNlpProviderId, parseOptiNlpProviderId } from "./providerFactory";
import { OptiNlpProviderResult } from "./providerTypes";

interface CliOptions {
  readonly command: OptiNlpCliCommand;
  readonly filePath: string;
  readonly userRequest: string;
  readonly json: boolean;
  readonly provider: OptiNlpProviderId;
  readonly model?: string;
  readonly root?: string;
  readonly sessionSummary?: string;
}

export async function runOptiNlpCli(argv: readonly string[], stdout: NodeJS.WritableStream = process.stdout, stderr: NodeJS.WritableStream = process.stderr): Promise<number> {
  let options: CliOptions;
  try {
    options = parseArgs(argv);
  } catch (error) {
    stderr.write(`${error instanceof Error ? error.message : String(error)}\n\n${usage()}\n`);
    return 2;
  }

  if (options.command === "target" && options.userRequest === "__help__") {
    stdout.write(`${usage()}\n`);
    return 0;
  }

  try {
    const mode = modeFromCliCommand(options.command);
    if (!mode) {
      throw new Error(`Unknown OptiNLP command '${options.command}'.`);
    }
    const request = await buildOptiNlpProviderRequest({
      root: options.root,
      mode,
      filePath: options.filePath,
      userRequest: options.userRequest,
      sessionSummary: options.sessionSummary
    });
    const provider = createOptiNlpProvider({
      provider: options.provider,
      gemini: { model: options.model },
      openai: { model: options.model },
      mock: { model: options.model }
    });
    const result = await generateOptiNlp(provider, request);
    writeResult(stdout, result, options.json);
    return 0;
  } catch (error) {
    if (error instanceof OptiNlpProviderError) {
      stderr.write(`${error.userMessage}\n`);
      if (error.technicalDetail) {
        stderr.write(`Detail: ${error.technicalDetail}\n`);
      }
      return 1;
    }
    stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
    return 1;
  }
}

function parseArgs(argv: readonly string[]): CliOptions {
  const [commandArg, ...rest] = argv;
  if (commandArg === "--help" || commandArg === "-h" || !commandArg) {
    return {
      command: "target",
      filePath: "",
      userRequest: "__help__",
      json: false,
      provider: DEFAULT_OPTINLP_PROVIDER
    };
  }
  if (!isOptiNlpCliCommand(commandArg)) {
    throw new Error(`Unknown OptiNLP command '${commandArg}'.`);
  }

  const values = new Map<string, string>();
  const flags = new Set<string>();
  for (let index = 0; index < rest.length; index += 1) {
    const arg = rest[index];
    if (arg === "--json") {
      flags.add(arg);
      continue;
    }
    if (!arg.startsWith("--")) {
      throw new Error(`Unexpected argument '${arg}'.`);
    }
    const value = rest[index + 1];
    if (!value || value.startsWith("--")) {
      throw new Error(`Missing value for ${arg}.`);
    }
    values.set(arg, value);
    index += 1;
  }

  const filePath = values.get("--file");
  if (!filePath) {
    throw new Error("Missing required --file option.");
  }
  const userRequest = values.get("--request") ?? values.get("--goal");
  if (!userRequest) {
    throw new Error(commandArg === "candidates" ? "Missing required --goal option." : "Missing required --request option.");
  }

  return {
    command: commandArg,
    filePath,
    userRequest,
    json: flags.has("--json"),
    provider: parseProvider(values.get("--provider") ?? process.env.OPTINLP_PROVIDER ?? DEFAULT_OPTINLP_PROVIDER),
    model: values.get("--model"),
    root: values.get("--root"),
    sessionSummary: values.get("--session-summary")
  };
}

function parseProvider(value: string): OptiNlpProviderId {
  const provider = parseOptiNlpProviderId(value);
  if (provider) {
    return provider;
  }
  throw new Error(`Unknown OptiNLP provider '${value}'.`);
}

function writeResult(stdout: NodeJS.WritableStream, result: OptiNlpProviderResult, json: boolean): void {
  if (json) {
    stdout.write(
      `${JSON.stringify(
        {
          provider: result.provider,
          model: result.model,
          markdownOutput: result.markdownOutput,
          structured: result.structured
        },
        null,
        2
      )}\n`
    );
    return;
  }
  stdout.write(`${result.markdownOutput}\n`);
}

function usage(): string {
  const providers = IMPLEMENTED_OPTINLP_PROVIDER_IDS.join("|");
  const commands = ["target", "script", "candidates"]
    .map(command => {
      const definition = modeFromCliCommand(command);
      const placeholder = definition ? modeDefinition(definition).placeholder : "...";
      const requestFlag = command === "candidates" ? "--goal" : "--request";
      return `  optinlp ${command} --file path ${requestFlag} "${placeholder}" [--json] [--provider ${providers}]`;
    })
    .join("\n");
  return [
    "Usage:",
    commands,
    "",
    "Options:",
    "  --model name             Override provider model.",
    "  --root path              OptiTrust root. Defaults to auto-detection from cwd.",
    "  --session-summary text   Optional in-memory session summary text.",
    "",
    "Environment:",
    "  GEMINI_API_KEY           Gemini API key for the default gemini provider.",
    "  OPENAI_API_KEY           OpenAI API key for the openai provider.",
    "  OPTINLP_PROVIDER         Optional default provider override, for example mock."
  ].join("\n");
}

if (require.main === module) {
  void runOptiNlpCli(process.argv.slice(2)).then(exitCode => {
    process.exitCode = exitCode;
  });
}
