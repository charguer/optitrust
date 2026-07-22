import { createHighlighterCore } from "../vscode-optitrust/node_modules/shiki/dist/core.mjs";
import { createOnigurumaEngine } from "../vscode-optitrust/node_modules/shiki/dist/engine-oniguruma.mjs";
import getWasmInstance from "../vscode-optitrust/node_modules/shiki/dist/wasm.mjs";
import c from "../vscode-optitrust/node_modules/shiki/dist/langs/c.mjs";
import cpp from "../vscode-optitrust/node_modules/shiki/dist/langs/cpp.mjs";
import darkPlus from "../vscode-optitrust/node_modules/shiki/dist/themes/dark-plus.mjs";
import lightPlus from "../vscode-optitrust/node_modules/shiki/dist/themes/light-plus.mjs";

const ACTIVE_THEME = "optitrust-active-vscode-theme";
let highlighterPromise;
let cachedActiveTheme;
let didReadActiveTheme = false;

window.OptitrustSyntaxHighlight = {
  highlightDiff
};

async function highlightDiff(container) {
  const highlighter = await getHighlighter();
  const wrappers = container.querySelectorAll(".d2h-file-wrapper");

  for (const wrapper of wrappers) {
    const language = languageForWrapper(wrapper);
    if (!language) {
      continue;
    }
    highlightWrapper(highlighter, wrapper, language);
  }
}

function getHighlighter() {
  if (!highlighterPromise) {
    highlighterPromise = createHighlighter();
  }
  return highlighterPromise;
}

async function createHighlighter() {
  const optilambda = await loadOptilambdaGrammar();
  const theme = activeTheme() ?? fallbackTheme();
  const themes = activeTheme() ? [theme, darkPlus, lightPlus] : [darkPlus, lightPlus];
  return createHighlighterCore({
    themes,
    langs: [c, cpp, optilambda],
    engine: createOnigurumaEngine(getWasmInstance)
  });
}

async function loadOptilambdaGrammar() {
  const grammarUrl = new URL("../vscode-optitrust/syntaxes/optilambda.tmLanguage.json", import.meta.url);
  const response = await fetch(grammarUrl);
  if (!response.ok) {
    throw new Error(`failed to load OptiLambda grammar: ${response.status}`);
  }
  const grammar = await response.json();
  return {
    ...grammar,
    name: "optilambda",
    aliases: ["opti"]
  };
}

function activeTheme() {
  if (didReadActiveTheme) {
    return cachedActiveTheme;
  }
  didReadActiveTheme = true;
  const element = document.getElementById("optitrustSyntaxHighlightConfig");
  if (!element?.textContent) {
    return undefined;
  }
  try {
    cachedActiveTheme = normalizeTheme(JSON.parse(element.textContent).theme);
    return cachedActiveTheme;
  } catch (error) {
    console.warn("OptiTrust: failed to parse syntax highlight theme config", error);
    return undefined;
  }
}

function normalizeTheme(theme) {
  if (!theme || typeof theme !== "object") {
    return undefined;
  }
  return {
    ...theme,
    name: ACTIVE_THEME,
    tokenColors: Array.isArray(theme.tokenColors) ? theme.tokenColors : []
  };
}

function fallbackTheme() {
  if (document.body.classList.contains("vscode-dark") ||
      document.body.classList.contains("vscode-high-contrast")) {
    return darkPlus;
  }
  return lightPlus;
}

function languageForWrapper(wrapper) {
  const language = wrapper.getAttribute("data-lang");
  switch (language) {
    case "c":
    case "h":
      return "c";
    case "cc":
    case "cpp":
    case "cxx":
    case "hpp":
    case "hh":
    case "hxx":
      return "cpp";
    case "opti":
    case "optilambda":
      return "optilambda";
    default:
      return undefined;
  }
}

function highlightWrapper(highlighter, wrapper, language) {
  const groups = wrapper.querySelectorAll(".d2h-file-side-diff");
  if (groups.length === 0) {
    highlightLineGroup(highlighter, wrapper, language);
    return;
  }
  for (const group of groups) {
    highlightLineGroup(highlighter, group, language);
  }
}

function highlightLineGroup(highlighter, group, language) {
  let grammarState;
  const lines = group.querySelectorAll(".d2h-code-line-ctn");
  for (const line of lines) {
    const text = line.textContent ?? "";
    if (text.length === 0) {
      line.innerHTML = "<br>";
      continue;
    }

    const result = highlighter.codeToTokens(text, {
      lang: language,
      theme: themeName(),
      grammarState
    });
    grammarState = result.grammarState;
    line.classList.add("optitrust-shiki");
    line.style.color = result.fg ?? "";
    line.innerHTML = tokensToHtml(result.tokens[0] ?? []);
  }
}

function themeName() {
  return activeTheme() ? ACTIVE_THEME : fallbackTheme().name;
}

function tokensToHtml(tokens) {
  return tokens.map(tokenToHtml).join("");
}

function tokenToHtml(token) {
  const style = tokenStyle(token);
  const content = escapeHtml(token.content);
  if (!style) {
    return content;
  }
  return `<span style="${style}">${content}</span>`;
}

function tokenStyle(token) {
  const properties = [];
  if (token.color) {
    properties.push(`color:${token.color}`);
  }
  if (token.fontStyle & 1) {
    properties.push("font-style:italic");
  }
  if (token.fontStyle & 2) {
    properties.push("font-weight:bold");
  }
  if (token.fontStyle & 4) {
    properties.push("text-decoration:underline");
  }
  return properties.join(";");
}

function escapeHtml(text) {
  return text
    .replace(/&/gu, "&amp;")
    .replace(/</gu, "&lt;")
    .replace(/>/gu, "&gt;")
    .replace(/"/gu, "&quot;");
}
