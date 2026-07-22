(function () {
  "use strict";

  const contractKeywords = new Set([
    "requires",
    "reads",
    "writes",
    "consumes",
    "ensures",
    "produces",
    "preserves",
    "xrequires",
    "xconsumes",
    "xensures",
    "xproduces",
    "strict",
    "reverts"
  ]);
  const syntaxKeywords = new Set([
    "if",
    "else",
    "for",
    "while",
    "do",
    "switch",
    "case",
    "default",
    "break",
    "continue",
    "return",
    "goto",
    "in",
    "fun",
    "ghost",
    "let",
    "letmut",
    "record",
    "enum",
    "type",
    "namespace",
    "extern",
    "template",
    "using",
    "true",
    "false",
    "null",
    "unit",
    "strict",
    ...contractKeywords
  ]);

  function classifyLine(text) {
    const trimmed = text.trim();
    if (trimmed === "") {
      return undefined;
    }
    if (/^ghost(?:\s|_|\()/.test(trimmed)) {
      return "ghost";
    }
    const keyword = trimmed.match(/^([A-Za-z_][A-Za-z0-9_]*)\b/);
    if (keyword && contractKeywords.has(keyword[1])) {
      return "contract";
    }
    return undefined;
  }

  function contractLineIsComplete(text) {
    const trimmed = text.trim();
    return /;\s*$/.test(trimmed);
  }

  function groupLabel(kind, index) {
    return (kind === "ghost" ? "G" : "C") + index;
  }

  function closePopup() {
    document.querySelectorAll(".opti-info-popup").forEach((popup) => popup.remove());
    document.querySelectorAll(".opti-group-button[aria-expanded='true']").forEach((button) => {
      button.setAttribute("aria-expanded", "false");
    });
    activeGroupButton = undefined;
  }

  function popupCodeBlock(lines) {
    const pre = document.createElement("pre");
    pre.className = "opti-rich-code opti-popup-code";
    for (const line of lines) {
      const row = document.createElement("div");
      row.className = "opti-code-line";
      const content = document.createElement("span");
      content.className = "opti-code-line-content";
      content.setAttribute("data-opti-line-text", line);
      content.textContent = line === "" ? " " : line;
      row.appendChild(content);
      pre.appendChild(row);
    }
    return pre;
  }

  function positionPopup(anchor, popup) {
    const anchorRect = anchor.getBoundingClientRect();
    const maxLeft = Math.max(8, window.innerWidth - popup.offsetWidth - 8);
    popup.style.left = Math.min(Math.max(8, anchorRect.left), maxLeft) + "px";
    popup.style.top = Math.min(anchorRect.bottom + 6, window.innerHeight - popup.offsetHeight - 8) + "px";
  }

  function highlightPopupBody(anchor, popup, body) {
    if (!window.OptitrustSyntaxHighlight || !window.OptitrustSyntaxHighlight.highlightCodeBlock) {
      body.dataset.optitrustHighlighter = "fallback";
      return;
    }
    window.OptitrustSyntaxHighlight.highlightCodeBlock(body, "optilambda")
      .then(() => {
        if (popup.isConnected) {
          positionPopup(anchor, popup);
        }
      })
      .catch(() => {
        body.dataset.optitrustHighlighter = "fallback";
      });
  }

  function showPopup(anchor, title, lines) {
    closePopup();
    const popup = document.createElement("div");
    popup.className = "opti-info-popup";
    popup.setAttribute("role", "dialog");

    const heading = document.createElement("div");
    heading.className = "opti-info-popup-title";
    heading.textContent = title;
    popup.appendChild(heading);

    const body = popupCodeBlock(lines);
    popup.appendChild(body);

    document.body.appendChild(popup);
    positionPopup(anchor, popup);
    highlightPopupBody(anchor, popup, body);
    anchor.setAttribute("aria-expanded", "true");
  }

  let activeGroupButton;
  let groupPopupMode = "text";

  function groupLinesFromButton(button) {
    try {
      const lines = JSON.parse(button.dataset.optiGroupLines || "[]");
      return Array.isArray(lines) ? lines.map((line) => String(line)) : [];
    } catch {
      return [];
    }
  }

  function toggleGroupPopup(button) {
    if (button.getAttribute("aria-expanded") === "true") {
      closePopup();
      return;
    }
    showGroupPopup(button);
  }

  function makeGroupButton(kind, index, lines, metadata = {}) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "opti-group-button opti-group-" + kind;
    button.textContent = groupLabel(kind, index);
    button.title = kind === "ghost" ? "Show grouped ghost code" : "Show grouped contract clauses";
    button.setAttribute("aria-expanded", "false");
    button.dataset.optiGroupTitle = button.textContent;
    button.dataset.optiGroupLines = JSON.stringify(lines);
    button.dataset.optiGroupKind = kind;
    button.dataset.optiGroupIndex = String(index);
    button.dataset.optiGroupSide = metadata.side || "single";
    button.dataset.optiGroupContext = metadata.context || "";
    button.dataset.optiGroupFile = metadata.file || "";
    button.dataset.optiGroupOrder = String(metadata.order ?? 0);
    return button;
  }

  function showGroupPopup(button) {
    closePopup();
    activeGroupButton = button;
    const popup = document.createElement("div");
    popup.className = "opti-info-popup opti-group-popup";
    popup.setAttribute("role", "dialog");

    const header = document.createElement("div");
    header.className = "opti-info-popup-header";

    const heading = document.createElement("div");
    heading.className = "opti-info-popup-title";
    heading.textContent = button.dataset.optiGroupTitle || button.textContent || "group";
    header.appendChild(heading);

    if (canShowGroupDiff(button)) {
      const toggle = document.createElement("button");
      toggle.type = "button";
      toggle.className = "opti-popup-mode-toggle";
      toggle.textContent = groupPopupMode === "diff" ? "Text" : "Diff";
      toggle.title = groupPopupMode === "diff" ? "Show grouped text" : "Show old/new diff for this context";
      header.appendChild(toggle);
    }

    popup.appendChild(header);

    const body = document.createElement("div");
    body.className = "opti-popup-body";
    popup.appendChild(body);

    document.body.appendChild(popup);
    renderGroupPopupBody(button, popup, body);
    positionPopup(button, popup);
    button.setAttribute("aria-expanded", "true");
  }

  function canShowGroupDiff(button) {
    return button.dataset.optiGroupSide !== "single" && button.dataset.optiGroupContext !== "" && Boolean(button.closest("#diffDiv"));
  }

  function renderGroupPopupBody(button, popup, body) {
    body.textContent = "";
    const mode = canShowGroupDiff(button) ? groupPopupMode : "text";
    const block = mode === "diff"
      ? groupDiffCodeBlock(groupDiffForButton(button))
      : popupCodeBlock(groupLinesFromButton(button));
    body.appendChild(block);
    highlightPopupBody(button, popup, block);
    const toggle = popup.querySelector(".opti-popup-mode-toggle");
    if (toggle) {
      toggle.textContent = groupPopupMode === "diff" ? "Text" : "Diff";
      toggle.title = groupPopupMode === "diff" ? "Show grouped text" : "Show old/new diff for this context";
    }
  }

  function toggleActiveGroupPopupMode() {
    if (!activeGroupButton) {
      return;
    }
    groupPopupMode = groupPopupMode === "diff" ? "text" : "diff";
    const popup = document.querySelector(".opti-group-popup");
    const body = popup?.querySelector(".opti-popup-body");
    if (popup && body) {
      renderGroupPopupBody(activeGroupButton, popup, body);
      positionPopup(activeGroupButton, popup);
    }
  }

  function groupDiffForButton(button) {
    const currentSide = button.dataset.optiGroupSide || "old";
    const oldLines = groupLinesForContext(button, "old");
    const newLines = groupLinesForContext(button, "new");
    if (oldLines.length === 0 && currentSide === "old") {
      return groupLinesFromButton(button).map((line) => ({ kind: "removed", line }));
    }
    if (newLines.length === 0 && currentSide === "new") {
      return groupLinesFromButton(button).map((line) => ({ kind: "added", line }));
    }
    return diffLines(oldLines, newLines);
  }

  function groupLinesForContext(button, side) {
    const root = button.closest("#diffDiv");
    if (!root) {
      return [];
    }
    const kind = button.dataset.optiGroupKind;
    const context = button.dataset.optiGroupContext;
    const file = button.dataset.optiGroupFile;
    return Array.from(root.querySelectorAll(".opti-group-button"))
      .filter((candidate) =>
        candidate.dataset.optiGroupKind === kind &&
        candidate.dataset.optiGroupContext === context &&
        candidate.dataset.optiGroupFile === file &&
        candidate.dataset.optiGroupSide === side
      )
      .sort((left, right) => Number(left.dataset.optiGroupOrder || 0) - Number(right.dataset.optiGroupOrder || 0))
      .flatMap((candidate) => groupLinesFromButton(candidate));
  }

  function normalizeDiffLine(line) {
    return line.trim().replace(/\s+/g, " ");
  }

  function diffLines(oldLines, newLines) {
    const oldKeys = oldLines.map(normalizeDiffLine);
    const newKeys = newLines.map(normalizeDiffLine);
    const rows = Array.from({ length: oldLines.length + 1 }, () => Array(newLines.length + 1).fill(0));
    for (let oldIndex = oldLines.length - 1; oldIndex >= 0; oldIndex--) {
      for (let newIndex = newLines.length - 1; newIndex >= 0; newIndex--) {
        rows[oldIndex][newIndex] = oldKeys[oldIndex] === newKeys[newIndex]
          ? rows[oldIndex + 1][newIndex + 1] + 1
          : Math.max(rows[oldIndex + 1][newIndex], rows[oldIndex][newIndex + 1]);
      }
    }

    const diff = [];
    let oldIndex = 0;
    let newIndex = 0;
    while (oldIndex < oldLines.length && newIndex < newLines.length) {
      if (oldKeys[oldIndex] === newKeys[newIndex]) {
        diff.push({ kind: "same", line: oldLines[oldIndex] });
        oldIndex += 1;
        newIndex += 1;
      } else if (rows[oldIndex + 1][newIndex] >= rows[oldIndex][newIndex + 1]) {
        diff.push({ kind: "removed", line: oldLines[oldIndex] });
        oldIndex += 1;
      } else {
        diff.push({ kind: "added", line: newLines[newIndex] });
        newIndex += 1;
      }
    }
    while (oldIndex < oldLines.length) {
      diff.push({ kind: "removed", line: oldLines[oldIndex] });
      oldIndex += 1;
    }
    while (newIndex < newLines.length) {
      diff.push({ kind: "added", line: newLines[newIndex] });
      newIndex += 1;
    }
    return diff;
  }

  function groupDiffCodeBlock(diff) {
    const pre = document.createElement("pre");
    pre.className = "opti-rich-code opti-popup-code opti-group-diff";
    for (const entry of diff) {
      const row = document.createElement("div");
      row.className = "opti-code-line opti-group-diff-line opti-group-diff-" + entry.kind;

      const prefix = document.createElement("span");
      prefix.className = "opti-group-diff-prefix";
      prefix.textContent = entry.kind === "added" ? "+" : entry.kind === "removed" ? "-" : " ";
      row.appendChild(prefix);

      const content = document.createElement("span");
      content.className = "opti-code-line-content opti-group-diff-content";
      content.setAttribute("data-opti-line-text", entry.line);
      content.textContent = entry.line === "" ? " " : entry.line;
      row.appendChild(content);
      pre.appendChild(row);
    }
    return pre;
  }

  function buildTypeMap(typeSource) {
    const typeMap = new Map();
    if (!typeSource) {
      return typeMap;
    }
    const pattern = /\b([A-Za-z_][A-Za-z0-9_']*)\s*:\s*([^,\)\]\{;=\n]+)/g;
    let match;
    while ((match = pattern.exec(typeSource)) !== null) {
      const name = match[1];
      const typ = match[2].trim();
      if (typ !== "" && !typeMap.has(name)) {
        typeMap.set(name, typ);
      }
    }
    const refPattern = /\blet\s+([A-Za-z_][A-Za-z0-9_']*)\s*=\s*ref(?:_uninit)?<([^>\n]+)>/g;
    while ((match = refPattern.exec(typeSource)) !== null) {
      const name = match[1];
      const typ = match[2].trim();
      if (typ !== "" && !typeMap.has(name)) {
        typeMap.set(name, "mutable " + typ);
      }
    }
    return typeMap;
  }

  function splitParams(params) {
    const trimmed = params.trim();
    if (trimmed === "") {
      return [];
    }
    return trimmed.split(",").map((param) => param.trim()).filter(Boolean);
  }

  function shortText(text, maxLength = 96) {
    const compact = text.trim().replace(/\s+/g, " ");
    if (compact.length <= maxLength) {
      return compact;
    }
    return compact.slice(0, maxLength - 1) + "...";
  }

  function countArgs(args) {
    const trimmed = args.trim();
    if (trimmed === "") {
      return 0;
    }
    let depth = 0;
    let count = 1;
    for (const character of trimmed) {
      if (character === "(" || character === "[" || character === "{" || character === "<") {
        depth += 1;
      } else if ((character === ")" || character === "]" || character === "}" || character === ">") && depth > 0) {
        depth -= 1;
      } else if (character === "," && depth === 0) {
        count += 1;
      }
    }
    return count;
  }

  function hover(title, lines, priority) {
    return { title, lines: lines.filter(Boolean), priority };
  }

  function mergeHover(previous, next) {
    if (!previous || next.priority > previous.priority) {
      return next;
    }
    if (next.priority === previous.priority) {
      const lines = previous.lines.slice();
      for (const line of next.lines) {
        if (!lines.includes(line)) {
          lines.push(line);
        }
      }
      return { title: previous.title, lines, priority: previous.priority };
    }
    return previous;
  }

  function addHover(index, name, info) {
    if (!name || syntaxKeywords.has(name)) {
      return;
    }
    index.set(name, mergeHover(index.get(name), info));
  }

  function functionInfoFromLine(line) {
    const match = line.match(/\bfun\s+([A-Za-z_][A-Za-z0-9_']*)\s*\(([^)]*)\)\s*(?::\s*([^\{\[\n;]+))?\s*(\[[^\n{]*\])?/);
    if (!match) {
      return undefined;
    }
    const [, name, params, returnType, contracts] = match;
    const paramList = splitParams(params);
    return {
      name,
      info: hover(
        name,
        [
          "kind: function",
          "signature: fun " + name + "(" + paramList.join(", ") + ")" + (returnType ? ": " + returnType.trim() : ""),
          paramList.length > 0 ? "parameters: " + paramList.join(", ") : "parameters: none",
          returnType ? "returns: " + returnType.trim() : "returns: inferred/unspecified",
          contracts ? "contracts: " + shortText(contracts, 120) : ""
        ],
        80
      )
    };
  }

  function letInfosFromLine(line, typeMap) {
    const infos = [];
    const pattern = /\blet(mut)?\s+([A-Za-z_][A-Za-z0-9_']*)\s*(?::\s*([^=;,\n]+))?\s*(?:=\s*([^;\n]+))?/g;
    let match;
    while ((match = pattern.exec(line)) !== null) {
      const [, mutable, name, inlineType, initializer] = match;
      const typ = (inlineType || typeMap.get(name) || "").trim();
      infos.push({
        name,
        keyword: mutable ? "letmut" : "let",
        info: hover(
          name,
          [
            "kind: " + (mutable ? "mutable local binding" : "local binding"),
            typ ? "type: " + typ : "type: inferred/unknown",
            initializer ? "initial value: " + shortText(initializer) : ""
          ],
          70
        )
      });
    }
    return infos;
  }

  function loopInfosFromLine(line, typeMap) {
    const infos = [];
    const pattern = /\bfor(?:<([^>]+)>)?\s+([A-Za-z_][A-Za-z0-9_']*)\s+in\s+([^{;\n]+)/g;
    let match;
    while ((match = pattern.exec(line)) !== null) {
      const [, mode, name, range] = match;
      infos.push({
        name,
        info: hover(
          name,
          [
            "kind: loop variable",
            typeMap.get(name) ? "type: " + typeMap.get(name) : "type: inferred/unknown",
            mode ? "mode: " + mode.trim() : "",
            "range: " + shortText(range)
          ],
          65
        )
      });
    }
    return infos;
  }

  function callInfosFromLine(line) {
    const infos = [];
    const pattern = /\b([A-Za-z_][A-Za-z0-9_']*)\s*(?:<[^>\n(){};]*>)?\s*\(([^()\n;]*)\)/g;
    let match;
    while ((match = pattern.exec(line)) !== null) {
      const [full, name, args] = match;
      if (syntaxKeywords.has(name)) {
        continue;
      }
      infos.push({
        name,
        info: hover(
          name,
          [
            "kind: call expression",
            "callee: " + name,
            "arguments: " + countArgs(args),
            "expression: " + shortText(full)
          ],
          40
        )
      });
    }
    return infos;
  }

  function buildHoverIndex(lines, typeSource) {
    const typeMap = buildTypeMap(typeSource);
    const index = new Map();
    for (const [name, typ] of typeMap.entries()) {
      addHover(index, name, hover(name, ["kind: variable", "type: " + typ], 50));
    }
    for (const line of lines) {
      const functionInfo = functionInfoFromLine(line);
      if (functionInfo) {
        addHover(index, functionInfo.name, functionInfo.info);
      }
      for (const info of letInfosFromLine(line, typeMap)) {
        addHover(index, info.name, info.info);
      }
      for (const info of loopInfosFromLine(line, typeMap)) {
        addHover(index, info.name, info.info);
      }
      for (const info of callInfosFromLine(line)) {
        addHover(index, info.name, info.info);
      }
    }
    return { index, typeMap };
  }

  function lineKeywordHover(keyword, line, typeMap) {
    if (keyword === "fun") {
      const info = functionInfoFromLine(line);
      return info ? info.info : hover("fun", ["kind: function declaration"], 10);
    }
    if (keyword === "let" || keyword === "letmut") {
      const infos = letInfosFromLine(line, typeMap).filter((info) => info.keyword === keyword);
      if (infos.length === 0) {
        return hover(keyword, ["kind: " + (keyword === "letmut" ? "mutable binding" : "binding")], 10);
      }
      const lines = [];
      for (const info of infos) {
        lines.push(info.name + ":");
        lines.push(...info.info.lines.map((lineInfo) => "  " + lineInfo));
      }
      return hover(keyword, lines, 70);
    }
    return undefined;
  }

  function setHoverAttributes(element, info, typ) {
    element.classList.add("opti-hover-symbol");
    element.setAttribute("data-opti-hover-title", info.title);
    element.setAttribute("data-opti-hover-info", info.lines.join("\n"));
    if (typ) {
      element.setAttribute("data-opti-type", typ);
    }
  }

  function annotateSemanticTextNodes(root, hoverData) {
    if (root.dataset.optiAnnotated === "true") {
      return;
    }
    const { index, typeMap } = hoverData;
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
      acceptNode(node) {
        const parent = node.parentElement;
        if (!parent || !node.nodeValue || parent.closest("[data-opti-hover-info], [data-opti-type], .opti-group-button, .opti-info-popup, .opti-search-widget")) {
          return NodeFilter.FILTER_REJECT;
        }
        return NodeFilter.FILTER_ACCEPT;
      }
    });
    const nodes = [];
    while (walker.nextNode()) {
      nodes.push(walker.currentNode);
    }
    for (const node of nodes) {
      const text = node.nodeValue || "";
      const pattern = /\b(?:fun|letmut|let|[A-Za-z_][A-Za-z0-9_']*)\b/g;
      const fragment = document.createDocumentFragment();
      let offset = 0;
      let match;
      while ((match = pattern.exec(text)) !== null) {
        const name = match[0];
        const line = node.parentElement?.closest(".opti-code-line-content, .d2h-code-line-ctn")?.getAttribute("data-opti-line-text") || "";
        const info = lineKeywordHover(name, line, typeMap) || index.get(name);
        if (!info) {
          continue;
        }
        if (match.index > offset) {
          fragment.appendChild(document.createTextNode(text.slice(offset, match.index)));
        }
        const symbol = document.createElement("span");
        symbol.textContent = name;
        setHoverAttributes(symbol, info, typeMap.get(name));
        fragment.appendChild(symbol);
        offset = match.index + name.length;
      }
      if (offset < text.length) {
        fragment.appendChild(document.createTextNode(text.slice(offset)));
      }
      node.parentNode.replaceChild(fragment, node);
    }
    root.dataset.optiAnnotated = "true";
  }

  function contextKeyFromLine(line) {
    const compact = normalizeDiffLine(line);
    if (compact === "") {
      return undefined;
    }
    const functionMatch = compact.match(/\b(?:ghost\s+)?fun\s+([A-Za-z_][A-Za-z0-9_']*)\b/);
    if (functionMatch) {
      return "fun:" + functionMatch[1];
    }
    const loopMatch = compact.match(/\b(?:desync_)?for(?:<[^>]+>)?\s+([A-Za-z_][A-Za-z0-9_']*)\s+in\b/);
    if (loopMatch) {
      return "for:" + loopMatch[1];
    }
    const whileMatch = compact.match(/\bwhile\s*\(([^)]*)\)/);
    if (whileMatch) {
      return "while:" + shortText(whileMatch[1], 80);
    }
    const ifMatch = compact.match(/\bif\s*\(([^)]*)\)/);
    if (ifMatch) {
      return "if:" + shortText(ifMatch[1], 80);
    }
    const callMatch = compact.match(/\b([A-Za-z_][A-Za-z0-9_']*)\s*(?:<[^>\n(){};]*>)?\s*\(/);
    if (callMatch && !syntaxKeywords.has(callMatch[1])) {
      return "call:" + callMatch[1];
    }
    const letMatch = compact.match(/\blet(?:mut)?\s+([A-Za-z_][A-Za-z0-9_']*)\b/);
    if (letMatch) {
      return "let:" + letMatch[1];
    }
    if (/[{;]$/u.test(compact)) {
      return "stmt:" + shortText(compact, 100);
    }
    return undefined;
  }

  function nearestContextBefore(rows, start, predicate) {
    for (let index = start - 1; index >= Math.max(0, start - 100); index--) {
      if (classifyLine(rows[index].text)) {
        continue;
      }
      const key = contextKeyFromLine(rows[index].text);
      if (key && predicate(key)) {
        return key;
      }
    }
    return undefined;
  }

  function contextForGroup(rows, start, end, kind) {
    const functionContext = nearestContextBefore(rows, start, (key) => key.startsWith("fun:"));
    for (let index = start - 1; index >= Math.max(0, start - 30); index--) {
      if (!classifyLine(rows[index].text)) {
        const key = contextKeyFromLine(rows[index].text);
        if (key) {
          if (key.startsWith("fun:")) {
            return key;
          }
          return functionContext ? functionContext + ">" + key : key;
        }
      }
    }
    for (let index = end; index < Math.min(rows.length, end + 30); index++) {
      if (!classifyLine(rows[index].text)) {
        const key = contextKeyFromLine(rows[index].text);
        if (key) {
          if (key.startsWith("fun:")) {
            return key;
          }
          return functionContext ? functionContext + ">" + key : key;
        }
      }
    }
    return functionContext ? functionContext + ">unscoped:" + kind : "unscoped:" + kind;
  }

  function replaceLineWithGroup(cell, kind, index, lines, metadata) {
    cell.textContent = "";
    cell.appendChild(makeGroupButton(kind, index, lines, metadata));
  }

  function collapseRows(rows, options = {}) {
    let ghostIndex = 0;
    let contractIndex = 0;
    let cursor = 0;
    while (cursor < rows.length) {
      const current = rows[cursor];
      const kind = classifyLine(current.text);
      if (!kind) {
        cursor += 1;
        continue;
      }

      const group = [current];
      let next = cursor + 1;
      if (kind === "contract") {
        let previousLineComplete = contractLineIsComplete(current.text);
        while (next < rows.length) {
          const nextKind = classifyLine(rows[next].text);
          if (nextKind === "contract") {
            group.push(rows[next]);
            previousLineComplete = contractLineIsComplete(rows[next].text);
            next += 1;
            continue;
          }
          if (!previousLineComplete && nextKind === undefined && rows[next].text.trim() !== "") {
            group.push(rows[next]);
            previousLineComplete = contractLineIsComplete(rows[next].text);
            next += 1;
            continue;
          }
          break;
        }
      } else {
        while (next < rows.length && classifyLine(rows[next].text) === kind) {
          group.push(rows[next]);
          next += 1;
        }
      }

      const index = kind === "ghost" ? ++ghostIndex : ++contractIndex;
      replaceLineWithGroup(group[0].cell, kind, index, group.map((row) => row.text), {
        side: options.side || group[0].side || "single",
        context: contextForGroup(rows, cursor, next, kind),
        file: group[0].file || "",
        order: group[0].order ?? cursor
      });
      for (let i = 1; i < group.length; i++) {
        group[i].hideTarget.classList.add("opti-collapsed-line");
      }
      cursor = next;
    }
  }

  function enhanceSemanticHovers(root) {
    root.querySelectorAll("[data-opti-hover-info], [data-opti-type]").forEach((element) => {
      element.classList.add("opti-hover-symbol");
    });
  }

  function showHoverPopup(element) {
    const info = element.getAttribute("data-opti-hover-info");
    const typ = element.getAttribute("data-opti-type");
    if (info) {
      showPopup(element, element.getAttribute("data-opti-hover-title") || element.textContent || "symbol", info.split("\n"));
    } else if (typ) {
      showPopup(element, element.textContent || "symbol", ["kind: variable", "type: " + typ]);
    }
  }

  function hoverTargetFromEvent(event) {
    const target = event.target instanceof Element ? event.target : undefined;
    return target?.closest("[data-opti-hover-info], [data-opti-type]");
  }

  function containsRelatedTarget(element, relatedTarget) {
    return relatedTarget instanceof Node && element.contains(relatedTarget);
  }

  function rowsFromDiffTable(table) {
    const side = sideForDiffTable(table);
    const file = fileForDiffTable(table);
    return Array.from(table.querySelectorAll("tr"))
      .map((row, order) => {
        const cell = row.querySelector(".d2h-code-line-ctn");
        return cell ? { row, hideTarget: row, cell, text: cell.textContent || "", side, file, order } : undefined;
      })
      .filter(Boolean);
  }

  function sideForDiffTable(table) {
    const sideContainer = table.closest(".d2h-file-side-diff");
    if (!sideContainer || !sideContainer.parentElement) {
      return "single";
    }
    const sides = Array.from(sideContainer.parentElement.children).filter((child) => child.classList.contains("d2h-file-side-diff"));
    return sides.indexOf(sideContainer) === 0 ? "old" : "new";
  }

  function fileForDiffTable(table) {
    const wrapper = table.closest(".d2h-file-wrapper");
    return wrapper?.id || "";
  }

  function enhanceDiff(root, representation, typeSource) {
    closePopup();
    if (representation === "cpp") {
      return;
    }
    const signature = representation + ":" + root.querySelectorAll(".d2h-code-line-ctn").length + ":" + root.textContent.length;
    if (root.dataset.optiInteractiveSignature === signature && root.querySelector(".opti-group-button, .opti-hover-symbol")) {
      return;
    }
    root.querySelectorAll(".opti-collapsed-line").forEach((row) => row.classList.remove("opti-collapsed-line"));
    const rows = Array.from(root.querySelectorAll(".d2h-code-line-ctn"));
    const lines = rows.map((cell) => cell.textContent || "");
    const hoverData = buildHoverIndex(lines, representation === "surface" ? typeSource : undefined);
    rows.forEach((cell, index) => cell.setAttribute("data-opti-line-text", lines[index] || ""));
    rows.forEach((cell) => annotateSemanticTextNodes(cell, hoverData));
    root.querySelectorAll(".d2h-diff-tbody").forEach((table) => collapseRows(rowsFromDiffTable(table), { side: sideForDiffTable(table) }));
    enhanceSemanticHovers(root);
    root.dataset.optiInteractiveSignature = signature;
  }

  function enhanceCode(container, code, representation, typeSource) {
    closePopup();
    container.textContent = "";
    const pre = document.createElement("pre");
    pre.className = "opti-rich-code";
    const rows = code.split("\n").map((line) => {
      const row = document.createElement("div");
      row.className = "opti-code-line";
      const content = document.createElement("span");
      content.className = "opti-code-line-content";
      content.setAttribute("data-opti-line-text", line);
      content.textContent = line === "" ? " " : line;
      row.appendChild(content);
      pre.appendChild(row);
      return { row, cell: content, text: line };
    });
    container.appendChild(pre);
    if (representation !== "cpp") {
      const finish = () => {
        const hoverData = buildHoverIndex(rows.map((row) => row.text), representation === "surface" ? typeSource : undefined);
        pre.querySelectorAll(".opti-code-line-content").forEach((line) => annotateSemanticTextNodes(line, hoverData));
        collapseRows(rows.map((row) => ({ ...row, hideTarget: row.row })));
        enhanceSemanticHovers(container);
      };
      if (window.OptitrustSyntaxHighlight && window.OptitrustSyntaxHighlight.highlightCodeBlock) {
        window.OptitrustSyntaxHighlight.highlightCodeBlock(pre, "optilambda").then(finish).catch(() => {
          pre.dataset.optitrustHighlighter = "fallback";
          finish();
        });
      } else {
        pre.dataset.optitrustHighlighter = "fallback";
        finish();
      }
    }
  }

  let searchWidget;
  let searchInput;
  let searchCount;
  let searchIndex = -1;

  function clearSearchMarks() {
    document.querySelectorAll("mark.opti-search-match").forEach((mark) => {
      const parent = mark.parentNode;
      if (!parent) {
        return;
      }
      parent.replaceChild(document.createTextNode(mark.textContent || ""), mark);
      parent.normalize();
    });
    searchIndex = -1;
  }

  function isSearchableTextNode(node) {
    const parent = node.parentElement;
    if (!parent || !node.nodeValue || node.nodeValue.trim() === "") {
      return false;
    }
    if (parent.closest(".opti-search-widget, .opti-info-popup, script, style, textarea, input, button")) {
      return false;
    }
    return parent.getClientRects().length > 0;
  }

  function searchRoots() {
    return Array.from(document.querySelectorAll("#diffDiv, #sourceDiv, #treeDiv"))
      .filter((root) => root.getClientRects().length > 0);
  }

  function markMatches(root, query) {
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
      acceptNode(node) {
        return isSearchableTextNode(node) ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT;
      }
    });
    const nodes = [];
    while (walker.nextNode()) {
      nodes.push(walker.currentNode);
    }

    const needle = query.toLowerCase();
    for (const node of nodes) {
      const text = node.nodeValue || "";
      const lower = text.toLowerCase();
      let start = 0;
      let index = lower.indexOf(needle, start);
      if (index < 0) {
        continue;
      }

      const fragment = document.createDocumentFragment();
      while (index >= 0) {
        if (index > start) {
          fragment.appendChild(document.createTextNode(text.slice(start, index)));
        }
        const mark = document.createElement("mark");
        mark.className = "opti-search-match";
        mark.textContent = text.slice(index, index + query.length);
        fragment.appendChild(mark);
        start = index + query.length;
        index = lower.indexOf(needle, start);
      }
      if (start < text.length) {
        fragment.appendChild(document.createTextNode(text.slice(start)));
      }
      node.parentNode.replaceChild(fragment, node);
    }
  }

  function matches() {
    return Array.from(document.querySelectorAll("mark.opti-search-match"));
  }

  function updateSearchCount() {
    const total = matches().length;
    if (!searchCount) {
      return;
    }
    searchCount.textContent = total === 0 ? "No results" : (searchIndex + 1) + " / " + total;
  }

  function selectSearchMatch(index) {
    const all = matches();
    all.forEach((mark) => mark.classList.remove("opti-search-current"));
    if (all.length === 0) {
      searchIndex = -1;
      updateSearchCount();
      return;
    }
    searchIndex = (index + all.length) % all.length;
    const current = all[searchIndex];
    current.classList.add("opti-search-current");
    current.scrollIntoView({ block: "center", inline: "nearest" });
    updateSearchCount();
  }

  function runSearch(query) {
    clearSearchMarks();
    if (!query) {
      updateSearchCount();
      return;
    }
    searchRoots().forEach((root) => markMatches(root, query));
    selectSearchMatch(0);
  }

  function openSearchWidget() {
    if (!searchWidget) {
      searchWidget = document.createElement("div");
      searchWidget.className = "opti-search-widget";
      searchWidget.innerHTML =
        '<input class="opti-search-input" type="search" aria-label="Search in OptiTrust view">' +
        '<span class="opti-search-count"></span>' +
        '<button class="opti-search-prev" type="button" title="Previous match">Prev</button>' +
        '<button class="opti-search-next" type="button" title="Next match">Next</button>' +
        '<button class="opti-search-close" type="button" title="Close search">Close</button>';
      document.body.appendChild(searchWidget);
      searchInput = searchWidget.querySelector(".opti-search-input");
      searchCount = searchWidget.querySelector(".opti-search-count");

      searchInput.addEventListener("input", () => runSearch(searchInput.value));
      searchInput.addEventListener("keydown", (event) => {
        if (event.key === "Enter") {
          event.preventDefault();
          selectSearchMatch(searchIndex + (event.shiftKey ? -1 : 1));
        } else if (event.key === "Escape") {
          event.preventDefault();
          closeSearchWidget();
        }
      });
      searchWidget.querySelector(".opti-search-prev").addEventListener("click", () => selectSearchMatch(searchIndex - 1));
      searchWidget.querySelector(".opti-search-next").addEventListener("click", () => selectSearchMatch(searchIndex + 1));
      searchWidget.querySelector(".opti-search-close").addEventListener("click", closeSearchWidget);
    }

    searchWidget.hidden = false;
    searchInput.focus();
    searchInput.select();
    runSearch(searchInput.value);
  }

  function closeSearchWidget() {
    if (searchWidget) {
      searchWidget.hidden = true;
    }
    clearSearchMarks();
  }

  document.addEventListener("click", (event) => {
    const target = event.target instanceof Element ? event.target : undefined;
    if (target?.closest(".opti-popup-mode-toggle")) {
      event.preventDefault();
      event.stopPropagation();
      toggleActiveGroupPopupMode();
      return;
    }
    const groupButton = target?.closest(".opti-group-button");
    if (groupButton) {
      event.preventDefault();
      event.stopPropagation();
      toggleGroupPopup(groupButton);
      return;
    }
    if (!target || !target.closest(".opti-info-popup")) {
      closePopup();
    }
  });
  document.addEventListener("mouseover", (event) => {
    const target = hoverTargetFromEvent(event);
    if (!target || containsRelatedTarget(target, event.relatedTarget)) {
      return;
    }
    showHoverPopup(target);
  });
  document.addEventListener("mouseout", (event) => {
    const target = hoverTargetFromEvent(event);
    if (!target || containsRelatedTarget(target, event.relatedTarget)) {
      return;
    }
    closePopup();
  });
  function handleGlobalKeyDown(event) {
    if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "f") {
      event.preventDefault();
      event.stopPropagation();
      event.stopImmediatePropagation();
      openSearchWidget();
      return;
    }
    if (event.key === "Escape") {
      closePopup();
    }
  }

  window.addEventListener("keydown", handleGlobalKeyDown, true);
  document.addEventListener("keydown", handleGlobalKeyDown, true);

  window.OptitrustInteractiveCode = {
    enhanceDiff,
    enhanceCode,
    openSearchWidget,
    closePopup
  };
}());
