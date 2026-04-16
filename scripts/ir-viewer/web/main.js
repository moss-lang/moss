import { instance as createViz } from "@viz-js/viz";

const dom = {
  status: document.querySelector("#status"),
  summary: document.querySelector("#summary"),
  graphMeta: document.querySelector("#graph-meta"),
  lines: document.querySelector("#lines"),
  graph: document.querySelector("#graph"),
  filter: document.querySelector("#filter"),
  reload: document.querySelector("#reload"),
  selectVisible: document.querySelector("#select-visible"),
  clearVisible: document.querySelector("#clear-visible"),
  clearAll: document.querySelector("#clear-all"),
  autoReload: document.querySelector("#auto-reload"),
  showSelectedOnly: document.querySelector("#show-selected-only"),
};

const state = {
  ir: null,
  selectedLines: new Set(),
  filterText: "",
  showSelectedOnly: false,
  viz: null,
  renderToken: 0,
  lastMtimeMs: null,
};

const NODE_SHAPES = {
  Nothing: "plaintext",
  Lambda: "ellipse",
  Apply: "box",
  List: "folder",
  NeedTydef: "box",
  NeedSigdef: "box",
  NeedValdef: "box",
  NeedCtxdef: "box",
  Tagdef: "note",
  Aliasdef: "note",
  Tuple: "box3d",
  Context: "component",
  Fndef: "note",
  Get: "box",
  Lit: "note",
  Bind: "box",
  BindTydef: "box",
  BindSigdef: "box",
  BindValdef: "box",
  BindCtxdef: "box",
  Sig: "diamond",
};

main().catch((error) => {
  dom.status.textContent = `Failed to start viewer: ${String(error)}`;
});

async function main() {
  state.viz = await createViz();
  wireEvents();
  await loadIr({ preserveSelection: false });
  setInterval(pollForUpdates, 1500);
}

function wireEvents() {
  dom.reload.addEventListener("click", () => {
    void loadIr({ preserveSelection: true });
  });

  dom.filter.addEventListener("input", () => {
    state.filterText = dom.filter.value;
    renderLines();
    updateSummary();
  });

  dom.showSelectedOnly.addEventListener("change", () => {
    state.showSelectedOnly = dom.showSelectedOnly.checked;
    renderLines();
    updateSummary();
  });

  dom.selectVisible.addEventListener("click", () => {
    for (const line of visibleLines()) {
      if (line.rootIds.length > 0) {
        state.selectedLines.add(line.index);
      }
    }
    renderLines();
    updateSummary();
    void renderGraph();
  });

  dom.clearVisible.addEventListener("click", () => {
    for (const line of visibleLines()) {
      state.selectedLines.delete(line.index);
    }
    renderLines();
    updateSummary();
    void renderGraph();
  });

  dom.clearAll.addEventListener("click", () => {
    state.selectedLines.clear();
    renderLines();
    updateSummary();
    void renderGraph();
  });

  dom.lines.addEventListener("change", (event) => {
    const target = event.target;
    if (!(target instanceof HTMLInputElement)) {
      return;
    }
    const rawIndex = target.getAttribute("data-line");
    if (rawIndex == null) {
      return;
    }
    const lineIndex = Number(rawIndex);
    if (target.checked) {
      state.selectedLines.add(lineIndex);
    } else {
      state.selectedLines.delete(lineIndex);
    }
    syncLineSelection(target.closest(".line"), target.checked);
    updateSummary();
    void renderGraph();
  });

  dom.lines.addEventListener("click", (event) => {
    const target = event.target;
    if (!(target instanceof HTMLElement)) {
      return;
    }
    if (target.tagName === "INPUT") {
      return;
    }
    const row = target.closest(".line");
    if (!row) {
      return;
    }
    const checkbox = row.querySelector('input[type="checkbox"]');
    if (!(checkbox instanceof HTMLInputElement) || checkbox.disabled) {
      return;
    }
    checkbox.checked = !checkbox.checked;
    checkbox.dispatchEvent(new Event("change", { bubbles: true }));
  });
}

async function pollForUpdates() {
  if (!dom.autoReload.checked) {
    return;
  }

  try {
    const response = await fetch("/api/meta", { cache: "no-store" });
    if (!response.ok) {
      return;
    }
    const meta = await response.json();
    if (state.lastMtimeMs == null || meta.mtimeMs !== state.lastMtimeMs) {
      await loadIr({ preserveSelection: true });
    }
  } catch {
    // Ignore polling failures; the explicit reload button stays available.
  }
}

async function loadIr({ preserveSelection }) {
  dom.status.textContent = "Loading IR dump…";

  const response = await fetch("/api/ir", { cache: "no-store" });
  const payload = await response.json();
  if (!response.ok) {
    dom.status.textContent = payload.error ?? "Failed to load IR dump";
    state.ir = null;
    renderLines();
    updateSummary();
    await renderGraph();
    return;
  }

  const previous = preserveSelection ? new Set(state.selectedLines) : new Set();
  state.ir = parseIr(payload.text, payload.path);
  state.lastMtimeMs = payload.mtimeMs;
  state.selectedLines.clear();
  for (const line of state.ir.lines) {
    if (previous.has(line.index) && line.rootIds.length > 0) {
      state.selectedLines.add(line.index);
    }
  }

  dom.status.textContent = `${payload.path} • ${state.ir.lines.length} lines • ${state.ir.nodes.size} nodes`;
  renderLines();
  updateSummary();
  await renderGraph();
}

function parseIr(text, path) {
  const lines = [];
  const nodes = new Map();
  const defsByNode = new Map();
  const rawLines = text.replace(/\r/g, "").split("\n");

  for (let index = 0; index < rawLines.length; index += 1) {
    const raw = rawLines[index];
    if (raw.length === 0) {
      continue;
    }

    const nodeMatch = raw.match(/^%(\d+) = (.+)$/);
    if (nodeMatch) {
      const nodeId = Number(nodeMatch[1]);
      const body = nodeMatch[2];
      const node = parseNode(nodeId, body);
      nodes.set(nodeId, node);
      lines.push({
        index,
        text: raw,
        kind: "node",
        rootIds: [nodeId],
      });
      continue;
    }

    const defMatch = raw.match(/^(module|type|tag|alias|sig|fn|val|context) #\d+(?: "(?:[^"\\]|\\.)*")? = %(\d+)$/);
    if (defMatch) {
      const nodeId = Number(defMatch[2]);
      const defs = defsByNode.get(nodeId) ?? [];
      defs.push(raw.slice(0, raw.lastIndexOf(" = ")));
      defsByNode.set(nodeId, defs);
      lines.push({
        index,
        text: raw,
        kind: "def",
        rootIds: [nodeId],
      });
      continue;
    }

    lines.push({
      index,
      text: raw,
      kind: "other",
      rootIds: [],
    });
  }

  return { lines, nodes, defsByNode, path };
}

function parseNode(nodeId, body) {
  let match = body.match(/^lambda\((\d+)\) need \[(.*)\] return %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Lambda",
      level: match[1],
      needs: parseRefs(match[2]),
      result: Number(match[3]),
      text: body,
    };
  }

  match = body.match(/^apply %(\d+) to \[(.*)\]$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Apply",
      lambda: Number(match[1]),
      args: parseRefs(match[2]),
      text: body,
    };
  }

  match = body.match(/^\[(.*)\]$/);
  if (match) {
    return {
      id: nodeId,
      kind: "List",
      items: parseRefs(match[1]),
      text: body,
    };
  }

  match = body.match(/^need\((\d+)\) (.+) via %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: namedKind("Need", match[2]),
      level: match[1],
      named: match[2],
      param: Number(match[3]),
      text: body,
    };
  }

  match = body.match(/^tuple type \((.*)\)$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Tuple",
      elems: parseRefs(match[1]),
      text: body,
    };
  }

  match = body.match(/^slot (\d+) of %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Get",
      slot: match[1],
      ctx: Number(match[2]),
      text: body,
    };
  }

  match = body.match(/^bind for \[(.*)\] using %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Bind",
      args: parseRefs(match[1]),
      bind: Number(match[2]),
      text: body,
    };
  }

  match = body.match(/^bind (.+) via %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: namedKind("Bind", match[1]),
      named: match[1],
      bind: Number(match[2]),
      text: body,
    };
  }

  match = body.match(/^function signature %(\d+) -> %(\d+)$/);
  if (match) {
    return {
      id: nodeId,
      kind: "Sig",
      param: Number(match[1]),
      result: Number(match[2]),
      text: body,
    };
  }

  if (body === "nothing") {
    return { id: nodeId, kind: "Nothing", text: body };
  }
  if (body === "context type") {
    return { id: nodeId, kind: "Context", text: body };
  }
  if (body.startsWith("tag #")) {
    return { id: nodeId, kind: "Tagdef", named: body, text: body };
  }
  if (body.startsWith("alias #")) {
    return { id: nodeId, kind: "Aliasdef", named: body, text: body };
  }
  if (body.startsWith("fn #")) {
    return { id: nodeId, kind: "Fndef", named: body, text: body };
  }
  if (
    /^(uint31|uint32|int32|uint63|uint64|int64|uint|int|string|c )/.test(body)
  ) {
    return { id: nodeId, kind: "Lit", text: body };
  }

  throw new Error(`Unrecognized IR node syntax for %${nodeId}: ${body}`);
}

function namedKind(prefix, text) {
  if (text.startsWith("type #")) {
    return `${prefix}Tydef`;
  }
  if (text.startsWith("sig #")) {
    return `${prefix}Sigdef`;
  }
  if (text.startsWith("val #")) {
    return `${prefix}Valdef`;
  }
  if (text.startsWith("context #")) {
    return `${prefix}Ctxdef`;
  }
  return prefix;
}

function parseRefs(text) {
  if (!text.trim()) {
    return [];
  }
  return [...text.matchAll(/%(\d+)/g)].map((match) => Number(match[1]));
}

function nodeEdges(node) {
  switch (node.kind) {
    case "Lambda":
      return [...node.needs, node.result];
    case "Apply":
      return [node.lambda, ...node.args];
    case "List":
      return [...node.items];
    case "NeedTydef":
    case "NeedSigdef":
    case "NeedValdef":
    case "NeedCtxdef":
      return [node.param];
    case "Tuple":
      return [...node.elems];
    case "Get":
      return [node.ctx];
    case "Bind":
      return [...node.args, node.bind];
    case "BindTydef":
    case "BindSigdef":
    case "BindValdef":
    case "BindCtxdef":
      return [node.bind];
    case "Sig":
      return [node.param, node.result];
    default:
      return [];
  }
}

function visibleLines() {
  if (!state.ir) {
    return [];
  }

  const needle = state.filterText.trim().toLowerCase();
  return state.ir.lines.filter((line) => {
    if (state.showSelectedOnly && !state.selectedLines.has(line.index)) {
      return false;
    }
    if (!needle) {
      return true;
    }
    return line.text.toLowerCase().includes(needle);
  });
}

function renderLines() {
  dom.lines.replaceChildren();
  if (!state.ir) {
    const div = document.createElement("div");
    div.className = "empty";
    div.textContent = "No IR dump loaded.";
    dom.lines.append(div);
    return;
  }

  const fragment = document.createDocumentFragment();
  for (const line of visibleLines()) {
    const row = document.createElement("label");
    row.className = `line ${line.kind}`;
    if (state.selectedLines.has(line.index)) {
      row.classList.add("selected");
    }

    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.checked = state.selectedLines.has(line.index);
    checkbox.disabled = line.rootIds.length === 0;
    checkbox.setAttribute("data-line", String(line.index));

    const number = document.createElement("span");
    number.className = "line-number";
    number.textContent = String(line.index + 1);

    const text = document.createElement("span");
    text.className = "line-text";
    text.textContent = line.text;

    row.append(checkbox, number, text);
    fragment.append(row);
  }
  dom.lines.append(fragment);
}

function syncLineSelection(row, checked) {
  if (!row) {
    return;
  }
  row.classList.toggle("selected", checked);
}

function updateSummary() {
  if (!state.ir) {
    dom.summary.textContent = "";
    return;
  }

  const total = state.ir.lines.length;
  const visible = visibleLines().length;
  const selected = state.selectedLines.size;
  dom.summary.textContent = `${selected} selected • ${visible}/${total} visible`;
}

async function renderGraph() {
  const token = ++state.renderToken;

  if (!state.ir) {
    dom.graphMeta.textContent = "";
    dom.graph.replaceChildren(emptyNode("No IR dump loaded."));
    return;
  }

  const focus = buildFocus(state.ir, state.selectedLines);
  if (focus.roots.size === 0) {
    dom.graphMeta.textContent = "Select one or more node or definition lines to seed the graph.";
    dom.graph.replaceChildren(emptyNode("No roots selected."));
    return;
  }

  dom.graphMeta.textContent = `${focus.roots.size} roots • ${focus.keep.size} reachable nodes • ${focus.materialized.size} rendered nodes`;
  dom.graph.replaceChildren(emptyNode("Rendering graph…"));

  try {
    const dot = renderDot(state.ir, focus);
    const svg = await state.viz.renderSVGElement(dot);
    if (token !== state.renderToken) {
      return;
    }
    dom.graph.replaceChildren(svg);
  } catch (error) {
    if (token !== state.renderToken) {
      return;
    }
    dom.graph.replaceChildren(errorNode(String(error)));
  }
}

function buildFocus(ir, selectedLines) {
  const roots = new Map();
  const queue = [];

  for (const line of ir.lines) {
    if (!selectedLines.has(line.index)) {
      continue;
    }
    for (const nodeId of line.rootIds) {
      const labels = roots.get(nodeId) ?? [];
      labels.push(rootLabel(line));
      roots.set(nodeId, labels);
      queue.push(nodeId);
    }
  }

  const keep = new Set();
  while (queue.length > 0) {
    const nodeId = queue.shift();
    if (keep.has(nodeId)) {
      continue;
    }
    keep.add(nodeId);
    const node = ir.nodes.get(nodeId);
    if (!node) {
      continue;
    }
    for (const next of nodeEdges(node)) {
      queue.push(next);
    }
  }

  const incoming = new Map();
  for (const nodeId of keep) {
    const node = ir.nodes.get(nodeId);
    if (!node) {
      continue;
    }
    for (const next of nodeEdges(node)) {
      if (keep.has(next)) {
        incoming.set(next, (incoming.get(next) ?? 0) + 1);
      }
    }
  }

  const materialized = new Set();
  for (const nodeId of keep) {
    if (roots.has(nodeId) || (incoming.get(nodeId) ?? 0) !== 1) {
      materialized.add(nodeId);
    }
  }

  return { roots, keep, materialized };
}

function rootLabel(line) {
  if (line.kind === "def") {
    return line.text.slice(0, line.text.lastIndexOf(" = "));
  }
  if (line.kind === "node") {
    return line.text.slice(0, line.text.indexOf(" = "));
  }
  return `line ${line.index + 1}`;
}

function renderDot(ir, focus) {
  const parts = [];
  parts.push("digraph IR {");
  parts.push('  graph [rankdir=LR, splines=true, overlap=false, fontname="monospace"];');
  parts.push(
    '  node [shape=box, style="rounded,filled", fillcolor="#f8f8f8", color="#444444", fontname="monospace", fontsize=10];',
  );
  parts.push('  edge [color="#666666"];');

  const nodes = [...focus.materialized].sort((a, b) => a - b);
  for (const nodeId of nodes) {
    const node = ir.nodes.get(nodeId);
    const label = escapeDot(materializedLabel(ir, focus, nodeId));
    const fill = focus.roots.has(nodeId) ? "#fff2cc" : "#f8f8f8";
    parts.push(
      `  n${nodeId} [shape=${NODE_SHAPES[node?.kind] ?? "box"}, fillcolor="${fill}", label="${label}\\l"];`,
    );
  }

  for (const nodeId of nodes) {
    for (const next of materializedEdges(ir, focus.materialized, nodeId)) {
      parts.push(`  n${nodeId} -> n${next};`);
    }
  }

  parts.push("}");
  return parts.join("\n");
}

function materializedLabel(ir, focus, nodeId) {
  const lines = [`%${nodeId}`];
  for (const def of ir.defsByNode.get(nodeId) ?? []) {
    lines.push(def);
  }
  if (focus.roots.has(nodeId)) {
    lines.push(`roots: ${focus.roots.get(nodeId).join(", ")}`);
  }
  lines.push(...renderNodePretty(ir, focus.materialized, nodeId, 0, new Set()));
  return lines.join("\n");
}

function renderNodePretty(ir, materialized, nodeId, indentLevel, stack) {
  if (stack.has(nodeId)) {
    return [`${indent(indentLevel)}(cycle to %${nodeId})`];
  }
  stack.add(nodeId);

  const node = ir.nodes.get(nodeId);
  if (!node) {
    stack.delete(nodeId);
    return [`${indent(indentLevel)}(missing %${nodeId})`];
  }

  let lines;
  switch (node.kind) {
    case "Nothing":
      lines = [`${indent(indentLevel)}nothing`];
      break;
    case "Lambda":
      lines = [`${indent(indentLevel)}lambda(${node.level})`];
      if (node.needs.length === 0) {
        lines.push(`${indent(indentLevel + 1)}need []`);
      } else {
        lines.push(`${indent(indentLevel + 1)}need [`);
        for (const need of node.needs) {
          pushItemBlock(lines, renderRefPretty(ir, materialized, need, indentLevel + 2, stack));
        }
        lines.push(`${indent(indentLevel + 1)}]`);
      }
      lines.push(`${indent(indentLevel + 1)}return`);
      pushBlock(lines, renderRefPretty(ir, materialized, node.result, indentLevel + 2, stack));
      break;
    case "Apply":
      lines = [`${indent(indentLevel)}apply`];
      pushBlock(lines, renderRefPretty(ir, materialized, node.lambda, indentLevel + 1, stack));
      if (node.args.length === 0) {
        lines.push(`${indent(indentLevel)}to []`);
      } else {
        lines.push(`${indent(indentLevel)}to [`);
        for (const arg of node.args) {
          pushItemBlock(lines, renderRefPretty(ir, materialized, arg, indentLevel + 1, stack));
        }
        lines.push(`${indent(indentLevel)}]`);
      }
      break;
    case "List":
      if (node.items.length === 0) {
        lines = [`${indent(indentLevel)}[]`];
      } else {
        lines = [`${indent(indentLevel)}[`];
        for (const item of node.items) {
          pushItemBlock(lines, renderRefPretty(ir, materialized, item, indentLevel + 1, stack));
        }
        lines.push(`${indent(indentLevel)}]`);
      }
      break;
    case "NeedTydef":
    case "NeedSigdef":
    case "NeedValdef":
    case "NeedCtxdef":
      lines = [`${indent(indentLevel)}need(${node.level}) ${node.named} via`];
      pushBlock(lines, renderRefPretty(ir, materialized, node.param, indentLevel + 1, stack));
      break;
    case "Tagdef":
    case "Aliasdef":
    case "Fndef":
    case "Lit":
      lines = [`${indent(indentLevel)}${node.text}`];
      break;
    case "Tuple":
      if (node.elems.length === 0) {
        lines = [`${indent(indentLevel)}tuple type ()`];
      } else {
        lines = [`${indent(indentLevel)}tuple type (`];
        for (const elem of node.elems) {
          pushItemBlock(lines, renderRefPretty(ir, materialized, elem, indentLevel + 1, stack));
        }
        lines.push(`${indent(indentLevel)})`);
      }
      break;
    case "Context":
      lines = [`${indent(indentLevel)}context type`];
      break;
    case "Get":
      lines = [`${indent(indentLevel)}slot ${node.slot} of`];
      pushBlock(lines, renderRefPretty(ir, materialized, node.ctx, indentLevel + 1, stack));
      break;
    case "Bind":
      lines =
        node.args.length === 0
          ? [`${indent(indentLevel)}bind for [] using`]
          : [`${indent(indentLevel)}bind for [`];
      for (const arg of node.args) {
        pushItemBlock(lines, renderRefPretty(ir, materialized, arg, indentLevel + 1, stack));
      }
      if (node.args.length !== 0) {
        lines.push(`${indent(indentLevel)}] using`);
      }
      pushBlock(lines, renderRefPretty(ir, materialized, node.bind, indentLevel + 1, stack));
      break;
    case "BindTydef":
    case "BindSigdef":
    case "BindValdef":
    case "BindCtxdef":
      lines = [`${indent(indentLevel)}bind ${node.named} via`];
      pushBlock(lines, renderRefPretty(ir, materialized, node.bind, indentLevel + 1, stack));
      break;
    case "Sig":
      lines = [`${indent(indentLevel)}function signature`];
      pushBlock(lines, renderRefPretty(ir, materialized, node.param, indentLevel + 1, stack));
      lines.push(`${indent(indentLevel)}->`);
      pushBlock(lines, renderRefPretty(ir, materialized, node.result, indentLevel + 1, stack));
      break;
    default:
      lines = [`${indent(indentLevel)}${node.text}`];
      break;
  }

  stack.delete(nodeId);
  return lines;
}

function renderRefPretty(ir, materialized, nodeId, indentLevel, stack) {
  if (materialized.has(nodeId)) {
    return [`${indent(indentLevel)}%${nodeId}`];
  }

  const inner = renderNodePretty(ir, materialized, nodeId, indentLevel + 1, stack);
  if (inner.length === 1) {
    return [`${indent(indentLevel)}(${inner[0].trimStart()})`];
  }

  const out = inner.slice();
  out[0] = `${indent(indentLevel)}(${out[0].trimStart()}`;
  out[out.length - 1] = `${out[out.length - 1]})`;
  return out;
}

function materializedEdges(ir, materialized, nodeId) {
  const out = new Set();
  collectMaterializedEdges(ir, materialized, nodeId, new Set(), out);
  return [...out].sort((a, b) => a - b);
}

function collectMaterializedEdges(ir, materialized, nodeId, seen, out) {
  if (seen.has(nodeId)) {
    return;
  }
  seen.add(nodeId);
  const node = ir.nodes.get(nodeId);
  if (!node) {
    seen.delete(nodeId);
    return;
  }
  for (const next of nodeEdges(node)) {
    if (materialized.has(next)) {
      out.add(next);
    } else {
      collectMaterializedEdges(ir, materialized, next, seen, out);
    }
  }
  seen.delete(nodeId);
}

function pushBlock(lines, block) {
  lines.push(...block);
}

function pushItemBlock(lines, block) {
  const out = block.slice();
  if (out.length > 0) {
    out[out.length - 1] = `${out[out.length - 1]},`;
  }
  lines.push(...out);
}

function indent(level) {
  return "  ".repeat(level);
}

function escapeDot(text) {
  return text
    .replaceAll("\\", "\\\\")
    .replaceAll('"', '\\"')
    .replaceAll("\n", "\\l");
}

function emptyNode(text) {
  const div = document.createElement("div");
  div.className = "empty";
  div.textContent = text;
  return div;
}

function errorNode(text) {
  const pre = document.createElement("pre");
  pre.className = "error";
  pre.textContent = text;
  return pre;
}
