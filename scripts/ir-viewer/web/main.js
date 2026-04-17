const dom = {
  graphMeta: document.querySelector("#graph-meta"),
  lines: document.querySelector("#lines"),
  graph: document.querySelector("#graph"),
  filter: document.querySelector("#filter"),
  clearAll: document.querySelector("#clear-all"),
};

const state = {
  ir: null,
  selectedLines: new Set(),
  filterText: "",
  renderToken: 0,
  lastMtimeMs: null,
  graphView: {
    scale: 1,
    minScale: 0.08,
    maxScale: 4,
    offsetX: 0,
    offsetY: 0,
    pointerId: null,
    panStartX: 0,
    panStartY: 0,
    svgWidth: 0,
    svgHeight: 0,
  },
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
  console.error("Failed to start viewer", error);
});

async function main() {
  wireEvents();
  await loadIr({ preserveSelection: false });
  setInterval(pollForUpdates, 1500);
  window.addEventListener("resize", () => {
    fitGraphToViewport();
  });
}

function wireEvents() {
  dom.filter.addEventListener("input", () => {
    state.filterText = dom.filter.value;
    renderLines();
  });

  dom.clearAll.addEventListener("click", () => {
    state.selectedLines.clear();
    renderLines();
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
    // Each row is a label, so cancel the native label toggle before applying
    // the explicit state change. Otherwise row clicks toggle twice and flicker.
    event.preventDefault();
    checkbox.checked = !checkbox.checked;
    checkbox.dispatchEvent(new Event("change", { bubbles: true }));
  });

  dom.graph.addEventListener("wheel", onGraphWheel, { passive: false });
  dom.graph.addEventListener("pointerdown", onGraphPointerDown);
  dom.graph.addEventListener("pointermove", onGraphPointerMove);
  dom.graph.addEventListener("pointerup", onGraphPointerUp);
  dom.graph.addEventListener("pointercancel", onGraphPointerUp);
  dom.graph.addEventListener("pointerleave", onGraphPointerUp);
  dom.graph.addEventListener("dblclick", () => {
    fitGraphToViewport();
  });
}

async function pollForUpdates() {
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
    // Ignore polling failures; the next poll will retry.
  }
}

async function loadIr({ preserveSelection }) {
  const response = await fetch("/api/ir", { cache: "no-store" });
  const payload = await response.json();
  if (!response.ok) {
    state.ir = null;
    renderLines();
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

  renderLines();
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
    const response = await fetch("/api/render-svg", {
      method: "POST",
      headers: {
        "content-type": "application/json; charset=utf-8",
      },
      body: JSON.stringify({ dot }),
    });
    if (!response.ok) {
      const payload = await response.json().catch(() => null);
      throw new Error(payload?.error ?? `render failed with ${response.status}`);
    }
    const svgText = await response.text();
    if (token !== state.renderToken) {
      return;
    }
    mountGraph(svgText);
  } catch (error) {
    if (token !== state.renderToken) {
      return;
    }
    dom.graph.replaceChildren(errorNode(String(error)));
  }
}

function mountGraph(svgText) {
  const doc = new DOMParser().parseFromString(svgText, "image/svg+xml");
  const svg = doc.documentElement;
  if (!(svg instanceof SVGSVGElement) || svg.tagName !== "svg") {
    throw new Error("render response did not contain an SVG document");
  }

  const viewport = document.createElement("div");
  viewport.className = "graph-viewport";

  const canvas = document.createElement("div");
  canvas.className = "graph-canvas";
  canvas.append(svg);
  viewport.append(canvas);

  dom.graph.replaceChildren(viewport);

  const viewBox = svg.viewBox.baseVal;
  const width =
    viewBox && viewBox.width > 0
      ? viewBox.width
      : Number(svg.getAttribute("width") ?? "0");
  const height =
    viewBox && viewBox.height > 0
      ? viewBox.height
      : Number(svg.getAttribute("height") ?? "0");

  svg.setAttribute("width", String(width));
  svg.setAttribute("height", String(height));
  svg.style.width = `${width}px`;
  svg.style.height = `${height}px`;

  state.graphView.svgWidth = width;
  state.graphView.svgHeight = height;
  fitGraphToViewport();
}

function graphCanvas() {
  return dom.graph.querySelector(".graph-canvas");
}

function fitGraphToViewport() {
  const canvas = graphCanvas();
  if (!canvas) {
    return;
  }

  const { svgWidth, svgHeight, minScale, maxScale } = state.graphView;
  if (svgWidth <= 0 || svgHeight <= 0) {
    state.graphView.scale = 1;
    state.graphView.offsetX = 0;
    state.graphView.offsetY = 0;
    applyGraphTransform();
    return;
  }

  const bounds = dom.graph.getBoundingClientRect();
  const scaleX = (bounds.width - 32) / svgWidth;
  const scaleY = (bounds.height - 32) / svgHeight;
  const scale = clamp(Math.min(scaleX, scaleY, 1), minScale, maxScale);

  state.graphView.scale = scale;
  state.graphView.offsetX = (bounds.width - svgWidth * scale) / 2;
  state.graphView.offsetY = (bounds.height - svgHeight * scale) / 2;
  applyGraphTransform();
}

function applyGraphTransform() {
  const canvas = graphCanvas();
  if (!canvas) {
    return;
  }

  canvas.style.transform = `translate(${state.graphView.offsetX}px, ${state.graphView.offsetY}px) scale(${state.graphView.scale})`;
}

function onGraphWheel(event) {
  const canvas = graphCanvas();
  if (!canvas) {
    return;
  }

  event.preventDefault();

  const before = clientToGraphPoint(event.clientX, event.clientY);
  const factor = event.deltaY < 0 ? 1.12 : 1 / 1.12;
  state.graphView.scale = clamp(
    state.graphView.scale * factor,
    state.graphView.minScale,
    state.graphView.maxScale,
  );

  const rect = dom.graph.getBoundingClientRect();
  state.graphView.offsetX = event.clientX - rect.left - before.x * state.graphView.scale;
  state.graphView.offsetY = event.clientY - rect.top - before.y * state.graphView.scale;
  applyGraphTransform();
}

function onGraphPointerDown(event) {
  if (event.button !== 0) {
    return;
  }

  const canvas = graphCanvas();
  if (!canvas) {
    return;
  }

  state.graphView.pointerId = event.pointerId;
  state.graphView.panStartX = event.clientX - state.graphView.offsetX;
  state.graphView.panStartY = event.clientY - state.graphView.offsetY;
  dom.graph.classList.add("is-panning");
  dom.graph.setPointerCapture(event.pointerId);
}

function onGraphPointerMove(event) {
  if (state.graphView.pointerId !== event.pointerId) {
    return;
  }

  state.graphView.offsetX = event.clientX - state.graphView.panStartX;
  state.graphView.offsetY = event.clientY - state.graphView.panStartY;
  applyGraphTransform();
}

function onGraphPointerUp(event) {
  if (state.graphView.pointerId !== event.pointerId) {
    return;
  }

  state.graphView.pointerId = null;
  dom.graph.classList.remove("is-panning");
  if (dom.graph.hasPointerCapture(event.pointerId)) {
    dom.graph.releasePointerCapture(event.pointerId);
  }
}

function clientToGraphPoint(clientX, clientY) {
  const rect = dom.graph.getBoundingClientRect();
  return {
    x: (clientX - rect.left - state.graphView.offsetX) / state.graphView.scale,
    y: (clientY - rect.top - state.graphView.offsetY) / state.graphView.scale,
  };
}

function clamp(value, min, max) {
  return Math.min(Math.max(value, min), max);
}

function buildFocus(ir, selectedLines) {
  const roots = new Map();
  const queue = [];

  for (const line of ir.lines) {
    if (!selectedLines.has(line.index)) {
      continue;
    }
    for (const nodeId of line.rootIds) {
      roots.set(nodeId, true);
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
