#!/usr/bin/env python3
"""Validate the task DAG and generate its human-readable Mermaid roadmap."""

import argparse
from functools import cache
import json
import os
from pathlib import Path
import re
import subprocess


ROOT = Path(__file__).resolve().parent
TARGETS = {"G01", "G02", "G03", "G04", "G05"}
MARK = {"done": "✓", "partial": "◐", "open": "○"}
BUDGET = {"done": "Proved / defined", "hour": "≤1h, tentative", "unknown": "? Decompose"}


def validate(plan):
    assert plan["schema"] == 2, "Unsupported roadmap schema"
    nodes = {node["id"]: node for node in plan["nodes"]}
    assert len(nodes) == len(plan["nodes"]), "Duplicate task ID"
    groups = {group["id"] for group in plan["groups"]}
    assert len(groups) == len(plan["groups"]), "Duplicate group ID"
    for key, node in nodes.items():
        assert re.fullmatch(r"[A-Z][0-9]{2}", key), key
        assert node["group"] in groups, key
        assert node["status"] in MARK and node["budget"] in BUDGET, key
        assert (node["status"] == "done") == (node["budget"] == "done"), key
        assert node["goal"] and node["approach"] and node["sources"], key
        assert set(node["requires"]) <= nodes.keys(), key
        assert len(node["requires"]) == len(set(node["requires"])), key
        origin = node["origin"]
        assert origin["kind"] in {"baseline", "refinement", "discovery"}, key
        if origin["kind"] != "baseline":
            assert origin["parent"] in nodes and origin["parent"] != key, key
            if origin["kind"] == "refinement":
                assert origin.get("note"), f"Unexplained refinement: {key}"
            else:
                assert origin.get("reason") and origin.get("sources"), f"Unexplained discovery: {key}"
                assert all((ROOT / path).is_file() for path in origin["sources"]), key
        for source in node["sources"]:
            assert (ROOT / source).is_file(), (key, source)
        if "witness" in node:
            assert any(re.search(r"\b" + re.escape(node["witness"]) + r"\b",
                                 (ROOT / source).read_text()) for source in node["sources"]), key
        if node["status"] == "done":
            assert node.get("witness") or node.get("evidence"), f"Missing completion evidence: {key}"
            assert all(nodes[dep]["status"] == "done" for dep in node["requires"]), key
        for evidence in node.get("evidence", []):
            assert evidence.get("summary") and evidence.get("sources"), key
            assert all((ROOT / path).is_file() for path in evidence["sources"]), key
            if "witness" in evidence:
                assert any(re.search(r"\b" + re.escape(evidence["witness"]) + r"\b",
                                     (ROOT / path).read_text()) for path in evidence["sources"]), key
    visiting, visited = set(), set()

    def visit(key):
        assert key not in visiting, f"Cycle at {key}"
        if key not in visited:
            visiting.add(key)
            for dep in nodes[key]["requires"]:
                visit(dep)
            visiting.remove(key)
            visited.add(key)

    for key in nodes:
        visit(key)
    @cache
    def depends_on(parent, child):
        return any(dep == child or depends_on(dep, child) for dep in nodes[parent]["requires"])

    for key, node in nodes.items():
        if node["origin"]["kind"] != "baseline":
            assert depends_on(node["origin"]["parent"], key), f"Origin parent does not depend on {key}"
    nonterminal = {dep for node in nodes.values() for dep in node["requires"]}
    assert nodes.keys() - nonterminal == TARGETS, "Expected exactly five terminal nodes"
    assert nodes["G05"]["requires"] == ["T20"], "Safety must stay semantic"
    safety_ancestors = set()

    def ancestors(key):
        for dep in nodes[key]["requires"]:
            if dep not in safety_ancestors:
                safety_ancestors.add(dep)
                ancestors(dep)

    ancestors("G05")
    assert all(nodes[key]["group"] == "T" for key in safety_ancestors), (
        "Semantic safety must not wait for executable migration or agreement")
    assert all("X14" in nodes[key]["requires"] for key in ("G03", "G04")), (
        "The selected executable must pass the integrated bootstrap gate")
    superseded = plan.get("superseded", [])
    evidence_only = plan.get("evidence_only", [])
    retired_ids = {node["id"] for node in superseded + evidence_only}
    assert len(retired_ids) == len(superseded + evidence_only), "Duplicate retired ID"
    assert not retired_ids & nodes.keys(), "A superseded ID was reused"
    for node in superseded:
        assert node["superseded_reason"] and node["replaced_by"], node["id"]
        assert set(node["replaced_by"]) <= nodes.keys(), node["id"]
    for item in evidence_only:
        assert item["evidence_for"] in nodes and item["classification_reason"], item["id"]
        assert any(e.get("witness") == item.get("witness")
                   for e in nodes[item["evidence_for"]].get("evidence", [])), item["id"]
    for node in nodes.values():
        prose_refs = set(re.findall(r"\b[A-Z][0-9]{2}\b", node["goal"] + " " + node["approach"]))
        assert not prose_refs & retired_ids, f"Stale superseded reference in {node['id']}"
    assert plan["strategy"]["decisions"] and plan["strategy"]["checkpoint"]
    views = plan["views"]
    overview = views["overview"]
    assert len(overview) == len(set(overview)), "Repeated overview milestone"
    assert TARGETS <= set(overview) <= nodes.keys(), "Overview must contain the five targets"
    safety = view_ids(plan, "safety")
    build_run = view_ids(plan, "build-run")
    assert not safety & build_run and safety | build_run == nodes.keys(), "Detail views must partition tasks"
    assert safety == {key for key, n in nodes.items() if n["group"] == "T" or key == "G05"}, (
        "Safety view must retain its independent semantic branch")
    assert set(views["safety"]["focus"]) <= safety, "Safety focus references another branch"
    for pane in views["build-run"]["diagrams"]:
        selected = {key for key, n in nodes.items()
                    if n["group"] in pane["groups"] or key in pane["targets"]}
        assert selected and selected <= build_run, "Build/run diagram includes a foreign task"
    return nodes


def validate_transition(previous, plan):
    """Check edit provenance; semantic sufficiency still requires review."""
    before = {n["id"]: n for n in previous["nodes"]}
    after = {n["id"]: n for n in plan["nodes"]}
    archived = {n["id"] for n in plan.get("superseded", []) + plan.get("evidence_only", [])}
    assert set(before) - set(after) <= archived, "Removed tasks must be explicitly archived"
    changes = plan.get("goal_changes", [])
    for key in before.keys() & after.keys():
        if "origin" in before[key] and before[key]["origin"] != after[key]["origin"]:
            assert any(c.get("id") == key and c.get("before") == before[key]["origin"]
                       and c.get("after") == after[key]["origin"] and c.get("reason")
                       for c in plan.get("origin_changes", [])), f"Unrecorded origin change: {key}"
        if before[key]["goal"] != after[key]["goal"]:
            assert any(c.get("id") == key and c.get("before") == before[key]["goal"]
                       and c.get("after") == after[key]["goal"] and c.get("reason")
                       for c in changes), f"Unrecorded finish-condition change: {key}"
        if before[key]["status"] == "done" and after[key]["status"] != "done":
            assert any(c.get("id") == key and c.get("reason")
                       for c in plan.get("revalidations", [])), f"Unexplained reopened task: {key}"
    for key in after.keys() - before.keys():
        assert after[key]["origin"]["kind"] != "baseline", f"New task lacks origin: {key}"
        parent = after[key]["origin"]["parent"]
        seen = {key}
        while parent not in before:
            assert parent not in seen, f"Cyclic origin: {key}"
            seen.add(parent)
            parent = after[parent]["origin"]["parent"]
        assert before[parent]["status"] != "done" or after[parent]["status"] != "done", (
            f"New task refines an already completed obligation: {key}")
    retired = {n["id"] for n in previous.get("superseded", []) + previous.get("evidence_only", [])}
    assert retired <= archived, "Retired ID records must be retained"
    assert not retired & after.keys(), "A retired ID was reused"
    closed = sorted(key for key in before.keys() & after.keys()
                    if before[key]["status"] != "done" and after[key]["status"] == "done")
    return closed


POLICY = """The DAG plans obligations; evidence and work history live alongside it.

1. Keep each ID and finish condition stable. A different approach is allowed;
   a changed finish condition requires a `goal_changes` record with the exact
   before/after statements and a reason. Reopening a checked task requires a
   `revalidations` record explaining the affected specification or evidence.
2. Every added task identifies its `origin`: `refinement` of an existing
   obligation, or an explicit `discovery` with its affected parent, reason and sources.
   The parent's dependency route must actually include the added task.
3. Refinement preserves the parent's original finish condition. The parent
   remains the assembly obligation. Keep uncertain remaining work purple;
   do not present a partial decomposition as a complete list of prerequisites.
4. Attach already-proved helper results and regressions as `evidence` by default.
   Retrospective task decomposition must identify its parent and be reported
   separately from closing previously planned obligations. Keep shared tasks once.
5. Mark done only when evidence establishes the exact finish condition.
   A conditional result retains its premises; examples do not establish a universal claim.

Report existing obligations closed, decompositions with remaining gaps, and
discoveries separately. Total green-node counts are not progress measures.

Edit [proof-plan.json](proof-plan.json), then run:

```sh
python3 bootstrap/proof-roadmap.py
python3 bootstrap/proof-roadmap.py --check
python3 bootstrap/test_proof_roadmap.py
```

Before committing, compare against the previous checkpoint as well:

```sh
python3 bootstrap/proof-roadmap.py --check --base HEAD
```

After committing, use `--base HEAD~1` or the chosen earlier checkpoint.
This validates edit provenance and flags unrecorded finish-condition changes.

Existing origins are stable too: corrections need an `origin_changes` record.
New work attaches to an unfinished obligation; a discovered gap in a completed
obligation requires reopening/revalidation rather than silently adding children.

All three Markdown files are generated together. Edit task data only in
`proof-plan.json`; detail views own task finish conditions and evidence exactly once.
The overview contracts dependency paths rather than introducing new obligations.
Cross-view prerequisites keep their original IDs and link to their owning page.

The generator checks IDs, dependencies, acyclicity, source links, named witness
presence, completion evidence, origin/dependency relationships, status/budget
consistency, exactly five terminal nodes, semantic-safety
independence, the integrated performance gate, and non-reused superseded IDs. It does
not check that prose goals follow from their prerequisites or that estimates
are accurate; those require review and Rocq proofs.

Keep a task's statement stable. Mark it done only with its checked proof (or
the explicit definition promised by a definition task), not an axiom, example
or still-assumed project callback. For conditional component results, retain
their premises and separately discharge them in downstream assembly tasks.
If a prerequisite is discovered, record its origin and keep the original finish condition.
When changing strategy, archive superseded obligations explicitly rather than
marking them done or reusing their IDs for different goals.
Changes to Audit or the executable path require reviewing affected nodes.
The graph can grow while completed proofs accumulate under fixed statements.

Use incremental Rocq checks during work and full proof/assumption checks at
integration checkpoints. Runtime tests and bootstrap fixpoints remain regression
evidence, not replacements for universal proofs.

"""

SCOPE = """The finite source snapshot and exact read/domain facts are implemented;
see [SourceSnapshot.v](theories/SourceSnapshot.v) and the A01/A02 evidence.
F10 must still justify elaborator recursion/budgets: finite files alone do not
prove elaboration completeness. No arbitrary fixed bound is silently added to Builds.

A03/A07 settle the exact common-target/lowering boundary with evidence. The
direction is chosen, but the port and its universal proofs remain open.
The retained engine must implement the supplied compatible interface. Public
examples and a passing prototype do not prove compiler or elaborator correctness.

X13 covers only the optional transformations actually selected by measurement.
Omitted passes need no port. Any retained fuel/stuck exceptions must be discharged
using T20 or stronger exact agreement, rather than ignored. G05 is independent
of executable emission, runtime lowering, host implementation and performance measurements.

Executable safety follows from semantic safety, build soundness and run soundness.
Neither completeness theorem is needed for that corollary. Build completeness
only promises some output, so it cannot lift an implementation-only safety
result to every declarative program.

Existing internal component proofs retain their exact statements and premises.
They are reusable evidence/reference material for direct ports, not automatic
proofs about public programs. Completing the old internal whole-elaboration
theorem, a whole-program two-model back-translation, and the staged candidate
in [ReifyProgram.v](theories/ReifyProgram.v) are not required intermediate milestones
in this plan. Certificate-producing elaboration is also deferred.

"""

LEGEND = """- **✓ Done (green):** the exact component statement/definition exists; its stated
  hypotheses remain part of that result. A conditional lemma is not a universal certificate.
- **◐ Partial (amber):** relevant implementation or proof material exists; the finish condition is open.
  This does not claim an agent is currently running.
- **○ Open (gray):** the finish condition is not established, with a tentative hour estimate.
- **? No credible hour estimate (purple):** keep this task visible until it has
  concrete subgoals; do not count a design note as completion of its proof.
  Purple overrides the status color; ✓/◐/○ still record the task's completion status.

The ≤1h estimates are planning judgments based on the proposed strategy,
not measured or guaranteed bounds. They assume the prerequisites supply
the invariant/representation they promise. If a node overruns an hour,
report the remaining obstruction and refine its prerequisites before continuing autonomously.

"""


VIEW_FILES = {"safety": Path("proofs/safety.md"), "build-run": Path("proofs/build-run.md")}
FENCE = chr(96) * 3
STYLES = [
    "  classDef done stroke:#238636,stroke-width:2px",
    "  classDef partial stroke:#b8860b,stroke-width:2px,stroke-dasharray:5 3",
    "  classDef open stroke:#888,stroke-dasharray:2 3",
    "  classDef unknown stroke:#8250df,stroke-width:2px,stroke-dasharray:7 3",
]


def view_ids(plan, name):
    view = plan["views"][name]
    return {n["id"] for n in plan["nodes"]
            if n["group"] in view["groups"] or n["id"] in view["targets"]}


def task_owner(plan, key):
    return VIEW_FILES["safety" if key in view_ids(plan, "safety") else "build-run"]


def relative_link(current, destination):
    return os.path.relpath(destination, current.parent)


def task_link(plan, key, current):
    owner = task_owner(plan, key)
    path = "" if owner == current else relative_link(current, owner)
    return f"[{key}]({path}#{key.lower()})"


def source_link(path, current):
    return f"[{Path(path).name}]({relative_link(current, Path(path))})"


def nearest_edges(nodes, selected):
    """Contract omitted tasks without inventing prerequisite paths."""
    @cache
    def nearest(key):
        return frozenset(dep for direct in nodes[key]["requires"]
                         for dep in ([direct] if direct in selected else nearest(direct)))
    return {(dep, key) for key in selected for dep in nearest(key)}


def diagram(plan, ids, current, *, contracted=False):
    nodes = {n["id"]: n for n in plan["nodes"]}
    lines = [FENCE + "mermaid", "flowchart TB"]
    for group in plan["groups"]:
        members = [n for n in nodes.values() if n["id"] in ids and n["group"] == group["id"]]
        if not members:
            continue
        lines.append(f'  subgraph {group["id"]}_group["{group["title"]}"]')
        for node in members:
            key = node["id"]
            uncertain = "? " if node["budget"] == "unknown" else ""
            style = "unknown" if node["budget"] == "unknown" else node["status"]
            label = f'{MARK[node["status"]]} {uncertain}{key}: {node["title"]}'
            opening, closing = ("[[", "]]") if key in TARGETS else ("[", "]")
            lines.append(f'    {key}{opening}"{label}"{closing}:::{style}')
        lines.append("  end")
    edges = (nearest_edges(nodes, ids) if contracted else
             {(dep, key) for key in ids for dep in nodes[key]["requires"] if dep in ids})
    lines += [f"  {left} --> {right}" for left, right in sorted(edges)]
    for key in sorted(ids):
        owner = task_owner(plan, key)
        target = ("" if owner == current else relative_link(current, owner)) + "#" + key.lower()
        lines.append(f'  click {key} "{target}"')
    return "\n".join(lines + STYLES + [FENCE, ""])


def frontier(plan, ids, current):
    nodes = {n["id"]: n for n in plan["nodes"]}
    ready = [n for n in nodes.values() if n["id"] in ids and n["status"] != "done"
             and all(nodes[dep]["status"] == "done" for dep in n["requires"])]
    def links(budget):
        return ", ".join(task_link(plan, n["id"], current)
                         for n in ready if n["budget"] == budget) or "none"
    return (f"Ready tasks with tentative hour estimates: {links('hour')}.\n\n"
            f"Ready tasks needing decomposition: {links('unknown')}.")


def task_catalog(plan, ids, current):
    lines = ["## Task records", "",
             "Each finish condition and its evidence has one home. Prerequisites link",
             "to that record, including across diagrams and pages.", ""]
    for group in plan["groups"]:
        members = [n for n in plan["nodes"] if n["id"] in ids and n["group"] == group["id"]]
        if not members:
            continue
        lines += [f'### {group["title"]}', "",
                  "| Task | Status / budget | Finish condition | Prerequisites | Strategy and evidence |",
                  "| --- | --- | --- | --- | --- |"]
        for node in members:
            key = node["id"]
            sources = ", ".join(source_link(path, current) for path in node["sources"])
            origin = node["origin"]
            provenance = (f' Origin: {origin["kind"]} under {task_link(plan, origin["parent"], current)}.'
                          if origin["kind"] != "baseline" else "")
            witness = f' Named result: {chr(96)}{node["witness"]}{chr(96)}.' if "witness" in node else ""
            evidence = " ".join(
                e["summary"] + " " + ", ".join(source_link(path, current) for path in e["sources"])
                for e in node.get("evidence", []))
            requires = ", ".join(task_link(plan, dep, current) for dep in node["requires"]) or "None"
            cells = [f'<a id="{key.lower()}"></a> **{key}** {node["title"]}',
                     f'{MARK[node["status"]]} {node["status"]}; {BUDGET[node["budget"]]}',
                     node["goal"], requires, node["approach"] + provenance + witness + " " + evidence + " " + sources]
            lines.append("| " + " | ".join(cell.replace("|", "\\|") for cell in cells) + " |")
        lines.append("")
    return "\n".join(lines)


def detail_diagram(plan, title, candidates, current):
    nodes = {n["id"]: n for n in plan["nodes"]}
    # Completed helper decompositions stay in the catalog, keeping active diagrams readable.
    visible = {key for key in candidates if not (
        nodes[key]["status"] == "done" and nodes[key]["origin"]["kind"] == "refinement")}
    boundary = [(key, dep) for key in sorted(visible)
                for dep in nodes[key]["requires"] if dep not in visible]
    lines = [f"## {title}", "", diagram(plan, visible, current),
             "Arrows show prerequisites within this diagram. Completed helper refinements",
             "are folded into the catalog; other diagrams' prerequisites are linked below.", ""]
    if boundary:
        lines += ["| Task in this diagram | Prerequisite outside the diagram |",
                  "| --- | --- |",
                  *[f"| {task_link(plan, key, current)} | {task_link(plan, dep, current)} |"
                    for key, dep in boundary], ""]
    return "\n".join(lines)


def history(plan, current):
    lines = ["## Checkpoint changes", "",
             "These report previously planned obligations closed, retrospective decompositions,",
             "and discovered requirements separately. They are not green-node progress totals.", ""]
    for update in plan["updates"]:
        lines += [f'### {update["title"]}', ""]
        for field, label in [("previously_planned_closed", "Previously planned obligations closed"),
                             ("retrospective_refinements", "Retrospective refinements"),
                             ("decomposed", "Other decomposed work"), ("discoveries", "Discovered requirements")]:
            links = ", ".join(task_link(plan, key, current) for key in update[field]) or "none"
            lines.append(f"- {label}: {links}.")
        lines += ["", update["note"], ""]
    lines += ["## Superseded route tasks", "",
              "These are archived obligations, not completions. Full original records remain in",
              source_link("proof-plan.json", current) + ".", "",
              "<details>", "<summary>Earlier tasks and replacements</summary>", "",
              "| Earlier task | Replacements | Reason |", "| --- | --- | --- |"]
    for node in plan.get("superseded", []):
        replacements = ", ".join(task_link(plan, key, current) for key in node["replaced_by"])
        lines.append(f'| {node["id"]}: {node["title"]} | {replacements} | {node["superseded_reason"]} |')
    return "\n".join(lines + ["", "</details>", ""])


def detail_page(plan, name):
    current = VIEW_FILES[name]
    ids = view_ids(plan, name)
    lines = ["# " + ("Semantic safety" if name == "safety" else "Executable build and run"), "",
             "[Milestone overview and roadmap policy](../PROOFS.md). "
             "[Other branch](" + ("build-run.md" if name == "safety" else "safety.md") + ").", "",
             "Generated from [one canonical task plan](../proof-plan.json). "
             "Task IDs and finish conditions are shared across views.", ""]
    if name == "safety":
        lines += ["[Current focus](#current-focus) · [Diagram](#safety-dependency-detail) · "
                  "[Task records](#task-records)", ""]
    else:
        lines += [" · ".join(f'[{view["title"]}](#' +
                             re.sub(r"[^a-z0-9 -]", "", view["title"].lower()).replace(" ", "-") + ")"
                             for view in plan["views"]["build-run"]["diagrams"]), "",
                  "[Task records](#task-records) · [Checkpoint history](#checkpoint-changes)", ""]
    lines += [frontier(plan, ids, current), ""]
    if name == "safety":
        focus = ", ".join(task_link(plan, key, current) for key in plan["views"]["safety"]["focus"])
        lines += ["## Current focus", "",
                  f"{focus}. T02 is the assembly milestone; the formation, provenance and",
                  "environment-realization gaps remain explicit in T29, T30 and T31.", "",
                  "Before starting a frontier task, name the checked result expected within roughly",
                  "an hour. At the checkpoint compare its remaining gap with the previous one.",
                  "If helper results accumulate without shrinking that gap, reevaluate the approach.", "",
                  detail_diagram(plan, "Safety dependency detail", ids, current)]
    else:
        for view in plan["views"]["build-run"]["diagrams"]:
            candidates = {n["id"] for n in plan["nodes"]
                          if n["group"] in view["groups"] or n["id"] in view["targets"]}
            lines.append(detail_diagram(plan, view["title"], candidates, current))
        lines += ["## Scope and unresolved decisions", "", rebase_links(SCOPE, current)]
    lines += [task_catalog(plan, ids, current)]
    if name == "safety":
        lines += ["## Regression evidence outside the DAG", ""]
        for item in plan.get("evidence_only", []):
            if item["evidence_for"] in ids:
                lines += [f'**{item["id"]}**, evidence for {task_link(plan, item["evidence_for"], current)}: '
                          + item["classification_reason"], ""]
    else:
        lines.append(history(plan, current))
    return "\n".join(lines + ["<!-- Generated by bootstrap/proof-roadmap.py; edit proof-plan.json. -->", ""])


def rebase_links(text, current):
    def link(match):
        label, target = match.groups()
        if "://" in target or target.startswith("#"):
            return match.group(0)
        return f"[{label}]({relative_link(current, Path(target))})"
    return re.sub(r"\[([^]]+)\]\(([^)]+)\)", link, text)


def render(plan):
    """Generate the overview and both detail views from the same validated DAG."""
    nodes = validate(plan)
    current = Path("PROOFS.md")
    milestones = set(plan["views"]["overview"])
    lines = ["# Roadmap to the five public theorems", "",
             f'Reviewed against {chr(96)}origin/main{chr(96)} at {chr(96)}{plan["reviewed_base"][:7]}{chr(96)} '
             f'on {plan["reviewed_date"]}.',
             "The targets are the five fields of [Audit.Public.CORRECT](theories/Audit.v).",
             "All five remain unproved. Semantic safety is independent of executable build/run.", "",
             "The DAG plans obligations; evidence and work history live alongside it.",
             "Total green-node counts are not progress measures.", "",
             "## Milestone overview", "",
             "[Safety detail](proofs/safety.md) · [Build/run detail](proofs/build-run.md)", "",
             "This view contracts dependency paths through omitted tasks. An arrow means",
             "there is a prerequisite path in the full plan; the detailed task records retain",
             "every direct prerequisite. Milestone meanings and IDs stay fixed.", "",
             diagram(plan, milestones, current, contracted=True),
             "| Milestone | Detail |", "| --- | --- |",
             *[f'| {task_link(plan, key, current)} | {nodes[key]["title"]} |'
               for key in plan["views"]["overview"]], "",
             "## Current focus", "",
             "Safety first: settle source grounding, code provenance and callable/environment",
             "realization under T02. See the [named safety frontier](proofs/safety.md#current-focus).",
             "Independent executable prerequisites remain visible in [build/run](proofs/build-run.md).", "",
             plan["strategy"]["checkpoint"], "",
             "## Reading the views", "", LEGEND,
             "The milestone overview is deliberately small. Detail diagrams expand active work",
             "and retain completed landmarks; completed helper refinements are folded into their",
             "catalog records. Shared tasks have one owning page, with cross-links elsewhere.",
             "Do not change task granularity merely to meet a visual node quota.", "",
             "## Selected strategy", "",
             *[f"- {decision}" for decision in plan["strategy"]["decisions"]], "",
             plan["strategy"]["performance"], "",
             "Checkpoint history, decompositions and discoveries are recorded in the",
             "[build/run appendix](proofs/build-run.md#checkpoint-changes).", "",
             "## Updating the roadmap", "", POLICY,
             "<!-- Generated by bootstrap/proof-roadmap.py; edit proof-plan.json. -->", ""]
    return {current: "\n".join(lines),
            **{path: detail_page(plan, name) for name, path in VIEW_FILES.items()}}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="check without rewriting PROOFS.md")
    parser.add_argument("--base", help="Git checkpoint against which to validate task edits")
    args = parser.parse_args()
    plan = json.loads((ROOT / "proof-plan.json").read_text())
    content = render(plan)
    if args.base:
        previous = json.loads(subprocess.check_output(
            ["git", "show", f"{args.base}:bootstrap/proof-plan.json"], cwd=ROOT, text=True))
        closed = validate_transition(previous, plan)
        print("Previously planned obligations closed: " + (", ".join(closed) or "none"))
    for path, page in content.items():
        target = ROOT / path
        if args.check:
            if not target.exists() or target.read_text() != page:
                parser.error(f"{path} is stale; run python3 bootstrap/proof-roadmap.py")
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(page)
    print(f"Validated {len(plan['nodes'])} tasks; exactly five terminal nodes.")


if __name__ == "__main__":
    main()
