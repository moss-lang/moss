#!/usr/bin/env python3
"""Validate the task DAG and generate its human-readable Mermaid roadmap."""

import argparse
from collections import Counter
import json
from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
TARGETS = {"G01", "G02", "G03", "G04", "G05"}
MARK = {"done": "✓", "partial": "◐", "open": "○"}
BUDGET = {"done": "Proved / defined", "hour": "≤1h, tentative", "unknown": "? Decompose"}


def validate(plan):
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
        for source in node["sources"]:
            assert (ROOT / source).is_file(), (key, source)
        if "witness" in node:
            assert any(re.search(r"\b" + re.escape(node["witness"]) + r"\b",
                                 (ROOT / source).read_text()) for source in node["sources"]), key
        if node["status"] == "done":
            assert all(nodes[dep]["status"] == "done" for dep in node["requires"]), key
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
    retired_ids = {node["id"] for node in superseded}
    assert len(retired_ids) == len(superseded), "Duplicate superseded ID"
    assert not retired_ids & nodes.keys(), "A superseded ID was reused"
    for node in superseded:
        assert node["superseded_reason"] and node["replaced_by"], node["id"]
        assert set(node["replaced_by"]) <= nodes.keys(), node["id"]
    for node in nodes.values():
        prose_refs = set(re.findall(r"\b[A-Z][0-9]{2}\b", node["goal"] + " " + node["approach"]))
        assert not prose_refs & retired_ids, f"Stale superseded reference in {node['id']}"
    assert plan["strategy"]["decisions"] and plan["strategy"]["checkpoint"]
    return nodes


def render(plan):
    nodes = validate(plan)
    states = Counter(node["status"] for node in nodes.values())
    budgets = Counter(node["budget"] for node in nodes.values())
    ready = [node for node in nodes.values() if node["status"] != "done"
             and all(nodes[dep]["status"] == "done" for dep in node["requires"])]
    ready_hours = ", ".join(node["id"] for node in ready if node["budget"] == "hour") or "none"
    ready_unknowns = ", ".join(node["id"] for node in ready if node["budget"] == "unknown") or "none"
    lines = [
        "# Roadmap to the five public theorems", "",
        f"Reviewed against `origin/main` at `{plan['reviewed_base'][:7]}` on {plan['reviewed_date']}.",
        "Targets: the five fields of [`Audit.Public.CORRECT`](theories/Audit.v).",
        "All five remain unproved. The safety statement on this branch concerns",
        "declarative `Admissible`/`Runs`, independently of executable build/run.", "",
        f"**{len(nodes)} nodes:** {states['done']} done, {states['partial']} partial, {states['open']} open.",
        f"Of the unfinished nodes, **{budgets['hour']}** have a tentative ≤1h estimate once their",
        f"prerequisites are discharged; **{budgets['unknown']}** are explicitly **? needs decomposition**.",
        "These counts are not a total time estimate. Completed component results",
        "are kept at useful statement granularity, rather than split into historical hour-sized work.", "",
        "- **✓ Done (green):** the exact component statement/definition exists; its stated",
        "  hypotheses remain part of that result. A conditional lemma is not a universal certificate.",
        "- **◐ Partial (amber):** relevant implementation or proof material exists; the finish condition is open.",
        "  This does not claim an agent is currently running.",
        "- **○ Open (gray):** the finish condition is not established, with a tentative hour estimate.",
        "- **? No credible hour estimate (purple):** keep this task visible until it has",
        "  concrete subgoals; do not count a design note as completion of its proof.",
        "  Purple overrides the status color; ✓/◐/○ still record the task's completion status.", "",
        "The ≤1h estimates are planning judgments based on the proposed strategy,",
        "not measured or guaranteed bounds. They assume the prerequisites supply",
        "the invariant/representation they promise. If a node overruns an hour,",
        "report the remaining obstruction and refine its prerequisites before continuing autonomously.", "",
        f"**Ready frontier:** tentative hour tasks: {ready_hours}.",
        f"Ready tasks requiring decomposition: {ready_unknowns}.", "",
        "## Selected plan and next checkpoint", "",
        *[f"- {decision}" for decision in plan["strategy"]["decisions"]], "",
        plan["strategy"]["checkpoint"], "",
        plan["strategy"]["performance"], "",
        "## One dependency DAG", "",
        "Arrows point from prerequisite to task; all incoming prerequisites are needed",
        "for this proposed route. The five double-bordered nodes are the only sinks.",
        "Groups organize the same graph; they are not additional task nodes.", "",
        "Architecture/measurement nodes and X14 are engineering prerequisites for the",
        "selected deliverable. They are not extra hypotheses in the five theorem statements.", "",
        "```mermaid", "flowchart TB",
    ]
    for group in plan["groups"]:
        lines += [f'  subgraph {group["id"]}_group["{group["title"]}"]']
        for node in nodes.values():
            if node["group"] != group["id"]:
                continue
            uncertain = "? " if node["budget"] == "unknown" else ""
            node_class = "unknown" if node["budget"] == "unknown" else node["status"]
            label = f'{MARK[node["status"]]} {uncertain}{node["id"]}: {node["title"]}'
            opening, closing = ("[[", "]]") if node["id"] in TARGETS else ("[", "]")
            lines.append(f'    {node["id"]}{opening}"{label}"{closing}:::{node_class}')
        lines.append("  end")
    for node in nodes.values():
        for dep in node["requires"]:
            lines.append(f'  {dep} --> {node["id"]}')
    lines += [
        "  classDef done stroke:#238636,stroke-width:2px",
        "  classDef partial stroke:#b8860b,stroke-width:2px,stroke-dasharray:5 3",
        "  classDef open stroke:#888,stroke-dasharray:2 3",
        "  classDef unknown stroke:#8250df,stroke-width:2px,stroke-dasharray:7 3", "```", "",
        "## Finish conditions, strategy and evidence", "",
        "Links on unfinished tasks are starting points, not proof-completion claims.",
        "Some ? nodes are deliberately broad: their constructor-level breakdown depends",
        "on earlier design decisions. No remaining universal result is hidden behind",
        "an estimate of one hour just because its final assembly has a short name.", "",
    ]
    for group in plan["groups"]:
        lines += [f'### {group["title"]}', "",
                  "| Task | Status / budget | Finish condition | Strategy and evidence |",
                  "| --- | --- | --- | --- |"]
        for node in nodes.values():
            if node["group"] != group["id"]:
                continue
            sources = ", ".join(f'[{Path(path).name}]({path})' for path in node["sources"])
            witness = f' Named result: `{node["witness"]}`.' if "witness" in node else ""
            cells = [f'**{node["id"]}** {node["title"]}',
                     f'{MARK[node["status"]]} {node["status"]}; {BUDGET[node["budget"]]}',
                     node["goal"], node["approach"] + witness + " " + sources]
            lines.append("| " + " | ".join(cell.replace("|", "\\|") for cell in cells) + " |")
        lines.append("")
    lines += [
        "## Scope and unresolved decisions", "",
        "Finite sources are the selected direction; the public source type has not yet",
        "been changed. A01/A02 must make the snapshot and inventory contract explicit.",
        "F10 must still justify elaborator recursion/budgets: finite files alone do not",
        "prove elaboration completeness. No arbitrary fixed bound is silently added to Builds.", "",
        "A03/A07 settle the exact common-target/lowering boundary with evidence. The",
        "direction is chosen, but the port and its universal proofs remain open.",
        "The retained engine must implement the supplied compatible interface. Public",
        "examples and a passing prototype do not prove compiler or elaborator correctness.", "",
        "X13 covers only the optional transformations actually selected by measurement.",
        "Omitted passes need no port. Any retained fuel/stuck exceptions must be discharged",
        "using T20 or stronger exact agreement, rather than ignored. G05 is independent",
        "of executable emission, runtime lowering, host implementation and performance measurements.", "",
        "Executable safety follows from semantic safety, build soundness and run soundness.",
        "Neither completeness theorem is needed for that corollary. Build completeness",
        "only promises some output, so it cannot lift an implementation-only safety",
        "result to every declarative program.", "",
        "Existing internal component proofs retain their exact statements and premises.",
        "They are reusable evidence/reference material for direct ports, not automatic",
        "proofs about public programs. Completing the old internal whole-elaboration",
        "theorem, a whole-program two-model back-translation, and the staged candidate",
        "in [ReifyProgram.v](theories/ReifyProgram.v) are not required intermediate milestones",
        "in this plan. Certificate-producing elaboration is also deferred.", "",
        "## Superseded route tasks", "",
        f"**{len(plan.get('superseded', []))} earlier tasks** are removed from the active route,",
        "not marked complete. Their original statements/statuses remain in",
        "[proof-plan.json](proof-plan.json) with replacement IDs and reasons. Existing",
        "proof/code files are preserved. Active task counts exclude these records.", "",
        "<details>", "<summary>Earlier tasks and their replacements</summary>", "",
        "| Earlier ID / task | Replacement tasks | Reason |",
        "| --- | --- | --- |",
        *[f'| {node["id"]}: {node["title"]} | {", ".join(node["replaced_by"])} | {node["superseded_reason"]} |'
          for node in plan.get("superseded", [])], "",
        "</details>", "",
        "## Updating the roadmap", "",
        "Edit [proof-plan.json](proof-plan.json), then run:", "",
        "```sh", "python3 bootstrap/proof-roadmap.py",
        "python3 bootstrap/proof-roadmap.py --check", "```", "",
        "The generator checks IDs, dependencies, acyclicity, source links, named witness",
        "presence, status/budget consistency, exactly five terminal nodes, semantic-safety",
        "independence, the integrated performance gate, and non-reused superseded IDs. It does",
        "not check that prose goals follow from their prerequisites or that estimates",
        "are accurate; those require review and Rocq proofs.", "",
        "Keep a task's statement stable. Mark it done only with its checked proof (or",
        "the explicit definition promised by a definition task), not an axiom, example",
        "or still-assumed project callback. For conditional component results, retain",
        "their premises and separately discharge them in downstream assembly tasks.",
        "If a prerequisite is discovered, add it and keep the original finish condition.",
        "When changing strategy, archive superseded obligations explicitly rather than",
        "marking them done or reusing their IDs for different goals.",
        "Changes to Audit or the executable path require reviewing affected nodes.",
        "The graph can grow while completed proofs accumulate under fixed statements.", "",
        "Use incremental Rocq checks during work and full proof/assumption checks at",
        "integration checkpoints. Runtime tests and bootstrap fixpoints remain regression",
        "evidence, not replacements for universal proofs.", "",
        "<!-- Generated by bootstrap/proof-roadmap.py; edit proof-plan.json. -->", "",
    ]
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="check without rewriting PROOFS.md")
    args = parser.parse_args()
    plan = json.loads((ROOT / "proof-plan.json").read_text())
    content = render(plan)
    target = ROOT / "PROOFS.md"
    if args.check:
        if target.read_text() != content:
            parser.error("PROOFS.md is stale; run python3 bootstrap/proof-roadmap.py")
    else:
        target.write_text(content)
    print(f"Validated {len(plan['nodes'])} tasks; exactly five terminal nodes.")


if __name__ == "__main__":
    main()
