"""Policy regressions for roadmap edits; no Rocq build is needed."""
import copy
import importlib.util
import json
from pathlib import Path
import posixpath
import re
import sys
import unittest

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("proof_roadmap", ROOT / "proof-roadmap.py")
roadmap = importlib.util.module_from_spec(spec)
spec.loader.exec_module(roadmap)


class RoadmapPolicyTests(unittest.TestCase):
    def setUp(self):
        self.plan = json.loads((ROOT / "proof-plan.json").read_text())

    def node(self, key):
        return next(n for n in self.plan["nodes"] if n["id"] == key)

    def test_current_graph_and_unchanged_transition(self):
        roadmap.validate(self.plan)
        self.assertEqual(roadmap.validate_transition(self.plan, self.plan), [])

    def test_missing_completion_evidence(self):
        self.node("A01").pop("evidence")
        with self.assertRaisesRegex(AssertionError, "Missing completion evidence"):
            roadmap.validate(self.plan)

    def test_refinement_must_feed_parent(self):
        self.node("T24")["origin"]["parent"] = "T01"
        with self.assertRaisesRegex(AssertionError, "Origin parent does not depend"):
            roadmap.validate(self.plan)

    def test_discovery_requires_reason(self):
        self.node("T26")["origin"].pop("reason")
        with self.assertRaisesRegex(AssertionError, "Unexplained discovery"):
            roadmap.validate(self.plan)

    def test_finish_condition_change_requires_exact_record(self):
        previous = copy.deepcopy(self.plan)
        before = self.node("T02")["goal"]
        self.node("T02")["goal"] = "A revised explicit finish condition."
        with self.assertRaisesRegex(AssertionError, "Unrecorded finish-condition change"):
            roadmap.validate_transition(previous, self.plan)
        self.plan["goal_changes"] = [{"id": "T02", "before": before,
                                      "after": self.node("T02")["goal"], "reason": "Reviewed correction."}]
        roadmap.validate_transition(previous, self.plan)

    def test_added_task_cannot_claim_baseline_origin(self):
        previous = copy.deepcopy(self.plan)
        previous["nodes"] = [n for n in previous["nodes"] if n["id"] != "F17"]
        self.node("F17")["origin"] = {"kind": "baseline"}
        with self.assertRaisesRegex(AssertionError, "New task lacks origin"):
            roadmap.validate_transition(previous, self.plan)

    def test_existing_origin_cannot_silently_change(self):
        previous = copy.deepcopy(self.plan)
        self.node("F17")["origin"] = {"kind": "baseline"}
        with self.assertRaisesRegex(AssertionError, "Unrecorded origin change"):
            roadmap.validate_transition(previous, self.plan)

    def test_new_child_cannot_silently_expand_finished_parent(self):
        previous = copy.deepcopy(self.plan)
        previous["nodes"] = [n for n in previous["nodes"] if n["id"] != "F17"]
        next(n for n in previous["nodes"] if n["id"] == "B02")["status"] = "done"
        self.node("B02")["status"] = "done"
        with self.assertRaisesRegex(AssertionError, "already completed obligation"):
            roadmap.validate_transition(previous, self.plan)

    def test_new_completed_child_is_not_an_existing_completion(self):
        previous = copy.deepcopy(self.plan)
        previous["nodes"] = [n for n in previous["nodes"] if n["id"] != "F17"]
        self.node("F17")["status"] = "done"
        self.assertEqual(roadmap.validate_transition(previous, self.plan), [])

    def test_existing_completion_is_reported(self):
        previous = copy.deepcopy(self.plan)
        next(n for n in previous["nodes"] if n["id"] == "F07")["status"] = "open"
        self.assertEqual(roadmap.validate_transition(previous, self.plan), ["F07"])

    def test_task_cannot_disappear(self):
        previous = copy.deepcopy(self.plan)
        self.plan["nodes"].remove(self.node("F17"))
        with self.assertRaisesRegex(AssertionError, "Removed tasks must be explicitly archived"):
            roadmap.validate_transition(previous, self.plan)

    def test_reopened_task_requires_revalidation_record(self):
        previous = copy.deepcopy(self.plan)
        self.node("F07")["status"] = "open"
        with self.assertRaisesRegex(AssertionError, "Unexplained reopened task"):
            roadmap.validate_transition(previous, self.plan)
        self.plan["revalidations"] = [{"id": "F07", "reason": "Input contract changed."}]
        roadmap.validate_transition(previous, self.plan)

    def test_retired_ids_stay_reserved(self):
        previous = copy.deepcopy(self.plan)
        self.plan["evidence_only"] = []
        with self.assertRaisesRegex(AssertionError, "Retired ID records must be retained"):
            roadmap.validate_transition(previous, self.plan)

    def test_three_generated_views(self):
        pages = roadmap.render(self.plan)
        self.assertEqual(set(pages), {Path("PROOFS.md"), Path("proofs/safety.md"),
                                      Path("proofs/build-run.md")})

    def test_task_records_have_one_home_and_keep_every_prerequisite(self):
        pages = roadmap.render(self.plan)
        for node in self.plan["nodes"]:
            key = node["id"]
            anchor = f'<a id="{key.lower()}"></a>'
            self.assertEqual(sum(page.count(anchor) for page in pages.values()), 1, key)
            owner = roadmap.task_owner(self.plan, key)
            row = next(line for line in pages[owner].splitlines() if anchor in line)
            for dep in node["requires"]:
                self.assertIn(roadmap.task_link(self.plan, dep, owner), row)

    def test_cross_view_dependencies_link_without_duplicate_records(self):
        pages = roadmap.render(self.plan)
        build = pages[Path("proofs/build-run.md")]
        self.assertIn("[T26](safety.md#t26)", build)
        self.assertNotIn('<a id="t26"></a>', build)

    def test_generated_links_and_diagram_clicks_resolve(self):
        pages = roadmap.render(self.plan)
        anchors = {}
        for path, page in pages.items():
            anchors[path] = set(re.findall(r'<a id="([^"]+)">', page))
            anchors[path].update(re.sub(r"[^a-z0-9 -]", "", title.lower()).replace(" ", "-")
                                 for title in re.findall(r"^#+ (.+)$", page, re.M))
        for path, page in pages.items():
            targets = re.findall(r"\[[^\]]+\]\(([^)]+)\)", page)
            targets += re.findall(r'click \w+ "([^"]+)"', page)
            for href in targets:
                if "://" in href:
                    continue
                link, _, fragment = href.partition("#")
                destination = (Path(posixpath.normpath(str(path.parent / link))) if link else path)
                if destination in pages:
                    if fragment:
                        self.assertIn(fragment, anchors[destination], (path, href))
                else:
                    self.assertTrue((ROOT / destination).is_file(), (path, href))

    def test_overview_contracts_real_dependency_paths(self):
        nodes = {"A": {"requires": []}, "B": {"requires": ["A"]},
                 "C": {"requires": ["B"]}}
        self.assertEqual(roadmap.nearest_edges(nodes, {"A", "C"}), {("A", "C")})

    def test_overlapping_detail_ownership_is_rejected(self):
        self.plan["views"]["build-run"]["groups"].append("T")
        with self.assertRaisesRegex(AssertionError, "partition tasks"):
            roadmap.validate(self.plan)


if __name__ == "__main__":
    unittest.main()
