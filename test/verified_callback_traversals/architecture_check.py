#!/usr/bin/env python3
"""Focused structural limits for the accepted VERO-104 core tranche."""

from __future__ import annotations

import hashlib
import re
from pathlib import Path


ROOT = Path.cwd().parents[1]
EXPECTED_READ_ONLY = {
    "ppx/vero_ppx_logical_builtin_private.ml": "0c7470c0a7539ef5a52c5fc8252b53ee618e7d28974d15be6d06e292a0adc75d",
    "ppx/vero_ppx_logical_builtin_private.mli": "a5903773ad5164e53db25416db59096f66c7a47f1d4945b06e6d2e33a6d3840b",
    "src/typedtree_logical_builtin_private.ml": "474a69410ed52c31cf575aaa91d1dee44367b0e6ec0d536864015d10b1beedf5",
    "src/typedtree_logical_builtin_private.mli": "dbce584075376880f4453411c71dfb366a96a87a11ef3e1dba12358dfef43d9d",
}
OWNER_LIMITS = {
    "src/logic_quantifier_private.ml": 650,
    "src/logic_quantifier_private.mli": 180,
    "src/quantifier_validation_private.ml": 650,
    "src/quantifier_validation_private.mli": 180,
}


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def function_spans(path: Path) -> list[int]:
    lines = path.read_text().splitlines()
    starts = [
        index
        for index, line in enumerate(lines)
        if re.match(r"^(?:let|and)\s+(?:rec\s+)?[A-Za-z_]", line)
    ]
    return [
        (starts[index + 1] if index + 1 < len(starts) else len(lines)) - start
        for index, start in enumerate(starts)
    ]


for relative, expected in EXPECTED_READ_ONLY.items():
    actual = digest(ROOT / relative)
    assert actual == expected, f"read-only carrier changed: {relative}"

owner_lines: list[str] = []
for relative, limit in OWNER_LIMITS.items():
    path = ROOT / relative
    count = len(path.read_text().splitlines())
    assert count <= limit, f"{relative}: {count}>{limit}"
    owner_lines.append(f"{Path(relative).name}={count}/{limit}")
    if path.suffix == ".ml":
        maximum = max(function_spans(path), default=0)
        assert maximum <= 140, f"{relative}: function span {maximum}>140"

validator_text = (ROOT / "src/quantifier_validation_private.ml").read_text()
assert "[@warning" not in validator_text
assert "allow_unclassified" in validator_text
assert "expected_owner" in validator_text
assert "logical_application" in validator_text

auth = (
    ROOT
    / "test/verified_callback_traversals/fixtures/negative_quantifier_authentication.ml"
).read_text()
trigger = (
    ROOT / "test/verified_callback_traversals/fixtures/negative_trigger.ml"
).read_text()
assert auth.count("(* CASE ") // 2 == 12
assert trigger.count("(* CASE ") // 2 == 13

print("read-only-carrier=4/4")
print("owners " + " ".join(owner_lines) + " function-cap=140")
print("negative-matrix authentication=12 trigger=13")
