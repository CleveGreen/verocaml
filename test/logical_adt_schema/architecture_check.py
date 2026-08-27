from pathlib import Path
import sys

root = Path(sys.argv[1]).resolve()
pairs = [
    "logical_adt_schema_private",
    "logical_adt_encoding_private",
    "z3_datatype_private",
]
for name in pairs:
    for suffix in (".ml", ".mli"):
        path = root / "src" / f"{name}{suffix}"
        if not path.is_file():
            raise SystemExit(f"missing={path}")
        cap = 650 if suffix == ".ml" else 180
        lines = len(path.read_text().splitlines())
        if lines > cap:
            raise SystemExit(f"loc-cap={name}{suffix}:{lines}>{cap}")
print("private-pairs=present loc-caps=pass")

for path in (root / "src").glob("*.ml"):
    text = path.read_text()
    if path.name not in {"z3_bridge.ml", "z3_datatype_private.ml"} and "Z3." in text:
        raise SystemExit(f"backend-handle-owner={path.name}")
print("backend-handles=worker-local")
