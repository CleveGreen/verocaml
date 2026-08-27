from pathlib import Path
import sys

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)

cases = {
    "duplicate_binding": "let bad x = let[@ghost][@ghost] y = (x [@ghost]) in y",
    "conflicting_binding": "let bad x = let[@ghost][@tracked] y = (x [@ghost]) in y",
    "duplicate_expression": "let bad x = ((x [@ghost]) [@tracked])",
    "multi_binding": "let bad x = let[@ghost] y = (x [@ghost]) and z = (x [@ghost]) in y",
    "record_pun": "type t = { run : int; ghost : int [@ghost] }\nlet bad p = let { run; ghost } = p in run",
    "missing_record_component": "type t = { run : int; ghost : int [@ghost] }\nlet bad x = { run = x; ghost = x }",
    "missing_positional_component": "type t = C of int * (int [@ghost])\nlet bad x = C (x, x)",
    "update_mismatch": "type t = { mutable run : int; mutable ghost : int [@ghost] }\nlet bad p x = ((p.ghost <- (x [@tracked])) [@tracked])",
    "erased_mutation": "let bad x = let cell = ref 0 in let[@ghost] _ = ((cell := x; x) [@ghost]) in x",
    "erased_exception": "let bad x = let[@ghost] _ = ((raise Exit) [@ghost]) in x",
    "erased_divergence": "let rec loop x = loop x\nlet bad x = let[@ghost] _ = (loop x [@ghost]) in x",
    "erased_allocation": "let bad x = let[@ghost] _ = (ref x [@ghost]) in x",
    "erased_capture": "let bad x = let[@ghost] _ = ((fun () -> x) [@ghost]) in x",
    "nested_update_effect": "type t = { mutable run : int; mutable ghost : int [@ghost] }\nlet effect x = x + 1\nlet bad p x = ((p.ghost <- (effect x [@ghost])) [@ghost])",
}

for name, source in cases.items():
    (out / f"{name}.ml").write_text(source + "\n")
