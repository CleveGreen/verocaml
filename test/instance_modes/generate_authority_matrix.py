from pathlib import Path
import sys

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
prelude = Path(sys.argv[2]).read_text()

attacks = {
    "direct": "[%verocaml.use_type_invariant x]; ()",
    "alias": "let y = x in [%verocaml.use_type_invariant y]; ()",
    "pattern": "let (y : Box.t) = x in [%verocaml.use_type_invariant y]; ()",
    "branch": "let y = if true then x else x in [%verocaml.use_type_invariant y]; ()",
    "result": "let y = relay x in [%verocaml.use_type_invariant y]; ()",
    "tracked_formal": "tracked_sink (x [@tracked])",
    "tracked_return": "((x [@tracked]) : Box.t)",
}

actuals = {
    "exec": "let actual = source in",
    "tracked": "let[@tracked] actual = (source [@tracked]) in",
    "ghost": "let[@ghost] actual = (source [@ghost]) in",
}

for category in ("spec", "proof"):
    for actual_name, setup in actuals.items():
        for attack_name, attack in attacks.items():
            result = "(Box.t [@tracked])" if attack_name == "tracked_return" else "unit"
            body = attack
            text = prelude + "\n\n"
            text += "let tracked_sink (value : Box.t [@tracked]) : unit = ()\n[@@verocaml.proof]\n\n"
            if attack_name == "result":
                text += (
                    "let relay (value : Box.t [@ghost]) : (Box.t [@ghost]) =\n"
                    "  (value [@ghost])\n\n"
                )
            text += f"let consume (x : Box.t) : {result} =\n  {body}\n[@@verocaml.{category}]\n\n"
            text += "let flow (source : Box.t) : unit =\n"
            text += f"  {setup}\n"
            text += "  let[@ghost] _observed = (consume (actual [@ghost]) [@ghost]) in\n  ()\n"
            name = f"{category}_{actual_name}_{attack_name}"
            (out / f"{name}.ml").write_text(text)
