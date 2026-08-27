from pathlib import Path
import sys

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
prelude = Path(sys.argv[2]).read_text()

attacks = {
    "direct": "[%verocaml.use_type_invariant x]; ()",
    "alias": "let y = x in [%verocaml.use_type_invariant y]; ()",
    "pattern": "let (y : Box.t) = x in [%verocaml.use_type_invariant y]; ()",
    "branch": (
        "let y = if true then x else x in "
        "[%verocaml.use_type_invariant y]; ()"
    ),
    "result": "let y = relay x in [%verocaml.use_type_invariant y]; ()",
}

actuals = {
    "exec": "let actual = source in",
    "tracked": "let[@tracked] actual = (source [@tracked]) in",
    "ghost": "let[@ghost] actual = (source [@ghost]) in",
}

for category in ("spec", "proof"):
    for actual_name, setup in actuals.items():
        for attack_name in (*attacks, "tracked_escalation"):
            text = prelude + "\n\n"
            if attack_name in ("result", "tracked_escalation"):
                text += (
                    "let consume (x : Box.t) : Box.t = x\n"
                    f"[@@verocaml.{category}]\n\n"
                    "let flow (source : Box.t) : unit =\n"
                    f"  {setup}\n"
                    "  let[@ghost] observed = (consume actual [@ghost]) in\n"
                )
                if attack_name == "result":
                    text += (
                        "  [%verocaml.proof\n"
                        "    [%verocaml.use_type_invariant observed]];\n"
                        "  ()\n"
                    )
                else:
                    text += (
                        "  let[@tracked] _escalated = "
                        "(observed [@tracked]) in\n"
                        "  ()\n"
                    )
            else:
                text += (
                    "let consume (x : Box.t) : unit =\n"
                    f"  {attacks[attack_name]}\n"
                    f"[@@verocaml.{category}]\n\n"
                    "let flow (source : Box.t) : unit =\n"
                    f"  {setup}\n"
                    "  let[@ghost] _observed = (consume actual [@ghost]) in\n"
                    "  ()\n"
                )
            name = f"{category}_{actual_name}_{attack_name}"
            (out / f"{name}.ml").write_text(text)
