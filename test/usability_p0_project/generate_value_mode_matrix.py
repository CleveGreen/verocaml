#!/usr/bin/env python3
"""Generate pinned-OxCaml value-mode position twins for the integration gate."""

from pathlib import Path
import sys


AXES = (
    ("areality", ("global", "regional")),
    ("forkable", ("forkable", "unforkable")),
    ("yielding", ("unyielding", "yielding")),
    ("linearity", ("many", "once")),
    ("statefulness", ("stateless", "observing", "stateful")),
    ("portability", ("portable", "shareable", "nonportable")),
    ("uniqueness", ("unique", "aliased")),
    ("visibility", ("read_write", "read", "immutable")),
    ("contention", ("uncontended", "shared", "contended")),
    ("staticity", ("static", "dynamic")),
)


def source_mode(axis: str, state: str) -> str:
    # Regionality is a real Value mode but has no source spelling at this pin.
    # A local function parameter is regional to the callee.
    return "local" if axis == "areality" and state == "regional" else state


def returning(axis: str, state: str, expression: str) -> str:
    return f"exclave_ {expression}" if axis == "areality" and state == "regional" else expression


def identifier(position: str, axis: str, state: str, kind: str) -> str:
    return f"{position}__{axis}__{state}__{kind}"


def generate(destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    positions = ["parameter", "return", "payload", "direct", "retained_import"]
    manifest = []
    source = []
    provider_mli = []
    provider_ml = ["[@@@verocaml.verify]"]
    client_ml = ["[@@@verocaml.verify]"]

    for axis, states in AXES:
        for state in states:
            mode = source_mode(axis, state)
            annotated = f"@ {mode}"
            modal = f"@@ {mode}"
            argument = f"inventory__{axis}__{state}"

            parameter = identifier("parameter", axis, state, "annotated")
            parameter_twin = identifier("parameter", axis, state, "inferred")
            source.extend(
                [
                    f"let {parameter} ({argument} : 'a {annotated}) = {argument}",
                    f"let {parameter_twin} (value : 'a) = value",
                ]
            )

            returned = identifier("return", axis, state, "annotated")
            returned_twin = identifier("return", axis, state, "inferred")
            source.extend(
                [
                    f"let {returned} ({argument} : 'a {annotated}) : "
                    f"'a {annotated} = {argument}",
                    f"let {returned_twin} (value : 'a) = value",
                ]
            )

            payload = identifier("payload", axis, state, "annotated")
            payload_twin = identifier("payload", axis, state, "inferred")
            source.extend(
                [
                    f"type 'a {payload} = {payload.capitalize()} of 'a {modal}",
                    f"type 'a {payload_twin} = "
                    f"{payload_twin.capitalize()} of 'a",
                ]
            )

            direct = identifier("direct", axis, state, "annotated")
            direct_twin = identifier("direct", axis, state, "inferred")
            source.extend(
                [
                    f"let {direct} ({argument} : 'a {annotated}) = "
                    f"{returning(axis, state, f'{parameter} {argument}')}",
                    f"let {direct_twin} (value : 'a) = {parameter_twin} value",
                ]
            )

            imported = identifier("retained_import", axis, state, "annotated")
            imported_twin = identifier("retained_import", axis, state, "inferred")
            provider_mli.extend(
                [
                    f"val {imported} : int {annotated} -> int",
                    f"val {imported_twin} : int -> int",
                ]
            )
            provider_ml.extend(
                [
                    f"let {imported} (value : int {annotated}) = value",
                    f"let {imported_twin} value = value",
                ]
            )
            client_ml.extend(
                [
                    f"let {imported} (value : int {annotated}) = "
                    f"Mode_provider.{imported} value",
                    f"let {imported_twin} value = "
                    f"Mode_provider.{imported_twin} value",
                ]
            )

            for position in positions:
                for kind in ("annotated", "inferred"):
                    manifest.append(identifier(position, axis, state, kind))

    # This binding is local to the current region, completing the internal
    # Areality state that cannot be written as a source mode annotation.
    source.extend(
        [
            "let inferred_areality_local value =",
            "  let local_ inventory__areality__local = (value, value) in",
            "  match inventory__areality__local with _, _ -> value",
        ]
    )

    (destination / "positions.ml").write_text("\n".join(source) + "\n")
    (destination / "mode_provider.mli").write_text("\n".join(provider_mli) + "\n")
    (destination / "mode_provider.ml").write_text("\n".join(provider_ml) + "\n")
    (destination / "mode_client.ml").write_text("\n".join(client_ml) + "\n")
    (destination / "manifest").write_text("\n".join(sorted(manifest)) + "\n")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("usage: generate_value_mode_matrix.py DESTINATION")
    generate(Path(sys.argv[1]))
