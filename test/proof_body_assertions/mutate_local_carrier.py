#!/usr/bin/env python3
import pathlib
import re
import sys

source = pathlib.Path(sys.argv[1])
target = pathlib.Path(sys.argv[2])
attack = sys.argv[3] if len(sys.argv) > 3 else "ordinal"
payload = source.read_bytes()
local_carrier_pattern = re.compile(
    rb"verocaml:proof-region-capture:1:issuer=ppx-v1"
    rb"\|callable=[0-9a-f]+"
    rb"\|binding=[0-9]+,[0-9]+"
    rb"\|body=[0-9]+,[0-9]+"
    rb"\|region=[0-9]+,[0-9]+"
    rb"\|slots=[0-9a-f,;]*"
    rb"\|kind=local-assert"
    rb"\|ordinal=[0-9]+"
    rb"\|predicate=[0-9]+,[0-9]+"
)


def replace_once(needle: bytes, replacement: bytes) -> None:
    global payload
    if len(needle) != len(replacement) or payload.count(needle) != 1:
        raise SystemExit(
            f"{attack}: expected one same-length marker, found {payload.count(needle)}"
        )
    payload = payload.replace(needle, replacement)


def mutate_local_carrier(pattern: bytes) -> None:
    carriers = list(local_carrier_pattern.finditer(payload))
    if len(carriers) != 1:
        raise SystemExit(
            f"{attack}: expected one exact local carrier, found {len(carriers)}"
        )
    carrier = carriers[0].group(0)
    fields = list(re.finditer(pattern, carrier))
    if len(fields) != 1:
        raise SystemExit(
            f"{attack}: expected one exact local-carrier field, found {len(fields)}"
        )
    field = fields[0].group(0)
    last = field[-1:]
    replacement_digit = b"0" if last != b"0" else b"1"
    mutated = carrier[: fields[0].start()] + field[:-1] + replacement_digit
    mutated += carrier[fields[0].end() :]
    replace_once(carrier, mutated)


if attack == "local-id":
    replace_once(b"verocaml:local-assert:1:", b"verocaml:local-asserx:1:")
elif attack in {
    "ordinal",
    "kind",
    "predicate-span",
    "callable",
    "body",
    "region",
}:
    patterns = {
        "ordinal": rb"\|ordinal=[0-9]+",
        "kind": rb"\|kind=local-assert",
        "predicate-span": rb"\|predicate=([0-9]+),([0-9]+)",
        "callable": rb"\|callable=([0-9a-f]+)",
        "body": rb"\|body=([0-9]+),([0-9]+)",
        "region": rb"\|region=([0-9]+),([0-9]+)",
    }
    mutate_local_carrier(patterns[attack])
else:
    raise SystemExit(f"unknown attack {attack}")

target.write_bytes(payload)
