#!/usr/bin/env python3
import pathlib
import re
import sys


source = pathlib.Path(sys.argv[1])
target = pathlib.Path(sys.argv[2])
attack = sys.argv[3]
payload = source.read_bytes()


def mutate_match(pattern: bytes, label: str, group: int) -> None:
    global payload
    matches = list(re.finditer(pattern, payload))
    if not matches:
        raise SystemExit(f"{attack}: no {label} carrier found")
    match = matches[0]
    value = bytearray(match.group(group))
    candidates = [
        index
        for index, byte in enumerate(value)
        if chr(byte) in "0123456789abcdef"
    ]
    if not candidates:
        raise SystemExit(f"{attack}: {label} has no mutable identity byte")
    index = candidates[-1]
    value[index] = ord("0") if value[index] != ord("0") else ord("1")
    start, stop = match.span(group)
    payload = payload[:start] + bytes(value) + payload[stop:]


def mutate_decimal(pattern: bytes, label: str, group: int) -> None:
    global payload
    matches = list(re.finditer(pattern, payload))
    if not matches:
        raise SystemExit(f"{attack}: no {label} span found")
    match = matches[0]
    value = bytearray(match.group(group))
    index = len(value) - 1
    value[index] = ord("0") if value[index] != ord("0") else ord("1")
    start, stop = match.span(group)
    payload = payload[:start] + bytes(value) + payload[stop:]


if attack == "declaration-id":
    mutate_match(
        rb"(declaration\.[0-9a-f]{32})\|[0-9]+\|[0-9]+",
        "declaration",
        1,
    )
elif attack == "carrier-id":
    mutate_match(
        rb"structure\|(structure\.[0-9a-f]{32})\|\|[0-9]+\|[0-9]+",
        "structure",
        1,
    )
elif attack == "group-id":
    mutate_match(
        rb"group\|(group\.[0-9a-f]{32})\|[A-Za-z_][A-Za-z0-9_']*\|[0-9]+\|[0-9]+",
        "group",
        1,
    )
elif attack == "marker":
    old = b"verocaml:broadcast:carrier:v1:"
    new = b"verocaml:broadcast:carriem:v1:"
    if payload.count(old) == 0 or len(old) != len(new):
        raise SystemExit(f"{attack}: marker target is absent")
    payload = payload.replace(old, new, 1)
elif attack == "declaration-span":
    mutate_decimal(
        rb"declaration\.[0-9a-f]{32}\|([0-9]+)\|[0-9]+",
        "declaration",
        1,
    )
elif attack == "carrier-span":
    mutate_decimal(
        rb"structure\|structure\.[0-9a-f]{32}\|\|([0-9]+)\|[0-9]+",
        "structure",
        1,
    )
elif attack == "group-span":
    mutate_decimal(
        rb"group\|group\.[0-9a-f]{32}\|[A-Za-z_][A-Za-z0-9_']*\|([0-9]+)\|[0-9]+",
        "group",
        1,
    )
elif attack == "scope-span":
    mutate_decimal(
        rb"expression\.[0-9a-f]{32}\|([0-9]+)\|[0-9]+",
        "expression scope",
        1,
    )
elif attack in ("legacy-lemma", "legacy-axiom"):
    old = b"declaration."
    new = b"lemma|lemma." if attack == "legacy-lemma" else b"axiom|axiom."
    if payload.count(old) == 0 or len(old) != len(new):
        raise SystemExit(f"{attack}: neutral declaration carrier is absent")
    payload = payload.replace(old, new, 1)
else:
    raise SystemExit(f"unknown attack {attack}")

target.write_bytes(payload)
