#!/usr/bin/env python3
import pathlib
import re
import sys


source = pathlib.Path(sys.argv[1])
target = pathlib.Path(sys.argv[2])
attack = sys.argv[3]
payload = source.read_bytes()


def replace_group(pattern: bytes, group: int, label: str) -> None:
    global payload
    match = re.search(pattern, payload)
    if match is None:
        raise SystemExit(f"{attack}: no {label} found")
    value = bytearray(match.group(group))
    mutable = [
        index
        for index, byte in enumerate(value)
        if chr(byte) in "0123456789abcdef"
    ]
    if not mutable:
        raise SystemExit(f"{attack}: {label} has no mutable byte")
    index = mutable[-1]
    value[index] = ord("0") if value[index] != ord("0") else ord("1")
    start, stop = match.span(group)
    payload = payload[:start] + bytes(value) + payload[stop:]


carrier = (
    rb"v1\|(symbolic\.[0-9a-f]{32})\|"
    rb"([A-Za-z_][A-Za-z0-9_']*)\|([0-9]+)\|([0-9]+)\|"
    rb"([0-9a-f]{32})"
)

if attack == "marker":
    replace_group(carrier, 1, "marker")
elif attack == "type-vector":
    replace_group(carrier, 5, "type vector")
elif attack == "span":
    replace_group(carrier, 3, "source span")
elif attack == "path":
    match = re.search(carrier, payload)
    if match is None:
        raise SystemExit(f"{attack}: no declaration path found")
    value = bytearray(match.group(2))
    value[0] = ord("z") if value[0] != ord("z") else ord("y")
    start, stop = match.span(2)
    payload = payload[:start] + bytes(value) + payload[stop:]
elif attack == "uid":
    match = re.search(carrier, payload)
    if match is None:
        raise SystemExit(f"{attack}: no declaration UID anchor found")
    name = match.group(2)
    occurrence = payload.find(name)
    if occurrence < 0 or occurrence >= match.start(2):
        raise SystemExit(f"{attack}: no independent declaration UID/name found")
    replacement = bytes([ord("z") if name[0] != ord("z") else ord("y")])
    payload = (
        payload[:occurrence]
        + replacement
        + payload[occurrence + 1 :]
    )
else:
    raise SystemExit(f"unknown attack {attack}")

target.write_bytes(payload)
