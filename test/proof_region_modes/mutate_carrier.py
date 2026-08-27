#!/usr/bin/env python3
import re
import sys
from pathlib import Path


source = Path(sys.argv[1])
target = Path(sys.argv[2])
attack = sys.argv[3]
data = bytearray(source.read_bytes())
pattern = re.compile(
    rb"verocaml:proof-region-capture:1:issuer=ppx-v1"
    rb"\|callable=[0-9a-f]+"
    rb"\|binding=[0-9]+,[0-9]+"
    rb"\|body=[0-9]+,[0-9]+"
    rb"\|region=[0-9]+,[0-9]+"
    rb"\|slots=[0-9a-f,;]*"
)
matches = list(pattern.finditer(data))
if len(matches) != 1:
    raise SystemExit(f"expected one shared manifest, found {len(matches)}")

manifest = bytes(matches[0].group())


def same_length_replace(old: bytes, new: bytes, *, occurrence=None):
    global data
    if len(old) != len(new):
        raise SystemExit("mutation changed serialized string length")
    if occurrence is None:
        count = data.count(old)
        if count == 0:
            raise SystemExit(f"missing mutation target {old!r}")
        data = bytearray(bytes(data).replace(old, new))
    else:
        positions = [match.start() for match in re.finditer(re.escape(old), data)]
        if occurrence >= len(positions):
            raise SystemExit("missing requested mutation occurrence")
        start = positions[occurrence]
        data[start : start + len(old)] = new


text = manifest.decode()
slots_text = text.split("|slots=", 1)[1]
slots = slots_text.split(";") if slots_text else []

if attack == "wrong-callable":
    field = re.search(rb"callable=([0-9a-f]+)", manifest).group(1)
    changed = (b"0" if field[:1] != b"0" else b"1") + field[1:]
    same_length_replace(b"callable=" + field, b"callable=" + changed)
elif attack == "wrong-binding-span":
    field = re.search(rb"binding=([0-9]+),([0-9]+)", manifest)
    start = field.group(1)
    changed = start[:-1] + (b"0" if start[-1:] != b"0" else b"1")
    same_length_replace(b"binding=" + start, b"binding=" + changed)
elif attack == "wrong-region-id":
    region = re.search(rb"region=([0-9]+),([0-9]+)", manifest)
    start, end = region.groups()
    region_id = b"verocaml:proof-region:2:" + start + b":" + end
    changed = region_id[:-1] + (
        b"0" if region_id[-1:] != b"0" else b"1"
    )
    same_length_replace(region_id, changed)
elif attack == "missing-slot":
    if len(slots) < 2:
        raise SystemExit("missing-slot needs two slots")
    changed = slots_text.replace(";", ":", 1)
    same_length_replace(
        ("slots=" + slots_text).encode(), ("slots=" + changed).encode()
    )
elif attack == "reordered-slots":
    if len(slots) != 2 or len(slots[0]) != len(slots[1]):
        raise SystemExit("reordered-slots needs two equal-length slots")
    changed = ";".join(reversed(slots))
    same_length_replace(
        ("slots=" + slots_text).encode(), ("slots=" + changed).encode()
    )
elif attack == "duplicate-slot":
    if len(slots) != 2 or len(slots[0]) != len(slots[1]):
        raise SystemExit("duplicate-slot needs two equal-length slots")
    changed = slots[0] + ";" + slots[0]
    same_length_replace(
        ("slots=" + slots_text).encode(), ("slots=" + changed).encode()
    )
else:
    raise SystemExit(f"unknown attack {attack}")

target.write_bytes(data)
