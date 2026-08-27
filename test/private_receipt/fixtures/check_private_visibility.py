import pathlib
import sys

text = pathlib.Path(sys.argv[1]).read_text()
blocks = text.split("\n   (module\n")[1:]
for authority in sys.argv[2:]:
    matches = [block for block in blocks if f"(path {authority})" in block]
    if len(matches) != 1 or "(visibility private)" not in matches[0]:
        raise SystemExit(f"not-private:{authority}")
print(f"private-authority-modules={len(sys.argv) - 2}")
