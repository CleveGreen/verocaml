from pathlib import Path
import re, sys
root = Path(sys.argv[1]).resolve()
owner = (root / "src/parametric_rank_domain_private.ml").read_text()
interface = (root / "src/parametric_rank_domain_private.mli").read_text()
adapter = (root / "src/typedtree_adapter_issuance_private.ml").read_text()
assert len(owner.splitlines()) <= 650
assert len(interface.splitlines()) <= 180
assert "type validated_rank_domain" in owner
assert "let derive_application" in owner
assert "let seal_local_schemas" in owner
assert "issued_rank_domain_registry" not in adapter
for path in ["finite_value_registry.mli", "rank_encoding.mli", "recursive_spec_preservation.mli", "sst_rank_domain_validation_private.mli", "sst_validation.mli", "termination.mli"]:
    assert (root / "src" / path).exists()
starts = [m.start() for m in re.finditer(r"(?m)^let (?:rec )?[a-zA-Z0-9_]+", owner)]
starts.append(len(owner))
assert max(b-a for a,b in zip(starts, starts[1:])) < 12000
print(f"owner-lines={len(owner.splitlines())} interface-lines={len(interface.splitlines())} single-owner=true")
