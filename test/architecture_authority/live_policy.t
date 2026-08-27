The live policy and materialized parametric ratchet run without repository
metadata and reject one independent mutation per preserved property.

  $ root="${PWD%%/_build/*}"; python3 live_policy_tests.py "$root"
  live-policy controls positive=2 negatives=14 gitless=passed
  install-provenance wrappers matched=2 cross-pair=2
  preserved-properties owner/span/package/runtime/forbidden/concentration=passed
