Immutable ticket replay compares only the recorded Git objects and exact record
tuple. Worktree dirt is deliberately irrelevant.

  $ root="${PWD%%/_build/*}"; python3 replay_tests.py "$root"
  object-replay records=2 positive=6 per-record-negatives=18
  object-replay dirt-independence=passed bare=2 missing-repository/object=passed
