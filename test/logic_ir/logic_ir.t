The typed logic IR rejects sort, scope, signature, and pattern attacks without
requiring a backend.
This is the local `typed-boundary-and-backend-ratchet` architecture/resource
exception and is intentionally independent of `logic-ir-outcome-check`.

  $ ./logic_ir_tool.exe
  logic IR checks: declarations, sorts, scope, patterns, ids, spans, requirements
