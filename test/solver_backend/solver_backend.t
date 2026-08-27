  $ ./solver_backend_tool.exe unit
  solver backend unit checks: smt.ml/direct-Z3 parity, ordinary-aggregate-int, explicit-named-sort, typed AUFLIA, projection, capability preflight, cleanup, isolation, Z3 4.15.2

The direct bridge preserves declaration order, its named sort, explicit
multi-pattern, and stable qid/skid in the exact Z3 rendering.

  $ ./solver_backend_tool.exe quantifier-render
  sort:0:Fuel
  fun:1:fuel_succ:(Fuel)->Fuel
  fun:2:observe$fuel:(Int,Fuel)->Int
  ---
  (declare-sort Fuel 0)
  (declare-fun fuel_succ (Fuel) Fuel)
  (declare-fun observe$fuel (Int Fuel) Int)
  (assert (forall ((verocaml_b0_x Int) (verocaml_b1_fuel Fuel))
    (! (= (observe$fuel verocaml_b0_x (fuel_succ verocaml_b1_fuel))
          (observe$fuel verocaml_b0_x (fuel_succ verocaml_b1_fuel)))
       :pattern ((observe$fuel verocaml_b0_x (fuel_succ verocaml_b1_fuel))
                 (fuel_succ verocaml_b1_fuel))
       :skolemid verocaml.observe.probe.skolem
       :qid verocaml.observe.probe)))
  (assert true)

The real Z3 smoke uses the minimum positive timeout.  Unknown is acceptable
here because the deterministic seam above tests its classification; this smoke
only requires that the real pinned backend remains callable.

  $ ./solver_backend_tool.exe timeout-smoke
  real 1ms timeout smoke: backend returned a classified outcome

The public configuration boundary rejects a non-positive timeout before a
solver is created.

  $ ./solver_backend_tool.exe invalid-config
  invalid solver configuration: solver timeout must be positive, found 0
