The standalone mapper and public Ppxlib driver remain silent unless Delator is
explicitly enabled.

  $ mkdir artifacts
  $ env -u DELATOR_LOG OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o artifacts/standalone.cmo fixtures/implementation.ml > artifacts/standalone.out 2> artifacts/standalone.err
  $ test ! -s artifacts/standalone.out
  $ test ! -s artifacts/standalone.err
  $ env -u DELATOR_LOG ./ppx_driver_observability_tool.exe > artifacts/driver.out 2> artifacts/driver.err
  $ test ! -s artifacts/driver.out
  $ test ! -s artifacts/driver.err

PPX observability executes in the rewriting process and does not introduce a
Delator dependency into transformed user code.

  $ ocamlobjinfo artifacts/standalone.cmo | grep -q Delator; test $? = 1

Explicit tracing is available through standalone implementation and interface
rewrites without making the telemetry text part of the compatibility surface.

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o artifacts/traced.cmo fixtures/implementation.ml > artifacts/traced.out 2> artifacts/traced.err
  $ test -s artifacts/traced.err
  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o artifacts/interface.cmi fixtures/interface.mli > artifacts/interface.out 2> artifacts/interface.err
  $ test -s artifacts/interface.err

The public Ppxlib entry points expose the same opt-in tracing boundary.

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_COLOR=never ./ppx_driver_observability_tool.exe > artifacts/public.out 2> artifacts/public.err
  $ test -s artifacts/public.err
