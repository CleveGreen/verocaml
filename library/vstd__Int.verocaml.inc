(rule
 (targets vstd__Int.vri vstd__Int.verocaml-retained-interface vstd__Int.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .vstd.objs/byte/vstd__Int.cmt)
  (:cmi .vstd.objs/byte/vstd__Int.cmi)
  (:cmti .vstd.objs/byte/vstd__Int.cmti)
  (:transport_0_cmi .vstd.objs/byte/vstd.cmi))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} vstd__Int.vri
    --artifact-directory .vstd.objs/byte)
   (run %{emitter} manifest vstd__Int.verocaml-retained-interface .vstd.objs/byte/vstd__Int.cmt .vstd.objs/byte/vstd__Int.cmi .vstd.objs/byte/vstd__Int.cmti vstd__Int.vri)
   (run %{emitter} manifest vstd__Int.verocaml-retained-interface.install vstd__Int.cmt vstd__Int.cmi vstd__Int.cmti vstd__Int.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps vstd__Int.vri vstd__Int.verocaml-retained-interface))

(alias
 (name all)
 (deps vstd__Int.vri vstd__Int.verocaml-retained-interface))

(install
 (package verocaml)
 (section lib)
 (files
  (vstd__Int.vri as vstd/vstd__Int.vri)
  (vstd__Int.verocaml-retained-interface.install as vstd/vstd__Int.verocaml-retained-interface)))
