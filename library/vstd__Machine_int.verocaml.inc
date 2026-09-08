(rule
 (targets vstd__Machine_int.vri vstd__Machine_int.verocaml-retained-interface vstd__Machine_int.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .vstd.objs/byte/vstd__Machine_int.cmt)
  (:cmi .vstd.objs/byte/vstd__Machine_int.cmi)
  (:cmti .vstd.objs/byte/vstd__Machine_int.cmti)
  (:transport_0_cmi .vstd.objs/byte/vstd.cmi)
  (:dependency_0_cmt .vstd.objs/byte/vstd__Int.cmt)
  (:dependency_0_cmi .vstd.objs/byte/vstd__Int.cmi)
  (:dependency_0_cmti .vstd.objs/byte/vstd__Int.cmti)
  (:dependency_0_vri vstd__Int.vri)
  (:dependency_0_manifest vstd__Int.verocaml-retained-interface))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} vstd__Machine_int.vri
    --artifact-directory .vstd.objs/byte --artifact-directory . --artifact-directory .vstd.objs/byte)
   (run %{emitter} manifest vstd__Machine_int.verocaml-retained-interface .vstd.objs/byte/vstd__Machine_int.cmt .vstd.objs/byte/vstd__Machine_int.cmi .vstd.objs/byte/vstd__Machine_int.cmti vstd__Machine_int.vri)
   (run %{emitter} manifest vstd__Machine_int.verocaml-retained-interface.install vstd__Machine_int.cmt vstd__Machine_int.cmi vstd__Machine_int.cmti vstd__Machine_int.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps vstd__Machine_int.vri vstd__Machine_int.verocaml-retained-interface))

(alias
 (name all)
 (deps vstd__Machine_int.vri vstd__Machine_int.verocaml-retained-interface))

(install
 (package verocaml)
 (section lib)
 (files
  (vstd__Machine_int.vri as vstd/vstd__Machine_int.vri)
  (vstd__Machine_int.verocaml-retained-interface.install as vstd/vstd__Machine_int.verocaml-retained-interface)))
