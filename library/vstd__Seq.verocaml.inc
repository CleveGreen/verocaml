(rule
 (targets vstd__Seq.vri vstd__Seq.verocaml-retained-interface vstd__Seq.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .vstd.objs/byte/vstd__Seq.cmt)
  (:cmi .vstd.objs/byte/vstd__Seq.cmi)
  (:cmti .vstd.objs/byte/vstd__Seq.cmti)
  (:transport_0_cmi .vstd.objs/byte/vstd.cmi)
  (:dependency_0_cmt .vstd.objs/byte/vstd__Int.cmt)
  (:dependency_0_cmi .vstd.objs/byte/vstd__Int.cmi)
  (:dependency_0_cmti .vstd.objs/byte/vstd__Int.cmti)
  (:dependency_0_vri vstd__Int.vri)
  (:dependency_0_manifest vstd__Int.verocaml-retained-interface))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} vstd__Seq.vri
    --artifact-directory .vstd.objs/byte --artifact-directory . --artifact-directory .vstd.objs/byte)
   (run %{emitter} manifest vstd__Seq.verocaml-retained-interface .vstd.objs/byte/vstd__Seq.cmt .vstd.objs/byte/vstd__Seq.cmi .vstd.objs/byte/vstd__Seq.cmti vstd__Seq.vri)
   (run %{emitter} manifest vstd__Seq.verocaml-retained-interface.install vstd__Seq.cmt vstd__Seq.cmi vstd__Seq.cmti vstd__Seq.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps vstd__Seq.vri vstd__Seq.verocaml-retained-interface))

(alias
 (name all)
 (deps vstd__Seq.vri vstd__Seq.verocaml-retained-interface))

(install
 (package verocaml)
 (section lib)
 (files
  (vstd__Seq.vri as vstd/vstd__Seq.vri)
  (vstd__Seq.verocaml-retained-interface.install as vstd/vstd__Seq.verocaml-retained-interface)))
