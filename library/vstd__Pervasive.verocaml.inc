(rule
 (targets vstd__Pervasive.vri vstd__Pervasive.verocaml-retained-interface vstd__Pervasive.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .vstd.objs/byte/vstd__Pervasive.cmt)
  (:cmi .vstd.objs/byte/vstd__Pervasive.cmi)
  (:cmti .vstd.objs/byte/vstd__Pervasive.cmti)
  (:transport_0_cmi .vstd.objs/byte/vstd.cmi))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} vstd__Pervasive.vri
    --artifact-directory .vstd.objs/byte)
   (run %{emitter} manifest vstd__Pervasive.verocaml-retained-interface .vstd.objs/byte/vstd__Pervasive.cmt .vstd.objs/byte/vstd__Pervasive.cmi .vstd.objs/byte/vstd__Pervasive.cmti vstd__Pervasive.vri)
   (run %{emitter} manifest vstd__Pervasive.verocaml-retained-interface.install vstd__Pervasive.cmt vstd__Pervasive.cmi vstd__Pervasive.cmti vstd__Pervasive.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps vstd__Pervasive.vri vstd__Pervasive.verocaml-retained-interface))

(alias
 (name all)
 (deps vstd__Pervasive.vri vstd__Pervasive.verocaml-retained-interface))

(install
 (package verocaml)
 (section lib)
 (files
  (vstd__Pervasive.vri as vstd/vstd__Pervasive.vri)
  (vstd__Pervasive.verocaml-retained-interface.install as vstd/vstd__Pervasive.verocaml-retained-interface)))
