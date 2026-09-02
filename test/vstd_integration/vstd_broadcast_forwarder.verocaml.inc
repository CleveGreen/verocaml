(rule
 (targets vstd_broadcast_forwarder.vri vstd_broadcast_forwarder.verocaml-retained-interface vstd_broadcast_forwarder.verocaml-retained-interface.install)
 (deps
  (sandbox always)
  (:emitter %{bin:verocaml-retained-interface})
  (:cmt .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmt)
  (:cmi .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmi)
  (:cmti .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmti)
  (:dependency_transport_0_cmi ../../library/.vstd.objs/byte/vstd.cmi)
  (:dependency_0_cmt ../../library/.vstd.objs/byte/vstd__Seq.cmt)
  (:dependency_0_cmi ../../library/.vstd.objs/byte/vstd__Seq.cmi)
  (:dependency_0_cmti ../../library/.vstd.objs/byte/vstd__Seq.cmti)
  (:dependency_0_vri ../../library/vstd__Seq.vri)
  (:dependency_0_manifest ../../library/vstd__Seq.verocaml-retained-interface)
  (:dependency_1_cmt ../../library/.vstd.objs/byte/vstd__Int.cmt)
  (:dependency_1_cmi ../../library/.vstd.objs/byte/vstd__Int.cmi)
  (:dependency_1_cmti ../../library/.vstd.objs/byte/vstd__Int.cmti)
  (:dependency_1_vri ../../library/vstd__Int.vri)
  (:dependency_1_manifest ../../library/vstd__Int.verocaml-retained-interface))
 (action
  (progn
   (run %{emitter} emit %{cmt} %{cmi} %{cmti} vstd_broadcast_forwarder.vri
    --artifact-directory .vstd_broadcast_forwarder.objs/byte --artifact-directory ../../library --artifact-directory ../../library/.vstd.objs/byte)
   (run %{emitter} manifest vstd_broadcast_forwarder.verocaml-retained-interface .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmt .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmi .vstd_broadcast_forwarder.objs/byte/vstd_broadcast_forwarder.cmti vstd_broadcast_forwarder.vri)
   (run %{emitter} manifest vstd_broadcast_forwarder.verocaml-retained-interface.install vstd_broadcast_forwarder.cmt vstd_broadcast_forwarder.cmi vstd_broadcast_forwarder.cmti vstd_broadcast_forwarder.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps vstd_broadcast_forwarder.vri vstd_broadcast_forwarder.verocaml-retained-interface))

(alias
 (name all)
 (deps vstd_broadcast_forwarder.vri vstd_broadcast_forwarder.verocaml-retained-interface))

(install
 (package verocaml)
 (section lib)
 (files
  (vstd_broadcast_forwarder.vri as vstd-integration/vstd_broadcast_forwarder.vri)
  (vstd_broadcast_forwarder.verocaml-retained-interface.install as vstd-integration/vstd_broadcast_forwarder.verocaml-retained-interface)))
