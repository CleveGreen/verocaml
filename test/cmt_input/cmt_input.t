Compile pinned OxCaml implementation, interface, and packed artifacts. The OCaml
assertion tool checks exact 63-bit bounds, startup-before-I/O target rejection,
malformed and incompatible artifacts, accepted implementation metadata and
mode evidence, resolved standard-ref paths, and source-located interface/pack
classification.

  $ mkdir artifacts
  $ cp fixtures/implementation.ml fixtures/interface.mli fixtures/packed_member.ml artifacts/
  $ (cd artifacts && ocamlc -bin-annot -c implementation.ml)
  $ (cd artifacts && ocamlc -bin-annot -c interface.mli)
  $ (cd artifacts && ocamlc -bin-annot -c packed_member.ml)
  $ (cd artifacts && ocamlc -bin-annot -pack -o packed.cmo packed_member.cmo)
  $ ./cmt_input_tool.exe artifacts/implementation.cmt artifacts/interface.cmti artifacts/packed.cmt
  CMT input checks passed
