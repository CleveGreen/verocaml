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

An ordinary load authenticates the exact adjacent explicit CMI unit and
implementation identities, even when its self CRC slot still matches the CMT.

  $ cp fixtures/explicit_interface.ml fixtures/explicit_interface.mli artifacts/
  $ (cd artifacts && ocamlc -bin-annot -c explicit_interface.mli && ocamlc -bin-annot -c explicit_interface.ml)
  $ cp artifacts/explicit_interface.cmi artifacts/explicit_interface.valid.cmi
  $ ./cmt_input_tool.exe forge-interface-identity artifacts/explicit_interface.valid.cmi artifacts/explicit_interface.cmi Wrong_unit
  $ ./cmt_input_tool.exe rejected-implementation artifacts/explicit_interface.cmt
  ordinary CMT/CMI identity rejected

Recorded relative load paths relocate from the compiler output directory to the
actual CMT directory.  A working directory containing the same relative names
cannot override that mapping, and visible precedence is unchanged.

  $ mkdir -p relocation/original/{objects,first,second} relocation/current/{objects,first,second} relocation/poison/{objects,first,second}
  $ cp fixtures/relocated.ml relocation/original/
  $ (cd relocation/original && ocamlc -bin-annot -I objects -I first -I second -c -o objects/relocated.cmo relocated.ml)
  $ cp relocation/original/objects/relocated.{cmi,cmt} relocation/current/objects/
  $ tool="$PWD/cmt_input_tool.exe"
  $ (cd relocation/poison && "$tool" relocated-load-path ../current/objects/relocated.cmt ../current/objects ../current/first ../current/second)
  relocated load-path order=objects,first,second
