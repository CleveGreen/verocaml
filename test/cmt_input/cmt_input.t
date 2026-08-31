This transcript is the explicit CMT-loader specialist lane and is not a
dependency of the ordinary cmt-input outcome alias. Prepared implementation and
interface status/code behavior is asserted by outcome_cases.ml; the remaining
direct loader checks cover target bounds, malformed/incompatible artifacts,
accepted metadata and mode evidence, resolved standard-ref paths, and
interface/pack classification without asserting diagnostic spans.

Compile pinned OxCaml implementation, interface, and packed artifacts.

  $ mkdir artifacts
  $ cp fixtures/packed_member.ml artifacts/
  $ (cd artifacts && ocamlc -bin-annot -c packed_member.ml)
  $ (cd artifacts && ocamlc -bin-annot -pack -o packed.cmo packed_member.cmo)
  $ ./cmt_input_tool.exe prepared-implementation.cmt prepared-interface.cmti artifacts/packed.cmt
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
