Direct source verification uses retained ghost annotations in private temporary
storage and leaves no compiler artifacts beside the source. The retained-CMT
route remains available.

  $ mkdir -p artifacts/temp artifacts/nested
  $ TMPDIR="$PWD/artifacts/temp" OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/verified.ml --dump-sst artifacts/verified.sst --dump-vir artifacts/verified.vir > artifacts/verified.out 2> artifacts/verified.err
  $ cat artifacts/verified.out
  verocaml: verified file=fixtures/verified.ml functions=1 obligations=1
  $ TMPDIR="$PWD/artifacts/temp" OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/verified.ml --dump-sst artifacts/verified-again.sst --dump-vir artifacts/verified-again.vir > artifacts/verified-again.out 2> artifacts/verified-again.err
  $ cmp artifacts/verified.sst artifacts/verified-again.sst
  $ cmp artifacts/verified.vir artifacts/verified-again.vir
  $ cmp artifacts/verified.out artifacts/verified-again.out
  $ cmp artifacts/verified.err artifacts/verified-again.err
  $ grep '^authority=retained ' artifacts/verified.sst | sed -E 's/interface=[0-9a-f]+/interface=<digest>/'
  authority=retained unit=Source source=fixtures/verified.ml cmt=<private-source-compilation-cmt> interface=<digest>
  $ grep -v '^authority=retained ' artifacts/verified.sst > artifacts/verified.non-cmt
  $ grep -v '^authority=retained ' artifacts/verified-again.sst > artifacts/verified-again.non-cmt
  $ cmp artifacts/verified.non-cmt artifacts/verified-again.non-cmt
  $ test -s artifacts/verified.sst && test -s artifacts/verified.vir
  $ test -z "$(find artifacts/temp -mindepth 1 -print -quit)"
  $ test ! -e fixtures/verified.cmi
  $ test ! -e fixtures/verified.cmo
  $ test ! -e fixtures/verified.cmt
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/verified.cmo fixtures/verified.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/verified.cmt --dump-sst artifacts/direct-first.sst --dump-vir artifacts/direct-first.vir > artifacts/direct-first.out 2> artifacts/direct-first.err
  $ cat artifacts/direct-first.out
  verocaml: verified file=artifacts/verified.cmt functions=1 obligations=1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/verified.cmt --dump-sst artifacts/direct-second.sst --dump-vir artifacts/direct-second.vir > artifacts/direct-second.out 2> artifacts/direct-second.err
  $ cmp artifacts/direct-first.sst artifacts/direct-second.sst
  $ cmp artifacts/direct-first.vir artifacts/direct-second.vir
  $ cmp artifacts/direct-first.out artifacts/direct-second.out
  $ cmp artifacts/direct-first.err artifacts/direct-second.err
  $ grep '^authority=retained ' artifacts/direct-first.sst | sed -E 's/interface=[0-9a-f]+/interface=<digest>/'
  authority=retained unit=Verified source=fixtures/verified.ml cmt=artifacts/verified.cmt interface=<digest>
  $ ! grep -q '<private-source-compilation-cmt>' artifacts/direct-first.sst

The stable source-CMT presentation is exact even when the CLI source path has
spaces and contains text resembling the private temporary-directory prefix.
The complete one-line source, status and counts, source-visible paths, spans,
interface digest, non-CMT SST lines, and VIR remain byte-identical.

  $ mkdir -p "artifacts/path with spaces"
  $ printf 'let identity (value : int) = value\n' > "artifacts/path with spaces/verocaml-source-visible.ml"
  $ for run in first second; do TMPDIR="$PWD/artifacts/temp" OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/path with spaces/verocaml-source-visible.ml" --dump-sst "artifacts/space-$run.sst" --dump-vir "artifacts/space-$run.vir" > "artifacts/space-$run.out" 2> "artifacts/space-$run.err"; done
  $ cmp artifacts/space-first.sst artifacts/space-second.sst
  $ cmp artifacts/space-first.vir artifacts/space-second.vir
  $ cmp artifacts/space-first.out artifacts/space-second.out
  $ cmp artifacts/space-first.err artifacts/space-second.err
  $ grep -F 'verocaml: verified file=artifacts/path with spaces/verocaml-source-visible.ml functions=1 obligations=0' artifacts/space-first.out
  verocaml: verified file=artifacts/path with spaces/verocaml-source-visible.ml functions=1 obligations=0
  $ grep '^authority=retained ' artifacts/space-first.sst | sed -E 's/interface=[0-9a-f]+/interface=<digest>/'
  authority=retained unit=Source source=artifacts/path with spaces/verocaml-source-visible.ml cmt=<private-source-compilation-cmt> interface=<digest>
  $ grep -F '@ verocaml-source-visible.ml:1:0-1:34' artifacts/space-first.sst
  function identity#0 mode=exec recursive=false result=int policy=default-linear/default-z3 @ verocaml-source-visible.ml:1:0-1:34
  $ test "$(grep -c '<private-source-compilation-cmt>' artifacts/space-first.sst)" = 1
  $ ! grep -E 'cmt=.*/verocaml-source-[^/]*/source[.]cmt' artifacts/space-first.sst
  $ grep -v '^authority=retained ' artifacts/space-first.sst > artifacts/space-first.non-cmt
  $ grep -v '^authority=retained ' artifacts/space-second.sst > artifacts/space-second.non-cmt
  $ cmp artifacts/space-first.non-cmt artifacts/space-second.non-cmt
  $ test -z "$(find artifacts/temp -mindepth 1 -print -quit)"

The mandatory input operand may begin with one or two dashes. A maximal legal
source basename does not flow into the bounded internal output name.

  $ cp fixtures/verified.ml ./-impl.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify -impl.ml
  verocaml: verified file=-impl.ml functions=1 obligations=1
  $ cp fixtures/verified.ml ./--source.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify --source.ml
  verocaml: verified file=--source.ml functions=1 obligations=1
  $ long_name="$(printf '%0252d' 0 | tr 0 a).ml"
  $ cp fixtures/verified.ml "$long_name"
  $ OCAML_COLOR=never ../../src/verocaml.exe verify "$long_name" | sed "s/$long_name/<maximal-name>.ml/"
  verocaml: verified file=<maximal-name>.ml functions=1 obligations=1
  $ test ! -e ./-impl.cmi && test ! -e ./-impl.cmo && test ! -e ./-impl.cmt
  $ test ! -e ./--source.cmi && test ! -e ./--source.cmo && test ! -e ./--source.cmt

Input and dump paths are pairwise distinct by normalized absolute spelling,
existing canonical target, and existing device/inode identity. Every rejection
happens before source/CMT consumption or dump creation and preserves content.

  $ reject_preserving () { label=$1; protected=$2; shift 2; sha256sum "$protected" > "artifacts/$label.before"; if OCAML_COLOR=never ../../src/verocaml.exe "$@" > "artifacts/$label.out" 2>&1; then return 1; else code=$?; fi; test "$code" = 2; grep -F 'identify the same file' "artifacts/$label.out" >/dev/null; sha256sum "$protected" > "artifacts/$label.after"; cmp "artifacts/$label.before" "artifacts/$label.after"; printf '%s: rejected and preserved\n' "$label"; }
  $ cp fixtures/verified.ml artifacts/protected-source.ml
  $ reject_preserving source-sst artifacts/protected-source.ml verify artifacts/protected-source.ml --dump-sst artifacts/protected-source.ml
  source-sst: rejected and preserved
  $ reject_preserving source-vir artifacts/protected-source.ml verify artifacts/protected-source.ml --dump-vir artifacts/protected-source.ml
  source-vir: rejected and preserved
  $ reject_preserving normalized artifacts/protected-source.ml verify "$PWD/artifacts/protected-source.ml" --dump-sst artifacts/nested/../protected-source.ml
  normalized: rejected and preserved
  $ ln -s protected-source.ml artifacts/source-link.ml
  $ reject_preserving canonical artifacts/protected-source.ml verify artifacts/source-link.ml --dump-sst artifacts/protected-source.ml
  canonical: rejected and preserved
  $ ln artifacts/protected-source.ml artifacts/source-hardlink.ml
  $ reject_preserving hardlink artifacts/protected-source.ml verify artifacts/protected-source.ml --dump-sst artifacts/source-hardlink.ml
  hardlink: rejected and preserved
  $ mkdir -p artifacts/spelling artifacts/real/nested
  $ ln -s ../real/nested artifacts/spelling/via-link
  $ cp fixtures/verified.ml artifacts/real/protected-source.ml
  $ reject_preserving symlink-prefix-parent artifacts/real/protected-source.ml verify artifacts/real/protected-source.ml --dump-sst artifacts/spelling/via-link/../protected-source.ml
  symlink-prefix-parent: rejected and preserved
  $ reject_preserving cmt-sst artifacts/verified.cmt verify artifacts/verified.cmt --dump-sst artifacts/verified.cmt
  cmt-sst: rejected and preserved
  $ reject_preserving cmt-vir artifacts/verified.cmt verify artifacts/verified.cmt --dump-vir artifacts/verified.cmt
  cmt-vir: rejected and preserved
  $ printf 'same dump sentinel\n' > artifacts/shared.dump
  $ reject_preserving dumps-same artifacts/shared.dump verify fixtures/verified.ml --dump-sst artifacts/shared.dump --dump-vir artifacts/shared.dump
  dumps-same: rejected and preserved
  $ printf 'normalized dump sentinel\n' > artifacts/normalized.dump
  $ reject_preserving dumps-normalized artifacts/normalized.dump verify fixtures/verified.ml --dump-sst "$PWD/artifacts/normalized.dump" --dump-vir artifacts/nested/../normalized.dump
  dumps-normalized: rejected and preserved
  $ printf 'symlink dump sentinel\n' > artifacts/symlink-target.dump
  $ ln -s symlink-target.dump artifacts/symlink-alias.dump
  $ reject_preserving dumps-symlink artifacts/symlink-target.dump verify fixtures/verified.ml --dump-sst artifacts/symlink-target.dump --dump-vir artifacts/symlink-alias.dump
  dumps-symlink: rejected and preserved
  $ printf 'hardlink dump sentinel\n' > artifacts/hardlink-target.dump
  $ ln artifacts/hardlink-target.dump artifacts/hardlink-alias.dump
  $ reject_preserving dumps-hardlink artifacts/hardlink-target.dump verify fixtures/verified.ml --dump-sst artifacts/hardlink-target.dump --dump-vir artifacts/hardlink-alias.dump
  dumps-hardlink: rejected and preserved
  $ printf 'symlink parent dump sentinel\n' > artifacts/real/protected.dump
  $ reject_preserving dumps-symlink-prefix-parent artifacts/real/protected.dump verify fixtures/verified.ml --dump-sst artifacts/real/protected.dump --dump-vir artifacts/spelling/via-link/../protected.dump
  dumps-symlink-prefix-parent: rejected and preserved

The immutable direct-source route carries accepted trusted external
specification disclosure into the public CLI.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/external_specification.ml | grep -E 'trusted external specification|verified-with-trusted-axioms'
  verocaml: trusted external specification trust=axiomatic target=opaque#0 wrapper=opaque_specification#1 target-span=fixtures/external_specification.ml:1:0-1:34 wrapper-span=fixtures/external_specification.ml:3:0-7:35 witness-span=fixtures/external_specification.ml:7:0-7:35 call=fixtures/external_specification.ml:11:2-11:10 requires=1 ensures=1 result=unconstrained
  verocaml: verified-with-trusted-axioms file=fixtures/external_specification.ml functions=1 obligations=1 trusted-external-spec-uses=1
