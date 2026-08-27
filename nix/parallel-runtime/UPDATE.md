# Parallel runtime update procedure

This directory owns only the VERO-084 compiler/package prerequisite.

For an authorized derivative revision:

1. authenticate each upstream commit and Git tree in `manifest.nix`;
2. update the corresponding locked flake input and record its NAR hash;
3. retain only the manifest's staged paths and regenerate the package metadata
   from the exact OxCaml Opam repository descriptor;
4. increment the derivative suffix for every source, metadata, patch, or
   closure change;
5. reproduce the deterministic archive and license hashes;
6. verify the Parallel patch applies with no fuzz and that it is the only
   upstream source patch;
7. regenerate the closed catalog from the exact opam-nix scope, without
   copying generated metadata or source bytes into Git;
8. compare its sorted package, logical-source, declared-checksum,
   package-local payload, and content-addressed manifests;
9. build the package-only scope and isolated ordinary rank-1 repository
   consumer; and
10. run compiler, closure/inventory, pointer, mode, and lifecycle checks.

Do not add another derivative, staged path, source patch, Parallel library, or
runtime dependency without a newly accepted boundary.

The closed-catalog oracle is:

```text
selected package records              99
primary logical source rows           91
extra logical source rows              3
logical / physical source objects     94 / 92
duplicate source pairs                 2
package-local files/ owners            8
logical / physical payload objects   122 / 63
distinct payload paths/basenames      63 / 63
physical payload dedup savings        59
```

All 94 source provenance rows and all 122 package-local payload rows remain
materialized. Content deduplication happens only below those logical
owner/path inventories. The source cache indexes exact locked bytes by every
checksum algorithm declared by the selected descriptor; SHA-256 derived from
locked bytes is additional content identity only and never changes metadata.

The only descriptor adaptation is `ocaml-system.5.2.0`: its generated
configuration check accepts the configured compiler's `5.2.0+ox` identity.
The catalog builder normalizes that exact build action back to the selected
descriptor and requires byte identity, so any second metadata change fails.

Review an update atomically with:

```sh
nix build --no-link --print-out-paths \
  .#parallel-runtime-closed-repository
nix build --no-link --print-out-paths \
  .#checks.x86_64-linux.parallel-runtime-installed-inventory
nix build --no-link --print-out-paths \
  .#checks.x86_64-linux.parallel-runtime-ordinary-opam-consumer
```

The first output's `provenance/manifests.sha256` and `provenance/counts` are
the auditable catalog result. The ordinary consumer must retain one repository,
an empty isolated switch, a rejecting non-local fetch command, no
`OCAMLPATH`, no fake action, and no Nix library package input.
