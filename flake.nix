{
  description = "VeroCaml hermetic OxCaml prototype";

  nixConfig.allow-import-from-derivation = true;

  inputs = {
    oxcaml.url = "github:oxcaml/oxcaml/076f16f8677a4589b088a65215337574f6ed732e";

    nixpkgs.follows = "oxcaml/nixpkgs";

    opam-nix = {
      url = "github:tweag/opam-nix/583fb2ed4db44fcda4f6222c554949503d50a352";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    opam-repository.follows = "opam-nix/opam-repository";

    verocaml-opam-repository = {
      url = "github:ocaml/opam-repository/4817f2453c85025e66642c4d61e953167e350bd6";
      flake = false;
    };

    oxcaml-opam-repository = {
      url = "github:oxcaml/opam-repository/ce75a27e9a742f1159d42adf2ba9850d49206e6f";
      flake = false;
    };

    await-source = {
      url = "github:janestreet/await/19c663468dd0dcda0198bb5281f597b363bd16b6";
      flake = false;
    };

    concurrent-source = {
      url = "github:janestreet/concurrent/12a47ed4e77e04eafb6e21e29e0731446f3abdc4";
      flake = false;
    };

    delator-source = {
      url = "github:CleveGreen/delator/c6d9de5092eabbcb5711f4d5b7b1231f509294e8";
      flake = false;
    };

    parallel-source = {
      url = "github:janestreet/parallel/e488373bb887e8cce4dab95bb23ec5d2f41d4f17";
      flake = false;
    };
  };

  outputs =
    {
      self,
      await-source,
      concurrent-source,
      delator-source,
      nixpkgs,
      opam-nix,
      opam-repository,
      oxcaml,
      oxcaml-opam-repository,
      parallel-source,
      verocaml-opam-repository,
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      parallelRuntime = import ./nix/parallel-runtime {
        inherit
          pkgs
          system
          opam-nix
          opam-repository
          oxcaml
          oxcaml-opam-repository
          ;
        sourceInputs = {
          await = await-source;
          concurrent = concurrent-source;
          parallel = parallel-source;
        };
        compilerOverlay = ./nix/oxcaml-opam-nix.nix;
        tests = ./test/parallel_runtime;
      };
      oxcamlCompiler = parallelRuntime.compiler;
      z3ContextLocalRepository = import ./nix/z3-context-local-portable/repository.nix {
        inherit
          pkgs
          system
          opam-nix
          ;
      };
      delatorRepository = opam-nix.lib.${system}.makeOpamRepo delator-source;

      scope =
        opam-nix.lib.${system}.buildOpamProject
          {
            pkgs = pkgs;
            repos = [
              delatorRepository
              parallelRuntime.repository.opamNix
              z3ContextLocalRepository.opamNix
              oxcaml-opam-repository
              verocaml-opam-repository
            ];
            resolveArgs.env = {
              sys-ocaml-version = "5.2.0";
            };
            overlays = [
              opam-nix.overlays.ocaml-overlay
              (import ./nix/oxcaml-opam-nix.nix {
                inherit oxcamlCompiler;
              })
            ];
          }
          "verocaml"
          self
          {
            ocaml-system = "5.2.0";
            base = "v0.18~preview.130.83+317";
            delator = "dev";
            ocaml_intrinsics = "v0.18~preview.130.83+317";
            ppx_deriving = "6.1.1+ox";
            ppx_enumerate = "v0.18~preview.130.83+317";
            ppxlib = "0.33.0+ox";
            ppxlib_ast = "0.33.0+ox";
            ppxlib_jane = "v0.18~preview.130.83+317";
            parallel = "v0.18~preview.130.83+317+verocaml.1";
            smtml = "0.25.0";
            yojson = "2.2.2";
            zarith = "1.14";
            z3 = "4.15.2+verocaml.1";
          };

      verocaml = scope.verocaml.overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
        DELATOR_STATIC_LEVEL = "info";
        postFixup = (old.postFixup or "") + ''
          wrapProgram "$out/bin/verocaml" \
            --set VEROCAML_OCAMLC "${oxcamlCompiler}/bin/ocamlc" \
            --set VEROCAML_DUNE "${scope.dune}/bin/dune" \
            --set VEROCAML_PPX "$out/bin/verocaml-ppx" \
            --set VEROCAML_GHOST_DIR \
              "$out/lib/ocaml/5.2.0/site-lib/verocaml/ghost" \
            --prefix PATH : "${oxcamlCompiler}/bin:${scope.dune}/bin" \
            --prefix OCAMLPATH : \
              "$out/lib/ocaml/5.2.0/site-lib:${scope.delator}/lib/ocaml/5.2.0/site-lib:${scope.ppxlib}/lib/ocaml/5.2.0/site-lib"
        '';
      });

      z3ContextLocalPortability = import ./nix/z3-context-local-portable/smoke.nix {
        inherit pkgs;
        compiler = oxcamlCompiler;
        ocamlfind = scope.ocamlfind;
        parallel = parallelRuntime.packages.parallel;
        tests = ./test/z3_context_local_portability;
        z3 = scope.z3;
      };

      parallelRuntimeClosureCheck = parallelRuntime.mkClosureCheck verocaml;

      versionAssertions =
        assert oxcamlCompiler.version == "5.2.0+ox";
        assert scope.ocaml-system.version == "5.2.0";
        assert scope.base.version == "v0.18_preview.130.83+317";
        assert scope.delator.version == "dev";
        assert scope.ocaml_intrinsics.version == "v0.18_preview.130.83+317";
        assert scope.ppx_deriving.version == "6.1.1+ox";
        assert scope.ppx_enumerate.version == "v0.18_preview.130.83+317";
        assert scope.ppxlib.version == "0.33.0+ox";
        assert scope.ppxlib_ast.version == "0.33.0+ox";
        assert scope.ppxlib_jane.version == "v0.18_preview.130.83+317";
        assert scope.parallel.version == "v0.18_preview.130.83+317+verocaml.1";
        assert scope.smtml.version == "0.25.0";
        assert scope.yojson.version == "2.2.2";
        assert scope.zarith.version == "1.14";
        assert scope.z3.version == "4.15.2+verocaml.1";
        true;

      toolchainCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-toolchain-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"

            test "$(ocamlc -vnum)" = "5.2.0+ox"
            test "$(ocamlc -config-var architecture)" = "amd64"

            cp ${./test/toolchain/unique_mode.ml} unique_mode.ml
            ocamlc -bin-annot -o unique_mode unique_mode.ml
            test -f unique_mode.cmt
            test "$(./unique_mode)" = "63"

            test ! -e "$TMPDIR/opam-root"
            export OPAMROOT="$TMPDIR/opam-root"
            dune build --root ${self} --build-dir "$TMPDIR/dune-build" @install
            test ! -e "$OPAMROOT"

            mkdir -p "$out"
            printf '%s\n' \
              "ocaml=$(ocamlc -vnum)" \
              "base=${scope.base.version}" \
              "ocaml_intrinsics=${scope.ocaml_intrinsics.version}" \
              "ppx_deriving=${scope.ppx_deriving.version}" \
              "ppx_enumerate=${scope.ppx_enumerate.version}" \
              "ppxlib=${scope.ppxlib.version}" \
              "ppxlib_ast=${scope.ppxlib_ast.version}" \
              "ppxlib_jane=${scope.ppxlib_jane.version}" \
              "parallel=${scope.parallel.version}" \
              "smtml=${scope.smtml.version}" \
              "yojson=${scope.yojson.version}" \
              "zarith=${scope.zarith.version}" \
              "z3=${scope.z3.version}" \
              > "$out/versions"
          '';

      cmtInputCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-cmt-input-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/cmt_input/cmt-input-check
            touch "$out"
          '';

      specificationFrontendCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-specification-frontend-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/specification_frontend/specification-frontend-check
            touch "$out"
          '';

      pureSstCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-pure-sst-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/pure_sst/pure-sst-check
            touch "$out"
          '';

      checkedIntegerVirCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-checked-integer-vir-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/checked_integer_vir/checked-integer-vir-check
            touch "$out"
          '';

      logicIrCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-logic-ir-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/logic_ir/logic-ir-check
            touch "$out"
          '';

      solverBackendCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-solver-backend-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/solver_backend/solver-backend-check
            touch "$out"
          '';

      contractsAndCallsCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-contracts-and-calls-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/contracts_and_calls/contracts-and-calls-check
            touch "$out"
          '';

      directTotalityCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-direct-totality-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            VEROCAML_TEST_INSTALL_ROOT="$TMPDIR/dune-build/install/default" \
              dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @install \
              @test/direct_totality/direct-totality-check
            touch "$out"
          '';

      recursiveSpecificationsCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-recursive-specifications-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/recursive_specifications/recursive-specifications-check
            test ! -e \
              "$TMPDIR/dune-build/install/default/lib/verocaml/core/recursive_spec_encoding.cmi"
            touch "$out"
          '';

      recursiveAggregatesCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-recursive-aggregates-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/recursive_aggregates/recursive-aggregates-check
            touch "$out"
          '';

      ownedRecursiveContentsCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-owned-recursive-contents-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/owned_recursive_contents/owned-recursive-contents-check
            touch "$out"
          '';

      recursiveRankCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-recursive-rank-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/recursive_rank/recursive-rank-check
            touch "$out"
          '';

      aggregateSpecificationsCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-aggregate-specifications-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/aggregate_specifications/aggregate-specifications-check
            touch "$out"
          '';

      verifiedInterfacesCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-verified-interfaces-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            VEROCAML_TEST_INSTALL_ROOT="$TMPDIR/dune-build/install/default" \
              dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @install \
              @test/verified_interfaces/verified-interfaces-check
            touch "$out"
          '';

      sharedInvariantCellCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-shared-invariant-cell-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            VEROCAML_TEST_INSTALL_ROOT="$TMPDIR/dune-build/install/default" \
              dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @install \
              @test/shared_invariant_cell/shared-invariant-cell-check
            touch "$out"
          '';

      sharedRecursiveFrozenSpineCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-shared-recursive-frozen-spine-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            VEROCAML_TEST_INSTALL_ROOT="$TMPDIR/dune-build/install/default" \
              dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @install \
              @test/shared_recursive_frozen_spine/shared-recursive-frozen-spine-check
            touch "$out"
          '';

      finiteFormalPropagationCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-finite-formal-propagation-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/finite_formal_propagation/finite-formal-propagation-check
            touch "$out"
          '';

      genericCloneDependenciesCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-generic-clone-dependencies-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/generic_clone_dependencies/generic-clone-dependency-check
            touch "$out"
          '';

      finiteResultPromotionCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-finite-result-promotion-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/finite_result_promotion/finite-result-promotion-check
            touch "$out"
          '';

      privateReceiptCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-private-receipt-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
              pkgs.findutils
              pkgs.python3
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            VEROCAML_TEST_INSTALL_ROOT="$TMPDIR/dune-build/install/default" \
              dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @install \
              @test/private_receipt/private-receipt-check
            touch "$out"
          '';

      uniqueMutationCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-unique-mutation-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/unique_mutation/unique-mutation-check
            touch "$out"
          '';

      sourceInputCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-source-input-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/source_input/source-input-check

            cp ${self}/test/source_input/fixtures/verified.ml direct.ml
            ${pkgs.coreutils}/bin/env -i \
              HOME="$TMPDIR" \
              OCAML_COLOR=never \
              PATH=/nonexistent \
              TMPDIR="$TMPDIR" \
              VEROCAML_GHOST_DIR=/untrusted \
              VEROCAML_OCAMLC=/untrusted \
              VEROCAML_PPX=/untrusted \
              ${verocaml}/bin/verocaml verify direct.ml \
              > direct.out
            grep -F "functions=1 obligations=1" direct.out
            test ! -e direct.cmi
            test ! -e direct.cmo
            test ! -e direct.cmt

            ${oxcamlCompiler}/bin/ocamlc \
              -w -A -alert -all -bin-annot \
              -I ${verocaml}/lib/ocaml/5.2.0/site-lib/verocaml/ghost \
              -ppx "${verocaml}/bin/verocaml-ppx --keep-ghost" \
              -c -o retained.cmo direct.ml
            ${pkgs.coreutils}/bin/env -i \
              HOME="$TMPDIR" \
              OCAML_COLOR=never \
              PATH=/nonexistent \
              TMPDIR="$TMPDIR" \
              ${verocaml}/bin/verocaml verify retained.cmt \
              > retained.out
            grep -F "functions=1 obligations=1" retained.out
            touch "$out"
          '';

      duneProjectCliCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-dune-project-cli-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              scope.dune
              verocaml
            ];
            buildInputs = [
              scope.delator
              scope.ppxlib
            ];
          }
          ''
            set -eu
            export HOME="$TMPDIR/home"
            export XDG_CACHE_HOME="$TMPDIR/cache"
            mkdir -p "$HOME" "$XDG_CACHE_HOME"
            test "$(ocamlc -vnum)" = "5.2.0+ox"

            cp -R ${./test/dune_project_cli/project} project
            chmod -R u+w project
            OCAML_COLOR=never DELATOR_COLOR=never \
              ${verocaml}/bin/verocaml verify project/lib \
                --threads 1 --timeout-ms 20000 \
                > verified.out 2> verified.err
            grep -F 'result=verified' verified.out
            grep -F 'verocaml: project directory=' verified.out
            grep -F 'roots=2 result=verified' verified.out
            test ! -s verified.err

            OCAML_COLOR=never DELATOR_COLOR=never DELATOR_FORMAT=flat \
              DELATOR_LOG='Verocaml_bin_dune_private=trace,Verocaml_bin=info,warn' \
              ${verocaml}/bin/verocaml verify project/lib \
                --threads 2 --timeout-ms 20000 \
                > traced.out 2> traced.err
            grep -F 'result=verified' traced.out
            test -s traced.err

            OCAML_COLOR=never DELATOR_COLOR=never DELATOR_FORMAT=json \
              DELATOR_LOG='Verocaml_bin=info,warn' \
              ${verocaml}/bin/verocaml verify project/lib \
                --threads 2 --timeout-ms 20000 \
                > json.out 2> json.err
            grep -F 'roots=2 result=verified' json.out
            test -s json.err

            cp -R ${./test/dune_project_cli/project} unmarked-project
            chmod -R u+w unmarked-project
            sed -i '/\[@@@verocaml.verify\]/d' unmarked-project/lib/*.ml
            if ${verocaml}/bin/verocaml verify unmarked-project/lib \
                 > unmarked.out 2> unmarked.err; then
              echo 'unmarked Dune project unexpectedly verified' >&2
              exit 1
            fi
            grep -F 'found no [@@@verocaml.verify] modules' unmarked.err

            if ${verocaml}/bin/verocaml verify project/lib \
                 --dependency missing.cmt > option.out 2> option.err; then
              echo 'directory verification accepted --dependency' >&2
              exit 1
            fi
            grep -F 'discovers dependencies automatically' option.err

            mkdir not-a-project
            if ${verocaml}/bin/verocaml verify not-a-project \
                 > no-project.out 2> no-project.err; then
              echo 'directory without dune-project unexpectedly verified' >&2
              exit 1
            fi
            grep -F 'could not find dune-project' no-project.err

            cp -R ${./test/dune_project_cli/project} broken-project
            chmod -R u+w broken-project
            printf '\nlet broken =\n' >> broken-project/lib/second_root.ml
            if ${verocaml}/bin/verocaml verify broken-project/lib \
                 > broken.out 2> broken.err; then
              echo 'Dune compilation failure unexpectedly verified' >&2
              exit 1
            fi
            grep -F 'dune build' broken.err
            grep -F 'second_root.ml' broken.err
            touch "$out"
          '';

      trustedExternalBodiesCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-trusted-external-bodies-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/trusted_external_bodies/trusted-external-bodies-check
            touch "$out"
          '';

      releaseVerificationCheck =
        assert versionAssertions;
        pkgs.runCommand "verocaml-release-verification-check"
          {
            nativeBuildInputs = [
              oxcamlCompiler
              pkgs.stdenv.cc
              scope.dune
            ];
            buildInputs = [
              scope.smtml
              scope.zarith
              scope.parallel
              scope.z3
            ];
          }
          ''
            set -eu
            export XDG_CACHE_HOME="$TMPDIR/cache"
            test "$(ocamlc -vnum)" = "5.2.0+ox"
            dune build \
              --root ${self} \
              --build-dir "$TMPDIR/dune-build" \
              @test/usability_p0_project/usability-p0-project-check \
              @test/release_verification/release-verification-check
            cp \
              ${self}/test/release_verification/fixtures/pure_verified.ml \
              pure_verified.ml
            ${pkgs.coreutils}/bin/env -i \
              HOME="$TMPDIR" \
              OCAML_COLOR=never \
              PATH=/nonexistent \
              TMPDIR="$TMPDIR" \
              ${verocaml}/bin/verocaml verify pure_verified.ml \
              > source-verification.out
            grep -F \
              "functions=2 obligations=12" \
              source-verification.out
            test ! -e pure_verified.cmi
            test ! -e pure_verified.cmo
            test ! -e pure_verified.cmt
            touch "$out"
          '';
    in
    {
      packages.${system} = {
        default = verocaml;
        inherit
          verocaml
          ;
        oxcaml-multidomain = oxcamlCompiler;
        parallel-runtime = parallelRuntime.packages.parallel;
        parallel-runtime-await = parallelRuntime.packages.await;
        parallel-runtime-concurrent = parallelRuntime.packages.concurrent;
        parallel-runtime-closed-repository = parallelRuntime.closedRepository;
        parallel-runtime-repository = parallelRuntime.repository.ordinary;
        z3-context-local-portable = scope.z3;
      };

      apps.${system}.default = {
        type = "app";
        program = "${verocaml}/bin/verocaml";
      };

      devShells.${system}.default = pkgs.mkShell {
        DELATOR_STATIC_LEVEL = "trace";
        packages = [
          oxcamlCompiler
          scope.dune
          scope.delator
          scope.smtml
          scope.parallel
          scope.z3
        ];

        shellHook = ''
          if [ "$(ocamlc -vnum)" != "5.2.0+ox" ]; then
            echo "VeroCaml requires the pinned OxCaml 5.2.0+ox compiler" >&2
            return 1 2>/dev/null || exit 1
          fi
        '';
      };

      formatter.${system} = pkgs.nixfmt-tree;

      checks.${system} = {
        inherit
          aggregateSpecificationsCheck
          checkedIntegerVirCheck
          contractsAndCallsCheck
          directTotalityCheck
          duneProjectCliCheck
          finiteFormalPropagationCheck
          finiteResultPromotionCheck
          genericCloneDependenciesCheck
          cmtInputCheck
          logicIrCheck
          ownedRecursiveContentsCheck
          pureSstCheck
          privateReceiptCheck
          releaseVerificationCheck
          recursiveAggregatesCheck
          recursiveRankCheck
          recursiveSpecificationsCheck
          solverBackendCheck
          sourceInputCheck
          sharedInvariantCellCheck
          sharedRecursiveFrozenSpineCheck
          specificationFrontendCheck
          toolchainCheck
          trustedExternalBodiesCheck
          uniqueMutationCheck
          verifiedInterfacesCheck
          ;
        parallel-runtime-closure = parallelRuntimeClosureCheck;
        parallel-runtime-compiler = parallelRuntime.checks.compiler;
        parallel-runtime-installed-inventory = parallelRuntime.checks.installedInventory;
        parallel-runtime-ordinary-opam-consumer = parallelRuntime.checks.ordinaryConsumer;
        parallel-runtime-pointer-harness = parallelRuntime.checks.pointerHarness;
        parallel-runtime-smoke = parallelRuntime.checks.smoke;
        z3-context-local-portability = z3ContextLocalPortability.positive;
        z3-context-local-portability-negative-context = z3ContextLocalPortability.negativeContext;
        z3-context-local-portability-negative-global = z3ContextLocalPortability.negativeGlobal;
        package = verocaml;
      };
    };
}
