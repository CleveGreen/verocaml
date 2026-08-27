{
  pkgs,
  system,
  opam-nix,
  opam-repository,
  oxcaml,
  oxcaml-opam-repository,
  sourceInputs,
  compilerOverlay,
  tests,
}:
let
  manifest = import ./manifest.nix;
  compilerSelection = import ./compiler.nix {
    inherit pkgs manifest;
    upstreamCompiler = oxcaml.packages.${system}.oxcaml;
  };
  stagedSources = import ./sources.nix {
    inherit pkgs manifest;
    sources = sourceInputs;
  };
  repository = import ./repository.nix {
    inherit
      pkgs
      opam-nix
      system
      stagedSources
      manifest
      ;
  };
  packageSelection = import ./packages.nix {
    inherit
      pkgs
      system
      opam-nix
      oxcaml-opam-repository
      opam-repository
      repository
      manifest
      compilerOverlay
      ;
    compiler = compilerSelection.compiler;
  };
  closedRepository = import ./closed-repository.nix {
    inherit
      pkgs
      repository
      manifest
      ;
    scope = packageSelection.scope;
  };
  inventory = import ./inventory.nix {
    inherit pkgs manifest;
    compiler = compilerSelection.compiler;
    oldCompiler = oxcaml.packages.${system}.oxcaml;
    scope = packageSelection.scope;
  };
  smoke = import ./smoke.nix {
    inherit
      pkgs
      stagedSources
      tests
      ;
    compiler = compilerSelection.compiler;
    scope = packageSelection.scope;
  };
  ordinaryConsumer = import ./consumer.nix {
    inherit
      pkgs
      closedRepository
      manifest
      tests
      ;
    compiler = compilerSelection.compiler;
  };
in
{
  inherit
    manifest
    stagedSources
    repository
    closedRepository
    ;
  compiler = compilerSelection.compiler;
  scope = packageSelection.scope;
  packages = packageSelection.packages;
  checks = {
    compiler = compilerSelection.check;
    installedInventory = inventory.installed;
    smoke = smoke.packageSmoke;
    pointerHarness = smoke.pointerHarness;
    inherit ordinaryConsumer;
  };
  inherit (inventory) mkClosureCheck;
}
