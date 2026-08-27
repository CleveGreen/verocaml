{
  pkgs,
  system,
  opam-nix,
  oxcaml-opam-repository,
  opam-repository,
  repository,
  compiler,
  manifest,
  compilerOverlay,
}:
let
  scope =
    opam-nix.lib.${system}.queryToScope
      {
        inherit pkgs;
        repos = [
          repository.opamNix
          oxcaml-opam-repository
          opam-repository
        ];
        resolveArgs.env = {
          sys-ocaml-version = "5.2.0";
        };
        overlays = [
          opam-nix.overlays.ocaml-overlay
          (import compilerOverlay {
            oxcamlCompiler = compiler;
          })
        ];
      }
      {
        ocaml-system = "5.2.0";
        base = manifest.upstreamVersion;
        ppxlib = "0.33.0+ox";
        ppxlib_ast = "0.33.0+ox";
        ppxlib_jane = manifest.upstreamVersion;
        parallel = manifest.derivativeVersion;
      };
in
assert scope.ocaml-system.version == "5.2.0";
assert scope.await.version == builtins.replaceStrings [ "~" ] [ "_" ] manifest.derivativeVersion;
assert
  scope.concurrent.version == builtins.replaceStrings [ "~" ] [ "_" ] manifest.derivativeVersion;
assert scope.parallel.version == builtins.replaceStrings [ "~" ] [ "_" ] manifest.derivativeVersion;
{
  inherit scope;
  packages = {
    inherit (scope)
      await
      concurrent
      parallel
      ;
  };
}
