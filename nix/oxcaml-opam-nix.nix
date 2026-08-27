{ oxcamlCompiler }:
final: prev: {
  ocaml-system = prev.ocaml-system.overrideAttrs (old: {
    nativeBuildInputs = [ oxcamlCompiler ];
    postPatch = (old.postPatch or "") + ''
      substituteInPlace gen_ocaml_config.ml \
        --replace-fail \
          'if Sys.ocaml_version <> "5.2.0" then' \
          'if Sys.ocaml_version <> "5.2.0+ox" then'
    '';
    passthru = (old.passthru or { }) // {
      compiler = oxcamlCompiler;
    };
  });
}
