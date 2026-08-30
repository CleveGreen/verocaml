The selected OxCaml runtime and PPX permit structured events and nested spans
directly inside a portable function.

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_COLOR=never ./oxcaml_backend.exe >trace.out 2>&1
  $ test -s trace.out && echo 'portable-flat-tracing=enabled'
  portable-flat-tracing=enabled

The packaged Delator dependency includes its optional Yojson renderer. Its
schema and presentation remain Delator's compatibility boundary rather than a
VeroCaml snapshot.

  $ DELATOR_LOG=trace DELATOR_FORMAT=json DELATOR_COLOR=never ./oxcaml_backend.exe >json.out 2>&1
  $ test -s json.out && echo 'portable-json-tracing=enabled'
  portable-json-tracing=enabled
