{
  pkgs,
  scope,
  repository,
  manifest,
}:
let
  lib = pkgs.lib;
  q = lib.escapeShellArg;
  selectedPackages = lib.sort (
    left: right:
    "${left.OPAM_PACKAGE_NAME}.${left.OPAM_PACKAGE_VERSION}"
    < "${right.OPAM_PACKAGE_NAME}.${right.OPAM_PACKAGE_VERSION}"
  ) (lib.attrValues (lib.filterAttrs (_: value: lib.isDerivation value && value ? pkgdef) scope));
  checksumList = checksum: if builtins.isList checksum then checksum else [ checksum ];
  parseChecksum =
    checksum:
    let
      matched = builtins.match "([a-z0-9]+)=(.+)" checksum;
      algorithm = builtins.elemAt matched 0;
    in
    assert matched != null;
    assert lib.elem algorithm [
      "md5"
      "sha256"
      "sha512"
    ];
    {
      inherit algorithm;
      digest = builtins.elemAt matched 1;
      original = checksum;
    };
  checksumEntries = checksum: map parseChecksum (checksumList checksum);
  sha256From =
    checksum:
    let
      selected = lib.findFirst (entry: entry.algorithm == "sha256") null (checksumEntries checksum);
    in
    if selected == null then null else selected.digest;
  derivativeNames = [
    "await"
    "concurrent"
    "parallel"
  ];
  derivativeArchive =
    package:
    "${repository.ordinary}/archives/${package.OPAM_PACKAGE_NAME}-${package.OPAM_PACKAGE_VERSION}.tar.gz";
  externalPrimarySources = lib.filter (entry: entry != null) (
    map (
      package:
      if package.pkgdef ? url && !(lib.elem package.OPAM_PACKAGE_NAME derivativeNames) then
        let
          section = package.pkgdef.url.section;
          name = package.OPAM_PACKAGE_NAME;
        in
        {
          inherit package;
          kind = "primary";
          logicalName = "${name}-${package.OPAM_PACKAGE_VERSION}";
          upstream = section.src;
          declaredChecksums = checksumEntries section.checksum;
          storePath = package.src;
        }
      else
        null
    ) selectedPackages
  );
  derivativeSources = map (
    name:
    let
      package = scope.${name};
      packageManifest = manifest.packages.${name};
    in
    {
      inherit package;
      kind = "primary";
      logicalName = "${name}-${package.OPAM_PACKAGE_VERSION}";
      upstream = "https://github.com/janestreet/${name}/archive/${packageManifest.commit}.tar.gz";
      declaredChecksums = [
        {
          algorithm = "sha256";
          digest = packageManifest.archiveSha256;
          original = "sha256=${packageManifest.archiveSha256}";
        }
      ];
      storePath = derivativeArchive package;
    }
  ) derivativeNames;
  primarySources = externalPrimarySources ++ derivativeSources;
  extraSources = lib.concatMap (
    package:
    if package.pkgdef ? "extra-source" then
      lib.mapAttrsToList (
        logicalName: section:
        let
          declaredChecksums = checksumEntries section.checksum;
          sha256 = sha256From section.checksum;
        in
        assert sha256 != null;
        {
          inherit
            package
            logicalName
            declaredChecksums
            ;
          kind = "extra";
          upstream = section.src;
          storePath = pkgs.fetchurl {
            url = section.src;
            inherit sha256;
            name = "${package.OPAM_PACKAGE_NAME}-${logicalName}";
          };
        }
      ) package.pkgdef."extra-source".section
    else
      [ ]
  ) selectedPackages;
  sourceObjects = primarySources ++ extraSources;
  filesOwners = lib.filter (package: package.pkgdef ? files) selectedPackages;
  filesPayloads = lib.concatMap (
    package:
    map (
      extraFile:
      let
        relativePath = builtins.elemAt extraFile 0;
        sha256 = sha256From (builtins.elemAt extraFile 1);
      in
      assert sha256 != null;
      {
        inherit
          package
          relativePath
          sha256
          ;
        sourcePath = "${package.pkgdef.files}/${relativePath}";
      }
    ) (package.pkgdef."extra-files" or [ ])
  ) filesOwners;
  packageCommands = lib.concatMapStringsSep "\n" (
    package:
    let
      name = package.OPAM_PACKAGE_NAME;
      version = package.OPAM_PACKAGE_VERSION;
      descriptor = if name == "ocaml-system" then ./ocaml-system.5.2.0.opam else package.pkgdef.opamFile;
      descriptorKind = if name == "ocaml-system" then "adapted-system-bootstrap" else "selected-exact";
      repositoryPath = package.pkgdef.repo or "generated-verocaml-derivative";
      derivativeUrlCommand = lib.optionalString (lib.elem name derivativeNames) ''
        install -m 0644 \
          ${q "${repository.ordinary}/packages/${name}/${name}.${version}/url"} \
          "$package_dir/url"
      '';
      filesOwnerCommand = lib.optionalString (package.pkgdef ? files) ''
        mkdir -p "$package_dir/files"
        printf '%s\n' ${q "${name}.${version}"} \
          >> "$out/provenance/files-payload-owners"
      '';
      unchangedCommand = lib.optionalString (name != "ocaml-system") ''
        cmp ${q "${package.pkgdef.opamFile}"} "$package_dir/opam"
      '';
    in
    ''
      package_dir="$out/packages/${name}/${name}.${version}"
      mkdir -p "$package_dir"
      install -m 0644 ${q "${descriptor}"} "$package_dir/opam"
      ${derivativeUrlCommand}
      ${filesOwnerCommand}
      ${unchangedCommand}
      descriptor_sha256="$(sha256sum "$package_dir/opam" | cut -d ' ' -f 1)"
      printf '%s\t%s\t%s\t%s\t%s\n' \
        ${q name} \
        ${q version} \
        ${q descriptorKind} \
        ${q repositoryPath} \
        "$descriptor_sha256" \
        >> "$out/provenance/package-records.tsv"
    ''
  ) selectedPackages;
  sourceCommands = lib.concatMapStringsSep "\n" (
    entry:
    let
      package = entry.package;
      declaredChecksumCommands = lib.concatMapStringsSep "\n" (
        checksum:
        let
          checksumProgram = "${checksum.algorithm}sum";
          checksumPrefix = builtins.substring 0 2 checksum.digest;
        in
        ''
          declared_actual="$(${checksumProgram} "$source_path" | cut -d ' ' -f 1)"
          test "$declared_actual" = ${q checksum.digest}
          cache_path="$out/cache/${checksum.algorithm}/${checksumPrefix}/${checksum.digest}"
          mkdir -p "$(dirname "$cache_path")"
          if test -e "$cache_path"; then
            cmp "$source_path" "$cache_path"
          else
            ln "$content_path" "$cache_path"
          fi
          printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            ${q package.OPAM_PACKAGE_NAME} \
            ${q package.OPAM_PACKAGE_VERSION} \
            ${q entry.kind} \
            ${q entry.logicalName} \
            ${q checksum.algorithm} \
            ${q checksum.digest} \
            "$actual_sha256" \
            >> "$out/provenance/source-declared-checksums.tsv"
        ''
      ) entry.declaredChecksums;
    in
    assert builtins.length entry.declaredChecksums > 0;
    ''
      source_path=${q "${entry.storePath}"}
      test -f "$source_path"
      actual_sha256="$(sha256sum "$source_path" | cut -d ' ' -f 1)"
      content_path="$out/source-content/sha256/''${actual_sha256:0:2}/$actual_sha256"
      mkdir -p "$(dirname "$content_path")"
      if test -e "$content_path"; then
        cmp "$source_path" "$content_path"
      else
        install -m 0644 "$source_path" "$content_path"
      fi
      ${declaredChecksumCommands}
      printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        ${q package.OPAM_PACKAGE_NAME} \
        ${q package.OPAM_PACKAGE_VERSION} \
        ${q entry.kind} \
        ${q entry.logicalName} \
        ${q entry.upstream} \
        "$actual_sha256" \
        ${q "${entry.storePath}"} \
        >> "$out/provenance/source-objects.tsv"
    ''
  ) sourceObjects;
  payloadCommands = lib.concatMapStringsSep "\n" (
    entry:
    let
      package = entry.package;
      name = package.OPAM_PACKAGE_NAME;
      version = package.OPAM_PACKAGE_VERSION;
      basename = builtins.baseNameOf entry.relativePath;
      contentPrefix = builtins.substring 0 2 entry.sha256;
    in
    ''
      payload_source=${q entry.sourcePath}
      test -f "$payload_source"
      payload_sha256="$(sha256sum "$payload_source" | cut -d ' ' -f 1)"
      test "$payload_sha256" = ${q entry.sha256}
      payload_bytes="$(stat -c %s "$payload_source")"
      content_path="$out/payload-content/sha256/${contentPrefix}/${entry.sha256}"
      mkdir -p "$(dirname "$content_path")"
      if test -e "$content_path"; then
        cmp "$payload_source" "$content_path"
      else
        install -m 0644 "$payload_source" "$content_path"
      fi
      package_path="$out/packages/${name}/${name}.${version}/files/${entry.relativePath}"
      mkdir -p "$(dirname "$package_path")"
      ln "$content_path" "$package_path"
      cmp "$payload_source" "$package_path"
      printf '%s\t%s\t%s\t%s\n' \
        ${q name} \
        ${q version} \
        ${q entry.relativePath} \
        "$payload_sha256" \
        >> "$out/provenance/files-payloads.tsv"
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
        ${q "${name}.${version}"} \
        oxcaml \
        ${q entry.relativePath} \
        ${q basename} \
        "$payload_sha256" \
        "$payload_bytes" \
        >> "$out/provenance/files-payload-audit.body"
    ''
  ) filesPayloads;
in
assert builtins.length selectedPackages == 99;
assert builtins.length primarySources == 91;
assert builtins.length extraSources == 3;
assert builtins.length sourceObjects == 94;
assert builtins.length filesOwners == 8;
assert builtins.length filesPayloads == 122;
pkgs.runCommand "verocaml-parallel-runtime-closed-opam-repository"
  {
    nativeBuildInputs = [
      pkgs.diffutils
      pkgs.findutils
    ];
    passthru = {
      inherit
        selectedPackages
        sourceObjects
        filesOwners
        filesPayloads
        ;
    };
  }
  ''
    set -euo pipefail
    mkdir -p \
      "$out"/{cache,packages,payload-content/sha256,provenance,source-content/sha256}
    : > "$out/provenance/package-records.tsv"
    : > "$out/provenance/source-objects.tsv"
    : > "$out/provenance/source-declared-checksums.tsv"
    : > "$out/provenance/files-payload-owners"
    : > "$out/provenance/files-payloads.tsv"
    : > "$out/provenance/files-payload-audit.body"

    cat > "$out/repo" <<EOF
    opam-version: "2.0"
    archive-mirrors: "file://$out/cache"
    EOF

    ${packageCommands}
    ${sourceCommands}
    ${payloadCommands}

    for manifest_file in \
      package-records.tsv \
      source-objects.tsv \
      source-declared-checksums.tsv \
      files-payload-owners \
      files-payloads.tsv \
      files-payload-audit.body; do
      LC_ALL=C sort -o "$out/provenance/$manifest_file" \
        "$out/provenance/$manifest_file"
    done

    {
      printf 'record\trepository\tpath\tbasename\tsha256\tbytes\n'
      cat "$out/provenance/files-payload-audit.body"
    } > "$out/provenance/files-payload-audit.tsv"
    rm "$out/provenance/files-payload-audit.body"

    awk -F '\t' '
      NR > 1 {
        count[$5]++
        row = $1 ":" $3
        if ($5 in rows) rows[$5] = rows[$5] " | " row
        else rows[$5] = row
      }
      END {
        for (hash in count)
          if (count[hash] > 1)
            print hash "\t" count[hash] "\t" rows[hash]
      }
    ' "$out/provenance/files-payload-audit.tsv" \
      | LC_ALL=C sort > "$out/provenance/duplicate-payload-groups.body"
    {
      printf 'sha256\trow_count\trows\n'
      cat "$out/provenance/duplicate-payload-groups.body"
    } > "$out/provenance/duplicate-payload-groups.tsv"
    rm "$out/provenance/duplicate-payload-groups.body"

    awk -F '\t' '
      {
        count[$6]++
        row = $1 "." $2 ":" $3 ":" $4
        if ($6 in rows) rows[$6] = rows[$6] " | " row
        else rows[$6] = row
      }
      END {
        for (hash in count)
          if (count[hash] > 1)
            print hash "\t" count[hash] "\t" rows[hash]
      }
    ' "$out/provenance/source-objects.tsv" \
      | LC_ALL=C sort > "$out/provenance/duplicate-source-groups.tsv"

    find "$out/source-content/sha256" -type f -printf '%P\n' \
      | LC_ALL=C sort > "$out/provenance/source-content-sha256"
    find "$out/payload-content/sha256" -type f -printf '%P\n' \
      | LC_ALL=C sort > "$out/provenance/payload-content-sha256"
    find "$out/cache" -type f -printf '%P\n' \
      | LC_ALL=C sort > "$out/provenance/cache-aliases.tsv"

    test "$(wc -l < "$out/provenance/package-records.tsv")" -eq 99
    test "$(
      awk -F '\t' '$3 == "primary" { count++ } END { print count + 0 }' \
        "$out/provenance/source-objects.tsv"
    )" -eq 91
    test "$(
      awk -F '\t' '$3 == "extra" { count++ } END { print count + 0 }' \
        "$out/provenance/source-objects.tsv"
    )" -eq 3
    test "$(wc -l < "$out/provenance/source-objects.tsv")" -eq 94
    test "$(wc -l < "$out/provenance/source-content-sha256")" -eq 92
    test "$(wc -l < "$out/provenance/duplicate-source-groups.tsv")" -eq 2
    grep -Fx \
      '54afd640f863c2f39a6c844f6ceff63c8796e865da7db6d49c60019516fc00d4' \
      <(cut -f1 "$out/provenance/duplicate-source-groups.tsv")
    grep -Fx \
      'c2ccf8bc6b17afa47c450297357496303aa7c8680e329b79d98c68e35013a118' \
      <(cut -f1 "$out/provenance/duplicate-source-groups.tsv")

    test "$(wc -l < "$out/provenance/files-payload-owners")" -eq 8
    test "$(wc -l < "$out/provenance/files-payloads.tsv")" -eq 122
    test "$(
      cut -f3 "$out/provenance/files-payloads.tsv" \
        | LC_ALL=C sort -u \
        | wc -l
    )" -eq 63
    test "$(
      cut -f3 "$out/provenance/files-payloads.tsv" \
        | while IFS= read -r path; do basename "$path"; done \
        | LC_ALL=C sort -u \
        | wc -l
    )" -eq 63
    test "$(
      cut -f4 "$out/provenance/files-payloads.tsv" \
        | LC_ALL=C sort -u \
        | wc -l
    )" -eq 63
    test "$(wc -l < "$out/provenance/payload-content-sha256")" -eq 63
    test "$(
      find "$out/packages" -path '*/files/*' -type f | wc -l
    )" -eq 122
    test "$(
      find "$out/packages" -path '*/files/*' -type l | wc -l
    )" -eq 0
    test "$(
      tail -n +2 "$out/provenance/duplicate-payload-groups.tsv" | wc -l
    )" -eq 59
    test "$(
      awk -F '\t' 'NR > 1 { savings += $2 - 1 } END { print savings + 0 }' \
        "$out/provenance/duplicate-payload-groups.tsv"
    )" -eq 59

    test "$(
      sha256sum "$out/provenance/package-records.tsv" | cut -d ' ' -f 1
    )" = 3716a6df60760d18a87c3b8566b034b2866bdafce6b7f274ed52713fc6f8768f
    test "$(
      sha256sum "$out/provenance/source-objects.tsv" | cut -d ' ' -f 1
    )" = 1e798d5f60c5f83ef89ad3c97f31586fde3e2feb31ce3acfd2d5ff79245514fb
    test "$(
      sha256sum "$out/provenance/source-content-sha256" | cut -d ' ' -f 1
    )" = ced3956a7e9cb22aff6f932473fe70fcf02ae40cc239c7cd590dd8ef4f25e1fb
    test "$(
      sha256sum "$out/provenance/files-payload-owners" | cut -d ' ' -f 1
    )" = 23e81ca6c8c49352ea1a09ca2682ff7ba27120eab2b500279730d76fd44f8f6c
    test "$(
      sha256sum "$out/provenance/files-payloads.tsv" | cut -d ' ' -f 1
    )" = c940e299337d3b88b329a23dcefb3062c9fe6341826c4d5e75d2516ceed9760e
    test "$(
      sha256sum "$out/provenance/files-payload-audit.tsv" | cut -d ' ' -f 1
    )" = 21c9916d1da06710441271c48fadafc57fcb0ba1ad7869c9058edb4cccf46a8d
    test "$(
      sha256sum "$out/provenance/duplicate-payload-groups.tsv" \
        | cut -d ' ' -f 1
    )" = 0dd823820c427e98cc862992a8c36b69b2059232f2435a3b06a4cd3b5ca312a6

    install -m 0644 \
      ${q "${scope.ocaml-system.pkgdef.opamFile}"} \
      "$out/provenance/ocaml-system.5.2.0.original.opam"
    adapted="$out/packages/ocaml-system/ocaml-system.5.2.0/opam"
    original="$out/provenance/ocaml-system.5.2.0.original.opam"
    test "$(grep -Fxc 'build: ["ocaml" "gen_ocaml_config.ml"]' "$original")" -eq 1
    test "$(grep -Fxc 'build: [' "$adapted")" -eq 1
    test "$(
      grep -Fxc \
        '    "s/if Sys.ocaml_version <> \"5.2.0\" then/if Sys.ocaml_version <> \"5.2.0+ox\" then/"' \
        "$adapted"
    )" -eq 1
    sed \
      '/^build: \["ocaml" "gen_ocaml_config.ml"\]$/c\
    build: [\
      [\
        "sed"\
        "-i"\
        "s/if Sys.ocaml_version <> \\"5.2.0\\" then/if Sys.ocaml_version <> \\"5.2.0+ox\\" then/"\
        "gen_ocaml_config.ml"\
      ]\
      ["ocaml" "gen_ocaml_config.ml"]\
    ]' \
      "$original" > "$out/provenance/ocaml-system.5.2.0.expected.opam"
    cmp "$out/provenance/ocaml-system.5.2.0.expected.opam" "$adapted"
    if diff -u "$original" "$adapted" \
      > "$out/provenance/ocaml-system.5.2.0.diff"; then
      echo "ocaml-system descriptor was not adapted" >&2
      exit 1
    else
      test "$?" -eq 1
    fi

    find "$out/packages" -mindepth 2 -maxdepth 2 -type d -printf '%P\n' \
      | LC_ALL=C sort > "$out/provenance/package-inventory"
    test "$(wc -l < "$out/provenance/package-inventory")" -eq 99
    grep -Fx \
      'await/await.${manifest.derivativeVersion}' \
      "$out/provenance/package-inventory"
    grep -Fx \
      'concurrent/concurrent.${manifest.derivativeVersion}' \
      "$out/provenance/package-inventory"
    grep -Fx \
      'parallel/parallel.${manifest.derivativeVersion}' \
      "$out/provenance/package-inventory"

    {
      printf 'selected-package-records=99\n'
      printf 'primary-logical-source-rows=91\n'
      printf 'extra-logical-source-rows=3\n'
      printf 'logical-source-rows=94\n'
      printf 'physical-source-byte-objects=92\n'
      printf 'duplicate-source-pairs=2\n'
      printf 'files-payload-owners=8\n'
      printf 'logical-files-payload-rows=122\n'
      printf 'distinct-payload-basenames=63\n'
      printf 'distinct-payload-relative-paths=63\n'
      printf 'physical-payload-byte-objects=63\n'
      printf 'payload-physical-dedup-savings=59\n'
      printf 'system-bootstrap-adaptations=1\n'
      printf 'jane-runtime-derivatives=3\n'
    } > "$out/provenance/counts"

    (
      cd "$out/provenance"
      sha256sum \
        cache-aliases.tsv \
        counts \
        duplicate-payload-groups.tsv \
        duplicate-source-groups.tsv \
        files-payload-audit.tsv \
        files-payload-owners \
        files-payloads.tsv \
        ocaml-system.5.2.0.diff \
        package-inventory \
        package-records.tsv \
        payload-content-sha256 \
        source-content-sha256 \
        source-declared-checksums.tsv \
        source-objects.tsv \
        > manifests.sha256
    )
  ''
