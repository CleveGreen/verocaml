#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import pathlib
import re
import subprocess
import sys


POLICY_ROOT = pathlib.Path(__file__).resolve().parent
DEFAULT_LIVE_POLICY = POLICY_ROOT / "records/live-policy.json"
DEFAULT_PARAMETRIC_RATCHET = POLICY_ROOT / "records/parametric-ratchet.jsonl"
SOURCE_ROOTS = ("ppx", "src", "test", "library", "runtime", "examples", "nix")
SOURCE_FILES = ("dune-project", "flake.lock", "flake.nix", "verocaml.opam")
HYGIENE_PATHS = (*SOURCE_ROOTS, *SOURCE_FILES)
COLLAPSED_CONTROL = re.compile(r" with \|| in match| then match| else match")


class PolicyError(Exception):
    def __init__(self, category, detail):
        super().__init__(detail)
        self.category = category
        self.detail = detail


def fail(category, detail):
    raise PolicyError(category, detail)


def canonical_json(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()


def sha256(contents):
    return hashlib.sha256(contents).hexdigest()


def load_json(path, category):
    try:
        return json.loads(path.read_text())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        fail(category, f"{path}: {error}")


def load_parametric_ratchet(path):
    try:
        contents = path.read_text()
        try:
            return json.loads(contents)
        except json.JSONDecodeError:
            lines = contents.splitlines()
            ratchet = json.loads(lines[0])
            modules = {}
            for line in lines[1:]:
                module = json.loads(line)
                functions = {
                    "\t".join((row[0], row[1], row[2], str(row[3]))): {
                        "span": row[4],
                        "cap": row[5],
                    }
                    for row in module.pop("functions")
                }
                modules[module.pop("path")] = {**module, "functions": functions}
            ratchet["modules"] = modules
            return ratchet
    except (OSError, UnicodeDecodeError, json.JSONDecodeError, IndexError, TypeError) as error:
        fail("live-parametric-ratchet-invalid", f"{path}: {error}")


def relative_entries(root, roots, files=()):
    entries = []
    for name in roots:
        candidate = root / name
        if not candidate.exists() and not candidate.is_symlink():
            continue
        paths = [candidate]
        if candidate.is_dir() and not candidate.is_symlink():
            paths.extend(candidate.rglob("*"))
        for path in paths:
            relative = path.relative_to(root).as_posix()
            if path.is_symlink():
                entries.append((relative, "l", os.readlink(path).encode()))
            elif path.is_file():
                entries.append((relative, "f", path.read_bytes()))
            elif path.is_dir():
                entries.append((relative, "d", b""))
    for name in files:
        path = root / name
        if path.is_symlink():
            entries.append((name, "l", os.readlink(path).encode()))
        elif path.is_file():
            entries.append((name, "f", path.read_bytes()))
    return sorted(entries)


def manifest_digest(entries):
    digest = hashlib.sha256()
    for relative, kind, contents in entries:
        digest.update(relative.encode())
        digest.update(b"\0")
        digest.update(kind.encode())
        digest.update(b"\0")
        digest.update(hashlib.sha256(contents).digest())
        digest.update(b"\n")
    return digest.hexdigest()


def install_projection(install_root):
    entries = relative_entries(install_root, (".",))
    normalized = []
    for relative, kind, contents in entries:
        if relative == ".":
            continue
        if relative.startswith("./"):
            relative = relative[2:]
        normalized.append((relative, kind, contents))
    normalized.sort()
    path_rows = sorted(f"{kind} {relative}" for relative, kind, _ in normalized)
    interfaces = []
    public = []
    for relative, kind, _ in normalized:
        path = install_root / relative
        if kind not in ("f", "l"):
            continue
        if path.suffix in (".cmi", ".mli"):
            interfaces.append(relative)
        if path.suffix == ".cmi" and ".private" not in pathlib.PurePosixPath(relative).parts:
            public.append(path.stem)
    public.sort()
    interface_blobs = [
        f"{relative}\t{sha256((install_root / relative).read_bytes())}"
        for relative in interfaces
    ]
    public_blobs = [
        f"{name}\t{sha256((install_root / relative).read_bytes())}"
        for relative, name in (
            (relative, pathlib.PurePosixPath(relative).stem)
            for relative, kind, _ in normalized
            if kind in ("f", "l")
            and relative.endswith(".cmi")
            and ".private" not in pathlib.PurePosixPath(relative).parts
        )
    ]
    return {
        "path_rows": path_rows,
        "interfaces": interfaces,
        "public": public,
        "path_digest": sha256(("\n".join(path_rows) + "\n").encode()),
        "interface_digest": sha256(("\n".join(interface_blobs) + "\n").encode()),
        "public_digest": sha256(("\n".join(public_blobs) + "\n").encode()),
    }


def source_digest(source_root):
    return manifest_digest(relative_entries(source_root, SOURCE_ROOTS, SOURCE_FILES))


def receipt_body(source_root, install_root):
    projection = install_projection(install_root)
    return {
        "format": "verocaml-live-install-receipt-v1",
        "source_manifest_sha256": source_digest(source_root),
        "install_path_count": len(projection["path_rows"]),
        "install_interface_count": len(projection["interfaces"]),
        "install_public_count": len(projection["public"]),
        "install_path_projection_sha256": projection["path_digest"],
        "install_interface_projection_sha256": projection["interface_digest"],
        "install_public_projection_sha256": projection["public_digest"],
    }


def write_receipt(source_root, install_root, output):
    body = receipt_body(source_root, install_root)
    receipt = dict(body)
    receipt["receipt_sha256"] = sha256(canonical_json(body))
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")


def validate_receipt(source_root, install_root, receipt_path):
    receipt = load_json(receipt_path, "live-install-provenance-mismatch")
    supplied_digest = receipt.pop("receipt_sha256", None)
    if supplied_digest != sha256(canonical_json(receipt)):
        fail("live-install-provenance-mismatch", "receipt integrity")
    expected = receipt_body(source_root, install_root)
    if receipt != expected:
        differing = sorted(
            key for key in set(receipt) | set(expected) if receipt.get(key) != expected.get(key)
        )
        fail("live-install-provenance-mismatch", f"fields={','.join(differing)}")
    return install_projection(install_root)


def line_count(path, category="live-owner-delegation-violation"):
    try:
        return len(path.read_text().splitlines())
    except (OSError, UnicodeDecodeError) as error:
        fail(category, f"{path}: {error}")


def function_inventory(inventory_executable, path):
    result = subprocess.run(
        [str(inventory_executable), str(path)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        fail("live-function-cap-exceeded", f"inventory {path}: {result.stderr.strip()}")
    occurrences = {}
    rows = []
    for line in result.stdout.splitlines():
        fields = line.split("\t")
        if len(fields) != 6:
            fail("live-function-cap-exceeded", f"malformed inventory row: {line}")
        kind, name, context, attributes, begin, end = fields
        identity = (kind, name, context)
        occurrence = occurrences.get(identity, 0)
        occurrences[identity] = occurrence + 1
        rows.append(
            {
                "kind": kind,
                "name": name,
                "context": context,
                "occurrence": occurrence,
                "attributes": int(attributes),
                "begin": int(begin),
                "end": int(end),
                "span": int(end) - int(begin) + 1,
            }
        )
    return rows


def expected_manifest(source_root, policy, key):
    record = policy["package_manifests"][key]
    path = source_root / record["path"]
    try:
        contents = path.read_bytes()
    except OSError as error:
        fail("live-package-projection-mismatch", f"{key}: {error}")
    if len(contents.splitlines()) != record["rows"] or sha256(contents) != record["sha256"]:
        fail("live-package-projection-mismatch", f"{key} manifest integrity")
    return contents.decode().splitlines()


def verify_package_projection(source_root, projection, policy, suite):
    expected_paths = expected_manifest(source_root, policy, "paths")
    expected_interfaces = expected_manifest(source_root, policy, "interfaces")
    expected_public = expected_manifest(source_root, policy, "public")
    if projection["path_rows"] != expected_paths:
        fail("live-package-projection-mismatch", "installed paths")
    if projection["interfaces"] != expected_interfaces:
        fail("live-package-projection-mismatch", "installed interfaces")
    if projection["public"] != expected_public:
        fail("live-public-projection-mismatch", "installed public modules")
    interface_set = set(expected_interfaces)
    public_set = set(expected_public)
    for module in suite.get("private_package_modules", []):
        if (
            f"lib/verocaml/core/.private/{module}.cmi" not in interface_set
            or f"lib/verocaml/core/{module}.mli" not in interface_set
            or module in public_set
        ):
            fail("live-public-projection-mismatch", f"private module {module}")


def verify_runtime(source_root, policy):
    actual = {}
    runtime_root = source_root / "runtime"
    if runtime_root.is_dir():
        for path in sorted(runtime_root.rglob("*")):
            if path.is_file() and not path.is_symlink():
                actual[path.relative_to(source_root).as_posix()] = sha256(path.read_bytes())
    if actual != policy["runtime_ratchet"]:
        fail("live-runtime-ratchet-mismatch", "runtime path/blob projection")


def verify_ownership_rules(source_root, rules):
    for rule in rules:
        path = source_root / rule["path"]
        try:
            text = path.read_text()
        except (OSError, UnicodeDecodeError) as error:
            fail("live-owner-delegation-violation", f"{rule['path']}: {error}")
        for required in rule.get("required", []):
            if "regex" in required:
                count = len(re.findall(required["regex"], text, re.MULTILINE))
                required_value = required["regex"]
            else:
                count = text.count(required["text"])
                required_value = required["text"]
            expected_count = required.get("count")
            minimum_count = required.get("minimum", 1)
            if (
                expected_count is not None
                and count != expected_count
                or expected_count is None
                and count < minimum_count
            ):
                fail(
                    "live-owner-delegation-violation",
                    f"{rule['path']} required={required_value!r} count={count}",
                )
        for forbidden in rule.get("forbidden", []):
            if forbidden in text:
                fail(
                    "live-owner-delegation-violation",
                    f"{rule['path']} forbidden={forbidden!r}",
                )
    for rule in rules:
        if "unique_regex" not in rule:
            continue
        matches = []
        expression = re.compile(rule["unique_regex"], re.MULTILINE)
        for path in sorted((source_root / rule.get("root", "src")).glob("*.ml")):
            if expression.search(path.read_text()):
                matches.append(path.relative_to(source_root).as_posix())
        if matches != rule["expected_paths"]:
            fail("live-owner-delegation-violation", f"unique owner paths={matches}")


def verify_live(source_root, install_root, receipt_path, inventory_executable, suite_name, policy_path):
    policy = load_json(policy_path, "live-policy-record-invalid")
    suites = policy.get("suites", {})
    if suite_name not in suites:
        fail("live-policy-record-invalid", f"unknown suite {suite_name}")
    suite = suites[suite_name]
    projection = validate_receipt(source_root, install_root, receipt_path)
    verify_package_projection(source_root, projection, policy, suite)
    if suite.get("runtime_ratchet", False):
        verify_runtime(source_root, policy)

    module_sizes = {}
    interface_sizes = {}
    function_sizes = {}
    for relative in suite["owners"]:
        source = source_root / relative
        interface = source_root / f"{relative}i"
        module_sizes[relative] = line_count(source)
        interface_sizes[f"{relative}i"] = line_count(interface)
        rows = function_inventory(inventory_executable, source)
        function_sizes[relative] = max((row["span"] for row in rows), default=0)
    if max(module_sizes.values(), default=0) > suite["module_cap"]:
        fail("live-module-cap-exceeded", str(module_sizes))
    if max(interface_sizes.values(), default=0) > suite["interface_cap"]:
        fail("live-interface-cap-exceeded", str(interface_sizes))
    if max(function_sizes.values(), default=0) > suite["function_cap"]:
        fail("live-function-cap-exceeded", str(function_sizes))

    concentration = sum(line_count(source_root / path) for path in suite["concentration_paths"])
    if concentration > suite["concentration_cap"]:
        fail(
            "live-concentration-cap-exceeded",
            f"{concentration}>{suite['concentration_cap']}",
        )
    owner_source = "\n".join((source_root / path).read_text() for path in suite["owners"])
    for token in suite.get("forbidden_tokens", []):
        if token in owner_source:
            fail("live-forbidden-authority", token)
    verify_ownership_rules(source_root, suite.get("ownership_rules", []))
    print(
        f"{suite['label']} architecture "
        f"owners={len(module_sizes)} "
        f"module={max(module_sizes.values())}/{suite['module_cap']} "
        f"interface={max(interface_sizes.values())}/{suite['interface_cap']} "
        f"function={max(function_sizes.values())}/{suite['function_cap']} "
        f"concentration={concentration}/{suite['concentration_cap']} "
        f"installed={len(projection['path_rows'])}/{len(projection['interfaces'])}/{len(projection['public'])} "
        "receipt=matched"
    )


def product_ml_paths(source_root):
    paths = []
    for root_name in ("ppx", "src", "test", "library", "runtime", "examples"):
        root = source_root / root_name
        if root.is_dir():
            paths.extend(
                path.relative_to(source_root).as_posix()
                for path in root.rglob("*.ml")
                if path.is_file() and not path.name.startswith(".#")
            )
    return sorted(paths)


def function_key(row):
    return f"{row['kind']}\t{row['name']}\t{row['context']}\t{row['occurrence']}"


def verify_parametric(
    source_root,
    install_root,
    receipt_path,
    inventory_executable,
    policy_path,
    ratchet_path,
):
    policy = load_json(policy_path, "live-policy-record-invalid")
    ratchet = load_parametric_ratchet(ratchet_path)
    projection = validate_receipt(source_root, install_root, receipt_path)
    verify_package_projection(source_root, projection, policy, {})
    verify_runtime(source_root, policy)
    verify_ownership_rules(source_root, ratchet["ownership_rules"])

    current_paths = product_ml_paths(source_root)
    recorded_modules = ratchet["modules"]
    largest_module = (0, "none")
    largest_function = (0, "none")
    current_function_count = 0
    for relative in current_paths:
        path = source_root / relative
        lines = line_count(path, "live-parametric-module-cap-exceeded")
        largest_module = max(largest_module, (lines, relative))
        record = recorded_modules.get(relative)
        cap = ratchet["new_module_cap"] if record is None else record["line_cap"]
        if lines > cap:
            fail("live-parametric-module-cap-exceeded", f"{relative} {lines}>{cap}")
        collapsed = len(COLLAPSED_CONTROL.findall(path.read_text()))
        collapsed_cap = 0 if record is None else record["collapsed_control_cap"]
        if collapsed > collapsed_cap:
            fail(
                "live-parametric-collapsed-control-exceeded",
                f"{relative} {collapsed}>{collapsed_cap}",
            )
        recorded_functions = {} if record is None else record["functions"]
        current_functions = function_inventory(inventory_executable, path)
        current_function_count += len(current_functions)
        for row in current_functions:
            key = function_key(row)
            recorded_function = recorded_functions.get(key)
            if recorded_function is None:
                function_cap = ratchet["new_function_cap"]
            elif isinstance(recorded_function, dict):
                function_cap = recorded_function["cap"]
            else:
                function_cap = recorded_function
            largest_function = max(largest_function, (row["span"], f"{relative}:{key}"))
            if row["span"] > function_cap:
                fail(
                    "live-parametric-function-cap-exceeded",
                    f"{relative} {key} {row['span']}>{function_cap}",
                )

    concentration = sum(
        line_count(source_root / path, "live-parametric-concentration-exceeded")
        for path in ratchet["concentration_paths"]
        if (source_root / path).is_file()
    )
    if concentration > ratchet["concentration_cap"]:
        fail(
            "live-parametric-concentration-exceeded",
            f"{concentration}>{ratchet['concentration_cap']}",
        )
    recorded_function_count = sum(
        len(record["functions"]) for record in recorded_modules.values()
    )
    print(
        "parametric architecture "
        f"current-modules={len(current_paths)} recorded-modules={len(recorded_modules)} "
        f"current-functions={current_function_count} recorded-functions={recorded_function_count} "
        f"new-modules=checked module-max={largest_module[0]} "
        f"function-max={largest_function[0]} caps={ratchet['new_module_cap']}/{ratchet['new_function_cap']} "
        f"concentration={concentration}/{ratchet['concentration_cap']} "
        f"installed={len(projection['path_rows'])}/{len(projection['interfaces'])}/{len(projection['public'])} "
        "collapsed=ratcheted ownership=delegated receipt=matched"
    )


def git(repository, *arguments, check=True):
    result = subprocess.run(
        ["git", "-C", str(repository), *arguments],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if check and result.returncode != 0:
        fail("git-repository-missing", result.stderr.strip() or str(repository))
    return result


def require_repository(repository, *, checkout):
    result = git(repository, "rev-parse", "--git-dir", check=False)
    if result.returncode != 0:
        fail("git-repository-missing", str(repository))
    if not checkout:
        top = git(repository, "rev-parse", "--show-toplevel", check=False)
        if top.returncode == 0:
            return pathlib.Path(top.stdout.strip()).resolve()
        return repository.resolve()
    top = git(repository, "rev-parse", "--show-toplevel", check=False)
    if top.returncode != 0:
        fail("git-repository-missing", str(repository))
    return pathlib.Path(top.stdout.strip()).resolve()


def require_object(repository, revision):
    result = git(repository, "cat-file", "-e", f"{revision}^{{commit}}", check=False)
    if result.returncode != 0:
        fail("git-object-missing", revision)


def replay(repository, record_path, base, target, manifest_path, rows, digest):
    record = load_json(record_path, "replay-record-invalid")
    for field in ("base", "target"):
        if not re.fullmatch(r"[0-9a-f]{40}", record.get(field, "")):
            fail(f"replay-{field}-mismatch", str(record.get(field)))
    supplied_base = base or record["base"]
    supplied_target = target or record["target"]
    if supplied_base != record["base"]:
        fail("replay-base-mismatch", supplied_base)
    if supplied_target != record["target"]:
        fail("replay-target-mismatch", supplied_target)
    supplied_manifest = manifest_path or (record_path.parent / record["manifest"])
    supplied_rows = record["rows"] if rows is None else rows
    supplied_digest = record["sha256"] if digest is None else digest
    try:
        contents = supplied_manifest.read_bytes()
    except OSError as error:
        fail("replay-manifest-missing", str(error))
    if contents and not contents.endswith(b"\n"):
        fail("replay-manifest-digest-mismatch", "missing final newline")
    manifest_rows = contents.decode().splitlines()
    if len(manifest_rows) != supplied_rows:
        fail("replay-manifest-row-count-mismatch", f"{len(manifest_rows)}!={supplied_rows}")
    if sha256(contents) != supplied_digest:
        fail("replay-manifest-digest-mismatch", sha256(contents))
    if manifest_rows != sorted(manifest_rows) or len(manifest_rows) != len(set(manifest_rows)):
        fail("replay-manifest-order-mismatch", "manifest must be sorted and unique")
    repository = require_repository(repository, checkout=False)
    require_object(repository, supplied_base)
    require_object(repository, supplied_target)
    arguments = [
        "git",
        "-C",
        str(repository),
        "-c",
        "core.quotepath=false",
        "diff-tree",
        "--no-commit-id",
        "--name-only",
        "-r",
        "-z",
        "--no-renames",
        supplied_base,
        supplied_target,
    ]
    scope = record["scope"]
    if scope:
        arguments.extend(["--", *scope])
    result = subprocess.run(arguments, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        fail("git-object-missing", result.stderr.decode(errors="replace").strip())
    actual = sorted(
        path.decode("utf-8") for path in result.stdout.split(b"\0") if path
    )
    missing = sorted(set(actual) - set(manifest_rows))
    extra = sorted(set(manifest_rows) - set(actual))
    if missing:
        fail("replay-path-missing", ",".join(missing))
    if extra:
        fail("replay-path-extra", ",".join(extra))
    print(
        f"{record['id']} object replay accepted "
        f"base={supplied_base[:12]} target={supplied_target[:12]} "
        f"paths={len(actual)} manifest={supplied_digest[:12]} scope={record['scope_label']}"
    )


def hygiene(repository, target):
    repository = require_repository(repository, checkout=True)
    require_object(repository, target)
    head = git(repository, "rev-parse", "HEAD").stdout.strip()
    if head != target:
        fail("hygiene-target-mismatch", f"HEAD={head} target={target}")
    tracked = git(repository, "diff", "--name-only", "--", *HYGIENE_PATHS).stdout.splitlines()
    if tracked:
        fail("hygiene-tracked-product-dirt", ",".join(sorted(tracked)))
    staged = git(
        repository, "diff", "--cached", "--name-only", "--", *HYGIENE_PATHS
    ).stdout.splitlines()
    if staged:
        fail("hygiene-staged-product-dirt", ",".join(sorted(staged)))
    untracked = git(
        repository,
        "ls-files",
        "--others",
        "--exclude-standard",
        "--",
        *HYGIENE_PATHS,
    ).stdout.splitlines()
    if untracked:
        fail("hygiene-untracked-product-dirt", ",".join(sorted(untracked)))
    print(f"exact-checkout hygiene accepted target={target[:12]} product-roots=11")


def parse_arguments():
    parser = argparse.ArgumentParser()
    commands = parser.add_subparsers(dest="command", required=True)

    receipt_parser = commands.add_parser("issue-install-receipt")
    receipt_parser.add_argument("--source", type=pathlib.Path, required=True)
    receipt_parser.add_argument("--context", required=True)
    receipt_parser.add_argument("--output", type=pathlib.Path, required=True)

    live_parser = commands.add_parser("live")
    live_parser.add_argument("--source", type=pathlib.Path, required=True)
    live_parser.add_argument("--install", type=pathlib.Path, required=True)
    live_parser.add_argument("--receipt", type=pathlib.Path, required=True)
    live_parser.add_argument("--inventory", type=pathlib.Path, required=True)
    live_parser.add_argument("--suite", required=True)
    live_parser.add_argument("--policy", type=pathlib.Path, default=DEFAULT_LIVE_POLICY)

    parametric_parser = commands.add_parser("parametric-live")
    parametric_parser.add_argument("--source", type=pathlib.Path, required=True)
    parametric_parser.add_argument("--install", type=pathlib.Path, required=True)
    parametric_parser.add_argument("--receipt", type=pathlib.Path, required=True)
    parametric_parser.add_argument("--inventory", type=pathlib.Path, required=True)
    parametric_parser.add_argument("--policy", type=pathlib.Path, default=DEFAULT_LIVE_POLICY)
    parametric_parser.add_argument(
        "--ratchet", type=pathlib.Path, default=DEFAULT_PARAMETRIC_RATCHET
    )

    replay_parser = commands.add_parser("replay")
    replay_parser.add_argument("--repository", type=pathlib.Path, required=True)
    replay_parser.add_argument("--record", type=pathlib.Path, required=True)
    replay_parser.add_argument("--base")
    replay_parser.add_argument("--target")
    replay_parser.add_argument("--manifest", type=pathlib.Path)
    replay_parser.add_argument("--rows", type=int)
    replay_parser.add_argument("--sha256")

    hygiene_parser = commands.add_parser("hygiene")
    hygiene_parser.add_argument("--repository", type=pathlib.Path, required=True)
    hygiene_parser.add_argument("--target", required=True)
    return parser.parse_args()


def main():
    arguments = parse_arguments()
    if arguments.command == "issue-install-receipt":
        source = arguments.source.resolve()
        install = source / "_build/install" / arguments.context
        write_receipt(source, install, arguments.output)
    elif arguments.command == "live":
        verify_live(
            arguments.source.resolve(),
            arguments.install.resolve(),
            arguments.receipt,
            arguments.inventory.resolve(),
            arguments.suite,
            arguments.policy.resolve(),
        )
    elif arguments.command == "parametric-live":
        verify_parametric(
            arguments.source.resolve(),
            arguments.install.resolve(),
            arguments.receipt,
            arguments.inventory.resolve(),
            arguments.policy.resolve(),
            arguments.ratchet.resolve(),
        )
    elif arguments.command == "replay":
        replay(
            arguments.repository.resolve(),
            arguments.record.resolve(),
            arguments.base,
            arguments.target,
            arguments.manifest.resolve() if arguments.manifest else None,
            arguments.rows,
            arguments.sha256,
        )
    elif arguments.command == "hygiene":
        hygiene(arguments.repository.resolve(), arguments.target)


if __name__ == "__main__":
    try:
        main()
    except PolicyError as error:
        print(f"{error.category}: {error.detail}", file=sys.stderr)
        raise SystemExit(1)
