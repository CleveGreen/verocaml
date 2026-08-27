#!/usr/bin/env python3
import contextlib
import hashlib
import io
import json
import pathlib
import shutil
import sys
import tempfile

import live_wrapper
import policy


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


class Fixture:
    def __init__(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="verocaml-live-policy-")
        self.base = pathlib.Path(self.temporary.name)
        self.source = self.base / "source"
        self.install = self.base / "install"
        self.record = self.base / "live-policy.json"
        self.ratchet = self.base / "parametric-ratchet.json"
        self.receipt = self.base / "receipt.json"
        self.inventory = self.base / "inventory.py"
        self._materialize()

    def close(self):
        self.temporary.cleanup()

    def _write(self, relative, contents):
        path = self.source / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)
        return path

    def _materialize(self):
        self._write("src/owner.ml", "let owner x = x (* OWNER_TOKEN *)\n")
        self._write("src/owner.mli", "val owner : 'a -> 'a\n")
        self._write("src/concentration.ml", "let concentration = 1\n")
        self._write("runtime/dune", "(library (name ghost))\n")
        self._write("runtime/ghost.ml", "let ghost = ()\n")
        self._write("runtime/ghost.mli", "val ghost : unit\n")
        for relative, contents in {
            "lib/verocaml/core/.private/owner.cmi": b"private-cmi",
            "lib/verocaml/core/owner.mli": b"val owner : unit\n",
            "lib/verocaml/core/public.cmi": b"public-cmi",
        }.items():
            path = self.install / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(contents)

        projection = policy.install_projection(self.install)
        manifests = self.source / "manifests"
        manifests.mkdir()
        (manifests / "paths").write_text("\n".join(projection["path_rows"]) + "\n")
        (manifests / "interfaces").write_text(
            "\n".join(projection["interfaces"]) + "\n"
        )
        (manifests / "public").write_text("\n".join(projection["public"]) + "\n")

        runtime = {
            relative: digest(self.source / relative)
            for relative in ("runtime/dune", "runtime/ghost.ml", "runtime/ghost.mli")
        }
        package_manifests = {}
        for key, filename in {
            "paths": "paths",
            "interfaces": "interfaces",
            "public": "public",
        }.items():
            path = manifests / filename
            package_manifests[key] = {
                "path": f"manifests/{filename}",
                "rows": len(path.read_bytes().splitlines()),
                "sha256": digest(path),
            }
        record = {
            "format": "test-live-policy-v1",
            "package_manifests": package_manifests,
            "runtime_ratchet": runtime,
            "suites": {
                "test": {
                    "label": "test",
                    "owners": ["src/owner.ml"],
                    "module_cap": 10,
                    "interface_cap": 10,
                    "function_cap": 10,
                    "concentration_paths": ["src/concentration.ml"],
                    "concentration_cap": 2,
                    "private_package_modules": ["owner"],
                    "runtime_ratchet": True,
                    "forbidden_tokens": ["FORBIDDEN_AUTHORITY"],
                    "ownership_rules": [
                        {
                            "path": "src/owner.ml",
                            "required": [{"text": "OWNER_TOKEN", "count": 1}],
                        }
                    ],
                }
            },
        }
        self.record.write_text(json.dumps(record, indent=2, sort_keys=True) + "\n")
        self.inventory.write_text(
            "#!/usr/bin/env python3\n"
            "import pathlib, sys\n"
            "text = pathlib.Path(sys.argv[1]).read_text()\n"
            "end = 20 if 'LONG_FUNCTION' in text else 2\n"
            "print(f'binding\\tmain\\t<module>\\t0\\t1\\t{end}')\n"
        )
        self.inventory.chmod(0o755)
        self.write_ratchet()
        self.refresh_receipt()

    def write_ratchet(self):
        key = "binding\tmain\t<module>\t0"
        ratchet = {
            "format": "test-parametric-ratchet-v1",
            "new_module_cap": 10,
            "new_function_cap": 10,
            "concentration_paths": ["src/concentration.ml"],
            "concentration_cap": 1,
            "ownership_rules": [],
            "modules": {
                "src/owner.ml": {
                    "lines": 1,
                    "line_cap": 10,
                    "collapsed_control": 0,
                    "collapsed_control_cap": 0,
                    "functions": {key: {"span": 2, "cap": 2}},
                },
                "src/concentration.ml": {
                    "lines": 1,
                    "line_cap": 10,
                    "collapsed_control": 0,
                    "collapsed_control_cap": 0,
                    "functions": {key: {"span": 2, "cap": 2}},
                },
            },
        }
        self.ratchet.write_text(json.dumps(ratchet, indent=2, sort_keys=True) + "\n")

    def refresh_receipt(self):
        policy.write_receipt(self.source, self.install, self.receipt)

    def accept_install_paths_and_interfaces(self):
        projection = policy.install_projection(self.install)
        paths = self.source / "manifests/paths"
        interfaces = self.source / "manifests/interfaces"
        paths.write_text("\n".join(projection["path_rows"]) + "\n")
        interfaces.write_text("\n".join(projection["interfaces"]) + "\n")
        record = json.loads(self.record.read_text())
        for key, path in (("paths", paths), ("interfaces", interfaces)):
            record["package_manifests"][key]["rows"] = len(path.read_bytes().splitlines())
            record["package_manifests"][key]["sha256"] = digest(path)
        self.record.write_text(json.dumps(record, indent=2, sort_keys=True) + "\n")

    def live(self):
        with contextlib.redirect_stdout(io.StringIO()):
            policy.verify_live(
                self.source,
                self.install,
                self.receipt,
                self.inventory,
                "test",
                self.record,
            )

    def parametric(self):
        with contextlib.redirect_stdout(io.StringIO()):
            policy.verify_parametric(
                self.source,
                self.install,
                self.receipt,
                self.inventory,
                self.record,
                self.ratchet,
            )


def expect_error(expected, operation):
    try:
        operation()
    except policy.PolicyError as error:
        if error.category != expected:
            raise AssertionError(f"expected {expected}, got {error.category}: {error.detail}")
        return
    raise AssertionError(f"expected {expected}")


def expect_wrapper_error(expected, operation):
    stderr = io.StringIO()
    try:
        with contextlib.redirect_stderr(stderr):
            operation()
    except SystemExit as error:
        if error.code != 1 or not stderr.getvalue().startswith(f"{expected}:"):
            raise AssertionError(
                f"expected {expected}, exit={error.code}, stderr={stderr.getvalue()!r}"
            )
        return
    raise AssertionError(f"expected {expected}")


def with_fixture(operation):
    fixture = Fixture()
    try:
        operation(fixture)
    finally:
        fixture.close()


def live_controls():
    with_fixture(lambda fixture: fixture.live())

    def owner(fixture):
        (fixture.source / "src/owner.ml").write_text("let owner x = x\n")
        fixture.refresh_receipt()
        expect_error("live-owner-delegation-violation", fixture.live)

    def module(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text(path.read_text() + "\n".join("(* growth *)" for _ in range(11)))
        fixture.refresh_receipt()
        expect_error("live-module-cap-exceeded", fixture.live)

    def function(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text("let owner x = x (* OWNER_TOKEN LONG_FUNCTION *)\n")
        fixture.refresh_receipt()
        expect_error("live-function-cap-exceeded", fixture.live)

    def package(fixture):
        path = fixture.install / "lib/verocaml/core/alternate.cmi"
        path.write_bytes(b"alternate")
        fixture.refresh_receipt()
        expect_error("live-package-projection-mismatch", fixture.live)

    def runtime(fixture):
        path = fixture.source / "runtime/ghost.mli"
        path.write_text(path.read_text() + "val alternate : unit\n")
        fixture.refresh_receipt()
        expect_error("live-runtime-ratchet-mismatch", fixture.live)

    def public(fixture):
        path = fixture.install / "lib/verocaml/core/alternate.cmi"
        path.write_bytes(b"alternate")
        fixture.accept_install_paths_and_interfaces()
        fixture.refresh_receipt()
        expect_error("live-public-projection-mismatch", fixture.live)

    def provenance(fixture):
        path = fixture.install / "lib/verocaml/core/public.cmi"
        path.write_bytes(b"different-public-cmi")
        expect_error("live-install-provenance-mismatch", fixture.live)

    def forbidden(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text(path.read_text() + "(* FORBIDDEN_AUTHORITY *)\n")
        fixture.refresh_receipt()
        expect_error("live-forbidden-authority", fixture.live)

    def concentration(fixture):
        path = fixture.source / "src/concentration.ml"
        path.write_text("let concentration = 1\nlet growth = 2\nlet more = 3\n")
        fixture.refresh_receipt()
        expect_error("live-concentration-cap-exceeded", fixture.live)

    for control in (
        owner,
        module,
        function,
        package,
        public,
        runtime,
        provenance,
        forbidden,
        concentration,
    ):
        with_fixture(control)


def parametric_controls():
    with_fixture(lambda fixture: fixture.parametric())

    def module(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text("\n".join("let x = 0" for _ in range(11)) + "\n")
        fixture.refresh_receipt()
        expect_error("live-parametric-module-cap-exceeded", fixture.parametric)

    def function(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text("let owner x = x (* LONG_FUNCTION *)\n")
        fixture.refresh_receipt()
        expect_error("live-parametric-function-cap-exceeded", fixture.parametric)

    def collapsed(fixture):
        path = fixture.source / "src/owner.ml"
        path.write_text(path.read_text() + "(* in match *)\n")
        fixture.refresh_receipt()
        expect_error("live-parametric-collapsed-control-exceeded", fixture.parametric)

    def concentration(fixture):
        path = fixture.source / "src/concentration.ml"
        path.write_text("let concentration = 1\nlet growth = 2\n")
        fixture.refresh_receipt()
        expect_error("live-parametric-concentration-exceeded", fixture.parametric)

    for control in (module, function, collapsed, concentration):
        with_fixture(control)


def cross_pair_controls():
    first = Fixture()
    second = Fixture()
    try:
        (second.source / "src/concentration.ml").write_text("let concentration = 2\n")
        (second.install / "lib/verocaml/core/public.cmi").write_bytes(b"second-install")
        second.refresh_receipt()
        expect_wrapper_error(
            "live-install-provenance-mismatch",
            lambda: live_wrapper.run(
                "test",
                [
                    "architecture_check.py",
                    str(first.source),
                    str(second.install),
                    str(second.receipt),
                    str(first.inventory),
                ],
                policy_path=first.record,
            ),
        )
        expect_wrapper_error(
            "live-install-provenance-mismatch",
            lambda: live_wrapper.run(
                "test",
                [
                    "architecture_check.py",
                    str(second.source),
                    str(first.install),
                    str(first.receipt),
                    str(second.inventory),
                ],
                policy_path=second.record,
            ),
        )
    finally:
        first.close()
        second.close()


def committed_ownership_control(source_root):
    ratchet = policy.load_parametric_ratchet(policy.DEFAULT_PARAMETRIC_RATCHET)
    with tempfile.TemporaryDirectory(prefix="verocaml-ownership-policy-") as temporary:
        copied_source = pathlib.Path(temporary) / "source"
        shutil.copytree(source_root / "src", copied_source / "src")
        translation = copied_source / "src/vir_logic_ir_translation_private.ml"
        translation.write_text(
            translation.read_text()
            + "\nlet duplicate_logic_sort = Parametric_logic_private.logic_sort\n"
        )
        expect_error(
            "live-owner-delegation-violation",
            lambda: policy.verify_ownership_rules(
                copied_source, ratchet["ownership_rules"]
            ),
        )


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: live_policy_tests.py SOURCE")
    source_root = pathlib.Path(sys.argv[1]).resolve()
    live_controls()
    parametric_controls()
    cross_pair_controls()
    committed_ownership_control(source_root)
    print("live-policy controls positive=2 negatives=14 gitless=passed")
    print("install-provenance wrappers matched=2 cross-pair=2")
    print("preserved-properties owner/span/package/runtime/forbidden/concentration=passed")


if __name__ == "__main__":
    main()
