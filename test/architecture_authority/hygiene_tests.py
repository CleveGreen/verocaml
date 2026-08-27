#!/usr/bin/env python3
import pathlib
import subprocess
import sys
import tempfile


POLICY = pathlib.Path(__file__).resolve().parent / "policy.py"
PRODUCT_ROOTS = ("src", "ppx", "runtime", "library", "test", "examples", "nix")
PRODUCT_FILES = ("dune-project", "flake.lock", "flake.nix", "verocaml.opam")


def git(repository, *arguments):
    return subprocess.run(
        ["git", "-C", str(repository), *arguments],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()


def initialize(repository):
    git(repository, "init", "-q")
    for relative in ("src/tracked.ml", "ppx/staged.ml"):
        path = repository / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("let fixture = ()\n")
    git(repository, "add", "src/tracked.ml", "ppx/staged.ml")
    git(
        repository,
        "-c",
        "user.name=Architecture Hygiene",
        "-c",
        "user.email=hygiene@example.invalid",
        "-c",
        "commit.gpgSign=false",
        "commit",
        "-q",
        "-m",
        "fixture",
    )
    return git(repository, "rev-parse", "HEAD")


def run(repository, target):
    return subprocess.run(
        [
            sys.executable,
            str(POLICY),
            "hygiene",
            "--repository",
            str(repository),
            "--target",
            target,
        ],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def expect_success(result):
    if result.returncode != 0:
        raise AssertionError(result.stderr)


def expect_failure(category, result):
    if result.returncode == 0 or not result.stderr.startswith(f"{category}:"):
        raise AssertionError(
            f"expected {category}, exit={result.returncode}, stderr={result.stderr!r}"
        )


def with_repository(operation):
    with tempfile.TemporaryDirectory(prefix="verocaml-hygiene-") as temporary:
        repository = pathlib.Path(temporary)
        target = initialize(repository)
        operation(repository, target)


def clean_control(repository, target):
    expect_success(run(repository, target))


def non_product_control(repository, target):
    for relative in ("notes/local.txt", "docs/local.txt", "TODO.local"):
        path = repository / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("unrelated\n")
    expect_success(run(repository, target))


def target_control(repository, target):
    path = repository / "README"
    path.write_text("second commit\n")
    git(repository, "add", "README")
    git(
        repository,
        "-c",
        "user.name=Architecture Hygiene",
        "-c",
        "user.email=hygiene@example.invalid",
        "-c",
        "commit.gpgSign=false",
        "commit",
        "-q",
        "-m",
        "second",
    )
    expect_failure("hygiene-target-mismatch", run(repository, target))


def tracked_control(repository, target):
    (repository / "src/tracked.ml").write_text("let fixture = 1\n")
    expect_failure("hygiene-tracked-product-dirt", run(repository, target))


def staged_control(repository, target):
    (repository / "ppx/staged.ml").write_text("let fixture = 2\n")
    git(repository, "add", "ppx/staged.ml")
    expect_failure("hygiene-staged-product-dirt", run(repository, target))


def untracked_control(relative):
    def control(repository, target):
        path = repository / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("untracked product\n")
        expect_failure("hygiene-untracked-product-dirt", run(repository, target))

    return control


def missing_repository_control():
    with tempfile.TemporaryDirectory(prefix="verocaml-hygiene-no-git-") as temporary:
        expect_failure("git-repository-missing", run(temporary, "0" * 40))


def missing_object_control(repository, target):
    expect_failure("git-object-missing", run(repository, "f" * 40))


def main():
    for control in (
        clean_control,
        non_product_control,
        target_control,
        tracked_control,
        staged_control,
        missing_object_control,
    ):
        with_repository(control)
    for root in PRODUCT_ROOTS:
        with_repository(untracked_control(f"{root}/untracked.control"))
    for filename in PRODUCT_FILES:
        with_repository(untracked_control(filename))
    missing_repository_control()
    print("checkout-hygiene clean=1 target/tracked/staged=3 untracked-product-roots=11")
    print("checkout-hygiene non-product-dirt=3 missing-repository/object=passed")


if __name__ == "__main__":
    main()
