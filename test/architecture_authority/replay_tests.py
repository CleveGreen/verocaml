#!/usr/bin/env python3
import hashlib
import json
import pathlib
import subprocess
import sys
import tempfile


POLICY = pathlib.Path(__file__).resolve().parent / "policy.py"
RECORDS = pathlib.Path(__file__).resolve().parent / "records"


def run(repository, record, *arguments):
    return subprocess.run(
        [
            sys.executable,
            str(POLICY),
            "replay",
            "--repository",
            str(repository),
            "--record",
            str(record),
            *arguments,
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


def write_manifest(path, rows):
    contents = ("\n".join(rows) + "\n").encode()
    path.write_bytes(contents)
    return len(rows), hashlib.sha256(contents).hexdigest()


def record_controls(repository, record_path):
    record = json.loads(record_path.read_text())
    manifest = record_path.parent / record["manifest"]
    rows = manifest.read_text().splitlines()
    expect_success(run(repository, record_path))
    with tempfile.TemporaryDirectory(prefix=f"{record['id'].lower()}-replay-") as temporary:
        temporary = pathlib.Path(temporary)
        tampered = temporary / "tampered.manifest"
        contents = bytearray(manifest.read_bytes())
        contents[0] = ord("z") if contents[0] != ord("z") else ord("y")
        tampered.write_bytes(contents)
        expect_failure(
            "replay-manifest-digest-mismatch",
            run(repository, record_path, "--manifest", str(tampered)),
        )
        expect_failure(
            "replay-manifest-row-count-mismatch",
            run(repository, record_path, "--rows", str(record["rows"] + 1)),
        )
        expect_failure(
            "replay-manifest-digest-mismatch",
            run(repository, record_path, "--sha256", "0" * 64),
        )
        reordered = temporary / "reordered.manifest"
        count, digest = write_manifest(reordered, list(reversed(rows)))
        expect_failure(
            "replay-manifest-order-mismatch",
            run(
                repository,
                record_path,
                "--manifest",
                str(reordered),
                "--rows",
                str(count),
                "--sha256",
                digest,
            ),
        )
        expect_failure(
            "replay-base-mismatch",
            run(repository, record_path, "--base", record["target"]),
        )
        expect_failure(
            "replay-target-mismatch",
            run(repository, record_path, "--target", record["base"]),
        )
        expect_failure(
            "replay-target-mismatch",
            run(repository, record_path, "--target", "HEAD"),
        )

        missing = temporary / "missing.manifest"
        count, digest = write_manifest(missing, rows[1:])
        expect_failure(
            "replay-path-missing",
            run(
                repository,
                record_path,
                "--manifest",
                str(missing),
                "--rows",
                str(count),
                "--sha256",
                digest,
            ),
        )

        extra = temporary / "extra.manifest"
        count, digest = write_manifest(extra, sorted([*rows, "zzzz/replay-extra"]))
        expect_failure(
            "replay-path-extra",
            run(
                repository,
                record_path,
                "--manifest",
                str(extra),
                "--rows",
                str(count),
                "--sha256",
                digest,
            ),
        )


def dirty_clone_controls(repository, records):
    with tempfile.TemporaryDirectory(prefix="verocaml-replay-dirty-") as temporary:
        clone = pathlib.Path(temporary) / "clone"
        subprocess.run(
            ["git", "clone", "-q", "--shared", str(repository), str(clone)], check=True
        )
        dirt = clone / "src/replay_untracked_product.ml"
        dirt.write_text("let dirt = ()\n")
        for record in records:
            expect_success(run(clone, record))


def bare_repository_controls(repository, records):
    with tempfile.TemporaryDirectory(prefix="verocaml-replay-bare-") as temporary:
        bare = pathlib.Path(temporary) / "objects.git"
        subprocess.run(
            ["git", "clone", "-q", "--bare", "--shared", str(repository), str(bare)],
            check=True,
        )
        for record in records:
            expect_success(run(bare, record))


def missing_repository_control(record):
    with tempfile.TemporaryDirectory(prefix="verocaml-replay-no-git-") as temporary:
        expect_failure("git-repository-missing", run(temporary, record))


def missing_object_control(record):
    with tempfile.TemporaryDirectory(prefix="verocaml-replay-missing-object-") as temporary:
        repository = pathlib.Path(temporary)
        subprocess.run(["git", "init", "-q", str(repository)], check=True)
        (repository / "README").write_text("fixture\n")
        subprocess.run(["git", "-C", str(repository), "add", "README"], check=True)
        subprocess.run(
            [
                "git",
                "-C",
                str(repository),
                "-c",
                "user.name=Architecture Replay",
                "-c",
                "user.email=replay@example.invalid",
                "-c",
                "commit.gpgSign=false",
                "commit",
                "-q",
                "-m",
                "fixture",
            ],
            check=True,
        )
        expect_failure("git-object-missing", run(repository, record))


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: replay_tests.py REPOSITORY")
    repository = pathlib.Path(sys.argv[1]).resolve()
    records = [RECORDS / name for name in ("vero063.json", "vero064.json")]
    for record in records:
        record_controls(repository, record)
    dirty_clone_controls(repository, records)
    bare_repository_controls(repository, records)
    missing_repository_control(records[0])
    missing_object_control(records[0])
    print("object-replay records=2 positive=6 per-record-negatives=18")
    print(
        "object-replay dirt-independence=passed bare=2 "
        "missing-repository/object=passed"
    )


if __name__ == "__main__":
    main()
