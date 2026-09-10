"""Prepare a reviewable FST release using Git and Python's standard library.

The default is a read-only comparison. --apply creates a local branch and stages
the selected committed snapshot. It never commits, pushes, merges, or deploys.
"""

import argparse
from datetime import datetime, timezone
import json
from pathlib import Path, PurePosixPath
import subprocess
import sys

APP = "apps/fst"
RECORD = f"{APP}/MIGRATION.json"


class MigrationError(Exception):
    pass


def git(repo, *args, check=True):
    result = subprocess.run(
        ["git", "-C", str(repo), *args], stdout=subprocess.PIPE,
        stderr=subprocess.PIPE, encoding="utf-8", errors="replace",
    )
    if check and result.returncode:
        raise MigrationError(result.stderr.strip() or result.stdout.strip())
    return result


def root(path):
    return Path(git(path, "rev-parse", "--show-toplevel").stdout.strip()).resolve()


def revision(repo, ref):
    return git(repo, "rev-parse", "--verify", "--end-of-options", f"{ref}^{{commit}}").stdout.strip()


def tree(repo, ref):
    entries = {}
    for entry in git(repo, "ls-tree", "-rz", "--full-tree", ref, "--", APP).stdout.split("\0"):
        if not entry:
            continue
        info, name = entry.split("\t", 1)
        mode, kind, blob = info.split()
        if kind != "blob" or mode not in ("100644", "100755"):
            raise MigrationError(f"Unsupported link or submodule in FST: {name}")
        entries[name] = {"mode": mode, "blob": blob}
    return entries


def record(repo, ref):
    result = git(repo, "show", f"{ref}:{RECORD}", check=False)
    if result.returncode:
        return None
    try:
        value = json.loads(result.stdout)
        if value.get("schema_version") != 1 or not isinstance(value.get("files"), dict):
            raise ValueError("unrecognized format")
        return value
    except (ValueError, AttributeError) as error:
        raise MigrationError(f"Invalid {RECORD}: {error}") from error


def check_paths(target, paths):
    """Resolve every affected path before Git can replace or remove any file."""
    app_dir = target / APP
    if app_dir.resolve() != app_dir:
        raise MigrationError("The target FST directory is a link or junction.")
    folded = set()
    for name in paths:
        relative = PurePosixPath(name)
        if not name.startswith(APP + "/") or any(
            part in ("", ".", "..") or ":" in part or "\\" in part
            for part in relative.parts
        ):
            raise MigrationError(f"Unsafe migration path: {name}")
        key = name.casefold()
        if key in folded:
            raise MigrationError(f"Case-insensitive path collision: {name}")
        folded.add(key)
        destination = target.joinpath(*relative.parts)
        if destination.resolve() != destination or not destination.resolve().is_relative_to(app_dir):
            raise MigrationError(f"Migration path resolves outside FST or through a link: {name}")


def plan(source, target, source_ref="HEAD", base_ref="origin/main"):
    source, target = root(source), root(target)
    if source == target or source.is_relative_to(target) or target.is_relative_to(source):
        raise MigrationError("Source and target must be separate, non-nested repositories.")
    source_sha, base_sha = revision(source, source_ref), revision(target, base_ref)
    incoming, existing = tree(source, source_sha), tree(target, base_sha)
    incoming.pop(RECORD, None)
    existing.pop(RECORD, None)
    if f"{APP}/app.R" not in incoming:
        raise MigrationError("The selected source commit has no FST app.R.")
    for name in incoming:
        parts = PurePosixPath(name).parts
        if any(part in (".git", ".venv", ".RData", ".Rhistory", ".env") or
               part.startswith(".env.") for part in parts):
            raise MigrationError(f"Refusing to migrate a tracked local environment file: {name}")
    check_paths(target, set(incoming) | set(existing) | {RECORD})
    changes = []
    for name in sorted(set(incoming) | set(existing)):
        if incoming.get(name) != existing.get(name):
            status = "ADD" if name not in existing else "REMOVE" if name not in incoming else "UPDATE"
            changes.append((status, name))
    previous = record(target, base_sha)
    official_edits = []
    if previous:
        prior_files = previous["files"]
        for name in sorted(set(prior_files) | set(existing)):
            if existing.get(name) != prior_files.get(name) and existing.get(name) != incoming.get(name):
                official_edits.append(name)
    return {
        "source": source, "target": target, "source_sha": source_sha,
        "base_sha": base_sha, "base_ref": base_ref, "files": incoming,
        "existing": existing, "changes": changes, "official_edits": official_edits,
        "first_migration": previous is None,
    }


def display(proposal):
    print(f"Source: {proposal['source']}\nCommit: {proposal['source_sha']}")
    print(f"Target: {proposal['target']}\nBase:   {proposal['base_sha']}")
    print("Scope:  tracked apps/fst files; GitHub workflows are excluded.\n")
    for status, name in proposal["changes"]:
        print(f"{status:6} {name}")
    print(f"\n{len(proposal['changes'])} application file change(s).")
    if proposal["first_migration"]:
        print("First migration: review all differences; there is no earlier migration record.")
    if proposal["official_edits"]:
        print("\nOfficial changes that need reconciliation (including any newer rate JSON):")
        for name in proposal["official_edits"]:
            print(f"  {name}")
    print("Only the selected commit is included; uncommitted development changes are excluded.")


def apply(proposal, branch=None, accept_official_changes=()):
    source, target = proposal["source"], proposal["target"]
    if git(target, "status", "--porcelain", "--untracked-files=all").stdout.strip():
        raise MigrationError("The official checkout has uncommitted or untracked files. Commit or stash them first.")
    rejected = set(proposal["official_edits"]) - set(accept_official_changes)
    if rejected:
        raise MigrationError("Reconcile these official edits before migration: " + ", ".join(sorted(rejected)))
    if not proposal["changes"] and not proposal["first_migration"]:
        print("The official FST files already match this development version. Nothing to stage.")
        return None
    current = tree(target, "HEAD")
    check_paths(target, set(proposal["files"]) | set(proposal["existing"]) | set(current) | {RECORD})
    # Protect ignored files too; Git status deliberately does not list .env, etc.
    for name in set(proposal["files"]) | set(proposal["existing"]) | {RECORD}:
        if name not in current and (target / name).exists():
            raise MigrationError(f"An untracked or ignored file would be replaced: {name}")
    branch = branch or f"codex/fst-release-{proposal['source_sha'][:12]}"
    git(target, "check-ref-format", "--branch", branch)
    if not git(target, "show-ref", "--verify", "--quiet", f"refs/heads/{branch}", check=False).returncode:
        raise MigrationError(f"Branch already exists: {branch}. Review it or choose --branch with a new name.")
    git(target, "fetch", "--no-tags", "--", str(source), proposal["source_sha"])
    if revision(target, "FETCH_HEAD") != proposal["source_sha"]:
        raise MigrationError("Fetched source did not match the selected commit.")
    if revision(target, proposal["base_ref"]) != proposal["base_sha"]:
        raise MigrationError("The target base changed after comparison. Run a fresh preview.")
    git(target, "switch", "-c", branch, proposal["base_sha"])
    git(target, "restore", "--source", proposal["source_sha"], "--staged", "--worktree", "--", APP)
    receipt = {
        "schema_version": 1,
        "source_repository": git(source, "remote", "get-url", "origin", check=False).stdout.strip() or source.name,
        "source_commit": proposal["source_sha"],
        "official_base_commit": proposal["base_sha"],
        "prepared_at_utc": datetime.now(timezone.utc).isoformat(),
        "files": proposal["files"],
    }
    (target / RECORD).write_text(json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    git(target, "add", "--", RECORD)
    changed = git(target, "diff", "--cached", "--name-only", "-z", proposal["base_sha"]).stdout.split("\0")
    if any(name and not name.startswith(APP + "/") for name in changed):
        raise MigrationError("Unexpected staged changes outside FST. Stop and inspect the release branch.")
    # Compare Git's index with the selected source; line-ending conversions in
    # the working tree must not change the committed application snapshot.
    staged = {}
    for entry in git(target, "ls-files", "--stage", "-z", "--", APP).stdout.split("\0"):
        if entry:
            info, name = entry.split("\t", 1)
            mode, blob, stage = info.split()
            if name != RECORD:
                staged[name] = {"mode": mode, "blob": blob}
    if staged != proposal["files"]:
        raise MigrationError("The staged application differs from the selected source commit.")
    print(f"\nPrepared branch: {branch}\nChanges are staged for GitHub Desktop review.")
    print(f"Suggested commit summary: Update FST from development {proposal['source_sha'][:12]}")
    print("Nothing has been committed, pushed, merged, or deployed.")
    return branch


def main():
    default_source = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=default_source)
    parser.add_argument("--target", type=Path, default=default_source.parent / "IAC-Decarb-Tools")
    parser.add_argument("--source-ref", default="HEAD", help="Committed development version; default HEAD")
    parser.add_argument("--base", default="origin/main", help="Official base; default origin/main")
    parser.add_argument("--branch", help="Optional new release branch name")
    parser.add_argument("--apply", action="store_true", help="Create a local release branch and stage changes")
    parser.add_argument("--expect-base", help="Stop if the official base differs from this reviewed commit")
    parser.add_argument("--accept-official-change", action="append", default=[], metavar="PATH",
                        help="After review, replace this specific repository-relative official path")
    args = parser.parse_args()
    try:
        proposal = plan(args.source, args.target, args.source_ref, args.base)
        display(proposal)
        if args.expect_base and proposal["base_sha"] != args.expect_base:
            raise MigrationError("Official base differs from --expect-base. Review a new comparison.")
        if args.apply:
            apply(proposal, args.branch, args.accept_official_change)
        else:
            print("\nPreview only. Fetch origin in GitHub Desktop, review this list, then rerun with --apply.")
        return 0
    except (MigrationError, OSError) as error:
        print(f"Migration stopped: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
