"""Exercise migrations against disposable repositories with unrelated histories."""

from contextlib import redirect_stdout
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "migrate_fst", Path(__file__).resolve().parents[1] / "migrate_fst.py"
)
migration = importlib.util.module_from_spec(spec)
spec.loader.exec_module(migration)


class MigrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="fst-migration-test-")
        self.workspace = Path(self.temp.name).resolve()
        # Cleanup is restricted to the newly created test directory.
        if self.workspace.parent != Path(tempfile.gettempdir()).resolve():
            raise AssertionError("Unexpected temporary-directory location")
        self.addCleanup(self.temp.cleanup)
        self.source = self.workspace / "development"
        self.target = self.workspace / "official"
        for repo in (self.source, self.target):
            repo.mkdir()
            migration.git(repo, "init", "-q", "-b", "main")
            migration.git(repo, "config", "user.name", "FST Test")
            migration.git(repo, "config", "user.email", "fst-test@example.invalid")
            migration.git(repo, "config", "core.autocrlf", "false")
        self.write(self.source, "apps/fst/app.R", "new application\n")
        self.write(self.source, "apps/fst/rates.json", '[{"rate": 0.1}]\n')
        self.write(self.source, ".github/workflows/refresh.yml", "development schedule\n")
        self.write(self.source, ".gitignore", ".env\n")
        self.commit(self.source, "Development release")
        self.write(self.target, "apps/fst/app.R", "old application\n")
        self.write(self.target, "apps/fst/obsolete.R", "retired\n")
        self.write(self.target, "apps/other/app.R", "official other tool\n")
        self.write(self.target, ".github/workflows/deploy.yml", "official deployment\n")
        self.write(self.target, ".gitignore", ".env\ncache.txt\n")
        self.commit(self.target, "Independent official history")

    def write(self, repo, name, content):
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def commit(self, repo, message):
        migration.git(repo, "add", ".")
        migration.git(repo, "commit", "-qm", message)

    def proposal(self):
        return migration.plan(self.source, self.target, "HEAD", "main")

    def apply(self, proposal=None):
        with redirect_stdout(io.StringIO()):
            return migration.apply(proposal or self.proposal())

    def finish_release(self):
        branch = self.apply()
        self.commit(self.target, "Release FST")
        migration.git(self.target, "switch", "main")
        migration.git(self.target, "merge", "--ff-only", branch)

    def test_preview_does_not_change_target(self):
        head = migration.revision(self.target, "HEAD")
        proposal = self.proposal()
        self.assertEqual(proposal["changes"], [
            ("UPDATE", "apps/fst/app.R"), ("REMOVE", "apps/fst/obsolete.R"),
            ("ADD", "apps/fst/rates.json")
        ])
        self.assertEqual(migration.revision(self.target, "HEAD"), head)
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")
        self.assertFalse((self.target / ".git/FETCH_HEAD").exists())

    def test_unrelated_repositories_transfer_only_fst_and_preserve_ignored_files(self):
        self.write(self.source, "apps/fst/.env", "DEVELOPMENT_LOCAL=1\n")
        self.write(self.target, "apps/fst/.env", "OFFICIAL_LOCAL=1\n")
        base = migration.revision(self.target, "main")
        self.apply()
        self.assertEqual((self.target / "apps/fst/app.R").read_text(), "new application\n")
        self.assertFalse((self.target / "apps/fst/obsolete.R").exists())
        self.assertEqual((self.target / "apps/fst/.env").read_text(), "OFFICIAL_LOCAL=1\n")
        self.assertFalse((self.target / ".github/workflows/refresh.yml").exists())
        self.assertEqual((self.target / ".github/workflows/deploy.yml").read_text(), "official deployment\n")
        self.assertEqual((self.target / "apps/other/app.R").read_text(), "official other tool\n")
        self.assertEqual(migration.revision(self.target, "HEAD"), base)  # no commit
        self.assertEqual(migration.revision(self.target, "main"), base)
        receipt = json.loads((self.target / migration.RECORD).read_text())
        self.assertEqual(receipt["source_commit"], migration.revision(self.source, "HEAD"))
        self.assertEqual(receipt["official_base_commit"], base)
        self.assertTrue(all(name.startswith("apps/fst/") for name in receipt["files"]))

    def test_same_release_is_a_noop_after_merge(self):
        self.finish_release()
        self.assertIsNone(self.apply())
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")

    def test_official_edits_and_newer_rates_stop_next_release(self):
        self.finish_release()
        self.write(self.target, "apps/fst/rates.json", '[{"rate": 0.2}]\n')
        self.commit(self.target, "New production rates")
        self.write(self.source, "apps/fst/app.R", "next application\n")
        self.commit(self.source, "Next development release")
        proposal = self.proposal()
        self.assertEqual(proposal["official_edits"], ["apps/fst/rates.json"])
        with self.assertRaisesRegex(migration.MigrationError, "Reconcile"):
            self.apply(proposal)
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")

    def test_dirty_target_stops_before_fetch_or_branch_creation(self):
        self.write(self.target, "apps/other/app.R", "uncommitted user work\n")
        with self.assertRaisesRegex(migration.MigrationError, "uncommitted"):
            self.apply()
        self.assertFalse((self.target / ".git/FETCH_HEAD").exists())
        self.assertEqual(migration.git(self.target, "branch", "--show-current").stdout.strip(), "main")

    def test_ignored_destination_collision_stops(self):
        self.write(self.source, "apps/fst/cache.txt", "committed release file\n")
        self.commit(self.source, "Add file")
        self.write(self.target, "apps/fst/cache.txt", "ignored local work\n")
        with self.assertRaisesRegex(migration.MigrationError, "ignored file"):
            self.apply()
        self.assertEqual((self.target / "apps/fst/cache.txt").read_text(), "ignored local work\n")

    def test_tracked_credentials_are_rejected(self):
        self.write(self.source, "apps/fst/.env", "TEST_PLACEHOLDER=1\n")
        migration.git(self.source, "add", "-f", "apps/fst/.env")
        migration.git(self.source, "commit", "-qm", "Accidental tracked environment")
        with self.assertRaisesRegex(migration.MigrationError, "environment file"):
            self.proposal()

    def test_existing_release_branch_is_not_overwritten(self):
        proposal = self.proposal()
        branch = f"codex/fst-release-{proposal['source_sha'][:12]}"
        migration.git(self.target, "branch", branch)
        with self.assertRaisesRegex(migration.MigrationError, "already exists"):
            self.apply(proposal)


if __name__ == "__main__":
    unittest.main()
