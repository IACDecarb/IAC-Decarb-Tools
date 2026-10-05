"""Exercise migrations against disposable repositories with unrelated histories."""

from contextlib import redirect_stdout
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "migrate_kmm", Path(__file__).resolve().parents[1] / "migrate_kmm.py"
)
migration = importlib.util.module_from_spec(spec)
spec.loader.exec_module(migration)


class MigrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="kmm-migration-test-")
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
            migration.git(repo, "config", "user.name", "KMM Test")
            migration.git(repo, "config", "user.email", "kmm-test@example.invalid")
            migration.git(repo, "config", "core.autocrlf", "false")
        self.write(self.source, "apps/kmm/app.R", "new application\n")
        self.write(self.source, "apps/kmm/reference.json", '[{"rate": 0.1}]\n')
        self.write(self.source, ".github/workflows/refresh.yml", "development schedule\n")
        self.write(self.source, ".gitignore", ".env\n")
        self.commit(self.source, "Development release")
        self.write(self.target, "apps/kmm/app.R", "old application\n")
        self.write(self.target, "apps/kmm/obsolete.R", "retired\n")
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
        self.commit(self.target, "Release KMM")
        migration.git(self.target, "switch", "main")
        migration.git(self.target, "merge", "--ff-only", branch)

    def test_preview_does_not_change_target(self):
        head = migration.revision(self.target, "HEAD")
        proposal = self.proposal()
        self.assertEqual(proposal["changes"], [
            ("UPDATE", "apps/kmm/app.R"), ("REMOVE", "apps/kmm/obsolete.R"),
            ("ADD", "apps/kmm/reference.json")
        ])
        self.assertEqual(migration.revision(self.target, "HEAD"), head)
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")
        self.assertFalse((self.target / ".git/FETCH_HEAD").exists())

    def test_unrelated_repositories_transfer_only_kmm_and_preserve_ignored_files(self):
        self.write(self.source, "apps/kmm/.env", "DEVELOPMENT_LOCAL=1\n")
        self.write(self.target, "apps/kmm/.env", "OFFICIAL_LOCAL=1\n")
        base = migration.revision(self.target, "main")
        self.apply()
        self.assertEqual((self.target / "apps/kmm/app.R").read_text(), "new application\n")
        self.assertFalse((self.target / "apps/kmm/obsolete.R").exists())
        self.assertEqual((self.target / "apps/kmm/.env").read_text(), "OFFICIAL_LOCAL=1\n")
        self.assertFalse((self.target / ".github/workflows/refresh.yml").exists())
        self.assertEqual((self.target / ".github/workflows/deploy.yml").read_text(), "official deployment\n")
        self.assertEqual((self.target / "apps/other/app.R").read_text(), "official other tool\n")
        self.assertEqual(migration.revision(self.target, "HEAD"), base)  # no commit
        self.assertEqual(migration.revision(self.target, "main"), base)
        receipt = json.loads((self.target / migration.RECORD).read_text())
        self.assertEqual(receipt["source_commit"], migration.revision(self.source, "HEAD"))
        self.assertEqual(receipt["official_base_commit"], base)
        self.assertTrue(all(name.startswith("apps/kmm/") for name in receipt["files"]))

    def test_same_release_is_a_noop_after_merge(self):
        self.finish_release()
        self.assertIsNone(self.apply())
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")

    def test_official_asset_edits_stop_next_release(self):
        self.finish_release()
        self.write(self.target, "apps/kmm/reference.json", '[{"rate": 0.2}]\n')
        self.commit(self.target, "Update official reference data")
        self.write(self.source, "apps/kmm/app.R", "next application\n")
        self.commit(self.source, "Next development release")
        proposal = self.proposal()
        self.assertEqual(proposal["official_edits"], ["apps/kmm/reference.json"])
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
        self.write(self.source, "apps/kmm/cache.txt", "committed release file\n")
        self.commit(self.source, "Add file")
        self.write(self.target, "apps/kmm/cache.txt", "ignored local work\n")
        with self.assertRaisesRegex(migration.MigrationError, "ignored file"):
            self.apply()
        self.assertEqual((self.target / "apps/kmm/cache.txt").read_text(), "ignored local work\n")

    def test_tracked_credentials_are_rejected(self):
        self.write(self.source, "apps/kmm/.env", "TEST_PLACEHOLDER=1\n")
        migration.git(self.source, "add", "-f", "apps/kmm/.env")
        migration.git(self.source, "commit", "-qm", "Accidental tracked environment")
        with self.assertRaisesRegex(migration.MigrationError, "environment file"):
            self.proposal()

    def test_selected_commit_excludes_later_commits_and_uncommitted_work(self):
        selected = migration.revision(self.source, "HEAD")
        self.write(self.source, "apps/kmm/app.R", "later committed app\n")
        self.commit(self.source, "Later development commit")
        self.write(self.source, "apps/kmm/reference.json", "uncommitted work\n")
        self.write(self.source, "apps/kmm/draft.R", "untracked work\n")
        proposal = migration.plan(self.source, self.target, selected, "main")
        self.apply(proposal)
        self.assertEqual((self.target / "apps/kmm/app.R").read_text(), "new application\n")
        self.assertEqual(
            (self.target / "apps/kmm/reference.json").read_text(), '[{"rate": 0.1}]\n'
        )
        self.assertFalse((self.target / "apps/kmm/draft.R").exists())

    def test_first_release_can_add_an_app_absent_from_official(self):
        migration.git(self.target, "rm", "--", "apps/kmm/app.R", "apps/kmm/obsolete.R")
        self.commit(self.target, "Official repository before adding this tool")
        self.assertFalse(migration.tree(self.target, "main"))
        self.apply()
        self.assertEqual((self.target / "apps/kmm/app.R").read_text(), "new application\n")
        self.assertTrue((self.target / migration.RECORD).is_file())

    def test_binary_guide_and_supporting_code_are_preserved(self):
        guide = "apps/kmm/User Guide for KMM Tool.pdf"
        payload = b"%PDF-1.4\n" + bytes(range(256)) * 8
        (self.source / guide).write_bytes(payload)
        self.write(self.source, "apps/kmm/helper.R", "helper <- TRUE\n")
        self.commit(self.source, "Add guide and helper")
        self.apply()
        self.assertEqual((self.target / guide).read_bytes(), payload)
        self.assertEqual((self.target / "apps/kmm/helper.R").read_text(), "helper <- TRUE\n")

    def test_changed_official_base_stops_before_switching_branch(self):
        proposal = self.proposal()
        self.write(self.target, "apps/other/app.R", "new official tool version\n")
        self.commit(self.target, "Official main advanced after preview")
        with self.assertRaisesRegex(migration.MigrationError, "base changed"):
            self.apply(proposal)
        self.assertEqual(migration.git(self.target, "branch", "--show-current").stdout.strip(), "main")
        self.assertEqual((self.target / "apps/kmm/app.R").read_text(), "old application\n")
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")

    def test_named_environment_files_are_rejected(self):
        for name in ("api.env", ".Renviron", ".env.production"):
            with self.subTest(name=name):
                path = f"apps/kmm/{name}"
                self.write(self.source, path, "TEST_PLACEHOLDER=1\n")
                migration.git(self.source, "add", "-f", "--", path)
                migration.git(self.source, "commit", "-qm", "Accidental tracked configuration")
                with self.assertRaisesRegex(migration.MigrationError, "environment file"):
                    self.proposal()
                migration.git(self.source, "rm", "--", path)
                self.commit(self.source, "Remove test configuration")

    def test_first_matching_release_creates_receipt_then_becomes_noop(self):
        self.write(self.target, "apps/kmm/app.R", "new application\n")
        self.write(self.target, "apps/kmm/reference.json", '[{"rate": 0.1}]\n')
        migration.git(self.target, "rm", "--", "apps/kmm/obsolete.R")
        self.commit(self.target, "App already synchronized manually")
        proposal = self.proposal()
        self.assertEqual(proposal["changes"], [])
        self.assertTrue(proposal["first_migration"])
        self.finish_release()
        self.assertTrue((self.target / migration.RECORD).is_file())
        self.assertIsNone(self.apply())
        self.assertEqual(migration.git(self.target, "status", "--porcelain").stdout, "")

    def test_existing_release_branch_is_not_overwritten(self):
        proposal = self.proposal()
        branch = f"codex/kmm-release-{proposal['source_sha'][:12]}"
        migration.git(self.target, "branch", branch)
        with self.assertRaisesRegex(migration.MigrationError, "already exists"):
            self.apply(proposal)


if __name__ == "__main__":
    unittest.main()
