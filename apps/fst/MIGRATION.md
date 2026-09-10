# Release FST from development to the official repository

Use the helper in the development checkout. It needs Git and an existing Python
runtime; it never installs packages. The default destination is the sibling
`IAC-Decarb-Tools` checkout.

## Routine release with GitHub Desktop

1. Test FST in development, then commit and push the changes to development
   `main`. Pull any newer development commits before choosing the release.
2. In GitHub Desktop, open the official repository and **Fetch origin**. Its
   checkout must have no uncommitted or untracked files before applying a release.
3. Open PowerShell in development `apps/fst` and preview:

   ```powershell
   .\Migrate-FST.ps1
   ```

   This only compares the current committed development version with the fetched
   official `origin/main`. It does not fetch, write files, or create a branch.
   Uncommitted development changes are excluded.
4. Review the additions, updates, removals, and source/base commit IDs. Prepare
   the reviewed release, substituting the commit IDs printed in the preview:

   ```powershell
   .\Migrate-FST.ps1 -SourceRef <development-commit> -ExpectBase <official-base-commit> -Apply
   ```

   If neither checkout's refs have changed since preview, `.\Migrate-FST.ps1 -Apply`
   uses the same versions. A new local `codex/fst-release-<commit>` branch is
   created and changes are staged. Nothing is committed, pushed, merged, or deployed.
5. Review the official branch in GitHub Desktop, run the checks below, commit,
   publish the branch, and create a PR into official `main`. Merge after review.
6. Have IT deploy the merged official commit and verify the image downloads on
   the official website. A merged PR alone does not update that website.

Use `-TargetPath` if the official checkout is elsewhere, or `-Branch` for a
different new branch name. Existing release branches are never overwritten.

## Scope and later releases

- Only tracked `apps/fst` files at the selected commit are transferred. Other
  tools, repository settings, `.github/workflows`, and deployment files are untouched.
- This promotes the entire selected FST version, including documents and assets.
  Official tracked FST files absent from that source version are removed on the
  release branch. Review the preview before applying it.
- `apps/fst/MIGRATION.json` records the source commit, official base, and file
  hashes. Later releases stop on official edits since that receipt, rather than
  silently replacing them. Reconcile those edits in development first.
- The first migration has no receipt; review all differences. The initial FST
  comparison on September 10, 2026 found only formatting differences and DEV's
  correction to the energy-cost percentage labels before this download fix.
- An already-matching release is a no-op. Ignored local files are preserved;
  path collisions and tracked environment files stop the operation.

If replacing a particular official edit is intentional after review, the Python
CLI supports `--accept-official-change apps/fst/path/to/file` for that specific
path. It also supports `--source`, `--target`, `--source-ref`, `--base`, `--branch`,
`--expect-base`, and `--apply`.

## Checks before release

Using already installed dependencies, from development `apps/fst`:

```text
python -B -m unittest discover -s tests -p test_migrate_fst.py
Rscript tests/test_export_wiring.R
Rscript tests/preview_exports.R
```

Use full paths to the installed executables if they are not on PATH. The preview
uses the shipped example workbook and the websites' image security policy. See
`tests/EXPORT_CHECKS.md` for the browser checks. Stop the local preview with Ctrl+C.
Repeat the browser checks on the development website after its deployment, then
on the official website after IT deploys. No packages are installed by these checks.

To roll back a published release, revert its PR in the official repository and
have IT deploy the revert.
