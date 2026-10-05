# Release KMM from development to the official repository

Use `Migrate-KMM.ps1` in `IAC-Decarb-Tools-Dev` to prepare a KMM release from
the Thermal Systems development code for the Industrial Decarbonization website.
This follows the ELPT/FST process and uses Git and an existing Python 3.9+
runtime. The default target is the sibling `IAC-Decarb-Tools` checkout.

## Routine release with GitHub Desktop

1. In GitHub Desktop, switch DEV to `main`, **Fetch/Pull origin**, and ensure
   your intended changes are tested, committed, and pushed.
2. Open the official repository, **Fetch origin**, and ensure it has no
   uncommitted or untracked files. The release starts from fetched `origin/main`.
3. Open PowerShell and preview:

   ```powershell
   cd "C:\Users\btuser\Documents\GitHub\IAC-Decarb-Tools-Dev\apps\kmm"
   .\Migrate-KMM.ps1
   ```

   The preview is read-only. It compares committed DEV `HEAD` with official
   `origin/main`; it does not fetch or include uncommitted development files.
4. Review the additions, updates, removals, and source/base commit IDs, then
   prepare the branch:

   ```powershell
   .\Migrate-KMM.ps1 -Apply
   ```

   This creates `codex/kmm-release-<source commit>` in the official checkout
   and stages only KMM changes. Existing branches are never overwritten.
   Nothing is committed, pushed, merged, or deployed by the helper.
5. Validate the release, review and commit it in GitHub Desktop, publish the
   branch, and merge its PR into official `main` after review. **IT then deploys
   official main to the website.** Verify the deployed KMM page afterward.

To pin exactly what you previewed, substitute the full commit IDs printed above:

```powershell
.\Migrate-KMM.ps1 -SourceRef 'DEVELOPMENT_COMMIT' -ExpectBase 'OFFICIAL_BASE_COMMIT' -Apply
```

Use `-TargetPath` for another official checkout, `-Branch` for a different new
branch name, or `-PythonPath` for an existing Python executable. No packages or
runtimes are installed. The Python CLI (`python -B migrate_kmm.py --help`)
also exposes `--source`, `--base`, and per-file `--accept-official-change` options.

## Scope and repeat releases

- Only Git-tracked `apps/kmm` files at the selected commit are transferred,
  including `profile_summary.R`, the PDF guide, sample workbook, images, and
  release helpers once committed. Official tracked KMM files absent from that
  version are removed.
- Other tools, the site landing page, and `.github/workflows` are unchanged.
  Website deployment and server configuration remain IT's responsibility.
- `apps/kmm/MIGRATION.json` records the source repository, exact source and
  official base commits, and promoted file hashes. Later releases stop if
  official KMM files have changed since the last receipt and differ from DEV.
  Reconcile those changes in DEV before releasing again.
- The first migration has no receipt, so review every difference. Its first
  `-Apply` records a baseline even if the application already matches. Once a
  receipt exists, a matching release is a no-op.
- Ignored local configuration stays local. Dirty target checkouts, conflicting
  ignored files, unsafe paths, and tracked environment files stop the operation.

Commit these helpers in DEV before preparing a release that should include them.
When releasing KMM and PIT, complete or stash the first prepared branch before
starting the second; merge and fetch the first release before basing another
release on its updated official `main`.

## Checks before release

From the DEV repository root, using an existing Python runtime:

```powershell
python -B -m unittest discover -s apps/kmm/tests -p test_migrate_kmm.py
```

Validate the prepared official copy in the official server's R environment:

- Confirm all packages in KMM's `required_packages` list are installed, plus
  `thematic`, which the app calls separately. Prepare these dependencies before
  starting the app; its existing startup code attempts to install missing
  packages from that list.
- Download an input template, upload a representative hourly Excel workbook
  (the bundled example is in `AllUploadFiles_ToolTesting`), and check the suggested
  cluster count, manual changes to that count, and plain-language summary.
- Test hourly Green Button XML import with a known file and confirm its values
  and timestamps. Check cluster profiles, the heatmap, and both box plot tabs.
- Confirm each cluster profile uses its own vertical scale with 5% padding,
  charts remain readable on wide and smaller screens, and the PNG exports match.
- Test chart data and metrics CSV downloads and the PDF user guide link. Repeat
  the main upload and download checks after IT deploys the merged commit.

Testing the migration helper confirms file transfer behavior, not clustering
results or compatibility with the website's R environment. Keep validation
results and source/base commit IDs with the PR. To roll back a published release,
revert its PR in the official repository and have IT deploy the revert.
