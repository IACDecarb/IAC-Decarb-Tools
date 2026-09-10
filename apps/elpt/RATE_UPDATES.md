# Quarterly utility-rate updates

The repository workflow `.github/workflows/refresh-elpt-rates.yml` refreshes
`apps/elpt/AllUploadFiles_ToolTesting/local_database_rates.json` on the second
Monday of January, April, July, and October. The next dates are **October 12,
2026**, January 11, April 12, and July 12, 2027. Runs are scheduled for 16:17 UTC
(9:17 a.m. Pacific daylight time / 8:17 a.m. Pacific standard time).

The workflow checks Mondays in those months; `refresh_schedule.py` permits only
days 8–14. This avoids cron's OR behavior when both weekday and day-of-month are
specified. Manual **Run workflow** runs on `main` bypass the date check.

## One-time activation

1. Push or merge these changes to the repository's `main` branch.
2. Add the existing OpenEI API key as the GitHub Actions repository secret
   `NREL_API_KEY` in **Settings → Secrets and variables → Actions**. The local
   `.env` stays untracked; it is not available to GitHub's hosted runner.
3. Allow the workflow's `GITHUB_TOKEN` to write repository contents and dispatch
   Actions workflows. If `main` is protected, its rules must permit this bot's
   rate-database commit. A rejected push fails the workflow without force-pushing.
4. The existing `deploy.yml` workflow uses the existing self-hosted runner and
   `DEPLOY_KEY`, `DEPLOY_HOST`, `DEPLOY_USER`, and `DEPLOY_PATH` secrets. Confirm
   those remain available. The added manual trigger lets the refresh request
   deployment after its commit.
5. Use **Actions → Refresh ELPT utility rates → Run workflow** when ready to
   perform an initial live refresh. Verify both the refresh and deployment runs.

No new Python or R packages are installed. The scheduled job uses the Python
standard library and GitHub CLI already present on GitHub's Ubuntu runner.

## Refresh and deployment behavior

- Fetch all configured utility IDs, with pagination and active-rate filtering.
- Reject failed requests and the disappearance of utilities represented in the
  previous database. Some configured IDs legitimately have no commercial or
  industrial rates and are absent from the existing database too.
- Preserve rates in `custom_elpt_rates.json`, including the LBNL planning rate.
- Write the local JSON atomically, then validate required fields, unique rate
  IDs, utility identities, and exact preservation of custom rates before commit.
- Commit only the database JSON to `main`, then explicitly dispatch `deploy.yml`.
  [GitHub does not trigger another push workflow for a `GITHUB_TOKEN` push](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/trigger-a-workflow).
- ELPT checks for a changed database every minute, refreshes rate choices, and
  recalculates the selected rate. A removed or unsupported rate clears the old
  calculation instead of silently retaining stale prices.

If a fetch or validation fails, no database commit or deployment is requested.
If deployment fails after a successful commit, rerun the refresh or run the
deployment manually; an unchanged database still allows deployment to be retried.
Keep Actions failure notifications enabled for the workflow owner.

## GitHub scheduling limits

GitHub can delay or drop scheduled runs, and public-repository schedules can be
disabled after 60 days without repository activity. A quarterly commit alone
does not prevent that timeout. Check that the workflow is enabled before each
quarter, and use **Run workflow** for a missed run. An external scheduler would
be needed if unattended operation through long periods of repository inactivity
is mandatory. See [GitHub's schedule documentation](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#schedule).

## Local checks using existing dependencies

```sh
python3 -B -m unittest discover -s apps/elpt/tests -p 'test_rate_refresh.py'
python3 -B apps/elpt/validate_rate_database.py
Rscript apps/elpt/tests/test_rate_calculations.R
Rscript apps/elpt/tests/test_rate_database_integration.R
Rscript apps/elpt/tests/test_hourly_cost_app.R
```

The Shiny integration check requires the app's existing R packages and testthat;
it stops with a missing-package error instead of installing anything.
