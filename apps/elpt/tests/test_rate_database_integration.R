script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[[1]])
script_dir <- dirname(normalizePath(script_path))
source(file.path(script_dir, "..", "rate_calculations.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("Skipping rate database integration test: jsonlite is not installed.\n")
  quit(status = 0)
}

database_path <- file.path(
  script_dir,
  "..",
  "AllUploadFiles_ToolTesting",
  "local_database_rates.json"
)
rates <- jsonlite::fromJSON(database_path)

supported <- 0L
unsupported <- 0L
unexpected <- character()
plm_verified <- FALSE
lbnl_test_case_verified <- FALSE

for (row_index in seq_len(nrow(rates))) {
  rate_row <- rates[row_index, , drop = FALSE]
  tryCatch({
    translated <- translate_urdb_to_elpt(rate_row)

    # Validate the tier schedules that can be active together in each month.
    for (month_index in seq_len(12)) {
      period_indices <- sort(unique(c(
        translated$e_sched[month_index, ],
        translated$w_sched[month_index, ]
      ))) + 1L
      if (any(period_indices > length(translated$e_tiers))) {
        elpt_unsupported_rate("The energy schedule refers to an undefined rate period.")
      }
      elpt_resolve_energy_tiers(
        translated$e_tiers[period_indices],
        billing_demand_kw = 100
      )
    }

    if (
      identical(as.character(rate_row$Utility_Name[[1]]), "Georgia Power Co") &&
        grepl("PLM-18", as.character(rate_row$Rate_Name[[1]]), fixed = TRUE)
    ) {
      plm_period <- translated$e_tiers[[1]]
      resolved <- elpt_resolve_energy_tiers(list(plm_period), 509)
      expected <- c(3000, 10000, 101800, 203600, 305400, Inf)
      if (!isTRUE(all.equal(resolved$bounds, expected))) {
        stop("Georgia Power PLM-18 did not resolve to the expected block limits.")
      }
      plm_verified <- TRUE
    }

    if (identical(as.character(rate_row$OpenEI_ID[[1]]), "LBNL-HYBRID-315000-V1")) {
      monthly_incremental_rates <- c(
        0.090721077, 0.040722557, 0.037444226, 0.038215054,
        0.035374621, 0.057496804, 0.133064758, 0.101464639,
        0.036376598, 0.034041349, 0.03229222, 0.041117497
      )
      expected_bounds <- c(3000, 10000, 101800, 203600, 305400, 315000, Inf)
      fuel_surcharge <- 0.045876
      base_cbl_cost <-
        3000 * 0.153054 + 7000 * 0.140178 + 91800 * 0.120861 +
        101800 * 0.015555 + 101800 * 0.011705 + 9600 * 0.010177 +
        315000 * fuel_surcharge

      for (month_index in seq_len(12)) {
        tiers <- translated$e_tiers[[month_index]]
        if (!isTRUE(all.equal(tiers$max, expected_bounds)) ||
            !isTRUE(all.equal(
              head(tiers$rate, -1),
              c(0.153054, 0.140178, 0.120861, 0.015555, 0.011705, 0.010177) +
                fuel_surcharge
            )) ||
            !isTRUE(all.equal(tail(tiers$rate, 1), monthly_incremental_rates[[month_index]])) ||
            !all(translated$e_sched[month_index, ] == month_index - 1L) ||
            !all(translated$w_sched[month_index, ] == month_index - 1L)) {
          stop("The LBNL test rate has incorrect monthly tiers, prices, or schedules.")
        }
        for (incremental_kwh in c(1400000, 2500000)) {
          actual_cost <- elpt_energy_charge_breakdown(
            315000 + incremental_kwh, list(tiers), billing_demand_kw = 0
          )$total_cost
          expected_cost <- base_cbl_cost +
            incremental_kwh * monthly_incremental_rates[[month_index]]
          if (!isTRUE(all.equal(actual_cost, expected_cost, tolerance = 1e-6))) {
            stop("The LBNL test rate produced an incorrect hybrid energy charge.")
          }
        }
      }
      if (translated$fixed_charge != 0 ||
          any(vapply(translated$d_tiers, function(tiers) any(tiers$rate != 0), logical(1))) ||
          any(vapply(translated$flat_tiers, function(tiers) any(tiers$rate != 0), logical(1)))) {
        stop("The LBNL test rate must not include fixed or demand charges.")
      }
      lbnl_test_case_verified <- TRUE
    }

    supported <- supported + 1L
  }, elpt_unsupported_rate = function(error) {
    unsupported <<- unsupported + 1L
  }, error = function(error) {
    unexpected <<- c(
      unexpected,
      paste(rate_row$Utility_Name[[1]], rate_row$Rate_Name[[1]], conditionMessage(error), sep = " | ")
    )
  })
}

if (length(unexpected) > 0) {
  stop(
    "Unexpected database parsing errors:\n",
    paste(head(unexpected, 20), collapse = "\n"),
    call. = FALSE
  )
}
if (!plm_verified) stop("Georgia Power PLM-18 was not verified.", call. = FALSE)
if (!lbnl_test_case_verified) stop("The LBNL custom test rate was not verified.", call. = FALSE)

cat(
  "Rate database integration passed:", supported, "supported,",
  unsupported, "rejected with explicit unsupported-rate errors.\n"
)
