script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[[1]])
script_dir <- dirname(normalizePath(script_path))
source(file.path(script_dir, "..", "rate_calculations.R"))

expect_equal <- function(actual, expected, tolerance = 1e-8, label = "value") {
  actual_numeric <- as.numeric(actual)
  expected_numeric <- as.numeric(expected)
  same_length <- length(actual_numeric) == length(expected_numeric)
  same_values <- FALSE
  if (same_length) {
    both_infinite <- is.infinite(actual_numeric) & is.infinite(expected_numeric) &
      sign(actual_numeric) == sign(expected_numeric)
    both_finite <- is.finite(actual_numeric) & is.finite(expected_numeric)
    same_values <- all(
      both_infinite |
        (both_finite & abs(actual_numeric - expected_numeric) <= tolerance)
    )
  }
  if (!same_length || !same_values) {
    stop(
      label, " mismatch. Expected ", paste(expected, collapse = ", "),
      "; got ", paste(actual, collapse = ", "),
      call. = FALSE
    )
  }
}

expect_unsupported <- function(expression, label) {
  unsupported <- FALSE
  tryCatch(
    force(expression),
    elpt_unsupported_rate = function(error) unsupported <<- TRUE
  )
  if (!unsupported) stop(label, " should have been rejected.", call. = FALSE)
}

tier_df <- function(maximum, rate, unit = "kWh") {
  data.frame(
    max = maximum,
    unit = rep(unit, length(maximum)),
    rate = rate,
    adj = 0,
    stringsAsFactors = FALSE
  )
}

# Ordinary cumulative kWh tiers: first 3,000, next 7,000, then the remainder.
ordinary_tiers <- tier_df(
  c(3000, 10000, Inf),
  c(0.15, 0.14, 0.12)
)
ordinary_charge <- elpt_energy_charge_breakdown(
  12000,
  list(ordinary_tiers),
  billing_demand_kw = 0
)
expect_equal(
  ordinary_charge$total_cost,
  3000 * 0.15 + 7000 * 0.14 + 2000 * 0.12,
  label = "ordinary tier charge"
)

# NREL's kWh/kW block-step method used by Georgia Power PLM-18.
plm_tiers <- data.frame(
  max = c(200, 3000, 10000, 200000, Inf, 400, 600, Inf),
  unit = c("kWh/kW", "kWh", "kWh", "kWh", "kWh", "kWh/kW", "kWh/kW", "kWh/kW"),
  rate = c(0, 0.153054, 0.140178, 0.120861, 0.093761, 0.015555, 0.011705, 0.010177),
  adj = 0,
  stringsAsFactors = FALSE
)
resolved_plm <- elpt_resolve_energy_tiers(list(plm_tiers), 509)
expect_equal(
  resolved_plm$bounds,
  c(3000, 10000, 101800, 203600, 305400, Inf),
  label = "PLM-18 resolved bounds"
)
expect_equal(
  resolved_plm$tier_indices,
  c(2, 3, 4, 6, 7, 8),
  label = "PLM-18 resolved tier indices"
)

plm_charge <- elpt_energy_charge_breakdown(
  343453,
  list(plm_tiers),
  billing_demand_kw = 509
)
expected_plm_charge <-
  3000 * 0.153054 +
  7000 * 0.140178 +
  91800 * 0.120861 +
  101800 * 0.015555 +
  101800 * 0.011705 +
  38053 * 0.010177
expect_equal(
  plm_charge$total_cost,
  expected_plm_charge,
  tolerance = 1e-6,
  label = "PLM-18 invoice block charge"
)

# TOU tier limits are shared across the month's total energy and prorated
# across periods, matching NREL's utilityrate5 reference implementation.
tou_period_1 <- tier_df(c(500, Inf), c(0.10, 0.20))
tou_period_2 <- tier_df(c(500, Inf), c(0.20, 0.40))
tou_charge <- elpt_energy_charge_breakdown(
  c(600, 400),
  list(tou_period_1, tou_period_2),
  billing_demand_kw = 0
)
expect_equal(tou_charge$total_cost, 210, label = "TOU tier charge")

# Demand tiers are progressive too.
demand_tiers <- tier_df(c(30, Inf), c(0, 2), unit = "kW")
expect_equal(
  elpt_progressive_charge(40, demand_tiers),
  20,
  label = "progressive demand charge"
)

# End-to-end monthly bill for 15-minute interval energy. Ten kWh over a
# 15-minute interval corresponds to 40 kW billing demand.
zero_schedule <- matrix(0L, nrow = 12, ncol = 24)
simple_rates <- list(
  e_sched = zero_schedule,
  w_sched = zero_schedule,
  e_tiers = list(tier_df(Inf, 0.10)),
  d_sched = zero_schedule,
  dw_sched = zero_schedule,
  d_tiers = list(tier_df(Inf, 0, unit = "kW")),
  flat_tiers = list(demand_tiers),
  flat_months = rep(0L, 12),
  fixed_charge = 5,
  fixed_charge_units = "$/month"
)
interval_data <- data.frame(
  datetime = as.POSIXct(c("2025-01-06 00:00:00", "2025-01-06 00:15:00"), tz = "UTC"),
  Load = c(10, 10)
)
monthly_bill <- calculate_tou_bill(interval_data, "Load", simple_rates)
expect_equal(monthly_bill$Usage_Cost, 7, label = "monthly usage and fixed charge")
expect_equal(monthly_bill$Demand_Cost, 20, label = "15-minute billing demand")

# Hourly Cost plots only the per-interval energy charge, including when the
# monthly bill also has a fixed fee and demand charges.
interval_costs <- calculate_interval_energy_costs(interval_data, "Load", simple_rates)
expect_equal(interval_costs$energy_cost, c(1, 1), label = "15-minute energy cost")
expect_equal(sum(interval_costs$energy_cost), monthly_bill$Usage_Cost - 5,
             label = "interval cost reconciles to usage excluding fixed fee")

# Shifting 100 kWh from the expensive TOU period to the cheaper period lowers
# cost while preserving energy. Tier prices are allocated separately by scenario.
plot_rates <- simple_rates
plot_rates$e_tiers <- list(tou_period_1, tou_period_2)
plot_rates$e_sched[, 2] <- 1L
plot_rates$w_sched <- plot_rates$e_sched
plot_loads <- data.frame(
  datetime = as.POSIXct(c("2025-01-06 00:00:00", "2025-01-06 01:00:00"), tz = "UTC"),
  Load = c(600, 400), mod_load = c(700, 300)
)
plot_baseline <- calculate_interval_energy_costs(plot_loads, "Load", plot_rates)
plot_modified <- calculate_interval_energy_costs(plot_loads, "mod_load", plot_rates)
expect_equal(plot_baseline$energy_cost, c(90, 120), label = "baseline interval TOU costs")
expect_equal(plot_modified$energy_cost, c(105, 90), label = "shifted interval TOU costs")
expect_equal(sum(plot_baseline$energy_cost) - sum(plot_modified$energy_cost), 15,
             label = "shift savings from hourly energy costs")

daily_rates <- simple_rates
daily_rates$fixed_charge <- 3
daily_rates$fixed_charge_units <- "$/day"
two_day_data <- data.frame(
  datetime = as.POSIXct(c(
    "2025-01-06 00:00:00", "2025-01-06 00:15:00",
    "2025-01-07 00:00:00", "2025-01-07 00:15:00"
  ), tz = "UTC"),
  Load = c(10, 10, 10, 10)
)
daily_bill <- calculate_tou_bill(two_day_data, "Load", daily_rates)
expect_equal(daily_bill$Usage_Cost, 10, label = "daily fixed charge")

# Dynamic prices and unsupported apparent-power tiers must fail closed.
expect_unsupported(
  translate_urdb_to_elpt(data.frame(Rate_Name = "RTDPLMI RTP DAY AHEAD")),
  "dynamic RTP rate"
)
expect_unsupported(
  translate_urdb_to_elpt(data.frame(
    Rate_Name = "Industrial Service",
    Rate_Description = "Prices are published through day-ahead hourly pricing."
  )),
  "dynamic rate identified from its description"
)
expect_unsupported(
  elpt_validate_energy_units(list(tier_df(Inf, 0.10, unit = "kWh/kVA"))),
  "kWh/kVA rate"
)

cat("All ELPT rate calculation tests passed.\n")
