# Utility-rate parsing and billing helpers for ELPT.
#
# These functions intentionally use base R so they can be regression-tested
# without installing any packages. URDB schedules are zero-indexed, while R
# list and matrix indices are one-indexed.

elpt_unsupported_rate <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("elpt_unsupported_rate", "error", "condition")
  ))
}

elpt_rate_field <- function(rate_row, field, default = NULL, unwrap = TRUE) {
  value <- rate_row[[field]]
  if (is.null(value) || length(value) == 0 ||
      (!is.list(value) && all(is.na(value)))) return(default)

  if (unwrap && is.list(value) && length(value) == 1) {
    return(value[[1]])
  }

  value
}

elpt_scalar_field <- function(rate_row, field, default = NA_character_) {
  value <- elpt_rate_field(rate_row, field, default = default, unwrap = FALSE)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (length(value) == 0 || is.na(value[[1]])) return(default)
  value[[1]]
}

elpt_rate_is_dynamic <- function(rate_row) {
  descriptive_fields <- c(
    "Rate_Name", "Rate_Description", "Description",
    "Basic_Information_Comments", "Energy_Comments", "Service_Type"
  )
  rate_text <- paste(
    vapply(
      descriptive_fields,
      function(field) as.character(elpt_scalar_field(rate_row, field, "")),
      character(1)
    ),
    collapse = " "
  )

  dynamic_pattern <- paste(
    c(
      "real[- ]?time", "day[- ]?ahead", "hourly[ -]?pricing",
      "market[- ]?based[ -]?pricing", "dynamic[ -]?pricing",
      "\\bRTP(?:[-A-Z0-9]*)?\\b", "\\bRTD(?:[-A-Z0-9]*)?\\b"
    ),
    collapse = "|"
  )

  grepl(dynamic_pattern, rate_text, ignore.case = TRUE, perl = TRUE)
}

elpt_normalize_schedule <- function(schedule, label, default_period = 0L) {
  if (is.null(schedule) || length(schedule) == 0) {
    return(matrix(default_period, nrow = 12, ncol = 24))
  }

  schedule <- as.matrix(schedule)
  if (!identical(dim(schedule), c(12L, 24L))) {
    elpt_unsupported_rate(paste0(
      label, " must contain a 12-by-24 month/hour schedule."
    ))
  }

  suppressWarnings(storage.mode(schedule) <- "integer")
  if (anyNA(schedule) || any(schedule < 0)) {
    elpt_unsupported_rate(paste0(label, " contains invalid period numbers."))
  }
  schedule
}

elpt_normalize_tier_period <- function(period, energy = FALSE) {
  if (is.null(period) || length(period) == 0) {
    return(data.frame(
      max = Inf,
      unit = if (energy) "kWh" else "kW",
      rate = 0,
      adj = 0,
      stringsAsFactors = FALSE
    ))
  }

  if (is.data.frame(period)) {
    tier_df <- period
  } else if (is.list(period) && !is.null(names(period)) && "rate" %in% names(period)) {
    tier_df <- as.data.frame(period, stringsAsFactors = FALSE)
  } else if (is.list(period)) {
    rows <- lapply(period, function(tier) {
      if (is.data.frame(tier)) return(tier)
      as.data.frame(tier, stringsAsFactors = FALSE)
    })
    tier_df <- do.call(rbind, rows)
  } else {
    elpt_unsupported_rate("A rate tier could not be parsed from the URDB record.")
  }

  if (!"rate" %in% names(tier_df)) tier_df$rate <- 0
  if (!"max" %in% names(tier_df)) tier_df$max <- NA_real_
  if (!"unit" %in% names(tier_df)) tier_df$unit <- if (energy) "kWh" else "kW"
  if (!"adj" %in% names(tier_df)) tier_df$adj <- 0

  max_value <- suppressWarnings(as.numeric(tier_df$max))
  max_value[is.na(max_value)] <- Inf
  rate_value <- suppressWarnings(as.numeric(tier_df$rate))
  rate_value[is.na(rate_value)] <- 0
  adjustment <- suppressWarnings(as.numeric(tier_df$adj))
  adjustment[is.na(adjustment)] <- 0
  unit_value <- trimws(as.character(tier_df$unit))
  unit_value[is.na(unit_value) | !nzchar(unit_value)] <- if (energy) "kWh" else "kW"

  data.frame(
    max = max_value,
    unit = unit_value,
    rate = rate_value,
    adj = adjustment,
    stringsAsFactors = FALSE
  )
}

elpt_normalize_rate_structure <- function(structure, energy = FALSE) {
  if (is.null(structure) || length(structure) == 0) return(list())

  if (is.data.frame(structure)) {
    structure <- list(structure)
  } else if (!is.list(structure)) {
    elpt_unsupported_rate("A URDB rate structure could not be parsed.")
  } else if (!is.null(names(structure)) && "rate" %in% names(structure)) {
    structure <- list(structure)
  }

  lapply(structure, elpt_normalize_tier_period, energy = energy)
}

elpt_validate_energy_units <- function(energy_tiers) {
  units <- unique(tolower(unlist(lapply(energy_tiers, `[[`, "unit"))))
  unsupported <- setdiff(units, c("kwh", "kwh/kw"))
  if (length(unsupported) > 0) {
    elpt_unsupported_rate(paste0(
      "ELPT cannot calculate energy tiers expressed in ",
      paste(sort(unique(unsupported)), collapse = ", "),
      ". Only kWh and kWh/kW tiers are currently supported."
    ))
  }

  has_demand_linked_tiers <- any(units == "kwh/kw")
  if (has_demand_linked_tiers) {
    first_units <- vapply(
      energy_tiers,
      function(period) tolower(period$unit[[1]]),
      character(1)
    )
    if (any(first_units != "kwh/kw")) {
      elpt_unsupported_rate(paste0(
        "This kWh/kW rate does not begin with a demand-linked tier in every ",
        "energy period, so its block boundaries cannot be calculated safely."
      ))
    }
  }

  invisible(TRUE)
}

elpt_default_rate_period <- function(unit) {
  data.frame(max = Inf, unit = unit, rate = 0, adj = 0, stringsAsFactors = FALSE)
}

translate_urdb_to_elpt <- function(rate_row) {
  rate_name <- as.character(elpt_scalar_field(rate_row, "Rate_Name", "Selected rate"))
  if (elpt_rate_is_dynamic(rate_row)) {
    elpt_unsupported_rate(paste0(
      rate_name,
      " uses dynamic, real-time, or day-ahead pricing. ELPT supports only ",
      "rates whose prices are known in advance."
    ))
  }

  energy_tiers <- elpt_normalize_rate_structure(
    elpt_rate_field(rate_row, "Energy_Rate_Structure"),
    energy = TRUE
  )
  if (length(energy_tiers) == 0) {
    elpt_unsupported_rate("The selected rate has no usable energy-rate structure.")
  }
  elpt_validate_energy_units(energy_tiers)

  demand_tiers <- elpt_normalize_rate_structure(
    elpt_rate_field(rate_row, "Demand_Rate_Structure"),
    energy = FALSE
  )
  flat_demand_tiers <- elpt_normalize_rate_structure(
    elpt_rate_field(rate_row, "FlatDemandStructure"),
    energy = FALSE
  )

  if (length(demand_tiers) == 0) demand_tiers <- list(elpt_default_rate_period("kW"))
  if (length(flat_demand_tiers) == 0) flat_demand_tiers <- list(elpt_default_rate_period("kW"))

  energy_weekday_schedule <- elpt_normalize_schedule(
    elpt_rate_field(rate_row, "Energy_Weekday_Schedule"),
    "The energy weekday schedule"
  )
  energy_weekend_schedule <- elpt_normalize_schedule(
    elpt_rate_field(rate_row, "Energy_Weekend_Schedule"),
    "The energy weekend schedule"
  )

  has_demand_schedule <- !is.null(elpt_rate_field(rate_row, "Demand_Rate_Structure"))
  demand_weekday_schedule <- elpt_normalize_schedule(
    if (has_demand_schedule) elpt_rate_field(rate_row, "Demand_Weekday_Schedule") else NULL,
    "The demand weekday schedule"
  )
  demand_weekend_schedule <- elpt_normalize_schedule(
    if (has_demand_schedule) elpt_rate_field(rate_row, "Demand_Weekend_Schedule") else NULL,
    "The demand weekend schedule"
  )

  flat_demand_months <- elpt_rate_field(rate_row, "FlatDemandMonths", rep(0L, 12))
  if (is.null(flat_demand_months) || length(flat_demand_months) == 0) {
    flat_demand_months <- rep(0L, 12)
  }
  flat_demand_months <- suppressWarnings(as.integer(flat_demand_months))
  if (length(flat_demand_months) != 12 || anyNA(flat_demand_months) || any(flat_demand_months < 0)) {
    elpt_unsupported_rate("The monthly demand schedule must contain 12 valid period numbers.")
  }

  if (any(c(energy_weekday_schedule, energy_weekend_schedule) >= length(energy_tiers))) {
    elpt_unsupported_rate("The energy schedule refers to an undefined rate period.")
  }
  if (has_demand_schedule &&
      any(c(demand_weekday_schedule, demand_weekend_schedule) >= length(demand_tiers))) {
    elpt_unsupported_rate("The demand schedule refers to an undefined rate period.")
  }
  if (any(flat_demand_months >= length(flat_demand_tiers))) {
    elpt_unsupported_rate("The monthly demand schedule refers to an undefined rate period.")
  }

  fixed_charge <- suppressWarnings(as.numeric(
    elpt_scalar_field(rate_row, "Fixed_Monthly_Charge", 0)
  ))
  if (is.na(fixed_charge)) fixed_charge <- 0
  fixed_charge_units <- tolower(trimws(as.character(
    elpt_scalar_field(rate_row, "Fixed_Charge_Units", "$/month")
  )))
  fixed_charge_units <- gsub("\\s+", "", fixed_charge_units)
  if (!nzchar(fixed_charge_units) || fixed_charge_units %in% c("na", "none")) {
    # Older ELPT databases stored a field explicitly named
    # Fixed_Monthly_Charge without retaining the URDB unit.
    fixed_charge_units <- "$/month"
  }
  if (fixed_charge_units %in% c("monthly", "month")) fixed_charge_units <- "$/month"
  if (fixed_charge_units %in% c("daily", "day")) fixed_charge_units <- "$/day"
  if (fixed_charge != 0 && !fixed_charge_units %in% c("$/month", "$/day")) {
    elpt_unsupported_rate(paste0(
      "ELPT cannot calculate the fixed charge unit '", fixed_charge_units, "'."
    ))
  }

  list(
    rate_name = rate_name,
    e_sched = energy_weekday_schedule,
    w_sched = energy_weekend_schedule,
    e_tiers = energy_tiers,
    # First-tier scalar arrays are retained for compatibility with older
    # server code; all billing uses the full tier lists above.
    e_rates = vapply(energy_tiers, function(period) period$rate[[1]], numeric(1)),
    d_sched = demand_weekday_schedule,
    dw_sched = demand_weekend_schedule,
    d_tiers = demand_tiers,
    d_rates = vapply(demand_tiers, function(period) period$rate[[1]], numeric(1)),
    flat_tiers = flat_demand_tiers,
    flat_rates = vapply(flat_demand_tiers, function(period) period$rate[[1]], numeric(1)),
    flat_months = flat_demand_months,
    fixed_charge = fixed_charge,
    fixed_charge_units = fixed_charge_units,
    is_tiered = any(vapply(energy_tiers, nrow, integer(1)) > 1),
    has_demand_linked_energy = any(vapply(
      energy_tiers,
      function(period) any(tolower(period$unit) == "kwh/kw"),
      logical(1)
    ))
  )
}

elpt_tier_schedules_match <- function(first_period, other_period) {
  if (nrow(first_period) != nrow(other_period)) return(FALSE)
  same_units <- identical(tolower(first_period$unit), tolower(other_period$unit))
  first_max <- first_period$max
  other_max <- other_period$max
  same_infinite <- is.infinite(first_max) == is.infinite(other_max)
  finite <- is.finite(first_max) & is.finite(other_max)
  same_finite <- rep(TRUE, length(first_max))
  same_finite[finite] <- abs(first_max[finite] - other_max[finite]) <=
    1e-7 * pmax(1, abs(first_max[finite]), abs(other_max[finite]))
  same_units && all(same_infinite) && all(same_finite)
}

elpt_resolve_energy_tiers <- function(period_tiers, billing_demand_kw) {
  if (length(period_tiers) == 0) {
    elpt_unsupported_rate("No energy periods were available for this billing month.")
  }

  first_period <- period_tiers[[1]]
  if (length(period_tiers) > 1) {
    matches <- vapply(
      period_tiers[-1],
      function(other_period) elpt_tier_schedules_match(first_period, other_period),
      logical(1)
    )
    if (!all(matches)) {
      elpt_unsupported_rate(paste0(
        "The selected rate uses incompatible tier boundaries across energy ",
        "periods active in the same month."
      ))
    }
  }

  units <- tolower(first_period$unit)
  if (!any(units == "kwh/kw")) {
    return(list(bounds = first_period$max, tier_indices = seq_len(nrow(first_period))))
  }

  if (!is.finite(billing_demand_kw) || billing_demand_kw < 0) {
    elpt_unsupported_rate("A valid monthly billing demand is required for kWh/kW tiers.")
  }

  demand_linked_bounds <- vapply(
    which(units == "kwh/kw"),
    function(index) {
      max_usage <- first_period$max[[index]]
      if (is.infinite(max_usage)) Inf else max_usage * billing_demand_kw
    },
    numeric(1)
  )

  block <- 1L
  resolved_bounds <- numeric()
  resolved_indices <- integer()
  tier_count <- nrow(first_period)

  for (tier_index in seq_len(tier_count)) {
    if (units[[tier_index]] == "kwh/kw") {
      original_max <- first_period$max[[tier_index]]
      original_bound <- if (is.infinite(original_max)) Inf else original_max * billing_demand_kw
      if (
        demand_linked_bounds[[block]] < original_bound &&
          block < length(demand_linked_bounds)
      ) {
        block <- block + 1L
      }

      next_is_demand_linked <- tier_index < tier_count &&
        units[[tier_index + 1L]] == "kwh/kw"
      if (next_is_demand_linked || tier_index == tier_count) {
        resolved_bounds <- c(resolved_bounds, demand_linked_bounds[[block]])
        resolved_indices <- c(resolved_indices, tier_index)
      }
    } else {
      fixed_bound <- first_period$max[[tier_index]]
      block_bound <- demand_linked_bounds[[block]]
      if (fixed_bound < block_bound) {
        resolved_bounds <- c(resolved_bounds, fixed_bound)
        resolved_indices <- c(resolved_indices, tier_index)
      } else if (
        length(resolved_bounds) == 0 ||
          tail(resolved_bounds, 1) < block_bound
      ) {
        resolved_bounds <- c(resolved_bounds, block_bound)
        resolved_indices <- c(resolved_indices, tier_index)
      }
    }
  }

  if (length(resolved_bounds) == 0 || any(diff(resolved_bounds) <= 0)) {
    elpt_unsupported_rate("The demand-linked energy tier boundaries could not be resolved safely.")
  }

  list(bounds = resolved_bounds, tier_indices = resolved_indices)
}

elpt_energy_charge_breakdown <- function(period_usage_kwh, energy_tiers, billing_demand_kw) {
  period_usage_kwh <- as.numeric(period_usage_kwh)
  if (length(period_usage_kwh) != length(energy_tiers)) {
    elpt_unsupported_rate("Energy-period usage did not match the selected rate structure.")
  }
  if (any(!is.finite(period_usage_kwh)) || any(period_usage_kwh < 0)) {
    elpt_unsupported_rate("Energy use must be finite and non-negative.")
  }

  period_cost <- rep(0, length(period_usage_kwh))
  effective_rate <- rep(0, length(period_usage_kwh))
  used_periods <- which(period_usage_kwh > 0)
  total_usage <- sum(period_usage_kwh)
  if (total_usage <= 0 || length(used_periods) == 0) {
    return(list(
      total_cost = 0,
      period_cost = period_cost,
      effective_rate = effective_rate,
      bounds = numeric(),
      tier_indices = integer()
    ))
  }

  resolved <- elpt_resolve_energy_tiers(
    energy_tiers[used_periods],
    billing_demand_kw
  )
  bounds <- resolved$bounds
  tier_indices <- resolved$tier_indices
  if (is.finite(tail(bounds, 1)) && total_usage > tail(bounds, 1)) {
    elpt_unsupported_rate(paste0(
      "Monthly energy use exceeds the maximum usage represented by this rate."
    ))
  }

  shares <- period_usage_kwh[used_periods] / total_usage
  lower_bound <- 0
  for (resolved_index in seq_along(bounds)) {
    upper_bound <- bounds[[resolved_index]]
    tier_usage <- max(0, min(total_usage, upper_bound) - lower_bound)
    if (tier_usage > 0) {
      original_tier <- tier_indices[[resolved_index]]
      for (share_index in seq_along(used_periods)) {
        period_index <- used_periods[[share_index]]
        allocated_usage <- tier_usage * shares[[share_index]]
        rate <- energy_tiers[[period_index]]$rate[[original_tier]]
        period_cost[[period_index]] <- period_cost[[period_index]] + allocated_usage * rate
      }
    }
    if (total_usage <= upper_bound) break
    lower_bound <- upper_bound
  }

  effective_rate[used_periods] <-
    period_cost[used_periods] / period_usage_kwh[used_periods]

  list(
    total_cost = sum(period_cost),
    period_cost = period_cost,
    effective_rate = effective_rate,
    bounds = bounds,
    tier_indices = tier_indices
  )
}

elpt_progressive_charge <- function(quantity, tiers) {
  quantity <- as.numeric(quantity)
  if (length(quantity) != 1 || is.na(quantity) || quantity <= 0) return(0)
  if (!is.finite(quantity)) elpt_unsupported_rate("Billing quantity must be finite.")

  lower_bound <- 0
  charge <- 0
  for (tier_index in seq_len(nrow(tiers))) {
    upper_bound <- tiers$max[[tier_index]]
    tier_quantity <- max(0, min(quantity, upper_bound) - lower_bound)
    charge <- charge + tier_quantity * tiers$rate[[tier_index]]
    if (quantity <= upper_bound) return(charge)
    lower_bound <- upper_bound
  }

  elpt_unsupported_rate("Billing quantity exceeds the maximum represented by this rate.")
}

elpt_infer_interval_hours <- function(datetime) {
  if (length(datetime) < 2) return(1)
  timestamp <- as.numeric(as.POSIXct(datetime))
  differences <- diff(sort(unique(timestamp))) / 3600
  differences <- differences[is.finite(differences) & differences > 0 & differences <= 24]
  if (length(differences) == 0) return(1)
  as.numeric(stats::median(differences))
}

elpt_month_start <- function(datetime) {
  as.Date(format(datetime, "%Y-%m-01"))
}

elpt_schedule_period <- function(datetime, weekday_schedule, weekend_schedule) {
  month_index <- as.integer(format(datetime, "%m"))
  hour_index <- as.integer(format(datetime, "%H")) + 1L
  weekend <- as.POSIXlt(datetime)$wday %in% c(0L, 6L)
  period <- weekday_schedule[cbind(month_index, hour_index)]
  period[weekend] <- weekend_schedule[cbind(month_index[weekend], hour_index[weekend])]
  as.integer(period)
}

calculate_interval_energy_costs <- function(df, load_col, rates) {
  if (!is.data.frame(df) || !all(c("datetime", load_col) %in% names(df))) {
    stop("The load data must contain datetime and the requested load column.")
  }

  result <- df
  result$month_start <- elpt_month_start(result$datetime)
  result$month_idx <- as.integer(format(result$datetime, "%m"))
  result$active_energy_kwh <- suppressWarnings(as.numeric(result[[load_col]]))
  interval_hours <- elpt_infer_interval_hours(result$datetime)
  result$active_power_kw <- result$active_energy_kwh / interval_hours
  result$e_period <- elpt_schedule_period(result$datetime, rates$e_sched, rates$w_sched)
  result$d_period <- elpt_schedule_period(result$datetime, rates$d_sched, rates$dw_sched)

  if (anyNA(result$active_energy_kwh) || any(result$active_energy_kwh < 0)) {
    stop("Load values must be non-negative numbers.")
  }
  if (any(result$e_period + 1L > length(rates$e_tiers))) {
    elpt_unsupported_rate("The energy schedule refers to an undefined rate period.")
  }
  if (any(result$d_period + 1L > length(rates$d_tiers))) {
    elpt_unsupported_rate("The demand schedule refers to an undefined rate period.")
  }

  result$effective_energy_rate <- 0
  result$energy_cost <- 0
  result$billing_demand_kw <- 0

  month_groups <- split(seq_len(nrow(result)), result$month_start)
  for (indices in month_groups) {
    billing_demand_kw <- max(result$active_power_kw[indices], na.rm = TRUE)
    period_usage <- numeric(length(rates$e_tiers))
    usage_by_period <- tapply(
      result$active_energy_kwh[indices],
      result$e_period[indices] + 1L,
      sum
    )
    period_usage[as.integer(names(usage_by_period))] <- as.numeric(usage_by_period)

    breakdown <- elpt_energy_charge_breakdown(
      period_usage,
      rates$e_tiers,
      billing_demand_kw
    )
    effective_rate <- breakdown$effective_rate[result$e_period[indices] + 1L]
    result$effective_energy_rate[indices] <- effective_rate
    result$energy_cost[indices] <- result$active_energy_kwh[indices] * effective_rate
    result$billing_demand_kw[indices] <- billing_demand_kw
  }

  result
}

calculate_tou_bill <- function(df, load_col, rates) {
  interval_costs <- calculate_interval_energy_costs(df, load_col, rates)
  month_groups <- split(seq_len(nrow(interval_costs)), interval_costs$month_start)

  monthly_rows <- lapply(month_groups, function(indices) {
    month_index <- interval_costs$month_idx[[indices[[1]]]]
    monthly_max_kw <- max(interval_costs$active_power_kw[indices], na.rm = TRUE)

    demand_cost <- 0
    demand_periods <- sort(unique(interval_costs$d_period[indices]))
    for (period in demand_periods) {
      period_indices <- indices[interval_costs$d_period[indices] == period]
      period_peak <- max(interval_costs$active_power_kw[period_indices], na.rm = TRUE)
      demand_cost <- demand_cost + elpt_progressive_charge(
        period_peak,
        rates$d_tiers[[period + 1L]]
      )
    }

    flat_period <- rates$flat_months[[month_index]]
    if (flat_period + 1L > length(rates$flat_tiers)) {
      elpt_unsupported_rate("The monthly demand schedule refers to an undefined rate period.")
    }
    flat_demand_cost <- elpt_progressive_charge(
      monthly_max_kw,
      rates$flat_tiers[[flat_period + 1L]]
    )

    fixed_charge_units <- rates$fixed_charge_units
    if (is.null(fixed_charge_units) || !nzchar(fixed_charge_units)) {
      fixed_charge_units <- "$/month"
    }
    fixed_charge <- rates$fixed_charge
    if (identical(fixed_charge_units, "$/day")) {
      fixed_charge <- fixed_charge * length(unique(as.Date(
        interval_costs$datetime[indices]
      )))
    }

    data.frame(
      month_start = interval_costs$month_start[[indices[[1]]]],
      Usage_Cost = sum(interval_costs$energy_cost[indices]) + fixed_charge,
      Demand_Cost = demand_cost + flat_demand_cost,
      stringsAsFactors = FALSE
    )
  })

  monthly_summary <- do.call(rbind, monthly_rows)
  rownames(monthly_summary) <- NULL
  monthly_summary[order(monthly_summary$month_start), , drop = FALSE]
}
