# Exercise the actual Shiny outputs with existing installed packages only.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
app_dir <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))))
setwd(app_dir)
expressions <- parse("app.R")

# Fail instead of allowing app.R's legacy startup code to install dependencies.
eval(expressions[[1]]) # cran_packages
required <- c(cran_packages, "ggmacc", "testthat")
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) stop("Missing existing packages: ", paste(missing, collapse = ", "))
source("app.R")

fixture <- tempfile(fileext = ".xlsx")
times <- seq(as.POSIXct("2025-01-06 00:00:00", tz = "UTC"), by = "hour", length.out = 48)
openxlsx::write.xlsx(data.frame(Date_Time = times, `Load (in kW)` = 100, check.names = FALSE), fixture)

shiny::testServer(server, {
  session$setInputs(
    user_time_zone = "UTC", green_manual = "Custom Hourly Load",
    date_range = as.Date(c("2025-01-06", "2025-01-07")),
    state = "AL", emissions_type = "2025", perc_clean = 0,
    addshed_shift = "Add/Shed Load", work_on_weekends = "Yes",
    plot_button = 0, plot_button_2 = 0
  )
  session$setInputs(loadpf_file = list(datapath = fixture, name = "test.xlsx"))
  session$setInputs(utility_selector = "Georgia Power Co")
  db <- urdb_data()
  rate_name <- db$Rate_Name[db$Utility_Name == "Georgia Power Co" & grepl("PLM-18", db$Rate_Name)][[1]]
  session$setInputs(rate_selector = rate_name)
  stopifnot(!is.null(rate_vals()))

  baseline <- hourly_cost_data()
  stopifnot(nrow(baseline) == 48, !"modified_energy_cost" %in% names(baseline))
  chart <- jsonlite::fromJSON(output$hourly_cost_plot, simplifyVector = FALSE)
  stopifnot(length(chart$x$data) == 1,
            chart$x$data[[1]]$name == "Baseline Energy Cost")

  window <- as.numeric(as.POSIXct(c("2025-01-06 12:00", "2025-01-06 14:00"), tz = "UTC"))
  session$setInputs(numeric_add_1 = 25, slider_to_add1 = window, plot_button_2 = 1)
  added <- hourly_cost_data()
  stopifnot(sum(added$modified_energy_cost) > sum(added$baseline_energy_cost))
  chart <- jsonlite::fromJSON(output$hourly_cost_plot, simplifyVector = FALSE)
  stopifnot(length(chart$x$data) == 2,
            chart$x$data[[1]]$name == "Modified Energy Cost",
            chart$x$data[[2]]$name == "Baseline Energy Cost",
            chart$x$data[[1]]$line$dash == "dash")

  session$setInputs(numeric_add_1 = -25, plot_button_2 = 2)
  shed <- hourly_cost_data()
  stopifnot(sum(shed$modified_energy_cost) < sum(shed$baseline_energy_cost))

  session$setInputs(
    addshed_shift = "Shift Load", numeric_1 = 25,
    slider_from1 = window,
    slider_to1 = as.numeric(as.POSIXct(c("2025-01-06 17:00", "2025-01-06 19:00"), tz = "UTC")),
    plot_button = 1
  )
  shifted <- hourly_cost_data()
  stopifnot(nrow(shifted) == 48, all(is.finite(shifted$modified_energy_cost)))
  stopifnot(sum(modified_load_data()$Load) == sum(modified_load_data()$mod_load))
  expected <- calculate_interval_energy_costs(modified_load_data(), "mod_load", rate_vals())
  stopifnot(isTRUE(all.equal(shifted$modified_energy_cost, expected$energy_cost)))

  # The per-chart XLSX contains exactly the plotted scenario values.
  exported <- openxlsx::read.xlsx(output$download_hourly_cost_data)
  stopifnot(nrow(exported) == 48, ncol(exported) == 3,
            isTRUE(all.equal(exported[[3]], shifted$modified_energy_cost)))

  # A removed/cleared tariff must never leave a previous tariff in effect.
  session$setInputs(rate_selector = "")
  stopifnot(is.null(rate_vals()))
})
unlink(fixture)
cat("Hourly Cost Shiny integration passed: baseline, add, shed, shift, XLSX, and cleared rate.\n")
