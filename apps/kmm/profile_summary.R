# Describe only the retained clustering results; no operational diagnoses.
kmm_results_summary <- function(df, unit) {
  days <- unique(df[c("Date", "Cluster")])
  counts <- table(droplevels(factor(days$Cluster)))
  n_days <- nrow(days)
  k <- length(counts)
  fmt <- function(x) format(round(x, 1), trim = TRUE, big.mark = ",")
  leading <- names(counts)[counts == max(counts)]
  opening <- sprintf("%s daily %s identified across %s analyzed %s.",
                     k, if (k == 1) "pattern was" else "patterns were",
                     format(n_days, big.mark = ","), if (n_days == 1) "day" else "days")
  frequency <- if (k == 1) {
    "All analyzed days belong to Cluster 1."
  } else if (length(leading) == 1) {
    sprintf("Cluster %s is the most common, representing %s days (%s%% of analyzed days).",
            leading, max(counts), fmt(100 * max(counts) / n_days))
  } else {
    sprintf("Clusters %s are equally common, each representing %s days (%s%% of analyzed days).",
            paste(leading, collapse = ", "), max(counts), fmt(100 * max(counts) / n_days))
  }
  # Describe a peak only for a uniquely most common cluster and unique peak.
  peak <- ""
  if (length(leading) == 1) {
    profile <- aggregate(Load ~ Hour, df[as.character(df$Cluster) == leading, ], mean)
    peak_hours <- profile$Hour[abs(profile$Load - max(profile$Load)) < 1e-8]
    if (length(peak_hours) == 1) {
      hour_12 <- if (peak_hours %% 12 == 0) 12 else peak_hours %% 12
      period <- if (peak_hours < 12) "AM" else "PM"
      peak <- sprintf("Cluster %s's typical daily profile peaks at %d %s.", leading, hour_12, period)
    } else if (length(peak_hours) == nrow(profile)) {
      peak <- sprintf("Cluster %s's typical daily profile is flat across the day.", leading)
    } else {
      peak <- sprintf("Cluster %s's typical daily profile shares its highest value across multiple hours.", leading)
    }
  }
  chart_guide <- "The colored profiles show the typical daily pattern for each cluster. The heatmap shows which days followed each pattern."
  sentences <- c(opening, frequency, peak, chart_guide)
  paste(sentences[nzchar(sentences)], collapse = " ")
}
