# Verify the app wires all six chart variants to the shared browser exporter.
# This checks parsed R expressions without loading or installing app packages.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
app_dir <- dirname(dirname(normalizePath(script)))
code <- parse(file.path(app_dir, "app.R"), encoding = "UTF-8")
helper <- parse(file.path(app_dir, "image_export.R"), encoding = "UTF-8")
calls <- list()
walk <- function(expr) {
  if (missing(expr)) return(invisible(NULL))
  if (is.call(expr)) calls[[length(calls) + 1L]] <<- expr
  if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
    for (node in as.list(expr)) walk(node)
  }
}
walk(code)
named_calls <- function(name) Filter(function(x) identical(x[[1L]], as.name(name)), calls)
buttons <- vapply(named_calls("fst_png_button"), function(x) x[[2L]], character(1))
stopifnot(setequal(buttons, c("downloadPNG", "downloadPNG_e", "downloadPNG_ec")))
cameras <- vapply(named_calls("fst_png_modebar"), function(x) x[[2L]], character(1))
stopifnot(setequal(cameras, c("Energy_Intensity", "Energy_Cost_Intensity", "Emissions_Intensity")))
configs <- named_calls("config")
stopifnot(length(configs) == 3L)
for (config in configs) {
  stopifnot("toImage" %in% unlist(as.list(config$modeBarButtonsToRemove)[-1L]))
  stopifnot(!is.null(config$modeBarButtonsToAdd))
}
legacy <- paste(deparse(code), collapse = "\n")
stopifnot(!grepl("webshot2|library\\(chromote\\)|saveWidget", legacy))
stopifnot(file.exists(file.path(app_dir, "www/fst-export.js")))
stopifnot(file.exists(file.path(app_dir, "www/fst-sankey-layout.js")))
sizes <- Filter(function(x) is.character(x[[2L]]) && x[[2L]] %in% c("height", "height_e", "height_ec", "width", "width_e", "width_ec"),
                named_calls("numericInput"))
stopifnot(length(sizes) == 6L)
for (size in sizes) stopifnot(size[[4L]] == if (startsWith(size[[2L]], "height")) 500 else 1000)
cat("PASS: all Sankey buttons and intensity cameras use the browser exporter; no server screenshot path remains.\n")
