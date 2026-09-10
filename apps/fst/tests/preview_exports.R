# Local browser regression fixture. No packages are installed.
# Rscript tests/preview_exports.R [app directory] [port] [PNG output directory] [dense]
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
app_dir <- if (length(args)) normalizePath(args[1]) else dirname(dirname(normalizePath(script)))
port <- if (length(args) >= 2) as.integer(args[2]) else 8771L
output_dir <- if (length(args) >= 3) normalizePath(args[3], mustWork = FALSE) else file.path(tempdir(), "fst-export-checks")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8")
setwd(app_dir)
app_env <- new.env(parent = globalenv())
invisible(sys.source("app.R", envir = app_env))
shiny::addResourcePath("fst-test-assets", file.path(app_dir, "www"))
fixture_assets <- function(tag) {
  if (inherits(tag, "shiny.tag")) {
    if (!is.null(tag$attribs$src) && tag$attribs$src %in% c("fst-export.js", "fst-sankey-layout.js", "lbnl.png", "ucdavis_logo_gold.png")) {
      tag$attribs$src <- paste0("fst-test-assets/", tag$attribs$src)
    }
    tag$children <- lapply(tag$children, fixture_assets)
  } else if (is.list(tag)) {
    tag[] <- lapply(tag, fixture_assets)
  }
  tag
}
app_env$ui <- fixture_assets(app_env$ui)
dense <- length(args) >= 4 && args[4] == "dense"
if (dense) source("tests/dense_sankey_fixture.R")
fixture <- data.frame(
  name = "Facility Sankey Tool - Input Sheet.xlsx", type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  size = file.info("Facility Sankey Tool - Input Sheet.xlsx")$size,
  datapath = normalizePath("Facility Sankey Tool - Input Sheet.xlsx")
)
`$.fst_fixture_input` <- function(x, name) {
  values <- unclass(x)
  if (name == "file") values$fixture else values$input[[name]]
}
`[[.fst_fixture_input` <- `$.fst_fixture_input`

capture_js <- "(function () {
  var create = URL.createObjectURL.bind(URL), count = 0;
  URL.createObjectURL = function (blob) {
    var url = create(blob);
    if (blob.type === 'image/png') {
      var reader = new FileReader(), sequence = ++count;
      reader.onload = function () {
        Shiny.setInputValue('fst_test_png', {data:reader.result, sequence:sequence}, {priority:'event'});
      };
      reader.readAsDataURL(blob);
    }
    return url;
  };
  document.addEventListener('click', function (event) {
    if (event.target.id !== 'test_original_png') return;
    var plot = ['enPlot','ecPlot','emPlot'].map(function (id) {return document.getElementById(id);})
      .find(function (node) {return node && node.getClientRects().length;});
    if (!plot) return;
    Plotly.toImage(plot, {format:'png'}).then(function () {
      Shiny.setInputValue('fst_test_original_result', 'Original Plotly PNG succeeded', {priority:'event'});
    }).catch(function (error) {
      Shiny.setInputValue('fst_test_original_result', 'Original Plotly PNG failed: ' + error, {priority:'event'});
    });
  });
}());"
ui <- shiny::tagList(
  shiny::tags$head(
    shiny::tags$meta(`http-equiv` = "Content-Security-Policy", content = "default-src 'self' https: data: 'unsafe-inline' 'unsafe-eval'"),
    shiny::tags$script(shiny::HTML(capture_js))
  ),
  shiny::div(style = "padding:12px;background:#fff3cd",
    if (dense) "LOCAL TEST: synthetic dense Sankey diagrams. Production CSP is enabled." else
      "LOCAL TEST: repository example workbook is preloaded. Production CSP is enabled.",
    shiny::verbatimTextOutput("fst_test_status"),
    shiny::actionButton("test_original_png", "Test original Plotly PNG path")),
  app_env$ui
)
server <- function(input, output, session) {
  proxy <- structure(list(input = input, fixture = fixture), class = "fst_fixture_input")
  app_env$server(proxy, output, session)
  if (dense) fst_dense_outputs(output)
  saved <- shiny::reactiveVal("No PNG captured yet")
  shiny::observeEvent(input$fst_test_png, {
    payload <- input$fst_test_png
    stopifnot(grepl("^data:image/png;base64,", payload$data), is.numeric(payload$sequence))
    path <- file.path(output_dir, sprintf("export-%03d.png", as.integer(payload$sequence)))
    png <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", payload$data))
    stopifnot(identical(as.integer(png[1:8]), c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)))
    writeBin(png, path)
    saved(paste("PNG captured:", path, "bytes:", length(png)))
  })
  output$fst_test_status <- shiny::renderText(paste(saved(), input$fst_test_original_result, sep = "\n"))
}
shiny::runApp(shiny::shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = FALSE)
