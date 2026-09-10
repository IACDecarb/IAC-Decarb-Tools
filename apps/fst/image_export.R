# Browser exports use the rendered chart and the hosts' existing image policy.
fst_png_button <- function(id) {
  tags$button(
    id = id, type = "button", class = "btn btn-default", `data-fst-export` = "sankey",
    icon("download"), "Click Here to Download plot as Image"
  )
}

fst_png_modebar <- function(filename) {
  list(
    name = "Download plot as a PNG", title = "Download plot as a PNG",
    icon = htmlwidgets::JS("Plotly.Icons.camera"),
    click = htmlwidgets::JS(paste0(
      "function(gd) { return window.FSTExport.downloadPlotly(gd, ",
      jsonlite::toJSON(filename, auto_unbox = TRUE), "); }"
    ))
  )
}
