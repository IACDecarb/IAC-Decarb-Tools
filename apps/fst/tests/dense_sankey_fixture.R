# Synthetic dense facility, patterned on long equipment labels and tightly
# packed small flows. Values are test data, not facility calculations.
fst_dense_sankey <- function(kind = "energy", savings = FALSE) {
  equipment <- c("Calor Dryer", "Tunnel Dryer", "Downstream Tunnel Dryer 11",
    "DTD Unit 1 12 +13", "DTD Unit 2 12 +13", "Flexo Press with Inline Cutter",
    "Flexo Printing Press", "Folder Gluers", "Air Compressors", "Air Cooled Chillers",
    "Water cooled Chillers", "Balers", "HVAC Pumps", "Stand Alone Platen Cutters",
    "Large HVAC Air Handlers", "Scrap Fans")
  values <- c(12116.4, 12116.4, 3926.6, 6731.3, 6731.3, 30186.2, 15030.5,
    9394.1, 10371, 1440, 19728, 2104, 1879, 150, 8764, 6889)
  values <- values * switch(kind, energy = 1, cost = 15, emissions = 0.07)
  total <- switch(kind, energy = "Total Energy", cost = "Total Energy Costs", emissions = "Total Emissions")
  names <- c(total, if (kind == "emissions") "Energy", "Fuel", "Electricity", "Natural Gas", equipment)
  if (savings) names <- c("Total Baseline", names, "Saved")
  links <- data.frame(source = character(), target = character(), value = numeric())
  add <- function(source, target, value) {
    links <<- rbind(links, data.frame(source = source, target = target, value = value))
  }
  if (savings) {
    add("Total Baseline", total, sum(values) * 0.85)
    add("Total Baseline", "Saved", sum(values) * 0.15)
    values <- values * 0.85
  }
  source <- total
  if (kind == "emissions") { add(total, "Energy", sum(values)); source <- "Energy" }
  add(source, "Fuel", sum(values[1:5]))
  add(source, "Electricity", sum(values[6:16]))
  add("Fuel", "Natural Gas", sum(values[1:5]))
  for (i in seq_along(equipment)) add(if (i <= 5) "Natural Gas" else "Electricity", equipment[i], values[i])
  links$group <- links$source
  links$source <- match(links$source, names) - 1L
  links$target <- match(links$target, names) - 1L
  widget <- networkD3::sankeyNetwork(Links = links, Nodes = data.frame(name = names),
    Source = "source", Target = "target", Value = "value", NodeID = "name", LinkGroup = "group",
    fontSize = 14, nodeWidth = 30, sinksRight = FALSE, iterations = 10,
    colourScale = htmlwidgets::JS('d3.scaleOrdinal().range(["#4f46a5", "#eceb52", "#8ccfba", "#d392bc"]);'))
  htmlwidgets::onRender(widget, 'function(el) {
    d3.select(el).selectAll(".node text").text(function(d) {
      return d.name + " (" + d.value.toLocaleString(undefined, {maximumFractionDigits:1}) + ")";
    });
  }')
}

fst_dense_outputs <- function(output) {
  for (kind in c("energy", "cost", "emissions")) for (savings in c(FALSE, TRUE)) local({
    k <- kind; saved <- savings
    suffix <- if (saved) "_new" else ""
    plot_id <- paste0(switch(k, energy = "sankey_energy", cost = "sankey_energy_costs", emissions = "sankey"), suffix)
    diagram_id <- paste0(switch(k, energy = "diagram_energy", cost = "diagram_energy_costs", emissions = "diagram"), suffix)
    output[[plot_id]] <- networkD3::renderSankeyNetwork(fst_dense_sankey(k, saved))
    output[[diagram_id]] <- shiny::renderUI(networkD3::sankeyNetworkOutput(plot_id, height = "1000px"))
  })
}
