library(ggplot2)
library(openxlsx)
library(imputeTS)
library(stringi)
library(utils)
library(tools)
library(writexl)
library(shinyalert)
library(bslib)
library(plot.matrix)
library(readxl)
library(tidyverse)
library(colorspace)
library(viridisLite)
library(units)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(shinyTime)
library(shinythemes)
library(reactlog)
library(dplyr)
library(janitor)
library(data.table)
library(mmr)
library(shinyBS)
library(pals)
library(hrbrthemes)
library(ggthemes)
library(kableExtra)
library(shinydashboard)
library(plotly)
library(scales)
library(ggtext)
library(ggmacc)
library(openair)
library(htmltools)
library(htmlwidgets)
library(devtools)
library(roxygen2)
library(testthat)
library(xml2)
library(XML)
library(xmlconvert)
library(splitstackshape)
library(extrafont)
library(prompter)

library(conflicted)

#conflicted packages
conflicts_prefer(lubridate::year)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::hour)
conflicts_prefer(plotly::layout)
conflicts_prefer(dplyr::lag)
conflicts_prefer(lubridate::yday)
conflicts_prefer(lubridate::wday)

conflict_scout()

## Tweaking ggmacc START##

macc_prep <- function(data, mac, abatement) {
  data %>%
    arrange(month) %>%
    mutate(
      xmax = cumsum({{ abatement }}),
      xmin = lag(.data$xmax, default = 0),
      ymin = ifelse({{ mac }} < 0, {{ mac }}, 0),
      ymax = ifelse({{ mac }} > 0, {{ mac }}, 0)
    )
}


geom_macc <- function(fill = NULL, ...) {
  geom_rect(
    aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax,
      fill = {{ fill }}
    ),
    ...
  )
}


ggmacc <- function(data, mac, abatement, fill = NULL, cost_threshold = NULL,
                   zero_line = FALSE, threshold_line = FALSE, threshold_fade = 1) {
  if (zero_line == TRUE) {
    zero_hline <- geom_hline(yintercept = 0, lty = 1, colour = "black")
  } else {
    zero_hline <- NULL
  }
  
  
  if (threshold_line == TRUE) {
    if (is.null(cost_threshold)) abort("No cost threshold supplied.")
    cost_hline <- geom_hline(yintercept = cost_threshold, lty = 2, colour = "black")
  } else {
    cost_hline <- NULL
  }
  
  
  data <- data %>%
    macc_prep(mac = {{ mac }}, abatement = {{ abatement }})
  
  
  if (!is.null(cost_threshold)) {
    alpha <- ifelse(pull(data, {{ mac }}) >= cost_threshold, threshold_fade, 1)
  } else {
    alpha <- rep(1, nrow(data))
  }
  
  # plot
  data %>%
    ggplot() +
    geom_macc(fill = {{ fill }}, alpha = alpha) +
    zero_hline +
    cost_hline
}

### Tweaking ggmacc end


### Adding radio buttons with tool tips ###

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            var infoIcon = $('<span class=\"info-icon\">?</span>');
            infoIcon.tooltip($.extend(", options, ", {html: true}));
            $(this).parent().append(infoIcon);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

checkboxTooltip <- function(id, title, placement = "bottom", trigger = "hover", options = NULL){
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        var infoIcon = $('<span class=\"info-icon\">?</span>');
        infoIcon.tooltip($.extend(", options, ", {html: true}));
        $('#", id, " .control-label').append(infoIcon);
        $('#", id, " .info-icon').attr('data-original-title', '", title, "');
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

dateRangeTooltip <- function(id, title, tooltip_title, placement = "right", trigger = "hover", options = NULL) {
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(tooltip_title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        var inputLabel = $('label[for=\"", id, "\"]');
        var infoIcon = $('<span class=\"info-icon\">?</span>');
        infoIcon.tooltip($.extend(", options, ", {html: true}));
        inputLabel.append(infoIcon);
      }, 500);
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Hourly Load Profile"),
    plotlyOutput("time_series_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("CO2e Plots"),
    plotlyOutput("co2_emissions_change_plot"),
    plotlyOutput("co2_plot_annual"),
    plotlyOutput("grid_co2_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("Cost Plots"),
    plotlyOutput("cost_plot"),
    plotlyOutput("cost_plot_annual"),
    plotlyOutput("lcac_plot")
  )
)


ui <- function(request){
  page_sidebar(
    useShinyjs(),
    use_prompt(),
    tags$head(
      tags$script('
      $(document).on("shiny:connected", function() {
        var userTimeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
        var timeZoneAbbr = new Date().toLocaleTimeString("en-us",{timeZoneName:"short"}).split(" ")[2];
        Shiny.setInputValue("user_time_zone", userTimeZone);
        Shiny.setInputValue("user_time_zone_abbr", timeZoneAbbr);
      });
    '),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTooltip', function(message) {
        var tooltip = $('#tou_ed .info-icon');
        tooltip.attr('data-original-title', message);
        tooltip.tooltip('hide').attr('data-original-title', message).tooltip('fixTitle').tooltip('show');
      });
    ")),
      tags$script(HTML("
      $(window).trigger('resize')
        ")),
      tags$style(HTML("
        .info-icon {
          display: inline-block;
          width: 18px;
          height: 18px;
          border-radius: 50%;
          background-color: #2c3e50;
          color: #fff;
          text-align: center;
          line-height: 18px;
          font-size: 12px;
          margin-left: 5px;
          cursor: pointer;
          position: relative;
          font-style: normal;
          font-family: 'Arial', sans-serif;
        }
        .custom-title {
        font-size: 24px; /* Change this value to adjust the title size */
        font-weight: bold;
      }
      "))
    ),
    title = div(class = "custom-title", "Electrical Load Planning Tool"),
    theme = bs_theme(bootswatch = "flatly"),
    sidebar = sidebar(
      shinyjs::useShinyjs(),
      id = "side-panel",
      width = "30%",
      fluidRow(
        column(12, h4("Step 1a: Select Electrical Load Type & Date Range:", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          radioButtons("green_manual","",
                       choices = c( "Custom Hourly Load", "Green Button: 15-Minute", "Green Button: Hourly","12 Months Utility Bills"),
                       selected = "Custom Hourly Load"
          ),
          radioTooltip(id = "green_manual", choice = "12 Months Utility Bills", title = "Download and fill out the custom 12 months utility bill excel template", placement = "right", trigger = "hover"),
          radioTooltip(id = "green_manual", choice = "Green Button: 15-Minute", title = "Upload XML file from your utility", placement = "right", trigger = "hover"),
          radioTooltip(id = "green_manual", choice = "Green Button: Hourly", title = "Upload XML file from your utility", placement = "right", trigger = "hover"),
          radioTooltip(id = "green_manual", choice = "Custom Hourly Load", title = "Download and fill out the custom hourly excel template", placement = "right", trigger = "hover"),
          dateRangeInput("date_range", "Select Date Range:",
                         start = "2023-01-01", end = "2023-12-31",
                         format = "yyyy-mm-dd",
                         separator = " - "
          ),
          
          dateRangeTooltip("date_range", "Select Date Range:", "Choose a date range that matches the dates in your electrical load data. You can also modify this range to filter and analyze just a subset of your data", placement = "right", trigger = "hover"),
          uiOutput("message_date_disc"),
          fluidRow(uiOutput("BillcustomUI")
          ) 
        )
      ),
      fluidRow(
        column(12, h4("Step 1b: Upload Electrical Load Data:", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          uiOutput("fileInputUI")
        )
      ),
      fluidRow(
        column(12, h4("Step 2: Choose GHG Emission Parameters", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          selectInput("emissions_type",
                      label = tags$span(
                        "Select Analysis Year:", 
                        tags$span(
                          icon(
                            name = "circle-exclamation",
                          ) 
                        ) |>
                          add_prompt(message = "Odd year emission factors are the same as the preceding even year as NREL Cambium only provides estimates for even years", position = "right", size = "medium")
                      ), 
                      choices = list(
                        "NREL Cambium Standard Scenarios 2021: Average Hourly Emission Forecast:" = list("","2022","2023","2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045","2046","2047","2048","2049","2050"),
                        "Annual Emissions Factor:" = list("U.S. EPA's 2022 eGRID")
                      ), 
                      selected = "2023"),
          tags$div(
            style = "display: flex; align-items: center;",
            tags$div(
              style = "display: flex; flex-direction: column;",
              tags$label(
                "Facility's Clean Electricity Share (in %): ", 
                tags$span(
                  icon(name = "circle-exclamation")
                ) |>
                  add_prompt(
                    message = "Enter the percentage of clean electricity utilized by your facility. For example, if 20% of total electricity use is renewably sourced, then enter 20", 
                    position = "right", 
                    size = "medium"
                  )
              ),
              tags$div(
                style = "display: flex; width: 70px; align-items: center; margin-top: 5px", # Control only the input box width
                numericInput(
                  "perc_clean", 
                  label = NULL, # Remove default label from numericInput
                  value = 10, 
                  min = 0, 
                  max = 100
                )
              )
            )
          ),
          selectInput("state", "Select State:", choices = c("","AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))
        )),
      fluidRow(
        column(12, h4("Step 3a: Specify Electric Rate Structure", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          checkboxGroupInput("tou_ed", "Select all Time-of-Use (TOU) bill components",
                             choices = c("Usage charge ($/kWh)", "Demand charge ($/kW)"),
                             selected = c("Usage charge ($/kWh)")
          ),
          checkboxTooltip("tou_ed", 
                          title = "Check all applicable TOU charges. If you have fixed usage and demand charges, uncheck both options."),
          conditionalPanel(
            condition = "input.tou_ed.length > 0",
            radioButtons("tou_periods", "Do you have a TOU period apart from on-peak and off-peak (e.g. part-peak)?",
                         choices = c("Yes", "No"),
                         selected = "No"
            ),
            radioTooltip(id = "tou_periods", choice = "Yes", title = "Select Yes if your TOU structure has a third TOU period besides on and off peak", placement = "right", trigger = "hover"),
            radioTooltip(id = "tou_periods", choice = "No", title = "Select No if your TOU structure only has an on-peak and off-peak period", placement = "right", trigger = "hover")
          ),
          radioButtons("tou_mm_y_n","Do you have a monthly/season maximum demand charge ($/kW)?",
                       choices = c("Yes", "No"),
                       selected = "Yes"
          ),
          radioTooltip(id = "tou_mm_y_n", choice = "Yes", title = "Select Yes if you have a fixed charge applying to your highest demand for the month", placement = "right", trigger = "hover"),
          radioTooltip(id = "tou_mm_y_n", choice = "No", title = "Select No if you don't have a fixed charge applying to your highest demand for the month", placement = "bottom", trigger = "hover")
        )
      ),
      fluidRow(
        column(12, h4("Step 3b: Enter Electricity Rate Structure Info:", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          accordion(open = F,
                    accordion_panel(
                      "Enter Summer Rate Structure",
                      fluidRow(
                        align = "center",
                        column(
                          12,
                          fluidRow(h5("Select summer months")),
                          fluidRow(
                            column(6, selectInput("summer_month1", "First Month", choices = month.name, selected = "April")),
                            column(6, selectInput("summer_month2", "Final Month", choices = month.name, selected = "September"))
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 || input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              h5("Select summer hours"),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, selectInput("summer_offpeak_start", "Off-Peak Start Hour", choices = 0:23, selected = 21)),
                                column(6, selectInput("summer_offpeak_end", "Off-Peak End Hour", choices = 0:23, selected = 16)),
                                column(6, selectInput("summer_partpeak_start_hour", "Part-Peak Start Hour", choices = 0:23, selected = 14)),
                                column(6, selectInput("summer_partpeak_end_hour", "Part-Peak End Hour", choices = 0:23, selected = 16))
                              ),
                              column(6, selectInput("summer_onpeak_start", "On-Peak Start Hour", choices = 0:23, selected = 16)),
                              column(6, selectInput("summer_onpeak_end", "On-Peak End Hour", choices = 0:23, selected = 21))
                            )
                          ),
                          fluidRow(h5("Select summer electricity rates")),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') == -1 && input.tou_ed.indexOf('Demand charge ($/kW)') == -1",
                            fluidRow(
                              column(6, numericInput("summer_fixed_rate", "Summer Fixed Rate($/kWh)", value = 0.3,step=0.02)),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("summer_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 && input.tou_ed.indexOf('Demand charge ($/kW)') == -1",
                            fluidRow(
                              column(6, numericInput("summer_offpeak_rate", "Off-Peak Rate($/kWh)", value = 0.25,step=0.02)),
                              column(6, numericInput("summer_onpeak_rate", "On-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("summer_partpeak_rate", "Part-Peak Rate($/kWh)", value = 0.35,step=0.02))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("summer_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') == -1 && input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              column(6, numericInput("summer_fixed_rate", "Summer Fixed Rate($/kWh)", value = 0.3,step=0.02)),
                              column(6, numericInput("summer_onpeak_demand", "On-Peak Demand Charge($/kW)", value = 20,step=2)),
                              column(6, numericInput("summer_offpeak_demand", "Off-Peak Demand Charge($/kW)", value = 0,step=2)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("summer_partpeak_demand", "Part-Peak Demand Charge($/kW)", value = 13,step=2))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("summer_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 && input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              column(6, numericInput("summer_offpeak_rate", "Off-Peak Rate($/kWh)", value = 0.25,step=0.02)),
                              column(6, numericInput("summer_onpeak_rate", "On-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                              column(6, numericInput("summer_onpeak_demand", "On-Peak Demand Charge($/kW)", value = 20,step=2)),
                              column(6, numericInput("summer_offpeak_demand", "Off-Peak Demand Charge($/kW)", value = 0,step=2)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("summer_partpeak_rate", "Part-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                                column(6, numericInput("summer_partpeak_demand", "Part-Peak Demand Charge($/kW)", value = 13,step=2))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("summer_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          )
                        )
                      )
                    ),
                    accordion_panel(
                      "Enter Winter Rate Structure",
                      fluidRow(
                        align = "center",
                        column(
                          12,
                          fluidRow(h5("Select winter months")),
                          fluidRow(
                            column(6, selectInput("winter_month1", "First Month", choices = month.name, selected = "October")),
                            column(6, selectInput("winter_month2", "Final Month", choices = month.name, selected = "March"))
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 || input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              h5("Select winter hours"),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, selectInput("winter_offpeak_start", "Off-Peak Start Hour", choices = 0:23, selected = 21)),
                                column(6, selectInput("winter_offpeak_end", "Off-Peak End Hour", choices = 0:23, selected = 16)),
                                column(6, selectInput("winter_partpeak_start_hour", "Part-Peak Start Hour", choices = 0:23, selected = 14)),
                                column(6, selectInput("winter_partpeak_end_hour", "Part-Peak End Hour", choices = 0:23, selected = 16))
                              ),
                              column(6, selectInput("winter_onpeak_start", "On-Peak Start Hour", choices = 0:23, selected = 16)),
                              column(6, selectInput("winter_onpeak_end", "On-Peak End Hour", choices = 0:23, selected = 21))
                            )
                          ),
                          fluidRow(h5("Select winter electricity rates")),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') == -1 && input.tou_ed.indexOf('Demand charge ($/kW)') == -1",
                            fluidRow(
                              column(6, numericInput("winter_fixed_rate", "Winter Fixed Rate($/kWh)", value = 0.3,step=0.02)),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("winter_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 && input.tou_ed.indexOf('Demand charge ($/kW)') == -1",
                            fluidRow(
                              column(6, numericInput("winter_offpeak_rate", "Off-Peak Rate($/kWh)", value = 0.25,step=0.02)),
                              column(6, numericInput("winter_onpeak_rate", "On-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("winter_partpeak_rate", "Part-Peak Rate($/kWh)", value = 0.35,step=0.02))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("winter_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') == -1 && input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              column(6, numericInput("winter_fixed_rate", "Winter Fixed Rate($/kWh)", value = 0.3,step=0.02)),
                              column(6, numericInput("winter_onpeak_demand", "On-Peak Demand Charge($/kW)", value = 20,step=2)),
                              column(6, numericInput("winter_offpeak_demand", "Off-Peak Demand Charge($/kW)", value = 0,step=2)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("winter_partpeak_demand", "Part-Peak Demand Charge($/kW)", value = 13,step=2))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("winter_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.tou_ed.indexOf('Usage charge ($/kWh)') != -1 && input.tou_ed.indexOf('Demand charge ($/kW)') != -1",
                            fluidRow(
                              column(6, numericInput("winter_offpeak_rate", "Off-Peak Rate($/kWh)", value = 0.25,step=0.02)),
                              column(6, numericInput("winter_onpeak_rate", "On-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                              column(6, numericInput("winter_onpeak_demand", "On-Peak Demand Charge($/kW)", value = 20,step=2)),
                              column(6, numericInput("winter_offpeak_demand", "Off-Peak Demand Charge($/kW)", value = 0,step=2)),
                              conditionalPanel(
                                condition = "input.tou_periods == 'Yes'",
                                column(6, numericInput("winter_partpeak_rate", "Part-Peak Rate($/kWh)", value = 0.35,step=0.02)),
                                column(6, numericInput("winter_partpeak_demand", "Part-Peak Demand Charge($/kW)", value = 13,step=2))
                              ),
                              conditionalPanel(
                                condition = "input.tou_mm_y_n == 'Yes'",
                                column(6, numericInput("winter_max_demand", "Monthly/Season Max Demand Charge($/kW)", value = 40,step=2))
                              )
                            )
                          )
                        )
                      )
                    ),
                    br(),
                    bookmarkButton(label = "Bookmark Rate Structure Inputs", title = "Click to bookmark/save your entered electricity rate inputs as a URL.")
          )
        )),
      fluidRow(br()),
      fluidRow(
        column(12, h4("Step 4: Analyze Load Management Aprroach:", style = "font-weight: bold; font-size: 17px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;")),
        wellPanel(
          style = "border: 2px solid #ccc; padding: 10px; border-radius: 5px;",
          tags$div(
            style = "display: flex; justify-content: flex-end; gap: 120px;",
            radioButtons("addshed_shift", "Type of load management",
                         choices = c("Add/Shed Load","Shift Load"),
                         selected = "Add/Shed Load",
                         inline = F,
                         width = '600px'
            ),
            radioTooltip(id = "addshed_shift", choice = "Add/Shed Load", title = "Enter a positive value to add load or a negative value to shed load", placement = "right", trigger = "hover"),
            radioTooltip(id = "addshed_shift", choice = "Shift Load", title = "Enter a positive value for the load you want to shift", placement = "right", trigger = "hover"),
            
            conditionalPanel(condition = "input.green_manual != '12 Months Utility Bills'",
                             radioButtons("work_on_weekends", "Manage load over the weekends?",
                         choices = c("Yes", "No"),
                         selected = "No",
                         inline = F,
                         width = '500px'
            )),
            radioTooltip(id = "work_on_weekends", choice = "Yes", title = "Select yes if you would like to manage your load on weekdays and weekends", placement = "right", trigger = "hover"),
            radioTooltip(id = "work_on_weekends", choice = "No", title = "Select no if you would like to manage your load only on weekdays", placement = "right", trigger = "hover"),
          ),
          conditionalPanel(
            condition = "input.addshed_shift == 'Add/Shed Load'",
            accordion(open = F,
                      accordion_panel(
                        "Enter Load Addition/Shedding Inputs",
                        radioButtons("addData", "Select Input Method:",
                                     choices = c("Manually Enter Added Load(s)", "Upload Existing Spreadsheet")
                        ),
                        conditionalPanel(
                          condition = "input.addData == 'Manually Enter Added Load(s)'",
                          uiOutput("inputSets_1")
                        ),
                        fluidRow(align = "center",
                                 column(4, actionButton("plot_button_2", "Generate Plot", style = "width:120px;padding:8px")),
                                 column(8, conditionalPanel(
                                   condition = "input.addData == 'Manually Enter Added Load(s)'",
                                   actionButton("addWindow_load", "Add more Load Input(s)", style = "width:200px;margin-right: 0px;padding:8px")
                                 )
                                 )
                        ),
                        fluidRow(
                          conditionalPanel(
                            condition = "input.addData == 'Upload Existing Spreadsheet'",
                            column(4, fileInput("file_upload_addinput", "Upload Input Sheet Excel File"))
                          )
                        )
                      )
            )
          ),
          conditionalPanel(
            condition = "input.addshed_shift == 'Shift Load'",
            accordion(open = F,
                      accordion_panel(
                        "Enter Load Shifting Inputs",
                        radioButtons("shiftData", "Select Input Method:",
                                     choices = c("Manually Enter Shaped Load(s)", "Upload Existing Spreadsheet")
                        ),
                        conditionalPanel(
                          condition = "input.shiftData == 'Manually Enter Shaped Load(s)'",
                          uiOutput("inputSets")
                        ),
                        fluidRow(align = "center",
                                 column(4, actionButton("plot_button", "Generate Plot", style = "width:120px;padding:8px")),
                                 column(8, conditionalPanel(
                                   condition = "input.shiftData == 'Manually Enter Shaped Load(s)'",
                                   actionButton("addWindow", "Add more Shaped Load(s)", style = "width:220px;margin-right: 0px;padding:8px")
                                 )) 
                        ),
                        br(),
                        fluidRow(conditionalPanel(
                          condition = "input.shiftData == 'Upload Existing Spreadsheet'",
                          column(4, fileInput("file_upload_shiftinput", "Upload Input Sheet Excel File"))
                        ))
                      )
            )
          )
        )
      ),
      fluidRow(align = "center",
               column(5, downloadButton("downloadInputData", "Download Input Data",style = "width:180px;padding:8px;font-size:80%")),
               column(7, downloadButton("downloadMergedData", "Download Modified Load Data",style = "width:240px;padding:8px;font-size:80%"))
      ),
      downloadLink("downloaddocu", "Download Tool Documentation")
    ),
    fluidRow(
      align = "right",
      column(12,
             actionButton("reset_input", " Reset inputs", icon = icon("power-off"))
             )
    ),

    accordion(
      open = c("Bill Length", "About"),
      navset_card_underline(
        title = HTML("<b>Electrical Load Plot</b>"),
        nav_panel(
          div("Hourly Electrical Load Plot", style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);"),
          style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1); width:100%; height:100%; overflow:auto;",
          conditionalPanel(
            condition = "output.fileUploaded | input.plot_button >= 1 | input.plot_button_2 >= 1",
            style = "display: none;",
            shinycssloaders::withSpinner(
              plotlyOutput("time_series_plot", width = "100%", height = "100%"),
              type = getOption("spinner.type", default = 8)
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "output.fileUploaded | input.plot_button >= 1 | input.plot_button_2 >= 1",
              downloadButton("download_load_data", "Download Plot Data (.XLSX)")
            )
          )
        )
      ),
      navset_card_underline(
        title = HTML("<b>CO<sub>2</sub>e Plots</b>"),
        nav_panel(
          div(HTML("Hourly CO<sub>2</sub>e Emissions Plot"), style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "output.fileUploaded & input.state != ''  || input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("co2_emissions_change_plot", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "output.fileUploaded & input.state != ''  || input.plot_button >= 1 || input.plot_button_2 >= 1",
              downloadButton("download_co2em_data", "Download Plot Data (.XLSX)")
            )
          )
        ),
        nav_panel(
          div(HTML("Monthly CO<sub>2</sub>e Emissions Plot"), style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "output.fileUploaded & input.state != ''  || input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("co2_plot_annual", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "output.fileUploaded & input.state != ''  || input.plot_button >= 1 || input.plot_button_2 >= 1",
              downloadButton("download_co2em_data_monthly", "Download Plot Data (.XLSX)")
            )
          )
        ),
        nav_panel(
          div(HTML("Electric Grid CO<sub>2</sub>e Emissions Factor Plot"), style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "output.fileUploaded & input.state != '' || input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("grid_co2_plot", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "output.fileUploaded & input.state != '' || input.plot_button >= 1 || input.plot_button_2 >= 1",
              downloadButton("grid_ef", "Download Plot Data (.XLSX)")
            )
          )
        )
      ),
      navset_card_underline(
        title = HTML("<b>Cost Plots</b>"),
        nav_panel(
          div("Monthly Cost Plot", style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "output.fileUploaded || input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("cost_plot", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "output.fileUploaded | input.plot_button >= 1 | input.plot_button_2 >= 1",
              downloadButton("download_costselect_data", "Download Plot Data (.XLSX)")
            )
          )
        ),
        nav_panel(
          div("Annual Cost Plot", style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "output.fileUploaded || input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("cost_plot_annual", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          )
        ),
        nav_panel(
          div("Carbon Abatement Cost Plot", style = "padding: 10px; box-shadow: 0 6px 8px rgba(0, 0, 0, 0.1);width:100%; height:100%; overflow:auto;"),
          conditionalPanel(
            condition = "input.plot_button >= 1 || input.plot_button_2 >= 1",
            style = "display: none;overflow-x: scroll;overflow-y: scroll;",
            shinycssloaders::withSpinner(plotlyOutput("lcac_plot", width = "100%", height = "100%"),
                                         type = getOption("spinner.type", default = 8)
            )
          ), div(
            style = "display: flex; justify-content: flex-end; position: absolute; bottom: 10px; right: 10px;",
            conditionalPanel(
              condition = "input.plot_button >= 1 | input.plot_button_2 >= 1",
              downloadButton("download_lcac_data", "Download Plot Data (.XLSX)")
            )
          )
        )
      )
    )
  )
}



thematic::thematic_shiny(font = "auto")


server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  docFilePath <- 'AllUploadFiles_ToolTesting/ELPT User Guide - Draft.pdf'
  
  output$downloaddocu <- downloadHandler(
    filename = function() {
      basename(docFilePath)
    },
    content = function(file) {
      file.copy(docFilePath, file)
    }
  )

  
  theme = bs_theme(bootswatch = "flatly")

    observeEvent(input$emissions_type, {
    
      # Extract selected year
      if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
        selected_year <- 2022} else {
    selected_year <- as.integer(input$emissions_type)
        }
    
    # Extract current selected start and end dates
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Update date range input
    updateDateRangeInput(session, "date_range",
                         start = paste0(selected_year, format(start_date, "-%m-%d")),
                         end = paste0(selected_year, format(end_date, "-%m-%d")))
  })
  
  
  
  observeEvent(input$tou_periods, {
    if (input$tou_periods == "Yes") {
      updateSelectInput(session, "summer_offpeak_end", selected = 14)
      updateSelectInput(session, "winter_offpeak_end", selected = 14)
    } else {
      updateSelectInput(session, "summer_offpeak_end", selected = 16)
      updateSelectInput(session, "winter_offpeak_end", selected = 16)
    }
  })
  
  
  output$fileInputUI <- renderUI({
    if (input$green_manual == "Custom Hourly Load") {
      fileInput("loadpf_file", "Upload Hourly Electrical Load Data (.XLSX):")
    } else if (input$green_manual == "12 Months Utility Bills") {
      fileInput("loadpf_file", "Upload 12 Months Utility Bill Data (.XLSX):")
    } else if (input$green_manual == "Green Button: 15-Minute") {
      fileInput("loadpf_file", "Upload Green Button Data (.XML):")
    } else if (input$green_manual == "Green Button: Hourly") {
      fileInput("loadpf_file", "Upload Green Button Data (.XML):")
    }
  })
  
  observeEvent(input$green_manual, {
    reset('loadpf_file')
  })
  
  
  
  output$BillcustomUI <- renderUI({
    if (input$green_manual == "Custom Hourly Load") {
      downloadButton("downloadSheet", label = "Download Custom Hourly Template",style = "width:300px;padding:8px;display:inline-block")} else if (input$green_manual == "12 Months Utility Bills") {
        downloadButton("downloadSheet1", label = "Download 12 Months Bills Template",style = "width:300px;padding:8px;display:inline-block")
      }
  })
  
  output$fileUploaded <- reactive({
    !is.null(input$loadpf_file)
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  input_sheet_finalData <- reactiveVal()
  
  date_range <- reactive(input$date_range)
  
  empty_input_sheet_defaults <- reactiveValues(data = NULL)
  
  observe({
    if (input$addshed_shift == "Shift Load") {
      flexible_load <- c(50, 25, 30)
      time_range_to_start <- as.POSIXct(c("10:00", "11:00", "13:00"), format = "%H:%M")
      time_range_to_end <- as.POSIXct(c("11:00", "12:00", "14:00"), format = "%H:%M")
      time_range_from_start <- as.POSIXct(c("16:00", "17:00", "20:00"), format = "%H:%M")
      time_range_from_end <- as.POSIXct(c("17:00", "18:00", "21:00"), format = "%H:%M")
      
      extract_time <- function(datetime) {
        format(datetime, format = "%H:%M")
      }
      
      time_range_to_start <- sapply(time_range_to_start, extract_time)
      time_range_to_end <- sapply(time_range_to_end, extract_time)
      time_range_from_start <- sapply(time_range_from_start, extract_time)
      time_range_from_end <- sapply(time_range_from_end, extract_time)
      
      data <- data.frame("Flexible Load (in kW)" = flexible_load,
                         "Time Range To Start" = time_range_to_start,
                         "Time Range To End" = time_range_to_end,
                         "Time Range From Start" = time_range_from_start,
                         "Time Range From End" = time_range_from_end,
                         check.names = FALSE )
      
    } else if (input$addshed_shift == "Add/Shed Load") {
      flexible_load <- c(50, 25, 30)
      time_range_to_start <- as.POSIXct(c("10:00", "11:00", "13:00"), format = "%H:%M")
      time_range_to_end <- as.POSIXct(c("11:00", "12:00", "14:00"), format = "%H:%M")
      
      extract_time <- function(datetime) {
        format(datetime, format = "%H:%M")
      }
      
      time_range_to_start <- sapply(time_range_to_start, extract_time)
      time_range_to_end <- sapply(time_range_to_end, extract_time)

      data <- data.frame("Added/Shedded Load (in kW)" = flexible_load,
                         "Time Range To Start" = time_range_to_start,
                         "Time Range To End" = time_range_to_end,
                         check.names = FALSE )
    }
    
    empty_input_sheet_defaults$data <- data
    
    output$downloadInputData <- downloadHandler(
      filename = function() {
        "input_data.xlsx"
      },
      content = function(file) {
        write.xlsx(data, file)
      }
    )
  }
  )
    
  # Reactive expression to create the input_load_df based on date range
  input_load_df <- reactive({
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if (start_date > end_date) {
      start_date <- input$date_range[2]
      end_date <- input$date_range[1]
    }
    
    datetime_seq <- seq(
      as.POSIXct(paste(start_date, "00:00:00")),
      as.POSIXct(paste(end_date, "23:00:00")),
      by = "hour"
    )
    
    formatted_datetime <- format(datetime_seq, "%m/%d/%y %H:%M")
    input_load_df <- data.frame(Date_Time = formatted_datetime)
    input_load_df$Date_Time <- as.POSIXct(input_load_df$Date_Time, format = "%m/%d/%y %H:%M")
    input_load_df$hours <- seq_along(input_load_df$Date_Time)
    input_load_df$month <- format(input_load_df$Date_Time, "%B")
    input_load_df$Day <- format(input_load_df$Date_Time, "%d")
    input_load_df$Day <- as.numeric(input_load_df$Day)
    
    df_pge_sample <- read_excel("AllUploadFiles_ToolTesting/PG&E_Default_Sample.xlsx")
    input_load_df$MonthDayTime <- format(input_load_df$Date_Time, "%m-%d %H:%M")
    df_pge_sample$MonthDayTime <- format(df_pge_sample$Date_Time, "%m-%d %H:%M")
    df_pge_sample <- df_pge_sample %>% 
      select(hours, `Load (in kW)`)
    input_load_df <- input_load_df %>% 
      left_join(df_pge_sample, by = "hours")
    
    input_load_df <- input_load_df %>%
      select(-MonthDayTime)
    
    input_load_df <- input_load_df %>% 
      select(Date_Time, `Load (in kW)`, everything())
    
    input_load_df$`Load (in kW)` <- na_ma(input_load_df$`Load (in kW)`, k = 4, weighting = "simple")
    
    input_load_df
  })
  
  # Reactive value to store the modified workbook
  reactive_workbook <- reactiveVal()
  
  observeEvent(input$date_range, {
    # Load the workbook
    excelFilePathBills_0 <- loadWorkbook("AllUploadFiles_ToolTesting/Utility Bills Template.xlsx")
    
    # Read the data from the specified sheet
    excelFilePathBills_0.5 <- read.xlsx("AllUploadFiles_ToolTesting/Utility Bills Template.xlsx", sheet = "Tool template", colNames = TRUE)
    
    # Get the start year from the date range
    start_date <- input$date_range[1]
    start_year <- year(start_date)
    
    # Modify only the first column (replace 2023 with the start year)
    excelFilePathBills_0.5[[1]] <- gsub("2023", start_year, excelFilePathBills_0.5[[1]])
    
    # Write the modified data back into the workbook object
    writeData(
      wb = excelFilePathBills_0, 
      sheet = "Tool template", 
      x = excelFilePathBills_0.5[[1]], 
      startCol = 1, 
      startRow = 2, 
      colNames = TRUE
    )
    
    # Store the modified workbook in the reactive value
    reactive_workbook(excelFilePathBills_0)
  })
  
  # Download handler for the load data Excel file
  output$downloadSheet <- downloadHandler(
    filename = function() {
      "load_pf_input.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", input_load_df())
      
      datetime_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss",
                                    fgFill = "#D3D3D3",
                                    borderColour = "black",
                                    locked = TRUE)
      
      yellow_style <- createStyle(
        fgFill = "#FFFF00",
        border = "TopBottomLeftRight",
        borderColour = "black",
        locked = FALSE
      )
      
      grey_style <- createStyle(
        fgFill = "#D3D3D3",
        border = "TopBottomLeftRight",
        borderColour = "black",
        locked = TRUE
      )
      
      addStyle(wb, sheet = "Sheet1", style = datetime_style, rows = 1:(nrow(input_load_df()) + 1), cols = 1, gridExpand = TRUE)
      addStyle(wb, sheet = "Sheet1", style = yellow_style, rows = 1:(nrow(input_load_df()) + 1), cols = 2, gridExpand = TRUE)
      addStyle(wb, sheet = "Sheet1", style = grey_style, rows = 1:(nrow(input_load_df()) + 1), cols = 3, gridExpand = TRUE)
      addStyle(wb, sheet = "Sheet1", style = grey_style, rows = 1:(nrow(input_load_df()) + 1), cols = 4, gridExpand = TRUE)
      addStyle(wb, sheet = "Sheet1", style = grey_style, rows = 1:(nrow(input_load_df()) + 1), cols = 5, gridExpand = TRUE)
      
      setColWidths(wb, sheet = "Sheet1", cols = 1, widths = "20")
      
      protectWorksheet(wb, sheet = "Sheet1", protect = TRUE)
      
      saveWorkbook(wb, file)
    }
  )
  
  # Download handler for the modified Excel file
  output$downloadSheet1 <- downloadHandler(
    filename = function() {
      "Utility Bills Template.xlsx"
    },
    content = function(file) {
      req(reactive_workbook())
      # Save the workbook to the specified file
      saveWorkbook(reactive_workbook(), file, overwrite = TRUE)
    }
  )
  
  
  count <- reactiveVal(1)
  inputSets <- reactiveValues(sets = list())
  count_add <- reactiveVal(1)
  inputSets_1 <- reactiveValues(sets_1 = list())
  
  observe({
    req(input$user_time_zone)
    inputSets$sets <- list(createInputSet(1, input$user_time_zone))
    inputSets_1$sets_1 <- list(createInputSet_1(1, input$user_time_zone))
    desired_tz <- input$user_time_zone
  })
  
  createInputSet <- function(id, user_tz) {
    tagList(
      tags$h4(paste0("Shaped Load ", id)),
      div(
        style = "display: flex; flex-direction: row; gap: 12px;align-items: center;",
        numericInput(inputId = paste0("numeric_", id),
                     label = "Flexible Load (kW)",
                     value = 100,
                     width = "70px",
                     step = 50,
                     min = 0),
        sliderInput(inputId = paste0("slider_from", id),
                    label = "Time Range From:",
                    min = as.POSIXct("00:00", format = "%H:%M"),
                    max = as.POSIXct("23:59", format = "%H:%M"),
                    value = c(as.POSIXct("17:00", format = "%H:%M"),
                              as.POSIXct("19:00", format = "%H:%M")),
                    timeFormat = "%H:%M",
                    width = "225px"),
        sliderInput(inputId = paste0("slider_to", id),
                    label = "Time Range To:",
                    min = as.POSIXct("00:00", format = "%H:%M"), 
                    max = as.POSIXct("23:59", format = "%H:%M"), 
                    value = c(as.POSIXct("12:00", format = "%H:%M"),
                              as.POSIXct("14:00", format = "%H:%M")),
                    timeFormat = "%H:%M", 
                    width = "225px")
      )
    )
  }
  
  
  
  createInputSet_1 <- function(id_1, user_tz) {
    tagList(
      tags$h4(paste0("New Load ", id_1)),
      div(
        style = "display: flex; flex-direction: row; gap: 30px;align-items: center;",
        numericInput(inputId = paste0("numeric_add_", id_1), label = "Added Load (kW)", value = 100, width = "80px", step = 50),
        sliderInput(inputId = paste0("slider_to_add", id_1), label = "Time Range To Add Load:", min = as.POSIXct("00:00", format = "%H:%M"), max = as.POSIXct("23:59", format = "%H:%M"), value = c(as.POSIXct("12:00", format = "%H:%M"), as.POSIXct("14:00", format = "%H:%M")), timeFormat = "%H:%M", width = "400px")
      )
    )
  }
  
  output$dynamicInputs <- renderUI({
    req(input$user_time_zone)
    createInputSet("1", input$user_time_zone)
  })
  
  
  observeEvent(input$addWindow, {
    req(input$user_time_zone)
    new_id <- count() + 1
    count(new_id)
    inputSets$sets <- append(inputSets$sets, list(createInputSet(new_id, input$user_time_zone)))
  })
  
  observeEvent(input$addWindow_load, {
    req(input$user_time_zone)
    new_id_1 <- count_add() + 1
    count_add(new_id_1)
    inputSets_1$sets_1 <- append(inputSets_1$sets_1, list(createInputSet_1(new_id_1, input$user_time_zone)))
  })
  
  output$inputSets <- renderUI({
    inputSets$sets
  })
  
  output$inputSets_1 <- renderUI({
    inputSets_1$sets_1
  })
  
  ##Excluding load management windows from the bookmarking features##
  
  observe({
    exclude_patterns <- c("^slider_from", "^numeric_", "^slider_to", "^slider_to_add")
    
    names_to_exclude <- unlist(lapply(exclude_patterns, function(pattern) {
      grep(pattern, names(input), value = TRUE)
    }))
    
    names_to_append <- c("green_manual", "start_date", "end_date", "emissions_type", "loadpf_file", "state")
    
    names_to_exclude <- c(names_to_exclude, names_to_append)
    
    setBookmarkExclude(names_to_exclude)
  })
  
  ##Excluding load management windows from the bookmarking features##
  
  
  observeEvent(input$plot_button, {
    
    if (is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Warning", "No input electrical load data and GHG emissions parameters provided. Please upload an electrical load data file and select your GHG emissions parameters.", type = "warning")
    } else if (is.null(input$loadpf_file)) {
      shinyalert("Warning", "No input electrical load data provided. Please upload an electrical load data file.", type = "warning")
    } else if (!is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "You have not specified an analysis state. Please specify a state in step 2", type = "error",
                 )
    }
  })
  
  
  observeEvent(input$plot_button_2, {
    if (is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "No input electrical load data and GHG emissions parameters provided. Please upload an electrical load data file and select your GHG emissions parameters.", type = "error")
    } else if (is.null(input$loadpf_file)) {
      shinyalert("Error", "No input electrical load data provided. Please upload an electrical load data file.", type = "error")
    } else if (!is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "You have not specified your facility's state. Please specify reload the page and specify a state in step 2 shaping your load", type = "error",
      )
    }
  })
  
  df_final_across_events <- reactiveValues(df_loadpf_long_fin = 0)
  
    observeEvent(input$loadpf_file, {
    desired_tz <- input$user_time_zone
    loadpf_file <- input$loadpf_file
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if (is.null(loadpf_file)) {
      return(NULL)
    }
    print("File Uploaded")
    
    start_year_check <- as.numeric(format(input$date_range[1], "%Y"))
    end_year_check <- as.numeric(format(input$date_range[2], "%Y"))
    
    if (end_year_check != start_year_check && start_year_check < end_year_check) {
      shinyalert(title = "Error",
                 text = "The dates you entered spanned more than one calendar year. Please see the corrected range",
                 type = "warning")
    } else if (end_year_check != start_year_check && start_year_check > end_year_check) {
      shinyalert(title = "Error",
                 text = "Entered start date greater than end date. Please see the corrected range",
                 type = "warning")
    }
    
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    
    if (start_date > end_date) {
      updateDateRangeInput(session, "date_range", start = Sys.Date(), end = Sys.Date()+1)
    }
    
    if (end_year_check != start_year_check && start_date < end_date) {
      updateDateRangeInput(session, "date_range", end = paste0(start_year_check, "-12-31"))
    }
    
    
    if (input$green_manual == "Custom Hourly Load") {
      if (is.null(input$loadpf_file$datapath)) {
        showModal(modalDialog(
          title = "Warning",
          "Please upload the template excel file",
          easyClose = TRUE
        ))
      } else {
        input$loadpf_file$datapath %>%
          read_excel() -> df_loadpf_long_fin
        
        df_loadpf_long_fin <- read_excel(loadpf_file$datapath)
        
        df_loadpf_long_fin <- df_loadpf_long_fin %>%
          rename(Load = `Load (in kW)`,
                 datetime = Date_Time)
        
        if ("Load" %in% colnames(df_loadpf_long_fin)) {
          
          df_loadpf_long_fin$datetime <- as.POSIXct(df_loadpf_long_fin$datetime, format = "%d/%m/%Y %H:%M")
          
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please Upload the Correct File",
            easyClose = TRUE
          ))
        }
      }
    } else if (input$green_manual == "12 Months Utility Bills") {
      if (is.null(input$loadpf_file$datapath)) {
        showModal(modalDialog(
          title = "Warning",
          "Please upload the template excel file",
          easyClose = TRUE
        ))
      } else {
        df_loadpf <- read_excel(loadpf_file$datapath, range = "A1:D13")
        
        if ("Energy Usage, (kWh)" %in% colnames(df_loadpf)) {
          df_loadpf <- read_excel(loadpf_file$datapath, range =  "A1:D13")
          
          df_loadpf <- janitor::clean_names(df_loadpf)
          
          df_loadpf$energy_usage_k_wh <- gsub(",", "", df_loadpf$energy_usage_k_wh)
          df_loadpf$energy_usage_k_wh <- as.numeric(df_loadpf$energy_usage_k_wh)
          df_loadpf$billed_demand_k_w <- gsub(",", "", df_loadpf$billed_demand_k_w)
          df_loadpf$billed_demand_k_w <- as.numeric(df_loadpf$billed_demand_k_w)
          average_demand <- mean(df_loadpf$billed_demand_k_w) # Average annual billed demand
          work_on_weekends_0 <- read_excel(loadpf_file$datapath, range = "G6", col_names = F)
          work_on_weekends <- work_on_weekends_0$...1
          shift_start_time_0 <- read_excel(loadpf_file$datapath, range = "c108", col_names = F)
          start_time <- shift_start_time_0$...1
          shift_end_time_0 <- read_excel(loadpf_file$datapath, range = "c109", col_names = F)
          end_time <- shift_end_time_0$...1
          hours_of_operation_day_hrs_day <- if_else(end_time==start_time,24,if_else(end_time<start_time,24-start_time+end_time,end_time-start_time))
          non_working_hours <- abs(24-hours_of_operation_day_hrs_day)
          peak_hours <- 1
          mid_low_hours <- hours_of_operation_day_hrs_day-peak_hours
          pd_0 <- read_excel(loadpf_file$datapath, range = "c110", col_names = F)
          pd <- pd_0$...1 
          
          
          annual_oh <- if_else(work_on_weekends == "N", hours_of_operation_day_hrs_day * 52 * 5, hours_of_operation_day_hrs_day * 365) # Calculating annual operating hours
          
          desired_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # order of months
          
          df_loadpf_1 <- column_to_rownames(df_loadpf, var = "month") # make column names as rownames
          
          df_loadpf_sorted <- df_loadpf_1[desired_order, , drop = FALSE] %>%
            rownames_to_column("month") # sort the bills from jan to dec
          
          days <- data.frame(days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) # vectro data for number of days in each month
          order <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) # order of months from 1 to 12; jan to dec
          
          df_loadpf_2 <- cbind(df_loadpf_sorted, days, order) # bind the three dataframes
          
          if (work_on_weekends == "Y") {
            
            df_loadpf_3 <- df_loadpf_2 %>%
              mutate(
                kwhpd = energy_usage_k_wh/days,
                baseline_demand_pd = 0.3*billed_demand_k_w,
                peak_demand_pd = billed_demand_k_w,
                mid_demand_pd = (kwhpd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
              )
            
            df_loadpf_4 <-  df_loadpf_3 %>% 
              pivot_longer(cols = !c("month":"kwhpd"),
                           names_to = "energy_type",
                           values_to = "kW")
            
            # Create a data frame with a 24-hour column
            hours_df <- data.frame(hour = 0:23) 
            
            hours_df_1 <- hours_df %>% 
              mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                             hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                             hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
            
            
            hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
            
            month_index <- data.frame(month = rep(month.name, each = 24))
            
            hours_df_3 <- cbind(month_index,hours_df_2)
            
            df_loadpf_5 <- data.frame(seq(
              as.POSIXct(paste(start_date, "00:00:00"),, tz = desired_tz),
              as.POSIXct(paste(end_date, "23:00:00"),, tz = desired_tz),
              by = "hour"
            )) %>%
              rename(datetime = nth(names(.), 1)) %>%
              mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                     hour = (hour(datetime) ) %% 24 ) %>%
              left_join(hours_df_3,by = c("month","hour")) %>% 
              left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
              mutate(Day = day(datetime))

            
            df_loadpf_long_fin_0 <- df_loadpf_5
            
          } else {
            
            # Define the count_weekdays function
            count_weekdays <- function(year, month) {
              # Generate a sequence of dates for the given month
              start_date <- make_date(year, month, 1)
              end_date <- start_date + months(1) - days(1)
              dates <- seq.Date(start_date, end_date, by = "day")
              
              # Filter weekdays
              weekdays_count <- weekdays(dates) %>%
                table() %>%
                as.data.frame() %>%
                filter(!`.` %in% c("Saturday", "Sunday")) %>%
                summarize(count = sum(Freq))
              
              return(weekdays_count$count)
            }

            df_loadpf_3 <- df_loadpf_2 %>%
              mutate(
                no_of_weekdays = mapply(count_weekdays, year, order)
              )
            
            df_loadpf_3.5 <- df_loadpf_3 %>%
              mutate(
                no_of_weekends = days- no_of_weekdays,
                baseline_demand_pd = 0.3*billed_demand_k_w,
                kwh_wknd = baseline_demand_pd*24*no_of_weekends,
                kwh_wkd = energy_usage_k_wh-kwh_wknd,
                kwhpd_wknd = baseline_demand_pd,
                kwhpd_wkd = kwh_wkd/no_of_weekdays,
                peak_demand_pd = billed_demand_k_w,
                mid_demand_pd = (kwhpd_wkd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
              ) %>% 
              relocate(baseline_demand_pd,.after = kwhpd_wkd)
            
            df_loadpf_3.5 <- df_loadpf_3.5
            
            df_loadpf_4 <-  df_loadpf_3.5 %>% 
              pivot_longer(cols = !c("month":"kwhpd_wkd"),
                           names_to = "energy_type",
                           values_to = "kW")
            
            # Create a data frame with a 24-hour column
            hours_df <- data.frame(hour = 0:23) 
            
            hours_df_1 <- hours_df %>% 
              mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                             hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                             hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
            
            
           hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
            
            month_index <- data.frame(month = rep(month.name, each = 24))
            
            hours_df_3 <- cbind(month_index,hours_df_2)
            
            #df_loadpf_5 <- left_join(hours_df_3,df_loadpf_4,by = c("month","energy_type"))
            
            df_loadpf_5 <- data.frame(seq(
              as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
              as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz),
              by = "hour"
            )) %>%
              rename(datetime = nth(names(.), 1)) %>%
              mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                     hour = (hour(datetime) ) %% 24 ) %>%
              left_join(hours_df_3,by = c("month","hour")) %>% 
              left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
              mutate(Day = day(datetime))
            
            df_loadpf_5 <- df_loadpf_5
            
            
            df_loadpf_7 <- df_loadpf_5 %>% 
              mutate(day = weekdays(datetime),
                     energy_type = if_else(day == "Saturday" | day == "Sunday", "baseline_demand_pd",energy_type),
                     kW = if_else(day == "Saturday" | day == "Sunday", 0.3*billed_demand_k_w,kW)
              )
            
            df_loadpf_long_fin_0 <- df_loadpf_7
          }
          
          df_loadpf_long_fin <- df_loadpf_long_fin_0 %>%
            select(datetime,month,Day,hour,kW) %>% 
            rename(Load = kW,
                   hours = hour)
          
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please Upload the Correct File",
            easyClose = TRUE
          ))
        }
      }
    } else if (input$green_manual == "Green Button: Hourly") {
      if (is.null(input$loadpf_file$datapath)) {
        showModal(modalDialog(
          title = "Warning",
          "Please upload an XML file",
          easyClose = TRUE
        ))
      } else {
        xml_address <- loadpf_file$datapath
        loadpf <- read_xml(xml_address)
        xml_text <- xml_text(loadpf)
        ns <- c(espi = "http://naesb.org/espi")
        interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
        
        extract_and_convert <- function(node, xpath) {
          values <- as.integer(xml_text(xml_find_all(node, xpath)))
          return(values)
        }
        
        extract_and_parse_datetime <- function(node, xpath) {
          values <- xml_text(xml_find_all(node, xpath))
          values_as_integer <- as.integer(values)
          parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
          return(parsed_values)
        }
        
        starts <- interval_readings %>%
          xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
          extract_and_parse_datetime(xpath = ".")
        
        values <- interval_readings %>%
          xml_find_all(".//espi:value", ns) %>%
          extract_and_convert(xpath = ".")
        
        df_loadpf <- data.frame(
          DATE = starts,
          USAGE = values
        )
        
        if ("USAGE" %in% colnames(df_loadpf)) {
          
          df_loadpf_long <- df_loadpf %>%
            mutate(
              DATE = (DATE),
              Load = USAGE,
              datetime = DATE,
              month = format(datetime, "%B"),
              hour = format(as.POSIXct(datetime), format = "%H:%M")
            )
          
          df_loadpf_long <- df_loadpf_long %>%
            select(Load, month, datetime, hour)
          
          
          df_loadpf_long_fin <- df_loadpf_long
          
          df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
          
          df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
          
          ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
          df_loadpf_long_fin <- df_loadpf_long_fin %>%
            distinct(datetime, .keep_all = TRUE)
          ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
          
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please Upload the Correct File",
            easyClose = TRUE
          ))
        }
      }
    } else if (input$green_manual == "Green Button: 15-Minute") {
     
      if (is.null(input$loadpf_file$datapath)) {
        showModal(modalDialog(
          title = "Warning",
          "Please upload an XML file",
          easyClose = TRUE
        ))
      } else {
        xml_address <- loadpf_file$datapath
        loadpf <- read_xml(xml_address)
        xml_text <- xml_text(loadpf)
        ns <- c(espi = "http://naesb.org/espi")
        interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
        
        extract_and_convert <- function(node, xpath) {
          values <- as.integer(xml_text(xml_find_all(node, xpath)))
          return(values)
        }
        
        extract_and_parse_datetime <- function(node, xpath) {
          values <- xml_text(xml_find_all(node, xpath))
          values_as_integer <- as.integer(values)
          parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
          return(parsed_values)
        }
        
        starts <- interval_readings %>%
          xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
          extract_and_parse_datetime(xpath = ".")
        
        values <- interval_readings %>%
          xml_find_all(".//espi:value", ns) %>%
          extract_and_convert(xpath = ".")
        
        df_loadpf <- data.frame(
          DATE = starts,
          USAGE = values
        )
        
        if ("USAGE" %in% colnames(df_loadpf)) {
          
          df_loadpf_long <- df_loadpf %>%
            mutate(
              DATE = (DATE),
              Load = USAGE,
              datetime = DATE,
              month = format(datetime, "%B"),
              hour = format(as.POSIXct(datetime), format = "%H:%M")
            )
          
          df_loadpf_long <- df_loadpf_long %>%
            select(Load, month, datetime, hour)
          
          
          df_loadpf_long_fin <- df_loadpf_long
          
          df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
          
          df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
          
          
          ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
          df_loadpf_long_fin <- df_loadpf_long_fin %>%
            distinct(datetime, .keep_all = TRUE)
          ######
          
          
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please Upload the Correct File",
            easyClose = TRUE
          ))
        }
      }
    }
    
    date_range <- reactive(input$date_range)
    
    filtered_load_data <- reactive({
      if (!is.null(input$date_range)) {
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
           # selected_year <- as.integer(input$emissions_type)
            selected_year <-  year(date_range()[1])
          }
        start_date <- input$date_range[1]
        end_date <- input$date_range[2]
        df_loadpf_long_fin$datetime <- update(df_loadpf_long_fin$datetime, year = selected_year)
        subset(df_loadpf_long_fin, datetime >= start_date & datetime <= end_date)
        df_loadpf_long_fin<-df_loadpf_long_fin
      } else {
        df_loadpf_long_fin
      }
    })
    

    filtered_load_data <- filtered_load_data
    
    df_loadpf_long_fin <- df_loadpf_long_fin
    
    df_loadpf_long_fin$datetime <- as.POSIXct(df_loadpf_long_fin$datetime, format = "%Y-%m-%d %H:%M")
    
    df_final_across_events$df_loadpf_long_fin <- df_loadpf_long_fin

    
    df_loadpf_long_fin <- df_loadpf_long_fin
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6
    
    
    plot_data_time_series_plot <- reactive({
      date_range <- reactive(input$date_range)
      start_date_graph <- as.Date(date_range()[1])
      end_date_graph <- start_date_graph + 6
      df_loadpf_long_fin <- filtered_load_data()
      plot_ly() %>%
        add_lines(data = df_loadpf_long_fin,
                  x = ~datetime,
                  y = ~Load, 
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nBaseline Load: ", round(Load)," kW"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red",
                              opacity = 0.62),
                  name = "Baseline Load") %>%
        layout(
          title = "<b>Hourly Electrical Load</b>",
          xaxis = list(title = "Time",
                       rangeslider = list(type = "date"),
                       range = c(start_date_graph, end_date_graph)),
          yaxis = list(title = "Load (in kW)"),
          legend = list(title = "Load Type"),
          showlegend = TRUE,
          font = list(size = 12),
          margin = list(l = 20,
                        r = 20,
                        b = 20,
                        t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        ) #%>%
        # config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", 
        #                                                          "zoomIn2d",
        #                                                          "zoomOut2d",
        #                                                          "autoScale2d",
        #                                                          "resetScale2d",
        #                                                          "hoverClosestCartesian", 
        #                                                          "hoverCompareCartesian",
        #                                                          "lasso2d",
        #                                                          "select2d",
        #                                                          "zoom3d",
        #                                                          "pan3d",
        #                                                          "orbitRotation",
        #                                                          "tableRotation",
        #                                                          "handleDrag3d",
        #                                                          "resetCameraDefault3d",
        #                                                          "resetCameraLastSave3d",
        #                                                          "hoverClosest3d",
        #                                                          "zoomInGeo", 
        #                                                          "zoomOutGeo", 
        #                                                          "resetGeo", 
        #                                                          "hoverClosestGeo", 
        #                                                          "hoverClosestGl2d",
        #                                                          "hoverClosestPie",
        #                                                          "toggleHover",
        #                                                          "resetViews",
        #                                                          "toggleSpikelines"))
    })
    
    output$time_series_plot <- renderPlotly({
      plot_data_time_series_plot()
    })
    
    df_loadpf_long_fin <- filtered_load_data()
    df_loadpf_long_fin_export <- df_loadpf_long_fin %>%
      rename("Baseline Load(in kW)" = Load, "Date_Time" = datetime)
    
    output$download_load_data <- downloadHandler(
      filename = function() {
        "load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_loadpf_long_fin_export, file) 
      }
    )
    
    ##Cost Calculations for Baseline Only
    
    summer_start_month <- reactive({
      match(input$summer_month1, month.name)
    })
    summer_end_month <- reactive({
      match(input$summer_month2, month.name)
    })
    winter_start_month <- reactive({
      match(input$winter_month1, month.name)
    })
    
    winter_end_month <- reactive({
      match(input$winter_month2, month.name)
    })
    
    df_merged <- df_loadpf_long_fin
    df_merged$ogcost <- df_merged$Load
    
    if (is.null(input$tou_ed) && input$tou_mm_y_n == "Yes") { ##No TOU for either energy or demand. But fixed demand charge present.
      
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      df_final$month <- factor(df_final$month, levels = month.name)

    } else if (is.null(input$tou_ed) && input$tou_mm_y_n == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      df_final$cost[df_final$energy_demand == "Demand Cost"] <- 0
      
      df_final <- df_final[df_final$energy_demand != "Demand Cost", ]
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)

      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour)
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    }else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {

      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)

      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour )
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      df_final <- df_final[df_final$energy_demand != "Demand Cost", ]
      
      df_final$month <- factor(df_final$month, levels = month.name)
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0

      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      

      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      df_final <- df_final[df_final$energy_demand != "Demand Cost", ]
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)

      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour )
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)

      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour)
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour)
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })

      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
     
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- 0
      
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- 0
        
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      df_merged <- df_loadpf_long_fin
      
      df_merged$ogcost <- df_merged$Load
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
    }

    
    filtered_cost_data <- reactive({
      req(input$date_range, input$loadpf_file)
      start_date_monthraw <- input$date_range[1]
      end_date_monthraw <- input$date_range[2]
      start_date_month <- format(start_date_monthraw, "%B")
      end_date_month <- format(end_date_monthraw, "%B")
      months_range <- month.name[match(start_date_month, month.name):match(end_date_month, month.name)]
      subset(df_final, month %in% months_range)
    })
    
    
    df_final <- filtered_cost_data()
    
    output$cost_plot <- renderPlotly({
      df_final <- filtered_cost_data()
      gg_cost_plot <- ggplot() +
        geom_bar(
          data = df_final,
          aes(
            x = original_modified,
            y = cost,
            fill = energy_demand,
            text = paste0(original_modified," ",energy_demand ," = ",
                          "\n",scales::dollar(cost,accuracy=1),"/month")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(
          title = "Electricity Costs",
          x = "Baseline Period",
          y = "Total Cost ($/month)"
        ) +
        theme_clean() +
        scale_y_continuous(labels = dollar_format()) +
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_blank(),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.ticks.x = element_blank()
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        ) +
        facet_wrap(.~ month, labeller = labeller(month = function(x) substr(x, 1, 3)),
                   nrow = 1
        ) 

      
      gg_cost_plot2 <- ggplotly(gg_cost_plot, tooltip = "text")
      
      gg_cost_plot2 <- gg_cost_plot2 %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      
      return(gg_cost_plot2)
    })
    
    output$download_costselect_data <- downloadHandler(
      filename = function() {
        "cost_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_final, file) 
      }
    )
    
    df_final <- filtered_cost_data()
    
    df_annual_final <- df_final %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_costs = sum(cost))
    
    annual_cost_for_annual_graph <- df_final %>% 
      group_by(original_modified) %>% 
      summarise(annual_cost = sum(cost))
    
    annual_cost_for_annual_graph_y_axis <- max(annual_cost_for_annual_graph$annual_cost)
    
    output$cost_plot_annual <- renderPlotly({
      df_final <- filtered_cost_data()
      df_annual_final <- df_final %>%
        group_by(original_modified, energy_demand) %>%
        summarise(annual_costs = sum(cost))
      p <- ggplot() +
        geom_bar(
          data = df_annual_final,
          aes(
            x = original_modified,
            y = annual_costs,
            fill = energy_demand,
            text = paste0("Baseline Annual ",energy_demand ," = ","\n",scales::dollar(annual_costs,accuracy=1),"/yr")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(
          title = paste("","Annual Costs Summary"),
          x = "",
          y = "<b>Total Cost ($/yr)</b>"
        ) +
        theme_clean() +
        scale_y_continuous(labels = dollar_format(),
                           limits = c(0,annual_cost_for_annual_graph_y_axis*1.2)) +
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_text(family = "Open Sans",size = 10),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines")
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        )
      
      
      ggplotly(p, tooltip = "text")
    })
    

  })
  

  observe({
    req(input$loadpf_file)
    
    if(input$emissions_type == "U.S. EPA's 2022 eGRID" && input$state != "") {
      df_em_rate_0 <-  read_excel("AllUploadFiles_ToolTesting/States Emission Factors/eGRID/eGRID 2022.xlsx")
      if (input$green_manual == "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784*4
        } else {
          total_hours <- 8760*4
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:45:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "15 min")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_loadpf_long_fin <- df_final_across_events$df_loadpf_long_fin
        
        df_loadpf_long_fin <- df_loadpf_long_fin
        
        updated_df <- reactive({
          if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
            selected_year <- 2022} else {
              selected_year <- as.integer(input$emissions_type)
            }
          df <- df_loadpf_long_fin
          df$datetime <- update(df$datetime, year = selected_year)
          return(df)
        })
        
        df_loadpf_long_fin_1 <- updated_df()
        
        df_em_rate$datetime <-  as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_loadpf_long_fin_1$datetime <-  as.POSIXct(df_loadpf_long_fin_1$datetime, format = "%Y-%m-%d %H:%M")
        
        df_merged <- merge( df_loadpf_long_fin_1,df_em_rate, by = "datetime", all = TRUE)
        
        df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
        
        ## ANNUAL CO2 FOR LOAD SHAPING
        annual_co2 <- df_merged %>%
          drop_na() %>% 
          group_by(month) %>%
          summarise(
            og_co2 = sum(co2em_inv)
          ) 
        
        co2_annual_final <- annual_co2 %>%
          mutate(concat = paste(month, og_co2)) %>%
          distinct(concat, .keep_all = TRUE)
        
        min_co2 <- min(0.8 * co2_annual_final$og_co2)
        max_co2 <- max(1.2 * co2_annual_final$og_co2)
        
        co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
        
      } else if (input$green_manual != "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784
        } else {
          total_hours <- 8760
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:00:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "hour")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_loadpf_long_fin <- df_final_across_events$df_loadpf_long_fin
        
        df_loadpf_long_fin$datetime <-  as.POSIXct(df_loadpf_long_fin$datetime, format = "%Y-%m-%d %H:%M")
        
        updated_df <- reactive({
          if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
            selected_year <- 2022} else {
              selected_year <- as.integer(input$emissions_type)
            }
          df <- df_loadpf_long_fin
          df$datetime <- update(df$datetime, year = selected_year)
          return(df)
        })
        
        df_loadpf_long_fin_1 <- updated_df()
        
        df_em_rate$datetime <-  as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_loadpf_long_fin_1$datetime <-  as.POSIXct(df_loadpf_long_fin_1$datetime, format = "%Y-%m-%d %H:%M")
        
        df_merged <- merge( df_loadpf_long_fin_1,df_em_rate, by = "datetime", all = TRUE)
        
        
        df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load * (1-input$perc_clean * 0.01)) / 1000
        
        ## ANNUAL CO2 FOR LOAD SHAPING
        annual_co2 <- df_merged %>%
          drop_na() %>% 
          group_by(month) %>%
          summarise(
            og_co2 = sum(co2em_inv)
          ) 
        
        co2_annual_final <- annual_co2 %>%
          mutate(concat = paste(month, og_co2)) %>%
          distinct(concat, .keep_all = TRUE)
        
        min_co2 <- min(0.8 * co2_annual_final$og_co2)
        max_co2 <- max(1.2 * co2_annual_final$og_co2)
        
        co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
        
        df_merged_export_co2inventory_initial <- df_merged %>%
          rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
                 "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv)
        
        co2_impact_value <- round((sum(df_merged$co2em_inv, na.rm = T)) / 1000)
        
        
      }
    } else if (input$emissions_type != "U.S. EPA's 2022 eGRID" && input$state != "") {
      state_value <- input$state
      emissions_type <- input$emissions_type
      
      lookup_value <- paste0(state_value,"_",substr(emissions_type, 1, 4))
      
      zipfile <- "AllUploadFiles_ToolTesting/States Emission Factors/Cambium.zip"#
      
      files <- unzip(zipfile, list = TRUE)#
      
      matching_file <- files$Name[grepl(tolower(lookup_value), tolower(basename(files$Name)))]#
      
      # Check if a matching file was found
      if (length(matching_file) == 0) {
        message("No matching file found")
      } else {
        temp_dir <- tempdir()
        unzip(zipfile, files = matching_file, exdir = temp_dir)
        
        # Create the full path to the extracted file
        csv_path <- file.path(temp_dir, matching_file)
        
        # Read the csv file
        df_em_rate_0 <- read.csv(csv_path)
      }
      
      desired_tz <- input$user_time_zone
      
      if (input$green_manual == "Green Button: 15-Minute") {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
        names(df_em_rate)[names(df_em_rate) == "datetime"] <- "date"
        
        df_em_rate <- timeAverage(df_em_rate, avg.time = "15 min", statistic = "mean", fill = T)
        
        df_em_rate <- df_em_rate %>%
          group_by(date) %>%
          summarise(em_rate = em_rate / 4)
        names(df_em_rate)[names(df_em_rate) == "date"] <- "datetime"
        
        df_loadpf_long_fin <- df_final_across_events$df_loadpf_long_fin
        
        df_loadpf_long_fin <- df_loadpf_long_fin
        
        updated_df <- reactive({
          if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
            selected_year <- 2022} else {
              selected_year <- as.integer(input$emissions_type)
            }
          df <- df_loadpf_long_fin
          df$datetime <- update(df$datetime, year = selected_year)
          return(df)
        })
        
        df_loadpf_long_fin_1 <- updated_df()
        
        df_merged <- merge( df_loadpf_long_fin_1,df_em_rate, by = "datetime", all = TRUE)
        
        df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load * (1-input$perc_clean * 0.01)) / 1000
        
        ## ANNUAL CO2 FOR LOAD SHAPING
        annual_co2 <- df_merged %>%
          drop_na() %>% 
          group_by(month) %>%
          summarise(
            og_co2 = sum(co2em_inv)
          ) 
        
        co2_annual_final <- annual_co2 %>%
          mutate(concat = paste(month, og_co2)) %>%
          distinct(concat, .keep_all = TRUE)
        
        min_co2 <- min(0.8 * co2_annual_final$og_co2)
        max_co2 <- max(1.2 * co2_annual_final$og_co2)
        
        co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
      } else {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")

        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
        df_loadpf_long_fin <- df_final_across_events$df_loadpf_long_fin
        
        updated_df <- reactive({
          if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
            selected_year <- 2022} else {
              selected_year <- as.integer(input$emissions_type)
            }
          df <- df_loadpf_long_fin
          df$datetime <- update(df$datetime, year = selected_year)
          return(df)
        })
        
        df_loadpf_long_fin_1 <- updated_df()
        
        df_merged <- merge(df_loadpf_long_fin_1,df_em_rate, by = "datetime", all = TRUE)
       
        df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
        
        
        
        ## ANNUAL CO2 FOR LOAD SHAPING
        annual_co2 <- df_merged %>%
          drop_na() %>% 
          group_by(month) %>%
          summarise(
            og_co2 = sum(co2em_inv)
          ) 
        
        co2_annual_final <- annual_co2 %>%
          mutate(concat = paste(month, og_co2)) %>%
          distinct(concat, .keep_all = TRUE)
        
        min_co2 <-0.8 * min( co2_annual_final$og_co2)
        max_co2 <- 1.2 * max( co2_annual_final$og_co2)
        
        co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
        
        co2_annual_final <- co2_annual_final
      }
      
      co2_annual_final_export_initial <- co2_annual_final %>% 
        select(-concat) %>% 
        rename("Baseline CO2e Emissions Profile (kg/month)" = og_co2) %>% 
        mutate(month = factor(month, levels = month.name)) %>%
        arrange(month)
      
      df_merged_export_co2inventory_initial <- df_merged %>%
        rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
               "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv)

      co2_impact_value <- round((sum(df_merged$co2em_inv, na.rm = T)) / 1000)
      
    }
    
      start_date_graph <- as.Date(date_range()[1])
      end_date_graph <- start_date_graph + 6

       output$co2_emissions_change_plot <- renderPlotly({
        plot_ly() %>%
          add_lines(data = df_merged,
                    x = ~datetime,
                    y = ~co2em_inv,
                    name = "Facility Baseline Emissions",
                    hoverinfo="text",
                    text = ~paste0(datetime, "\nBaseline Emissions: ", round(co2em_inv)," kgCO<sub>2</sub>e/hr"),
                    type = "scatter",
                    mode = "lines",
                    line = list(color = "red", opacity = 0.62)) %>%
          layout(
            title = if_else(input$emissions_type == "<b>U.S. EPA's 2022 eGRID</b>",paste0("<b>2022 </b>","<b>Facility </b>","<b>CO<sub>2</sub>e Emissions</b>"),paste0("<b>",input$emissions_type,"</b>","<b> Facility CO<sub>2</sub>e Emissions</b>")),
            xaxis = list(title = "<b>Time</b>",
                         rangeslider = list(type = "date"),
                         range = c(start_date_graph, end_date_graph)),
            yaxis = list(title = "<b>CO<sub>2</sub>e Emissions (kg/hr)</b>"),
            legend = list(title = "Emissions Type"),
            showlegend = TRUE,
            font = list(size = 12),
            margin = list(l = 20,
                          r = 20,
                          b = 20,
                          t = 40),
            plot_bgcolor = "white",
            paper_bgcolor = "white",
            annotations = list(
              list(
                text = paste("<b>", "Annual CO<sub>2</sub>e emissions: ", format(round(co2_impact_value), big.mark = ","),"MTCO<sub>2</sub>e/yr"),
                xref = "paper",
                yref = "paper",
                x = 1,
                y = 0.95,
                xanchor = "right",
                yanchor = "bottom",
                showarrow = FALSE
              )
            )
          )  %>%
          config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d",
                                                                   "zoomIn2d",
                                                                   "zoomOut2d",
                                                                   "autoScale2d",
                                                                   "resetScale2d",
                                                                   "hoverClosestCartesian",
                                                                   "hoverCompareCartesian",
                                                                   "lasso2d",
                                                                   "select2d",
                                                                   "zoom3d",
                                                                   "pan3d",
                                                                   "orbitRotation",
                                                                   "tableRotation",
                                                                   "handleDrag3d",
                                                                   "resetCameraDefault3d",
                                                                   "resetCameraLastSave3d",
                                                                   "hoverClosest3d",
                                                                   "zoomInGeo",
                                                                   "zoomOutGeo",
                                                                   "resetGeo",
                                                                   "hoverClosestGeo",
                                                                   "hoverClosestGl2d",
                                                                   "hoverClosestPie",
                                                                   "toggleHover",
                                                                   "resetViews",
                                                                   "toggleSpikelines"))
      })
       
       
       output$download_co2em_data  <- downloadHandler(
         filename = function() {
           "hourly_co2_data.xlsx"
         },
         content = function(file) {
           write.xlsx(df_merged_export_co2inventory_initial, file) 
         }
       )
       

       output$co2_plot_annual <- renderPlotly({
         p <-  ggplot() +
           geom_bar(
             data = co2_annual_final,
             aes(
               x = month,
               y = og_co2,
               text = paste0("CO<sub>2</sub>e Emissions",
                             "\nMonth: ",month,
                             "\nEmissions: ", scales::comma(og_co2), " kgCO<sub>2</sub>e/month")
             ),
             fill = "#00313C",
             stat = "identity",
             position = "dodge",
             alpha = 0.9
           ) +
           labs(title = "Facility Baseline Monthly CO<sub>2</sub>e Emissions", x = "Time", y = "Total CO<sub>2</sub>e Emissions (kgCO<sub>2</sub>e/month)") +
           theme_clean() +
           theme(
             text = element_text(family = "Open Sans",size = 14),
             axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
             legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
             legend.text = element_text(family = "Open Sans",size = 12),
             plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
             panel.grid.major = element_line(color = "lightgray"),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(family = "Open Sans",size = 12),
             axis.text.x = element_text(family = "Open Sans",size = 12)
           ) +
           scale_y_continuous(
             labels = comma,
             limits = c(min_co2, max_co2),
             oob = rescale_none
           ) +
           scale_x_discrete(labels = substr(month.name, 1, 3))+ 
           annotate("text",
                    x=11.5,
                    y=max(co2_annual_final$og_co2)*1.1,
                    label = paste("<b>", "Annual CO<sub>2</sub>e emissions: ", format(round(co2_impact_value), big.mark = ","),"MTCO<sub>2</sub>e/yr"))
         
         p2 <- ggplotly(p, tooltip = "text")
         
         p2 <- p2 %>%
           config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
         
         return(p2)
       })
       
   
       output$download_co2em_data_monthly  <- downloadHandler(
         filename = function() {
           "monthly_co2_data.xlsx"
         },
         content = function(file) {
           write.xlsx(co2_annual_final_export_initial, file) 
         }
       )
    
  })
  

  observe({
    if(input$emissions_type == "U.S. EPA's 2022 eGRID" && input$state != "") {
      df_em_rate_0 <-  read_excel("AllUploadFiles_ToolTesting/States Emission Factors/eGRID/eGRID 2022.xlsx")
      if (input$green_manual == "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784*4
        } else {
          total_hours <- 8760*4
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:45:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "15 min")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_em_rate_export <- df_em_rate %>% 
          rename("CO2e Emissions Factor (kg/MWh)" = em_rate)
        
      } else if (input$green_manual != "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784
        } else {
          total_hours <- 8760
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:00:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "hour")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_em_rate_export <- df_em_rate %>% 
          rename("CO2e Emissions Factor (kg/MWh)" = em_rate)
      }
    } else if (input$emissions_type != "U.S. EPA's 2022 eGRID" && input$state != "") {
      state_value <- input$state
      emissions_type <- input$emissions_type
      
      lookup_value <- paste0(state_value,"_",substr(emissions_type, 1, 4))
      
      zipfile <- "AllUploadFiles_ToolTesting/States Emission Factors/Cambium.zip"#
      
      files <- unzip(zipfile, list = TRUE)#
      
      matching_file <- files$Name[grepl(tolower(lookup_value), tolower(basename(files$Name)))]#
      
      # Check if a matching file was found
      if (length(matching_file) == 0) {
        message("No matching file found")
      } else {
        temp_dir <- tempdir()
        unzip(zipfile, files = matching_file, exdir = temp_dir)
        
        # Create the full path to the extracted file
        csv_path <- file.path(temp_dir, matching_file)
        
        # Read the csv file
        df_em_rate_0 <- read.csv(csv_path)
      }
      
      desired_tz <- input$user_time_zone
      
      if (input$green_manual == "Green Button: 15-Minute") {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
        names(df_em_rate)[names(df_em_rate) == "datetime"] <- "date"
        
        df_em_rate <- timeAverage(df_em_rate, avg.time = "15 min", statistic = "mean", fill = T)
        
        df_em_rate <- df_em_rate %>%
          group_by(date) %>%
          summarise(em_rate = em_rate / 4)
        names(df_em_rate)[names(df_em_rate) == "date"] <- "datetime"
        
        df_loadpf_long_fin <- df_final_across_events$df_loadpf_long_fin
        
        df_em_rate_export <- df_em_rate %>% 
          rename("CO2e Emissions Factor (kg/MWh)" = em_rate)
      
      } else {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
        df_em_rate_export <- df_em_rate %>% 
          rename("CO2e Emissions Factor (kg/MWh)" = em_rate)
        
      }
    }
    
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6
    
    output$grid_co2_plot <- renderPlotly({
      plot_ly() %>%
        add_lines(data = df_em_rate,
                  x = ~datetime,
                  y = ~em_rate,
                  name = "Grid Emissions Factor",
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nGrid Emissions Factor: ", round(em_rate)," kgCO<sub>2</sub>e/MWh"),
                  type = "scatter", 
                  mode = "lines",
                  line = list(color = "red", opacity = 0.62)) %>%
        layout(
          title = if_else(input$emissions_type == "U.S. EPA's 2022 eGRID",paste0("<b>2022 </b>","<b>eGRID </b>","<b>CO<sub>2</sub>e Emissions Factor for </b>","<b>",input$state,"</b>"),paste0("<b>NREL Cambium </b>","<b>",input$emissions_type,"</b>","<b> CO<sub>2</sub>e Emissions Factor for </b>","<b>",input$state,"</b>"," ","")),
          xaxis = list(title = "<b>Time</b>",
                       rangeslider = list(type = "date"),
                       range = c(start_date_graph, end_date_graph)),
          yaxis = list(title = "<b>State's CO<sub>2</sub>e Emission Factor (kg/MWh)</b>"),
          legend = list(title = "Emissions Type"),
          showlegend = TRUE,
          font = list(size = 12),
          margin = list(l = 20,
                        r = 20,
                        b = 20,
                        t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        ) %>%
        add_annotations(
          text =if_else(input$emissions_type == "U.S. EPA's 2022 eGRID","<b>eGRID annual values selected. <br> There will be no variation in\n emission factors.",""),
          align = "left",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          x = 1.15,  # Adjust X position based on your legend placement
          y = 0.2   # Adjust Y position for placement below legend
        ) %>% 
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d",
                                                                 "zoomIn2d",
                                                                 "zoomOut2d",
                                                                 "autoScale2d",
                                                                 "resetScale2d",
                                                                 "hoverClosestCartesian",
                                                                 "hoverCompareCartesian", 
                                                                 "lasso2d",
                                                                 "select2d",
                                                                 "zoom3d",
                                                                 "pan3d", 
                                                                 "orbitRotation",
                                                                 "tableRotation",
                                                                 "handleDrag3d",
                                                                 "resetCameraDefault3d", 
                                                                 "resetCameraLastSave3d",
                                                                 "hoverClosest3d", 
                                                                 "zoomInGeo",
                                                                 "zoomOutGeo", 
                                                                 "resetGeo",
                                                                 "hoverClosestGeo",
                                                                 "hoverClosestGl2d",
                                                                 "hoverClosestPie",
                                                                 "toggleHover",
                                                                 "resetViews",
                                                                 "toggleSpikelines"))
    })
    
    output$grid_ef  <- downloadHandler(
      filename = function() {
        "grid_co2_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_em_rate_export, file) 
      }
    )
    
  })
 
  observeEvent(input$plot_button, {
    desired_tz <- input$user_time_zone
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    
    if (input$shiftData == "Manually Enter Shaped Load(s)") {
      data <-
        data.frame(
          NumericValue = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("numeric_", i)]])),
          SliderMin_from = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_from", i)]][1])),
          SliderMax_from = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_from", i)]][2])),
          SliderMin_to = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_to", i)]][1])),
          SliderMax_to = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_to", i)]][2]))
        )
    } else {
      data_file <- input$file_upload_shiftinput
      if (is.null(data_file)) {
        return(NULL)
      }
      data <- read_excel(data_file$datapath)
      data <- data %>%
        rename(SliderMin_to = "Time Range To Start",
               SliderMax_to = "Time Range To End",
               SliderMin_from = "Time Range From Start",
               SliderMax_from = "Time Range From End",
               NumericValue = "Flexible Load (in kW)")
      
    }
    
    loadpf_file <- input$loadpf_file
    
    if (is.null(loadpf_file)) {
      return(NULL)
    }
    
    if(input$emissions_type == "U.S. EPA's 2022 eGRID" && input$state != "") {
      df_em_rate_0 <-  read_excel("AllUploadFiles_ToolTesting/States Emission Factors/eGRID/eGRID 2022.xlsx")
      if (input$green_manual == "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784*4
        } else {
          total_hours <- 8760*4
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:45:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "15 min")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_em_rate_15 <-  df_em_rate
        
      } else if (input$green_manual != "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784
        } else {
          total_hours <- 8760
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:00:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "hour")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
      }
    } else if (input$emissions_type != "U.S. EPA's 2022 eGRID" && input$state != "") {
      state_value <- input$state
      emissions_type <- input$emissions_type
      
      lookup_value <- paste0(state_value,"_",substr(emissions_type, 1, 4))
      
      zipfile <- "AllUploadFiles_ToolTesting/States Emission Factors/Cambium.zip"#
      
      files <- unzip(zipfile, list = TRUE)#
      
      matching_file <- files$Name[grepl(tolower(lookup_value), tolower(basename(files$Name)))]#
      
      # Check if a matching file was found
      if (length(matching_file) == 0) {
        message("No matching file found")
      } else {
        temp_dir <- tempdir()
        unzip(zipfile, files = matching_file, exdir = temp_dir)
        
        # Create the full path to the extracted file
        csv_path <- file.path(temp_dir, matching_file)
        
        # Read the csv file
        df_em_rate_0 <- read.csv(csv_path)
      }
      
      desired_tz <- input$user_time_zone
      
      if (input$green_manual == "Green Button: 15-Minute") {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
        names(df_em_rate)[names(df_em_rate) == "datetime"] <- "date"
        
        df_em_rate <- timeAverage(df_em_rate, avg.time = "15 min", statistic = "mean", fill = T)
        
        df_em_rate <- df_em_rate %>%
          group_by(date) %>%
          summarise(em_rate = em_rate / 4)
        names(df_em_rate)[names(df_em_rate) == "date"] <- "datetime"
        
        df_em_rate_15 <- df_em_rate
      } else {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
        
      }
    }
    
    if (input$green_manual == "Custom Hourly Load") {
      
      df_loadpf_long_fin <- read_excel(loadpf_file$datapath)
      
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        rename(Load = `Load (in kW)`,
               datetime = Date_Time)
      
      df_loadpf_long_fin$datetime <- as.POSIXct(df_loadpf_long_fin$datetime, format = "%d/%m/%Y %H:%M")
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
  
      df_merged <- merge(df_loadpf_long_fin,df_em_rate , by = "datetime", all = TRUE)
      
      df_merged <- na.omit(df_merged)
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      
      df_merged$mod_load <- df_merged$Load
      df_merged_annual <- df_merged
      
    } else if (input$green_manual == "12 Months Utility Bills") {
      df_loadpf <- read_excel(loadpf_file$datapath, range =  "A1:D13")
      
      df_loadpf <- janitor::clean_names(df_loadpf)
      
      df_loadpf$energy_usage_k_wh <- gsub(",", "", df_loadpf$energy_usage_k_wh)
      df_loadpf$energy_usage_k_wh <- as.numeric(df_loadpf$energy_usage_k_wh)
      df_loadpf$billed_demand_k_w <- gsub(",", "", df_loadpf$billed_demand_k_w)
      df_loadpf$billed_demand_k_w <- as.numeric(df_loadpf$billed_demand_k_w)
      average_demand <- mean(df_loadpf$billed_demand_k_w) # Average annual billed demand
      work_on_weekends_0 <- read_excel(loadpf_file$datapath, range = "G6", col_names = F)
      work_on_weekends <- work_on_weekends_0$...1
      shift_start_time_0 <- read_excel(loadpf_file$datapath, range = "c108", col_names = F)
      start_time <- shift_start_time_0$...1
      shift_end_time_0 <- read_excel(loadpf_file$datapath, range = "c109", col_names = F)
      end_time <- shift_end_time_0$...1
      hours_of_operation_day_hrs_day <- if_else(end_time==start_time,24,if_else(end_time<start_time,24-start_time+end_time,end_time-start_time))
      non_working_hours <- abs(24-hours_of_operation_day_hrs_day)
      peak_hours <- 1
      mid_low_hours <- hours_of_operation_day_hrs_day-peak_hours
      pd_0 <- read_excel(loadpf_file$datapath, range = "c110", col_names = F)
      pd <- pd_0$...1 
      
      
      annual_oh <- if_else(work_on_weekends == "N", hours_of_operation_day_hrs_day * 52 * 5, hours_of_operation_day_hrs_day * 365) # Calculating annual operating hours
      
      desired_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # order of months
      
      df_loadpf_1 <- column_to_rownames(df_loadpf, var = "month") # make column names as rownames
      
      df_loadpf_sorted <- df_loadpf_1[desired_order, , drop = FALSE] %>%
        rownames_to_column("month") # sort the bills from jan to dec
      
      days <- data.frame(days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) # vectro data for number of days in each month
      order <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) # order of months from 1 to 12; jan to dec
      
      df_loadpf_2 <- cbind(df_loadpf_sorted, days, order) # bind the three dataframes
      
      if (work_on_weekends == "Y") {
        
        df_loadpf_3 <- df_loadpf_2 %>%
          mutate(
            kwhpd = energy_usage_k_wh/days,
            baseline_demand_pd = 0.3*billed_demand_k_w,
            peak_demand_pd = billed_demand_k_w,
            mid_demand_pd = (kwhpd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
          )
        
        df_loadpf_4 <-  df_loadpf_3 %>% 
          pivot_longer(cols = !c("month":"kwhpd"),
                       names_to = "energy_type",
                       values_to = "kW")
        
        # Create a data frame with a 24-hour column
        hours_df <- data.frame(hour = 0:23) 
        
        hours_df_1 <- hours_df %>% 
          mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                         hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                         hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
        
        hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
        
        month_index <- data.frame(month = rep(month.name, each = 24))
        
        hours_df_3 <- cbind(month_index,hours_df_2)
        
        df_loadpf_5 <- data.frame(seq(
          as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
          as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz),
          by = "hour"
        )) %>%
          rename(datetime = nth(names(.), 1)) %>%
          mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                 hour = (hour(datetime) ) %% 24 ) %>%
          left_join(hours_df_3,by = c("month","hour")) %>% 
          left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
          mutate(Day = day(datetime))
        
        df_loadpf_long_fin_0 <- df_loadpf_5
        
      } else {
        
        # Define the count_weekdays function
        count_weekdays <- function(year, month) {
          # Generate a sequence of dates for the given month
          start_date <- make_date(year, month, 1)
          end_date <- start_date + months(1) - days(1)
          dates <- seq.Date(start_date, end_date, by = "day")
          
          # Filter weekdays
          weekdays_count <- weekdays(dates) %>%
            table() %>%
            as.data.frame() %>%
            filter(!`.` %in% c("Saturday", "Sunday")) %>%
            summarize(count = sum(Freq))
          
          return(weekdays_count$count)
        }
        
        df_loadpf_3 <- df_loadpf_2 %>%
          mutate(
            no_of_weekdays = mapply(count_weekdays, year, order)
          )
        
        df_loadpf_3.5 <- df_loadpf_3 %>%
          mutate(
            no_of_weekends = days- no_of_weekdays,
            baseline_demand_pd = 0.3*billed_demand_k_w,
            kwh_wknd = baseline_demand_pd*24*no_of_weekends,
            kwh_wkd = energy_usage_k_wh-kwh_wknd,
            kwhpd_wknd = baseline_demand_pd,
            kwhpd_wkd = kwh_wkd/no_of_weekdays,
            peak_demand_pd = billed_demand_k_w,
            mid_demand_pd = (kwhpd_wkd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
          ) %>% 
          relocate(baseline_demand_pd,.after = kwhpd_wkd)
        
        df_loadpf_4 <-  df_loadpf_3.5 %>% 
          pivot_longer(cols = !c("month":"kwhpd_wkd"),
                       names_to = "energy_type",
                       values_to = "kW")
        
        # Create a data frame with a 24-hour column
        hours_df <- data.frame(hour = 0:23) 
        
        hours_df_1 <- hours_df %>% 
          mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                         hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                         hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
        
        hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
        
        month_index <- data.frame(month = rep(month.name, each = 24))
        
        hours_df_3 <- cbind(month_index,hours_df_2)
        
        df_loadpf_5 <- data.frame(seq(
          as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
          as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz),
          by = "hour"
        )) %>%
          rename(datetime = nth(names(.), 1)) %>%
          mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                 hour = (hour(datetime) ) %% 24 ) %>%
          left_join(hours_df_3,by = c("month","hour")) %>% 
          left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
          mutate(Day = day(datetime))
        
        
        df_loadpf_7 <- df_loadpf_5 %>% 
          mutate(day = weekdays(datetime),
                 energy_type = if_else(day == "Saturday" | day == "Sunday", "baseline_demand_pd",energy_type),
                 kW = if_else(day == "Saturday" | day == "Sunday", 0.3*billed_demand_k_w,kW)
          )
        
        df_loadpf_long_fin_0 <- df_loadpf_7
      }
      
      df_loadpf_long_fin <- df_loadpf_long_fin_0 %>%
        select(datetime,month,Day,hour,kW) %>% 
        rename(Load = kW,
               hours = hour)
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
      
      
      df_merged <- df_loadpf_long_fin %>%
        full_join(df_em_rate, by = "datetime")
      
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged[, 5]* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      
      df_merged_annual <- df_merged
      
    } else if (input$green_manual == "Green Button: Hourly") {
      
      xml_address <- loadpf_file$datapath
      loadpf <- read_xml(xml_address)
      xml_text <- xml_text(loadpf)
      ns <- c(espi = "http://naesb.org/espi")
      interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      extract_and_convert <- function(node, xpath) {
        values <- as.integer(xml_text(xml_find_all(node, xpath)))
        return(values)
      }
      
      extract_and_parse_datetime <- function(node, xpath) {
        values <- xml_text(xml_find_all(node, xpath))
        values_as_integer <- as.integer(values)
        parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
        return(parsed_values)
      }
      
      starts <- interval_readings %>%
        xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
        extract_and_parse_datetime(xpath = ".")
      
      values <- interval_readings %>%
        xml_find_all(".//espi:value", ns) %>%
        extract_and_convert(xpath = ".")
      
      df_loadpf <- data.frame(
        DATE = starts,
        USAGE = values
      )
      
      df_loadpf_long <- df_loadpf %>%
        mutate(
          DATE = (DATE),
          Load = USAGE,
          datetime = DATE,
          month = format(datetime, "%B"),
          hour = format(as.POSIXct(datetime), format = "%H:%M")
        )
      
      df_loadpf_long <- df_loadpf_long %>%
        select(Load, month, datetime, hour)
      
      
      df_loadpf_long_fin <- df_loadpf_long
      
      df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
      
      df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
      
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        distinct(datetime, .keep_all = TRUE)
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
      
     df_merged <- merge( df_loadpf_long_fin,df_em_rate, by = "datetime", all = TRUE)
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      
      df_merged <- df_merged %>%
        mutate(hour = strftime(datetime, format = "%H:%M"))
      
      df_merged_annual <- df_merged
      
      
    } else if (input$green_manual == "Green Button: 15-Minute") {
      xml_address <- loadpf_file$datapath
      loadpf <- read_xml(xml_address)
      xml_text <- xml_text(loadpf)
      ns <- c(espi = "http://naesb.org/espi")
      interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      extract_and_convert <- function(node, xpath) {
        values <- as.integer(xml_text(xml_find_all(node, xpath)))
        return(values)
      }
      
      extract_and_parse_datetime <- function(node, xpath) {
        values <- xml_text(xml_find_all(node, xpath))
        values_as_integer <- as.integer(values)
        parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
        return(parsed_values)
      }
      
      starts <- interval_readings %>%
        xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
        extract_and_parse_datetime(xpath = ".")
      
      values <- interval_readings %>%
        xml_find_all(".//espi:value", ns) %>%
        extract_and_convert(xpath = ".")
      
      df_loadpf <- data.frame(
        DATE = starts,
        USAGE = values
      )
      
      df_loadpf_long <- df_loadpf %>%
        mutate(
          DATE = (DATE),
          Load = USAGE,
          datetime = DATE,
          month = format(datetime, "%B"),
          hour = format(as.POSIXct(datetime), format = "%H:%M")
        )
      
      df_loadpf_long <- df_loadpf_long %>%
        select(Load, month, datetime, hour)
      
      
      df_loadpf_long_fin <- df_loadpf_long
      
      df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
      
      df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
      
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        distinct(datetime, .keep_all = TRUE)
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
     
      df_merged <- merge( df_loadpf_long_fin, df_em_rate_15,by = "datetime", all = TRUE)
      
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      
    
      df_merged_annual <- df_merged
    } # Make this function that sorts the 1 and 0. Half zeros in either end, and all ones in the middle
    
    
    date_range <- reactive(input$date_range)
    if (!is.null(date_range())) {
      start_date <- date_range()[1]
      end_date <- date_range()[2]+1
      df_merged <- subset(df_merged, datetime >= start_date & datetime < end_date)
      df_merged <- df_merged
    }
    
    for  (i in 1:nrow(data)) {
      if (input$shiftData == "Upload Existing Spreadsheet") {
        time_to_min <- data$SliderMin_to[i]
        time_to_min <- format(as.POSIXct(time_to_min, format = "%H:%M"), format = "%H:%M")
        time_to_min <- as.ITime(time_to_min)
        time_to_max <- data$SliderMax_to[i]
        time_to_max <- format(as.POSIXct(time_to_max, format = "%H:%M"), format = "%H:%M")
        time_to_max <- as.ITime(time_to_max)
        
        time_from_min <- data$SliderMin_from[i]
        time_from_min <- format(as.POSIXct(time_from_min, format = "%H:%M"), format = "%H:%M")
        time_from_min <- as.ITime(time_from_min)
        time_from_max <- data$SliderMax_from[i]
        time_from_max <- format(as.POSIXct(time_from_max, format = "%H:%M"), format = "%H:%M")
        time_from_max <- as.ITime(time_from_max)
        scl_value <- data$NumericValue[i]
        
        if (!exists("work_on_weekends")) {
          work_on_weekends <- 'Placeholder for non 12-Month Template Data'  
        }
        
        if (input$work_on_weekends == "Yes" | work_on_weekends == "Y") {
          df_merged$day_number <- yday(df_merged$datetime)
          
          is_weekend_test <- function(date) {
            weekday <- weekdays(date)
            return(weekday %in% c("Saturday", "Sunday"))
          }
          
          
          df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
          
          df_merged_daysscl <- df_merged %>%
            group_by(day_number) %>%
            filter(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max) %>%
            mutate(temp_id = ifelse(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max, "shiftfromn_hours", NA_character_)) %>%
            ungroup()
          
          
          df_merged_daysscl$temp_id_2 <- paste(df_merged_daysscl$day_number, df_merged_daysscl$temp_id)
          
          df_merged_daysscl <- df_merged_daysscl %>%
            group_by(temp_id_2) %>%
            mutate(min_load = min(Load)) %>%
            filter(min_load >= scl_value) %>%
            ungroup()
          
          if (nrow(df_merged_daysscl) == 0) {
            days_to_keep <- df_merged$day_number
            scl_value <- 0
            shinyalert("Warning", "The flexible load you have specified is greater than the load available over the Time Range From Interval. Please enter a lower value and try again", type = "warning")
          } else {
            days_to_keep <- df_merged_daysscl$day_number
          }
          
          df_merged1 <- df_merged %>%
            group_by(day_number) %>%
            mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                   mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_from_min & as.ITime(time) <= time_from_max, mod_load - scl_value, mod_load)) %>%
            select(-time) %>% 
            ungroup()
          
          
          
          df_merged2 <- df_merged1 %>%
            group_by(day_number) %>%
            mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                   mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max , mod_load + scl_value, mod_load)) %>%
            ungroup() %>%
            select(-day_number, -is_weekend_test, -time)
        } else {
          df_merged$day_number <- yday(df_merged$datetime)
          
          is_weekend_test <- function(date) {
            weekday <- weekdays(date)
            return(weekday %in% c("Saturday", "Sunday"))
          }
          
          
          df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
          
          df_merged_daysscl <- df_merged %>%
            group_by(day_number) %>%
            filter(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max) %>%
            mutate(temp_id = ifelse(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max, "shiftfromn_hours", NA_character_)) %>%
            ungroup()
          
          
          df_merged_daysscl$temp_id_2 <- paste(df_merged_daysscl$day_number, df_merged_daysscl$temp_id)
          
          df_merged_daysscl <- df_merged_daysscl %>%
            group_by(temp_id_2) %>%
            mutate(min_load = min(Load)) %>%
            filter(min_load >= scl_value) %>%
            ungroup()
          
          df_merged_daysscl <- df_merged_daysscl %>%
            filter(!is_weekend_test)
          
          if (nrow(df_merged_daysscl) == 0) {
            days_to_keep <- df_merged$day_number
            scl_value <- 0
            shinyalert("Warning", "The flexible load you have specified is greater than the load available over the Time Range From Interval. Please enter a lower value and try again", type = "warning")
          } else {
            days_to_keep <- df_merged_daysscl$day_number
          }
          
          df_merged1 <- df_merged %>%
            group_by(day_number) %>%
            mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                   mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_from_min & as.ITime(time) <= time_from_max, mod_load - scl_value, mod_load)) %>%
            select(-time) %>% 
            ungroup()
          
          df_merged2 <- df_merged1 %>%
            group_by(day_number) %>%
            mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                   mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max , mod_load + scl_value, mod_load)) %>%
            ungroup() %>%
            select(-day_number, -is_weekend_test,-time)
        }
        
        df_merged <- df_merged2
        
      } else {
        time_from_min <- data$SliderMin_from[i]
        time_from_max <- data$SliderMax_from[i]
        time_from_min <- format(as.POSIXct(time_from_min,origin = "1970-01-01"), format = "%H:%M")
        time_from_min <- as.ITime(time_from_min)
        time_from_max <- format(as.POSIXct(time_from_max, origin = "1970-01-01"), format = "%H:%M")
        time_from_max <- as.ITime(time_from_max)
        time_to_min <- data$SliderMin_to[i]
        time_to_max <- data$SliderMax_to[i]
        time_to_min <- format(as.POSIXct(time_to_min, origin = "1970-01-01",), format = "%H:%M")
        time_to_min <- as.ITime(time_to_min)
        time_to_max <- format(as.POSIXct(time_to_max, origin = "1970-01-01"), format = "%H:%M")
        time_to_max <- as.ITime(time_to_max)
        scl_value <- data$NumericValue[i]
        
        if (!exists("work_on_weekends")) {
          work_on_weekends <- 'Placeholder for non 12-Month Template Data'  
        }
          
          if (input$work_on_weekends == "Yes" | work_on_weekends == "Y") {
            df_merged$day_number <- yday(df_merged$datetime)
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }
            
            
            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged_daysscl <- df_merged %>%
              group_by(day_number) %>%
              filter(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max) %>%
              mutate(temp_id = ifelse(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max, "shiftfromn_hours", NA_character_)) %>%
              ungroup()
            
            
            df_merged_daysscl$temp_id_2 <- paste(df_merged_daysscl$day_number, df_merged_daysscl$temp_id)
            
            df_merged_daysscl <- df_merged_daysscl %>%
              group_by(temp_id_2) %>%
              mutate(min_load = min(Load)) %>%
              filter(min_load >= scl_value) %>%
              ungroup()
            
            if (nrow(df_merged_daysscl) == 0) {
              days_to_keep <- df_merged$day_number
              scl_value <- 0
              shinyalert("Warning", "The flexible load you have specified is greater than the load available over the Time Range From Interval. Please enter a lower value and try again", type = "warning")
            } else {
              days_to_keep <- df_merged_daysscl$day_number
            }
            
            df_merged1 <- df_merged %>%
              group_by(day_number) %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_from_min & as.ITime(time) <= time_from_max, mod_load - scl_value, mod_load)) %>%
              select(-time) %>% 
              ungroup()
            
            
            
            df_merged2 <- df_merged1 %>%
              group_by(day_number) %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max , mod_load + scl_value, mod_load)) %>%
              ungroup() %>%
              select(-day_number, -is_weekend_test,-time)
          } else {
            df_merged$day_number <- yday(df_merged$datetime)
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }
            
            
            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged_daysscl <- df_merged %>%
              group_by(day_number) %>%
              filter(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max) %>%
              mutate(temp_id = ifelse(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max, "shiftfromn_hours", NA_character_)) %>%
              ungroup()
            
            
            df_merged_daysscl$temp_id_2 <- paste(df_merged_daysscl$day_number, df_merged_daysscl$temp_id)
            
            df_merged_daysscl <- df_merged_daysscl %>%
              group_by(temp_id_2) %>%
              mutate(min_load = min(Load)) %>%
              filter(min_load >= scl_value) %>%
              ungroup()
            
            df_merged_daysscl <- df_merged_daysscl %>%
              filter(!is_weekend_test)
            
            if (nrow(df_merged_daysscl) == 0) {
              days_to_keep <- df_merged$day_number
              scl_value <- 0
              shinyalert("Warning", "The flexible load you have specified is greater than the load available over the Time Range From Interval. Please enter a lower value and try again", type = "warning")
            } else {
              days_to_keep <- df_merged_daysscl$day_number
            }
            
            df_merged1 <- df_merged %>%
              group_by(day_number) %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_from_min & as.ITime(time) <= time_from_max, mod_load - scl_value, mod_load)) %>%
              select(-time) %>% 
              ungroup()
            
            df_merged2 <- df_merged1 %>%
              group_by(day_number) %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(day_number %in% days_to_keep & as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max , mod_load + scl_value, mod_load)) %>%
              ungroup() %>%
              select(-day_number, -is_weekend_test,-time)
          }
        
        df_merged <- df_merged2
      }
    }
    
    df_merged$co2_em_inv_mod <- (df_merged$em_rate * df_merged$mod_load* (1-input$perc_clean * 0.01)) / 1000
    total_co2_saved <- sum(df_merged$co2em_inv) - sum(df_merged$co2_em_inv_mod)
    
    df_merged_lcac <- df_merged
    df_merged_lcac$co2_impact <-  df_merged_lcac$co2em_inv - df_merged_lcac$co2_em_inv_mod
    
    df_merged_lcac <- df_merged_lcac %>%
      group_by(month) %>%
      summarise(
        co2_impact = sum(co2_impact)
      )
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6

    output$time_series_plot <- renderPlotly({
      plot_ly() %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~mod_load,
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nModified Load: ", round(mod_load)," kW"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "blue",dash = "dash"),
                  name = "Modified Load") %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~Load,
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nBaseline Load: ", round(Load)," kW"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red",
                              opacity = 0.62),
                  name = "Baseline Load") %>%
        layout(
          title = "<b>Hourly Electrical Load</b>",
          xaxis = list(title = "<b>Time</b>",
                       rangeslider = list(type = "date"),
                       range = c(start_date_graph,
                                 end_date_graph)),
          yaxis = list(title = "<b>Load (in kW)</b>"),
          legend = list(title = "Load Type"),
          showlegend = TRUE,
          font = list(size = 12),
          margin = list(l = 20, 
                        r = 20,
                        b = 20, 
                        t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })
    
    df_merged_export <- df_merged %>%
      rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
             "Modified Load(in kW)" = mod_load,
             "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv,
             "Modified CO2e Emissions Profile (kg/hr)" = co2_em_inv_mod)
    
        output$download_load_data <- downloadHandler(
      filename = function() {
        "modified_load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    co2_impact_value <- round((sum(df_merged$co2em_inv, na.rm = T) - sum(df_merged$co2_em_inv_mod, na.rm = T)) / 1000)
    
    if (co2_impact_value > 0) {
      impact_text <- paste("Baseline CO<sub>2</sub>e: ",format(round(sum(df_merged$co2em_inv, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Modified CO<sub>2</sub>e: ",format(round(sum(df_merged$co2_em_inv_mod, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Avoided CO<sub>2</sub>e: ")
    } else {
      impact_text <- paste("Baseline CO<sub>2</sub>e: ",format(round(sum(df_merged$co2em_inv, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Modified CO<sub>2</sub>e: ",format(round(sum(df_merged$co2_em_inv_mod, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Added CO<sub>2</sub>e: ")
    }
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6
    
    output$co2_emissions_change_plot <- renderPlotly({
      plot_ly() %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~co2_em_inv_mod,
                  name = "Modified Emissions",
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nModified Emissions: ", round(co2_em_inv_mod)," kgCO<sub>2</sub>e/hr"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "green",dash = "dash")) %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~co2em_inv,
                  name = "Baseline Emissions",
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nBaseline Emissions: ", round(co2em_inv)," kgCO<sub>2</sub>e/hr"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red",
                              opacity = 0.62)) %>%
        layout(
          title =  if_else(input$emissions_type == "<b>U.S. EPA's 2022 eGRID</b>",paste0("<b>2022 </b>","<b>Facility Baseline and Modified </b>","<b>CO<sub>2</sub>e Emissions</b>"),paste0("<b>",input$emissions_type,"</b>","<b> Facility Baseline and Modified CO<sub>2</sub>e Emissions</b>")),
          xaxis = list(title = "<b>Time</b>",
                       rangeslider = list(type = "date"),
                       range = c(start_date_graph,
                                 end_date_graph)),
          yaxis = list(title = "<b>CO<sub>2</sub>e Emissions (kg/hr)</b>"),
          legend = list(title = "Emissions Type"),
          showlegend = TRUE,
          font = list(size = 12),
          margin = list(l = 20, 
                        r = 20,
                        b = 20,
                        t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          annotations = list(
            list(
              text = paste("<b>", impact_text, format(abs(round(co2_impact_value)), big.mark = ","),"MTCO<sub>2</sub>e/yr"),
              xref = "paper",
              yref = "paper",
              x = 1,
              y = 0.90,
              xanchor = "right",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
        ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
    })
    
    output$download_co2em_data <- downloadHandler(
      filename = function() {
        "hourly_co2_em_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    
    ## COST CALCULATIONS
    df_merged$ogcost <- df_merged$Load
    df_merged$modcost <- df_merged$mod_load

    summer_start_month <- reactive({
      match(input$summer_month1, month.name)
    })
    summer_end_month <- reactive({
      match(input$summer_month2, month.name)
    })
    winter_start_month <- reactive({
      match(input$winter_month1, month.name)
    })
    
    winter_end_month <- reactive({
      match(input$winter_month2, month.name)
    })
    
    if (is.null(input$tou_ed) && input$tou_mm_y_n == "Yes") { ##No TOU for either energy or demand. But fixed demand charge present.
      
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate(),
               modcost = modcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load),
                  modified_demandcost = sum(demand_billed_modload))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost),
                  modified_energycost = sum(modcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (is.null(input$tou_ed) && input$tou_mm_y_n == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate(),
               modcost = modcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate(),
               modcost = modcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load),
                  modified_demandcost = sum(demand_billed_modload))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost),
                  modified_energycost = sum(modcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour)
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    }else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour )
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      
      
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * 0
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * 0
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour )
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * 0
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour)
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * 0
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour)
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- 0
        
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- 0
        
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
    }
    
    print(df_final)
    print(df_merged_lcac)
    df_lcac <- merge(
      x = merged_df_costfinal_lcac, y = df_merged_lcac,
      by = "month", all.x = TRUE
    )
    
    
    df_lcac$co2_impact[df_lcac$co2_impact <= 0] <- 0
    
    df_lcac$co2_impact <- abs(df_lcac$co2_impact)
    
    df_lcac$lcac <- df_lcac$cost / df_lcac$co2_impact
    
    df_lcac[is.na(df_lcac)] <- 0
    
    df_lcac$lcac <- df_lcac$lcac / 1000
    
    df_lcac$co2_impact <- df_lcac$co2_impact / 1000
    
    df_lcac$month <- factor(df_lcac$month, levels = month.name)
    
    df_lcac <- na.omit(df_lcac)
    
    annual_costsavings <- sum(df_lcac$cost)

    df_lcac <- df_lcac %>%
      drop_na() %>% 
      group_by(month) %>%
      summarize(
        co2_impact = mean(co2_impact),
        lcac = sum(lcac)
      )
    
    df_lcac$month <- factor(df_lcac$month, levels = month.name)
    lcac_macc <- df_lcac %>%
      ggmacc(
        abatement = co2_impact, mac = lcac, fill = month, cost_threshold = 0,
        zero_line = TRUE, threshold_line = TRUE
      )
    
    lcac_macc <- lcac_macc +
      labs(
        x = "Abatement MTCO<sub>2</sub>e",
        y = "Abatement Cost $/MTCO<sub>2</sub>e",
        title = "Monthly Abatement Cost Plot",
        fill = "Month"
      ) +
      theme_clean()
    
    df_lcac <- na.omit(df_lcac)
    
    annual_co2savings <- sum(df_lcac$co2_impact)
    
    lcac_macc_plotly <- ggplotly(lcac_macc)
    
   if( any(df_lcac$co2_impact > 0)) {
    
  lcac_macc_plotly <- lcac_macc_plotly %>%
      layout(showlegend = TRUE, legend = list(font = list(size = 10)),
        annotations = list(
          list(
            x =0,
            y = 0.99,
            text = if_else(annual_costsavings<=0,
                           paste("Annual Costs Reduction = $",format(round(-annual_costsavings), big.mark = ",", scientific = FALSE),"/yr",sep = ""),
                           paste("Annual Costs Increase = $",format(round(annual_costsavings), big.mark = ",", scientific = FALSE),"/yr",sep = "")),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "top"
          ),
          list(
            x =0,
            y = 0.85,
            text = if_else(annual_co2savings>=0,
                           paste("Annual CO<sub>2</sub>e Reduction = ",format(round(annual_co2savings), big.mark = ",", scientific = FALSE)," MTCO<sub>2</sub>e/yr",sep = ""),
                           paste("Annual CO<sub>2</sub>e Increase = ",format(round(-annual_co2savings), big.mark = ",", scientific = FALSE)," MTCO<sub>2</sub>e/yr",sep = "")),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom"
          )
         )
      )
 
    output$lcac_plot <- renderPlotly({
      lcac_macc_plotly <- ggplotly(lcac_macc_plotly)
      lcac_macc_plotly <- lcac_macc_plotly %>%
        config(displayModeBar = T,
               modeBarButtonsToRemove = list("zoom2d",
                                             "zoomIn2d",
                                             "zoomOut2d",
                                             "autoScale2d",
                                             "resetScale2d",
                                             "hoverClosestCartesian",
                                             "hoverCompareCartesian", 
                                             "lasso2d",
                                             "select2d",
                                             "zoom3d",
                                             "pan3d",
                                             "orbitRotation",
                                             "tableRotation",
                                             "handleDrag3d",
                                             "resetCameraDefault3d",
                                             "resetCameraLastSave3d", 
                                             "hoverClosest3d",
                                             "zoomInGeo",
                                             "zoomOutGeo",
                                             "resetGeo", 
                                             "hoverClosestGeo",
                                             "hoverClosestGl2d",
                                             "hoverClosestPie", 
                                             "toggleHover", 
                                             "resetViews", 
                                             "toggleSpikelines"))
      return(lcac_macc_plotly)
    })  }  else {
      output$lcac_plot <- renderPlotly({
        plotly_empty() %>% layout(
          annotations = list(
            text = "The 'Added Load' inputs do not lead to emission savings.\n Therefore, we cannot create an abatement plot.",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 20)
          )
        )
      })
    }

   cost_savings_summary <- df_final %>% 
     drop_na() %>% 
      group_by(original_modified, energy_demand) %>% 
      summarise(cost = sum(cost))
    
    usage_cost_savings_table_test <- cost_savings_summary %>% 
      filter(energy_demand == "Usage Cost") %>% 
      select(-energy_demand) %>% 
      pivot_wider(names_from = original_modified,
                  values_from = cost)
    
    
    usage_cost_savings_table <- cost_savings_summary %>% 
      filter(energy_demand == "Usage Cost") %>% 
      select(-energy_demand) %>% 
      pivot_wider(names_from = original_modified,
                  values_from = cost) %>% 
      mutate(`Baseline - Modified` = Baseline - Modified)
    
    usage_cost_savings <- usage_cost_savings_table$`Baseline - Modified`
    
    demand_cost_savings_table <- cost_savings_summary %>% 
      filter(energy_demand == "Demand Cost") %>% 
      select(-energy_demand) %>% 
      pivot_wider(names_from = original_modified,
                  values_from = cost) %>% 
      mutate(`Baseline - Modified` = Baseline - Modified)
    
    demand_cost_savings <- demand_cost_savings_table$`Baseline - Modified`
    
    highest_value <- df_final %>% 
      pivot_wider(names_from = energy_demand,
                  values_from = cost) 
    
    highest_value <- highest_value %>% 
      mutate(total_cost = `Usage Cost`+`Demand Cost`)
    
    highest_value_for_graph <- max(highest_value$total_cost)
    
    
    output$cost_plot <- renderPlotly({
      gg_cost_plot <- ggplot() +
        geom_bar(
          data = df_final,
          aes(
            x = original_modified,
            y = cost,
            fill = energy_demand,
            text = paste0(original_modified," ",energy_demand ," = ",
                          "\n",scales::dollar(cost,accuracy=1),"/month")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(
          title = "Electricity Costs",
          x = "Baseline and Modified Periods",
          y = "Total Cost ($/month)"
        ) +
        theme_clean() +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_discrete(labels = c("Baseline" = "B","Modified"="M"))+
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_text(family = "Open Sans",size = 10),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines")
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        ) +
        facet_wrap(.~ month, labeller = labeller(month = function(x) substr(x, 1, 3)),
                   nrow = 1
        ) 
      
      gg_cost_plot2 <- ggplotly(gg_cost_plot, tooltip = "text")
      
      gg_cost_plot2 <- gg_cost_plot2 %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      
      return(gg_cost_plot2)
    })
    
    ###ANNUAL COST CALCULATIONS FOR LOAD SHAPING####
    chosen_date <- unique(as.vector(df_merged$datetime))
    
    
    df_merged_annual <- df_merged_annual %>%
      filter(!datetime %in% chosen_date)
    
    df_merged_annual$co2_em_inv_mod <- (df_merged_annual$em_rate * df_merged_annual$mod_load) / 1000
    total_co2_saved_annual <- sum(df_merged_annual$co2em_inv) - sum(df_merged_annual$co2_em_inv_mod)
    
    df_merged_annual$ogcost <- df_merged_annual$Load
    df_merged_annual$modcost <- df_merged_annual$mod_load
    
    df_annual_final <- df_final %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_costs = sum(cost))
    
    min <- min(0.8 * df_annual_final$value)
    max <- max(1.3 * df_annual_final$value)
    
    annual_cost_for_annual_graph <- df_final %>% 
      group_by(original_modified) %>% 
      summarise(annual_cost = sum(cost))

    annual_cost_for_annual_graph_y_axis <- max(annual_cost_for_annual_graph$annual_cost)
    
   label_cost_second <-  if_else(usage_cost_savings>=0 & demand_cost_savings >=0,
            paste0("Usage Cost Savings= ",dollar(usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Savings= ",dollar(demand_cost_savings, accuracy = 1),"/yr"),
            if_else(usage_cost_savings<0 & demand_cost_savings <0,
            paste0("Usage Cost Increase= ",dollar(-usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Increase= ",dollar(-demand_cost_savings, accuracy = 1),"/yr"),
            if_else(usage_cost_savings<0 & demand_cost_savings >=0,
            paste0("Usage Cost Increase= ",dollar(-usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Savings= ",dollar(demand_cost_savings, accuracy = 1),"/yr"),
            paste0("Usage Cost Savings= ",dollar(usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Increase= ",dollar(-demand_cost_savings, accuracy = 1),"/yr"))))

    
    output$cost_plot_annual <- renderPlotly({
      p <- ggplot() +
        geom_bar(
          data = df_annual_final,
          aes(
            x = original_modified,
            y = annual_costs,
            fill = energy_demand,
            text = paste0(original_modified," Annual ",energy_demand ," = ","\n",scales::dollar(annual_costs,accuracy=1),"/yr")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(
          title = paste(year(start_date),"Annual Costs Summary"),
          x = "",
          y = "Total Cost ($/yr)"
        ) +
        theme_clean() +
        scale_y_continuous(labels = dollar_format(),
                           limits = c(0,annual_cost_for_annual_graph_y_axis*1.2)) +
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_text(family = "Open Sans",size = 10),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines")
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        ) +
        annotate("text",
                 x=1.5,
                 y=annual_cost_for_annual_graph_y_axis*1.13,
                 label = label_cost_second)
      
      
      ggplotly(p, tooltip = "text") 
      
    })
    
    ## ANNUAL CO2 FOR LOAD SHAPING
    annual_co2 <- df_merged %>%
      drop_na() %>% 
      group_by(month) %>%
      summarise(
        og_co2 = sum(co2em_inv),
        mod_co2 = sum(co2_em_inv_mod)
      ) %>%
      pivot_longer(
        cols = !month,
        names_to = "type",
        values_to = "co2"
      )
    
    co2_annual_final <- annual_co2 %>%
      mutate(concat = paste(month, co2)) %>%
      distinct(concat, .keep_all = TRUE)
    
    co2_annual_final$type[co2_annual_final$type == "og_co2"] <- paste0("Baseline CO<sub>2</sub>e Emissions")
    co2_annual_final$type[co2_annual_final$type == "mod_co2"] <- paste0("Modified CO<sub>2</sub>e Emissions")
    
    min_co2 <- min(0.8 * co2_annual_final$co2)
    max_co2 <- max(1.2 * co2_annual_final$co2)
    
    co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
    
    co2_annual_final_export <- co2_annual_final %>% 
      select(-concat) %>% 
      rename("CO2e Emissions Profile (kg/month)" = co2) %>% 
      mutate(type = sub(" .*", "", type))  %>% 
      mutate(month = factor(month, levels = month.name)) %>%
      arrange(type, month)
      
    
    output$co2_plot_annual <- renderPlotly({
      p <-  ggplot() +
        geom_bar(
          data = co2_annual_final,
          aes(
            x = month,
            y = co2,
            fill = type,
            text = paste0(word(type,1)," CO<sub>2</sub>e Emissions",
                          "\nMonth: ",month,
                          "\nEmissions: ", scales::comma(co2), " kgCO<sub>2</sub>e/month")
          ),
          stat = "identity",
          position = "dodge",
          alpha = 0.9
        ) +
        labs(title = "Monthly CO<sub>2</sub>e Plot", x = "Time", y = "Total CO<sub>2</sub>e Emissions (kgCO<sub>2</sub>e/month)") +
        theme_clean() +
        theme(
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",,size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 12),
          axis.text.x = element_text(family = "Open Sans",size = 12)
        ) +
        scale_fill_manual(
          name = "Emissions Type",
          labels = c("Baseline Emissions", "Modified Emissions"),
          values = c("#FFA600", "#00313C")
        ) +
        scale_y_continuous(
          labels = comma,
          limits = c(min_co2, max_co2),
          oob = rescale_none
        ) + 
        annotate("text",
                 x=11.5,
                 y=max(co2_annual_final$co2)*1.1,
                 label = paste0("<b>", impact_text, abs(co2_impact_value)," MTCO<sub>2</sub>e/yr"))+
        scale_x_discrete(labels = substr(month.name, 1, 3))
      
      
      p2 <- ggplotly(p, tooltip = "text")
      
      p2 <- p2%>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      
      return(p2)
          })
    
    output$download_co2em_data_monthly  <- downloadHandler(
      filename = function() {
        "monthly_co2_data.xlsx"
      },
      content = function(file) {
        write.xlsx(co2_annual_final_export, file) 
      }
    )
    
    
    data$SliderMin_from <- format(as.POSIXct(data$SliderMin_from, format = "%H:%M"), format = "%H:%M")
    data$SliderMin_from <- as.ITime(data$SliderMin_from)
    data$SliderMax_from <- format(as.POSIXct(data$SliderMax_from, format = "%H:%M"), format = "%H:%M")
    data$SliderMax_from <- as.ITime(data$SliderMax_from)
    data$SliderMin_to <- format(as.POSIXct(data$SliderMin_to, format = "%H:%M"), format = "%H:%M")
    data$SliderMin_to <- as.ITime(data$SliderMin_to)
    data$SliderMax_to <- format(as.POSIXct(data$SliderMax_to, format = "%H:%M"), format = "%H:%M")
    data$SliderMax_to <- as.ITime(data$SliderMax_to)
    
    df_merged_export <- df_merged %>%
      rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
             "Modified Load(in kW)" = mod_load,
             "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv,
             "Modified CO2e Emissions Profile (kg/hr)" = co2_em_inv_mod,
             "Original Cost ($/hr)" = ogcost,
             "Modified Cost ($/hr)" = modcost,)
    
    # TO DOWNLOAD OUTPUT DATA
    output$download_load_data <- downloadHandler(
      filename = function() {
        "modified_load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    output$download_co2em_data <- downloadHandler(
      filename = function() {
        "hourly_co2_emissions_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    output$download_lcac_data <- downloadHandler(
      filename = function() {
        "lcac_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_lcac, file) 
      }
    )
    
    output$download_costselect_data <- downloadHandler(
      filename = function() {
        "cost_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_final, file) 
      }
    )
    
    
    # TO DOWNLOAD OUTPUT DATA
    output$downloadMergedData <- downloadHandler(
      filename = function() {
        "modified_load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    data <- data %>%
      rename("Time Range To Start" = SliderMin_to,
             "Time Range To End" = SliderMax_to,
             "Time Range From Start" = SliderMin_from,
             "Time Range From End" = SliderMax_from,
             "Flexible Load (in kW)" = NumericValue)
    
    # TO DOWNLOAD INPUT DATA
    output$downloadInputData <- downloadHandler(
      filename = function() {
        "input_data.xlsx"
      },
      content = function(file) {
        write.xlsx(data, file)
      }
    )
  })
  
  observeEvent(input$plot_button_2, {
    desired_tz <- input$user_time_zone
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    if (input$addData == "Manually Enter Added Load(s)") {
      data <-
        data.frame(
          data.frame(
            NumericValue = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("numeric_add_", i)]])),
            SliderMin_to = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("slider_to_add", i)]][1])),
            SliderMax_to = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("slider_to_add", i)]][2]))
          )
        )
    }else {
      data_file <- input$file_upload_addinput
      if (is.null(data_file)) {
        return(NULL)
      }
      data <- read_excel(data_file$datapath)
      data <- data %>%
        rename(SliderMin_to = "Time Range To Start",
               SliderMax_to = "Time Range To End",
               NumericValue = "Added/Shedded Load (in kW)")
    }
    
    loadpf_file <- input$loadpf_file
    
    if (is.null(loadpf_file)) {
      return(NULL)
    }

    if(input$emissions_type == "U.S. EPA's 2022 eGRID" && input$state != "") {
      df_em_rate_0 <-  read_excel("AllUploadFiles_ToolTesting/States Emission Factors/eGRID/eGRID 2022.xlsx")
      if (input$green_manual == "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784*4
        } else {
          total_hours <- 8760*4
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:45:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "15 min")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
        
        df_em_rate_15 <-  df_em_rate
        
      } else if (input$green_manual != "Green Button: 15-Minute") {
        
        date_range <- reactive(input$date_range)
        start_date <- year(date_range()[1])
        
        if (leap_year(start_date)) {
          total_hours <- 8784
        } else {
          total_hours <- 8760
        }
        
        df_em_rate_0.5 <- df_em_rate_0 %>%
          filter(State %in% input$state) %>%
          slice(rep(1:n(), each = total_hours))
        
        df_em_rate_0.5 <- clean_names(df_em_rate_0.5)
        
        desired_tz <- input$user_time_zone
        
        # Generate sequence of hours for the entire year
        start_time <- ymd_hms(paste0(start_date, "-01-01 00:00:00"))
        end_time <- ymd_hms(paste0(start_date, "-12-31 23:00:00"))
        
        # Create the time sequence with hourly intervals
        time_sequence <- seq(start_time, end_time, by = "hour")
        
        hourly_profile <- data.frame(
          datetime = time_sequence,
          hour_of_year = 1:length(time_sequence)
        )
        
        df_em_rate <- cbind(df_em_rate_0.5,hourly_profile)
        colnames(df_em_rate)[2] <- "em_rate"
      }
    } else if (input$emissions_type != "U.S. EPA's 2022 eGRID" && input$state != "") {
      state_value <- input$state
      emissions_type <- input$emissions_type
      
      lookup_value <- paste0(state_value,"_",substr(emissions_type, 1, 4))
      
      zipfile <- "AllUploadFiles_ToolTesting/States Emission Factors/Cambium.zip"#
      
      files <- unzip(zipfile, list = TRUE)#
      
      matching_file <- files$Name[grepl(tolower(lookup_value), tolower(basename(files$Name)))]#
      
      # Check if a matching file was found
      if (length(matching_file) == 0) {
        message("No matching file found")
      } else {
        temp_dir <- tempdir()
        unzip(zipfile, files = matching_file, exdir = temp_dir)
        
        # Create the full path to the extracted file
        csv_path <- file.path(temp_dir, matching_file)
        
        # Read the csv file
        df_em_rate_0 <- read.csv(csv_path)
      }
      
      desired_tz <- input$user_time_zone
      
      if (input$green_manual == "Green Button: 15-Minute") {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC" )
        
        names(df_em_rate)[names(df_em_rate) == "datetime"] <- "date"
        
        df_em_rate <- timeAverage(df_em_rate, avg.time = "15 min", statistic = "mean", fill = T)
        
        df_em_rate <- df_em_rate %>%
          group_by(date) %>%
          summarise(em_rate = em_rate / 4)
        names(df_em_rate)[names(df_em_rate) == "date"] <- "datetime"
        
        df_em_rate_15 <- df_em_rate
      } else {
        
        df_em_rate <- subset(df_em_rate_0, select = c(timestamp_local, aer_gen_co2e_c))
        
        colnames(df_em_rate) <- c("datetime", "em_rate")
        
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%m-%d %H:%M")
        df_em_rate$datetime <- format(df_em_rate$datetime, "%Y-%d-%m %H:%M")
        df_em_rate$datetime <- as.POSIXct(df_em_rate$datetime, format = "%Y-%d-%m %H:%M", tz = "UTC")
      }
    }
    
    if (input$green_manual == "Custom Hourly Load") {

      df_loadpf_long_fin <- read_excel(loadpf_file$datapath)
      
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        rename(Load = `Load (in kW)`,
               datetime = Date_Time)
      
      df_loadpf_long_fin$datetime <- as.POSIXct(df_loadpf_long_fin$datetime, format = "%d/%m/%Y %H:%M")
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
      
      df_merged <- merge( df_loadpf_long_fin,df_em_rate, by = "datetime", all = TRUE)
      
      df_merged <- na.omit(df_merged)
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      df_merged_annual <- df_merged
    } else if (input$green_manual == "12 Months Utility Bills") {
      df_loadpf <- read_excel(loadpf_file$datapath, range =  "A1:D13")
      
      df_loadpf <- janitor::clean_names(df_loadpf)
      
      df_loadpf$energy_usage_k_wh <- gsub(",", "", df_loadpf$energy_usage_k_wh)
      df_loadpf$energy_usage_k_wh <- as.numeric(df_loadpf$energy_usage_k_wh)
      df_loadpf$billed_demand_k_w <- gsub(",", "", df_loadpf$billed_demand_k_w)
      df_loadpf$billed_demand_k_w <- as.numeric(df_loadpf$billed_demand_k_w)
      average_demand <- mean(df_loadpf$billed_demand_k_w) # Average annual billed demand
      work_on_weekends_0 <- read_excel(loadpf_file$datapath, range = "G6", col_names = F)
      work_on_weekends <- work_on_weekends_0$...1
      shift_start_time_0 <- read_excel(loadpf_file$datapath, range = "c108", col_names = F)
      start_time <- shift_start_time_0$...1
      shift_end_time_0 <- read_excel(loadpf_file$datapath, range = "c109", col_names = F)
      end_time <- shift_end_time_0$...1
      hours_of_operation_day_hrs_day <- if_else(end_time==start_time,24,if_else(end_time<start_time,24-start_time+end_time,end_time-start_time)) 
      non_working_hours <- abs(24-hours_of_operation_day_hrs_day)
      peak_hours <- 1
      mid_low_hours <- hours_of_operation_day_hrs_day-peak_hours
      pd_0 <- read_excel(loadpf_file$datapath, range = "c110", col_names = F)
      pd <- pd_0$...1 
      
      
      annual_oh <- if_else(work_on_weekends == "N", hours_of_operation_day_hrs_day * 52 * 5, hours_of_operation_day_hrs_day * 365) # Calculating annual operating hours
      
      desired_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # order of months
      
      df_loadpf_1 <- column_to_rownames(df_loadpf, var = "month") # make column names as rownames
      
      df_loadpf_sorted <- df_loadpf_1[desired_order, , drop = FALSE] %>%
        rownames_to_column("month") # sort the bills from jan to dec
      
      days <- data.frame(days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) # vectro data for number of days in each month
      order <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) # order of months from 1 to 12; jan to dec
      
      df_loadpf_2 <- cbind(df_loadpf_sorted, days, order) # bind the three dataframes
      
       if (work_on_weekends == "Y") {
        
         df_loadpf_3 <- df_loadpf_2 %>%
           mutate(
             kwhpd = energy_usage_k_wh/days,
             baseline_demand_pd = 0.3*billed_demand_k_w,
             peak_demand_pd = billed_demand_k_w,
             mid_demand_pd = (kwhpd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
           )
        
        df_loadpf_4 <-  df_loadpf_3 %>% 
          pivot_longer(cols = !c("month":"kwhpd"),
                       names_to = "energy_type",
                       values_to = "kW")
        
        # Create a data frame with a 24-hour column
        hours_df <- data.frame(hour = 0:23) 
        
        hours_df_1 <- hours_df %>% 
          mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                         hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                         hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
        
        hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
        
        month_index <- data.frame(month = rep(month.name, each = 24))
        
        hours_df_3 <- cbind(month_index,hours_df_2)
        
        df_loadpf_5 <- data.frame(seq(
          as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
          as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz),
          by = "hour"
        )) %>%
          rename(datetime = nth(names(.), 1)) %>%
          mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                 hour = (hour(datetime) ) %% 24 ) %>%
          left_join(hours_df_3,by = c("month","hour")) %>% 
          left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
          mutate(Day = day(datetime))
        
        df_loadpf_long_fin_0 <- df_loadpf_5
        
      } else {
        
        # Define the count_weekdays function
        count_weekdays <- function(year, month) {
          # Generate a sequence of dates for the given month
          start_date <- make_date(year, month, 1)
          end_date <- start_date + months(1) - days(1)
          dates <- seq.Date(start_date, end_date, by = "day")
          
          # Filter weekdays
          weekdays_count <- weekdays(dates) %>%
            table() %>%
            as.data.frame() %>%
            filter(!`.` %in% c("Saturday", "Sunday")) %>%
            summarize(count = sum(Freq))
          
          return(weekdays_count$count)
        }
        
      
        df_loadpf_3 <- df_loadpf_2 %>%
          mutate(
            no_of_weekdays = mapply(count_weekdays, year, order)
          )
        
        df_loadpf_3.5 <- df_loadpf_3 %>%
          mutate(
            no_of_weekends = days- no_of_weekdays,
            baseline_demand_pd = 0.3*billed_demand_k_w,
            kwh_wknd = baseline_demand_pd*24*no_of_weekends,
            kwh_wkd = energy_usage_k_wh-kwh_wknd,
            kwhpd_wknd = baseline_demand_pd,
            kwhpd_wkd = kwh_wkd/no_of_weekdays,
            peak_demand_pd = billed_demand_k_w,
            mid_demand_pd = (kwhpd_wkd - non_working_hours*baseline_demand_pd-peak_demand_pd*peak_hours)/mid_low_hours
          ) %>% 
          relocate(baseline_demand_pd,.after = kwhpd_wkd)
        
        df_loadpf_4 <-  df_loadpf_3.5 %>% 
          pivot_longer(cols = !c("month":"kwhpd_wkd"),
                       names_to = "energy_type",
                       values_to = "kW")
        
        # Create a data frame with a 24-hour column
        hours_df <- data.frame(hour = 0:23) 
        
        hours_df_1 <- hours_df %>% 
          mutate(energy_type = case_when(hour == pd ~"peak_demand_pd",
                                         hour>=start_time & hour <pd | hour<end_time & hour > pd | end_time < start_time & hour >=start_time | end_time < start_time & hour < end_time  ~ "mid_demand_pd",
                                         hour<24 & hour>=end_time | hour>=0 & hour<start_time~"baseline_demand_pd"))
        
        hours_df_2 <- do.call(rbind, replicate(12, hours_df_1, simplify = FALSE))
        
        month_index <- data.frame(month = rep(month.name, each = 24))
        
        hours_df_3 <- cbind(month_index,hours_df_2)
        
        df_loadpf_5 <- data.frame(seq(
          as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
          as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz),
          by = "hour"
        )) %>%
          rename(datetime = nth(names(.), 1)) %>%
          mutate(month = month(datetime,label = TRUE, abbr = FALSE),
                 hour = (hour(datetime) ) %% 24 ) %>%
          left_join(hours_df_3,by = c("month","hour")) %>% 
          left_join(df_loadpf_4,by = c("month","energy_type")) %>% 
          mutate(Day = day(datetime))
        
        
        df_loadpf_7 <- df_loadpf_5 %>% 
          mutate(day = weekdays(datetime),
                 energy_type = if_else(day == "Saturday" | day == "Sunday", "baseline_demand_pd",energy_type),
                 kW = if_else(day == "Saturday" | day == "Sunday", 0.3*billed_demand_k_w,kW)
          )
        
        df_loadpf_long_fin_0 <- df_loadpf_7
      }
      
      df_loadpf_long_fin <- df_loadpf_long_fin_0 %>%
        select(datetime,month,Day,hour,kW) %>% 
        rename(Load = kW,
               hours = hour)
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()
      
      df_merged <- df_loadpf_long_fin %>%
        full_join(df_em_rate, by = "datetime")

      df_merged$co2em_inv <- (df_merged$em_rate * df_merged[, 5]* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      df_merged_annual <- df_merged
      
    } else if (input$green_manual == "Green Button: Hourly") {
      xml_address <- loadpf_file$datapath
      loadpf <- read_xml(xml_address)
      xml_text <- xml_text(loadpf)
      ns <- c(espi = "http://naesb.org/espi")
      interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      extract_and_convert <- function(node, xpath) {
        values <- as.integer(xml_text(xml_find_all(node, xpath)))
        return(values)
      }
      
      extract_and_parse_datetime <- function(node, xpath) {
        values <- xml_text(xml_find_all(node, xpath))
        values_as_integer <- as.integer(values)
        parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
        return(parsed_values)
      }
      
      starts <- interval_readings %>%
        xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
        extract_and_parse_datetime(xpath = ".")
      
      values <- interval_readings %>%
        xml_find_all(".//espi:value", ns) %>%
        extract_and_convert(xpath = ".")
      
      df_loadpf <- data.frame(
        DATE = starts,
        USAGE = values
      )
      
      df_loadpf_long <- df_loadpf %>%
        mutate(
          DATE = (DATE),
          Load = USAGE,
          datetime = DATE,
          month = format(datetime, "%B"),
          hour = format(as.POSIXct(datetime), format = "%H:%M")
        )
      
      df_loadpf_long <- df_loadpf_long %>%
        select(Load, month, datetime, hour)
      
      
      df_loadpf_long_fin <- df_loadpf_long
      
      df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
      
      df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
      
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        distinct(datetime, .keep_all = TRUE)
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()

      df_merged <- merge( df_loadpf_long_fin, df_em_rate,by = "datetime", all = TRUE)
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      
      df_merged <- df_merged %>%
        mutate(hour = strftime(datetime, format = "%H:%M"))
      
      df_merged_annual <- df_merged
    } else {
      xml_address <- loadpf_file$datapath
      loadpf <- read_xml(xml_address)
      xml_text <- xml_text(loadpf)
      ns <- c(espi = "http://naesb.org/espi")
      interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      extract_and_convert <- function(node, xpath) {
        values <- as.integer(xml_text(xml_find_all(node, xpath)))
        return(values)
      }
      
      extract_and_parse_datetime <- function(node, xpath) {
        values <- xml_text(xml_find_all(node, xpath))
        values_as_integer <- as.integer(values)
        parsed_values <- as.POSIXct(values_as_integer, origin = "1970-01-01")
        return(parsed_values)
      }
      
      starts <- interval_readings %>%
        xml_find_all(".//espi:timePeriod/espi:start", ns) %>%
        extract_and_parse_datetime(xpath = ".")
      
      values <- interval_readings %>%
        xml_find_all(".//espi:value", ns) %>%
        extract_and_convert(xpath = ".")
      
      df_loadpf <- data.frame(
        DATE = starts,
        USAGE = values
      )
      
      df_loadpf_long <- df_loadpf %>%
        mutate(
          DATE = (DATE),
          Load = USAGE,
          datetime = DATE,
          month = format(datetime, "%B"),
          hour = format(as.POSIXct(datetime), format = "%H:%M")
        )
      
      df_loadpf_long <- df_loadpf_long %>%
        select(Load, month, datetime, hour)
      
      
      df_loadpf_long_fin <- df_loadpf_long
      
      df_loadpf_long_fin$hour <- hour(df_loadpf_long_fin$datetime)
      
      df_loadpf_long_fin$Load <- df_loadpf_long_fin$Load/1000
      
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      df_loadpf_long_fin <- df_loadpf_long_fin %>%
        distinct(datetime, .keep_all = TRUE)
      ###adding the below because for some reason duplicate datetime values with 0 load value are entering the dataset####
      
      updated_df <- reactive({
        if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
          selected_year <- 2022} else {
            selected_year <- as.integer(input$emissions_type)
          }
        df <- df_loadpf_long_fin
        df$datetime <- update(df$datetime, year = selected_year)
        return(df)
      })
      
      df_loadpf_long_fin <- updated_df()

      df_merged <- merge( df_loadpf_long_fin,df_em_rate_15, by = "datetime", all = TRUE)
      
      df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load* (1-input$perc_clean * 0.01)) / 1000
      
      df_merged$mod_load <- df_merged$Load
      
      df_merged <- df_merged %>%
        mutate(hour = strftime(datetime, format = "%H:%M"))
      
      df_merged_annual <- df_merged
    } # Make this function that sorts the 1 and 0. Half zeros in either end, and all ones in the middle
    
    date_range <- reactive({
      input$date_range
    })
    if (!is.null(date_range())) {
      start_date <- date_range()[1]
      end_date <- date_range()[2]+1
      df_merged <- subset(df_merged, datetime >= start_date & datetime < end_date)
    }

    
    for (i in 1:nrow(data)) {
      if (input$addData == "Upload Existing Spreadsheet") {
        time_to_min <- data$SliderMin_to[i]
        time_to_min <- format(as.POSIXct(time_to_min, format = "%H:%M"), format = "%H:%M")
        time_to_min <- as.ITime(time_to_min)
        time_to_max <- data$SliderMax_to[i]
        time_to_max <- format(as.POSIXct(time_to_max, format = "%H:%M"), format = "%H:%M")
        time_to_max <- as.ITime(time_to_max)
        scl_value <- data$NumericValue[i]
        
        print(str(data))
        
        if(input$green_manual == "12 Months Utility Bills") {
          
          if (work_on_weekends == "Y") {
            scl_value <- scl_value
            
            
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & (mod_load + scl_value) >=0 , mod_load + scl_value, ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & (mod_load + scl_value) <0 , 0, mod_load))) %>% 
              select(-time)
          
            
          } else {
            
            is_weekday <- function(date) {
              day_of_week <- wday(date)  # week_start = 1 makes Monday = 1, ..., Sunday = 7
              return(day_of_week >= 2 & day_of_week <= 6)
            }
            
            scl_value <- scl_value
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max  & is_weekday(time)=="TRUE" & (mod_load + scl_value)>=0, mod_load + scl_value,ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max  & is_weekday(time)=="TRUE" & (mod_load + scl_value)<0, 0, mod_load))) %>% 
              select(-time)
            
          }
        } else {
          if(input$work_on_weekends == "No") {
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }

            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & is_weekend_test==FALSE & (mod_load + scl_value)>=0, mod_load + scl_value, ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & is_weekend_test == FALSE & (mod_load + scl_value)<0,0,mod_load))) %>%
              select(-is_weekend_test,-time)
            
            
            
          } else {
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }
            
            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max& (mod_load + scl_value)>=0, mod_load + scl_value,ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max& (mod_load + scl_value)<0,0,mod_load ) )) %>%
              select(-is_weekend_test,-time)
          }
          
        }
        
        df_merged <- df_merged1
      } else {
        scl_value <- data$NumericValue[i]
        time_to_min <- data$SliderMin_to[i]
        time_to_max <- data$SliderMax_to[i]
        time_to_min <- format(as.POSIXct(time_to_min, origin = "1970-01-01"), format = "%H:%M")
        time_to_min <- as.ITime(time_to_min)
        time_to_max <- format(as.POSIXct(time_to_max, origin = "1970-01-01"), format = "%H:%M")
        time_to_max <- as.ITime(time_to_max)
        
        if(input$green_manual == "12 Months Utility Bills") {
          
          if (work_on_weekends == "Y") {
            
            scl_value <- scl_value
            
            df_merged <- df_merged

            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                mod_load = if_else((as.ITime(time) >= time_to_min) & (as.ITime(time) <= time_to_max) & ((mod_load + scl_value) >=0) ,
                                       mod_load + scl_value,
                                       if_else((as.ITime(time) >= time_to_min) & (as.ITime(time) <= time_to_max) & ((mod_load + scl_value) <0) , 0, mod_load))) %>% 
              select(-time)
            
          } else {
            
            is_weekday <- function(date) {
              day_of_week <- wday(date)  # week_start = 1 makes Monday = 1, ..., Sunday = 7
              return(day_of_week >= 2 & day_of_week <= 6)
            }
            
            scl_value <- scl_value
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                mod_load = if_else((as.ITime(time) >= time_to_min) & (as.ITime(time) <= time_to_max) & ((mod_load + scl_value) >=0) ,
                                       mod_load + scl_value,
                                       if_else((as.ITime(time) >= time_to_min) & (as.ITime(time) <= time_to_max) & ((mod_load + scl_value) <0) , 0, mod_load))) %>% 
              select(-time)
            
          }
        } else {
          if(input$work_on_weekends == "No") {
            
            scl_value <- scl_value
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }
            
            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged1 <- df_merged %>%
              mutate(time = format(as.POSIXct(datetime, origin = "1970-01-01"), format = "%H:%M"),
                     mod_load = ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & is_weekend_test==FALSE & (mod_load + scl_value)>=0, mod_load + scl_value, ifelse(as.ITime(time) >= time_to_min & as.ITime(time) <= time_to_max & is_weekend_test == FALSE & (mod_load + scl_value)<0,0,mod_load))) %>%
              select(-c(is_weekend_test,time))

            
          } else {
            
            is_weekend_test <- function(date) {
              weekday <- weekdays(date)
              return(weekday %in% c("Saturday", "Sunday"))
            }
            
            df_merged$is_weekend_test <- sapply(df_merged$datetime, is_weekend_test)
            
            df_merged1 <- df_merged %>%
              mutate(mod_load = ifelse(as.ITime(datetime) >= time_to_min & as.ITime(datetime) <= time_to_max & (mod_load + scl_value)>=0, mod_load + scl_value, ifelse(as.ITime(datetime) >= time_to_min & as.ITime(datetime) <= time_to_max & (mod_load + scl_value)<0,0,mod_load))) %>%
              select(-is_weekend_test)
          }
          
        }
        
        df_merged_test <- df_merged1 %>% 
          mutate(error = if_else(mod_load == 0 & Load != 0,"E","N")) %>% 
          drop_na()
        
        if (any(df_merged_test$error == "E")) {
          shinyalert("Caution", "Please note that the shedded load is more than the baseline load for at least one time period. For those instances, the tool has set the modified load as 0 instead of a negative value (<0).", type = "warning")
        }
        
        df_merged <- df_merged1
        
      }
    }
    
    df_merged <- df_merged

    df_merged$co2_em_inv_mod <- (df_merged$em_rate * df_merged$mod_load* (1-input$perc_clean * 0.01)) / 1000
    total_co2_saved <- sum(df_merged$co2em_inv) - sum(df_merged$co2_em_inv_mod)
    df_merged_lcac <- df_merged
    df_merged_lcac$co2_impact <-  df_merged_lcac$co2em_inv - df_merged_lcac$co2_em_inv_mod
    print(df_merged_lcac)
    
    df_merged_lcac <- df_merged_lcac %>%
      group_by(month) %>%
      summarise(
        co2_impact = sum(co2_impact)
      )
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6
    
    output$time_series_plot <- renderPlotly({
      plot_ly() %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~mod_load,
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nModified Load: ", round(mod_load)," kW"),
                  type = "scatter",
                  mode = "lines", 
                  line = list(color = "blue",dash = "dash"),
                  name = "Modified Load") %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~Load,
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nBaseline Load: ", round(Load)," kW"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red", opacity = 0.62),
                  name = "Baseline Load") %>%
        layout(
          title = "<b>Hourly Electrical Load</b>",
          xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date"), range = c(start_date_graph, end_date_graph)),
          yaxis = list(title = "<b>Load (in kW)</b>"),
          legend = list(title = "Load Type"),
          showlegend = TRUE,
          font = list(size = 14),
          margin = list(l = 20, r = 20, b = 20, t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
    })
    
    df_merged_export <- df_merged %>%
      rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
             "Modified Load(in kW)" = mod_load,
             "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv,
             "Modified CO2e Emissions Profile (kg/hr)" = co2_em_inv_mod)
    
    # TO DOWNLOAD OUTPUT DATA
    output$download_load_data <- downloadHandler(
      filename = function() {
        "modified_load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    
    co2_impact_value <- round((sum(df_merged$co2em_inv, na.rm = T) - sum(df_merged$co2_em_inv_mod, na.rm = T)) / 1000)
    
    if (co2_impact_value > 0) {
      impact_text <- paste("Baseline CO<sub>2</sub>e: ",format(round(sum(df_merged$co2em_inv, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Modified CO<sub>2</sub>e: ",format(round(sum(df_merged$co2_em_inv_mod, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Avoided CO<sub>2</sub>e: ")
    } else {
      impact_text <- paste("Baseline CO<sub>2</sub>e: ",format(round(sum(df_merged$co2em_inv, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Modified CO<sub>2</sub>e: ",format(round(sum(df_merged$co2_em_inv_mod, na.rm = T)/1000), big.mark = ","),"MTCO<sub>2</sub>e/yr","\n","Added CO<sub>2</sub>e: ")
    }
    
    start_date_graph <- as.Date(date_range()[1])
    end_date_graph <- start_date_graph + 6
 
    output$co2_emissions_change_plot <- renderPlotly({
      plot_ly() %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~co2_em_inv_mod,
                  name = "Modified Emissions",
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nModified Emissions: ", round(co2_em_inv_mod)," kgCO<sub>2</sub>e/hr"),
                  type = "scatter",
                  mode = "lines", 
                  line = list(color = "green",dash = "dash")) %>%
        add_lines(data = df_merged,
                  x = ~datetime,
                  y = ~co2em_inv,
                  name = "Baseline Emissions", 
                  hoverinfo="text", 
                  text = ~paste0(datetime, "\nBaseline Emissions: ", round(co2em_inv)," kgCO<sub>2</sub>e/hr"),
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red", opacity = 0.62)) %>%
        layout(
          title =  if_else(input$emissions_type == "<b>U.S. EPA's 2022 eGRID</b>",paste0("<b>2022 </b>","<b>Facility Baseline and Modified </b>","<b>CO<sub>2</sub>e Emissions</b>"),paste0("<b>",input$emissions_type,"</b>","<b> Facility Baseline and Modified CO<sub>2</sub>e Emissions</b>")),
          xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date"), range = c(start_date_graph, end_date_graph)),
          yaxis = list(title = "<b>CO<sub>2</sub>e Emissions (kg/hr)</b>"),
          legend = list(title = "Emissions Type"),
          showlegend = TRUE,
          font = list(size = 12),
          margin = list(l = 20, r = 20, b = 20, t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          annotations = list(
            list(
              text = paste("<b>", impact_text, format(abs(round(co2_impact_value)), big.mark = ","),"MTCO<sub>2</sub>e/yr"),
              xref = "paper",
              yref = "paper",
              x = 1,
              y = 0.90,
              xanchor = "right",
              yanchor = "bottom",
              showarrow = FALSE
            )
          )
        ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
    })
    
    
    
    output$download_co2em_data <- downloadHandler(
      filename = function() {
        "hourly_co2_emissions_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    ## COST CALCULATIONS
    df_merged$ogcost <- df_merged$Load
    df_merged$modcost <- df_merged$mod_load
    
    summer_start_month <- reactive({
      match(input$summer_month1, month.name)
    })
    summer_end_month <- reactive({
      match(input$summer_month2, month.name)
    })
    winter_start_month <- reactive({
      match(input$winter_month1, month.name)
    })
    
    winter_end_month <- reactive({
      match(input$winter_month2, month.name)
    })
    
    if (is.null(input$tou_ed) && input$tou_mm_y_n == "Yes") { ##No TOU for either energy or demand. But fixed demand charge present.
      
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate(),
               modcost = modcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load),
                  modified_demandcost = sum(demand_billed_modload))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost),
                  modified_energycost = sum(modcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (is.null(input$tou_ed) && input$tou_mm_y_n == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      winter_fixed_rate <- reactive({
        input$winter_fixed_rate
      })
      
      summer_fixed_rate <- reactive({
        input$summer_fixed_rate
      })
      
      # Filter for summer months
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      # Mutate cost with summer fixed rate
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate(),
               modcost = modcost * summer_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      # Adding Maximum Demand billing for summer
      demand_summer_max <- filtered_df_summeroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate(),
               modcost = modcost * winter_fixed_rate())
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      # Adding Maximum Demand billing for winter
      demand_winter_max <- filtered_df_winteroff %>%
        group_by(month) %>%
        summarise(max_billed_load = max(Load),
                  max_billed_modload = max(mod_load))
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      combined_df_demand <- rbind(combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(original_demandcost = sum(demand_billed_load),
                  modified_demandcost = sum(demand_billed_modload))
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(original_energycost = sum(ogcost),
                  modified_energycost = sum(modcost))
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour)
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    }else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      winter_dc_onpeak <- 0
      summer_dc_onpeak <- 0
      summer_dc_offpeak <- 0
      winter_dc_offpeak <- 0
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour & hour < winter_offpeak_end_hour )
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      
      
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      summer_start_month <- reactive({
        match(input$summer_month1, month.name)
      })
      summer_end_month <- reactive({
        match(input$summer_month2, month.name)
      })
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_start_month <- reactive({
        match(input$winter_month1, month.name)
      })
      winter_end_month <- reactive({
        match(input$winter_month2, month.name)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Usage charge ($/kWh)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      summer_partpeak_rate <- reactive({
        input$summer_partpeak_rate
      })
      
      winter_partpeak_rate <- reactive({
        input$winter_partpeak_rate
      })
      
      
      
      
      
      summer_dc_onpeak <- 0
      
      summer_dc_offpeak <- 0
      
      winter_dc_onpeak <- 0
      
      winter_dc_offpeak <- 0
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                          hour(datetime) < summer_partpeak_end_hour(),
                                        (ogcost * summer_partpeak_rate()),
                                        if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                  hour(datetime) < summer_offpeak_end_hour(),
                                                (ogcost * summer_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(hour(datetime) >= summer_partpeak_start_hour() &
                                           hour(datetime) < summer_partpeak_end_hour(),
                                         (modcost * summer_partpeak_rate()),
                                         if_else(hour(datetime) >= summer_offpeak_start_hour() |
                                                   hour(datetime) < summer_offpeak_end_hour(),
                                                 (modcost * summer_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                          hour(datetime) < winter_partpeak_end_hour(),
                                        (ogcost * winter_partpeak_rate()),
                                        if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                  hour(datetime) < winter_offpeak_end_hour(),
                                                (ogcost * winter_offpeak_rate()),
                                                ogcost
                                        )
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(hour(datetime) >= winter_partpeak_start_hour() &
                                           hour(datetime) < winter_partpeak_end_hour(),
                                         (modcost * winter_partpeak_rate()),
                                         if_else(hour(datetime) >= winter_offpeak_start_hour() |
                                                   hour(datetime) < winter_offpeak_end_hour(),
                                                 (modcost * winter_offpeak_rate()),
                                                 modcost
                                         )
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * 0
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour )
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * 0
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour )
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "No") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_summer <- data.frame(allhours = summer_onpeak_start_hour():summer_onpeak_end_hour())
      all_hours_onpeak_summer <- all_hours_onpeak_summer$allhours
      all_hours_offpeak_summer <- all_hours %>% 
        filter(all_hours >= summer_onpeak_start_hour() & all_hours <= summer_onpeak_end_hour())
      all_hours_offpeak_summer <- all_hours_offpeak_summer$allhours
      summer_offpeak_start_hour <- min(all_hours_offpeak_summer)
      summer_offpeak_end_hour <- max(all_hours_offpeak_summer)
      
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      all_hours <- data.frame(allhours = 0:23)
      all_hours_onpeak_winter <- data.frame(allhours = winter_onpeak_start_hour():winter_onpeak_end_hour())
      all_hours_onpeak_winter <- all_hours_onpeak_winter$allhours
      all_hours_offpeak_winter <- all_hours %>% 
        filter(all_hours >= winter_onpeak_start_hour() & all_hours <= winter_onpeak_end_hour())
      all_hours_offpeak_winter <- all_hours_offpeak_winter$allhours
      winter_offpeak_start_hour <- min(all_hours_offpeak_winter)
      winter_offpeak_end_hour <- max(all_hours_offpeak_winter)
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      
      summer_partpeak_demand <- 0
      
      winter_partpeak_demand <- 0
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * 0
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * 0
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour | hour < summer_offpeak_end_hour)
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * 0
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * 0
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour | hour < winter_offpeak_end_hour)
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "Yes" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- reactive({
        as.numeric(input$summer_max_demand)
      })
      
      winter_max_demand <- reactive({
        as.numeric(input$winter_max_demand)
      })
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (identical(input$tou_ed, c("Demand charge ($/kW)")) && input$tou_mm_y_n == "No" && input$tou_periods == "Yes") {
      
      
      summer_max_demand <- 0
      
      winter_max_demand <- 0
      
      summer_fixed_rate <- reactive({
        as.numeric(input$summer_fixed_rate)
      })
      
      winter_fixed_rate <- reactive({
        as.numeric(input$winter_fixed_rate)
      })
      
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      summer_partpeak_start_hour <- reactive({
        as.numeric(input$summer_partpeak_start_hour)
      })
      summer_partpeak_end_hour <- reactive({
        as.numeric(input$summer_partpeak_end_hour)
      })
      
      winter_partpeak_start_hour <- reactive({
        as.numeric(input$winter_partpeak_start_hour)
      })
      
      winter_partpeak_end_hour <- reactive({
        as.numeric(input$winter_partpeak_end_hour)
      })
      
      
      
      
      
      
      summer_dc_onpeak <- reactive({
        as.numeric(input$summer_onpeak_demand)
      })
      
      summer_dc_offpeak <- reactive({
        as.numeric(input$summer_offpeak_demand)
      })
      
      winter_dc_onpeak <- reactive({
        as.numeric(input$winter_onpeak_demand)
      })
      
      winter_dc_offpeak <- reactive({
        as.numeric(input$winter_offpeak_demand)
      })
      
      summer_partpeak_demand <- reactive({
        input$summer_partpeak_demand
      })
      
      winter_partpeak_demand <- reactive({
        input$winter_partpeak_demand
      })
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      
      filtered_df_summerpart_demand <- filtered_df_summeroff
      
      filtered_df_summerpart_demand$hour <- hour(filtered_df_summerpart_demand$datetime)
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        filter(hour >= summer_partpeak_start_hour() & hour <= summer_partpeak_end_hour())
      
      filtered_df_summerpart_demand <- filtered_df_summerpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summerpart_demand$demand_billed_load <- filtered_df_summerpart_demand$max_billed_load * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_load <- replace(filtered_df_summerpart_demand$demand_billed_load, is.infinite(filtered_df_summerpart_demand$demand_billed_load), 0)
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_load[is.na(filtered_df_summerpart_demand$demand_billed_load)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload <- filtered_df_summerpart_demand$max_billed_modload * summer_partpeak_demand()
      filtered_df_summerpart_demand$demand_billed_modload <- replace(filtered_df_summerpart_demand$demand_billed_modload, is.infinite(filtered_df_summerpart_demand$demand_billed_modload), 0)
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$demand_billed_modload[is.na(filtered_df_summerpart_demand$demand_billed_modload)] <- 0
      filtered_df_summerpart_demand$id <- "Summer Part-Peak"
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = ogcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = modcost * summer_fixed_rate())
      
      
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * 0
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * 0
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(hour = hour(datetime))
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winterpart_demand <- filtered_df_winteroff
      
      
      filtered_df_winterpart_demand$hour <- hour(filtered_df_winterpart_demand$datetime)
      
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        filter(hour >= winter_partpeak_start_hour() & hour <= winter_partpeak_end_hour())
      
      filtered_df_winterpart_demand <- filtered_df_winterpart_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      
      filtered_df_winterpart_demand$demand_billed_load <- filtered_df_winterpart_demand$max_billed_load * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_load <- replace(filtered_df_winterpart_demand$demand_billed_load, is.infinite(filtered_df_winterpart_demand$demand_billed_load), 0)
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_load[is.na(filtered_df_winterpart_demand$demand_billed_load)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload <- filtered_df_winterpart_demand$max_billed_modload * winter_partpeak_demand()
      filtered_df_winterpart_demand$demand_billed_modload <- replace(filtered_df_winterpart_demand$demand_billed_modload, is.infinite(filtered_df_winterpart_demand$demand_billed_modload), 0)
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$demand_billed_modload[is.na(filtered_df_winterpart_demand$demand_billed_modload)] <- 0
      filtered_df_winterpart_demand$id <- "winter Part-Peak"
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() | hour < winter_offpeak_end_hour())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "winter Off-Peak"
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = ogcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = modcost * winter_fixed_rate())
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * 0
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * 0
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      ###
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_summerpart_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, filtered_df_winterpart_demand, combined_df_max_demand)
      
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else if (all(c("Usage charge ($/kWh)", "Demand charge ($/kW)") %in% input$tou_ed) && input$tou_mm_y_n == "Yes" && input$tou_periods == "No") {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- reactive({
        input$summer_max_demand
      })
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- reactive({
        input$winter_max_demand
      })
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand()
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand()
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand()
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand()
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
      
    } else {
      summer_offpeak_start_hour <- reactive({
        as.numeric(input$summer_offpeak_start)
      })
      summer_offpeak_end_hour <- reactive({
        as.numeric(input$summer_offpeak_end)
      })
      summer_onpeak_start_hour <- reactive({
        as.numeric(input$summer_onpeak_start)
      })
      summer_onpeak_end_hour <- reactive({
        as.numeric(input$summer_onpeak_end)
      })
      winter_offpeak_start_hour <- reactive({
        as.numeric(input$winter_offpeak_start)
      })
      winter_offpeak_end_hour <- reactive({
        as.numeric(input$winter_offpeak_end)
      })
      winter_onpeak_start_hour <- reactive({
        as.numeric(input$winter_onpeak_start)
      })
      winter_onpeak_end_hour <- reactive({
        as.numeric(input$winter_onpeak_end)
      })
      summer_dc_onpeak <- reactive({
        input$summer_onpeak_demand
      })
      summer_dc_offpeak <- reactive({
        input$summer_offpeak_demand
      })
      summer_max_demand <- 0
      
      winter_dc_onpeak <- reactive({
        input$winter_onpeak_demand
      })
      winter_dc_offpeak <- reactive({
        input$winter_offpeak_demand
      })
      winter_max_demand <- 0
      
      summer_offpeak_rate <- reactive({
        input$summer_offpeak_rate
      })
      summer_onpeak_rate <- reactive({
        input$summer_onpeak_rate
      })
      winter_offpeak_rate <- reactive({
        input$winter_offpeak_rate
      })
      winter_onpeak_rate <- reactive({
        input$winter_onpeak_rate
      })
      
      
      
      
      
      
      filtered_df_summeroff <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff
      
      filtered_df_summeroff_demand$hour <- hour(filtered_df_summeroff_demand$datetime)
      
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        filter(hour >= summer_offpeak_start_hour() | hour < summer_offpeak_end_hour())
      
      filtered_df_summeroff_demand <- filtered_df_summeroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeroff_demand$demand_billed_load <- filtered_df_summeroff_demand$max_billed_load * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_load <- replace(filtered_df_summeroff_demand$demand_billed_load, is.infinite(filtered_df_summeroff_demand$demand_billed_load), 0)
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_load[is.na(filtered_df_summeroff_demand$demand_billed_load)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload <- filtered_df_summeroff_demand$max_billed_modload * summer_dc_offpeak()
      filtered_df_summeroff_demand$demand_billed_modload <- replace(filtered_df_summeroff_demand$demand_billed_modload, is.infinite(filtered_df_summeroff_demand$demand_billed_modload), 0)
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$demand_billed_modload[is.na(filtered_df_summeroff_demand$demand_billed_modload)] <- 0
      filtered_df_summeroff_demand$id <- "Summer Off-Peak"
      
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(ogcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                  hour(datetime) < summer_onpeak_end_hour(),
                                (ogcost * summer_onpeak_rate()),
                                if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                            hour(datetime) < summer_onpeak_end_hour()),
                                        (ogcost * summer_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "ogcost"] <- filtered_df_summeroff$ogcost
      
      filtered_df_summeroff <- filtered_df_summeroff %>%
        mutate(modcost = if_else(hour(datetime) >= summer_onpeak_start_hour() &
                                   hour(datetime) < summer_onpeak_end_hour(),
                                 (modcost * summer_onpeak_rate()),
                                 if_else(!(hour(datetime) >= summer_onpeak_start_hour() &
                                             hour(datetime) < summer_onpeak_end_hour()),
                                         (modcost * summer_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      df_merged[df_merged$datetime %in% filtered_df_summeroff$datetime, "modcost"] <- filtered_df_summeroff$modcost
      
      
      filtered_df_summeron <- df_merged %>%
        filter(month(datetime) >= summer_start_month() & month(datetime) <= summer_end_month())
      
      ## Adding Maximum Demand billing
      
      demand_summer_max <- filtered_df_summeron
      
      demand_summer_max <- demand_summer_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_summer_max$demand_billed_load <- demand_summer_max$max_billed_load * summer_max_demand
      demand_summer_max$demand_billed_load <- replace(demand_summer_max$demand_billed_load, is.infinite(demand_summer_max$demand_billed_load), 0)
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_load[is.na(demand_summer_max$demand_billed_load)] <- 0
      demand_summer_max$demand_billed_modload <- demand_summer_max$max_billed_modload * summer_max_demand
      demand_summer_max$demand_billed_modload <- replace(demand_summer_max$demand_billed_modload, is.infinite(demand_summer_max$demand_billed_modload), 0)
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$demand_billed_modload[is.na(demand_summer_max$demand_billed_modload)] <- 0
      demand_summer_max$id <- "Summer Maximum"
      
      ###
      
      filtered_df_summeron_demand <- filtered_df_summeron
      
      filtered_df_summeron_demand$hour <- hour(filtered_df_summeron_demand$datetime)
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        filter(hour >= summer_onpeak_start_hour() & hour < summer_onpeak_end_hour())
      
      
      filtered_df_summeron_demand <- filtered_df_summeron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_summeron_demand$demand_billed_load <- filtered_df_summeron_demand$max_billed_load * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_load <- replace(filtered_df_summeron_demand$demand_billed_load, is.infinite(filtered_df_summeron_demand$demand_billed_load), 0)
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_load[is.na(filtered_df_summeron_demand$demand_billed_load)] <- 0
      filtered_df_summeron_demand$demand_billed_modload <- filtered_df_summeron_demand$max_billed_modload * summer_dc_onpeak()
      filtered_df_summeron_demand$demand_billed_modload <- replace(filtered_df_summeron_demand$demand_billed_modload, is.infinite(filtered_df_summeron_demand$demand_billed_modload), 0)
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$demand_billed_modload[is.na(filtered_df_summeron_demand$demand_billed_modload)] <- 0
      filtered_df_summeron_demand$id <- "Summer on-Peak"
      
      
      
      
      filtered_df_winteroff <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      filtered_df_winteroff_demand <- filtered_df_winteroff
      
      filtered_df_winteroff_demand$hour <- hour(filtered_df_winteroff_demand$datetime)
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        filter(hour >= winter_offpeak_start_hour() & hour < winter_offpeak_end_hour())
      
      
      
      filtered_df_winteroff_demand <- filtered_df_winteroff_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteroff_demand$demand_billed_load <- filtered_df_winteroff_demand$max_billed_load * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_load <- replace(filtered_df_winteroff_demand$demand_billed_load, is.infinite(filtered_df_winteroff_demand$demand_billed_load), 0)
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_load[is.na(filtered_df_winteroff_demand$demand_billed_load)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload <- filtered_df_winteroff_demand$max_billed_modload * winter_dc_offpeak()
      filtered_df_winteroff_demand$demand_billed_modload <- replace(filtered_df_winteroff_demand$demand_billed_modload, is.infinite(filtered_df_winteroff_demand$demand_billed_modload), 0)
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$demand_billed_modload[is.na(filtered_df_winteroff_demand$demand_billed_modload)] <- 0
      filtered_df_winteroff_demand$id <- "Winter Off-Peak"
      
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(ogcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                  hour(datetime) < winter_onpeak_end_hour(),
                                (ogcost * winter_onpeak_rate()),
                                if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                            hour(datetime) < winter_onpeak_end_hour()),
                                        (ogcost * winter_offpeak_rate()),
                                        ogcost
                                )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "ogcost"] <- filtered_df_winteroff$ogcost
      
      filtered_df_winteroff <- filtered_df_winteroff %>%
        mutate(modcost = if_else(hour(datetime) >= winter_onpeak_start_hour() &
                                   hour(datetime) < winter_onpeak_end_hour(),
                                 (modcost * winter_onpeak_rate()),
                                 if_else(!(hour(datetime) >= winter_onpeak_start_hour() &
                                             hour(datetime) < winter_onpeak_end_hour()),
                                         (modcost * winter_offpeak_rate()),
                                         modcost
                                 )
        ))
      
      
      df_merged[df_merged$datetime %in% filtered_df_winteroff$datetime, "modcost"] <- filtered_df_winteroff$modcost
      
      
      filtered_df_winteron <- df_merged %>%
        filter(month(datetime) >= winter_start_month() | month(datetime) <= winter_end_month())
      
      demand_winter_max <- filtered_df_winteron
      
      demand_winter_max <- demand_winter_max %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      demand_winter_max$demand_billed_load <- demand_winter_max$max_billed_load * winter_max_demand
      demand_winter_max$demand_billed_load <- replace(demand_winter_max$demand_billed_load, is.infinite(demand_winter_max$demand_billed_load), 0)
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_load[is.na(demand_winter_max$demand_billed_load)] <- 0
      demand_winter_max$demand_billed_modload <- demand_winter_max$max_billed_modload * winter_max_demand
      demand_winter_max$demand_billed_modload <- replace(demand_winter_max$demand_billed_modload, is.infinite(demand_winter_max$demand_billed_modload), 0)
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$demand_billed_modload[is.na(demand_winter_max$demand_billed_modload)] <- 0
      demand_winter_max$id <- "winter Maximum"
      
      filtered_df_winteron_demand <- filtered_df_winteron
      
      filtered_df_winteron_demand$hour <- hour(filtered_df_winteron_demand$datetime)
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        filter(hour >= winter_onpeak_start_hour() & hour < winter_onpeak_end_hour())
      
      
      
      filtered_df_winteron_demand <- filtered_df_winteron_demand %>%
        group_by(month) %>%
        summarise(
          max_billed_load = max(Load),
          max_billed_modload = max(mod_load)
        )
      
      filtered_df_winteron_demand$demand_billed_load <- filtered_df_winteron_demand$max_billed_load * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_load <- replace(filtered_df_winteron_demand$demand_billed_load, is.infinite(filtered_df_winteron_demand$demand_billed_load), 0)
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_load[is.na(filtered_df_winteron_demand$demand_billed_load)] <- 0
      filtered_df_winteron_demand$demand_billed_modload <- filtered_df_winteron_demand$max_billed_modload * winter_dc_onpeak()
      filtered_df_winteron_demand$demand_billed_modload <- replace(filtered_df_winteron_demand$demand_billed_modload, is.infinite(filtered_df_winteron_demand$demand_billed_modload), 0)
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$demand_billed_modload[is.na(filtered_df_winteron_demand$demand_billed_modload)] <- 0
      filtered_df_winteron_demand$id <- "winter on-Peak"
      
      combined_df_max_demand <- rbind(demand_summer_max, demand_winter_max)
      
      
      combined_df_demand <- rbind(filtered_df_summeron_demand, filtered_df_summeroff_demand, filtered_df_winteroff_demand, filtered_df_winteron_demand, combined_df_max_demand)
      
      
      df_final_demand <- combined_df_demand %>%
        group_by(month) %>%
        summarise(
          original_demandcost = sum(demand_billed_load),
          modified_demandcost = sum(demand_billed_modload)
        )
      
      
      df_final <- df_merged %>%
        group_by(month) %>%
        summarise(
          original_energycost = sum(ogcost),
          modified_energycost = sum(modcost)
        )
      
      merged_df_costfinal <- merge(df_final, df_final_demand, by = "month")
      
      merged_df_costfinal_lcac <- merged_df_costfinal
      
      
      df_final <- merged_df_costfinal %>%
        pivot_longer(
          cols = !month,
          names_to = c("original_modified", "energy_demand"),
          names_sep = "_",
          values_to = "cost"
        )
      
      merged_df_costfinal_lcac$energycostimpact <- (merged_df_costfinal_lcac$modified_energycost) - (merged_df_costfinal_lcac$original_energycost)
      merged_df_costfinal_lcac$demandcostimpact <- merged_df_costfinal_lcac$modified_demandcost - merged_df_costfinal_lcac$original_demandcost
      merged_df_costfinal_lcac <- merged_df_costfinal_lcac[, c("month", "energycostimpact", "demandcostimpact")]
      merged_df_costfinal_lcac <- pivot_longer(merged_df_costfinal_lcac,
                                               cols = c("energycostimpact", "demandcostimpact"),
                                               names_to = "energy_demand",
                                               values_to = "cost"
      )
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "energycostimpact"] <- "Usage Cost"
      merged_df_costfinal_lcac$energy_demand[merged_df_costfinal_lcac$energy_demand == "demandcostimpact"] <- "Demand Cost"

      df_final$original_modified[df_final$original_modified == "original"] <- "Baseline"
      df_final$original_modified[df_final$original_modified == "modified"] <- "Modified"
      
      df_final$energy_demand[df_final$energy_demand == "energycost"] <- "Usage Cost"
      df_final$energy_demand[df_final$energy_demand == "demandcost"] <- "Demand Cost"
      
      
      df_final$month <- factor(df_final$month, levels = month.name)
    }
    
    

    
    print(df_final)
    
    annual_cost_savings_test <- df_final %>%
      group_by(original_modified, energy_demand) %>%
      summarise(cost_savings = sum(cost)) %>%
      pivot_wider(
        names_from = c("original_modified", "energy_demand"),
        values_from = cost_savings
      ) %>%
      ungroup()
    
    print(annual_cost_savings_test)
    
    annual_cost_savings <- df_final %>%
      group_by(original_modified, energy_demand) %>%
      summarise(cost_savings = sum(cost)) %>%
      pivot_wider(
        names_from = c("original_modified", "energy_demand"),
        values_from = cost_savings
      ) %>%
      ungroup() %>%
      rename(`Baseline_Demand Cost` = `Baseline_Demand Cost`,
             `Modified_Demand Cost` = `Modified_Demand Cost`,
             `Modified_Usage Cost` = `Modified_Usage Cost`,
             `Baseline_Usage Cost` = `Baseline_Usage Cost`) %>%
      mutate(
        annual_demand_cost_savings = `Baseline_Demand Cost` - `Modified_Demand Cost`,
        annual_usage_cost_savings = `Baseline_Usage Cost` - `Modified_Usage Cost`,
        annual_total_cost_savings = annual_demand_cost_savings + annual_usage_cost_savings
      )
    
    annual_demand_cost_savings <- annual_cost_savings$annual_demand_cost_savings
    annual_usage_cost_savings <- annual_cost_savings$annual_usage_cost_savings
    annual_total_cost_savings <- annual_cost_savings$annual_total_cost_savings
    
    
    df_lcac <- merge(
      x = merged_df_costfinal_lcac, y = df_merged_lcac,
      by = "month", all.x = TRUE
    )
    
    
    
    
    df_lcac$co2_impact[df_lcac$co2_impact < 0] <- 0
    
    df_lcac$co2_impact <- abs(df_lcac$co2_impact)
    
    df_lcac$lcac <- df_lcac$cost / df_lcac$co2_impact
    
    df_lcac <- na.omit(df_lcac)
    
    df_lcac$lcac <- df_lcac$lcac / 1000
    
    df_lcac$co2_impact <- df_lcac$co2_impact / 1000
    
    df_lcac$month <- factor(df_lcac$month, levels = month.name)
    
    df_lcac <- na.omit(df_lcac)
    
    annual_costsavings <- sum(df_lcac$cost)
    
    
    df_lcac <- df_lcac %>%
      drop_na() %>% 
      group_by(month) %>%
      summarize(
        co2_impact = mean(co2_impact),
        lcac = sum(lcac)
      )
    
    df_lcac$month <- factor(df_lcac$month, levels = month.name)
    
    df_lcac <- na.omit(df_lcac)
    
    
    lcac_macc <- df_lcac %>%
      ggmacc(
        abatement = co2_impact, mac = lcac, fill = month, cost_threshold = 0,
        zero_line = TRUE, threshold_line = TRUE
      )
    
    
    lcac_macc <- lcac_macc +
      labs(
        x = "Abatement MTCO<sub>2</sub>e",
        y = "Abatement Cost $/MTCO<sub>2</sub>e",
        title = "Monthly Abatement Cost Plot",
        fill = "Month"
      ) +
      theme_clean()
    
    annual_co2savings <- sum(df_lcac$co2_impact)
    
    lcac_macc_plotly <- ggplotly(lcac_macc)
    
    if(annual_co2savings >0) {
    
    lcac_macc_plotly <- lcac_macc_plotly %>%
      layout(showlegend = TRUE, legend = list(font = list(size = 10)),
        annotations = list(
          list(
            x = 0.5,
            y = 0.99,
            text = if_else(annual_costsavings<=0,
                           paste("Annual Costs Reduction = $",format(round(-annual_costsavings), big.mark = ",", scientific = FALSE),"/yr",sep = ""),
                           paste("Annual Costs Increase = $",format(round(annual_costsavings), big.mark = ",", scientific = FALSE),"/yr",sep = "")),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "top"
          ),
          list(
            x = 0.5,
            y = 0.99,
            text = if_else(annual_co2savings>=0,
                           paste("Annual CO<sub>2</sub>e Reduction = ",format(round(annual_co2savings), big.mark = ",", scientific = FALSE)," MTCO<sub>2</sub>e/yr",sep = ""),
                           paste("Annual CO<sub>2</sub>e Increase = ",format(round(-annual_co2savings), big.mark = ",", scientific = FALSE)," MTCO<sub>2</sub>e/yr",sep = "")),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "bottom"
          )
        )
      )
    
    output$lcac_plot <- renderPlotly({
      lcac_macc_plotly <- ggplotly(lcac_macc_plotly)
      lcac_macc_plotly <- lcac_macc_plotly %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      return(lcac_macc_plotly)
    })
    
    
    
    output$download_lcac_data <- downloadHandler(
      filename = function() {
        "lcac_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_lcac, file) 
      }
    )
    
    }  else {
      output$lcac_plot <- renderPlotly({
        plotly_empty() %>% layout(
          annotations = list(
            text = "The 'Added Load' inputs do not lead to emission savings. Therefore, we cannot create an abatement plot.",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 20)
          )
        )
      })
    }
    
    #### Start Adding Code from Here. All the above code is copied from the first generate plot button
    
    cost_savings_summary <- df_final %>% 
      drop_na() %>% 
      group_by(original_modified, energy_demand) %>% 
      summarise(cost = sum(cost))
    
    print(cost_savings_summary)
    
    usage_cost_savings_table <- cost_savings_summary %>% 
      filter(energy_demand == "Usage Cost") %>% 
      select(-energy_demand) %>% 
      pivot_wider(names_from = original_modified,
                  values_from = cost) %>% 
      mutate(`Baseline - Modified` = Baseline - Modified)
    
    usage_cost_savings <- usage_cost_savings_table$`Baseline - Modified`
    
    demand_cost_savings_table <- cost_savings_summary %>% 
      filter(energy_demand == "Demand Cost") %>% 
      select(-energy_demand) %>% 
      pivot_wider(names_from = original_modified,
                  values_from = cost) %>% 
      mutate(`Baseline - Modified` = Baseline - Modified)
    
    demand_cost_savings <- demand_cost_savings_table$`Baseline - Modified`
    
    highest_value <- df_final %>% 
      pivot_wider(names_from = energy_demand,
                  values_from = cost) 
    
    highest_value <- highest_value %>% 
      mutate(total_cost = `Usage Cost`+`Demand Cost`)
    
    highest_value_for_graph <- max(highest_value$total_cost)
    
    df_final <- na.omit(df_final)
    
    output$cost_plot <- renderPlotly({
      gg_cost_plot <- ggplot() +
        geom_bar(
          data = df_final,
          aes(
            x = original_modified,
            y = cost,
            fill = energy_demand,
            text = paste0(original_modified," ",energy_demand ," = ",
                          "\n",scales::dollar(cost,accuracy=1),"/month")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(title = "Electricity Costs",
             x = "Baseline and Modified Periods",
             y = "Total Cost ($/month)") +
        theme_clean() +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_discrete(labels = c("Baseline" = "B","Modified"="M"))+
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_text(family = "Open Sans",size = 10),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines")
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        ) +
        facet_wrap(.~ month, labeller = labeller(month = function(x) substr(x, 1, 3)),
                   nrow = 1
        )
      

      gg_cost_plot2 <- ggplotly(gg_cost_plot, tooltip = "text")
      
      gg_cost_plot2 <- gg_cost_plot2 %>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      
      return(gg_cost_plot2)
    })
    
    
    output$download_costselect_data <- downloadHandler(
      filename = function() {
        "cost_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_final, file) 
      }
    )
    
    data.frame(
      mpg = 15, wt = 5, lab = "Text",
      cyl = factor(8, levels = c("4", "6", "8"))
    )
    
    # ANNUAL COST CALCULATIONS FOR LOAD ADDITION - WITH DEMAND AND ENERGY CHARGE CALCULATION FIX
    
    chosen_date <- unique(as.vector(df_merged$datetime))
    
    df_merged_annual <- df_merged_annual %>%
      filter(!datetime %in% chosen_date)
    
    df_merged_annual$co2_em_inv_mod <- (df_merged_annual$em_rate * df_merged_annual$mod_load) / 1000
    total_co2_saved_annual <- sum(df_merged_annual$co2em_inv) - sum(df_merged_annual$co2_em_inv_mod)
    
    df_merged_annual$ogcost <- df_merged_annual$Load
    df_merged_annual$modcost <- df_merged_annual$mod_load
    
    df_annual_final <- df_final %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_costs = sum(cost))
    
    
    min <- min(0.8 * df_annual_final$value)
    max <- max(1.3 * df_annual_final$value)
    
    annual_cost_for_annual_graph <- df_final %>% 
      group_by(original_modified) %>% 
      summarise(annual_cost = sum(cost))
    
    annual_cost_for_annual_graph_y_axis <- max(annual_cost_for_annual_graph$annual_cost)
    annual_cost_for_annual_graph_y_axis <- annual_cost_for_annual_graph_y_axis
    
    label_cost_second <-  if_else(usage_cost_savings>=0 & demand_cost_savings >=0,
                                  paste0("Usage Cost Savings= ",dollar(usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Savings= ",dollar(demand_cost_savings, accuracy = 1),"/yr"),
                                  if_else(usage_cost_savings<0 & demand_cost_savings <0,
                                          paste0("Usage Cost Increase= ",dollar(-usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Increase= ",dollar(-demand_cost_savings, accuracy = 1),"/yr"),
                                          if_else(usage_cost_savings<0 & demand_cost_savings >=0,
                                                  paste0("Usage Cost Increase= ",dollar(-usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Savings= ",dollar(demand_cost_savings, accuracy = 1),"/yr"),
                                                  paste0("Usage Cost Savings= ",dollar(usage_cost_savings, accuracy = 1) , "/yr","\n","Demand Cost Increase= ",dollar(-demand_cost_savings, accuracy = 1),"/yr"))))
    
    label_cost_second <- label_cost_second
    
    output$cost_plot_annual <- renderPlotly({
      p <- ggplot() +
        geom_bar(
          data = df_annual_final,
          aes(
            x = original_modified,
            y=annual_costs,
            fill = energy_demand,
            text = paste0(original_modified," Annual ",energy_demand ," = ","\n", scales::dollar(annual_costs,accuracy=1),"/yr")
          ),
          stat = "identity",
          position = "stack",
          alpha = 0.9
        ) +
        labs(
          title = paste(year(start_date),"Annual Costs Summary"),
          x = "",
          y = "Total Cost ($/yr)"
        ) +
        theme_clean() +
        scale_y_continuous(labels = dollar_format(),
                           limits = c(0,annual_cost_for_annual_graph_y_axis*1.2)) +
        theme(
          legend.position = "bottom",
          text = element_text(family = "Open Sans",size = 14),
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 11),
          axis.text.x = element_text(family = "Open Sans",size = 10),
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines")
        ) +
        scale_fill_manual(
          name = "Cost Type",
          labels = c("Usage Cost", "Demand Cost"),
          values = c("#FFA600", "#00313C")
        ) + 
        annotate("text",
                 x=1.5,
                 y=annual_cost_for_annual_graph_y_axis*1.13,
                 label = label_cost_second)
      
      ggplotly(p, tooltip = "text") 
    })
    
    ## ANNUAL CO2 FOR LOAD AFFING
    annual_co2 <- df_merged %>%
      drop_na() %>% 
      group_by(month) %>%
      summarise(
        og_co2 = sum(co2em_inv),
        mod_co2 = sum(co2_em_inv_mod)
      ) %>%
      pivot_longer(
        cols = !month,
        names_to = "type",
        values_to = "co2"
      )
    
    co2_annual_final <- annual_co2 %>%
      mutate(concat = paste(month, co2)) %>%
      distinct(concat, .keep_all = TRUE)
    
    co2_annual_final$type[co2_annual_final$type == "og_co2"] <- paste0("Baseline CO<sub>2</sub>e Emissions")
    co2_annual_final$type[co2_annual_final$type == "mod_co2"] <- paste0("Modified CO<sub>2</sub>e Emissions")
    
    min_co2 <- min(0.8 * co2_annual_final$co2)
    max_co2 <- max(1.2 * co2_annual_final$co2)
    
    co2_annual_final$month <- factor(co2_annual_final$month, levels = month.name)
    
    co2_annual_final <- co2_annual_final
    
    co2_annual_final_export <- co2_annual_final %>% 
       select(-concat) %>% 
       rename("CO2e Emissions Profile (kg/month)" = co2) %>% 
       mutate(type = sub(" .*", "", type))  %>% 
       mutate(month = factor(month, levels = month.name)) %>%
      arrange(type, month)
    
    output$co2_plot_annual <- renderPlotly({
      p <-  ggplot() +
        geom_bar(
          data = co2_annual_final,
          aes(
            x = month,
            y = co2,
            fill = type,
            text = paste0(word(type,1)," CO<sub>2</sub>e Emissions",
                          "\nMonth: ",month,
                          "\nEmissions: ", scales::comma(co2), " kgCO<sub>2</sub>e/month")
          ),
          stat = "identity",
          position = "dodge",
          alpha = 0.9
        ) +
        labs(title = "Monthly CO<sub>2</sub>e Plot", x = "Time", y = "Total CO<sub>2</sub>e Emissions (kgCO<sub>2</sub>e/month)") +
        theme_clean() +
        theme(
          text = element_text(family = "Open Sans",size = 14),,
          axis.title = element_text(family = "Open Sans",size = 16, face = "bold"),
          legend.title = element_text(family = "Open Sans",size = 14, face = "bold"),
          legend.text = element_text(family = "Open Sans",size = 12),
          plot.title = element_text(family = "Open Sans",hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(family = "Open Sans",size = 12),
          axis.text.x = element_text(family = "Open Sans",size = 12)
        ) +
        scale_fill_manual(
          name = "Emissions Type",
          labels = c("Baseline Emissions", "Modified Emissions"),
          values = c("#FFA600", "#00313C")
        ) +
        scale_y_continuous(
          labels = comma,
          limits = c(min_co2, max_co2),
          oob = rescale_none
        )+
        annotate("text",
                 x=11.5,
                 y=max(co2_annual_final$co2)*1.10,
                 label = paste0("<b>", impact_text, abs(co2_impact_value)," MTCO<sub>2</sub>e/yr"))+
        scale_x_discrete(labels = substr(month.name, 1, 3))
      
      p2 <- ggplotly(p, tooltip = "text")
      
      p2 <- p2%>%
        config(displayModeBar = T, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines"))
      
      return(p2)
    })
    
    output$download_co2em_data_monthly  <- downloadHandler(
      filename = function() {
        "monthly_co2_data.xlsx"
      },
      content = function(file) {
        write.xlsx(co2_annual_final_export, file) 
      }
    )
    
    
    
    data$SliderMin_to <- format(as.POSIXct(data$SliderMin_to, format = "%H:%M"), format = "%H:%M")
    data$SliderMin_to <- as.ITime(data$SliderMin_to)
    data$SliderMax_to <- format(as.POSIXct(data$SliderMax_to, format = "%H:%M"), format = "%H:%M")
    data$SliderMax_to <- as.ITime(data$SliderMax_to)
    
    
    df_merged_export <- df_merged %>%
      rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate,
             "Modified Load(in kW)" = mod_load,
             "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv,
             "Modified CO2e Emissions Profile (kg/hr)" = co2_em_inv_mod,
             "Original Cost ($/hr)" = ogcost,
             "Modified Cost ($/hr)" = modcost,)
    
    # TO DOWNLOAD OUTPUT DATA
    output$downloadMergedData <- downloadHandler(
      filename = function() {
        "modified_load_data.xlsx"
      },
      content = function(file) {
        write.xlsx(df_merged_export, file) 
      }
    )
    
    data <- data %>%
      rename("Time Range To Start" = SliderMin_to,
             "Time Range To End" = SliderMax_to,
             "Added/Shedded Load (in kW)" = NumericValue)
    
    # TO DOWNLOAD INPUT DATA
    output$downloadInputData <- downloadHandler(
      filename = function() {
        "input_data.xlsx"
      },
      content = function(file) {
        write.xlsx(data, file)
      }
    )
  })
  
   observeEvent(input$reset_input, {
     shinyjs::reset("side-panel")
   
   #Reset button event handler
    updateRadioButtons(session, "green_manual", selected = "12 Months Utility Bills")
    updateSelectInput(session, "emissions_type", selected = "2022")
    updateSelectInput(session, "state", selected = "")
    updateCheckboxGroupInput(session, "tou_ed", selected = c("Usage charge ($/kWh)"))
    updateRadioButtons(session, "tou_mm_y_n", selected = "Yes")
    updateRadioButtons(session, "addshed_shift", selected = "Shift Load")
    updateRadioButtons(session, "shiftData", selected = "Manually Enter Shaped Load(s)")
    
    output$time_series_plot <- renderPlotly({
      plotly_empty()
    })
    
    output$co2_emissions_change_plot <- renderPlotly({
      plotly_empty()
    })
    
    output$co2_plot_annual <- renderPlotly({
      plotly_empty()
    })
    
    output$cost_plot <- renderPlotly({
      plotly_empty()
    })
    
    output$cost_plot_annual <- renderPlotly({
      plotly_empty()
    })
    
    output$lcac_plot <- renderPlotly({
      plotly_empty()
    })
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")