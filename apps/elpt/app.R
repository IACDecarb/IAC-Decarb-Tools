# ==========================================
# 1. INSTALL & LOAD PACKAGES
# ==========================================

cran_packages <- c(
  "shiny", "bslib", "shinythemes", "shinyBS", "shinyalert", "shinyjs", "shinycssloaders", 
  "tidyverse", "data.table", "lubridate", "janitor", "imputeTS", 
  "openxlsx", "readxl", "xml2", "jsonlite",
  "plotly", "ggplot2", "ggthemes", "scales", "openair", 
  "htmltools", "thematic", "conflicted"
)

options(scipen = 999)

# Install missing CRAN packages
installed <- rownames(installed.packages())
missing_pkgs <- cran_packages[!(cran_packages %in% installed)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

# Install ggmacc from GitHub if missing
if (!("ggmacc" %in% installed)) devtools::install_github("aj-sykes92/ggmacc")

# Load Core UI & Reactivity
library(shiny)
library(bslib)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(shinyjs)
library(shinycssloaders)
library(htmltools)

# Load Data Wrangling
library(tidyverse) # Loads dplyr, tidyr, stringr, purrr, etc.
library(data.table)
library(lubridate)
library(janitor)
library(imputeTS)

# Load File I/O
library(openxlsx)
library(readxl)
library(xml2)
library(jsonlite)

# Load Visualization
library(plotly)
library(ggplot2)
library(ggthemes)
library(scales)
library(openair)
library(ggmacc)
library(thematic)

# ==========================================
# 2. RESOLVE NAMESPACE CONFLICTS
# ==========================================
library(conflicted)
#
conflicts_prefer(
  lubridate::year,
  lubridate::month,
  lubridate::hour,
  lubridate::yday,
  lubridate::wday,
  dplyr::filter,
  dplyr::lag,
  plotly::layout,
  .quiet = TRUE  # <--- Note the period here!
)

# Configure Plotly's browser-side image export consistently for every chart.
configure_plotly_download <- function(plot, filename, remove_buttons = NULL) {
  config_args <- list(
    p = plot,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    toImageButtonOptions = list(
      format = "png",
      filename = filename,
      width = NULL,
      height = NULL,
      scale = 2
    )
  )

  if (length(remove_buttons) > 0) {
    config_args$modeBarButtonsToRemove <- remove_buttons
  }

  do.call(plotly::config, config_args)
}

cost_plot_modebar_buttons_to_remove <- c(
  "zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
  "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "select2d",
  "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d",
  "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
  "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
  "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews",
  "toggleSpikelines"
)

# Reporting periods follow the uploaded data, not the January-December calendar.
# Keeping a real year-month key prevents (for example) August 2025 and August
# 2026 from being combined into the same bill or emissions bar.
reporting_month_start <- function(datetime) {
  month_floor <- lubridate::floor_date(datetime, unit = "month")
  if (inherits(month_floor, "POSIXt")) {
    return(as.Date(month_floor, tz = lubridate::tz(datetime)))
  }
  as.Date(month_floor)
}

reporting_month_label <- function(month_start) {
  month_start <- as.Date(month_start)
  paste(month.abb[as.integer(format(month_start, "%m"))], format(month_start, "%Y"))
}

reporting_period_label <- function(datetime) {
  valid_datetime <- datetime[!is.na(datetime)]
  if (length(valid_datetime) == 0) return("")

  start_month <- reporting_month_start(min(valid_datetime))
  end_month <- reporting_month_start(max(valid_datetime))
  paste(reporting_month_label(start_month), reporting_month_label(end_month), sep = " – ")
}

# Keep URDB parsing and billing math in a standalone, package-free module so
# tier calculations can be regression-tested without launching the Shiny app.
source("rate_calculations.R")

# conflict_scout() # Leave commented out for production

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


radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL) {
  options <- shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options <- paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
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

checkboxTooltip <- function(id, title, placement = "bottom", trigger = "hover", options = NULL) {
  options <- shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options <- paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
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
  options <- shinyBS:::buildTooltipOrPopoverOptionsList(tooltip_title, placement, trigger, options)
  options <- paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        
        // This new selector is more robust and reliably finds the label for all input types.
        var inputLabel = $('#", id, "').closest('.form-group').find('label');

        var infoIcon = $('<span class=\"info-icon\">?</span>');
        infoIcon.tooltip($.extend(", options, ", {html: true}));
        inputLabel.append(infoIcon);
      }, 500);
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

load_input_label <- function(label, help) {
  tagList(
    label,
    tags$span(
      "?", class = "info-icon load-input-help", tabindex = "0",
      title = help, `aria-label` = help
    )
  )
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

# Reload the shared rate database after deployment without restarting Shiny.
urdb_data <- reactiveFileReader(
  intervalMillis = 60000, session = NULL,
  filePath = "AllUploadFiles_ToolTesting/local_database_rates.json",
  readFunc = jsonlite::fromJSON
)


# Main UI

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(
    # Render the Plotly image in the browser, then send it to Shiny so it can be
    # returned through a normal same-origin downloadHandler. This avoids hosted
    # browser restrictions on blob: and data: URL downloads.
    tags$script(HTML("
      function renderPlotlyPngWithoutBlob(plot, options) {
        var width = Number(options.width) ||
          (plot._fullLayout && plot._fullLayout.width) ||
          plot.clientWidth || 700;
        var height = Number(options.height) ||
          (plot._fullLayout && plot._fullLayout.height) ||
          plot.clientHeight || 450;
        var scale = Number(options.scale) || 1;

        // Plotly's PNG renderer uses a blob: URL internally. Request SVG so
        // Plotly uses a data: URL, then rasterize that SVG ourselves.
        return window.Plotly.toImage(plot, {
          format: 'svg',
          width: width,
          height: height,
          scale: 1
        }).then(function(svgDataUrl) {
          return new Promise(function(resolve, reject) {
            var image = new window.Image();

            image.onload = function() {
              try {
                var canvas = document.createElement('canvas');
                canvas.width = Math.round(width * scale);
                canvas.height = Math.round(height * scale);

                var context = canvas.getContext('2d');
                if (!context) {
                  reject(new Error('The browser could not create a PNG canvas.'));
                  return;
                }

                context.drawImage(image, 0, 0, canvas.width, canvas.height);
                resolve(canvas.toDataURL('image/png'));
              } catch (error) {
                reject(error);
              }
            };

            image.onerror = function() {
              reject(new Error(
                'The browser could not load the Plotly SVG for PNG conversion.'
              ));
            };

            image.src = svgDataUrl;
          });
        });
      }

      document.addEventListener('click', function(event) {
        var target = event.target;
        var button = target.closest && target.closest(
          '.modebar-btn[data-title^=\"Download plot\"]'
        );

        if (!button) return;

        var plot = button.closest('.js-plotly-plot');
        if (!plot || !window.Plotly) return;

        event.preventDefault();
        event.stopImmediatePropagation();

        if (!window.Shiny || !window.Shiny.setInputValue) {
          console.error('Shiny is not connected; the plot image cannot be downloaded.');
          return;
        }

        if (button.dataset.exporting === 'true') return;
        button.dataset.exporting = 'true';

        var options = plot._context.toImageButtonOptions || {};
        var filename = options.filename || plot.id || 'plot';

        renderPlotlyPngWithoutBlob(plot, options).then(function(dataUrl) {
          window.Shiny.setInputValue('plot_image_export', {
            dataUrl: dataUrl,
            filename: filename,
            nonce: Date.now()
          }, {priority: 'event'});
        }).catch(function(error) {
          console.error('Plotly image download failed:', error);
          window.Shiny.setInputValue('plot_image_export_error', {
            message: error && error.message ? error.message : String(error),
            nonce: Date.now()
          }, {priority: 'event'});
        }).finally(function() {
          delete button.dataset.exporting;
        });
      }, true);

      function downloadPlotImageWhenReady(linkId, attemptsRemaining) {
        var link = document.getElementById(linkId);
        var href = link && link.getAttribute('href');

        if (href) {
          // The response has Content-Disposition: attachment, so navigation
          // starts a download without replacing the current Shiny page.
          window.location.assign(href);
          return;
        }

        if (attemptsRemaining > 0) {
          window.setTimeout(function() {
            downloadPlotImageWhenReady(linkId, attemptsRemaining - 1);
          }, 100);
          return;
        }

        var message = 'The Shiny plot-image download link is unavailable.';
        console.error(message);
        window.Shiny.setInputValue('plot_image_export_error', {
          message: message,
          nonce: Date.now()
        }, {priority: 'event'});
      }

      window.Shiny.addCustomMessageHandler('downloadPlotImage', function(message) {
        downloadPlotImageWhenReady(message.linkId, 30);
      });
    ")),
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
    tags$script(HTML("
      $(function() {
        $('body').tooltip({
          selector: '.load-input-help',
          container: 'body',
          placement: 'right',
          trigger: 'hover focus'
        });
      });
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
      "))
  ),
  div(
    style = "display: none;",
    downloadLink("download_plot_image", "Download plot image")
  ),
  titlePanel(
    HTML("Electrical Load Planning Tool"),
    windowTitle = "ELPT"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tabsetPanel(
        id = "tabs",
        
        # Step 1
        tabPanel(
          "Load Input",
          tags$p(tags$strong("1. Select Electrical Load Type")),
          radioButtons("green_manual", NULL,
                       choices = c(
                         "Custom Hourly Load",
                         "Green Button: 15-Minute",
                         "Green Button: Hourly",
                         "12 Months Utility Bills"
                       ),
                       selected = "Custom Hourly Load"
          ),
          radioTooltip("green_manual", "12 Months Utility Bills",
                       "Download and fill out the custom 12 months utility bill template",
                       placement = "right"
          ),
          radioTooltip("green_manual", "Green Button: 15-Minute",
                       "Upload XML file from your utility",
                       placement = "right"
          ),
          radioTooltip("green_manual", "Green Button: Hourly",
                       "Upload XML file from your utility",
                       placement = "right"
          ),
          radioTooltip("green_manual", "Custom Hourly Load",
                       "Download and fill out the custom hourly template",
                       placement = "right"
          ),
          dateRangeInput("date_range", "2. Select Date Range:",
                         start     = "2025-01-01",
                         end       = "2025-12-31",
                         format    = "yyyy-mm-dd",
                         separator = " – "
          ),
          dateRangeTooltip("date_range", "Select Date Range:", "Choose a date range that matches the dates in your electrical load data. You can also modify this range to filter and analyze just a subset of your data", placement = "right", trigger = "hover"),
          uiOutput("message_date_disc"),
          fluidRow(
            column(
              12,
              uiOutput("BillcustomUI")
            )
          ),
          uiOutput("fileInputUI"),
          downloadLink("downloaddocu", "Download Tool Documentation")
        ),
        
        # Step 2
        # tabPanel(
        #   "Rate Input",
        #   tags$p(tags$strong("Select Utility and Rate Plan from Database")),
        #   
        #   # Dropdown 1: The 188 Utilities (Pre-populated from global.R)
        #   selectInput("utility_selector", "1. Select Utility Company:",
        #               choices = c("Please select a utility..." = "", unique_utilities),
        #               width = "100%"
        #   ),
        #   
        #   # Dropdown 2: The Rates (Starts empty, populated by the server)
        #   selectInput("rate_selector", "2. Select Specific Rate Plan:",
        #               choices = NULL,
        #               width = "100%"
        #   )
        # ),
        
        tabPanel(
          "Rate Input",
          tags$p(tags$strong("Select Utility and Rate Plan from Database")),
          
          # Dropdown 1: The 188 Utilities (Pre-populated from global.R)
          selectInput("utility_selector", "1. Select Utility Company:",
                      choices = c("Please select a utility..." = ""),
                      width = "100%"
          ),
          
          # Dropdown 2: The Rates (Starts empty, populated by the server)
          selectInput("rate_selector", "2. Select Specific Rate Plan:",
                      choices = NULL,
                      width = "100%"
          ),
          
          # NEW: Dynamic UI Output for the Rate Preview
          uiOutput("rate_preview_ui")
        ),
        
        
        # Step 3
        tabPanel(
          "Manage Loads",
          tags$p(tags$strong("1. Type of load management")),
          tags$div(
            # Switched to flex-wrap so it stacks if the screen gets too small
            style = "display:flex; flex-wrap: wrap; justify-content:flex-start; gap: 20px; margin-bottom: 15px;",
            radioButtons("addshed_shift",
                         NULL,
                         choices = c("Add/Shed Load", "Shift Load"),
                         selected = "Add/Shed Load",
                         inline = TRUE, width = "100%" # Changed to 100% instead of 600px
            )
          ),
          tags$p(tags$strong("2. Add/Shed/Shift load on weekends?")),
          tags$div(
            style = "display:flex; justify-content:flex-start; gap:20px; margin-top:10px;",
            conditionalPanel(
              "input.green_manual != '12 Months Utility Bills'",
              radioButtons("work_on_weekends",
                           NULL,
                           choices = c("Yes", "No"),
                           selected = "No",
                           inline = FALSE, width = "100%"
              )
            ),
            radioTooltip("work_on_weekends", "Yes", "Include weekends", placement = "right"),
            radioTooltip("work_on_weekends", "No", "Weekdays only", placement = "right")
          ),
          br(),
          conditionalPanel(
            condition = "input.addshed_shift == 'Add/Shed Load'",
            bsCollapse(
              id = "loadMgmtPanel_Add",
              multiple = FALSE,
              open = "add_panel",
              bsCollapsePanel(
                title = HTML('<strong>3. Enter Load Addition/Shedding Inputs</strong>
                                          <span class="toggle-icon glyphicon glyphicon-chevron-down"></span>'),
                value = "add_panel",
                style = "default",
                uiOutput("inputSets_1"),
                actionButton("addWindow_load",
                             "Add more Load Input(s)",
                             width = "100%",
                             style = "display:block; padding:8px; margin-bottom:10px;"
                ),
                actionButton("plot_button_2",
                             "Generate Plot",
                             width = "100%",
                             style = "display:block; padding:8px;"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.addshed_shift == 'Shift Load'",
            bsCollapse(
              id = "loadMgmtPanel_Shift",
              multiple = FALSE,
              open = "shift_panel",
              bsCollapsePanel(
                title = HTML('<strong>3. Enter Load Shifting Inputs</strong>
                                          <span class="toggle-icon glyphicon glyphicon-chevron-down"></span>'),
                value = "shift_panel",
                style = "default",
                uiOutput("inputSets"),
                actionButton("addWindow",
                             "Add more Shaped Load(s)",
                             width = "100%",
                             style = "display:block; padding:8px; margin-bottom:10px;"
                ),
                actionButton("plot_button",
                             "Generate Plot",
                             width = "100%",
                             style = "display:block; padding:8px;"
                )
              )
            )
          )
        ),
        # Step 4
        tabPanel(
          "Emissions Input",
          selectInput("emissions_type", "1. Select Analysis Year:",
                      choices = list(
                        "NREL Cambium Standard Scenarios 2021:" = as.character(2022:2050),
                        "Annual Emissions Factor:" =
                          list("U.S. EPA's 2022 eGRID" = "U.S. EPA's 2022 eGRID")
                      ),
                      selected = "2025"
          ),
          numericInput("perc_clean",
                       "2. Facility's Clean Electricity Share (%)", 10,
                       min = 0, max = 100
          ),
          dateRangeTooltip(
            id = "perc_clean", title = "Facility's Clean Electricity Share (%)",
            tooltip_title = "This input allows you to account for any renewable energy certificates (RECs) or clean electricity procurement by the facility. Use 0 for no onsite or procured clean energy or RECs"
          ),
          selectInput("state", "3. Select State:", c("", state.abb), selected = "AL"),
          actionButton("generate_co2_plot", "Generate CO2e Plot", width = "100%", style = "margin-top: 15px;"),
          dateRangeTooltip(
            id = "state", title = "Select State:",
            tooltip_title = "Select the state where the facility is located to use its respective emissions profile"
          )
        )
      )
    ),
    
    #  Right‐hand column (Results Panels)
    mainPanel(
      width = 9,
      conditionalPanel(
        condition = "output.fileUploaded",
        tags$p(
          tags$strong("Chart labels: "), "B = Baseline; M = Modified.",
          style = "margin-bottom: 15px;"
        )
      ),
      
      # 1. Electrical Load Plot Panel
      conditionalPanel(
        condition = "output.fileUploaded",
        navset_card_underline(
          id = "load_panel",
          title = HTML("<b>Electrical Load Plot</b>"),
          nav_panel(
            "Hourly Electrical Load",
            shinycssloaders::withSpinner(
              plotlyOutput("time_series_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_load_data", "Download Plot Data (.XLSX)")
            )
          )
        )
      ),
      
      # 2. Cost Plots Panel
      conditionalPanel(
        condition = "output.fileUploaded && output.rateFileUploaded",
        navset_card_underline(
          id = "cost_panel",
          title = HTML("<b>Cost Plots</b>"),
          nav_panel(
            "Monthly Cost",
            
            # --- VALUE BOXES AT THE TOP OF THE DEFAULT TAB ---
            div(style = "margin-bottom: 20px; margin-top: 10px;",
                uiOutput("cost_kpi_ui")
            ),
            
            shinycssloaders::withSpinner(
              plotlyOutput("cost_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_costselect_data", "Download Plot Data (.XLSX)")
            )
          ),
          nav_panel(
            "Hourly Cost",
            shinycssloaders::withSpinner(
              plotlyOutput("hourly_cost_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_hourly_cost_data", "Download Plot Data (.XLSX)")
            )
          ),
          nav_panel(
            "Annual Cost",
            shinycssloaders::withSpinner(
              plotlyOutput("cost_plot_annual", height = "450px"),
              type = getOption("spinner.type", default = 8)
            )
          ),
          
          # <--- ERROR 2 WAS HERE: YOU ACCIDENTALLY DELETED THIS ENTIRE WRAPPER
          nav_panel(
            "Carbon Abatement Cost",
            shinycssloaders::withSpinner(
              plotlyOutput("lcac_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_lcac_data", "Download Plot Data (.XLSX)")
            )
          )
        )
      ),
      
      # 3. CO2e Plots Panel
      conditionalPanel(
        condition = "output.showCO2Panel",
        navset_card_underline(
          id = "co2_panel",
          title = HTML("<b>CO<sub>2</sub>e Plots</b>"),
          nav_panel(
            HTML("Hourly CO<sub>2</sub>e Emissions"),
            shinycssloaders::withSpinner(
              plotlyOutput("co2_emissions_change_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_co2em_data", "Download Plot Data (.XLSX)")
            )
          ),
          nav_panel(
            HTML("Monthly CO<sub>2</sub>e Emissions"),
            shinycssloaders::withSpinner(
              plotlyOutput("co2_plot_annual", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("download_co2em_data_monthly", "Download Plot Data (.XLSX)")
            )
          ),
          nav_panel(
            HTML("Grid CO<sub>2</sub>e Factor"),
            shinycssloaders::withSpinner(
              plotlyOutput("grid_co2_plot", height = "450px"),
              type = getOption("spinner.type", default = 8)
            ),
            div(
              style = "width: 100%; text-align: right; padding: 10px;",
              downloadButton("grid_ef", "Download Plot Data (.XLSX)")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  thematic::thematic_shiny(font = "auto")

  # Store the most recently rendered client-side PNG for this user session.
  plot_image_download <- reactiveVal(NULL)

  observeEvent(input$plot_image_export, {
    payload <- input$plot_image_export

    tryCatch({
      if (
        !is.list(payload) ||
          !is.character(payload$dataUrl) ||
          length(payload$dataUrl) != 1
      ) {
        stop("The browser returned an invalid plot-image payload.")
      }

      if (!startsWith(payload$dataUrl, "data:image/png;base64,")) {
        stop("The browser did not return a PNG image.")
      }

      encoded_image <- sub(
        "^data:image/png;base64,",
        "",
        payload$dataUrl
      )
      image_raw <- jsonlite::base64_dec(encoded_image)

      if (length(image_raw) == 0) {
        stop("Plotly returned an empty PNG image.")
      }

      filename <- if (
        is.character(payload$filename) &&
          length(payload$filename) == 1 &&
          nzchar(payload$filename)
      ) {
        payload$filename
      } else {
        "plot"
      }

      filename <- gsub("[^A-Za-z0-9_-]+", "_", filename)
      plot_image_download(list(
        filename = paste0(filename, ".png"),
        content = image_raw
      ))

      session$sendCustomMessage(
        "downloadPlotImage",
        list(linkId = "download_plot_image")
      )
    }, error = function(error) {
      showNotification(
        paste("Unable to prepare the plot image:", conditionMessage(error)),
        type = "error"
      )
    })
  }, ignoreInit = TRUE)

  observeEvent(input$plot_image_export_error, {
    message <- input$plot_image_export_error$message
    if (!is.character(message) || length(message) != 1) {
      message <- "Unknown browser rendering error."
    }

    showNotification(
      paste("Unable to render the plot image:", message),
      type = "error"
    )
  }, ignoreInit = TRUE)

  output$download_plot_image <- downloadHandler(
    filename = function() {
      req(plot_image_download())
      plot_image_download()$filename
    },
    content = function(file) {
      req(plot_image_download())
      writeBin(plot_image_download()$content, file)
    },
    contentType = "image/png"
  )
  outputOptions(output, "download_plot_image", suspendWhenHidden = FALSE)

  docFilePath <- "AllUploadFiles_ToolTesting/User Guide for ELPT.pdf"

  output$downloaddocu <- downloadHandler(
    filename = function() {
      basename(docFilePath)
    },
    content = function(file) {
      file.copy(docFilePath, file)
    }
  )

  trigger_co2_panel <- reactiveVal(FALSE)

  # --- Cascading Dropdown Logic For Utility Tariffs from JSON File ---
  observeEvent(urdb_data(), {
    utilities <- sort(unique(urdb_data()$Utility_Name))
    selected <- isolate(input$utility_selector)
    if (is.null(selected) || !selected %in% utilities) selected <- ""
    updateSelectInput(session, "utility_selector",
      choices = c("Please select a utility..." = "", utilities), selected = selected
    )
  })

  observeEvent(list(input$utility_selector, urdb_data()), {
    selected_utility <- input$utility_selector
    if (is.null(selected_utility)) selected_utility <- ""

    # Filter the JSON dataframe down to just the selected utility
    available_rates <- urdb_data() %>%
      filter(Utility_Name == selected_utility) %>%
      pull(Rate_Name) %>%
      sort()

    # Push those specific rate names into the second dropdown menu
    selected <- isolate(input$rate_selector)
    if (is.null(selected) || !selected %in% available_rates) selected <- ""
    updateSelectInput(session, "rate_selector",
      choices = c("Please select a rate..." = "", available_rates), selected = selected
    )
  })


  observeEvent(list(input$rate_selector, input$utility_selector, urdb_data()), {
    # Clear stale values if the selection disappears during a database refresh.
    rate_vals(NULL)
    req(input$utility_selector, input$rate_selector)

    # 1. Grab the single JSON row for the selected utility and rate combination
    selected_rate_data <- urdb_data() %>%
      filter(Utility_Name == input$utility_selector & Rate_Name == input$rate_selector) %>%
      slice(1)

    if (nrow(selected_rate_data) == 0) {
      shinyalert("Database Error", "Selected rate data could not be parsed.", type = "error")
      return()
    }

    tryCatch({
      extracted_elpt_list <- translate_urdb_to_elpt(selected_rate_data)
      rate_vals(extracted_elpt_list)

      notification_text <- paste(
        "Automated data for", input$rate_selector, "loaded successfully!"
      )
      if (isTRUE(extracted_elpt_list$has_demand_linked_energy)) {
        notification_text <- paste0(
          notification_text,
          " kWh/kW tier limits will use each month's measured peak demand."
        )
      }
      showNotification(notification_text, type = "message", duration = 7)
    }, elpt_unsupported_rate = function(error) {
      shinyalert(
        "Rate Not Supported",
        conditionMessage(error),
        type = "warning"
      )
    }, error = function(error) {
      shinyalert(
        "Database Error",
        paste("The selected rate could not be parsed:", conditionMessage(error)),
        type = "error"
      )
    })
  })


  rate_vals <- reactiveVal(NULL)
  
  # =====================================================================
  # RATE PREVIEW HELPER FUNCTIONS (Server Scope)
  # =====================================================================
  format_hours <- function(hrs) {
    if (length(hrs) == 24) return("All Day")
    breaks <- c(0, which(diff(hrs) != 1), length(hrs))
    res <- c()
    for(i in 1:(length(breaks)-1)) {
      start_h <- hrs[breaks[i]+1]
      end_h <- hrs[breaks[i+1]] + 1 # +1 because hour 23 means 11pm-12am
      
      fmt_h <- function(h) {
        if(h == 0 || h == 24) return("12am")
        if(h == 12) return("12pm")
        if(h > 12) return(paste0(h-12, "pm"))
        return(paste0(h, "am"))
      }
      res <- c(res, paste0(fmt_h(start_h), "-", fmt_h(end_h)))
    }
    paste(res, collapse = ", ")
  }
  
  format_months <- function(mos) {
    if (length(mos) == 12) return("All Year")
    breaks <- c(0, which(diff(mos) != 1), length(mos))
    res <- c()
    for(i in 1:(length(breaks)-1)) {
      start_m <- mos[breaks[i]+1]
      end_m <- mos[breaks[i+1]]
      if (start_m == end_m) {
        res <- c(res, month.abb[start_m])
      } else {
        res <- c(res, paste0(month.abb[start_m], "-", month.abb[end_m]))
      }
    }
    paste(res, collapse = ", ")
  }
  
  format_tier_limit <- function(maximum, unit) {
    if (is.infinite(maximum)) return(paste("No maximum", unit))
    paste0("Up to ", format(maximum, big.mark = ",", trim = TRUE), " ", unit)
  }

  get_period_df <- function(sched_matrix, tier_array, title_prefix, hide_zero_rates) {
    unique_p <- unique(as.vector(sched_matrix))
    df_list <- list()
    for (p in unique_p) {
      mos <- which(rowSums(sched_matrix == p) > 0)
      hrs <- which(colSums(sched_matrix == p) > 0) - 1
      period_desc <- paste0(title_prefix, format_months(mos), " (", format_hours(hrs), ")")
      tiers <- tier_array[[p + 1]]

      for (tier_index in seq_len(nrow(tiers))) {
        rate_val <- tiers$rate[[tier_index]]
        if (rate_val > 0 || !hide_zero_rates) {
          df_list[[length(df_list) + 1]] <- data.frame(
            desc = period_desc,
            tier = paste0("Tier ", tier_index),
            maximum = format_tier_limit(tiers$max[[tier_index]], tiers$unit[[tier_index]]),
            rate = rate_val,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(df_list) > 0) return(do.call(rbind, df_list))
    return(data.frame(desc = character(), tier = character(), maximum = character(), rate = numeric()))
  }
  
  get_flat_period_df <- function(month_array, tier_array) {
    unique_p <- unique(month_array)
    df_list <- list()
    for (p in unique_p) {
      mos <- which(month_array == p)
      period_desc <- format_months(mos)
      tiers <- tier_array[[p + 1]]
      for (tier_index in seq_len(nrow(tiers))) {
        rate_val <- tiers$rate[[tier_index]]
        if (rate_val > 0) {
          df_list[[length(df_list) + 1]] <- data.frame(
            desc = period_desc,
            tier = paste0("Tier ", tier_index),
            maximum = format_tier_limit(tiers$max[[tier_index]], tiers$unit[[tier_index]]),
            rate = rate_val,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(df_list) > 0) return(do.call(rbind, df_list))
    return(data.frame(desc = character(), tier = character(), maximum = character(), rate = numeric()))
  }
  
  # =====================================================================
  # 1. SHARED REACTIVE FOR UI AND EXCEL DOWNLOAD
  # =====================================================================
  rate_preview_data <- reactive({
    req(rate_vals())
    r <- rate_vals()
    
    # Energy Data
    if (identical(r$e_sched, r$w_sched)) {
      e_df <- get_period_df(r$e_sched, r$e_tiers, "", FALSE)
    } else {
      e_df <- rbind(
        get_period_df(r$e_sched, r$e_tiers, "Wkdy: ", FALSE),
        get_period_df(r$w_sched, r$e_tiers, "Wknd: ", FALSE)
      )
    }
    
    # TOU Demand Data
    d_df <- data.frame(desc = character(), tier = character(), maximum = character(), rate = numeric())
    if (any(vapply(r$d_tiers, function(tiers) any(tiers$rate > 0), logical(1)))) {
      if (identical(r$d_sched, r$dw_sched)) {
        d_df <- get_period_df(r$d_sched, r$d_tiers, "", TRUE)
      } else {
        d_df <- rbind(
          get_period_df(r$d_sched, r$d_tiers, "Wkdy: ", TRUE),
          get_period_df(r$dw_sched, r$d_tiers, "Wknd: ", TRUE)
        )
      }
    }
    
    # Flat Demand Data
    f_df <- data.frame(desc = character(), tier = character(), maximum = character(), rate = numeric())
    if (any(vapply(r$flat_tiers, function(tiers) any(tiers$rate > 0), logical(1)))) {
      f_df <- get_flat_period_df(r$flat_months, r$flat_tiers)
    }
    
    list(energy = e_df, tou = d_df, flat = f_df)
  })
  
  # =====================================================================
  # 2. RENDER UI PREVIEW
  # =====================================================================
  output$rate_preview_ui <- renderUI({
    req(rate_preview_data())
    tables <- rate_preview_data()
    
    df_to_html_rows <- function(df) {
      if (nrow(df) == 0) return(list())
      lapply(1:nrow(df), function(i) {
        tags$tr(
          tags$td(df$desc[i], style = "padding: 6px 4px; border-bottom: 1px solid #ddd; font-size: 11.5px;"),
          tags$td(paste(df$tier[i], df$maximum[i], sep = ": "),
                  style = "padding: 6px 4px; border-bottom: 1px solid #ddd; font-size: 11.5px;"),
          tags$td(paste0("$", format(round(df$rate[i], 4), nsmall=4)), 
                  style = "padding: 6px 4px; border-bottom: 1px solid #ddd; font-size: 11.5px; text-align: right;")
        )
      })
    }
    
    e_rows <- df_to_html_rows(tables$energy)
    d_rows <- df_to_html_rows(tables$tou)
    f_rows <- df_to_html_rows(tables$flat)
    
    div(
      style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;",
      tags$h5("Rate Preview", style = "margin-top: 0; border-bottom: 2px solid #2c3e50; padding-bottom: 10px; margin-bottom: 15px; color: #2c3e50;"),
      if (isTRUE(rate_vals()$has_demand_linked_energy)) {
        tags$p(
          "Demand-linked energy tiers: each kWh/kW maximum is multiplied by that month's measured peak demand.",
          style = "font-size: 11.5px; color: #555;"
        )
      },
      if (rate_vals()$fixed_charge > 0) {
        tags$p(
          paste0(
            "Fixed charge: $", format(round(rate_vals()$fixed_charge, 2), nsmall = 2),
            if (identical(rate_vals()$fixed_charge_units, "$/day")) " per day" else " per month"
          ),
          style = "font-size: 11.5px; color: #555;"
        )
      },
      
      # Energy Rates Table
      tags$strong("Energy Rates", style = "font-size: 13px;"),
      tags$table(
        style = "width: 100%; margin-top: 5px; margin-bottom: 15px; border-collapse: collapse;",
          tags$thead(tags$tr(
            tags$th("Period", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
            tags$th("Tier / Maximum", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
            tags$th("Rate ($/kWh)", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: right; font-size: 12px;")
        )),
        tags$tbody(e_rows)
      ),
      
      # TOU Demand Rates Table
      if (nrow(tables$tou) > 0) {
        tagList(
          tags$strong("Time of Use Demand", style = "font-size: 13px;"),
          tags$table(
            style = "width: 100%; margin-top: 5px; margin-bottom: 15px; border-collapse: collapse;",
            tags$thead(tags$tr(
              tags$th("Period", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
              tags$th("Tier / Maximum", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
              tags$th("Rate ($/kW)", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: right; font-size: 12px;")
            )),
            tags$tbody(d_rows)
          )
        )
      },
      
      # Flat Demand Rates Table
      if (nrow(tables$flat) > 0) {
        tagList(
          tags$strong("Seasonal/Monthly Demand", style = "font-size: 13px;"),
          tags$table(
            style = "width: 100%; margin-top: 5px; border-collapse: collapse;",
            tags$thead(tags$tr(
              tags$th("Period", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
              tags$th("Tier / Maximum", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: left; font-size: 12px;"),
              tags$th("Rate ($/kW)", style = "padding: 4px; border-bottom: 2px solid #2c3e50; text-align: right; font-size: 12px;")
            )),
            tags$tbody(f_rows)
          )
        )
      },
      
      # NEW: Excel Download Button
      div(
        style = "margin-top: 15px; text-align: center; border-top: 1px solid #dee2e6; padding-top: 10px;",
        downloadLink(
          outputId = "download_rate_preview", 
          label = HTML("<i class='glyphicon glyphicon-download-alt'></i> Download Rate Tables (.xlsx)"), 
          style = "font-size: 12px; font-weight: bold; color: #18bc9c;"
        )
      )
    )
  })
  
  # =====================================================================
  # 3. EXCEL DOWNLOAD HANDLER (Simplified - Single Sheet)
  # =====================================================================
  output$download_rate_preview <- downloadHandler(
    filename = function() {
      clean_util <- gsub("[^A-Za-z0-9]", "_", input$utility_selector)
      paste0("Rate_Summary_", clean_util, ".xlsx")
    },
    content = function(file) {
      req(rate_preview_data())
      tables <- rate_preview_data()
      
      # Tag each table with its Type and Unit, then combine them
      res <- list()
      if(nrow(tables$energy) > 0) {
        res[[length(res) + 1]] <- data.frame(Type = "Energy", Period = tables$energy$desc, Tier = tables$energy$tier, Maximum = tables$energy$maximum, Rate = tables$energy$rate, Unit = "$/kWh", stringsAsFactors = FALSE)
      }
      if(nrow(tables$tou) > 0) {
        res[[length(res) + 1]] <- data.frame(Type = "TOU Demand", Period = tables$tou$desc, Tier = tables$tou$tier, Maximum = tables$tou$maximum, Rate = tables$tou$rate, Unit = "$/kW", stringsAsFactors = FALSE)
      }
      if(nrow(tables$flat) > 0) {
        res[[length(res) + 1]] <- data.frame(Type = "Seasonal Demand", Period = tables$flat$desc, Tier = tables$flat$tier, Maximum = tables$flat$maximum, Rate = tables$flat$rate, Unit = "$/kW", stringsAsFactors = FALSE)
      }
      if(rate_vals()$fixed_charge > 0) {
        res[[length(res) + 1]] <- data.frame(
          Type = "Fixed Charge", Period = "Each billing period", Tier = "-",
          Maximum = "-", Rate = rate_vals()$fixed_charge,
          Unit = rate_vals()$fixed_charge_units, stringsAsFactors = FALSE
        )
      }
      
      # Bind into one master table
      export_df <- do.call(rbind, res)
      
      # Write directly to Excel (write.xlsx handles all the workbook background tasks automatically)
      write.xlsx(export_df, file, asTable = TRUE, colWidths = "auto")
    }
  )

  usage_incl <- reactive({
    req(rate_vals())
    rate_vals()$usage_incl
  })
  demand_incl <- reactive({
    req(rate_vals())
    rate_vals()$demand_incl
  })
  fixed_usage <- reactive({
    req(rate_vals())
    rate_vals()$fixed_usage
  })
  fixed_demand <- reactive({
    req(rate_vals())
    rate_vals()$fixed_demand
  })
  has_partpeak <- reactive({
    req(rate_vals())
    rate_vals()$has_partpeak
  })
  has_monthlymax <- reactive({
    req(rate_vals())
    rate_vals()$has_monthlymax
  })
  summer_start_month <- reactive({
    req(rate_vals())
    rate_vals()$summer_start_m
  })
  summer_end_month <- reactive({
    req(rate_vals())
    rate_vals()$summer_end_m
  })
  summer_offpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_off_start_s
  })
  summer_offpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_off_end_s
  })
  summer_partpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_part_start_s
  })
  summer_partpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_part_end_s
  })
  summer_onpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_on_start_s
  })
  summer_onpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_on_end_s
  })
  summer_offpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_off_s
  })
  summer_partpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_part_s
  })
  summer_onpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_on_s
  })
  summer_fixed_rate <- reactive({
    req(rate_vals())
    rate_vals()$summer_fixed_rate
  })
  summer_dc_offpeak <- reactive({
    req(rate_vals())
    rate_vals()$dem_off_s
  })
  summer_partpeak_demand <- reactive({
    req(rate_vals())
    rate_vals()$dem_part_s
  })
  summer_dc_onpeak <- reactive({
    req(rate_vals())
    rate_vals()$dem_on_s
  })
  summer_max_demand <- reactive({
    req(rate_vals())
    rate_vals()$max_dem_s
  })
  winter_start_month <- reactive({
    req(rate_vals())
    rate_vals()$winter_start_m
  })
  winter_end_month <- reactive({
    req(rate_vals())
    rate_vals()$winter_end_m
  })
  winter_offpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_off_start_w
  })
  winter_offpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_off_end_w
  })
  winter_partpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_part_start_w
  })
  winter_partpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_part_end_w
  })
  winter_onpeak_start_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_on_start_w
  })
  winter_onpeak_end_hour <- reactive({
    req(rate_vals())
    rate_vals()$tm_on_end_w
  })
  winter_offpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_off_w
  })
  winter_partpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_part_w
  })
  winter_onpeak_rate <- reactive({
    req(rate_vals())
    rate_vals()$rate_on_w
  })
  winter_fixed_rate <- reactive({
    req(rate_vals())
    rate_vals()$winter_fixed_rate
  })
  winter_dc_offpeak <- reactive({
    req(rate_vals())
    rate_vals()$dem_off_w
  })
  winter_partpeak_demand <- reactive({
    req(rate_vals())
    rate_vals()$dem_part_w
  })
  winter_dc_onpeak <- reactive({
    req(rate_vals())
    rate_vals()$dem_on_w
  })
  winter_max_demand <- reactive({
    req(rate_vals())
    rate_vals()$max_dem_w
  })


  theme <- shinytheme("flatly")

  observeEvent(input$generate_co2_plot, {
    if (is.null(input$loadpf_file) || input$state == "") {
      shinyalert(
        "Missing Inputs",
        "Please upload an electrical load file in Load Input and select a state in Emissions Input before generating the plot.",
        type = "warning"
      )
      trigger_co2_panel(FALSE)
    } else {
      trigger_co2_panel(TRUE)
      
      # --- NEW: One-line auto-scroll (delays 500ms to allow UI to render first) ---
      shinyjs::runjs("setTimeout(function() { document.getElementById('co2_panel').scrollIntoView({ behavior: 'smooth', block: 'start' }); }, 500);")
    }
  })


  output$fileInputUI <- renderUI({
    is_template_based <- input$green_manual %in% c("Custom Hourly Load", "12 Months Utility Bills")
    upload_number <- if (is_template_based) "4." else "3."
    upload_label_text <- switch(input$green_manual,
      "Custom Hourly Load"        = "Upload Hourly Electrical Load Data (.XLSX)",
      "12 Months Utility Bills"   = "Upload 12 Months Utility Bill Data (.XLSX)",
      "Green Button: 15-Minute"   = "Upload Green Button Data (.XML)",
      "Green Button: Hourly"      = "Upload Green Button Data (.XML)"
    )


    fileInput("loadpf_file", label = paste(upload_number, upload_label_text))
  })

  observeEvent(input$green_manual, {
    reset("loadpf_file")
  })

  output$BillcustomUI <- renderUI({
    if (input$green_manual == "Custom Hourly Load") {
      div(
        tags$p(tags$strong("3. Download Custom Hourly Template")),
        downloadLink("downloadSheet", "Click here to download the template")
      )
    } else if (input$green_manual == "12 Months Utility Bills") {
      div(
        tags$p(tags$strong("3. Download 12 Months Bills Template")),
        downloadLink("downloadSheet1", "Click here to download the template")
      )
    }
  })

  output$fileUploaded <- reactive({
    !is.null(input$loadpf_file)
  })

  output$rateFileUploaded <- reactive({
    !is.null(rate_vals())
  })

  outputOptions(output, "rateFileUploaded", suspendWhenHidden = FALSE)

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

      data <- data.frame(
        "Flexible Load (in kW)" = flexible_load,
        "Time Range To Start" = time_range_to_start,
        "Time Range To End" = time_range_to_end,
        "Time Range From Start" = time_range_from_start,
        "Time Range From End" = time_range_from_end,
        check.names = FALSE
      )
    } else if (input$addshed_shift == "Add/Shed Load") {
      flexible_load <- c(50, 25, 30)
      time_range_to_start <- as.POSIXct(c("10:00", "11:00", "13:00"), format = "%H:%M")
      time_range_to_end <- as.POSIXct(c("11:00", "12:00", "14:00"), format = "%H:%M")

      extract_time <- function(datetime) {
        format(datetime, format = "%H:%M")
      }

      time_range_to_start <- sapply(time_range_to_start, extract_time)
      time_range_to_end <- sapply(time_range_to_end, extract_time)

      data <- data.frame(
        "Added/Shedded Load (in kW)" = flexible_load,
        "Time Range To Start" = time_range_to_start,
        "Time Range To End" = time_range_to_end,
        check.names = FALSE
      )
    }

    empty_input_sheet_defaults$data <- data
  })

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

  # Download handler for the load data Excel file
  output$downloadSheet <- downloadHandler(
    filename = function() {
      "load_pf_input.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", input_load_df())

      datetime_style <- createStyle(
        numFmt = "yyyy-mm-dd hh:mm:ss",
        fgFill = "#D3D3D3",
        borderColour = "black",
        locked = TRUE
      )

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
  # Download handler for the pristine 12-Month Utility Bills Excel file
  output$downloadSheet1 <- downloadHandler(
    filename = function() {
      "Utility Bills Template.xlsx"
    },
    content = function(file) {
      # Simply copy the pristine template directly to the user's download
      file.copy("AllUploadFiles_ToolTesting/Utility Bills Template.xlsx", file)
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
        # Added flex-wrap and adjusted the gap
        style = "display: flex; flex-wrap: wrap; gap: 15px; align-items: flex-end; margin-bottom: 15px;",
        div(style = "flex: 0 1 100px;", 
            numericInput(inputId = paste0("numeric_", id),
                         label = load_input_label("Flexible Load (kW)", "Enter amount to shift load"),
                         value = 25, width = "100%", step = 25, min = 0)
        ),
        div(style = "flex: 1 1 200px;", 
            sliderInput(inputId = paste0("slider_from", id), label = "Time Range From:", min = as.POSIXct("00:00", format = "%H:%M"), max = as.POSIXct("23:59", format = "%H:%M"), value = c(as.POSIXct("17:00", format = "%H:%M"), as.POSIXct("19:00", format = "%H:%M")), timeFormat = "%H:%M", width = "100%")
        ),
        div(style = "flex: 1 1 200px;", 
            sliderInput(inputId = paste0("slider_to", id), label = "Time Range To:", min = as.POSIXct("00:00", format = "%H:%M"), max = as.POSIXct("23:59", format = "%H:%M"), value = c(as.POSIXct("12:00", format = "%H:%M"), as.POSIXct("14:00", format = "%H:%M")), timeFormat = "%H:%M", width = "100%")
        )
      )
    )
  }
  
  createInputSet_1 <- function(id_1, user_tz) {
    tagList(
      tags$h4(paste0("New Load ", id_1)),
      div(
        # Added flex-wrap and removed the 400px hardcode
        style = "display: flex; flex-wrap: wrap; gap: 15px; align-items: flex-end; margin-bottom: 15px;",
        div(style = "flex: 0 1 100px;", 
            numericInput(inputId = paste0("numeric_add_", id_1),
                         label = load_input_label("Added Load (kW)", "Enter + to add or – to shed load"),
                         value = 25, width = "100%", step = 25)
        ),
        div(style = "flex: 1 1 200px;", 
            sliderInput(inputId = paste0("slider_to_add", id_1), label = "Time Range To Add Load:", min = as.POSIXct("00:00", format = "%H:%M"), max = as.POSIXct("23:59", format = "%H:%M"), value = c(as.POSIXct("12:00", format = "%H:%M"), as.POSIXct("14:00", format = "%H:%M")), timeFormat = "%H:%M", width = "100%")
        )
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

  ## Excluding load management windows from the bookmarking features##

  observe({
    exclude_patterns <- c("^slider_from", "^numeric_", "^slider_to", "^slider_to_add")

    names_to_exclude <- unlist(lapply(exclude_patterns, function(pattern) {
      grep(pattern, names(input), value = TRUE)
    }))

    names_to_append <- c("green_manual", "start_date", "end_date", "emissions_type", "loadpf_file", "state")

    names_to_exclude <- c(names_to_exclude, names_to_append)

    setBookmarkExclude(names_to_exclude)
  })

  ## Excluding load management windows from the bookmarking features##


  observeEvent(input$plot_button, {
    if (is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Warning", "No input electrical load data and GHG emissions parameters provided. Please upload an electrical load data file and select your GHG emissions parameters.", type = "warning")
    } else if (is.null(input$loadpf_file)) {
      shinyalert("Warning", "No input electrical load data provided. Please upload an electrical load data file.", type = "warning")
    } else if (!is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "You have not specified an analysis state. Please select a state in Emissions Input.", type = "error", )
    }
  })


  observeEvent(input$plot_button_2, {
    if (is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "No input electrical load data and GHG emissions parameters provided. Please upload an electrical load data file and select your GHG emissions parameters.", type = "error")
    } else if (is.null(input$loadpf_file)) {
      shinyalert("Error", "No input electrical load data provided. Please upload an electrical load data file.", type = "error")
    } else if (!is.null(input$loadpf_file) && input$state == "") {
      shinyalert("Error", "You have not specified your facility's state. Please select a state in Emissions Input.", type = "error", )
    }
  })

  observeEvent(input$generate_co2_plot, {
    if (is.null(input$loadpf_file) || input$state == "") {
      shinyalert(
        "Missing Inputs",
        "Please upload an electrical load file in Load Input and select a state in Emissions Input before generating the plot.",
        type = "warning"
      )
      trigger_co2_panel(FALSE)
    } else {
      trigger_co2_panel(TRUE)
    }
  })


  output$showCO2Panel <- reactive({
    trigger_co2_panel()
  })
  outputOptions(output, "showCO2Panel", suspendWhenHidden = FALSE)


  df_final_across_events <- reactiveValues(df_loadpf_long_fin = 0)

  # =====================================================================
  # MASTER LOAD PARSER (Handles all 4 upload types safely)
  # =====================================================================
  parsed_base_load <- reactive({
    req(input$loadpf_file, input$date_range, input$user_time_zone)
    
    desired_tz <- input$user_time_zone
    loadpf_file <- input$loadpf_file
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Reporting years may cross December (for example Aug 2025-Jul 2026).
    if (start_date > end_date) {
      shinyalert("Error", "Entered start date is greater than end date. Please correct the range.", type = "warning")
      updateDateRangeInput(session, "date_range", start = Sys.Date(), end = Sys.Date() + 1)
      req(FALSE)
    }
    
    # ---------------------------------------------------------
    # PATH 1: CUSTOM HOURLY LOAD
    # ---------------------------------------------------------
    if (input$green_manual == "Custom Hourly Load") {
      df_loadpf_long_fin <- read_excel(loadpf_file$datapath) %>%
        rename(Load = `Load (in kW)`, datetime = Date_Time)
      
      if (!"Load" %in% colnames(df_loadpf_long_fin)) {
        showModal(modalDialog(title = "Warning", "Please Upload the Correct File", easyClose = TRUE))
        req(FALSE) # Stop execution safely
      }
      df_loadpf_long_fin$datetime <- as.POSIXct(df_loadpf_long_fin$datetime, format = "%d/%m/%Y %H:%M")
      
      # ---------------------------------------------------------
      # PATH 2: 12 MONTHS UTILITY BILLS (Safe Extraction Logic)
      # ---------------------------------------------------------
    } else if (input$green_manual == "12 Months Utility Bills") {
      
      df_loadpf <- read_excel(loadpf_file$datapath, range = "A1:D13") %>% janitor::clean_names()
      
      if (!"energy_usage_k_wh" %in% colnames(df_loadpf)) {
        showModal(modalDialog(title = "Warning", "Please Upload the Correct File", easyClose = TRUE))
        req(FALSE)
      }
      
      df_loadpf$energy_usage_k_wh <- as.numeric(gsub(",", "", df_loadpf$energy_usage_k_wh))
      df_loadpf$billed_demand_k_w <- as.numeric(gsub(",", "", df_loadpf$billed_demand_k_w))
      
      # SAFE CELL EXTRACTION HELPER (Prevents the $...1 crash!)
      extract_cell <- function(cell_range, default_val) {
        cell_data <- suppressMessages(try(read_excel(loadpf_file$datapath, range = cell_range, col_names = FALSE), silent = TRUE))
        if (inherits(cell_data, "try-error") || nrow(cell_data) == 0 || ncol(cell_data) == 0) return(default_val)
        val <- cell_data[[1]][1]
        if (is.na(val) || val == "") return(default_val)
        return(val)
      }
      
      work_on_weekends <- as.character(extract_cell("G6", "N"))
      start_time       <- as.numeric(extract_cell("C108", 8))
      end_time         <- as.numeric(extract_cell("C109", 17))
      pd               <- as.numeric(extract_cell("C110", 14))
      
      hours_of_operation <- if_else(end_time == start_time, 24, if_else(end_time < start_time, 24 - start_time + end_time, end_time - start_time))
      non_working_hours <- abs(24 - hours_of_operation)
      peak_hours <- 1
      mid_low_hours <- hours_of_operation - peak_hours
      
      # Rotate the template's January-December rows into the selected reporting
      # period. This also assigns the correct year and leap-year day count.
      bill_month_starts <- seq(
        floor_date(as.Date(start_date), unit = "month"),
        by = "month",
        length.out = 12
      )
      bill_calendar <- data.frame(
        month_start = as.Date(bill_month_starts),
        month = month.name[month(bill_month_starts)],
        calendar_year = year(bill_month_starts),
        calendar_month = month(bill_month_starts),
        days = days_in_month(bill_month_starts)
      )
      df_loadpf_2 <- bill_calendar %>%
        left_join(df_loadpf, by = "month")
      
      # Determine Weekday vs Weekend math
      if (work_on_weekends == "Y") {
        df_loadpf_3 <- df_loadpf_2 %>%
          mutate(
            kwhpd = energy_usage_k_wh / days,
            baseline_demand_pd = 0.3 * billed_demand_k_w,
            peak_demand_pd = billed_demand_k_w,
            mid_demand_pd = (kwhpd - non_working_hours * baseline_demand_pd - peak_demand_pd * peak_hours) / mid_low_hours
          )
      } else {
        count_weekdays <- function(year, month) {
          s_date <- make_date(year, month, 1)
          e_date <- s_date + months(1) - days(1)
          sum(!weekdays(seq.Date(s_date, e_date, by = "day")) %in% c("Saturday", "Sunday"))
        }
        df_loadpf_3 <- df_loadpf_2 %>%
          mutate(
            no_of_weekdays = mapply(count_weekdays, calendar_year, calendar_month),
            no_of_weekends = days - no_of_weekdays,
            baseline_demand_pd = 0.3 * billed_demand_k_w,
            kwh_wknd = baseline_demand_pd * 24 * no_of_weekends,
            kwh_wkd = energy_usage_k_wh - kwh_wknd,
            kwhpd_wknd = baseline_demand_pd,
            kwhpd_wkd = kwh_wkd / no_of_weekdays,
            peak_demand_pd = billed_demand_k_w,
            mid_demand_pd = (kwhpd_wkd - non_working_hours * baseline_demand_pd - peak_demand_pd * peak_hours) / mid_low_hours
          ) %>% rename(kwhpd = kwhpd_wkd) # Alias to match 'Y' logic below
      }
      
      df_loadpf_4 <- df_loadpf_3 %>% pivot_longer(cols = c("baseline_demand_pd", "peak_demand_pd", "mid_demand_pd"), names_to = "energy_type", values_to = "kW")
      
      hours_df <- data.frame(hour = 0:23) %>%
        mutate(energy_type = case_when(
          hour == pd ~ "peak_demand_pd",
          hour >= start_time & hour < pd | hour < end_time & hour > pd | end_time < start_time & hour >= start_time | end_time < start_time & hour < end_time ~ "mid_demand_pd",
          TRUE ~ "baseline_demand_pd"
        ))
      
      hours_df_3 <- cbind(data.frame(month = rep(month.name, each = 24)), do.call(rbind, replicate(12, hours_df, simplify = FALSE)))
      
      df_loadpf_5 <- data.frame(datetime = seq(as.POSIXct(paste(start_date, "00:00:00"), tz = desired_tz),
                                               as.POSIXct(paste(end_date, "23:00:00"), tz = desired_tz), by = "hour")) %>%
        mutate(month = month(datetime, label = TRUE, abbr = FALSE), hour = hour(datetime) %% 24, day = weekdays(datetime)) %>%
        left_join(hours_df_3, by = c("month", "hour")) %>%
        left_join(df_loadpf_4, by = c("month", "energy_type")) %>%
        mutate(Day = day(datetime))
      
      if (work_on_weekends == "N") {
        df_loadpf_5 <- df_loadpf_5 %>% mutate(
          energy_type = if_else(day %in% c("Saturday", "Sunday"), "baseline_demand_pd", energy_type),
          kW = if_else(day %in% c("Saturday", "Sunday"), 0.3 * billed_demand_k_w, kW)
        )
      }
      
      df_loadpf_long_fin <- df_loadpf_5 %>% select(datetime, month, Day, hour, kW) %>% rename(Load = kW, hours = hour)
      
      # ---------------------------------------------------------
      # PATH 3 & 4: GREEN BUTTON (Combines Hourly and 15-Min)
      # ---------------------------------------------------------
    } else if (input$green_manual %in% c("Green Button: Hourly", "Green Button: 15-Minute")) {
      
      loadpf <- read_xml(loadpf_file$datapath)
      ns <- c(espi = "http://naesb.org/espi")
      interval_readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      starts <- as.POSIXct(as.integer(xml_text(xml_find_all(interval_readings, ".//espi:timePeriod/espi:start", ns))), origin = "1970-01-01")
      values <- as.integer(xml_text(xml_find_all(interval_readings, ".//espi:value", ns)))
      
      df_loadpf <- data.frame(DATE = starts, USAGE = values)
      
      if (!"USAGE" %in% colnames(df_loadpf)) {
        showModal(modalDialog(title = "Warning", "Please Upload the Correct File", easyClose = TRUE))
        req(FALSE)
      }
      
      df_loadpf_long_fin <- df_loadpf %>%
        mutate(Load = USAGE / 1000, datetime = DATE, month = format(datetime, "%B"), hour = hour(datetime)) %>%
        select(Load, month, datetime, hour) %>%
        distinct(datetime, .keep_all = TRUE)
    }
    
    # ---------------------------------------------------------
    # FINAL FORMATTING (Applies to all paths)
    # ---------------------------------------------------------
    # Uploaded timestamps are authoritative. Emissions factors are aligned to
    # them later by month/day/time without rewriting the facility's dates.
    df_loadpf_long_fin %>%
      mutate(
        datetime = as.POSIXct(datetime, tz = desired_tz),
        Load = as.numeric(Load),
        month = format(datetime, "%B")
      ) %>%
      filter(!is.na(datetime), !is.na(Load)) %>%
      arrange(datetime)
  })

  # Interval files contain their own dates, so select their full uploaded range
  # automatically. The 12-month bill template has month names only and therefore
  # continues to use the date range selected by the user.
  observeEvent(input$loadpf_file, {
    req(input$loadpf_file)
    if (input$green_manual == "12 Months Utility Bills") return()

    uploaded_load <- isolate(parsed_base_load())
    req(is.data.frame(uploaded_load), nrow(uploaded_load) > 0)

    uploaded_start <- as.Date(min(uploaded_load$datetime, na.rm = TRUE))
    uploaded_end <- as.Date(max(uploaded_load$datetime, na.rm = TRUE))
    updateDateRangeInput(
      session,
      "date_range",
      start = uploaded_start,
      end = uploaded_end,
      min = uploaded_start,
      max = uploaded_end
    )
  }, ignoreInit = TRUE)

  # =====================================================================
  # 1. THE VECTORIZED COST ENGINE (Replaces 4,000 lines of if/else)
  # =====================================================================
  calculated_costs <- reactive({
    req(input$loadpf_file, rate_vals())

    df <- parsed_base_load() %>%
      filter(
        datetime >= as.POSIXct(input$date_range[1]),
        datetime < as.POSIXct(input$date_range[2] + 1)
      )
    req(is.data.frame(df))

    month_levels <- reporting_month_label(sort(unique(reporting_month_start(df$datetime))))
    calculate_tou_bill(df, "Load", rate_vals()) %>%
      pivot_longer(
        cols = c(Usage_Cost, Demand_Cost),
        names_to = "energy_demand", values_to = "cost"
      ) %>%
      mutate(
        original_modified = "Baseline",
        energy_demand = recode(energy_demand, Usage_Cost = "Usage Cost", Demand_Cost = "Demand Cost"),
        month = factor(reporting_month_label(month_start), levels = month_levels)
      )
  })

  # calculated_costs() is already based on the selected timestamp range.
  filtered_cost_data <- reactive({
    calculated_costs()
  })

  # =====================================================================
  # 2. COST PLOTS & DOWNLOADS (Safely un-nested!)
  # =====================================================================
  output$cost_plot <- renderPlotly({
    df_final <- filtered_cost_data()

    gg_cost_plot <- ggplot() +
      geom_bar(
        data = df_final,
        aes(
          x = original_modified,
          y = cost,
          fill = energy_demand,
          text = paste0(
            original_modified, " ", energy_demand, " = ",
            "\n", scales::dollar(cost, accuracy = 1), "/month"
          )
        ),
        stat = "identity",
        position = "stack",
        alpha = 0.9
      ) +
      labs(title = "Electricity Costs", x = "Baseline Period", y = "Total Cost ($/month)") +
      theme_clean() +
      scale_y_continuous(labels = dollar_format()) +
      theme(
        legend.position = "bottom",
        text = element_text(family = "Open Sans", size = 14),
        axis.title = element_text(family = "Open Sans", size = 16, face = "bold"),
        legend.title = element_text(family = "Open Sans", size = 14, face = "bold"),
        legend.text = element_text(family = "Open Sans", size = 12),
        plot.title = element_text(family = "Open Sans", hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(family = "Open Sans", size = 11),
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
      facet_wrap(. ~ month, nrow = 1)

    ggplotly(gg_cost_plot, tooltip = "text") %>%
      configure_plotly_download(
        filename = "monthly_electricity_costs",
        remove_buttons = cost_plot_modebar_buttons_to_remove
      )
  })

  output$download_costselect_data <- downloadHandler(
    filename = function() {
      "cost_data.xlsx"
    },
    content = function(file) {
      write.xlsx(filtered_cost_data(), file)
    }
  )
  
  # =====================================================================
  # COST KPI VALUE BOXES (Displays on Monthly Cost Tab)
  # =====================================================================
  output$cost_kpi_ui <- renderUI({
    req(cost_comparison())
    period_label <- reporting_period_label(modified_load_data()$datetime)
    
    # 1. Summarize annual costs by scenario and type
    df_sums <- cost_comparison() %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_cost = sum(cost), .groups = "drop")
    
    # Helper to safely extract values
    get_val <- function(om, ed) {
      val <- df_sums %>% filter(original_modified == om, energy_demand == ed) %>% pull(annual_cost)
      if(length(val) == 0) return(0)
      return(val[1])
    }
    
    # 2. Get Baseline Values
    base_usage <- get_val("Baseline", "Usage Cost")
    base_demand <- get_val("Baseline", "Demand Cost")
    base_total <- base_usage + base_demand
    
    # Check if user has generated modified loads
    is_modified <- (isTruthy(input$plot_button) && input$plot_button > 0) || 
      (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)
    
    # 3. Build the KPI UI
    if (!is_modified) {
      # SCENARIO A: Show Baseline Totals (Clean light theme, color on icon only)
      bslib::layout_column_wrap(
        width = "250px", gap = "15px",
        bslib::value_box(
          title = paste0("Total Cost (", period_label, ")"),
          value = scales::dollar(base_total, accuracy = 1),
          theme = "light",
          showcase = icon("money-bill-wave", class = "text-primary") # Subtle blue icon
        ),
        bslib::value_box(
          title = "Reporting-Period Usage Cost",
          value = scales::dollar(base_usage, accuracy = 1),
          theme = "light"
        ),
        bslib::value_box(
          title = "Reporting-Period Demand Cost",
          value = scales::dollar(base_demand, accuracy = 1),
          theme = "light"
        )
      )
    } else {
      # SCENARIO B: Show Modified Deltas (Clean light theme, red/green on arrows only)
      mod_usage <- get_val("Modified", "Usage Cost")
      mod_demand <- get_val("Modified", "Demand Cost")
      mod_total <- mod_usage + mod_demand
      
      diff_total <- mod_total - base_total
      diff_usage <- mod_usage - base_usage
      diff_demand <- mod_demand - base_demand
      
      # Helper function: Removes heavy background, applies color to text/icon only
      format_impact <- function(diff_val) {
        if (diff_val < -0.01) {
          list(txt = paste("Savings:", scales::dollar(abs(diff_val), accuracy = 1)), 
               thm = "light", 
               icn = icon("arrow-down", class = "text-success")) # Subtle green icon
        } else if (diff_val > 0.01) {
          list(txt = paste("Increase:", scales::dollar(abs(diff_val), accuracy = 1)), 
               thm = "light", 
               icn = icon("arrow-up", class = "text-danger"))   # Subtle red icon
        } else {
          list(txt = "No Change", 
               thm = "light", 
               icn = icon("minus", class = "text-muted"))       # Subtle gray icon
        }
      }
      
      fmt_total <- format_impact(diff_total)
      fmt_usage <- format_impact(diff_usage)
      fmt_demand <- format_impact(diff_demand)
      
      bslib::layout_column_wrap(
        width = 1/3, gap = "15px",
        bslib::value_box(
          title = "Total Net Impact",
          value = fmt_total$txt,
          theme = fmt_total$thm,
          showcase = fmt_total$icn
        ),
        bslib::value_box(
          title = "Usage Cost Impact",
          value = fmt_usage$txt,
          theme = fmt_usage$thm,
          showcase = fmt_usage$icn
        ),
        bslib::value_box(
          title = "Demand Cost Impact",
          value = fmt_demand$txt,
          theme = fmt_demand$thm,
          showcase = fmt_demand$icn
        )
      )
    }
  })

  output$cost_plot_annual <- renderPlotly({
    df_annual_final <- cost_comparison() %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_costs = sum(cost), .groups = "drop")
    
    annual_max <- sum(df_annual_final$annual_costs)
    
    period_label <- reporting_period_label(modified_load_data()$datetime)
    
    p <- ggplot(df_annual_final, aes(
      x = original_modified, y = annual_costs, fill = energy_demand,
      text = paste0(original_modified, " Annual ", energy_demand, " =\n", scales::dollar(annual_costs, accuracy = 1), "/yr")
    )) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
      labs(title = paste0("Annual Costs Summary (", period_label, ")"), x = "", y = "<b>Total Cost ($/yr)</b>") +
      theme_clean() +
      scale_y_continuous(labels = dollar_format(), limits = c(0, max(annual_max, 1) * 1.2)) +
      theme(
        legend.position = "bottom",
        text = element_text(family = "Open Sans", size = 14),
        axis.title = element_text(family = "Open Sans", size = 16, face = "bold"),
        legend.title = element_text(family = "Open Sans", size = 14, face = "bold"),
        legend.text = element_text(family = "Open Sans", size = 12),
        plot.title = element_text(family = "Open Sans", hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(family = "Open Sans", size = 11),
        axis.text.x = element_text(family = "Open Sans", size = 10),
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines")
      ) +
      scale_fill_manual(name = "Cost Type", labels = c("Usage Cost", "Demand Cost"), values = c("#FFA600", "#00313C"))
    
    ggplotly(p, tooltip = "text") %>%
      configure_plotly_download("annual_electricity_costs")
  })
  
  # =====================================================================
  # 6. LCAC / MACC ABATEMENT PLOT (The Final Graphic)
  # =====================================================================
  # =====================================================================
  # 6. LCAC / MACC ABATEMENT PLOT (The Final Graphic)
  # =====================================================================
  # =====================================================================
  # 6. LCAC / MACC ABATEMENT PLOT (The Final Graphic)
  # =====================================================================
  output$lcac_plot <- renderPlotly({
    
    # 1. Wait for user to trigger a calculation
    if (is.null(input$plot_button) && is.null(input$plot_button_2)) return(plotly_empty())
    if (input$plot_button == 0 && input$plot_button_2 == 0) return(plotly_empty())
    
    df_cost <- cost_comparison()
    df_em_list <- calculated_emissions()
    
    # 2. Calculate Monthly Cost Impacts (Modified - Baseline). Negative = Savings.
    df_cost_summary <- df_cost %>%
      group_by(month_start, month, original_modified) %>%
      summarise(total_cost = sum(cost), .groups = "drop") %>%
      pivot_wider(names_from = original_modified, values_from = total_cost) %>%
      mutate(cost_impact = Modified - Baseline) 
    
    # 3. Calculate Monthly CO2 Impacts (Baseline - Modified). Positive = Savings.
    # We grab the hourly data which ALREADY has both baseline and modified emissions!
    df_em_summary <- df_em_list$hourly %>%
      mutate(month_start = reporting_month_start(datetime)) %>%
      group_by(month_start) %>%
      summarise(
        og_co2 = sum(co2em_inv, na.rm = TRUE),
        mod_co2 = sum(co2_em_inv_mod, na.rm = TRUE),
        co2_impact = og_co2 - mod_co2,
        .groups = "drop"
      )
    
    # 4. Build the MACC Dataframe
    df_lcac <- df_cost_summary %>%
      left_join(df_em_summary, by = "month_start") %>%
      mutate(
        co2_impact_mt = co2_impact / 1000,          # Convert kg to MTCO2e
        lcac = cost_impact / co2_impact_mt          # $/MTCO2e
      ) %>%
      filter(co2_impact_mt > 0)                     # ONLY keep months where we actually saved CO2
    
    annual_co2savings <- sum(df_em_summary$co2_impact, na.rm = TRUE) / 1000
    annual_costsavings <- sum(df_cost_summary$cost_impact, na.rm = TRUE)
    
    # 5. Fallback: If no CO2 savings occurred, show the text warning instead of crashing
    if (annual_co2savings <= 0 || nrow(df_lcac) == 0) {
      return(
        plotly_empty() %>% 
          layout(annotations = list(
            text = "The load modifications do not lead to net emission savings.\nTherefore, we cannot create an abatement plot.", 
            x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
          ))
      )
    }
    
    # 6. Build the MACC Plot
    lcac_macc <- df_lcac %>%
      ggmacc(abatement = co2_impact_mt, mac = lcac, fill = month, cost_threshold = 0, zero_line = TRUE, threshold_line = TRUE) +
      labs(x = "Abatement MTCO2e", y = "Abatement Cost $/MTCO2e", title = "Monthly Abatement Cost Plot") +
      theme_clean()
    
    lcac_macc_plotly <- ggplotly(lcac_macc) %>% 
      layout(
        showlegend = TRUE, legend = list(font = list(size = 10)),
        annotations = list(
          list(
            x = 0, y = 0.99,
            text = ifelse(annual_costsavings <= 0,
                          paste0("Annual Costs Reduction = $", format(round(abs(annual_costsavings)), big.mark = ","), "/yr"),
                          paste0("Annual Costs Increase = $", format(round(annual_costsavings), big.mark = ","), "/yr")),
            showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top"
          ),
          list(
            x = 0, y = 0.85,
            text = paste0("Annual CO<sub>2</sub>e Reduction = ", format(round(annual_co2savings), big.mark = ","), " MTCO<sub>2</sub>e/yr"),
            showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom"
          )
        )
      ) %>%
      configure_plotly_download("carbon_abatement_cost")
    
    return(lcac_macc_plotly)
  })


  # =====================================================================
  # 3. EMISSIONS PROFILE ENGINE (Replaces two giant observe blocks)
  # =====================================================================

  # A. Generate the baseline grid emissions factor profile
  em_rate_profile <- reactive({
    req(input$state, input$emissions_type)

    sel_year <- if (input$emissions_type == "U.S. EPA's 2022 eGRID") 2022 else as.integer(input$emissions_type)
    load_datetimes <- modified_load_data() %>%
      distinct(datetime) %>%
      arrange(datetime)
    req(nrow(load_datetimes) > 0)

    if (input$emissions_type == "U.S. EPA's 2022 eGRID") {
      # --- eGRID (Flat Rate Pathway) ---
      egrid_data <- read_excel("AllUploadFiles_ToolTesting/States Emission Factors/eGRID/eGRID 2022.xlsx") %>%
        clean_names() %>%
        filter(state == input$state)

      flat_rate <- as.numeric(egrid_data[[2]][1]) # Extract the factor column

      df_em <- load_datetimes %>% mutate(em_rate = flat_rate)

      # 15-min adjustment (Fixes the legacy 400% overestimation bug)
      if (input$green_manual == "Green Button: 15-Minute") {
        df_em$em_rate <- df_em$em_rate / 4
      }
    } else {
      # --- CAMBIUM (Hourly Profile Pathway) ---
      lookup_value <- paste0(input$state, "_", substr(input$emissions_type, 1, 4))
      zipfile <- "AllUploadFiles_ToolTesting/States Emission Factors/Cambium.zip"
      files <- unzip(zipfile, list = TRUE)
      matching_file <- files$Name[grepl(tolower(lookup_value), tolower(basename(files$Name)))]

      req(length(matching_file) > 0)

      temp_dir <- tempdir()
      unzip(zipfile, files = matching_file, exdir = temp_dir)

      df_em_0 <- read.csv(file.path(temp_dir, matching_file))
      df_em <- df_em_0 %>% select(datetime = timestamp_local, em_rate = aer_gen_co2e_c)
      df_em$datetime <- as.POSIXct(df_em$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")

      # 15-min interpolation using openair
      if (input$green_manual == "Green Button: 15-Minute") {
        df_em <- df_em %>% rename(date = datetime)
        df_em <- timeAverage(df_em, avg.time = "15 min", statistic = "mean", fill = TRUE) %>%
          rename(datetime = date) %>%
          mutate(em_rate = em_rate / 4)
      }

      # Cambium contains one selected scenario year. Reuse its month/day/time
      # pattern across the facility's actual reporting period so an Aug-Jul
      # upload retains Aug 2025-Jul 2026 timestamps and chronological order.
      factor_lookup <- df_em %>%
        mutate(profile_key = format(datetime, "%m-%d %H:%M")) %>%
        group_by(profile_key) %>%
        summarise(em_rate = mean(em_rate, na.rm = TRUE), .groups = "drop")

      df_em <- load_datetimes %>%
        mutate(
          profile_key = format(datetime, "%m-%d %H:%M"),
          fallback_key = if_else(
            substr(profile_key, 1, 5) == "02-29",
            paste0("02-28", substr(profile_key, 6, nchar(profile_key))),
            profile_key
          )
        ) %>%
        left_join(factor_lookup, by = "profile_key")

      missing_rows <- which(is.na(df_em$em_rate))
      if (length(missing_rows) > 0) {
        fallback_rates <- factor_lookup$em_rate[
          match(df_em$fallback_key[missing_rows], factor_lookup$profile_key)
        ]
        df_em$em_rate[missing_rows] <- fallback_rates
      }

      df_em <- df_em %>% select(datetime, em_rate)
    }
    return(df_em)
  })

  # B. Merge with Load & Calculate CO2 Inventory
  # B. Merge with Load & Calculate CO2 Inventory
  calculated_emissions <- reactive({
    req(em_rate_profile())
    
    # CRITICAL FIX: Hook into the modified data, not just the baseline
    df_load <- modified_load_data() 
    req(is.data.frame(df_load))
    
    df_em <- em_rate_profile()
    
    # Merge and Calculate
    df_merged <- df_load %>%
      left_join(df_em, by = "datetime") %>%
      arrange(datetime)
    clean_factor <- 1 - (input$perc_clean / 100)
    
    # Calculate Both Basline and Modified Hourly CO2
    df_merged$co2em_inv <- (df_merged$em_rate * df_merged$Load * clean_factor) / 1000
    df_merged$co2_em_inv_mod <- (df_merged$em_rate * df_merged$mod_load * clean_factor) / 1000
    
    # Monthly Aggregation
    annual_co2 <- df_merged %>%
      drop_na(co2em_inv) %>%
      mutate(month_start = reporting_month_start(datetime)) %>%
      group_by(month_start) %>%
      summarise(
        og_co2 = sum(co2em_inv),
        mod_co2 = sum(co2_em_inv_mod),
        .groups = "drop"
      ) %>%
      arrange(month_start)

    month_levels <- reporting_month_label(annual_co2$month_start)
    annual_co2 <- annual_co2 %>%
      mutate(month = factor(reporting_month_label(month_start), levels = month_levels))
    
    total_mt <- sum(df_merged$co2em_inv, na.rm = TRUE) / 1000
    mod_total_mt <- sum(df_merged$co2_em_inv_mod, na.rm = TRUE) / 1000
    
    # Prepare data for plotting (long format)
    monthly_long <- annual_co2 %>%
      pivot_longer(cols = c(og_co2, mod_co2), names_to = "type", values_to = "co2") %>%
      mutate(
        type = if_else(type == "og_co2", "Baseline Emissions", "Modified Emissions"),
        type = factor(type, levels = c("Baseline Emissions", "Modified Emissions"))
      )
    
    # Check if a button was clicked
    button_clicked <- (isTruthy(input$plot_button) && input$plot_button > 0) || 
      (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)
    
    # If no modification, filter out the modified data from the plot
    if (!button_clicked) {
      monthly_long <- monthly_long %>% filter(type == "Baseline Emissions")
    }
    
    list(
      hourly = df_merged,
      monthly = annual_co2,
      monthly_long = monthly_long,
      total_mt = total_mt,
      mod_total_mt = mod_total_mt,
      min_co2 = 0.8 * min(monthly_long$co2, na.rm = TRUE),
      max_co2 = 1.2 * max(monthly_long$co2, na.rm = TRUE),
      is_modified = button_clicked
    )
  })

  # =====================================================================
  # 4. EMISSIONS PLOTS & DOWNLOADS
  # =====================================================================

  output$co2_emissions_change_plot <- renderPlotly({
    req(trigger_co2_panel())
    data <- calculated_emissions()
    period_label <- reporting_period_label(data$hourly$datetime)
    
    # Initialize an empty Plotly object
    p <- plot_ly()
    
    # 1. ADD MODIFIED EMISSIONS FIRST (Draws in the background)
    if (data$is_modified) {
      p <- p %>% add_lines(data = data$hourly, x = ~datetime, y = ~co2_em_inv_mod, name = "Modified Emissions",
                           hoverinfo="text", text = ~paste0(datetime, "\nModified Emissions: ", round(co2_em_inv_mod)," kgCO2e/hr"),
                           type = "scatter", mode = "lines", line = list(color = "green", dash = "dash"))
      
      # Used <br> instead of \n so HTML rendering respects the line break inside the box
      impact_text <- paste0("<b>", period_label, " Baseline:</b> ", format(round(data$total_mt), big.mark = ","), " MTCO2e<br>",
                           "<b>Modified:</b> ", format(round(data$mod_total_mt), big.mark = ","), " MTCO2e")
    } else {
      impact_text <- paste0("<b>", period_label, " CO2e emissions:</b> ", format(round(data$total_mt), big.mark = ","), " MTCO2e")
    }
    
    # 2. ADD BASELINE EMISSIONS SECOND (Draws cleanly on top)
    p <- p %>% add_lines(data = data$hourly, x = ~datetime, y = ~co2em_inv, name = "Baseline Emissions",
                         hoverinfo="text", text = ~paste0(datetime, "\nBaseline Emissions: ", round(co2em_inv)," kgCO2e/hr"),
                         type = "scatter", mode = "lines", line = list(color = "red", opacity = 0.62))
    
    p %>% layout(
      title = paste0("<b>", input$emissions_type, " Facility CO2e Emissions</b>"),
      xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date")),
      yaxis = list(title = "<b>CO2e Emissions (kg/hr)</b>"),
      legend = list(title = "Emissions Type"), showlegend = TRUE,
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(
          text = impact_text,
          xref = "paper", yref = "paper", 
          x = 1, y = 0.98, 
          xanchor = "right", yanchor = "top", 
          showarrow = FALSE,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "lightgray",
          borderwidth = 1,
          borderpad = 4
        )
      )
    ) %>%
      configure_plotly_download("hourly_co2_emissions")
  })
  
  output$co2_plot_annual <- renderPlotly({
    req(trigger_co2_panel())
    data <- calculated_emissions()
    period_label <- reporting_period_label(data$hourly$datetime)
    
    # 1. Calculate the total annual difference
    co2_impact_value <- round(data$total_mt - data$mod_total_mt)
    
    # 2. Format the custom annotation text
    if (data$is_modified) {
      if (co2_impact_value >= 0) {
        impact_text <- paste0("<b>Avoided CO<sub>2</sub>e: </b>", format(abs(co2_impact_value), big.mark = ","), " MTCO<sub>2</sub>e/yr")
      } else {
        impact_text <- paste0("<b>Added CO<sub>2</sub>e: </b>", format(abs(co2_impact_value), big.mark = ","), " MTCO<sub>2</sub>e/yr")
      }
    } else {
      impact_text <- paste0("<b>Annual CO<sub>2</sub>e: </b>", format(round(data$total_mt), big.mark = ","), " MTCO<sub>2</sub>e/yr")
    }
    
    # 3. Build the Base ggplot
    p <- ggplot(data$monthly_long, aes(x = month, y = co2, fill = type,
                                       text = paste0(type, "\nMonth: ", month, "\nEmissions: ", scales::comma(co2), " kgCO2e/month"))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
      labs(title = paste0("Facility Monthly CO<sub>2</sub>e Emissions (", period_label, ")"), x = "Time", y = "Total CO<sub>2</sub>e Emissions (kgCO<sub>2</sub>e/month)") +
      theme_clean() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      # Expanded the upper limit slightly (1.15) to make room for the text!
      scale_y_continuous(labels = comma, limits = c(data$min_co2, data$max_co2 * 1.15), oob = rescale_none) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(name = "Emissions Type", values = c("Baseline Emissions" = "#FFA600", "Modified Emissions" = "#00313C"))
    
    # 4. Convert to Plotly and overlay the text annotation
    ggplotly(p, tooltip = "text") %>% 
      layout(
        annotations = list(
          list(
            text = impact_text,
            xref = "paper", yref = "paper", 
            x = 1, y = 0.98,               # Puts the text in the top right
            xanchor = "right", yanchor = "top", 
            showarrow = FALSE
          )
        )
      ) %>%
      configure_plotly_download("monthly_co2_emissions")
  })

  output$grid_co2_plot <- renderPlotly({
    req(trigger_co2_panel())
    df_em <- em_rate_profile()

    plot_ly() %>%
      add_lines(
        data = df_em, x = ~datetime, y = ~em_rate, name = "Grid Emissions Factor",
        hoverinfo = "text", text = ~ paste0(datetime, "\nGrid Emissions Factor: ", round(em_rate), " kgCO2e/MWh"),
        type = "scatter", mode = "lines", line = list(color = "red", opacity = 0.62)
      ) %>%
      layout(
        title = paste0("<b>", input$emissions_type, " CO2e Emissions Factor for ", input$state, "</b>"),
        xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date")),
        yaxis = list(title = "<b>State's CO2e Emission Factor (kg/MWh)</b>"),
        plot_bgcolor = "white", paper_bgcolor = "white"
      ) %>%
      configure_plotly_download("grid_co2_emissions_factor")
  })

  # --- DOWNLOAD HANDLERS ---
  output$download_co2em_data <- downloadHandler(
    filename = function() {
      "hourly_co2_data.xlsx"
    },
    content = function(file) {
      export_df <- calculated_emissions()$hourly %>%
        rename("Baseline Load(in kW)" = Load, "CO2e Emissions Factor (kg/MWh)" = em_rate, "Baseline CO2e Emissions Profile (kg/hr)" = co2em_inv)
      write.xlsx(export_df, file)
    }
  )

  output$download_co2em_data_monthly <- downloadHandler(
    filename = function() {
      "monthly_co2_data.xlsx"
    },
    content = function(file) {
      export_df <- calculated_emissions()$monthly %>% rename("Baseline CO2e Emissions Profile (kg/month)" = og_co2)
      write.xlsx(export_df, file)
    }
  )

  output$grid_ef <- downloadHandler(
    filename = function() {
      "grid_co2_data.xlsx"
    },
    content = function(file) {
      export_df <- em_rate_profile() %>% rename("CO2e Emissions Factor (kg/MWh)" = em_rate)
      write.xlsx(export_df, file)
    }
  )


  # =====================================================================
  # 5. LOAD SHIFTING & ABATEMENT ENGINE

  # =====================================================================
  # 5. LOAD SHIFTING & ABATEMENT ENGINE
  
  modified_load_data <- reactive({
    req(parsed_base_load(), input$date_range)
    
    df_merged <- parsed_base_load()
    req(is.data.frame(df_merged))
    
    df_merged$mod_load <- df_merged$Load 
    df_merged <- subset(df_merged, datetime >= input$date_range[1] & datetime < (input$date_range[2] + 1))
    
    # --- Check if the user is attempting to SHIFT load ---
    if (input$addshed_shift == "Shift Load" && isTruthy(input$plot_button) && input$plot_button > 0) {
      
      # NEW: Wrap the input reading in isolate() so it only triggers on button click
      data <- isolate({
        data.frame(
          NumericValue = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("numeric_", i)]])),
          SliderMin_from = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_from", i)]][1])),
          SliderMax_from = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_from", i)]][2])),
          SliderMin_to = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_to", i)]][1])),
          SliderMax_to = unlist(lapply(1:length(inputSets$sets), function(i) input[[paste0("slider_to", i)]][2]))
        )
      })
      
      for (i in 1:nrow(data)) {
        time_from_min <- as.ITime(format(as.POSIXct(data$SliderMin_from[i], origin = "1970-01-01"), "%H:%M"))
        time_from_max <- as.ITime(format(as.POSIXct(data$SliderMax_from[i], origin = "1970-01-01"), "%H:%M"))
        time_to_min <- as.ITime(format(as.POSIXct(data$SliderMin_to[i], origin = "1970-01-01"), "%H:%M"))
        time_to_max <- as.ITime(format(as.POSIXct(data$SliderMax_to[i], origin = "1970-01-01"), "%H:%M"))
        scl_value <- data$NumericValue[i]
        
        df_merged$day_number <- yday(df_merged$datetime)
        df_merged$is_weekend_test <- sapply(df_merged$datetime, function(d) weekdays(d) %in% c("Saturday", "Sunday"))
        
        df_merged_daysscl <- df_merged %>%
          group_by(day_number) %>%
          filter(as.ITime(datetime) >= time_from_min & as.ITime(datetime) <= time_from_max) %>%
          mutate(min_load = min(Load)) %>%
          filter(min_load >= scl_value) %>%
          ungroup()
        
        if(input$work_on_weekends == "No") df_merged_daysscl <- filter(df_merged_daysscl, !is_weekend_test)
        
        if (nrow(df_merged_daysscl) == 0 && scl_value > 0) {
          days_to_keep <- c()
        } else {
          days_to_keep <- unique(df_merged_daysscl$day_number)
        }
        
        df_merged <- df_merged %>%
          mutate(
            time_val = as.ITime(format(datetime, "%H:%M")),
            mod_load = case_when(
              day_number %in% days_to_keep & time_val >= time_from_min & time_val <= time_from_max ~ mod_load - scl_value,
              day_number %in% days_to_keep & time_val >= time_to_min   & time_val <= time_to_max   ~ mod_load + scl_value,
              TRUE ~ mod_load
            ),
            mod_load = pmax(0, mod_load) # Prevents negative loads
          ) %>% select(-time_val, -day_number, -is_weekend_test)
      }
      
      # --- Check if the user is attempting to ADD/SHED load ---
    } else if (input$addshed_shift == "Add/Shed Load" && isTruthy(input$plot_button_2) && input$plot_button_2 > 0) {
      
      # NEW: Wrap the input reading in isolate() here too
      data <- isolate({
        data.frame(
          NumericValue = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("numeric_add_", i)]])),
          SliderMin_to = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("slider_to_add", i)]][1])),
          SliderMax_to = unlist(lapply(1:length(inputSets_1$sets_1), function(i) input[[paste0("slider_to_add", i)]][2]))
        )
      })
      
      for (i in 1:nrow(data)) {
        time_to_min <- as.ITime(format(as.POSIXct(data$SliderMin_to[i], origin = "1970-01-01"), "%H:%M"))
        time_to_max <- as.ITime(format(as.POSIXct(data$SliderMax_to[i], origin = "1970-01-01"), "%H:%M"))
        scl_value <- data$NumericValue[i]
        
        df_merged$is_weekend_test <- sapply(df_merged$datetime, function(d) weekdays(d) %in% c("Saturday", "Sunday"))
        
        df_merged <- df_merged %>%
          mutate(
            time_val = as.ITime(format(datetime, "%H:%M")),
            # If "No Weekends" is selected, ignore weekends. Otherwise apply to all days.
            apply_logic = ifelse(input$work_on_weekends == "No", !is_weekend_test, TRUE),
            mod_load = case_when(
              apply_logic & time_val >= time_to_min & time_val <= time_to_max ~ mod_load + scl_value,
              TRUE ~ mod_load
            ),
            mod_load = pmax(0, mod_load) # Prevents negative loads if shedding too much
          ) %>% select(-time_val, -is_weekend_test, -apply_logic)
      }
    }
    
    return(df_merged)
  })

  # B. Compare Costs (Calculates automatically on utility select!)
  cost_comparison <- reactive({
    req(rate_vals())
    df_mod <- modified_load_data()
    
    # Always calculate Baseline
    baseline_costs <- calculate_tou_bill(df_mod, "Load", rate_vals()) %>%
      mutate(original_modified = "Baseline")
    
    # If Shift OR Add/Shed was clicked, calculate Modified and bind.
    if ((isTruthy(input$plot_button) && input$plot_button > 0) || 
        (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)) {
      modified_costs <- calculate_tou_bill(df_mod, "mod_load", rate_vals()) %>%
        mutate(original_modified = "Modified")
      combined_costs <- bind_rows(baseline_costs, modified_costs)
    } else {
      combined_costs <- baseline_costs
    }
    
    month_levels <- combined_costs %>%
      distinct(month_start) %>%
      arrange(month_start) %>%
      pull(month_start) %>%
      reporting_month_label()

    combined_costs %>%
      pivot_longer(cols = c(Usage_Cost, Demand_Cost), names_to = "energy_demand", values_to = "cost") %>%
      mutate(
        energy_demand = recode(energy_demand, Usage_Cost = "Usage Cost", Demand_Cost = "Demand Cost"),
        month = factor(reporting_month_label(month_start), levels = month_levels),
        original_modified = factor(original_modified, levels = c("Baseline", "Modified"))
      )
  })

  # C. Generate the comparison plots
  output$cost_plot <- renderPlotly({
    df_final <- cost_comparison()

    gg_cost_plot <- ggplot(df_final, aes(
      x = original_modified, y = cost, fill = energy_demand,
      text = paste0(original_modified, " ", energy_demand, " =\n", scales::dollar(cost, accuracy = 1), "/month")
    )) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
      labs(title = "Electricity Costs", x = "Scenario", y = "Total Cost ($/month)") +
      theme_clean() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_discrete(labels = c("Baseline" = "B", "Modified" = "M")) +
      scale_fill_manual(name = "Cost Type", values = c("#FFA600", "#00313C")) +
      facet_wrap(. ~ month, nrow = 1)

    ggplotly(gg_cost_plot, tooltip = "text") %>%
      configure_plotly_download("monthly_electricity_costs")
  })

  output$cost_plot_annual <- renderPlotly({
    df_annual_final <- cost_comparison() %>%
      group_by(original_modified, energy_demand) %>%
      summarise(annual_costs = sum(cost), .groups = "drop")

    annual_max <- sum(df_annual_final$annual_costs)
    period_label <- reporting_period_label(modified_load_data()$datetime)

    p <- ggplot(df_annual_final, aes(
      x = original_modified, y = annual_costs, fill = energy_demand,
      text = paste0(original_modified, " Annual ", energy_demand, " =\n", scales::dollar(annual_costs, accuracy = 1), "/yr")
    )) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
      labs(title = paste0("Annual Costs Summary (", period_label, ")"), x = "", y = "<b>Total Cost ($/yr)</b>") +
      theme_clean() +
      scale_y_continuous(labels = dollar_format(), limits = c(0, max(annual_max, 1) * 1.2)) +
      theme(
        legend.position = "bottom",
        text = element_text(family = "Open Sans", size = 14),
        axis.title = element_text(family = "Open Sans", size = 16, face = "bold"),
        legend.title = element_text(family = "Open Sans", size = 14, face = "bold"),
        legend.text = element_text(family = "Open Sans", size = 12),
        plot.title = element_text(family = "Open Sans", hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(family = "Open Sans", size = 11),
        axis.text.x = element_text(family = "Open Sans", size = 10),
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines")
      ) +
      scale_fill_manual(name = "Cost Type", labels = c("Usage Cost", "Demand Cost"), values = c("#FFA600", "#00313C"))

    ggplotly(p, tooltip = "text") %>%
      configure_plotly_download("annual_electricity_costs")
  })

  hourly_cost_data <- reactive({
    req(rate_vals())
    df_load <- modified_load_data()
    baseline <- calculate_interval_energy_costs(df_load, "Load", rate_vals())
    result <- data.frame(
      datetime = baseline$datetime,
      baseline_energy_cost = baseline$energy_cost
    )

    is_modified <- (isTruthy(input$plot_button) && input$plot_button > 0) ||
      (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)
    if (is_modified) {
      modified <- calculate_interval_energy_costs(df_load, "mod_load", rate_vals())
      result$modified_energy_cost <- modified$energy_cost
    }
    result
  })

  output$hourly_cost_plot <- renderPlotly({
    req(input$date_range)
    df_plot <- hourly_cost_data()
    start_date_graph <- as.Date(input$date_range[1])
    end_date_graph <- start_date_graph + 6
    total_baseline_cost <- sum(df_plot$baseline_energy_cost)
    p <- plot_ly()

    # Match the electrical-load chart: modified behind baseline, same colors,
    # initial week, range slider, and reporting-period annotation.
    if ("modified_energy_cost" %in% names(df_plot)) {
      p <- p %>% add_lines(
        data = df_plot, x = ~datetime, y = ~modified_energy_cost,
        name = "Modified Energy Cost",
        text = ~paste0(datetime, "\nModified Energy Cost: ",
                      scales::dollar(modified_energy_cost, accuracy = 0.01), "/interval"),
        hoverinfo = "text", line = list(color = "blue", dash = "dash")
      )
      impact_text <- paste0(
        "<b>Baseline Energy Cost:</b> ", scales::dollar(total_baseline_cost, accuracy = 0.01), "<br>",
        "<b>Modified Energy Cost:</b> ",
        scales::dollar(sum(df_plot$modified_energy_cost), accuracy = 0.01)
      )
    } else {
      impact_text <- paste0(
        "<b>Total Reporting-Period Energy Cost:</b> ",
        scales::dollar(total_baseline_cost, accuracy = 0.01)
      )
    }

    p %>% add_lines(
      data = df_plot, x = ~datetime, y = ~baseline_energy_cost,
      name = "Baseline Energy Cost",
      text = ~paste0(datetime, "\nBaseline Energy Cost: ",
                    scales::dollar(baseline_energy_cost, accuracy = 0.01), "/interval"),
      hoverinfo = "text", line = list(color = "red", opacity = 0.62)
    ) %>% layout(
      title = "<b>Hourly Energy Cost</b>",
      xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date"),
                   range = c(start_date_graph, end_date_graph)),
      yaxis = list(title = "<b>Energy Cost ($/interval)</b>", tickprefix = "$"),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(list(
        text = impact_text, xref = "paper", yref = "paper",
        x = 1, y = 0.98, xanchor = "right", yanchor = "top", showarrow = FALSE,
        bgcolor = "rgba(255, 255, 255, 0.8)", bordercolor = "lightgray",
        borderwidth = 1, borderpad = 4
      ))
    ) %>% configure_plotly_download("hourly_energy_cost")
  })

  output$download_hourly_cost_data <- downloadHandler(
    filename = function() "hourly_energy_cost_data.xlsx",
    content = function(file) {
      df_export <- hourly_cost_data()
      names(df_export) <- c(
        "Date_Time", "Baseline Energy Cost ($/interval)",
        if ("modified_energy_cost" %in% names(df_export)) "Modified Energy Cost ($/interval)"
      )
      write.xlsx(df_export, file)
    }
  )

  output$time_series_plot <- renderPlotly({
    req(parsed_base_load())
    
    start_date_graph <- as.Date(input$date_range[1])
    end_date_graph <- start_date_graph + 6
    
    # Grab the baseline load
    df_plot <- parsed_base_load()
    req(is.data.frame(df_plot), input$date_range)
    df_plot <- subset(df_plot, datetime >= input$date_range[1] & datetime < (input$date_range[2] + 1))
    
    # Calculate total throughput (kWh) from the selected reporting period.
    total_annual_kwh <- sum(df_plot$Load, na.rm = TRUE)
    
    # Initialize an empty Plotly object
    p <- plot_ly()
    
    # 1. ADD MODIFIED LOAD FIRST (Draws in the background)
    is_modified <- (isTruthy(input$plot_button) && input$plot_button > 0) || 
      (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)
    
    if (is_modified) {
      df_mod <- modified_load_data()
      if("mod_load" %in% names(df_mod)) {
        total_mod_kwh <- sum(df_mod$mod_load, na.rm = TRUE)
        
        p <- p %>% add_lines(data = df_mod, x = ~datetime, y = ~mod_load, name = "Modified Load", 
                             text = ~paste0(datetime, "\nModified Load: ", round(mod_load)," kW"), hoverinfo="text",
                             line = list(color = "blue", dash = "dash"))
        
        # Text to display when load is modified
        impact_text <- paste(
          "<b>Baseline Load:</b>", format(round(total_annual_kwh), big.mark = ","), "kWh<br>",
          "<b>Modified Load:</b>", format(round(total_mod_kwh), big.mark = ","), "kWh"
        )
      }
    } else {
      # Text to display for baseline only
      impact_text <- paste("<b>Total Reporting-Period Load:</b>", format(round(total_annual_kwh), big.mark = ","), "kWh")
    }
    
    # 2. ADD BASELINE LOAD SECOND (Draws cleanly on top)
    p <- p %>% add_lines(data = df_plot, x = ~datetime, y = ~Load, name = "Baseline Load", 
                         text = ~paste0(datetime, "\nBaseline Load: ", round(Load)," kW"), hoverinfo="text",
                         line = list(color = "red", opacity = 0.62))
    
    # 3. APPLY LAYOUT AND ANNOTATION
    p %>% layout(
      title = "<b>Hourly Electrical Load</b>",
      xaxis = list(title = "<b>Time</b>", rangeslider = list(type = "date"), range = c(start_date_graph, end_date_graph)),
      yaxis = list(title = "<b>Load (in kW)</b>"),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(
          text = impact_text,
          xref = "paper", yref = "paper",
          x = 1, y = 0.98, # Positions text in the top right corner
          xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          bgcolor = "rgba(255, 255, 255, 0.8)", # Adds a subtle background so the text is readable over lines
          bordercolor = "lightgray",
          borderwidth = 1,
          borderpad = 4
        )
      )
    ) %>%
      configure_plotly_download("hourly_electrical_load")
  })

  # =====================================================================
  # LOAD DATA EXCEL DOWNLOAD
  # =====================================================================
  output$download_load_data <- downloadHandler(
    filename = function() {
      "hourly_master_data.xlsx"
    },
    content = function(file) {
      
      # 1. Grab the live Load Data
      df_export <- modified_load_data()
      
      # Determine if the user actually clicked a modify button
      is_modified <- (isTruthy(input$plot_button) && input$plot_button > 0) || 
        (isTruthy(input$plot_button_2) && input$plot_button_2 > 0)
      
      # Rename the core load columns
      df_export <- df_export %>%
        rename(
          "Date_Time" = datetime,
          "Baseline Load (kW)" = Load,
          "Modified Load (kW)" = mod_load
        ) %>%
        # NEW: Create a duplicate index to handle Daylight Saving Time (DST) fallback
        group_by(Date_Time) %>%
        mutate(dst_idx = row_number()) %>%
        ungroup()
      
      # 2. Check if Rate data (Step 2) is available to calculate Hourly Costs
      if (isTruthy(rate_vals())) {
        rates <- rate_vals()

        baseline_interval_costs <- calculate_interval_energy_costs(
          data.frame(
            datetime = df_export$Date_Time,
            Load = df_export$`Baseline Load (kW)`
          ),
          "Load",
          rates
        )
        df_export$`Baseline Effective Electricity Cost ($/kWh)` <-
          baseline_interval_costs$effective_energy_rate
        df_export$`Baseline Energy Cost ($/interval)` <-
          baseline_interval_costs$energy_cost
        
        if (is_modified) {
          modified_interval_costs <- calculate_interval_energy_costs(
            data.frame(
              datetime = df_export$Date_Time,
              Load = df_export$`Modified Load (kW)`
            ),
            "Load",
            rates
          )
          df_export$`Modified Effective Electricity Cost ($/kWh)` <-
            modified_interval_costs$effective_energy_rate
          df_export$`Modified Energy Cost ($/interval)` <-
            modified_interval_costs$energy_cost
        }
      }
      
      # 3. Check if Emissions parameters (Step 4) are filled out
      if (isTruthy(input$state) && input$state != "") {
        
        # Safely pull the hourly data from the emissions engine
        emissions_df <- calculated_emissions()$hourly 
        
        # Extract just the emissions columns and ADD the same DST index
        emissions_cols <- emissions_df %>%
          group_by(datetime) %>%
          mutate(dst_idx = row_number()) %>%
          ungroup() %>%
          select(datetime, dst_idx, em_rate, co2em_inv, co2_em_inv_mod) %>%
          rename(
            "Date_Time" = datetime,
            "Grid CO2e Factor (kg/MWh)" = em_rate,
            "Baseline CO2e (kg/hr)" = co2em_inv,
            "Modified CO2e (kg/hr)" = co2_em_inv_mod
          )
        
        # Merge the emissions data using BOTH Time and the DST Index
        df_export <- df_export %>% left_join(emissions_cols, by = c("Date_Time", "dst_idx"))
      }
      
      # 4. Enforce the strict column order: Load -> Costs -> Emissions
      final_cols <- c("Date_Time", "Baseline Load (kW)")
      if (is_modified) final_cols <- c(final_cols, "Modified Load (kW)")
      
      if (isTruthy(rate_vals())) {
        final_cols <- c(
          final_cols,
          "Baseline Effective Electricity Cost ($/kWh)",
          "Baseline Energy Cost ($/interval)"
        )
        if (is_modified) {
          final_cols <- c(
            final_cols,
            "Modified Effective Electricity Cost ($/kWh)",
            "Modified Energy Cost ($/interval)"
          )
        }
      }
      
      if (isTruthy(input$state) && input$state != "") {
        final_cols <- c(final_cols, "Grid CO2e Factor (kg/MWh)", "Baseline CO2e (kg/hr)")
        if (is_modified) final_cols <- c(final_cols, "Modified CO2e (kg/hr)")
      }
      
      # Filter the dataframe to only include the dynamically selected columns in perfect order
      # (This naturally drops the 'dst_idx' column so the user doesn't see it)
      df_export <- df_export %>% select(all_of(final_cols))
      
      # 5. Write the final product to Excel
      write.xlsx(df_export, file) 
    }
  )

  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")

    # Reset button event handler
    updateRadioButtons(session, "green_manual", selected = "12 Months Utility Bills")
    updateSelectInput(session, "emissions_type", selected = "2022")
    updateSelectInput(session, "state", selected = "")
    updateRadioButtons(session, "addshed_shift", selected = "Shift Load")

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
