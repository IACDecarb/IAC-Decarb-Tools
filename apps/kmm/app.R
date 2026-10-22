# =====================================================================
# AUTOMATED PACKAGE MANAGEMENT HEADER
# =====================================================================
required_packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "tidyverse", 
  "lubridate", "readxl", "tools", "openxlsx", "xml2", 
  "patchwork", "cluster", "plotly", "zoo"
)

# Install missing packages automatically
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Explicitly load all libraries into the R session
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(lubridate)
library(readxl)
library(tools)
library(openxlsx)
library(xml2)
library(patchwork)
library(cluster)
library(plotly)
library(zoo)

source("profile_summary.R", local = TRUE)

# Define a high-contrast, colorblind-friendly palette
hc_colors <- c("#E69F00", "#56B4E9", "#009E73", "#B8860B", "#0072B2", 
               "#D55E00", "#CC79A7", "#999999", "#1F78B4", "#33A02C")

# =====================================================================
# DATA HELPERS
# =====================================================================

# Reads .xlsx / .xls / .csv in the hourly template layout (col 1 = datetime, col 2 = value)
read_tabular <- function(path, ext) {
  df <- if (ext == "csv") readr::read_csv(path, show_col_types = FALSE) else readxl::read_excel(path)
  if (ncol(df) < 2) stop("The file needs at least two columns: a date/time column and a value column.")
  
  df <- df[, 1:2]
  names(df) <- c("Datetime", "Load")
  
  if (!inherits(df$Datetime, "POSIXct")) {
    df$Datetime <- suppressWarnings(lubridate::parse_date_time(
      as.character(df$Datetime),
      orders = c("ymd HMS", "ymd HM", "ymd H", "mdy HMS", "mdy HM", "mdy H", "dmy HMS", "dmy HM")
    ))
  }
  df$Load <- suppressWarnings(as.numeric(df$Load))
  
  if (all(is.na(df$Datetime))) stop("The date/time column couldn't be read. Please use a format like 2026-01-01 13:00:00.")
  if (all(is.na(df$Load)))     stop("The second column doesn't contain numeric values.")
  df
}

# Reads Green Button (ESPI) XML usage files
read_green_button <- function(path) {
  doc <- tryCatch(xml2::read_xml(path),
                  error = function(e) stop("This XML file couldn't be read. Please check that it is a valid Green Button export."))
  ns <- c(espi = "http://naesb.org/espi")
  readings <- xml_find_all(doc, ".//espi:IntervalReading", ns)
  if (length(readings) == 0) stop("No interval readings were found. Please check that this is a Green Button usage file.")
  
  # xml_find_first per reading keeps starts and values aligned even if a reading is malformed
  starts <- as.numeric(xml_text(xml_find_first(readings, ".//espi:timePeriod/espi:start", ns)))
  values <- as.numeric(xml_text(xml_find_first(readings, ".//espi:value", ns)))
  
  data.frame(
    Datetime = as.POSIXct(starts, origin = "1970-01-01"),
    Load = values / 1000
  ) %>% distinct(Datetime, .keep_all = TRUE)
}

# One row per COMPLETE day (all 24 hours present after hourly aggregation), one column per hour
build_daily_matrix <- function(df) {
  df %>%
    group_by(Date, Hour) %>%
    summarise(Load = mean(Load, na.rm = TRUE), .groups = "drop") %>%
    group_by(Date) %>%
    filter(n() == 24) %>%
    ungroup() %>%
    arrange(Date, Hour) %>%
    pivot_wider(names_from = Hour, values_from = Load)
}

# =====================================================================
# USER INTERFACE (UI)
# =====================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  thematic::thematic_shiny(font = "auto"),
  
  # Custom CSS for Tooltips, Unified Action Controls, and Flexible Layout Rules
  tags$head(
    tags$style(HTML("

      .cluster-profile-view {
        width: 100%;
        max-width: 1100px;
        margin: 0 auto;
      }
      
      /* Force absolute horizontal & vertical centering for all download actions */
      .custom-dl-btn {
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        vertical-align: middle !important;
        height: 34px !important;
        padding: 6px 14px !important;
        font-weight: 500 !important;
        font-size: 13px !important;
        line-height: normal !important;
        box-sizing: border-box !important;
        margin-left: 8px;
      }
      
      /* Pure CSS Hover Tooltip Engine for Headers & Inputs */
      .tooltip-container {
        position: relative;
        display: inline-block;
        cursor: pointer;
        margin-left: 6px;
      }
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
        font-weight: bold;
        font-style: normal;
        font-family: 'Arial', sans-serif;
      }
      .tooltip-text {
        visibility: hidden;
        width: 280px;
        background-color: #2c3e50;
        color: #fff;
        text-align: left;
        padding: 12px;
        border-radius: 6px;
        position: absolute;
        z-index: 999;
        bottom: 125%;
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
        font-size: 13px;
        font-weight: normal;
        line-height: 1.4;
        box-shadow: 0px 4px 10px rgba(0,0,0,0.2);
      }
      .tooltip-container:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
      /* ---- Chart layout: 55% profiles | 45% heatmap ---- */
      .chart-row {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        margin-bottom: 20px;
      }
      .profile-col { flex: 0 0 calc(55% - 10px); min-width: 0; }
      .heatmap-col { flex: 0 0 calc(45% - 10px); min-width: 0; }

      /* ---- Card styling for each chart ---- */
      .plot-card {
        background: #ffffff;
        border: 1px solid #e3e6ea;
        border-radius: 8px;
        padding: 14px 16px 10px 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      .plot-card-header {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 4px;
      }
      .plot-card-header h3 {
        margin: 0;
        font-size: 20px;
        font-weight: bold;
        color: #000;
      }
      .plot-card-subtitle {
        text-align: center;
        font-size: 13px;
        color: #555;
        margin: 4px 0 6px 0;
        min-height: 18px;
      }
      .plot-card-actions {
        text-align: right;
        margin-top: 6px;
      }

      /* Heatmap stays in view while scrolling through tall profile stacks */
      .sticky-card { position: sticky; top: 15px; }

      /* Stack vertically on narrow screens */
      @media (max-width: 991px) {
        .profile-col, .heatmap-col { flex: 0 0 100%; }
        .sticky-card { position: static; }
      }
      /* Compact download buttons */
      .custom-dl-btn {
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        gap: 5px;
        vertical-align: middle !important;
        height: 26px !important;
        padding: 2px 8px !important;
        font-weight: 500 !important;
        font-size: 13px !important;
        line-height: 1 !important;
        box-sizing: border-box !important;
        margin-left: 6px;
        border-radius: 3px !important;
      }
      /* Tighter sidebar spacing */
      #side-panel hr {
        margin-top: 10px !important;
        margin-bottom: 10px !important;
      }
      #side-panel .form-group,
      #side-panel .shiny-input-radiogroup {
        margin-bottom: 4px !important;
      }
      #side-panel .shiny-input-container:not(.shiny-input-container-inline) {
        margin-bottom: 4px !important;
      }
      #side-panel .radio {
        margin-top: 2px !important;
        margin-bottom: 2px !important;
      }
      #side-panel .progress {
        margin-bottom: 4px !important;
      }
      /* Excluded-days notice */
      .excluded-note {
        font-size: 13px;
        color: #8a6d3b;
        background: #fcf8e3;
        border: 1px solid #faebcc;
        border-radius: 4px;
        padding: 6px 10px;
        margin-bottom: 12px;
      }

      /* Click-to-highlight banner and hint */
      .selection-banner {
        display: table;
        margin: 0 auto 6px auto;
        font-size: 13px;
        background: #f4f6f8;
        border: 1px solid #e3e6ea;
        border-radius: 14px;
        padding: 3px 12px;
      }
      .selection-hint {
        text-align: center;
        font-size: 12px;
        color: #888;
        margin: 0 0 6px 0;
      }
      #heatmapPlot .nsewdrag { cursor: pointer !important; }
    "))
  ),
  
  titlePanel(
    HTML("K-Means Clustering Tool"),
    windowTitle = "K-Means Clustering Tool"
  ),
  
  sidebarLayout(
    # --- Consolidated Left-Hand Side Sidebar ---
    sidebarPanel(width = 3, id = "side-panel",
                 tags$p(tags$strong("1. Unit & Format Configuration")),
                 
                 # Dynamic Unit Input
                 div(style = "margin-bottom: 6px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Unit of Analysis (e.g. kW, MMBtu, tons, or gallons):", `for` = "unit_analysis", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Specify the unit of measurement (e.g., kW, MMBtu, tons, gallons) to dynamically update all chart labels, hover popups, and export files.")
                         )
                     ),
                     textInput("unit_analysis", NULL, value = "kW")
                 ),
                 
                 # Template generator (not needed for Green Button files)
                 div(style = "margin-bottom: 6px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Input Template Date Range:", `for` = "date_range", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Select the start and end dates to generate an hourly input template. Skip this step if you are uploading a Green Button (.xml) file.")
                         )
                     ),
                     dateRangeInput("date_range", NULL,
                                    start = "2026-01-01", end = "2026-12-31", format = "yyyy-mm-dd", separator = " to ")
                 ),
                 div(style = "display: flex; align-items: center;",
                     downloadLink("downloadTemplate", "Download Custom Hourly Input Sheet", style = "font-weight: bold; font-size: 14px;"),
                     div(class = "tooltip-container", style = "margin-left: 6px;",
                         span(class = "info-icon", "?"),
                         div(class = "tooltip-text", "Download an Excel template pre-populated with baseline hourly profile data for your selected date range.")
                     )
                 ),
                 
                 hr(),
                 tags$p(tags$strong("2. Upload Dataset")),
                 div(style = "margin-bottom: 6px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Choose File:", `for` = "file1", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Upload your completed hourly template (.xlsx, .xls, or .csv) or a Green Button file (.xml). The file type is detected automatically.")
                         )
                     ),
                     fileInput("file1", NULL, accept = c(".csv", ".xlsx", ".xls", ".xml"))
                 ),
                 tags$p("Accepted: .xlsx, .xls, .csv (template) or .xml (Green Button).",
                        style = "font-size: 12px; color: #777; margin-top: -2px;"),
                 
                 # Parameter tuning and metrics exports appear dynamically upon verification
                 uiOutput("conditionalSidebarUI"),
                 hr(),
                 downloadLink("downloadUserGuide", "Download User Guide (PDF)",
                              style = "font-weight: bold; font-size: 14px; color: #337ab7; text-decoration: underline;")
    ),
    
    # --- Absolute Vertically Stacked Main View ---
    mainPanel(width = 9,
              tabsetPanel(
                id = "main_tabs",
                tabPanel("Dashboard",
                         br(),
                         # Conditional Instructions Card
                         conditionalPanel(
                           condition = "output.fileUploaded",
                           
                           tags$p(textOutput("resultsSummary", inline = TRUE),
                                  style = "margin-bottom: 8px;"),
                           uiOutput("excludedDaysNote"),
                           
                           # ---- Row 1: Profiles (55%) | Heatmap (45%) ----
                           div(class = "chart-row",
                               
                               # LEFT: Cluster Profiles
                               div(class = "profile-col",
                                   div(class = "plot-card",
                                       div(class = "plot-card-header",
                                           h3("Cluster Profiles"),
                                           div(class = "tooltip-container",
                                               span(class = "info-icon", "?"),
                                               div(class = "tooltip-text", "Centroid plots show the diurnal variability within each cluster. The bold colored trace shows the hourly mean value for each cluster across 24 hours and how it varies whereas the grey lines represent the actual hourly data.")
                                           )
                                       ),
                                       p(class = "plot-card-subtitle",
                                         "Each cluster uses its own vertical scale. Compare axis values when comparing clusters."),
                                       uiOutput("selectedDayUI"),
                                       shinycssloaders::withSpinner(plotlyOutput("centroidPlot", height = "auto"), type = 8),
                                       div(class = "plot-card-actions",
                                           downloadButton("downloadCentroid", "Download View (PNG)", class = "btn-default custom-dl-btn"),
                                           downloadButton("downloadCentroidTable", "Download Chart Data (CSV)", class = "btn-default custom-dl-btn"))
                                   )
                               ),
                               
                               # RIGHT: Heatmap
                               div(class = "heatmap-col",
                                   div(class = "plot-card sticky-card",
                                       div(class = "plot-card-header",
                                           h3("Cluster Occurrences Heatmap"),
                                           div(class = "tooltip-container",
                                               span(class = "info-icon", "?"),
                                               div(class = "tooltip-text", "Each cell on this chart represents a day assigned to a cluster based on the uploaded hourly profile. It helps you visually identify seasonal patterns, operational peaks, and baseline anomalies for each day of the year.")
                                           )
                                       ),
                                       p(class = "plot-card-subtitle",
                                         "Each cell is one day. Click a day to highlight its profile."),
                                       shinycssloaders::withSpinner(plotlyOutput("heatmapPlot", height = "620px"), type = 8),
                                       div(class = "plot-card-actions",
                                           downloadButton("downloadHeatmap", "Download Heatmap View (PNG)", class = "btn-default custom-dl-btn"))
                                   )
                               )
                           ),
                           
                           # ---- Row 2: Box and Whisker Plots (full width) ----
                           div(class = "plot-card",
                               div(class = "plot-card-header",
                                   h3("Box and Whisker Plots"),
                                   div(class = "tooltip-container",
                                       span(class = "info-icon", "?"),
                                       div(class = "tooltip-text", "Aggregated boxplots give you statistical insights about each cluster. Hovering over each cluster box shows you its minimum, median, and maximum daily average values. It helps you contrast the different profiles within your dataset and identify the clusters with high and low variability. You can switch to the Hourly Analysis tab to see hourly variability independent of clusters.")
                                   )
                               ),
                               br(),
                               tabsetPanel(
                                 id = "box_plot_tabs",
                                 tabPanel("Cluster Analysis",
                                          br(),
                                          shinycssloaders::withSpinner(plotlyOutput("boxplotPlot", height = "480px"), type = 8)
                                 ),
                                 tabPanel("Hourly Analysis",
                                          br(),
                                          shinycssloaders::withSpinner(plotlyOutput("hourlyBoxplot", height = "480px"), type = 8)
                                 )
                               ),
                               div(class = "plot-card-actions",
                                   downloadButton("downloadBoxplot", "Download Box Plot View (PNG)", class = "btn-default custom-dl-btn"),
                                   downloadButton("downloadBoxplotTable", "Download Chart Data (CSV)", class = "btn-default custom-dl-btn"))
                           )
                         )
                ),
                tabPanel("Cluster Details Data", 
                         conditionalPanel(
                           condition = "output.fileUploaded",
                           br(),
                           h4("Extracted Engineering Parameters Table", style = "font-weight:bold; color:black;"),
                           tableOutput("clusterSummaryTable")
                         )
                )
              )
    )
  ),
  
  # --- Universal Tool Suite Sticky Footer ---
  tags$div(
    style = "margin: 40px -15px 0 -15px; background-color: #f8f8f8; display: flex; justify-content: center; align-items: center; padding: 15px 0; border-top: 1px solid #ddd;",
    tags$div(
      style = "text-align: left; margin-right: 150px;",
      tags$img(src = "lbnl.png", style = "max-height: 50px;"),
      tags$p(tags$b("Prakash Rao"), style = "margin-top: 5px; margin-bottom: 0px;"),
      tags$p("prao@lbl.gov", style = "margin-top: 0px; margin-bottom: 0px;")
    ),
    tags$div(
      style = "text-align: left;",
      tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
      tags$p(tags$b("Kelly Kissock"), style = "margin-top: 5px; margin-bottom: 0px;"),
      tags$p("jkissock@ucdavis.edu", style = "margin-top: 0px; margin-bottom: 0px;")
    )
  )
)

# =====================================================================
# SERVER LOGIC
# =====================================================================
server <- function(input, output, session) {

  output$downloadUserGuide <- downloadHandler(
    filename = function() "User Guide for KMM Tool.pdf",
    contentType = "application/pdf",
    content = function(file) {
      guide <- "User Guide for KMM Tool.pdf"
      validate(need(file.exists(guide), "The user guide is currently unavailable."))
      if (!file.copy(guide, file, overwrite = TRUE)) stop("Could not copy the user guide.")
    }
  )

  output$resultsSummary <- renderText({
    kmm_results_summary(clustered_data()$full_data, unit_lbl())
  })
  
  output$excludedDaysNote <- renderUI({
    ex <- clustered_data()$excluded_dates
    if (length(ex) == 0) return(NULL)
    
    shown <- format(head(ex, 5), "%b %d, %Y")
    more  <- if (length(ex) > 5) paste0(", and ", length(ex) - 5, " more") else ""
    
    div(class = "excluded-note",
        tags$b(paste0(length(ex), if (length(ex) == 1) " day was" else " days were", " excluded")),
        paste0(" from clustering because they didn't have data for all 24 hours: ",
               paste(shown, collapse = ", "), more, "."))
  })
  
  # --- Click-to-Highlight State ---
  selected_day <- reactiveVal(NULL)
  
  observeEvent(input$file1, selected_day(NULL))
  observeEvent(input$clear_day, selected_day(NULL))
  
  # Clicked date arrives from the heatmap's JavaScript handler (see output$heatmapPlot)
  observeEvent(input$heatmap_day, {
    clicked <- suppressWarnings(as.Date(input$heatmap_day))
    req(length(clicked) == 1, !is.na(clicked))
    # Clicking the same day again clears the highlight
    if (!is.null(selected_day()) && selected_day() == clicked) selected_day(NULL) else selected_day(clicked)
  })
  
  output$selectedDayUI <- renderUI({
    d <- selected_day()
    if (is.null(d)) {
      return(p(class = "selection-hint", "Tip: click any day on the heatmap to highlight it here."))
    }
    cl <- clustered_data()$matrix %>% filter(Date == d) %>% pull(Cluster)
    div(class = "selection-banner",
        "Highlighted: ", tags$b(format(d, "%a, %b %d, %Y")),
        if (length(cl) == 1) paste0(" (Cluster ", cl, ")"),
        actionLink("clear_day", "Clear", style = "margin-left: 10px;"))
  })
  
  # Reactive Visibility Track
  output$fileUploaded <- reactive({ !is.null(input$file1) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Reactive Storage for Precomputed Silhouette Scores Array (k = 2..10)
  silhouette_scores_val <- reactiveVal(NULL)
  
  # Wait 600 ms after the user stops typing before re-clustering
  k_val <- debounce(reactive(suppressWarnings(as.numeric(input$k_clusters))), 600)
  
  k_is_valid <- function(k) length(k) == 1 && !is.na(k) && k == round(k) && k >= 2 && k <= 10
  
  # Reactive Unit Label Helper
  unit_lbl <- reactive({
    if (is.null(input$unit_analysis) || trimws(input$unit_analysis) == "") {
      "kW"
    } else {
      trimws(input$unit_analysis)
    }
  })
  
  # --- Dynamic Input Sheet Generator using ELPT Reference Excel File ---
  output$downloadTemplate <- downloadHandler(
    filename = function() { "Custom_Hourly_Input_Sheet.xlsx" },
    content = function(file) {
      req(input$date_range)
      st <- as.POSIXct(paste(input$date_range[1], "00:00:00"), tz = "UTC")
      en <- as.POSIXct(paste(input$date_range[2], "23:00:00"), tz = "UTC")
      
      if (st > en) {
        tmp <- st
        st <- en
        en <- tmp
      }
      
      datetime_seq <- seq(st, en, by = "hour")
      user_df <- data.frame(
        Date_Time = datetime_seq,
        MonthDayHour = format(datetime_seq, "%m-%d %H")
      )
      
      # Look for reference file in folder first, then fallback to root
      ref_path <- "AllUploadFiles_ToolTesting/Custom_Hourly_LoadProfileTemplate_PGE.xlsx"
      if (!file.exists(ref_path)) {
        ref_path <- "Custom_Hourly_LoadProfileTemplate_PGE.xlsx"
      }
      
      if (file.exists(ref_path)) {
        df_ref <- read_excel(ref_path)
        
        names(df_ref)[1:2] <- c("ref_datetime", "ref_load")
        df_ref$ref_datetime <- as.POSIXct(df_ref$ref_datetime)
        df_ref$MonthDayHour <- format(df_ref$ref_datetime, "%m-%d %H")
        
        df_ref_clean <- df_ref %>% 
          distinct(MonthDayHour, .keep_all = TRUE) %>%
          select(MonthDayHour, ref_load)
        
        merged_df <- user_df %>%
          left_join(df_ref_clean, by = "MonthDayHour") %>%
          mutate(ref_load = zoo::na.approx(ref_load, na.rm = FALSE)) %>%
          tidyr::fill(ref_load, .direction = "downup")
        
        load_values <- round(merged_df$ref_load, 2)
      } else {
        set.seed(42)
        load_values <- round(runif(length(datetime_seq), min = 200, max = 400), 2)
      }
      
      col_header <- paste0("Input ", unit_lbl())
      
      template_df <- data.frame(
        Date_Time = format(datetime_seq, "%Y-%m-%d %H:%M:%S"),
        Value = load_values,
        check.names = FALSE
      )
      names(template_df)[2] <- col_header
      
      wb <- createWorkbook()
      addWorksheet(wb, "Data Input")
      writeData(wb, "Data Input", template_df)
      
      dt_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss", fgFill = "#f2f2f2", locked = TRUE)
      val_style <- createStyle(fgFill = "#ffffcc", locked = FALSE)
      
      addStyle(wb, "Data Input", style = dt_style, rows = 1:(nrow(template_df)+1), cols = 1, gridExpand = TRUE)
      addStyle(wb, "Data Input", style = val_style, rows = 1:(nrow(template_df)+1), cols = 2, gridExpand = TRUE)
      setColWidths(wb, "Data Input", cols = 1:2, widths = c(22, 22))
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- Multi-Format Processing Engine (file type auto-detected from extension) ---
  processed_data <- reactive({
    req(input$file1)
    path <- input$file1$datapath
    ext  <- tolower(tools::file_ext(input$file1$name))
    
    validate(need(ext %in% c("xlsx", "xls", "csv", "xml"),
                  "Unsupported file type. Please upload an .xlsx, .xls, .csv, or .xml file."))
    
    df <- tryCatch(
      if (ext == "xml") read_green_button(path) else read_tabular(path, ext),
      error = function(e) e
    )
    
    if (inherits(df, "error")) {
      showNotification(conditionMessage(df), type = "error", duration = 10)
      validate(need(FALSE, conditionMessage(df)))
    }
    
    df <- df %>%
      mutate(
        Date = as.Date(Datetime),
        Hour = hour(Datetime),
        DayOfWeek = wday(Date, label = TRUE, abbr = TRUE),
        WeekStart = floor_date(Date, unit = "week")
      ) %>%
      drop_na(Date, Load)
    
    validate(need(nrow(df) > 0, "No valid rows were found in the uploaded file."))
    df
  })
  
  # --- Auto-Detect Optimal K Using Highest Average Silhouette Score ---
  observeEvent(processed_data(), {
    df <- processed_data()
    
    if (is.data.frame(df) && nrow(df) > 100) {
      
      tryCatch({
        df_prep <- build_daily_matrix(df)
        
        clustering_data <- df_prep %>% select(-Date)
        
        # Ensure we have enough valid days to cluster
        if(nrow(clustering_data) > 15) {
          
          dist_matrix <- dist(clustering_data)
          sil_scores <- numeric(10)
          
          # Test k values from 2 to 10
          for (k in 2:10) {
            set.seed(42)
            km_res <- kmeans(clustering_data, centers = k, nstart = 25)
            ss <- cluster::silhouette(km_res$cluster, dist_matrix)
            sil_scores[k] <- mean(ss[, 3])
          }
          
          # Store pre-calculated scores array
          silhouette_scores_val(sil_scores)
          
          # Standard Methodology: Find the k with the highest average silhouette score
          optimal_k <- which.max(sil_scores)
          
          # Prevent edge cases where optimal k might calculate incorrectly
          if (length(optimal_k) == 0 || is.na(optimal_k) || optimal_k < 2) {
            optimal_k <- 4 
          }
          
          # Dynamically update the UI to reflect the optimal setting
          updateNumericInput(session, "k_clusters", value = optimal_k)
          
          # Notify the user that optimization occurred
          showNotification(
            paste("Cluster Analysis: Auto-detected", optimal_k, "optimal load profiles based on highest average silhouette score."), 
            type = "message", 
            duration = 7
          )
        }
      }, error = function(e) {
      })
    }
  })
  
  # --- Sidebar UI Controls (With Tooltips and Dynamic Silhouette Score Subtitle) ---
  output$conditionalSidebarUI <- renderUI({
    req(input$file1)
    tagList(
      hr(),
      div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
          tags$strong("Modify Clusters (Optional)", style = "font-size: 14px;"),
          div(class = "tooltip-container",
              span(class = "info-icon", "?"),
              div(class = "tooltip-text", "The tool analyzes the uploaded hourly profile and assigns it optimum number of clusters based on average silhouette score for k between 2 and 10. This option allows you to change the number of clusters to capture variations beyond the optimum k.")
          )
      ),
      numericInput("k_clusters", "Number of Clusters (k):", value = 4, min = 2, max = 10, step = 1),
      
      # Dynamic Subtitle displaying Average Silhouette Score directly beneath k_clusters
      uiOutput("silhouette_subtitle_ui"),
      
      hr(),
      downloadButton("downloadTable", "Download Metrics Log (CSV)", class = "btn-success", style = "width:100%;")
    )
  })
  
  # --- Dynamic Subtitle Output for Average Silhouette Score ---
  output$silhouette_subtitle_ui <- renderUI({
    k <- k_val()
    req(length(k) == 1)
    
    if (!k_is_valid(k)) {
      return(tags$p(style = "margin-top: -5px; margin-bottom: 12px; font-size: 13px; color: #c0392b; font-weight: 600;",
                    "Please enter a whole number between 2 and 10."))
    }
    
    scores <- silhouette_scores_val()
    if (is.null(scores) || is.na(scores[k])) return(NULL)
    
    badge_text <- if (k == which.max(scores)) " (Optimum k)" else ""
    tags$p(
      style = "margin-top: -5px; margin-bottom: 12px; font-size: 13px; color: #555;",
      tags$span(style = "font-weight: 600; color: #2c3e50;", "Average Silhouette Score: "),
      tags$span(style = "font-weight: 700; color: #16a085;", paste0(sprintf("%.4f", scores[k]), badge_text))
    )
  })
  
  # --- Core K-Means Execution ---
  clustered_data <- reactive({
    df <- processed_data()
    k  <- k_val()
    req(nrow(df) > 0, length(k) == 1)
    validate(need(k_is_valid(k), "Number of clusters (k) must be a whole number between 2 and 10."))
    k <- as.integer(k)
    
    daily_matrix <- build_daily_matrix(df)
    validate(need(nrow(daily_matrix) > k,
                  paste0("Only ", nrow(daily_matrix), " complete days were found. At least ", k + 1,
                         " complete days are needed for ", k, " clusters.")))
    
    clustering_data <- daily_matrix %>% select(-Date)
    
    set.seed(42)
    kmeans_result <- kmeans(clustering_data, centers = k, nstart = 25)
    
    raw_counts <- table(kmeans_result$cluster)
    rank_order <- order(raw_counts, decreasing = TRUE)
    new_labels <- match(kmeans_result$cluster, rank_order)
    daily_matrix$Cluster <- factor(new_labels, levels = 1:k)
    
    df_clustered <- df %>%
      left_join(daily_matrix %>% select(Date, Cluster), by = "Date") %>%
      drop_na(Cluster)
    
    all_dates <- sort(unique(df$Date))
    excluded_dates <- all_dates[!all_dates %in% daily_matrix$Date]
    
    list(full_data = df_clustered, matrix = daily_matrix, excluded_dates = excluded_dates)
  })
  # =====================================================================
  # INTERACTIVE PLOT GENERATORS
  # =====================================================================
  
  output$centroidPlot <- renderPlotly({
    req(clustered_data())
    u   <- unit_lbl()
    df  <- clustered_data()$full_data
    sel <- selected_day()
    
    cluster_counts <- df %>%
      select(Date, Cluster) %>%
      distinct() %>% 
      dplyr::count(Cluster, name = "Days") %>%
      arrange(as.numeric(as.character(Cluster))) %>%
      mutate(Cluster_Label = factor(paste0("Cluster ", Cluster, " (", Days, " days)"), 
                                    levels = paste0("Cluster ", Cluster, " (", Days, " days)")))
    
    df <- df %>% left_join(cluster_counts, by = "Cluster")
    centroids <- df %>%
      group_by(Cluster_Label, Hour) %>%
      summarise(Centroid_Load = mean(Load, na.rm = TRUE), Cluster = dplyr::first(Cluster), .groups = 'drop')
    
    hl <- if (!is.null(sel)) filter(df, Date == sel) else df[0, ]
    
    p <- ggplot() + 
      geom_line(data = df, aes(x = Hour, y = Load, group = Date), color = "dimgray", alpha = 0.25) +
      geom_line(data = centroids, aes(x = Hour, y = Centroid_Load, color = Cluster, group = Cluster,
                                      text = paste0("Hour: ", Hour, ":00\nMean Hourly Value: ", round(Centroid_Load, 1), " ", u)),
                linewidth = 0.8) +
      { if (nrow(hl) > 0) geom_line(data = hl, aes(x = Hour, y = Load, group = Date,
                                                   text = paste0("Selected day: ", format(Date, "%Y-%m-%d"),
                                                                 "\nHour: ", Hour, ":00\nValue: ", round(Load, 1), " ", u)),
                                    color = "black", linewidth = 1, linetype = "dashed") } +
      scale_color_manual(values = hc_colors) +
      scale_x_continuous(breaks = c(seq(0, 20, by = 5), 23), minor_breaks = 0:23, limits = c(0, 23)) +
      scale_y_continuous(expand = expansion(mult = 0.05)) +
      facet_wrap(~Cluster_Label, ncol = 1, scales = "free_y") +
      theme_minimal(base_size = 15) + 
      labs(x = "Hour of Day", y = paste0("Hourly Value (", u, ")")) +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black"),
        strip.text = element_text(size = 16, face = "bold", color = "black"),
        panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.4),
        panel.grid.major.x = element_line(color = "grey70", linewidth = 0.6)
      )
    
    k <- nlevels(centroids$Cluster_Label)
    ggplotly(p, tooltip = "text", height = k * 300 + 80) %>% 
      layout(hovermode = "x") %>% 
      config(displayModeBar = FALSE)
  })
  
  # 2. Interactive Heatmap
  output$heatmapPlot <- renderPlotly({
    req(clustered_data())
    u   <- unit_lbl()
    sel <- selected_day()
    
    df <- clustered_data()$full_data %>% select(Date, DayOfWeek, WeekStart, Cluster) %>% distinct()
    month_starts <- seq(floor_date(min(df$Date), "month"), max(df$Date), by = "1 month")
    month_breaks <- floor_date(month_starts, "week")
    
    df <- df %>%
      left_join(df_metrics_calculated(), by = "Cluster") %>%
      mutate(
        y     = as.numeric(WeekStart),
        key   = as.character(Date),
        hover = paste0(format(Date, "%Y-%m-%d"), " (", DayOfWeek, ")\n",
                       "Cluster Assigned: ", Cluster, "\n",
                       "Cluster Median: ", round(Median_Load, 1), " ", u, "\n",
                       "Click to highlight")
      )
    sel_df <- if (!is.null(sel)) filter(df, Date == sel) else df[0, ]
    
    p <- ggplot(df, aes(x = DayOfWeek, y = y)) +
      geom_tile(aes(fill = Cluster), color = "white") +
      geom_point(aes(text = hover, key = key), alpha = 0, size = 3) +   # invisible hover/click targets
      { if (nrow(sel_df) > 0) geom_tile(data = sel_df, fill = NA, color = "black", linewidth = 1.2) } +
      scale_y_reverse(breaks = as.numeric(month_breaks), labels = format(month_starts, "%b"),
                      expand = expansion(add = 3)) +
      scale_fill_manual(values = hc_colors) +
      theme_minimal(base_size = 15) + 
      labs(x = "Day of Week", y = "Week Starting Date") +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black")   
      )
    
    gp <- plotly_build(ggplotly(p, tooltip = "text"))
    
    # Only the invisible points respond to hover/click; they carry the date in customdata
    gp$x$data <- lapply(gp$x$data, function(tr) {
      if (identical(tr$mode, "markers") && !is.null(tr$key)) {
        tr$customdata <- tr$key
      } else {
        tr$hoverinfo <- "skip"
      }
      tr
    })
    
    gp %>%
      config(displayModeBar = FALSE) %>%
      htmlwidgets::onRender("
        function(el) {
          // Remove any handler from a previous render so clicks don't fire twice
          if (el._dayHandler && el.removeListener) {
            el.removeListener('plotly_click', el._dayHandler);
          }
          el._dayHandler = function(d) {
            var pt = d.points && d.points[0];
            if (!pt || pt.customdata === undefined || pt.customdata === null) return;
            var day = Array.isArray(pt.customdata) ? pt.customdata[0] : pt.customdata;
            Shiny.setInputValue('heatmap_day', day, {priority: 'event'});
          };
          el.on('plotly_click', el._dayHandler);
        }
      ")
  })
  
  # 3A. Box and Whisker Plot (Cluster Analysis)
  output$boxplotPlot <- renderPlotly({
    req(clustered_data())
    u <- unit_lbl()
    df <- clustered_data()$full_data
    daily_summaries <- df %>%
      group_by(Date, Cluster) %>%
      summarise(Daily_Mean_Load = mean(Load, na.rm = TRUE), .groups = 'drop')
    
    bounds_summary <- daily_summaries %>%
      group_by(Cluster) %>%
      summarise(
        Min = min(Daily_Mean_Load, na.rm = TRUE),
        Max = max(Daily_Mean_Load, na.rm = TRUE),
        Med = median(Daily_Mean_Load, na.rm = TRUE),
        Q1  = quantile(Daily_Mean_Load, 0.25, na.rm = TRUE),
        Q3  = quantile(Daily_Mean_Load, 0.75, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(bounds_summary, aes(x = Cluster)) +
      geom_linerange(aes(ymin = Min, ymax = Max, color = Cluster,
                         text = paste0("Cluster: ", Cluster, "\n",
                                       "Min: ", round(Min, 1), " ", u, "\n",
                                       "Median: ", round(Med, 1), " ", u, "\n",
                                       "Max: ", round(Max, 1), " ", u)), 
                     linewidth = 0.5) +
      geom_segment(aes(x = as.numeric(Cluster) - 0.15, xend = as.numeric(Cluster) + 0.15, y = Max, yend = Max, color = Cluster,
                       text = paste0("Cluster: ", Cluster, "\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   linewidth = 0.5) +
      geom_segment(aes(x = as.numeric(Cluster) - 0.15, xend = as.numeric(Cluster) + 0.15, y = Min, yend = Min, color = Cluster,
                       text = paste0("Cluster: ", Cluster, "\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   linewidth = 0.5) +
      geom_rect(aes(xmin = as.numeric(Cluster) - 0.3, xmax = as.numeric(Cluster) + 0.3, 
                    ymin = Q1, ymax = Q3, fill = Cluster,
                    text = paste0("Cluster: ", Cluster, "\n",
                                  "Min: ", round(Min, 1), " ", u, "\n",
                                  "Median: ", round(Med, 1), " ", u, "\n",
                                  "Max: ", round(Max, 1), " ", u)), 
                color = "black", alpha = 0.8) +
      geom_segment(aes(x = as.numeric(Cluster) - 0.3, xend = as.numeric(Cluster) + 0.3, y = Med, yend = Med, fill = Cluster,
                       text = paste0("Cluster: ", Cluster, "\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   color = "black", linewidth = 0.3) +
      scale_fill_manual(values = hc_colors) +
      scale_color_manual(values = hc_colors) +
      theme_minimal(base_size = 15) + 
      labs(x = "Cluster Group", y = paste0("Mean Daily Value (", u, ")")) +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black")
      )
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # 3B. Box and Whisker Plot (Hourly Analysis) - Limited to Min, Median, Max
  output$hourlyBoxplot <- renderPlotly({
    req(clustered_data())
    u <- unit_lbl()
    df <- clustered_data()$full_data
    
    hourly_summaries <- df %>%
      group_by(Hour) %>%
      summarise(
        Min = min(Load, na.rm = TRUE),
        Max = max(Load, na.rm = TRUE),
        Med = median(Load, na.rm = TRUE),
        Q1  = quantile(Load, 0.25, na.rm = TRUE),
        Q3  = quantile(Load, 0.75, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(Hour_Factor = factor(Hour, levels = 0:23))
    
    p <- ggplot(hourly_summaries, aes(x = Hour_Factor)) +
      geom_linerange(aes(ymin = Min, ymax = Max,
                         text = paste0("Hour: ", Hour, ":00\n",
                                       "Min: ", round(Min, 1), " ", u, "\n",
                                       "Median: ", round(Med, 1), " ", u, "\n",
                                       "Max: ", round(Max, 1), " ", u)),
                     color = "#2c3e50", linewidth = 0.5) +
      geom_segment(aes(x = as.numeric(Hour_Factor) - 0.25, xend = as.numeric(Hour_Factor) + 0.25, y = Max, yend = Max,
                       text = paste0("Hour: ", Hour, ":00\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   color = "#2c3e50", linewidth = 0.5) +
      geom_segment(aes(x = as.numeric(Hour_Factor) - 0.25, xend = as.numeric(Hour_Factor) + 0.25, y = Min, yend = Min,
                       text = paste0("Hour: ", Hour, ":00\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   color = "#2c3e50", linewidth = 0.5) +
      geom_rect(aes(xmin = as.numeric(Hour_Factor) - 0.35, xmax = as.numeric(Hour_Factor) + 0.35, 
                    ymin = Q1, ymax = Q3,
                    text = paste0("Hour: ", Hour, ":00\n",
                                  "Min: ", round(Min, 1), " ", u, "\n",
                                  "Median: ", round(Med, 1), " ", u, "\n",
                                  "Max: ", round(Max, 1), " ", u)), 
                fill = "#3498db", color = "#2c3e50", alpha = 0.7) +
      geom_segment(aes(x = as.numeric(Hour_Factor) - 0.35, xend = as.numeric(Hour_Factor) + 0.35, y = Med, yend = Med,
                       text = paste0("Hour: ", Hour, ":00\n",
                                     "Min: ", round(Min, 1), " ", u, "\n",
                                     "Median: ", round(Med, 1), " ", u, "\n",
                                     "Max: ", round(Max, 1), " ", u)), 
                   color = "black", linewidth = 0.3) +
      theme_minimal(base_size = 15) + 
      labs(x = "Hour of Day", y = paste0("Value (", u, ")")) +
      theme(
        axis.text = element_text(size = 13, color = "black"),   
        axis.title = element_text(size = 15, color = "black")
      )
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # --- Engineering Metrics Helper Reactive ---
  df_metrics_calculated <- reactive({
    req(clustered_data())
    clustered_data()$full_data %>%
      group_by(Date, Cluster) %>%
      summarise(Daily_Mean_Load = mean(Load, na.rm = TRUE), .groups = 'drop') %>%
      group_by(Cluster) %>%
      summarise(Median_Load = median(Daily_Mean_Load, na.rm = TRUE), .groups = "drop")
  })
  
  cluster_table_data <- reactive({
    req(clustered_data())
    u <- unit_lbl()
    df <- clustered_data()$full_data
    
    stats_df <- df %>%
      group_by(Cluster) %>%
      summarise(`Number of Days` = as.integer(n_distinct(Date)),
                `Mean` = round(mean(Load, na.rm = TRUE), 2),
                `Median` = round(median(Load, na.rm = TRUE), 2),
                `Min` = round(min(Load, na.rm = TRUE), 2),
                `Max` = round(max(Load, na.rm = TRUE), 2),
                `Standard Deviation` = round(sd(Load, na.rm = TRUE), 2),
                .groups = "drop")
    
    names(stats_df)[3:7] <- c(
      paste0("Mean (", u, ")"),
      paste0("Median (", u, ")"),
      paste0("Min (", u, ")"),
      paste0("Max (", u, ")"),
      paste0("Standard Deviation (", u, ")")
    )
    
    peak_hour_df <- df %>%
      group_by(Cluster, Hour) %>%
      summarise(MeanLoad = mean(Load, na.rm = TRUE), .groups = "drop") %>%
      group_by(Cluster) %>%
      slice_max(MeanLoad, n = 1, with_ties = FALSE) %>%
      select(Cluster, `Centroid Peak Hour` = Hour)
    
    left_join(stats_df, peak_hour_df, by = "Cluster") %>% arrange(as.numeric(as.character(Cluster)))
  })
  
  output$clusterSummaryTable <- renderTable({ cluster_table_data() }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # =====================================================================
  # DECENTRALIZED VIEW EXPORTS (PNG & CSV TABLES)
  # =====================================================================
  
  # 1A. Cluster Profiles Image PNG 
  output$downloadCentroid <- downloadHandler(
    filename = function() { paste("cluster_profiles_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      u <- unit_lbl()
      df_c <- clustered_data()$full_data
      hl <- if (!is.null(selected_day())) filter(df_c, Date == selected_day()) else df_c[0, ]
      counts <- df_c %>% 
        select(Date, Cluster) %>% 
        distinct() %>% 
        dplyr::count(Cluster, name = "Days") %>% 
        arrange(as.numeric(as.character(Cluster))) %>%
        mutate(CL = factor(paste0("Cluster ", Cluster, " (", Days, " days)"), 
                           levels = paste0("Cluster ", Cluster, " (", Days, " days)")))
      
      df_c <- df_c %>% left_join(counts, by = "Cluster")
      cents <- df_c %>% 
        group_by(CL, Hour) %>% 
        summarise(CLoad = mean(Load, na.rm = TRUE), Cluster = dplyr::first(Cluster), .groups = 'drop')
      
      p <- ggplot() + 
        geom_line(data = df_c, aes(x = Hour, y = Load, group = Date), color = "dimgray", alpha = 0.25) +
        geom_line(data = cents, aes(x = Hour, y = CLoad, color = Cluster), linewidth = 1.0) + 
        { if (nrow(hl) > 0) geom_line(data = hl, aes(x = Hour, y = Load, group = Date),
                                      color = "black", linewidth = 1, linetype = "dashed") } +
        scale_color_manual(values = hc_colors) +
        scale_x_continuous(breaks = c(seq(0, 20, by = 5), 23), minor_breaks = 0:23, limits = c(0, 23)) +
        scale_y_continuous(expand = expansion(mult = 0.05)) +
        facet_wrap(~CL, ncol = 1, scales = "free_y") +
        theme_minimal(base_size = 16) + 
        labs(x = "Hour of Day", y = paste0("Hourly Value (", u, ")"), title = "Cluster Profiles",
             subtitle = "Each cluster uses its own vertical scale. Compare axis values when comparing clusters.") +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#555555"),
          axis.title = element_text(size = 16, face = "bold", color = "black"),
          axis.text = element_text(size = 14, color = "black"),
          strip.text = element_text(size = 16, face = "bold", color = "black"),
          panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.4),
          panel.grid.major.x = element_line(color = "grey70", linewidth = 0.6)
        )
      
      k <- nlevels(counts$CL)
      calc_height <- 3 * k + 1.5
      
      ggsave(filename = file, plot = p, device = "png", width = 10, height = calc_height, dpi = 300, bg = "white")
    }
  )
  
  # 1B. Cluster Profiles Data CSV Table
  output$downloadCentroidTable <- downloadHandler(
    filename = function() { paste("cluster_profiles_centroids_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      df_c <- clustered_data()$full_data
      u <- unit_lbl()
      cents <- df_c %>% 
        group_by(Cluster, Hour) %>% 
        summarise(Centroid_Value = round(mean(Load, na.rm = TRUE), 2), .groups = 'drop') %>%
        mutate(Cluster_Name = paste0("Cluster ", Cluster, " (", u, ")")) %>%
        select(-Cluster) %>%
        pivot_wider(names_from = Cluster_Name, values_from = Centroid_Value) %>%
        arrange(Hour)
      
      write.csv(cents, file, row.names = FALSE)
    }
  )
  
  # 2. Cluster Heatmap Image PNG
  output$downloadHeatmap <- downloadHandler(
    filename = function() { paste("heatmap_view_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      df_c <- clustered_data()$full_data
      df_h <- df_c %>% select(Date, DayOfWeek, WeekStart, Cluster) %>% distinct()
      mb <- seq(min(df_h$WeekStart, na.rm = TRUE), max(df_h$WeekStart, na.rm = TRUE), by = "1 month")
      
      p <- ggplot(df_h, aes(x = DayOfWeek, y = as.numeric(WeekStart), fill = Cluster)) +
        geom_tile(color = "white") + scale_y_reverse(breaks = as.numeric(mb), labels = format(mb, "%b %d")) +
        scale_fill_manual(values = hc_colors) + theme_minimal(base_size = 14) + 
        labs(x = "Day of Week", y = "Week Starting Date", title = "Cluster Occurrences Heatmap") +
        theme(legend.position = "none", axis.text = element_text(color="black"), plot.title = element_text(face="bold", hjust=0.5))
      
      ggsave(filename = file, plot = p, device = "png", width = 11, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # 3A. Box Plot Image PNG (Exact Visual Match to Online Preview)
  output$downloadBoxplot <- downloadHandler(
    filename = function() { paste("boxplot_view_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      u <- unit_lbl()
      df_c <- clustered_data()$full_data
      daily_summaries <- df_c %>%
        group_by(Date, Cluster) %>%
        summarise(Daily_Mean_Load = mean(Load, na.rm = TRUE), .groups = 'drop')
      
      bounds_summary <- daily_summaries %>%
        group_by(Cluster) %>%
        summarise(
          Min = min(Daily_Mean_Load, na.rm = TRUE),
          Max = max(Daily_Mean_Load, na.rm = TRUE),
          Med = median(Daily_Mean_Load, na.rm = TRUE),
          Q1  = quantile(Daily_Mean_Load, 0.25, na.rm = TRUE),
          Q3  = quantile(Daily_Mean_Load, 0.75, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(bounds_summary, aes(x = Cluster)) +
        geom_linerange(aes(ymin = Min, ymax = Max, color = Cluster), linewidth = 0.5) +
        geom_segment(aes(x = as.numeric(Cluster) - 0.15, xend = as.numeric(Cluster) + 0.15, y = Max, yend = Max, color = Cluster), linewidth = 0.5) +
        geom_segment(aes(x = as.numeric(Cluster) - 0.15, xend = as.numeric(Cluster) + 0.15, y = Min, yend = Min, color = Cluster), linewidth = 0.5) +
        geom_rect(aes(xmin = as.numeric(Cluster) - 0.3, xmax = as.numeric(Cluster) + 0.3, ymin = Q1, ymax = Q3, fill = Cluster), color = "black", alpha = 0.8) +
        geom_segment(aes(x = as.numeric(Cluster) - 0.3, xend = as.numeric(Cluster) + 0.3, y = Med, yend = Med), color = "black", linewidth = 0.3) +
        scale_fill_manual(values = hc_colors) +
        scale_color_manual(values = hc_colors) +
        theme_minimal(base_size = 14) + 
        labs(x = "Cluster Group", y = paste0("Mean Daily Value (", u, ")"), title = "Box and Whisker Plots") +
        theme(
          legend.position = "none",
          axis.text = element_text(color = "black"),
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      
      ggsave(filename = file, plot = p, device = "png", width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # 3B. Box Plot Summary Data CSV Table
  output$downloadBoxplotTable <- downloadHandler(
    filename = function() { paste("cluster_details_summary_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(cluster_table_data(), file, row.names = FALSE)
    }
  )
  
  # Sidebar Metrics CSV Download Handler
  output$downloadTable <- downloadHandler(
    filename = function() { paste("cluster_metrics_log_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cluster_table_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)
