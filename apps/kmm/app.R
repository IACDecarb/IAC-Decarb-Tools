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
hc_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7", "#999999", "#1F78B4", "#33A02C")

# =====================================================================
# USER INTERFACE (UI)
# =====================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  thematic::thematic_shiny(font = "auto"),
  
  # Custom CSS for Tooltips, Unified Action Controls, and Flexible Layout Rules
  tags$head(
    tags$style(HTML("
      body { padding-bottom: 140px; }

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
                 div(style = "margin-bottom: 15px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Unit of Analysis (e.g. kW, MMBtu, tons, or gallons):", `for` = "unit_analysis", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Specify the unit of measurement (e.g., kW, MMBtu, tons, gallons) to dynamically update all chart labels, hover popups, and export files.")
                         )
                     ),
                     textInput("unit_analysis", NULL, value = "kW")
                 ),
                 
                 # Data Format Radio Input
                 div(style = "margin-bottom: 15px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Select Data Format:", `for` = "data_type", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Choose between the Custom Hourly Input Template workbook (.xlsx) or standard Green Button energy meter data (.XML).")
                         )
                     ),
                     radioButtons("data_type", NULL,
                                  choices = c(
                                    "Custom Hourly Input Template",
                                    "Green Button Data (.XML)"
                                  ),
                                  selected = "Custom Hourly Input Template"
                     )
                 ),
                 
                 # Conditional bounds logic for custom hourly generator
                 conditionalPanel(
                   condition = "input.data_type == 'Custom Hourly Input Template'",
                   hr(),
                   div(style = "margin-bottom: 15px;",
                       div(style = "display: flex; align-items: center; justify-content: space-between;",
                           tags$label("Select Template Boundaries:", `for` = "date_range", style = "font-weight: bold; margin-bottom: 5px;"),
                           div(class = "tooltip-container",
                               span(class = "info-icon", "?"),
                               div(class = "tooltip-text", "Select the start and end dates to generate a customized hourly data input template.")
                           )
                       ),
                       dateRangeInput("date_range", NULL,
                                      start = "2026-01-01", end = "2026-12-31", format = "yyyy-mm-dd", separator = " to "
                       )
                   ),
                   div(style = "display: flex; align-items: center;",
                       downloadLink("downloadTemplate", "Download Custom Hourly Input Sheet", style = "font-weight: bold; font-size: 14px;"),
                       div(class = "tooltip-container", style = "margin-left: 6px;",
                           span(class = "info-icon", "?"),
                           div(class = "tooltip-text", "Download an Excel template pre-populated with baseline hourly profile data for your selected date range.")
                       )
                   ),
                   br()
                 ),
                 
                 hr(),
                 tags$p(tags$strong("2. Upload Dataset")),
                 div(style = "margin-bottom: 15px;",
                     div(style = "display: flex; align-items: center; justify-content: space-between;",
                         tags$label("Choose File:", `for` = "file1", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(class = "tooltip-container",
                             span(class = "info-icon", "?"),
                             div(class = "tooltip-text", "Upload your modified input sheet for cluster processing.")
                         )
                     ),
                     fileInput("file1", NULL, accept = c(".csv", ".xlsx", ".xls", ".xml"))
                 ),
                 
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
                           condition = "!output.fileUploaded",
                           div(style = "padding: 20px; color: #2c3e50;",
                               h2("Hourly Profile Clustering Tool", style = "font-weight: bold;"),
                               p("Extract representative daily profiles from long-term data for your facility (For example: interval meter logs, thermal profiles, water usage, or production outputs etc.) using K-Means clustering.", style = "font-size: 16px;"),
                               br(),
                               h4("Getting Started:"),
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li("Specify your custom unit of analysis in the sidebar panel (e.g. kW, MMBtu, tons, or gallons)."),
                                       tags$li("Specify start and end date of your dataset to export an input sheet template or upload Green Button XML data as your input."),
                                       tags$li("Upload your file using the Browse button in the sidebar panel."),
                                       tags$li("The tool will auto-compute the optimal number of clusters for the uploaded profile based on the Silhouette method to establish the baseline cluster profile count."),
                                       tags$li("Modify the number of clusters if the optimum Silhouette clusters do not fully capture the variability in your uploaded profile (optional).")
                               )
                           )
                         ),
                         
                         # Force charts into an absolute vertical column layout (Profiles -> Heatmap -> Boxplots)
                         conditionalPanel(
                           condition = "output.fileUploaded",

                           column(12, tags$p(textOutput("resultsSummary", inline = TRUE))),
                           
                           # 1. TOP MODULE: Cluster Profiles Chart
                           column(12,
                                div(class = "cluster-profile-view",
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Cluster Profiles", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "Centroid plots show the diurnal variability within each cluster. The bold colored trace shows the hourly mean value for each cluster across 24 hours and how it varies whereas the grey lines represent the actual hourly data.")
                                      )
                                  ),
                                  p("Each cluster uses its own vertical scale. Compare axis values when comparing clusters.",
                                    style = "text-align:center; font-size:13px; color:#555555;"),
                                  shinycssloaders::withSpinner(plotlyOutput("centroidPlot", height = "auto"), type = 8),
                                  div(style = "text-align: right; margin-top: 15px; margin-bottom: 25px; padding-right: 10px;",
                                      downloadButton("downloadCentroid", "Download View (PNG)", class = "btn-default custom-dl-btn"),
                                      downloadButton("downloadCentroidTable", "Download Chart Data (CSV)", class = "btn-default custom-dl-btn"))
                                ),
                                  hr()
                           ),
                           
                           # 2. SECOND MODULE: Cluster Occurrences Heatmap
                           column(12,
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Cluster Occurrences Heatmap", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "Each cell on this chart represents a day assigned to a cluster based on the uploaded hourly profile. It helps you visually identify seasonal patterns, operational peaks, and baseline anomalies for each day of the year.")
                                      )
                                  ),
                                  shinycssloaders::withSpinner(plotlyOutput("heatmapPlot", height = "500px"), type = 8),
                                  div(style = "text-align: right; margin-top: 10px; margin-bottom: 25px; padding-right: 10px;",
                                      downloadButton("downloadHeatmap", "Download Heatmap View (PNG)", class = "btn-default custom-dl-btn")),
                                  hr()
                           ),
                           
                           # 3. THIRD MODULE: Box and Whisker Plots (With Sub-Tabs)
                           column(12,
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Box and Whisker Plots", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "Aggregated boxplots give you statistical insights about each cluster. Hovering over each cluster box shows you its minimum, median, and maximum daily average values. It helps you contrast the different profiles within your dataset and identify the clusters with high and low variability. You can switch to the Hourly Analysis tab to see hourly variability independent of clusters.")
                                      )
                                  ),
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
                                  div(style = "text-align: right; margin-top: 15px; margin-bottom: 25px; padding-right: 10px;",
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
    style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: center; padding: 15px 0; border-top: 1px solid #ddd; z-index: 1000; box-shadow: 0 -2px 5px rgba(0,0,0,0.1);",
    tags$div(
      style = "text-align: left; margin-right: 150px;",
      tags$img(src = "lbnl.png", style = "max-height: 50px;"),
      tags$p(tags$b("Prakash Rao"), style = "margin-top: 5px; margin-bottom: 0px;"),
      tags$p("prao@lbl.gov", style = "margin-top: 0px;")
    ),
    tags$div(
      style = "text-align: left;",
      tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
      tags$p(tags$b("Kelly Kissock"), style = "margin-top: 5px; margin-bottom: 0px;"),
      tags$p("jkissock@ucdavis.edu", style = "margin-top: 0px;")
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
  
  # Reactive Visibility Track
  output$fileUploaded <- reactive({ !is.null(input$file1) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Reactive Storage for Precomputed Silhouette Scores Array (k = 2..10)
  silhouette_scores_val <- reactiveVal(NULL)
  
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
      st <- as.POSIXct(paste(input$date_range[1], "00:00:00"))
      en <- as.POSIXct(paste(input$date_range[2], "23:00:00"))
      
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
  
  # --- Multi-Format Processing Engine ---
  processed_data <- reactive({
    req(input$file1)
    path <- input$file1$datapath
    
    if (input$data_type == "Custom Hourly Input Template") {
      df <- read_excel(path)
      names(df)[1:2] <- c("Datetime", "Load")
      df$Datetime <- as.POSIXct(df$Datetime, orders = "ymd_HMS")
    } else {
      loadpf <- read_xml(path)
      ns <- c(espi = "http://naesb.org/espi")
      readings <- xml_find_all(loadpf, ".//espi:IntervalReading", ns)
      
      starts <- as.integer(xml_text(xml_find_all(readings, ".//espi:timePeriod/espi:start", ns)))
      values <- as.integer(xml_text(xml_find_all(readings, ".//espi:value", ns)))
      
      df <- data.frame(
        Datetime = as.POSIXct(starts, origin = "1970-01-01"),
        Load = values / 1000 
      ) %>% distinct(Datetime, .keep_all = TRUE)
    }
    
    df <- df %>%
      mutate(
        Date = as.Date(Datetime),
        Hour = hour(Datetime),
        DayOfWeek = wday(Date, label = TRUE, abbr = TRUE),
        WeekStart = floor_date(Date, unit = "week")
      ) %>%
      drop_na(Date, Load)
    
    return(df)
  })
  
  # --- Auto-Detect Optimal K Using Highest Average Silhouette Score ---
  observeEvent(processed_data(), {
    df <- processed_data()
    
    if (is.data.frame(df) && nrow(df) > 100) {
      
      tryCatch({
        df_prep <- df %>%
          mutate(Date = as.Date(Datetime), Hour = hour(Datetime)) %>%
          drop_na(Date, Load) %>%
          group_by(Date, Hour) %>%
          summarise(Load = mean(Load, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = Hour, values_from = Load, values_fill = 0)
        
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
      numericInput("k_clusters", "Number of Clusters (k):", value = 4, min = 1, max = 10, step = 1),
      
      # Dynamic Subtitle displaying Average Silhouette Score directly beneath k_clusters
      uiOutput("silhouette_subtitle_ui"),
      
      hr(),
      downloadButton("downloadTable", "Download Metrics Log (CSV)", class = "btn-success", style = "width:100%;")
    )
  })
  
  # --- Dynamic Subtitle Output for Average Silhouette Score ---
  output$silhouette_subtitle_ui <- renderUI({
    req(input$k_clusters)
    k_curr <- as.numeric(input$k_clusters)
    scores <- silhouette_scores_val()
    
    if (!is.null(scores) && !is.na(k_curr) && k_curr >= 2 && k_curr <= length(scores)) {
      score_val <- scores[k_curr]
      if (!is.na(score_val)) {
        score_formatted <- sprintf("%.4f", score_val)
        
        opt_k <- which.max(scores)
        is_opt <- (k_curr == opt_k)
        badge_text <- if (is_opt) " (Optimum k)" else ""
        
        tags$p(
          style = "margin-top: -5px; margin-bottom: 12px; font-size: 13px; color: #555;",
          tags$span(style = "font-weight: 600; color: #2c3e50;", "Average Silhouette Score: "),
          tags$span(style = "font-weight: 700; color: #16a085;", paste0(score_formatted, badge_text))
        )
      } else {
        NULL
      }
    } else if (!is.null(clustered_data())) {
      tryCatch({
        df_matrix <- clustered_data()$matrix %>% select(-Date)
        dist_mat <- dist(df_matrix)
        set.seed(42)
        km <- kmeans(df_matrix, centers = k_curr, nstart = 25)
        ss <- cluster::silhouette(km$cluster, dist_mat)
        score_val <- mean(ss[, 3])
        score_formatted <- sprintf("%.4f", score_val)
        
        tags$p(
          style = "margin-top: -5px; margin-bottom: 12px; font-size: 13px; color: #555;",
          tags$span(style = "font-weight: 600; color: #2c3e50;", "Average Silhouette Score: "),
          tags$span(style = "font-weight: 700; color: #16a085;", score_formatted)
        )
      }, error = function(e) { NULL })
    } else {
      NULL
    }
  })
  
  # --- Core K-Means Execution Array ---
  clustered_data <- reactive({
    df <- processed_data()
    req(nrow(df) > 0, input$k_clusters)
    
    daily_matrix <- df %>%
      group_by(Date) %>%
      filter(n() == 24) %>% 
      ungroup() %>%
      group_by(Date, Hour) %>%
      summarise(Load = mean(Load, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Hour, values_from = Load, values_fill = 0)
    
    clustering_data <- daily_matrix %>% select(-Date)
    
    set.seed(42) 
    k <- input$k_clusters
    kmeans_result <- kmeans(clustering_data, centers = k, nstart = 25)
    
    raw_counts <- table(kmeans_result$cluster)
    rank_order <- order(raw_counts, decreasing = TRUE)
    new_labels <- match(kmeans_result$cluster, rank_order)
    
    daily_matrix$Cluster <- factor(new_labels, levels = 1:k)
    
    df_clustered <- df %>%
      left_join(daily_matrix %>% select(Date, Cluster), by = "Date") %>%
      drop_na(Cluster)
    
    return(list(full_data = df_clustered, matrix = daily_matrix))
  })
  
  # =====================================================================
  # INTERACTIVE PLOT GENERATORS
  # =====================================================================
  
  # 1. Cluster Profiles Chart (Centroids) 
  output$centroidPlot <- renderPlotly({
    req(clustered_data())
    u <- unit_lbl()
    df <- clustered_data()$full_data
    
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
    
    p <- ggplot() + 
      geom_line(data = df, aes(x = Hour, y = Load, group = Date), color = "dimgray", alpha = 0.25) +
      geom_line(data = centroids, aes(x = Hour, y = Centroid_Load, color = Cluster, group = Cluster,
                                      text = paste0("Hour: ", Hour, ":00\nMean Hourly Value: ", round(Centroid_Load, 1), " ", u)), linewidth = 0.8) +
      scale_color_manual(values = hc_colors) +
      scale_x_continuous(breaks = c(seq(0, 20, by = 5), 23), minor_breaks = 0:23, limits = c(0, 23)) +
      # Train each panel on all its daily readings, including the gray lines.
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
    
    k <- length(levels(centroids$Cluster_Label))
    calc_height <- k * 300 + 80
    
    # Keep each profile tall enough to read at the capped display width.
    ggplotly(p, tooltip = "text", height = calc_height) %>% 
      layout(hovermode = "x") %>% 
      config(displayModeBar = FALSE)
  })
  
  # 2. Interactive Heatmap
  output$heatmapPlot <- renderPlotly({
    req(clustered_data())
    u <- unit_lbl()
    df <- clustered_data()$full_data %>% select(Date, DayOfWeek, WeekStart, Cluster) %>% distinct()
    month_breaks <- seq(min(df$WeekStart, na.rm = TRUE), max(df$WeekStart, na.rm = TRUE), by = "1 month")
    
    # Ensure Cluster Median matches the Boxplot Cluster Median exactly
    medians <- df_metrics_calculated()
    df <- df %>% left_join(medians, by = "Cluster")
    
    p <- ggplot(df, aes(x = DayOfWeek, y = as.numeric(WeekStart), fill = Cluster,
                        text = paste0(format(Date, "%Y-%m-%d"), "_", DayOfWeek, "\n",
                                      "Cluster Assigned: ", Cluster, "\n",
                                      "Cluster Median: ", round(Median_Load, 1), " ", u))) +
      geom_tile(color = "white") +
      scale_y_reverse(breaks = as.numeric(month_breaks), labels = format(month_breaks, "%b %d")) +
      scale_fill_manual(values = hc_colors) +
      theme_minimal(base_size = 15) + 
      labs(x = "Day of Week", y = "Week Starting Date") +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black")   
      )
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
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
      req(input$k_clusters)
      u <- unit_lbl()
      df_c <- clustered_data()$full_data
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
      
      k <- as.numeric(input$k_clusters)
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
