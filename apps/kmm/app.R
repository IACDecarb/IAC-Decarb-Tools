# =====================================================================
# AUTOMATED PACKAGE MANAGEMENT HEADER
# =====================================================================
required_packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "tidyverse", 
  "lubridate", "readxl", "tools", "openxlsx", "xml2", 
  "patchwork", "cluster", "plotly"
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

# Define a high-contrast, colorblind-friendly palette
hc_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7", "#999999", "#1F78B4", "#33A02C")

# =====================================================================
# USER INTERFACE (UI)
# =====================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  thematic::thematic_shiny(font = "auto"),
  
  # Custom CSS for Tooltips, Unified Button Geometry, and Layout Rules
  tags$head(
    tags$style(HTML("
      body { padding-bottom: 140px; }
      
      /* Force absolute horizontal & vertical centering for all download actions */
      .custom-dl-btn {
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        vertical-align: middle !important;
        height: 32px !important;
        font-weight: 500;
      }
      
      /* Pure CSS Hover Tooltip Engine for Headers */
      .tooltip-container {
        position: relative;
        display: inline-block;
        cursor: pointer;
        margin-left: 8px;
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
    HTML("K-Means Clustering tool"),
    windowTitle = "K-Means Clustering Tool"
  ),
  
  sidebarLayout(
    # --- Consolidated Left-Hand Side Sidebar ---
    sidebarPanel(width = 3, id = "side-panel",
                 tags$p(tags$strong("1. Select Data Format")),
                 radioButtons("data_type", NULL,
                              choices = c(
                                "Custom Hourly Load Template",
                                "Green Button Data (.XML)"
                              ),
                              selected = "Custom Hourly Load Template"
                 ),
                 
                 # Conditional bounds logic for custom hourly generator
                 conditionalPanel(
                   condition = "input.data_type == 'Custom Hourly Load Template'",
                   hr(),
                   dateRangeInput("date_range", "Specify Start and End Dates:",
                                  start = "2026-01-01", end = "2026-12-31", format = "yyyy-mm-dd", separator = " to "
                   ),
                   downloadLink("downloadTemplate", "Download Custom Hourly Input Sheet", style = "font-weight: bold; font-size: 14px;"),
                   br()
                 ),
                 
                 hr(),
                 tags$p(tags$strong("2. Upload Dataset")),
                 fileInput("file1", "Choose File:", accept = c(".csv", ".xlsx", ".xls", ".xml")),
                 
                 # Parameter tuning and metrics exports appear dynamically upon verification
                 uiOutput("conditionalSidebarUI")
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
                               h2("Load Profile Clustering Tool", style = "font-weight: bold;"),
                               p("This tool allows you to extract daily load profiles based on the facility's energy consumption data", style = "font-size: 16px;"),
                               br(),
                               h4("Getting Started:"),
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li("Specify start and end date of your dataset to export an input sheet or download the Green Button XML data for the facility"),
                                       tags$li("Upload your file by clicking the Browse button in the sidebar panel."),
                                       tags$li("The tool will auto-compute the optimal number of clusters (k) based on the Silhouette method to establish the baseline cluster profile count.")
                               )
                           )
                         ),
                         
                         # Force charts into an absolute vertical column layout with loading spinners and decentralized download bars
                         conditionalPanel(
                           condition = "output.fileUploaded",
                           column(12,
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Cluster Occurrences Heatmap", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "Each cell on this chart represents a day assigned to a cluster based on load values. It helps you visually identify seasonal patterns, operational peaks, and baseline anomalies.")
                                      )
                                  ),
                                  shinycssloaders::withSpinner(plotlyOutput("heatmapPlot", height = "500px"), type = 8),
                                  div(style = "text-align: right; margin-top: 10px; margin-bottom: 25px; padding-right: 10px;",
                                      downloadButton("downloadHeatmap", "Download Heatmap View (PNG)", class = "btn-default custom-dl-btn")),
                                  hr()
                           ),
                           column(12,
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Daily Mean Load Distribution", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "These boxplots give you statistical insights about each cluster. Hovering over each box shows you its minimum, median, and maximum load values. It helps you contrast the different profiles within your dataset and identify the clusters with high and low variability. This information is also available in the cluster details table.")
                                      )
                                  ),
                                  shinycssloaders::withSpinner(plotlyOutput("boxplotPlot", height = "500px"), type = 8),
                                  div(style = "text-align: right; margin-top: 10px; margin-bottom: 25px; padding-right: 10px;",
                                      downloadButton("downloadBoxplot", "Download Box Plot View (PNG)", class = "btn-default custom-dl-btn")),
                                  hr()
                           ),
                           column(12,
                                  div(style = "width:100%; text-align:center;",
                                      div(class = "tooltip-container",
                                          h3("Cluster Load Profiles (Centroids)", style = "display:inline-block; font-weight:bold; color:black;"),
                                          span(class = "info-icon", "?"),
                                          div(class = "tooltip-text", "Centroid plots add the hourly resolution to the days within each cluster. The bold colored trace shows the mean load value for each cluster across 24 hours and how it varies and the grey lines represent the actual hourly day data. This chart offers insights about the diurnal variability within each cluster.")
                                      )
                                  ),
                                  shinycssloaders::withSpinner(plotlyOutput("centroidPlot", height = "auto"), type = 8),
                                  div(style = "text-align: right; margin-top: 15px; margin-bottom: 25px; padding-right: 10px;",
                                      downloadButton("downloadCentroid", "Download Centroid Profiles View (PNG)", class = "btn-default custom-dl-btn"))
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
  
  # Reactive visibility track for welcome dashboard swapping
  output$fileUploaded <- reactive({ !is.null(input$file1) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # --- Replicated PG&E B-20 Industrial Profile Archetype Template ---
  output$downloadTemplate <- downloadHandler(
    filename = function() { "Custom_Hourly_Load_Input_Sheet.xlsx" },
    content = function(file) {
      req(input$date_range)
      st <- as.POSIXct(paste(input$date_range[1], "00:00:00"))
      en <- as.POSIXct(paste(input$date_range[2], "23:00:00"))
      
      datetime_seq <- seq(st, en, by = "hour")
      hours_vector <- hour(datetime_seq)
      months_vector <- month(datetime_seq)
      day_types <- wday(datetime_seq)
      
      base_process <- 500 
      is_production_hour <- hours_vector >= 6 & hours_vector <= 22
      production_load <- ifelse(is_production_hour, 420, 0)
      production_load <- production_load + ifelse(hours_vector >= 11 & hours_vector <= 16, 60, 0)
      
      is_summer <- months_vector %in% 6:9
      summer_cooling <- ifelse(is_summer, 160 * sin((hours_vector - 6) * pi / 12), 0)
      summer_cooling[summer_cooling < 0] <- 0
      
      is_weekend <- day_types %in% c(1, 7)
      
      load_calculations <- ifelse(is_weekend,
                                  base_process + 40 + (summer_cooling * 0.35), 
                                  base_process + production_load + summer_cooling 
      )
      
      set.seed(42)
      load_calculations <- round(load_calculations + rnorm(length(datetime_seq), mean = 0, sd = 12), 1)
      load_calculations[load_calculations < 20] <- 20 
      
      template_df <- data.frame(
        Date_Time = format(datetime_seq, "%Y-%m-%d %H:%M:%S"),
        `Load (in kW)` = load_calculations,
        check.names = FALSE
      )
      
      wb <- createWorkbook()
      addWorksheet(wb, "Load Input")
      writeData(wb, "Load Input", template_df)
      
      dt_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss", fgFill = "#f2f2f2", locked = TRUE)
      val_style <- createStyle(fgFill = "#ffffcc", locked = FALSE)
      
      addStyle(wb, "Load Input", style = dt_style, rows = 1:(nrow(template_df)+1), cols = 1, gridExpand = TRUE)
      addStyle(wb, "Load Input", style = val_style, rows = 1:(nrow(template_df)+1), cols = 2, gridExpand = TRUE)
      setColWidths(wb, "Load Input", cols = 1:2, widths = c(22, 18))
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- Multi-Format Processing Engine ---
  processed_data <- reactive({
    req(input$file1)
    path <- input$file1$datapath
    
    if (input$data_type == "Custom Hourly Load Template") {
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
  
  # --- Background Silhouette Optimization Loop ---
  observeEvent(processed_data(), {
    df <- processed_data()
    if (is.data.frame(df) && nrow(df) > 100) {
      tryCatch({
        df_matrix <- df %>%
          group_by(Date) %>%
          filter(n() == 24) %>% 
          ungroup() %>%
          group_by(Date, Hour) %>%
          summarise(Load = mean(Load, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = Hour, values_from = Load, values_fill = 0)
        
        matrix_core <- df_matrix %>% select(-Date)
        
        if (nrow(matrix_core) > 15) {
          dist_matrix <- dist(matrix_core)
          scores <- numeric(10)
          
          for (k in 2:10) {
            set.seed(42)
            km <- kmeans(matrix_core, centers = k, nstart = 25)
            ss <- cluster::silhouette(km$cluster, dist_matrix)
            scores[k] = mean(ss[, 3])
          }
          
          optimal_k <- which.max(scores)
          if (length(optimal_k) == 0 || is.na(optimal_k) || optimal_k < 2) optimal_k <- 4
          
          updateNumericInput(session, "k_clusters", value = optimal_k)
          showNotification(paste("Optimization Engine: Initialized default configuration to", optimal_k, "distinct cluster profiles based on background validation."), type = "message", duration = 8)
        }
      }, error = function(e) {})
    }
  })
  
  # --- Conditional UI Sidebar Controls Engine ---
  output$conditionalSidebarUI <- renderUI({
    req(input$file1)
    tagList(
      hr(),
      tags$p(tags$strong("3. Cluster Controls")),
      numericInput("k_clusters", "Number of Clusters (k):", value = 4, min = 1, max = 10, step = 1),
      hr(),
      downloadButton("downloadTable", "Download Metrics Log (CSV)", class = "btn-success", style = "width:100%;")
    )
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
    
    daily_matrix$Cluster <- as.factor(kmeans_result$cluster)
    
    df_clustered <- df %>%
      left_join(daily_matrix %>% select(Date, Cluster), by = "Date") %>%
      drop_na(Cluster)
    
    return(list(full_data = df_clustered, matrix = daily_matrix))
  })
  
  # 1. Interactive Heatmap
  output$heatmapPlot <- renderPlotly({
    req(clustered_data())
    df <- clustered_data()$full_data %>% select(Date, DayOfWeek, WeekStart, Cluster) %>% distinct()
    month_breaks <- seq(min(df$WeekStart, na.rm = TRUE), max(df$WeekStart, na.rm = TRUE), by = "1 month")
    
    medians <- df_metrics_calculated()
    df <- df %>% left_join(medians, by = "Cluster")
    
    p <- ggplot(df, aes(x = DayOfWeek, y = as.numeric(WeekStart), fill = Cluster,
                        text = paste0(format(Date, "%Y-%m-%d"), "_", DayOfWeek, "\n",
                                      "Cluster Assigned: ", Cluster, "\n",
                                      "Cluster Median: ", round(Median_Load, 1), " kW"))) +
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
  
  # 2. Interactive Box Plot
  output$boxplotPlot <- renderPlotly({
    req(clustered_data())
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
    
    p <- ggplot(bounds_summary, aes(x = Cluster, text = paste0("Cluster: ", Cluster, "\n",
                                                               "Min: ", round(Min, 1), " kW\n",
                                                               "Median: ", round(Med, 1), " kW\n",
                                                               "Max: ", round(Max, 1), " kW"))) +
      geom_linerange(aes(ymin = Min, ymax = Max, color = Cluster), linewidth = 0.8) +
      geom_rect(aes(xmin = as.numeric(Cluster) - 0.3, xmax = as.numeric(Cluster) + 0.3, 
                    ymin = Q1, ymax = Q3, fill = Cluster), color = "black", alpha = 0.8) +
      geom_segment(aes(x = as.numeric(Cluster) - 0.3, xend = as.numeric(Cluster) + 0.3, 
                       y = Med, yend = Med), color = "black", linewidth = 1.2) +
      scale_fill_manual(values = hc_colors) +
      scale_color_manual(values = hc_colors) +
      theme_minimal(base_size = 15) + 
      labs(x = "Cluster Group", y = "Mean Daily Load (kW)") +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black")
      )
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # 3. Interactive Centroid Line Stack
  output$centroidPlot <- renderPlotly({
    req(clustered_data())
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
                                      text = paste0("Hour: ", Hour, ":00\nExpected Centroid Load: ", round(Centroid_Load, 1), " kW")), linewidth = 1.8) +
      scale_color_manual(values = hc_colors) +
      facet_wrap(~Cluster_Label, ncol = 1) +
      theme_minimal(base_size = 15) + 
      labs(x = "Hour of Day (0-23)", y = "Instantaneous Load (kW)") +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),   
        axis.title = element_text(size = 15, color = "black"),
        strip.text = element_text(size = 16, face = "bold", color = "black") 
      )
    
    k <- length(levels(centroids$Cluster_Label))
    calc_height <- k * 220 + 80
    
    ggplotly(p, tooltip = "text", height = calc_height) %>% 
      layout(hovermode = "x") %>% 
      config(displayModeBar = FALSE)
  })
  
  # --- Engineering Metrics Helper Reactive ---
  df_metrics_calculated <- reactive({
    req(clustered_data())
    clustered_data()$full_data %>%
      group_by(Cluster) %>%
      summarise(Median_Load = median(Load, na.rm = TRUE), .groups = "drop")
  })
  
  cluster_table_data <- reactive({
    req(clustered_data())
    df <- clustered_data()$full_data
    
    stats_df <- df %>%
      group_by(Cluster) %>%
      summarise(`Number of Days` = as.integer(n_distinct(Date)),
                `Mean Load (kW)` = round(mean(Load, na.rm = TRUE), 2),
                `Median Load (kW)` = round(median(Load, na.rm = TRUE), 2),
                `Min Load (kW)` = round(min(Load, na.rm = TRUE), 2),
                `Max Load (kW)` = round(max(Load, na.rm = TRUE), 2),
                `Volatility (Std Dev)` = round(sd(Load, na.rm = TRUE), 2),
                .groups = "drop")
    
    peak_hour_df <- df %>%
      group_by(Cluster, Hour) %>%
      summarise(MeanLoad = mean(Load, na.rm = TRUE), .groups = "drop") %>%
      group_by(Cluster) %>%
      slice_max(MeanLoad, n = 1, with_ties = FALSE) %>%
      select(Cluster, `Centroid Peak Hour` = Hour)
    
    left_join(stats_df, peak_hour_df, by = "Cluster") %>% arrange(as.numeric(as.character(Cluster)))
  })
  
  output$clusterSummaryTable <- renderTable({ cluster_table_data() }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- Decentralized View Exports ---
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
  
  output$downloadBoxplot <- downloadHandler(
    filename = function() { paste("boxplot_view_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      df_c <- clustered_data()$full_data
      ds <- df_c %>% group_by(Date, Cluster) %>% summarise(L = mean(Load, na.rm = TRUE), .groups='drop')
      
      p <- ggplot(ds, aes(x = Cluster, y = L, fill = Cluster)) +
        geom_boxplot(color = "black", alpha = 0.8) + scale_fill_manual(values = hc_colors) + theme_minimal(base_size = 14) + 
        labs(x = "Cluster Group", y = "Mean Daily Load (kW)", title = "Daily Mean Load Distribution") +
        theme(legend.position = "none", axis.text = element_text(color="black"), plot.title = element_text(face="bold", hjust=0.5))
      
      ggsave(filename = file, plot = p, device = "png", width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
  output$downloadCentroid <- downloadHandler(
    filename = function() { paste("centroid_profiles_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(input$k_clusters)
      df_c <- clustered_data()$full_data
      counts <- df_c %>% select(Date, Cluster) %>% distinct() %>% dplyr::count(Cluster, name = "Days") %>% arrange(as.numeric(as.character(Cluster))) %>%
        mutate(CL = factor(paste0("Cluster ", Cluster, " (", Days, " days)"), levels = paste0("Cluster ", Cluster, " (", Days, " days)")))
      df_c <- df_c %>% left_join(counts, by = "Cluster")
      cents <- df_c %>% group_by(CL, Hour) %>% summarise(CLoad = mean(Load, na.rm = TRUE), Cluster = dplyr::first(Cluster), .groups = 'drop')
      
      p <- ggplot() + geom_line(data = df_c, aes(x = Hour, y = Load, group = Date), color = "dimgray", alpha = 0.25) +
        geom_line(data = cents, aes(x = Hour, y = CLoad, color = Cluster), linewidth = 1.7) + scale_color_manual(values = hc_colors) +
        facet_wrap(~CL, ncol = 1) + theme_minimal(base_size = 14) + 
        labs(x = "Hour of Day (0-23)", y = "Instantaneous Load (kW)", title = "Cluster Load Profiles (Centroids)") +
        theme(legend.position = "none", axis.text = element_text(color="black"), strip.text = element_text(face="bold", color="black"), plot.title = element_text(face="bold", hjust=0.5))
      
      k <- as.numeric(input$k_clusters)
      calc_height <- 16 * (0.4 + (k * 0.22))
      
      ggsave(filename = file, plot = p, device = "png", width = 16, height = calc_height, dpi = 300, bg = "white")
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() { paste("cluster_metrics_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cluster_table_data(), file, row.names = FALSE) }
  )
}

# Run the standalone application 
shinyApp(ui = ui, server = server)