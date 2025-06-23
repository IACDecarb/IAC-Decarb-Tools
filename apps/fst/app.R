#Application starts here
library(shiny)
library(htmlwidgets)
library(readxl)
library(networkD3)
library(tidyverse)
library(webshot)
library(magick)
library(viridis)
library(shinythemes)
library(janitor)
library(reshape2)
library(shinyWidgets)
library(openxlsx)
library(htmltools)
library(DT)
library(shinycssloaders)
library(prompter)
library(bslib)
library(shinyFeedback)
library(plotly)
library(scales)

install_phantomjs(force = T)

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Function to check presence of an entry in other dataframes
check_presence <- function(entry, dataframes, current_index) {
  presence <- c()
  for (i in seq_along(dataframes)) {
    if (i != current_index) {
      if (entry %in% dataframes[[i]]$source) {
        presence <- c(presence, i)
      }
    }
  }
  if (length(presence) == 0) {
    return(current_index)
  } else {
    return(paste(current_index, "and", paste(presence, collapse = " and ")))
  }
}

# Function to calculate proportions
calculate_proportion <- function(presence, values) {
  ######
  present <- as.numeric(strsplit(as.character(presence), " and ")[[1]])######
  present_values <- values[present]######
  if (length(present_values) == 1) {
    ######
    return(1)  # If only one value, return 1 as the proportion######
  }######
  present_values / sum(present_values)######
}######


# Function to apply proportions to emissions
apply_proportions <- function(presence, emissions, values) {
  props <- calculate_proportion(presence, values)
  emissions * props[1]  # We take the first proportion as we're calculating for product 1
}

format_sentences <- function(sentences) {
  # First sentence as a paragraph (intro)
  intro <- paste0("<p>", sentences[1], "</p>")
  # Remaining sentences as list items
  if (length(sentences) > 1) {
    bullets <- paste0("<ul>", paste(sapply(sentences[-1], function(s)
      paste0(
        "<li style='margin-left: 30px;margin-bottom: 15px;'>",
        s,
        "</li>"
      )), collapse = ""), "</ul>")
    return(paste0(intro, bullets))
  } else {
    return(intro)
  }
}

format_sentences_bullet <- function(sentences) {
  bullets <- paste0(
    "<ul style='padding-inline-start: 30px; margin-left: 0;'>",
    paste(sapply(sentences, function(s)
      paste0(
        "<li style='margin-left: 30px; margin-bottom: 15px;'>",
        s,
        "</li>"
      )), collapse = ""),
    "</ul>"
  )
  return(bullets)
}



ui <- fluidPage(
  theme = shinytheme("flatly"),
  shinyFeedback::useShinyFeedback(),
  use_prompt(),
  includeCSS(system.file("css", "kable_extra.css", package = "kableExtra")),
  tags$head(
    tags$style(
      HTML(
        "
      .output-text {
        font-size: 20px;
        color: black;
        text-align: center;
      }
      .custom-tabset {
        width: 80%;
        margin-left: auto;
        margin-right: auto;
      }
      .custom-tabset > .nav-tabs {
        width: 100%;
      }
      .custom-tabset > .tab-content {
        width: 100%;
      }
      .brand-picker button.btn.dropdown-toggle.btn-default {
        background-color: #FFFFFF;
        color: black;
        border: none !important;
      box-shadow: none !important;
      outline: none !important;
      }
      #intensity_table table {
        font-family: 'Arial', sans-serif;
        font-size: 18px;
      }
      var timers = {};

    Shiny.addCustomMessageHandler('debounce', function(message) {
      if (timers[message.timer]) {
        clearTimeout(timers[message.timer]);
      }

      timers[message.timer] = setTimeout(function() {
        eval(message.expr);
        delete timers[message.timer];
      }, message.wait);
    });

    Shiny.addCustomMessageHandler('cancel-timer', function(timer) {
      if (timers[timer]) {
        clearTimeout(timers[timer]);
        delete timers[timer];
      }
    });
    
     .responsive-plot-container {
    position: relative;
    height: 100%;
    padding-bottom: 2%;
    overflow: auto;
    margin: 0 auto;
  }
  
  /* Large desktops (≥1700px) */
  @media (min-width: 1700px) {
    .responsive-plot-container {
      width: 65%;
      max-width: 1440px;
    }
  }
  
  /* Medium devices (992px - 1199px) */
  @media (min-width: 992px) and (max-width: 1699px) {
    .responsive-plot-container {
      width: 90%;
    }
  }
  
  /* Tablets (768px - 991px) */
  @media (min-width: 768px) and (max-width: 991px) {
    .responsive-plot-container {
      width: 80%;
    }
  }
  
  /* Mobile devices (<768px) */
  @media (max-width: 767px) {
    .responsive-plot-container {
      width: 95%;
      padding-bottom: 5%;
    }
  }    
      "
      )
    ),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
  ),
  titlePanel(HTML("Facility Sankey Tool"), windowTitle = "FST"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Load Inputs",
      sidebarLayout(
        sidebarPanel(
          tags$style(
            HTML(
              "
      #downloadData1 {
        font-weight: bold;
        font-size: 18px;
      }
    #textOutput1{
        color:black;
        font-size:20px;
        background: white;
        border: none;
        padding: 10px;
        font-family: Arial, sans-serif;
              }"
            )
          ),
          downloadLink("downloadData1", "Download Facility Sankey Tool - Input Sheet"),
          textInput("cname", "Enter Facility Name"),
          br(),
          br(),
          fileInput("file", "Upload \'FST Input Sheet\' Excel File", accept = ".xlsx"),
          br(),
          tags$style(HTML(
            "
      #downloadData2 {
        font-weight: bold;
        font-size: 18px;
      }
    "
          )),
          downloadLink("downloadData2", "Download Tool Documentation (Draft Version)")
          
        ),
        mainPanel(
          h1("Instructions"),
          br(),
          h3("Tab 1: Load Inputs"),
          tags$p("1. Download and fill out Input Sheet.", style = "font-size: 18px;"),
          tags$p("2. Upload completed Input Sheet.", style = "font-size: 18px;"),
          tags$p("3. Refer to documentation if needed.", style = "font-size: 18px;"),
          br(),
          h3("Tab 2: Energy Sankey"),
          tags$p("1. View and customize Energy Sankey Diagram.", style = "font-size: 18px;"),
          tags$p("2. Download Energy Sankey Diagram.", style = "font-size: 18px;"),
          h3("Tab 3: Energy Costs Sankey"),
          tags$p("1. View and customize Energy Costs Sankey Diagram.", style = "font-size: 18px;"),
          tags$p("2. Download Energy Costs Sankey Diagram.", style = "font-size: 18px;"),
          h3("Tab 4: Emissions Sankey"),
          tags$p("1. View and customize Emissions Sankey Diagram.", style = "font-size: 18px;"),
          tags$p("2. Download Emissions Sankey Diagram.", style = "font-size: 18px;"),
          br(),
          h3("Tab 5: Product Intensity Calculator"),
          tags$p(
            "1. Estimate Product Energy, Energy costs, and Emissions Intensity based on associated processes or equipment.",
            style = "font-size: 18px;"
          )
        )
      ),
      tags$div(
        style = "bottom: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: flex-end; padding: 10px 0;",
        # Center the container
        tags$div(
          style = "text-align: left; margin-right: 150px;",
          # Left-align content and add spacing
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          # Left-align content
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
      
    ),
    tabPanel(
      "Energy Sankey",
      sidebarLayout(
        sidebarPanel(
          selectInput("units_e", "Select Units", c("MMBtu/yr", "MWh/yr")),
          textOutput('move'),
          radioButtons("perc_e", "Select Value Type", c("Absolute", "Percentage")),
          numericInput(
            "precision_e",
            "Choose precision level of numeric values",
            1,
            -20,
            20,
            1
          ),
          sliderInput("vsc_e", "Adjust vertical scaling of the Sankey Diagram", 1, 100, 50),
          numericInput(
            "height_e",
            "Adjust height of downloaded image (px)",
            500,
            500,
            20000,
            250
          ),
          numericInput(
            "width_e",
            "Adjust width of downloaded image (px)",
            1000,
            750,
            20000,
            250
          ),
          downloadButton("downloadPNG_e", "Click Here to Download plot as Image"),
          
        ),
        mainPanel(
          div(uiOutput("output_text_e"), class = "output-text"),
          div(style = "position: relative; padding-bottom: 100px; width: 100%; max-height: 100%; preserveAspectRatio='xMinYMin meet';  background-color: #f8f8f8;", uiOutput("diagram_energy"))
        )
        
      ),
      tags$div(
        style = "bottom: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: flex-end; padding: 10px 0;",
        # Center the container
        tags$div(
          style = "text-align: left; margin-right: 150px;",
          # Left-align content and add spacing
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          # Left-align content
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
      
    ),
    tabPanel(
      "Energy Costs Sankey",
      sidebarLayout(
        sidebarPanel(
          radioButtons("perc_ec", "Select Value Type", c("Absolute", "Percentage")),
          numericInput(
            "precision_ec",
            "Choose precision level of numeric values",
            1,
            -20,
            20,
            1
          ),
          sliderInput(
            "vsc_ec",
            "Adjust vertical scaling of the Sankey Diagram",
            1,
            100,
            50
          ),
          numericInput(
            "height_ec",
            "Adjust height of downloaded image (px)",
            500,
            500,
            20000,
            250
          ),
          numericInput(
            "width_ec",
            "Adjust width of downloaded image (px)",
            1000,
            750,
            20000,
            250
          ),
          downloadButton("downloadPNG_ec", "Click Here to Download plot as Image"),
          
        ),
        mainPanel(
          div(uiOutput("output_text_ec"), class = "output-text"),
          div(style = "position: relative; padding-bottom: 100px; width: 100%; max-height: 100%; preserveAspectRatio='xMinYMin meet';  background-color: #f8f8f8;", uiOutput("diagram_energy_costs"))
        )
        
      ),
      tags$div(
        style = "bottom: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: flex-end; padding: 10px 0;",
        # Center the container
        tags$div(
          style = "text-align: left; margin-right: 150px;",
          # Left-align content and add spacing
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          # Left-align content
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
      
    ),
    tabPanel(
      "Emissions Sankey",
      sidebarLayout(
        sidebarPanel(
          selectInput("units", "Select Units", c("MT CO₂e/yr", "lbs. of CO₂e/yr")),
          textOutput('move'),
          radioButtons("perc", "Select Value Type", c("Absolute", "Percentage")),
          numericInput(
            "precision",
            "Choose precision level of numeric values",
            1,
            -20,
            20,
            1
          ),
          sliderInput("vsc", "Adjust vertical scaling of the Sankey Diagram", 1, 100, 50),
          numericInput(
            "height",
            "Adjust height of downloaded image (px)",
            500,
            500,
            20000,
            250
          ),
          numericInput(
            "width",
            "Adjust width of downloaded image (px)",
            1000,
            750,
            20000,
            250
          ),
          downloadButton("downloadPNG", "Click Here to Download plot as Image"),
          
        ),
        mainPanel(
          div(uiOutput("output_text"), class = "output-text"),
          div(style = "position: relative; width: 100%; max-height: 100%; preserveAspectRatio='xMinYMin meet';  background-color: #f8f8f8;", uiOutput("diagram")),
          br(),
          span(textOutput("titleef"), style = "font-size: 21px; margin-left: 10px;"),
          div(style = "padding-bottom: 100px;", tableOutput("table1"))
          
        )
        
      ),
      tags$div(
        style = "width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: flex-end; padding: 10px 0;",
        # Center the container
        tags$div(
          style = "text-align: left; margin-right: 150px;",
          # Left-align content and add spacing
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          # Left-align content
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
    ),
    tabPanel(
      "Product Intensity Calculator",
      sidebarLayout(
        sidebarPanel(
          width = 5,
          tags$style(HTML("
        font-weight: bold;
        font-size: 16px;
              ")),
          fluidRow(
            column(
              12,
              h4("Step 1: Select Method and Number of Products:", style = "font-weight: bold; font-size: 18px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;float: left;")
            ),
            wellPanel(
              style = "border: 2px solid #ccc; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              selectInput(
                "products_num",
                label = tags$span(
                  "Enter Number of Products:",
                  tags$span(icon(name = "circle-exclamation")) |>
                    add_prompt(
                      message = "Select the number of products that are manufactured. Alternatively, select the number of products whose intensities are being studied.",
                      position = "right",
                      size = "large"
                    )
                ),
                choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                width = "95%"
              ),
              radioButtons(
                "selected_method",
                label = tags$span(
                  "Select Allocation Approach:",
                  tags$span(icon(name = "circle-exclamation")) |>
                    add_prompt(
                      message = "Select quantity-based allocation if you would like to allocate based on underlying physical relationship, such as mass, volume, or number of units. Select revenue-based method if physical relationship cannot be established (or if production is driven by market value of products).",
                      position = "right",
                      size = "large"
                    )
                ),
                choices = c("Quantity-based", "Revenue-based"),
                selected = "Quantity-based",
                width = "95%"
              ),
              fluidRow(column(
                4,
                selectInput(
                  "energy_units_int",
                  "Select Energy Units:",
                  choices = c("MMBtu", "MWh"),
                  selected = "MMBtu"
                )
              )),
              uiOutput("dynamic_revenue")
            )
          ),
          fluidRow(column(12, uiOutput("product_inputs"))),
          fluidRow(style = "display: flex; justify-content: center; align-items: center;", column(
            12,
            actionButton("calc_int", "Calculate Product Intensity", width = "95%")
          )),
          div(
            style = "margin-bottom: -10px;margin-top:0px;",
            checkboxInput("en", "Energy Intensity", TRUE, width = "95%")
          ),
          div(
            style = "margin-bottom: -10px;",
            checkboxInput("ec", "Energy Costs Intensity", TRUE, width = "95%")
          ),
          div(
            style = "margin-bottom: 0px;",
            checkboxInput("em", "Emissions Intensity", FALSE, width = "95%")
          )
        ),
        mainPanel(    width = 7,
                      uiOutput("mainpanelUI")
        )
      ),
      tags$div(
        style = "bottom: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: center; align-items: flex-end; padding: 10px 0;",
        # Center the container
        tags$div(
          style = "text-align: left; margin-right: 150px;",
          # Left-align content and add spacing
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          # Left-align content
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  excelFilePath <- "Facility Sankey Tool - Input Sheet.xlsx"
  
  docFilePath <- 'User Guide for Facility Sankey Tool.pdf'
  
  observeEvent(input$file, {
    updateTabsetPanel(session, "tabs", selected = "Energy Sankey")
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      basename(excelFilePath)
    },
    content = function(file) {
      file.copy(excelFilePath, file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      basename(docFilePath)
    },
    content = function(file) {
      file.copy(docFilePath, file)
    }
  )
  
  # Read the uploaded nodes Excel file
  nodes_data <- reactive({
    req(input$file)
    
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source))
    aa <- aa %>%
      mutate(
        energy_or_emissions_category = if_else(
          energy_or_emissions_category == "Conserved Energy",
          "Avoided Emissions",
          energy_or_emissions_category
        )
      )
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    em.src <- tibble('Name' = unique(aa$`energy_or_emissions_category`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total'
    non_ele <- ene.src %>%
      
      filter(Name != 'Electricity')
    
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel'
    }
    nodes.h <- rbind(nodes.hh, ene.src, end.use, em.src)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    nodes
  })
  
  
  nodes_data_energy_costs <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(energy_source))
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total Energy Costs'
    non_ele <- ene.src %>%
      filter(Name != 'Electricity')
    
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel'
    }
    
    nodes.h <- rbind(nodes.hh, ene.src, end.use)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    nodes
  })
  
  nodes_data_energy <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(energy_source))
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total Energy'
    non_ele <- ene.src %>%
      filter(Name != 'Electricity')
    
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel'
    }
    
    aa.ne <- aa %>%
      filter(energy_or_emissions_category == "Conserved Energy")
    
    ce_link_val <- numeric(0)
    if (!is_empty(aa.ne$no)) {
      nodes.hh[3, 'Name'] <- 'Conserved Energy'
    }
    nodes.h <- rbind(nodes.hh, ene.src, end.use)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    nodes
  })
  
  output$dynamic_revenue <- renderUI({
    if (input$selected_method == "Revenue-based") {
      fluidRow(style = "display:flex;align-items:flex-end", column(
        12,
        selectInput(
          "revenue_name",
          "Select Revenue Calculation Method:",
          choices = c("$/unit product", "Gross product revenue ($)", "Revenue %"),
          width = "100%"
        )
      ))
    }
  })
  
  end_use <- reactiveValues(measures = NULL)
  
  stored_values_qty <- reactiveValues(
    name_inputs = list(),
    text_inputs = list(),
    num_inputs = list(),
    picker_inputs = list()
  )
  
  stored_values_rev_per <- reactiveValues(
    name_inputs = list(),
    text_inputs = list(),
    num_inputs_qty = list(),
    num_inputs_per = list(),
    picker_inputs = list()
  )
  
  stored_values_rev <- reactiveValues(
    name_inputs = list(),
    text_inputs = list(),
    num_inputs_qty = list(),
    num_inputs_cost = list(),
    picker_inputs = list()
  )
  
  observeEvent(input$products_num, {
    num <- as.numeric(input$products_num)
    isolate({
      stored_values_qty$name_inputs <- head(stored_values_qty$name_inputs, num)
      stored_values_qty$text_inputs <- head(stored_values_qty$text_inputs, num)
      stored_values_qty$num_inputs <- head(stored_values_qty$num_inputs, num)
      stored_values_qty$picker_inputs <- head(stored_values_qty$picker_inputs, num)
      
      stored_values_rev_per$name_inputs <- head(stored_values_rev_per$name_inputs, num)
      stored_values_rev_per$text_inputs <- head(stored_values_rev_per$text_inputs, num)
      stored_values_rev_per$num_inputs_qty <- head(stored_values_rev_per$num_inputs_qty, num)
      stored_values_rev_per$num_inputs_per <- head(stored_values_rev_per$num_inputs_per, num)
      stored_values_rev_per$picker_inputs <- head(stored_values_rev_per$picker_inputs, num)
      
      stored_values_rev$name_inputs <- head(stored_values_rev$name_inputs, num)
      stored_values_rev$text_inputs <- head(stored_values_rev$text_inputs, num)
      stored_values_rev$num_inputs_qty <- head(stored_values_rev$num_inputs_qty, num)
      stored_values_rev$num_inputs_cost <- head(stored_values_rev$num_inputs_cost, num)
      stored_values_rev$picker_inputs <- head(stored_values_rev$picker_inputs, num)
    })
  })
  
  observeEvent(input$products_num, {
    num <- as.numeric(input$products_num)
    if (input$selected_method == "Quantity-based") {
      lapply(1:num, function(i) {
        name_id <- paste0("products_name_", i)
        text_id <- paste0("units_name_", i)
        num_id <- paste0("qty_", i)
        picker_id <- paste0("process_", i)
        
        if (!is.null(input[[name_id]])) {
          stored_values_qty$name_inputs[[i]] <- input[[name_id]]
        }
        if (!is.null(input[[text_id]])) {
          stored_values_qty$text_inputs[[i]] <- input[[text_id]]
        }
        if (!is.null(input[[num_id]])) {
          stored_values_qty$num_inputs[[i]] <- input[[num_id]]
        }
        if (!is.null(input[[picker_id]])) {
          stored_values_qty$picker_inputs[[i]] <- input[[picker_id]]
        }
      })
    } else if (input$selected_method == "Revenue-based") {
      req(input$revenue_name)
      if (input$revenue_name == "Revenue %") {
        lapply(1:num, function(i) {
          name_id <- paste0("products_name_", i)
          text_id <- paste0("units_name_", i)
          num_id_qty <- paste0("qty_", i)
          num_id_per <- paste0("revenue_", i)
          picker_id <- paste0("process_", i)
          
          if (!is.null(input[[name_id]])) {
            stored_values_rev_per$name_inputs[[i]] <- input[[name_id]]
          }
          if (!is.null(input[[text_id]])) {
            stored_values_rev_per$text_inputs[[i]] <- input[[text_id]]
          }
          if (!is.null(input[[num_id_qty]])) {
            stored_values_rev_per$num_inputs_qty[[i]] <- input[[num_id_qty]]
          }
          if (!is.null(input[[num_id_per]])) {
            stored_values_rev_per$num_inputs_per[[i]] <- input[[num_id_per]]
          }
          if (!is.null(input[[picker_id]])) {
            stored_values_rev_per$picker_inputs[[i]] <- input[[picker_id]]
          }
        })
      } else {
        lapply(1:num, function(i) {
          name_id <- paste0("products_name_", i)
          text_id <- paste0("units_name_", i)
          num_id_qty <- paste0("qty_", i)
          num_id_cost <- paste0("revenue_", i)
          picker_id <- paste0("process_", i)
          
          if (!is.null(input[[name_id]])) {
            stored_values_rev$name_inputs[[i]] <- input[[name_id]]
          }
          if (!is.null(input[[text_id]])) {
            stored_values_rev$text_inputs[[i]] <- input[[text_id]]
          }
          if (!is.null(input[[num_id_qty]])) {
            stored_values_rev$num_inputs_qty[[i]] <- input[[num_id_qty]]
          }
          if (!is.null(input[[num_id_cost]])) {
            stored_values_rev$num_inputs_cost[[i]] <- input[[num_id_cost]]
          }
          if (!is.null(input[[picker_id]])) {
            stored_values_rev$picker_inputs[[i]] <- input[[picker_id]]
          }
        })
      }
    }
  })
  
  
  output$product_inputs <- renderUI({
    req(input$file)
    num <- input$products_num
    
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189") %>%
      clean_names()
    
    
    aa <- aa %>%
      filter(!is.na(source))
    end.use <- tibble('Name' = aa$`source`)
    end_use$measures <- end.use
    
    units_name_reactive <- reactive({
      input$units_name
    })
    
    if (input$selected_method == "Quantity-based") {
      product_boxes <- lapply(1:num, function(i) {
        name_val <- if (i <= length(stored_values_qty$name_inputs) &&
                        !is.null(stored_values_qty$name_inputs[[i]])) {
          stored_values_qty$name_inputs[[i]]
        } else {
          
        }
        
        text_val <- if (i <= length(stored_values_qty$text_inputs) &&
                        !is.null(stored_values_qty$text_inputs[[i]])) {
          stored_values_qty$text_inputs[[i]]
        } else {
          "metric ton"
        }
        
        picker_val <- if (i <= length(stored_values_qty$picker_inputs) &&
                          !is.null(stored_values_qty$picker_inputs[[i]])) {
          stored_values_qty$picker_inputs[[i]]
        } else {
          unlist(end_use$measures)
        }
        
        div(tagList(fluidRow(
          if (i == 1) {
            column(
              12,
              h4("Step 2: Enter Quantity-based Inputs", style = "font-weight: bold; font-size: 18px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; margin-bottom: 15px;")
            )
          },
          div(
            style = "border: 2px solid #ccc; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
            fluidRow(
              style = "display:flex;align-items:flex-end;",
              column(
                4,
                textInput(
                  paste0("products_name_", i),
                  "Enter Product Name:",
                  value = name_val,
                  width = "95%"
                )
              ),
              column(
                4,
                textInput(
                  inputId =  paste0("units_name_", i),
                  label = tags$span(
                    paste0("Enter Units of Quantity:"),
                    tags$span(icon(name = "circle-exclamation")) |>
                      add_prompt(
                        message = "Note that units name, such as metric ton, are placeholders only. If you entered kg and pounds for two products, respectively, the tool will not convert them to identical units",
                        position = "right",
                        size = "large"
                      )
                  ),
                  value = text_val,
                  width = "95%"
                )
              ),
              column(4, uiOutput(paste0(
                "qty_values_", i
              )))
            )
            ,
            tags$div(
              pickerInput(
                paste0("process_", i),
                label = tags$span(
                  paste("Select Processes/Equipment Relevant to this Product:"),
                  tags$span(icon(name = "circle-exclamation")) |>
                    add_prompt(
                      message = "The list below has been referenced from the excel input sheet.",
                      position = "right",
                      size = "large"
                    )
                ),
                choices = end_use$measures,
                options = list(`actions-box` = TRUE, virtualScroll = T),
                selected = picker_val,
                multiple = TRUE,
                width = "95%"
              ),
              class = "brand-picker"
            )
          )
        )))
      })
      
      do.call(tagList, product_boxes)
      
    } else if (input$selected_method == "Revenue-based") {
      if (input$revenue_name == "Revenue %") {
        product_boxes <- lapply(1:num, function(i) {
          name_val <- if (i <= length(stored_values_rev_per$name_inputs) &&
                          !is.null(stored_values_rev_per$name_inputs[[i]])) {
            stored_values_rev_per$name_inputs[[i]]
          } else {
            
          }
          
          text_val <- if (i <= length(stored_values_rev_per$text_inputs) &&
                          !is.null(stored_values_rev_per$text_inputs[[i]])) {
            stored_values_rev_per$text_inputs[[i]]
          } else {
            "metric ton"
          }
          
          num_per_val <- if (i <= length(stored_values_rev_per$num_inputs_per) &&
                             !is.null(stored_values_rev_per$num_inputs_per[[i]])) {
            stored_values_rev_per$num_inputs_per[[i]]
          } else {
            0
          }
          
          picker_val <- if (i <= length(stored_values_rev_per$picker_inputs) &&
                            !is.null(stored_values_rev_per$picker_inputs[[i]])) {
            stored_values_rev_per$picker_inputs[[i]]
          } else {
            unlist(end_use$measures)
          }
          
          div(tagList(fluidRow(
            if (i == 1) {
              column(
                12,
                h4(
                  "Step 2: Enter Quantity- and Revenue-based Inputs",
                  style = "font-weight: bold; font-size: 18px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;float: left;"
                )
              )
            },
            div(
              style = "border: 2px solid #ccc; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              fluidRow(
                style = "display:flex;align-items:flex-end;",
                column(
                  4,
                  textInput(
                    paste0("products_name_", i),
                    "Enter Product Name:",
                    value = name_val,
                    width = "95%"
                  )
                ),
                column(
                  4,
                  textInput(
                    inputId =  paste0("units_name_", i),
                    label = tags$span(
                      paste0("Enter Units of Quantity:"),
                      tags$span(icon(name = "circle-exclamation")) |>
                        add_prompt(
                          message = "Note that units name, such as metric ton, are placeholders only. If you entered kg and pounds for two products, respectively, the tool will not convert them to identical units",
                          position = "right",
                          size = "large"
                        )
                    ),
                    value = text_val,
                    width = "95%"
                  )
                ),
                column(4, uiOutput(paste0(
                  "qty_values_", i
                )))
                ,
                column(
                  4,
                  autonumericInput(
                    inputId =   paste0("revenue_", i),
                    label =  paste("Enter Product ", i, " Revenue %:"),
                    value = num_per_val,
                    currencySymbol = "%",
                    currencySymbolPlacement = "s",
                    minimumValue = "0",
                    maximumValue = "100",
                    align = "left",
                    width = "95%",
                    decimalPlaces = 0
                  )
                )
              ),
              tags$div(
                pickerInput(
                  paste0("process_", i),
                  label = tags$span(
                    paste(
                      "Select Processes/Equipment Relevant to this Product:"
                    ),
                    tags$span(icon(name = "circle-exclamation")) |>
                      add_prompt(
                        message = "The list below has been referenced from the excel input sheet.",
                        position = "right",
                        size = "large"
                      )
                  ),
                  choices = end_use$measures,
                  options = list(
                    `actions-box` = TRUE,
                    virtualScroll = T
                  ),
                  selected = picker_val,
                  multiple = TRUE,
                  width = "95%"
                ),
                class = "brand-picker"
              )
            )
          )))
          
        })
        do.call(tagList, product_boxes)
      } else {
        product_boxes <- lapply(1:num, function(i) {
          name_val <- if (i <= length(stored_values_rev$name_inputs) &&
                          !is.null(stored_values_rev$name_inputs[[i]])) {
            stored_values_rev$name_inputs[[i]]
          } else {
            
          }
          
          text_val <- if (i <= length(stored_values_rev$text_inputs) &&
                          !is.null(stored_values_rev$text_inputs[[i]])) {
            stored_values_rev$text_inputs[[i]]
          } else {
            "metric ton"
          }
          
          num_cost_val <- if (i <= length(stored_values_rev$num_inputs_cost) &&
                              !is.null(stored_values_rev$num_inputs_cost[[i]])) {
            stored_values_rev$num_inputs_cost[[i]]
          } else {
            0
          }
          
          picker_val <- if (i <= length(stored_values_rev$picker_inputs) &&
                            !is.null(stored_values_rev$picker_inputs[[i]])) {
            stored_values_rev$picker_inputs[[i]]
          } else {
            unlist(end_use$measures)
          }
          
          div(tagList(fluidRow(
            if (i == 1) {
              column(
                12,
                h4(
                  "Step 2: Enter Quantity- and Revenue-based Inputs",
                  style = "font-weight: bold; font-size: 18px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;float: left;"
                )
              )
            },
            div(
              style = "border: 2px solid #ccc; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              fluidRow(
                style = "display:flex;align-items:flex-end;",
                column(
                  4,
                  textInput(
                    paste0("products_name_", i),
                    "Enter Product Name:",
                    value = name_val,
                    width = "95%"
                  )
                ),
                column(
                  4,
                  textInput(
                    inputId =  paste0("units_name_", i),
                    label = tags$span(
                      paste0("Enter Units of Quantity:"),
                      tags$span(icon(name = "circle-exclamation")) |>
                        add_prompt(
                          message = "Note that units name, such as metric ton, are placeholders only. If you entered kg and pounds for two products, respectively, the tool will not convert them to identical units",
                          position = "right",
                          size = "large"
                        )
                    ),
                    value = text_val,
                    width = "95%"
                  )
                ),
                
                column(4, uiOutput(paste0(
                  "qty_values_", i
                ))),
                column(
                  4,
                  autonumericInput(
                    inputId =   paste0("revenue_", i),
                    label =  if_else(
                      input$revenue_name == "Gross product revenue ($)",
                      paste("Enter Product Gross Revenue ($):"),
                      paste("Enter Product Revenue per ", input$units_name, ":")
                    ),
                    value = num_cost_val,
                    decimalPlaces = 0,
                    currencySymbol = "$",
                    currencySymbolPlacement = "p",
                    minimumValue = "0",
                    maximumValue = "10000000000000000000",
                    align = "left",
                    width = "95%"
                  )
                )
              ),
              tags$div(
                pickerInput(
                  paste0("process_", i),
                  label = tags$span(
                    paste(
                      "Select Processes/Equipment Relevant to this Product:"
                    ),
                    tags$span(icon(name = "circle-exclamation")) |>
                      add_prompt(
                        message = "The list below has been referenced from the excel input sheet.",
                        position = "right",
                        size = "large"
                      )
                  ),
                  choices = end_use$measures,
                  options = list(
                    `actions-box` = TRUE,
                    virtualScroll = T
                  ),
                  selected = picker_val,
                  multiple = TRUE,
                  width = "95%"
                ),
                class = "brand-picker"
              )
            )
          )))
        })
        do.call(tagList, product_boxes)
      }
    }
  })
  
  observe({
    num <- input$products_num
    
    if (input$selected_method == "Quantity-based") {
      lapply(1:num, function(i) {
        num_val <- if (i <= length(stored_values_qty$num_inputs) &&
                       !is.null(stored_values_qty$num_inputs[[i]])) {
          stored_values_qty$num_inputs[[i]]
        } else {
          
        }
        
        output[[paste0("qty_values_", i)]] <- renderUI({
          numericInput(
            paste0("qty_", i),
            paste0("Enter Product Quantity (", input[[paste0("units_name_", i)]], "):"),
            value = num_val,
            min = 0.0001,
            width = "95%"
          )
        })
      })
    } else if (input$selected_method == "Revenue-based") {
      req(input$revenue_name)
      if (input$revenue_name == "Revenue %") {
        lapply(1:num, function(i) {
          num_qty_val <- if (i <= length(stored_values_rev_per$num_inputs_qty) &&
                             !is.null(stored_values_rev_per$num_inputs_qty[[i]])) {
            stored_values_rev_per$num_inputs_qty[[i]]
          } else {
            0
          }
          
          output[[paste0("qty_values_", i)]] <- renderUI({
            numericInput(
              paste0("qty_", i),
              label = tags$span(
                paste0("Enter Product Quantity (", input[[paste0("units_name_", i)]], "):"),
                tags$span(icon(name = "circle-exclamation", )) |>
                  add_prompt(
                    message = "If quantity value is not available, please select 'Gross Product' revenue calculation method",
                    position = "right",
                    size = "large"
                  )
              ),
              value = num_qty_val,
              min = 0.0001,
              width = "95%"
            )
          })
        })
      } else {
        lapply(1:num, function(i) {
          num_qty_val <- if (i <= length(stored_values_rev$num_inputs_qty) &&
                             !is.null(stored_values_rev$num_inputs_qty[[i]])) {
            stored_values_rev$num_inputs_qty[[i]]
          } else {
            0
          }
          
          output[[paste0("qty_values_", i)]] <- renderUI({
            numericInput(
              paste0("qty_", i),
              label = tags$span(
                paste0("Enter Product Quantity (", input[[paste0("units_name_", i)]], "):"),
                tags$span(icon(name = "circle-exclamation", )) |>
                  add_prompt(
                    message = "If quantity value is not available, please select 'Gross Product' revenue calculation method",
                    position = "right",
                    size = "large"
                  )
              ),
              value = num_qty_val,
              min = 0.0001,
              width = "95%"
            )
          })
        })
      }
    }
  })
  
  tabsShown <- reactiveVal(FALSE)
  
  # Observe the button press and update the reactive value
  observeEvent(input$calc_int, {
    tabsShown(TRUE)
  })
  
  # Render the tabs conditionally based on the reactive value
  output$mainpanelUI <- renderUI({
    if (tabsShown()) {
      tagList(tabsetPanel(
        id = "ResultTabs",
        tabPanel(
          "Graphical Results",
          div(
            class = "responsive-plot-container",
            conditionalPanel(
              condition = "input.calc_int >= 1",
              shinycssloaders::withSpinner(
                if (input$en && input$ec && input$em) {
                  navset_card_underline(
                    id = "graph_tabs",
                    nav_panel("Energy Intensity Plot", value = "enPlotvalue",plotlyOutput("enPlot")),
                    nav_panel(
                      "Energy Costs Intensity Plot",value = "ecPlotvalue",
                      plotlyOutput("ecPlot")
                    ),
                    nav_panel(
                      "Emissions Intensity Plot",value = "emPlotvalue",
                      plotlyOutput("emPlot")
                    )
                  )
                } else if (input$en && input$ec) {
                  navset_card_underline(
                    id = "graph_tabs",
                    nav_panel("Energy Intensity Plot", value = "enPlotvalue",plotlyOutput("enPlot")),
                    nav_panel(
                      "Energy Costs Intensity Plot",value = "ecPlotvalue",
                      plotlyOutput("ecPlot")
                    )
                  )
                } else if (input$en && input$em) {
                  navset_card_underline(
                    id = "graph_tabs",
                    nav_panel("Energy Intensity Plot",value = "enPlotvalue", plotlyOutput("enPlot")),
                    nav_panel(
                      "Emissions Intensity Plot",value = "emPlotvalue",
                      plotlyOutput("emPlot")
                    )
                  )
                } else if (input$ec && input$em) {
                  navset_card_underline(
                    id = "graph_tabs",
                    nav_panel(
                      "Energy Costs Intensity Plot",value = "ecPlotvalue",
                      plotlyOutput("ecPlot")
                    ),
                    nav_panel(
                      "Emissions Intensity Plot",value = "emPlotvalue",
                      plotlyOutput("emPlot")
                    )
                  )
                } else if (input$en) {
                  navset_card_underline(
                    id = "graph_tabs",nav_panel("Energy Intensity Plot",value = "enPlotvalue", plotlyOutput("enPlot")))
                } else if (input$ec) {
                  navset_card_underline(
                    id = "graph_tabs",nav_panel(
                      "Energy Costs Intensity Plot",value = "ecPlotvalue",
                      plotlyOutput("ecPlot")
                    ))
                } else if (input$em) {
                  navset_card_underline(
                    id = "graph_tabs",nav_panel(
                      "Emissions Intensity Plot",value = "emPlotvalue",
                      plotlyOutput("emPlot")
                    ))
                },
                type = getOption("spinner.type", default = 8)
              )
            )
          )
        )
        ,
        tabPanel(
          "Intensity Tables",
          div(
            class = "responsive-plot-container",
            conditionalPanel(
              condition = "input.calc_int >= 1",
              shinycssloaders::withSpinner(
                DTOutput("intensity_table"),
                type = getOption("spinner.type", default = 8)
              )
            )
          ),
          div(class = "responsive-plot-container", uiOutput("show_dl_link"))
          
        )
      ),
      uiOutput("textOutput1"))
    } else {
      # Optionally, you can display a message or nothing
      h3("")
    }
  })
  
  units_conversion <- reactive({
    if (input$units == "lbs. of CO₂e/yr" &
        input$perc != "Percentage") {
      2204.6226218 # Conversion factor
      
    } else {
      1
    }
  })
  
  units_conversion_e <- reactive({
    if (input$units_e == "MWh/yr" & input$perc_e != "Percentage") {
      0.293071 # Conversion factor
    } else {
      1
    }
  })
  
  # Read the uploaded links Excel file
  ef <- reactive({
    req(input$file)
    
    
    bb <- read_excel(input$file$datapath, sheet = 'Emission Inputs (Optional)', range = "k10:q27")
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source))
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    bb1 <- bb %>%
      filter(!is.na(Title))
    fortable <- aa %>%
      mutate(efta = paste0(energy_source, units)) %>%
      select(efta) %>%
      filter(!is.na(efta))
    
    result <- merge(bb1,
                    ene.src,
                    by.x = "Title",
                    by.y = "Name",
                    all = FALSE)
    
    long_data <- melt(
      result,
      id.vars = c("Title", "Source"),
      variable.name = "Units",
      value.name = "Factors"
    )
    
    display_data <- merge(bb1,
                          ene.src,
                          by.x = "Title",
                          by.y = "Name",
                          all = FALSE)
    
    dd1 <- long_data %>%
      mutate(efta = paste0(Title, Units))
    
    dd2 <- tibble('efta' = unique(fortable$`efta`))
    
    result2 <- merge(dd1, dd2, by = "efta", all = FALSE)
    result2 <- result2 %>%
      select(-efta) %>%
      select(Title, Factors, Units, Source) %>%
      mutate(Units = paste0("MTCO₂e/", Units))
    result2$Factors <- signif(result2$Factors, 3)
    result2$Factors <- format(result2$Factors, scientific = T)
    
    result2
  })
  
  
  tef <- reactive({
    ef <- ef()
    if (is_empty(ef$Factors)) {
      ""
    } else{
      "Emission Factors Used"
    }
  })
  
  output$titleef <- renderText({
    text <- tef()
  })
  
  output$table1 <- renderTable({
    # Set to 0 to always display in scientific notation
    ef()
  })
  
  temp <- reactive({
    req(input$file)
    
    
    temp <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    temp <- clean_names(temp)
    temp <- temp %>%
      filter(!is.na(source))
    temp
  })
  
  temp_e <- reactive({
    req(input$file)
    temp_e <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    temp_e <- clean_names(temp_e)
    temp_e <- temp_e %>%
      filter(!is.na(energy_source))
    temp_e
  })
  
  temp_ec <- reactive({
    req(input$file)
    temp_ec <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    temp_ec <- clean_names(temp_ec)
    temp_ec <- temp_ec %>%
      filter(!is.na(energy_source))
  })
  
  
  
  links_data <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source))
    aa <- aa %>%
      mutate(
        energy_or_emissions_category = if_else(
          energy_or_emissions_category == "Conserved Energy",
          "Avoided Emissions",
          energy_or_emissions_category
        )
      )
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    em.src <- tibble('Name' = unique(aa$`energy_or_emissions_category`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total'
    non_ele <- ene.src %>%
      filter(Name != 'Electricity')
    
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel'
    }
    
    nodes.h <- rbind(nodes.hh, ene.src, end.use, em.src)
    
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    
    links.h <- tibble(
      'No' = 0,
      'Source' = 0,
      'Target' = 0,
      'Value' = 0
    )
    
    aa.e <- aa %>%
      filter(energy_or_emissions_category == "Energy")
    
    for (i in 1:nrow(aa.e)) {
      links.h[i, 'No'] <- i
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'source'] == nodes[j, 'Name']) {
          links.h[i, 'Target'] <- nodes[[j, 'No']] - 1
        }
      }
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'energy_source'] == nodes[j, 'Name']) {
          links.h[i, 'Source'] <- nodes[[j, 'No']] - 1
        }
      }
      
      if (input$perc == "Percentage") {
        links.h[i, 'Value'] <- aa.e[i, 'percentage_of_total_emissions'] * 100
      } else {
        links.h[i, 'Value'] <- aa.e[i, 'co2e_emissions_mt_co2e_yr']
      }
      
    }
    
    links.hh <- links.h %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    ctr <- 0
    
    links.hh2 <- tibble(Source = c(), Value = c())
    ele_link <- tibble(Source = c(), Value = c())
    ele <- nodes %>%
      filter(Name == 'Electricity')
    
    if (!is_empty(ele$No)) {
      ele_link_val <- as.numeric(ele$No - 1)
      ele_link <- links.hh %>%
        filter(Source == ele_link_val)
      
      links.hh <- links.hh %>%
        filter(Source != ele_link_val)
      ctr <- ctr + 1
    }
    
    
    if (!is_empty(links.hh$Source)) {
      if (!is_empty(ele$Name)) {
        ctr2 <- n_src - 1
      } else {
        ctr2 <- n_src
      }
      l <- 0
      for (k in (nrow(links.h) + 1):(nrow(links.h) + ctr2)) {
        l <- l + 1
        links.h[k, 'No'] <- k
        links.h[k, 'Target'] <- links.hh[l, 'Source']
        links.h[k, 'Source'] <- 1
        links.h[k, 'Value'] <- links.hh[l, 'Value']
      }
      
      links.hh2 <- links.h %>%
        group_by(Source) %>%
        summarise(Value = sum(Value)) %>%
        filter(Source == 1)
      ctr <- ctr + 1
    }
    
    links.fe <- rbind(links.hh2, ele_link)
    
    ene <- nodes %>%
      filter(Name == 'Energy')
    
    ene_link_val <- as.numeric(ene$No - 1)
    
    p <- 0
    
    
    for (m in (nrow(links.h) + 1):(nrow(links.h) + ctr)) {
      p <- p + 1
      links.h[m, 'No'] <- m
      links.h[m, 'Target'] <- links.fe[p, 'Source']
      links.h[m, 'Source'] <- ene_link_val
      links.h[m, 'Value'] <- links.fe[p, 'Value']
    }
    
    aa.ne <- aa %>%
      filter(energy_or_emissions_category != "Energy")
    
    
    if (!is_empty(aa.ne$source)) {
      o <- 0
      for (q in (nrow(links.h) + 1):(nrow(links.h) + nrow(aa.ne))) {
        o <- o + 1
        links.h[q, 'No'] <- q
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'source'] == nodes[j, 'Name']) {
            links.h[q, 'Target'] <- nodes[[j, 'No']] - 1
          }
        }
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'energy_or_emissions_category'] == nodes[j, 'Name']) {
            links.h[q, 'Source'] <- nodes[[j, 'No']] - 1
          }
        }
        if (input$perc == "Percentage") {
          links.h[q, 'Value'] <- aa.ne[o, 'percentage_of_total_emissions'] * 100
        } else {
          links.h[q, 'Value'] <- aa.ne[o, 'co2e_emissions_mt_co2e_yr']
        }
      }
    }
    
    
    pr_link_val <- numeric(0)
    fg_link_val <- numeric(0)
    ac_link_val <- numeric(0)
    
    pr <- nodes %>%
      filter(Name == 'Process')
    fg <- nodes %>%
      filter(Name == 'Fugitive')
    ac <- nodes %>%
      filter(Name == 'Avoided Emissions')
    filter_criteria <- c()
    
    if (!is_empty(pr$No)) {
      pr_link_val <- as.numeric(pr$No - 1)
      filter_criteria <- c(filter_criteria, pr_link_val)
    }
    
    if (!is_empty(fg$No)) {
      fg_link_val <- as.numeric(fg$No - 1)
      filter_criteria <- c(filter_criteria, fg_link_val)
    }
    
    if (!is_empty(ac$No)) {
      ac_link_val <- as.numeric(ac$No - 1)
      filter_criteria <- c(filter_criteria, ac_link_val)
    }
    
    # Always include ene_link_val
    filter_criteria <- c(filter_criteria, ene_link_val)
    
    # Apply the filter and summarise
    links.t <- links.h %>%
      filter(Source %in% filter_criteria) %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    
    total_fields <- as.numeric(!is_empty(pr_link_val)) + as.numeric(!is_empty(ene_link_val)) +
      as.numeric(!is_empty(fg_link_val)) + as.numeric(!is_empty(ac_link_val))
    
    
    
    
    v <- 0
    for (m in (nrow(links.h) + 1):(nrow(links.h) + total_fields)) {
      v <- v + 1
      links.h[m, 'No'] <- m
      links.h[m, 'Target'] <- links.t[v, 'Source']
      links.h[m, 'Source'] <- 0
      links.h[m, 'Value'] <- links.t[v, 'Value']
    }
    
    
    links <- links.h
    links <- links %>%
      mutate(
        Value = round(Value * units_conversion(), input$precision),
        label = paste0(Source, " → ", Target, ": ", Value)
      ) %>%
      arrange(Source)
    links
    
  })
  
  links_data_energy_costs <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(energy_source))
    
    aa$percentage_of_total_energy_costs = aa$total_energy_costs_yr / sum(aa$total_energy_costs_yr)
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total Energy Costs'
    non_ele <- ene.src %>%
      filter(Name != 'Electricity')
    
    fuel_costs_link_val <- numeric(0)
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel Costs'
      fuel_costs_link_val = 1
    }
    
    nodes.h <- rbind(nodes.hh, ene.src, end.use)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    
    links.h <- tibble(
      'No' = 0,
      'Source' = 0,
      'Target' = 0,
      'Value' = 0
    )
    
    aa.e <- aa
    
    for (i in 1:nrow(aa.e)) {
      links.h[i, 'No'] <- i
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'source'] == nodes[j, 'Name']) {
          links.h[i, 'Target'] <- nodes[[j, 'No']] - 1
        }
      }
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'energy_source'] == nodes[j, 'Name']) {
          links.h[i, 'Source'] <- nodes[[j, 'No']] - 1
        }
      }
      
      if (input$perc_ec == "Percentage") {
        links.h[i, 'Value'] <- aa.e[i, 'percentage_of_total_energy_costs'] * 100
      } else {
        links.h[i, 'Value'] <- aa.e[i, 'total_energy_costs_yr']
      }
      
    }
    
    links.hh <- links.h %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    ctr <- 0
    
    links.hh2 <- tibble(Source = c(), Value = c())
    ele_link <- tibble(Source = c(), Value = c())
    ele <- nodes %>%
      filter(Name == 'Electricity')
    
    if (!is_empty(ele$No)) {
      ele_link_val <- as.numeric(ele$No - 1)
      ele_link <- links.hh %>%
        filter(Source == ele_link_val)
      
      links.hh <- links.hh %>%
        filter(Source != ele_link_val)
      ctr <- ctr + 1
    }
    
    
    if (!is_empty(links.hh$Source)) {
      if (!is_empty(ele$Name)) {
        ctr2 <- n_src - 1
      } else {
        ctr2 <- n_src
      }
      l <- 0
      for (k in (nrow(links.h) + 1):(nrow(links.h) + ctr2)) {
        l <- l + 1
        links.h[k, 'No'] <- k
        links.h[k, 'Target'] <- links.hh[l, 'Source']
        links.h[k, 'Source'] <- 1
        links.h[k, 'Value'] <- links.hh[l, 'Value']
      }
      
      links.hh2 <- links.h %>%
        group_by(Source) %>%
        summarise(Value = sum(Value)) %>%
        filter(Source == 1)
      ctr <- ctr + 1
    }
    
    links.fe <- rbind(links.hh2, ele_link)
    
    aa.ne <- aa %>%
      filter(energy_or_emissions_category != "Energy")
    
    
    if (!is_empty(aa.ne$source)) {
      o <- 0
      for (q in (nrow(links.h) + 1):(nrow(links.h) + nrow(aa.ne))) {
        o <- o + 1
        links.h[q, 'No'] <- q
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'source'] == nodes[j, 'Name']) {
            links.h[q, 'Target'] <- nodes[[j, 'No']] - 1
          }
        }
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'energy_or_emissions_category'] == nodes[j, 'Name']) {
            links.h[q, 'Source'] <- nodes[[j, 'No']] - 1
          }
        }
        if (input$perc_ec == "Percentage") {
          links.h[q, 'Value'] <- aa.ne[o, 'percentage_of_total_energy_costs'] * 100
        } else {
          links.h[q, 'Value'] <- aa.ne[o, 'total_energy_costs_yr']
        }
      }
    }
    
    # Create a vector of filtering criteria based on conditions
    filter_criteria <- c()
    
    if (!is_empty(ele_link_val)) {
      filter_criteria <- c(filter_criteria, ele_link_val)
    }
    
    if (!is_empty(fuel_costs_link_val)) {
      filter_criteria <- c(filter_criteria, fuel_costs_link_val)
    }
    
    
    # Apply the filter and summarise
    links.t <- links.h %>%
      filter(Source %in% filter_criteria) %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    
    
    total_fields <- as.numeric(!is_empty(fuel_costs_link_val)) + as.numeric(!is_empty(ele_link_val))
    
    v <- 0
    for (m in (nrow(links.h) + 1):(nrow(links.h) + total_fields)) {
      v <- v + 1
      links.h[m, 'No'] <- m
      links.h[m, 'Target'] <- links.t[v, 'Source']
      links.h[m, 'Source'] <- 0
      links.h[m, 'Value'] <- links.t[v, 'Value']
    }
    
    
    links <- links.h
    links <- links %>%
      mutate(
        Value = round(Value , input$precision_ec),
        label = paste0(Source, " → ", Target, ": ", Value)
      ) %>%
      arrange(Source)
    links
    
  })
  
  
  
  
  links_data_energy <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(energy_source))
    aa$percentage_of_total_energy = aa$total_energy_mm_btu_yr / sum(aa$total_energy_mm_btu_yr)
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    ene.src <- na.omit(ene.src)
    n_src <- nrow(ene.src)
    nodes.hh <- tibble("Name" = "")
    nodes.hh[1, 'Name'] <- 'Total Energy'
    non_ele <- ene.src %>%
      filter(Name != 'Electricity')
    
    fuel_link_val <- numeric(0)
    if (!is_empty(non_ele$Name)) {
      nodes.hh[2, 'Name'] <- 'Fuel'
      fuel_link_val = 1
    }
    
    aa.ne <- aa %>%
      filter(energy_or_emissions_category == "Conserved Energy")
    
    ce_link_val <- numeric(0)
    if (!is_empty(aa.ne$no)) {
      nodes.hh[3, 'Name'] <- 'Conserved Energy'
      ce_link_val = 2
    }
    
    nodes.h <- rbind(nodes.hh, ene.src, end.use)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    
    links.h <- tibble(
      'No' = 0,
      'Source' = 0,
      'Target' = 0,
      'Value' = 0
    )
    
    aa.e <- aa %>%
      filter(energy_or_emissions_category == "Energy")
    
    for (i in 1:nrow(aa.e)) {
      links.h[i, 'No'] <- i
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'source'] == nodes[j, 'Name']) {
          links.h[i, 'Target'] <- nodes[[j, 'No']] - 1
        }
      }
      for (j in 1:nrow(nodes)) {
        if (aa.e[i, 'energy_source'] == nodes[j, 'Name']) {
          links.h[i, 'Source'] <- nodes[[j, 'No']] - 1
        }
      }
      
      if (input$perc_e == "Percentage") {
        links.h[i, 'Value'] <- aa.e[i, 'percentage_of_total_energy'] * 100
      } else {
        links.h[i, 'Value'] <- aa.e[i, 'total_energy_mm_btu_yr']
      }
      
    }
    
    links.hh <- links.h %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    ctr <- 0
    
    links.hh2 <- tibble(Source = c(), Value = c())
    ele_link <- tibble(Source = c(), Value = c())
    ele <- nodes %>%
      filter(Name == 'Electricity')
    
    if (!is_empty(ele$No)) {
      ele_link_val <- as.numeric(ele$No - 1)
      ele_link <- links.hh %>%
        filter(Source == ele_link_val)
      
      links.hh <- links.hh %>%
        filter(Source != ele_link_val)
      ctr <- ctr + 1
    }
    
    
    if (!is_empty(links.hh$Source)) {
      if (!is_empty(ele$Name)) {
        ctr2 <- n_src - 1
      } else {
        ctr2 <- n_src
      }
      l <- 0
      for (k in (nrow(links.h) + 1):(nrow(links.h) + ctr2)) {
        l <- l + 1
        links.h[k, 'No'] <- k
        links.h[k, 'Target'] <- links.hh[l, 'Source']
        links.h[k, 'Source'] <- 1
        links.h[k, 'Value'] <- links.hh[l, 'Value']
      }
      
      links.hh2 <- links.h %>%
        group_by(Source) %>%
        summarise(Value = sum(Value)) %>%
        filter(Source == 1)
      ctr <- ctr + 1
    }
    
    links.fe <- rbind(links.hh2, ele_link)
    
    
    if (!is_empty(aa.ne$source)) {
      o <- 0
      for (q in (nrow(links.h) + 1):(nrow(links.h) + nrow(aa.ne))) {
        o <- o + 1
        links.h[q, 'No'] <- q
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'source'] == nodes[j, 'Name']) {
            links.h[q, 'Target'] <- nodes[[j, 'No']] - 1
          }
        }
        for (j in 1:nrow(nodes)) {
          if (aa.ne[o, 'energy_or_emissions_category'] == nodes[j, 'Name']) {
            links.h[q, 'Source'] <- nodes[[j, 'No']] - 1
          }
        }
        if (input$perc_e == "Percentage") {
          links.h[q, 'Value'] <- aa.ne[o, 'percentage_of_total_energy'] * 100
        } else {
          links.h[q, 'Value'] <- aa.ne[o, 'total_energy_mm_btu_yr']
        }
      }
    }
    
    
    # Create a vector of filtering criteria based on conditions
    filter_criteria <- c()
    
    if (!is_empty(ele_link_val)) {
      filter_criteria <- c(filter_criteria, ele_link_val)
    }
    
    if (!is_empty(fuel_link_val)) {
      filter_criteria <- c(filter_criteria, fuel_link_val)
    }
    
    if (!is_empty(aa.ne$no)) {
      filter_criteria <- c(filter_criteria, ce_link_val)
    }
    
    # Apply the filter and summarise
    links.t <- links.h %>%
      filter(Source %in% filter_criteria) %>%
      group_by(Source) %>%
      summarise(Value = sum(Value))
    
    
    total_fields <- as.numeric(!is_empty(fuel_link_val)) + as.numeric(!is_empty(ele_link_val)) + as.numeric(!is_empty(ce_link_val))
    
    v <- 0
    for (m in (nrow(links.h) + 1):(nrow(links.h) + total_fields)) {
      v <- v + 1
      links.h[m, 'No'] <- m
      links.h[m, 'Target'] <- links.t[v, 'Source']
      links.h[m, 'Source'] <- 0
      links.h[m, 'Value'] <- links.t[v, 'Value']
    }
    
    
    links <- links.h
    links <- links %>%
      mutate(
        Value = round(Value * units_conversion_e(), input$precision_e),
        label = paste0(Source, " → ", Target, ": ", Value)
      ) %>%
      arrange(Source)
    links
    
  })
  
  
  output$move <- renderText("Note: click and drag each node to
                            customize the chart \n")
  
  # Create the Sankey diagram
  s1 <- reactive({
    nodes <- nodes_data()
    links <- links_data()
    names(nodes) <- c('SN', "Name")
    names(links) <- c('SN', "Source", "Target", "Value", "label")
    
    
    sankey_reactive <- reactive({
      sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        iterations = 10,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
    })
    
    sankey <- sankey_reactive()
    
    javascript_string <-
      'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
    
    htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
  })
  
  
  s1_energy <- reactive({
    nodes <- nodes_data_energy()
    links <- links_data_energy()
    names(nodes) <- c('SN', "Name")
    names(links) <- c('SN', "Source", "Target", "Value", "label")
    
    sankey_reactive <- reactive({
      sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        iterations = 10,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
    })
    
    sankey <- sankey_reactive()
    
    javascript_string <-
      'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
    
    htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
  })
  
  s1_energy_costs <- reactive({
    nodes <- nodes_data_energy_costs()
    links <- links_data_energy_costs()
    names(nodes) <- c('SN', "Name")
    names(links) <- c('SN', "Source", "Target", "Value", "label")
    
    sankey_reactive <- reactive({
      sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        iterations = 10,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
    })
    
    sankey <- sankey_reactive()
    
    javascript_string <-
      'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
    
    htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
  })
  
  
  
  output$sankey <- renderSankeyNetwork(s1())
  
  output$diagram <- renderUI({
    temp <- temp ()
    nr <- nrow(temp) * input$vsc
    ht <- paste0(nr, "px")
    sankeyNetworkOutput("sankey", height = ht)
  })
  
  output$sankey_energy <- renderSankeyNetwork(s1_energy())
  
  output$diagram_energy <- renderUI({
    temp_e <- temp_e ()
    nr <- nrow(temp_e) * input$vsc_e
    ht <- paste0(nr, "px")
    sankeyNetworkOutput("sankey_energy", height = ht)
  })
  
  output$sankey_energy_costs <- renderSankeyNetwork(s1_energy_costs())
  
  output$diagram_energy_costs <- renderUI({
    temp_ec <- temp_ec ()
    nr <- nrow(temp_ec) * input$vsc_ec
    ht <- paste0(nr, "px")
    sankeyNetworkOutput("sankey_energy_costs", height = ht)
  })
  
  output$output_text <- renderUI({
    req(input$file)
    if (nchar(input$cname) > 0 & input$perc != "Percentage") {
      paste0("CO₂e Flow for ", input$cname, " (" , input$units, ")")
    } else if (nchar(input$cname) > 0 &
               input$perc == "Percentage") {
      paste0("CO₂e Flow for ", input$cname, " (%)")
    } else if (input$perc == "Percentage") {
      paste0("CO₂e Flow ", "(%)")
    } else {
      paste0("CO₂e Flow ", "(" , input$units, ")")
    }
    
  })
  
  
  output$output_text_e <- renderUI({
    req(input$file)
    if (nchar(input$cname) > 0 & input$perc_e != "Percentage") {
      paste0("Energy Flow for ", input$cname, " (" , input$units_e, ")")
    } else if (nchar(input$cname) > 0 &
               input$perc_e == "Percentage") {
      paste0("Energy Flow for ", input$cname, " (%)")
    } else if (input$perc_e == "Percentage") {
      paste0("Energy Flow ", "(%)")
    } else {
      paste0("Energy Flow ", "(" , input$units_e, ")")
    }
  })
  
  output$output_text_ec <- renderUI({
    req(input$file)
    if (nchar(input$cname) > 0 & input$perc_e != "Percentage") {
      paste0("Energy Costs Flow for ", input$cname, " ($)")
    } else if (nchar(input$cname) > 0 &
               input$perc_e == "Percentage") {
      paste0("Energy Costs Flow for ", input$cname, " (%)")
    } else if (input$perc_e == "Percentage") {
      paste0("Energy Costs Flow (%)")
    } else {
      paste0("Energy Costs Flow ($)")
    }
  })
  
  output$downloadPNG <- downloadHandler(
    filename = "CO2e Flow.png",
    content = function(file) {
      # Create a temporary HTML file to save the widget in
      tmp_file <- tempfile(fileext = ".html")
      nodes <- nodes_data()
      links <- links_data()
      names(nodes) <- c("SN", "Name")
      names(links) <- c("SN", "Source", "Target", "Value", "label")
      sankey <- sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
      
      
      javascript_string <-
        'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
      sankey <- htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
      # Save the widget to the temporary HTML file
      saveWidget(sankey, tmp_file)
      
      # Take a screenshot of the HTML file and save it to the output file
      x <- tempfile(fileext = ".png")
      webshot(
        tmp_file,
        x,
        zoom = 5,
        vwidth = input$width,
        vheight = input$height,
        delay = 0.2
      )
      # 1read the image file into R
      img1 <- image_read(x)
      # add the user's caption as a text label
      if (input$units == "MT CO₂e/yr") {
        un <- "MT CO2e/yr" # Conversion factor
      } else {
        un <- "lbs. of CO2e/yr"
      }
      
      if (nchar(input$cname) > 0) {
        caption <- paste0("CO2e Flow for ", input$cname, "(" , un, ")")
      } else {
        caption <- paste0("CO2e Flow ", "(" , un, ")")
      }
      img <- image_annotate(
        img1,
        caption,
        size = 100,
        color = "black",
        gravity = "North",
        location = "+0+10%"
      )
      # write the annotated image to file
      image_write(img, path = file)
      # Delete the temporary file
      unlink(tmp_file)
      unlink(x)
    }
  )
  
  output$downloadPNG_e <- downloadHandler(
    filename = "Energy Flow.png",
    content = function(file) {
      # Create a temporary HTML file to save the widget in
      tmp_file <- tempfile(fileext = ".html")
      nodes <- nodes_data_energy()
      links <- links_data_energy()
      names(nodes) <- c("SN", "Name")
      names(links) <- c("SN", "Source", "Target", "Value", "label")
      sankey <- sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
      
      
      javascript_string <-
        'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
      sankey <- htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
      # Save the widget to the temporary HTML file
      saveWidget(sankey, tmp_file)
      
      # Take a screenshot of the HTML file and save it to the output file
      x <- tempfile(fileext = ".png")
      webshot(
        tmp_file,
        x,
        zoom = 5,
        vwidth = input$width,
        vheight = input$height,
        delay = 0.2
      )
      # 1read the image file into R
      img1 <- image_read(x)
      # add the user's caption as a text label
      
      if (nchar(input$cname) > 0) {
        caption <- paste0("Energy Flow for ", input$cname, "(" , input$units_e, ")")
      } else {
        caption <- paste0("Energy Flow ", "(" , input$units_e, ")")
      }
      img <- image_annotate(
        img1,
        caption,
        size = 100,
        color = "black",
        gravity = "North",
        location = "+0+10%"
      )
      # write the annotated image to file
      image_write(img, path = file)
      # Delete the temporary file
      unlink(tmp_file)
      unlink(x)
    }
  )
  
  output$downloadPNG_ec <- downloadHandler(
    filename = "Energy Costs Flow.png",
    content = function(file) {
      # Create a temporary HTML file to save the widget in
      tmp_file <- tempfile(fileext = ".html")
      nodes <- nodes_data_energy_costs()
      links <- links_data_energy_costs()
      names(nodes) <- c("SN", "Name")
      names(links) <- c("SN", "Source", "Target", "Value", "label")
      sankey <- sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "Source",
        Target = "Target",
        Value = "Value",
        NodeID = "Name",
        LinkGroup = "label",
        sinksRight = F,
        fontSize = 14,
        nodeWidth = 30,
        colourScale = JS("d3.scaleSequential(d3.interpolatePlasma);")
      )
      
      
      javascript_string <-
        'function(el, x) {
  d3.select(el).selectAll(".node text")
    .text(function(d) {
      var value_str = d.value.toLocaleString();
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });

  // Clear the viewBox attribute of the first SVG element
  document.getElementsByTagName("svg")[0].setAttribute("viewBox", "");
}'
      sankey <- htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
      # Save the widget to the temporary HTML file
      saveWidget(sankey, tmp_file)
      
      # Take a screenshot of the HTML file and save it to the output file
      x <- tempfile(fileext = ".png")
      webshot(
        tmp_file,
        x,
        zoom = 5,
        vwidth = input$width,
        vheight = input$height,
        delay = 0.2
      )
      # 1read the image file into R
      img1 <- image_read(x)
      # add the user's caption as a text label
      
      if (nchar(input$cname) > 0) {
        caption <- paste0("Energy Costs Flow for ", input$cname, "($)")
      } else {
        caption <- paste0("Energy Costs Flow ($)")
      }
      img <- image_annotate(
        img1,
        caption,
        size = 100,
        color = "black",
        gravity = "North",
        location = "+0+10%"
      )
      # write the annotated image to file
      image_write(img, path = file)
      # Delete the temporary file
      unlink(tmp_file)
      unlink(x)
    }
  )
  
  
  observeEvent(input$calc_int, {
    req(input$file)
    num <- input$products_num
    end.use <- end_use$measures
    
    
    if (input$selected_method == "Quantity-based") {
      for (i in 1:num) {
        exists <- input[[paste0("qty_", i)]] > 0
        shinyFeedback::feedbackWarning(paste0("qty_", i),
                                       !exists,
                                       "Please enter quantity values")
        req(exists)
      }
    }
    
    if (input$selected_method == "Revenue-based") {
      for (i in 1:num) {
        exists <- input[[paste0("qty_", i)]] > 0
        shinyFeedback::feedbackWarning(paste0("qty_", i),
                                       !exists,
                                       "Please enter quantity values")
        req(exists)
      }
      
      for (i in 1:num) {
        if (input$revenue_name == "Revenue %") {
          exists_r <- input[[paste0("revenue_", i)]] > 0
          shinyFeedback::feedbackWarning(paste0("revenue_", i),
                                         !exists_r,
                                         "Please enter revenue %")
          req(exists_r)
        } else if (input$revenue_name == "Gross product revenue ($)") {
          exists_r <- input[[paste0("revenue_", i)]] > 0
          shinyFeedback::feedbackWarning(paste0("revenue_", i),
                                         !exists_r,
                                         "Please enter revenue %")
          req(exists_r)
        } else if (input$revenue_name == "$/unit product") {
          exists_r <- input[[paste0("revenue_", i)]] > 0
          shinyFeedback::feedbackWarning(paste0("revenue_", i),
                                         !exists_r,
                                         "Please enter revenue %")
          req(exists_r)
        }
      }
    }
    
    
    aa <- read_excel(input$file$datapath, sheet = 'Results', range = "a6:o189")
    
    aa <- clean_names(aa)
    
    aa <- aa %>%
      filter(!is.na(source)) %>%
      select(
        source,
        total_energy_mm_btu_yr,
        co2e_emissions_mt_co2e_yr,
        total_energy_costs_yr,
        energy_source
      )
    
    if (input$energy_units_int == "MWh") {
      aa <- aa %>%
        mutate(total_energy_mm_btu_yr = total_energy_mm_btu_yr * 0.293071)
    }
    
    aa <- aa
    
    product_dataframe <- function(i) {
      product_data <- aa %>%
        filter(source %in% input[[paste0("process_", i)]])
      
      return(product_data)
    }
    
    sentences <- reactiveVal(character(0))
    
    if (input$selected_method == "Quantity-based") {
      all_product_dfs <- list()
      qty_values <- numeric(num)
      
      for (i in 1:num) {
        df_name <- paste0("product_data_", i, "_df")
        df <- product_dataframe(i)
        
        assign(df_name, df, envir = .GlobalEnv)
        
        all_product_dfs[[df_name]] <- df
        
        ##quantity
        df_qty <- paste0("product_", i, "_qty")
        qty_value <- input[[paste0("qty_", i)]]
        assign(df_qty, qty_value, envir = .GlobalEnv)
      }
      
      assign("dataframes", all_product_dfs, envir = .GlobalEnv)
      
      qty_vector <- numeric(num)
      
      # Calculate process percentages
      for (i in 1:num) {
        df_name <- paste0("product_data_", i, "_df")
        df <- product_dataframe(i)
        
        assign(df_name, df, envir = .GlobalEnv)
        
        all_product_dfs[[df_name]] <- df
        
        
        ##quantity
        df_qty <- paste0("product_", i, "_qty")
        qty_value <- input[[paste0("qty_", i)]]
        assign(df_qty, qty_value, envir = .GlobalEnv)
        qty_vector[i] <- qty_value
      }
      
      # Create named vectors
      names(qty_vector) <- paste0("product_", 1:num, "_qty")
      
      # Assign vectors to global environment
      assign("product_qties", qty_vector, envir = .GlobalEnv)
      
      # Initialize an empty list to store individual product dataframes
      product_breakdowns <- list()
      
      for (i in seq_along(dataframes)) {
        df <- dataframes[[i]]
        
        results <- sapply(
          df$source,
          check_presence,
          dataframes = dataframes,
          current_index = i
        )
        
        product_df <- data.frame(
          product_number = i,
          source = df$source,
          presence = results,
          energy_source = df$energy_source,
          total_energy_mm_btu_yr = df$total_energy_mm_btu_yr,
          co2e_emissions_mt_co2e_yr = df$co2e_emissions_mt_co2e_yr,
          total_energy_costs_yr = df$total_energy_costs_yr,
          stringsAsFactors = FALSE
        )
        
        product_df <- product_df############################
        
        # Calculate quantity-based and revenue-based emissions
        product_df$qty_based_energy <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$total_energy_mm_btu_yr,
          MoreArgs = list(values = product_qties)
        )
        
        product_df <- product_df############################
        
        # Calculate quantity-based and revenue-based emissions
        product_df$qty_based_emissions <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$co2e_emissions_mt_co2e_yr,
          MoreArgs = list(values = product_qties)
        )
        
        product_df <- product_df############################
        
        product_df$product_qty <- product_qties[i]
        product_df$product_name <- input[[paste0("products_name_", i)]]
        product_df$product_unit <- input[[paste0("units_name_", i)]]
        
        # Calculate emissions intensities
        product_df$qty_based_en_intensity <- product_df$qty_based_energy / product_qties[i]
        product_df$qty_based_em_intensity <- product_df$qty_based_emissions / product_qties[i]
        
        # Store the dataframe in the list
        product_breakdowns[[i]] <- product_df
        
        # Assign individual dataframe to global environment
        assign(paste0("product_", i, "_breakdown"),
               product_df,
               envir = .GlobalEnv)
      }
      # }
      
      # Combine all product dataframes into a single dataframe
      all_products_breakdown <- do.call(rbind, product_breakdowns)
      
      # Assign the combined dataframe to the global environment
      assign("all_products_breakdown",
             all_products_breakdown,
             envir = .GlobalEnv)
      
      #Get the mass or revenue based ratio from energy breakdown and apply against costs
      all_products_breakdown <- all_products_breakdown %>%
        mutate(
          qty_based_energy_costs = (qty_based_energy / total_energy_mm_btu_yr) * total_energy_costs_yr,
          energy_cost_based_intensity = qty_based_energy_costs / product_qty
        ) %>%
        relocate(qty_based_energy_costs, .after = qty_based_energy)
      
      if (exists("all_products_breakdown")) {
        all_products_breakdown <- `rownames<-`(all_products_breakdown, NULL)
        all_products_breakdown <- all_products_breakdown %>%
          relocate(product_qty, .after = product_number) %>%
          relocate(product_unit, .after = product_qty) %>%
          relocate(product_name, .before = product_qty)
        
        all_products_summarized <- all_products_breakdown %>%
          group_by(product_name, product_unit) %>%
          summarise(
            total_energy_qty_based_mmbtu_yr = sum(qty_based_energy, na.rm = TRUE),
            qty_based_energy_intensity_mmbtu_ton = sum(qty_based_en_intensity, na.rm = TRUE),
            total_energy_costs = sum(qty_based_energy_costs, na.rm = TRUE),
            energy_costs_intensity_dollar_qty = sum(energy_cost_based_intensity, na.rm = TRUE),
            total_emissions_qty_based_mtco2e_yr = sum(qty_based_emissions, na.rm = TRUE),
            qty_based_emission_intensity_mtco2e_ton = sum(qty_based_em_intensity, na.rm = TRUE)
          )
        
        #Summarize all_products_breakdown by energy costs of each energy source types, and make the dataframe wider to be able to index in the
        #calculated_sentences dataframe below
        energy_costs_summarized <- all_products_breakdown %>%
          group_by(product_name, energy_source) %>%
          summarise(by_source_energy_costs_intensity = sum(energy_cost_based_intensity)) %>%
          pivot_wider(names_from = "energy_source", values_from = "by_source_energy_costs_intensity")
        
        calculated_sentences_en <- c(
          paste0(
            "Based on the user-selected ",
            strong("Quantity-based approach,"),
            " the amount of ",
            strong("Energy"),
            " (in ",
            input$energy_units_int,
            ") required to produce a unit of product is shown below: <br>"
          ),
          sapply(1:num, function(i) {
            product_value <- all_products_summarized$qty_based_energy_intensity_mmbtu_ton[i]
            product_name <- all_products_summarized$product_name[i]
            paste0(
              strong(product_name),
              ": ",
              tags$u(
                paste0(
                  scales::comma(product_value, accuracy = 0.01),
                  " ",
                  input$energy_units_int,
                  " energy"
                )
              ),
              " is required to produce ",
              tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
            )
          })
        )
        
        formatted_sentences_en <- format_sentences(calculated_sentences_en)
        
        calculated_sentences_ec_0 <- vector("list", num)
        
        for (i in 1:num) {
          # Product information part
          product_value <- all_products_summarized$energy_costs_intensity_dollar_qty[i]
          unit_name <- input[[paste0("units_name_", i)]]
          product_name <- all_products_summarized$product_name[i]
          
          product_part <- paste0(
            strong(product_name),
            ": ",
            tags$u(paste0("$", scales::comma(product_value, accuracy = 0.01))),
            " in energy costs is required to produce ",
            tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
          )
          
          # Get all energy sources and their values at once
          energy_values <- as.numeric(energy_costs_summarized[i, 2:ncol(energy_costs_summarized)])
          energy_sources <- colnames(energy_costs_summarized)[2:ncol(energy_costs_summarized)]
          
          # Create all energy source phrases in one vector
          energy_phrases <-
            if_else(
              is.na(energy_values) != T,
              paste0(
                energy_sources,
                "-based intensity is ",
                "$",
                scales::comma(energy_values, accuracy = 0.01),
                " ",
                tags$u(paste0("per ", unit_name))
              ),
              NA_character_
            )
          
          # Combine all energy phrases with commas
          energy_part <- paste(na.omit(energy_phrases), collapse = ", ")
          
          # Final sentence combining product info and all energy sources
          calculated_sentences_ec_0[[i]] <- paste0(product_part, ". ", energy_part, ".")
        }
        
        formatted_sentences_ec_0 <- format_sentences_bullet(calculated_sentences_ec_0)
        
        calculated_sentences_ec_1 <- c(
          if_else(
            input$en == FALSE &&
              input$ec == TRUE &&
              input$em == FALSE ||
              input$en == FALSE &&
              input$ec == TRUE && input$em == TRUE,
            paste0(
              "Based on the user-selected ",
              strong("Quantity-based approach"),
              ", the amount of ",
              strong("Energy Costs"),
              " required to produce a unit of product is shown below:"
            ),
            paste0(
              "The ",
              strong("Energy Costs"),
              " required to produce a unit of product is shown below:"
            )
          )
        )
        
        formatted_sentences_ec <- HTML(format_sentences(calculated_sentences_ec_1),
                                       formatted_sentences_ec_0)
        
        
        calculated_sentences_em <- c(
          if_else(
            input$en == FALSE && input$ec == FALSE && input$em == TRUE,
            paste0(
              "Based on the user-selected ",
              strong("Quantity-based approach"),
              ", the amount of ",
              tags$u("CO₂e (in metric ton)"),
              " emitted through the manfufacturing of a unit product is shown below: <br>"
            )
            ,
            paste0(
              "The amount of ",
              strong("CO₂e emissions"),
              " (in metric ton) emitted from a unit product is shown below:<br>"
            )
          ),
          sapply(1:num, function(i) {
            product_value <- all_products_summarized$qty_based_emission_intensity_mtco2e_ton[i]
            product_name <- all_products_summarized$product_name[i]
            paste0(
              strong(product_name),
              ": ",
              tags$u(paste0(
                scales::comma(product_value, accuracy = 0.001), " metric ton of CO₂e"
              )),
              " is generated to produce ",
              tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
            )
          })
        )
        
        formatted_sentences_em <- format_sentences(calculated_sentences_em)
        
        if (input$en == TRUE &&
            input$ec == FALSE && input$em == FALSE) {
          formatted_sentences <- paste0(formatted_sentences_en)
          
          all_products_summarized <- all_products_summarized %>%
            select(product_name:qty_based_energy_intensity_mmbtu_ton) %>%
            rename("Product Name" = product_name,
                   "Product Unit" = product_unit) %>%
            rename_with(
              ~ paste0(
                "Quantity-based\n Energy Consumption\n(",
                input$energy_units_int,
                ")"
              ),
              .cols = total_energy_qty_based_mmbtu_yr
            ) %>%
            rename_with(
              ~ paste0(
                "Energy Intensity\n(",
                input$energy_units_int,
                "/unit of Product)"
              ),
              .cols = qty_based_energy_intensity_mmbtu_ton
            )
          
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == FALSE) {
          formatted_sentences <- paste0(formatted_sentences_ec)
          
          all_products_summarized <- all_products_summarized %>%
            select(
              product_name,
              product_unit,
              total_energy_costs,
              energy_costs_intensity_dollar_qty
            ) %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Energy Costs\n($)" = total_energy_costs,
              "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_dollar_qty
            )
          
        } else if (input$en == FALSE &&
                   input$ec == FALSE && input$em == TRUE) {
          formatted_sentences <- paste0(formatted_sentences_em)
          
          all_products_summarized <- all_products_summarized %>%
            select(
              product_name,
              product_unit,
              total_emissions_qty_based_mtco2e_yr,
              qty_based_emission_intensity_mtco2e_ton
            ) %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Emissions\n(MTCO₂e)" = total_emissions_qty_based_mtco2e_yr,
              "Emissions Intensity\n(MTCO₂e/unit of Product)" = qty_based_emission_intensity_mtco2e_ton
            )
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == FALSE) {
          formatted_sentences_en <- paste0(formatted_sentences_en)                            
          formatted_sentences_ec <- paste0(formatted_sentences_ec)
          
          all_products_summarized <- all_products_summarized %>%
            select(
              product_name:qty_based_energy_intensity_mmbtu_ton,
              total_energy_costs,
              energy_costs_intensity_dollar_qty
            ) %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Energy Costs\n($)" = total_energy_costs,
              "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_dollar_qty
            ) %>%
            rename_with(
              ~ paste0(
                "Quantity-based\n Energy Consumption\n(",
                input$energy_units_int,
                ")"
              ),
              .cols = total_energy_qty_based_mmbtu_yr
            ) %>%
            rename_with(
              ~ paste0(
                "Energy Intensity\n(",
                input$energy_units_int,
                "/unit of Product)"
              ),
              .cols = qty_based_energy_intensity_mmbtu_ton
            )
          
        } else if (input$en == TRUE &&
                   input$ec == FALSE && input$em == TRUE) {
          
          formatted_sentences_en <- paste0(formatted_sentences_en)                            
          formatted_sentences_em <- paste0(formatted_sentences_em)
          
          all_products_summarized <- all_products_summarized %>%
            select(
              product_name:qty_based_energy_intensity_mmbtu_ton,
              total_emissions_qty_based_mtco2e_yr,
              qty_based_emission_intensity_mtco2e_ton
            ) %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Emissions\n(MTCO₂e)" = total_emissions_qty_based_mtco2e_yr,
              "Emissions Intensity\n(MTCO₂e/unit of Product)" = qty_based_emission_intensity_mtco2e_ton
            ) %>%
            rename_with(
              ~ paste0(
                "Quantity-based\n Energy Consumption\n(",
                input$energy_units_int,
                ")"
              ),
              .cols = total_energy_qty_based_mmbtu_yr
            ) %>%
            rename_with(
              ~ paste0(
                "Energy Intensity\n(",
                input$energy_units_int,
                "/unit of Product)"
              ),
              .cols = qty_based_energy_intensity_mmbtu_ton
            )
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == TRUE) {
          formatted_sentences_em <- paste0(formatted_sentences_em)                            
          formatted_sentences_ec <- paste0(formatted_sentences_ec)
          
          all_products_summarized <- all_products_summarized %>%
            select(
              product_name,
              product_unit,
              total_energy_costs,
              energy_costs_intensity_dollar_qty,
              total_emissions_qty_based_mtco2e_yr,
              qty_based_emission_intensity_mtco2e_ton
            ) %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Energy Costs\n($)" = total_energy_costs,
              "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_dollar_qty,
              "Quantity-based\n Emissions\n(MTCO₂e)" = total_emissions_qty_based_mtco2e_yr,
              "Emissions Intensity\n(MTCO₂e/unit of Product)" = qty_based_emission_intensity_mtco2e_ton
            )
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == TRUE) {
          formatted_sentences_en <- paste0(formatted_sentences_en)                            
          formatted_sentences_ec <- paste0(formatted_sentences_ec)
          formatted_sentences_em <- paste0(formatted_sentences_em) 
          
          all_products_summarized <- all_products_summarized %>%
            rename(
              "Product Name" = product_name,
              "Product Unit" = product_unit,
              "Quantity-based\n Energy Costs\n($)" = total_energy_costs,
              "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_dollar_qty,
              "Quantity-based\n Emissions\n(MTCO₂e)" = total_emissions_qty_based_mtco2e_yr,
              "Emissions Intensity\n(MTCO₂e/unit of Product)" = qty_based_emission_intensity_mtco2e_ton
            ) %>%
            rename_with(
              ~ paste0(
                "Quantity-based\n Energy Consumption\n(",
                input$energy_units_int,
                ")"
              ),
              .cols = total_energy_qty_based_mmbtu_yr
            ) %>%
            rename_with(
              ~ paste0(
                "Energy Intensity\n(",
                input$energy_units_int,
                "/unit of Product)"
              ),
              .cols = qty_based_energy_intensity_mmbtu_ton
            )
        }
        
        names(all_products_breakdown)
        
        download_excel_file <- all_products_breakdown %>%
          mutate(qty_weight_proportion = qty_based_energy / total_energy_mm_btu_yr) %>%
          rename(
            associated_products = presence,
            all_products_energy_consumption_mmbtu_yr = total_energy_mm_btu_yr,
            qty_based_energy_consumption_mmbtu_yr = qty_based_energy,
            qty_based_energy_intensity_mmbtu_per_mt = qty_based_en_intensity,
            all_products_co2e_emissions_mt_co2e_yr = co2e_emissions_mt_co2e_yr,
            qty_based_emissions_mt_co2e_yr = qty_based_emissions,
            qty_based_ei_mt_co2e_per_mt = qty_based_em_intensity,
          ) %>%
          relocate(energy_source, .after = source) %>%
          relocate(total_energy_costs_yr, .after = all_products_energy_consumption_mmbtu_yr) %>%
          relocate(qty_weight_proportion, .after = total_energy_costs_yr) %>%
          relocate(qty_based_energy_consumption_mmbtu_yr, .after = qty_weight_proportion) %>%
          relocate(qty_based_energy_intensity_mmbtu_per_mt, .after = qty_based_energy_consumption_mmbtu_yr) %>%
          relocate(all_products_co2e_emissions_mt_co2e_yr, .after = total_energy_costs_yr) %>%
          relocate(energy_cost_based_intensity, .after = qty_based_energy_costs)
        
        names(download_excel_file)
        
        
        colnames(download_excel_file) <-  c(
          "Product",
          "Product Name",
          "Quantity",
          "Unit",
          "Source",
          "Energy Source",
          "Associated Products",
          paste0(
            "Total Energy\n Consumption\n(",
            input$energy_units_int,
            ")"
          ),
          "Total Energy\n Costs\n($)",
          "Total Emissions\n(MTCO₂e)",
          "Share of Mass-based\nEnergy Consumption",
          paste0(
            "Product-level\n Energy Consumption \n(",
            input$energy_units_int,
            ")"
          ),
          paste0(
            "Mass-based\n Energy Intensity\n(",
            input$energy_units_int,
            "/unit of Product",
            ")"
          ),
          "Product-level\n Energy Costs \n($)",
          paste0(
            "Mass-based\n Energy Costs Intensity\n($/",
            "unit of Product",
            ")"
          ),
          "Product-level\n Emissions\n(MTCO₂e)",
          paste0(
            "Mass-Based\n Emissions Intensity\n(MTCO₂e/",
            "unit of Product)"
          )
        )
      }
      
      #}
    } else  {
      #Revenue based approach
      
      all_product_dfs <- list()
      qty_values <- numeric(num)
      revenue_values <- numeric(num)
      
      for (i in 1:num) {
        df_name <- paste0("product_data_", i, "_df")
        df <- product_dataframe(i)
        
        assign(df_name, df, envir = .GlobalEnv)
        all_product_dfs[[df_name]] <- df
        
        
        ##Quantity
        df_qty <- paste0("product_", i, "_qty")
        qty_value <- input[[paste0("qty_", i)]]
        assign(df_qty, qty_value, envir = .GlobalEnv)
        
        ##Revenue
        df_revenue <- paste0("product_", i, "_revenue")
        rev_value <- if_else(
          input$revenue_name == "Revenue %",
          input[[paste0("revenue_", i)]] / 100,
          if_else(
            input$revenue_name == "Gross product revenue ($)",
            input[[paste0("revenue_", i)]],
            input[[paste0("revenue_", i)]] * qty_value
          )
        )
        assign(df_revenue, rev_value , envir = .GlobalEnv)
      }
      
      assign("dataframes", all_product_dfs, envir = .GlobalEnv)
      
      qty_vector <- numeric(num)
      revenue_vector <- numeric(num)
      
      # Calculate process percentages
      for (i in 1:num) {
        df_name <- paste0("product_data_", i, "_df")
        df <- product_dataframe(i)
        
        assign(df_name, df, envir = .GlobalEnv)
        all_product_dfs[[df_name]] <- df
        
        
        ##qty
        df_qty <- paste0("product_", i, "_qty")
        qty_value <- input[[paste0("qty_", i)]]
        assign(df_qty, qty_value, envir = .GlobalEnv)
        qty_vector[i] <- qty_value
        
        ##Revenue
        df_revenue <- paste0("product_", i, "_revenue")
        rev_value <- if_else(
          input$revenue_name == "Revenue %",
          input[[paste0("revenue_", i)]] / 100,
          if_else(
            input$revenue_name == "Gross product revenue ($)",
            input[[paste0("revenue_", i)]],
            input[[paste0("revenue_", i)]] * qty_value
          )
        )
        assign(df_revenue, rev_value , envir = .GlobalEnv)
        revenue_vector[i] <- rev_value
      }
      
      # Create named vectors
      names(qty_vector) <- paste0("product_", 1:num, "_qty")
      names(revenue_vector) <- paste0("product_", 1:num, "_revenue")
      
      # Assign vectors to global environment
      assign("product_qties", qty_vector, envir = .GlobalEnv)
      assign("product_revenues", revenue_vector, envir = .GlobalEnv)
      
      # Initialize an empty list to store individual product dataframes
      product_breakdowns <- list()
      
      for (i in seq_along(dataframes)) {
        df <- dataframes[[i]]
        
        results <- sapply(
          df$source,
          check_presence,
          dataframes = dataframes,
          current_index = i
        )
        
        product_df <- data.frame(
          product_number = i,
          source = df$source,
          presence = results,
          energy_source = df$energy_source,
          total_energy_mm_btu_yr = df$total_energy_mm_btu_yr,
          co2e_emissions_mt_co2e_yr = df$co2e_emissions_mt_co2e_yr,
          total_energy_costs_yr = df$total_energy_costs_yr,
          stringsAsFactors = FALSE
        )
        
        product_df <- product_df
        
        # Calculate qty-based and revenue-based energy
        product_df$qty_based_energy <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$total_energy_mm_btu_yr,
          MoreArgs = list(values = product_qties)
        )
        
        # Calculate qty-based and revenue-based emissions
        product_df$qty_based_emissions <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$co2e_emissions_mt_co2e_yr,
          MoreArgs = list(values = product_qties)
        )
        
        
        product_df$revenue_based_energy <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$total_energy_mm_btu_yr,
          MoreArgs = list(values = product_revenues)
        )
        
        product_df$revenue_based_emissions <- mapply(
          apply_proportions,
          product_df$presence,
          product_df$co2e_emissions_mt_co2e_yr,
          MoreArgs = list(values = product_revenues)
        )
        
        product_df$product_qty <- product_qties[i]
        product_df$product_name <- input[[paste0("products_name_", i)]]
        product_df$product_unit <- input[[paste0("units_name_", i)]]
        
        # Calculate energy intensities
        product_df$qty_based_energy_intensity <- product_df$qty_based_energy / product_qties[i]
        product_df$revenue_based_energy_intensity_qty <- product_df$revenue_based_energy / product_qties[i]
        # Calculate emissions intensities
        product_df$qty_based_em_intensity <- product_df$qty_based_emissions / product_qties[i]
        product_df$revenue_based_em_intensity_qty <- product_df$revenue_based_emissions / product_qties[i]
        
        if (input$revenue_name != "Revenue %") {
          product_df$revenue_based_energy_intensity_dollar <- product_df$revenue_based_energy / product_revenues[i]
          product_df$revenue_based_em_intensity_dollar <- product_df$revenue_based_emissions / product_revenues[i]
          product_df$revenue_based_energy_costs_intensity_dollar <- ((
            product_df$revenue_based_energy / product_df$total_energy_mm_btu_yr
          ) * product_df$total_energy_costs_yr
          ) / product_revenues[i]
        }
        
        # Store the dataframe in the list
        product_breakdowns[[i]] <- product_df
        
        # Assign individual dataframe to global environment
        assign(paste0("product_", i, "_breakdown"),
               product_df,
               envir = .GlobalEnv)
      }
      
      # Combine all product dataframes into a single dataframe
      all_products_breakdown <- do.call(rbind, product_breakdowns)
      
      # Assign the combined dataframe to the global environment
      assign("all_products_breakdown",
             all_products_breakdown,
             envir = .GlobalEnv)
      
      #Get the mass or revenue based ratio from energy breakdown and apply against costs
      all_products_breakdown <- all_products_breakdown %>%
        mutate(
          qty_based_energy_costs = (qty_based_energy / total_energy_mm_btu_yr) * total_energy_costs_yr,
          revenue_based_energy_costs = (revenue_based_energy / total_energy_mm_btu_yr) *
            total_energy_costs_yr,
          qty_based_energy_costs_intensity = qty_based_energy_costs /
            product_qty,
          revenue_based_energy_costs_intensity_qty = revenue_based_energy_costs /
            product_qty
        ) %>%
        relocate(qty_based_energy_costs, .after = revenue_based_energy) %>%
        relocate(revenue_based_energy_costs, .after = qty_based_energy_costs)
      
      if (exists("all_products_breakdown")) {
        all_products_breakdown <- `rownames<-`(all_products_breakdown, NULL)
        all_products_breakdown <- all_products_breakdown %>%
          relocate(product_qty, .after = product_number) %>%
          relocate(product_unit, .after = product_qty) %>%
          relocate(product_name, .after = product_number)
        
        if (input$revenue_name == "Revenue %") {
          all_products_summarized <- all_products_breakdown %>%
            group_by(product_number, product_name, product_unit) %>%
            summarise(
              total_energy_revenue_based_mmbtu_yr = sum(revenue_based_energy, na.rm = T),
              revenue_based_energy_intensity_mmbtu_qty = sum(revenue_based_energy_intensity_qty, na.rm = T),
              total_energy_costs_revenue_based = sum(revenue_based_energy_costs, na.rm = T),
              energy_costs_intensity_revenue_based = sum(revenue_based_energy_costs_intensity_qty, na.rm = T),
              total_emissions_revenue_based_mtco2e_yr = sum(revenue_based_emissions, na.rm = T),
              revenue_based_emission_intensity_mtco2e_qty = sum(revenue_based_em_intensity_qty, na.rm = T)
            )
          
          #Summarize all_products_breakdown by energy costs of each energy source types for dollar, and make the dataframe wider to be able to index in the
          #calculated_sentences dataframe below
          energy_costs_summarized <- all_products_breakdown %>%
            group_by(product_number, product_name, energy_source) %>%
            summarise(
              by_source_energy_costs_intensity_qty = sum(revenue_based_energy_costs_intensity_qty)
            ) %>%
            pivot_wider(names_from = "energy_source", values_from = "by_source_energy_costs_intensity_qty")
          
          calculated_sentences_en <- c(
            paste0(
              "Based on the user-selected ",
              strong("Revenue percentage-based approach,"),
              " the amount of ",
              strong("Energy"),
              " (in ",
              input$energy_units_int,
              ") required to produce a unit of product is shown below:<br>"
            ),
            sapply(1:num, function(i) {
              product_value <- all_products_summarized$revenue_based_energy_intensity_mmbtu_qty[i]
              product_name <- all_products_summarized$product_name[i]
              paste0(
                strong(product_name),
                ": ",
                tags$u(
                  paste0(
                    scales::comma(product_value, accuracy = 0.01),
                    " ",
                    input$energy_units_int,
                    " of energy"
                  )
                ),
                " is required to produce ",
                tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
              )
            })
          )
          
          formatted_sentences_en <- format_sentences(calculated_sentences_en)
          
          calculated_sentences_em <- c(
            if_else(
              input$en == FALSE && input$ec == FALSE && input$em == TRUE,
              paste0(
                "Based on the user-selected ",
                strong("Revenue percentage-based approach,"),
                " the amount of ",
                tags$u("CO₂e (in metric ton)"),
                " emitted from the production of ",
                tags$u(paste0("one ", input[[paste0("units_name_", i)]])),
                " of product is shown below:<br>"
              ),
              paste0(
                "The amount of ",
                strong("CO₂e emissions"),
                " (in metric ton)",
                " emitted from the production of ",
                tags$u(paste0("one ", input[[paste0("units_name_", i)]])),
                " of product is shown below:<br>"
              )
            )
            ,
            sapply(1:num, function(i) {
              product_value <- all_products_summarized$revenue_based_emission_intensity_mtco2e_qty[i]
              product_name <- all_products_summarized$product_name[i]
              paste0(
                strong(product_name),
                ": ",
                tags$u(paste0(
                  scales::comma(product_value, accuracy = 0.001), " metric ton of CO₂e"
                )),
                " is generated to produce ",
                tags$u(paste0("one ", input[[paste0("units_name_", i)]])),
                " of "
              )
            })
          )
          
          formatted_sentences_em <- format_sentences(calculated_sentences_em)
          
          calculated_sentences_ec_0 <- vector("list", num)
          
          for (i in 1:num) {
            # Product information part
            product_value <- all_products_summarized$energy_costs_intensity_revenue_based[i]
            unit_name <- input[[paste0("units_name_", i)]]
            product_name <- all_products_summarized$product_name[i]
            
            product_part <- paste0(
              strong(product_name),
              ": ",
              tags$u(paste0("$", scales::comma(product_value, accuracy = 0.01))),
              " in energy costs is required to proudce ",
              tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
            )
            
            # Get all energy sources and their values at once
            energy_sources <- colnames(energy_costs_summarized)[2:ncol(energy_costs_summarized)]
            energy_values <- as.numeric(energy_costs_summarized[i, 2:ncol(energy_costs_summarized)])
            
            # Create all energy source phrases in one vector
            energy_phrases <-
              if_else(
                is.na(energy_values) != T,
                paste0(
                  energy_sources,
                  "-based intensity is ",
                  "$",
                  scales::comma(energy_values, accuracy = 0.01),
                  " ",
                  tags$u(paste0("per ", unit_name))
                ),
                NA_character_
              )
            
            # Combine all energy phrases with commas
            energy_part <- paste(na.omit(energy_phrases), collapse = ", ")
            
            # Final sentence combining product info and all energy sources
            calculated_sentences_ec_0[[i]] <- paste0(product_part, ". ", energy_part, ".")
          }
          
          formatted_sentences_ec_0 <- format_sentences_bullet(calculated_sentences_ec_0)
          
          calculated_sentences_ec_1 <- c(
            if_else(
              input$en == FALSE &&
                input$ec == TRUE &&
                input$em == FALSE ||
                input$en == FALSE &&
                input$ec == TRUE && input$em == TRUE,
              paste0(
                "Based on the user-selected ",
                strong("Revenue Percentage-based approach"),
                ", the amount of ",
                strong("Energy Costs"),
                " required to produce a unit of product is shown below:"
              ),
              paste0(
                "The ",
                strong("Energy Costs"),
                " required to produce a unit of product is shown below:"
              )
            )
          )
          
          formatted_sentences_ec <- HTML(format_sentences(calculated_sentences_ec_1),
                                         formatted_sentences_ec_0)
          
          all_products_summarized <- all_products_summarized %>%
            ungroup() %>%
            select(-product_number)
          
          if (input$en == TRUE &&
              input$ec == FALSE && input$em == FALSE) {
            formatted_sentences <- paste0(formatted_sentences_en)
            
            all_products_summarized <- all_products_summarized %>%
              select(product_name:revenue_based_energy_intensity_mmbtu_qty) %>%
              rename("Product Name" = product_name,
                     "Product Unit" = product_unit) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == FALSE &&
                     input$ec == TRUE && input$em == FALSE) {
            formatted_sentences <- paste0(formatted_sentences_ec)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_energy_costs_revenue_based,
                energy_costs_intensity_revenue_based
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_revenue_based
              )
            
          } else if (input$en == FALSE &&
                     input$ec == FALSE && input$em == TRUE) {
            formatted_sentences <- paste0(formatted_sentences_em)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == TRUE && input$em == FALSE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)                            
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name:revenue_based_energy_intensity_mmbtu_qty,
                total_energy_costs_revenue_based,
                energy_costs_intensity_revenue_based
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_revenue_based
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == FALSE && input$em == TRUE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)   
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name:revenue_based_energy_intensity_mmbtu_qty,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == FALSE &&
                     input$ec == TRUE && input$em == TRUE) {                           
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_energy_costs_revenue_based,
                energy_costs_intensity_revenue_based,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_revenue_based,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == TRUE && input$em == TRUE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)                            
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/unit of Product)" = energy_costs_intensity_revenue_based,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
                
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
          }
          
          download_excel_file <- all_products_breakdown %>%
            select(
              -c(
                qty_based_energy,
                qty_based_emissions,
                qty_based_energy_costs,
                qty_based_energy_intensity,
                qty_based_em_intensity,
                qty_based_energy_costs_intensity
              )
            ) %>%
            mutate(revenue_weight_proportion = revenue_based_energy / total_energy_mm_btu_yr) %>%
            rename(
              associated_products = presence,
              all_products_energy_consumption_mmbtu_yr = total_energy_mm_btu_yr,
              revenue_based_energy_consumption_mmbtu_yr = revenue_based_energy,
              revenue_based_energy_intensity_mmbtu_per_mt_qty = revenue_based_energy_intensity_qty
            ) %>%
            relocate(energy_source, .after = source) %>%
            relocate(total_energy_costs_yr, .after = all_products_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_weight_proportion, .before = revenue_based_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_based_energy_intensity_mmbtu_per_mt_qty,
                     .after = revenue_based_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_based_energy_costs_intensity_qty, .after = revenue_based_energy_costs)
          
          names(download_excel_file)
          
          colnames(download_excel_file) <-  c(
            "Product",
            "Product Name",
            "Quantity",
            "Unit",
            "Source",
            "Energy Source",
            "Associated Products",
            paste0(
              "Total Energy\n Consumption\n(",
              input$energy_units_int,
              ")"
            ),
            "Total Energy\n Costs\n($)",
            "Total Emissions\n(MTCO₂e)",
            "Share of Revenue-based\nEnergy Consumption",
            paste0(
              "Product-level\n Energy Consumption\n(",
              input$energy_units_int,
              ")"
            ),
            paste0(
              "Revenue-Based\n Energy Intensity\n(",
              input$energy_units_int,
              "/unit of Product)"
            ),
            "Product-level\n Energy Costs ($)",
            "Revenue-based\n Energy Costs Intensity\n($/unit of Product)",
            "Product-level Emissions\n(MTCO₂e)",
            paste0(
              "Revenue-Based Emissions Intensity\n(MTCO₂e/unit of Product)"
            )
          )
          
        } else {
          all_products_summarized <- all_products_breakdown %>%
            group_by(product_number, product_name, product_unit) %>%
            summarise(
              total_energy_revenue_based_mmbtu_yr = sum(revenue_based_energy, na.rm = T),
              revenue_based_energy_intensity_mmbtu_dollar = sum(revenue_based_energy_intensity_dollar, na.rm = T),
              revenue_based_energy_intensity_mmbtu_qty = sum(revenue_based_energy_intensity_qty, na.rm = T),
              total_energy_costs_revenue_based = sum(revenue_based_energy_costs, na.rm = T),
              revenue_based_energy_costs_intensity_dollar = sum(
                revenue_based_energy_costs_intensity_dollar,
                na.rm = T
              ),
              revenue_based_energy_costs_intensity_qty = sum(revenue_based_energy_costs_intensity_qty, na.rm = T),
              total_emissions_revenue_based_mtco2e_yr = sum(revenue_based_emissions, na.rm = T),
              revenue_based_emission_intensity_mtco2e_dollar = sum(revenue_based_em_intensity_dollar, na.rm = T),
              revenue_based_emission_intensity_mtco2e_qty = sum(revenue_based_em_intensity_qty, na.rm = T)
              
            )
          
          #Summarize all_products_breakdown by energy costs of each energy source types for dollar, and make the dataframe wider to be able to index in the
          #calculated_sentences dataframe below
          energy_costs_summarized <- all_products_breakdown %>%
            group_by(product_number, product_name, energy_source) %>%
            summarise(
              by_source_energy_costs_intensity_dollar = sum(revenue_based_energy_costs_intensity_dollar)
            ) %>%
            pivot_wider(names_from = "energy_source", values_from = "by_source_energy_costs_intensity_dollar")
          
          calculated_sentences_en <- c(
            paste0(
              "Based on the user-selected ",
              strong("Revenue-based approach,"),
              " the amount of ",
              strong("Energy"),
              " (in ",
              input$energy_units_int,
              ") required per unit production or ",
              tags$u("per dollar revenue"),
              " of product is shown below:<br>"
            ),
            sapply(1:num, function(i) {
              product_value_a <- all_products_summarized$revenue_based_energy_intensity_mmbtu_dollar[i]
              product_value_b <- all_products_summarized$revenue_based_energy_intensity_mmbtu_qty[i]
              product_name <- all_products_summarized$product_name[i]
              paste0(
                strong(product_name),
                ": ",
                paste0(
                  tags$u(
                    paste0(
                      scales::comma(product_value_a, accuracy = 0.01),
                      " ",
                      input$energy_units_int,
                      " of energy"
                    )
                  ),
                  " is required to produce a ",
                  tags$u("dollar revenue"),
                  "<br>",
                  "<span style='margin-left: 28px;'>",
                  tags$u(
                    paste0(
                      scales::comma(product_value_b, accuracy = 0.01),
                      " ",
                      input$energy_units_int,
                      " of energy",
                      sep = ""
                    )
                  ),
                  " is required to produce ",
                  tags$u(paste0("one ", input[[paste0("units_name_", i)]]))
                )
              )
            })
          )
          
          formatted_sentences_en <- format_sentences(calculated_sentences_en)
          
          calculated_sentences_ec_0 <- vector("list", num)
          
          for (i in 1:num) {
            # Product information part
            product_value <- all_products_summarized$revenue_based_energy_costs_intensity_dollar[i]
            unit_name <- input[[paste0("units_name_", i)]]
            product_name <- all_products_summarized$product_name[i]
            
            product_part <- paste0(
              strong(product_name),
              ": ",
              tags$u(paste0("$", scales::comma(product_value, accuracy = 0.01))),
              " in energy costs is required to generate a dollar in revenue"
            )
            
            # Get all energy sources and their values at once
            energy_sources <- colnames(energy_costs_summarized)[2:ncol(energy_costs_summarized)]
            energy_values <- as.numeric(energy_costs_summarized[i, 2:ncol(energy_costs_summarized)])
            
            # Create all energy source phrases in one vector
            energy_phrases <-
              if_else(
                is.na(energy_values) != T,
                paste0(
                  energy_sources,
                  "-based intensity is ",
                  "$",
                  scales::comma(energy_values, accuracy = 0.01),
                  " ",
                  tags$u(paste0("per dollar revenue"))
                ),
                NA_character_
              )
            
            # Combine all energy phrases with commas
            energy_part <- paste(na.omit(energy_phrases), collapse = ", ")
            
            # Final sentence combining product info and all energy sources
            calculated_sentences_ec_0[[i]] <- paste0(product_part, ". ", energy_part, ".")
          }
          
          formatted_sentences_ec_0 <- format_sentences_bullet(calculated_sentences_ec_0)
          
          
          calculated_sentences_ec_1 <- c(
            if_else(
              input$en == FALSE &&
                input$ec == TRUE &&
                input$em == FALSE ||
                input$en == FALSE &&
                input$ec == TRUE && input$em == TRUE,
              paste0(
                "Based on the user-selected ",
                strong("Revenue-based approach"),
                ", the amount of ",
                strong("Energy Costs"),
                " required to produce a dollar revenue of product is shown below:"
              ),
              paste0(
                "The ",
                strong("Energy Costs"),
                " required to produce a dollar revenue of product is shown below:"
              )
            )
          )
          
          formatted_sentences_ec <- HTML(format_sentences(calculated_sentences_ec_1),
                                         formatted_sentences_ec_0)
          
          calculated_sentences_em <- c(
            if_else(
              input$en == FALSE && input$ec == FALSE && input$em == TRUE,
              paste0(
                "Based on the user-selected ",
                strong("Revenue-based approach,"),
                " the amount of ",
                tags$u("CO₂e (in metric ton)"),
                " emitted from a dollar revenue of product is shown below:<br>"
              ),
              paste0(
                "The amount of ",
                strong("CO₂e emissions"),
                " (in metric ton)",
                " emitted from a dollar revenue of product is shown below:<br>"
              )
            )
            ,
            sapply(1:num, function(i) {
              product_value <- all_products_summarized$revenue_based_emission_intensity_mtco2e_dollar[i]
              product_name <- all_products_summarized$product_name[i]
              paste0(
                strong(product_name),
                ": ",
                tags$u(paste0(
                  scales::comma(product_value, accuracy = 0.001), " metric ton of CO₂e"
                )),
                " is generated per dollar revenue"
              )
            })
          )
          
          formatted_sentences_em <- format_sentences(calculated_sentences_em)
          
          all_products_summarized <- all_products_summarized %>%
            ungroup() %>%
            select(-product_number)
          
          names(all_products_summarized)
          
          if (input$en == TRUE &&
              input$ec == FALSE && input$em == FALSE) {
            formatted_sentences <- paste0(formatted_sentences_en)
            
            all_products_summarized <- all_products_summarized %>%
              select(product_name:revenue_based_energy_intensity_mmbtu_qty) %>%
              rename("Product Name" = product_name,
                     "Product Unit" = product_unit) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/$ Revenue)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_dollar
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == FALSE &&
                     input$ec == TRUE && input$em == FALSE) {
            formatted_sentences <- paste0(formatted_sentences_ec)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_energy_costs_revenue_based,
                revenue_based_energy_costs_intensity_dollar,
                revenue_based_energy_costs_intensity_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/$ Revenue)" =  revenue_based_energy_costs_intensity_dollar,
                "Energy Costs Intensity\n($/unit of Product)" = revenue_based_energy_costs_intensity_qty
              )
            
          } else if (input$en == FALSE &&
                     input$ec == FALSE && input$em == TRUE) {
            formatted_sentences <- paste0(formatted_sentences_em)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_dollar,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/$ Revenue)" = revenue_based_emission_intensity_mtco2e_dollar,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == TRUE && input$em == FALSE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)                            
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name:revenue_based_energy_intensity_mmbtu_qty,
                total_energy_costs_revenue_based,
                revenue_based_energy_costs_intensity_dollar,
                revenue_based_energy_costs_intensity_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/$ Revenue)" =  revenue_based_energy_costs_intensity_dollar,
                "Energy Costs Intensity\n($/unit of Product)" = revenue_based_energy_costs_intensity_qty
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/$ Revenue)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_dollar
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == FALSE && input$em == TRUE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)  
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name:revenue_based_energy_intensity_mmbtu_qty,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_dollar,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/$ Revenue)" = revenue_based_emission_intensity_mtco2e_dollar,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/$ Revenue)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_dollar
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
            
          } else if (input$en == FALSE &&
                     input$ec == TRUE && input$em == TRUE) {                            
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              select(
                product_name,
                product_unit,
                total_energy_costs_revenue_based,
                revenue_based_energy_costs_intensity_dollar,
                revenue_based_energy_costs_intensity_qty,
                total_emissions_revenue_based_mtco2e_yr,
                revenue_based_emission_intensity_mtco2e_dollar,
                revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/$ Revenue)" =  revenue_based_energy_costs_intensity_dollar,
                "Energy Costs Intensity\n($/unit of Product)" = revenue_based_energy_costs_intensity_qty,
                "Revenue-based\n Emissions\n(MTCO₂e)" = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/$ Revenue)" = revenue_based_emission_intensity_mtco2e_dollar,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              )
            
          } else if (input$en == TRUE &&
                     input$ec == TRUE && input$em == TRUE) {
            formatted_sentences_en <- paste0(formatted_sentences_en)                            
            formatted_sentences_ec <- paste0(formatted_sentences_ec)
            formatted_sentences_em <- paste0(formatted_sentences_em) 
            
            all_products_summarized <- all_products_summarized %>%
              rename(
                "Product Name" = product_name,
                "Product Unit" = product_unit,
                "Revenue-based\n Energy Costs\n($)" = total_energy_costs_revenue_based,
                "Energy Costs Intensity\n($/$ Revenue)" =  revenue_based_energy_costs_intensity_dollar,
                "Energy Costs Intensity\n($/unit of Product)" = revenue_based_energy_costs_intensity_qty,
                "Revenue-based\n Emissions\n(MTCO₂e)"  = total_emissions_revenue_based_mtco2e_yr,
                "Emissions Intensity\n(MTCO₂e/$ Revenue)" = revenue_based_emission_intensity_mtco2e_dollar,
                "Emissions Intensity\n(MTCO₂e/unit of Product)" = revenue_based_emission_intensity_mtco2e_qty
              ) %>%
              rename_with(
                ~ paste0(
                  "Revenue-based\n Energy Consumption\n(",
                  input$energy_units_int,
                  ")"
                ),
                .cols = total_energy_revenue_based_mmbtu_yr
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/$ Revenue)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_dollar
              ) %>%
              rename_with(
                ~ paste0(
                  "Energy Intensity\n(",
                  input$energy_units_int,
                  "/unit of Product)"
                ),
                .cols = revenue_based_energy_intensity_mmbtu_qty
              )
          }
          
          download_excel_file <- all_products_breakdown %>%
            select(
              -c(
                qty_based_energy,
                qty_based_emissions,
                qty_based_energy_costs,
                qty_based_energy_intensity,
                qty_based_em_intensity,
                qty_based_energy_costs_intensity
              )
            ) %>%
            mutate(revenue_weight_proportion = revenue_based_energy / total_energy_mm_btu_yr) %>%
            rename(
              associated_products = presence,
              all_products_energy_consumption_mmbtu_yr = total_energy_mm_btu_yr,
              revenue_based_energy_consumption_mmbtu_yr = revenue_based_energy,
              revenue_based_energy_intensity_mmbtu_per_dollar = revenue_based_energy_intensity_dollar,
              revenue_based_energy_intensity_mmbtu_per_mt_qty = revenue_based_energy_intensity_qty
            )  %>%
            relocate(energy_source, .after = source) %>%
            relocate(total_energy_costs_yr, .after = all_products_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_weight_proportion, .before = revenue_based_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_based_energy_intensity_mmbtu_per_dollar,
                     .after = revenue_based_energy_consumption_mmbtu_yr) %>%
            relocate(revenue_based_energy_intensity_mmbtu_per_mt_qty,
                     .after = revenue_based_energy_intensity_mmbtu_per_dollar) %>%
            relocate(revenue_based_energy_costs, .after = revenue_based_energy_intensity_mmbtu_per_mt_qty) %>%
            relocate(revenue_based_energy_costs_intensity_dollar,
                     .after = revenue_based_energy_costs) %>%
            relocate(revenue_based_energy_costs_intensity_qty, .after = revenue_based_energy_costs_intensity_dollar) %>%
            relocate(revenue_based_em_intensity_dollar, .after = revenue_based_emissions)
          
          names(download_excel_file)
          
          colnames(download_excel_file) <-  c(
            "Product",
            "Product Name",
            "Quantity",
            "Unit",
            "Source",
            "Energy Source",
            "Associated Products",
            paste0(
              "Total Energy Consumption\n(",
              input$energy_units_int,
              ")"
            ),
            "Total Energy Costs ($)",
            "Total Emissions\n(MTCO₂e)",
            "Share of Revenue-based\nEnergy Consumption",
            paste0(
              "Product-level\n Energy Consumption\n(",
              input$energy_units_int,
              ")"
            ),
            paste0(
              "Revenue-based\n Energy Intensity\n(",
              input$energy_units_int,
              "/$)"
            ),
            paste0(
              "Revenue-based\n Energy Intensity\n(",
              input$energy_units_int,
              "/unit of Product)"
            ),
            "Product-level\n Energy Costs\n($)",
            "Revenue-based\n Energy Costs Intensity\n ($ costs/$rev.)",
            "Revenue-based\n Energy Costs Intensity\n ($ costs/unit of product)",
            "Product-level\n Emissions\n(MTCO₂e)",
            paste0(
              "Revenue-Based\n Emissions Intensity\n(MTCO₂e/",
              "unit of Product)"
            ),
            "Revenue-Based\n Emissions Intensity\n(MTCO₂e/$)"
          )
          
        }
      }
    }
    
    output$enPlot <- renderPlotly({
      all_products_summarized_plot <<- all_products_summarized %>%
        select(1:2, contains("energy intensity")) %>%
        pivot_longer(cols = !c(1, 2),
                     names_to = "intensity_name",
                     values_to = "value")
      
      p <- ggplot(data = all_products_summarized_plot, 
                  aes(x = `Product Name`, 
                      y = value, 
                      fill = `Product Name`)) +
        geom_bar(stat = "identity",
                 position = position_dodge2(preserve = "single", padding = 0.1),
                 alpha = 0.9,
                 width = 0.5,
                 color = "white") +
        facet_wrap(~ intensity_name, 
                   scales = "free_y") +
        scale_fill_viridis_d(option = "plasma", 
                             begin = 0.1, 
                             end = 0.9,
                             direction = -1) +
        labs(x = "Product",
             y = paste0("Energy Intensity\n(",input$energy_units_int,"/unit)")) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(family = "sans"),
          axis.text.x = element_text( color = "#2d3436"),
          axis.title = element_text(face = "bold", 
                                    color = "#2d3436"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "#dfe6e9"),
          strip.background = element_rect(fill = "#f5f6fa",
                                          color = NA),
          strip.text = element_text(face = "bold",
                                    color = "#2d3436"),
          plot.margin = unit(c(1, 2, 1, 1), "cm")
        ) +
        scale_y_continuous(labels = comma) +
        coord_cartesian(clip = "off") +
        guides(fill = "none")
      
      
      p2 <- ggplotly(p, tooltip = "text")
      
      p2 <- p2 %>%
        config(
          displayModeBar = T,
          modeBarButtonsToRemove = list(
            "zoom2d",
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
            "toggleSpikelines"
          )
        )
      
      return(p2)
    })
    
    output$ecPlot <- renderPlotly({
      all_products_summarized_plot <- all_products_summarized %>%
        select(1:2, contains("energy costs intensity")) %>%
        pivot_longer(cols = !c(1, 2),
                     names_to = "intensity_name",
                     values_to = "value")
      
      p <- ggplot(data = all_products_summarized_plot, 
                  aes(x = `Product Name`, 
                      y = value, 
                      fill = `Product Name`)) +
        geom_bar(stat = "identity",
                 position = position_dodge2(preserve = "single", padding = 0.1),
                 alpha = 0.9,
                 width = 0.5,
                 color = "white") +
        facet_wrap(~ intensity_name, 
                   scales = "free_y") +
        scale_fill_viridis_d(option = "plasma", 
                             begin = 0.1, 
                             end = 0.9,
                             direction = -1) +
        labs(x = "Product",
             y = paste0("Energy Costs Intensity\n(",input$energy_units_int,"/unit)")) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(family = "sans"),
          axis.text.x = element_text( color = "#2d3436"),
          axis.title = element_text(face = "bold", 
                                    color = "#2d3436"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "#dfe6e9"),
          strip.background = element_rect(fill = "#f5f6fa",
                                          color = NA),
          strip.text = element_text(face = "bold",
                                    color = "#2d3436"),
          plot.margin = unit(c(1, 2, 1, 1), "cm")
        ) +
        scale_y_continuous(labels = comma) +
        coord_cartesian(clip = "off") +
        guides(fill = "none")
      
      p2 <- ggplotly(p, tooltip = "text")
      
      p2 <- p2 %>%
        config(
          displayModeBar = T,
          modeBarButtonsToRemove = list(
            "zoom2d",
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
            "toggleSpikelines"
          )
        )
      
      return(p2)
    })
    
    output$emPlot <- renderPlotly({
      all_products_summarized_plot <- all_products_summarized %>%
        select(1:2, contains("emissions intensity")) %>%
        pivot_longer(cols = !c(1, 2),
                     names_to = "intensity_name",
                     values_to = "value")
      
      p <- ggplot(data = all_products_summarized_plot, 
                  aes(x = `Product Name`, 
                      y = value, 
                      fill = `Product Name`)) +
        geom_bar(stat = "identity",
                 position = position_dodge2(preserve = "single", padding = 0.1),
                 alpha = 0.9,
                 width = 0.5,
                 color = "white") +
        facet_wrap(~ intensity_name, 
                   scales = "free_y") +
        scale_fill_viridis_d(option = "plasma", 
                             begin = 0.1, 
                             end = 0.9,
                             direction = -1) +
        labs(x = "Product",
             y = paste0("Energy Intensity\n(",input$energy_units_int,"/unit)")) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(family = "sans"),
          axis.text.x = element_text( color = "#2d3436"),
          axis.title = element_text(face = "bold", 
                                    color = "#2d3436"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "#dfe6e9"),
          strip.background = element_rect(fill = "#f5f6fa",
                                          color = NA),
          strip.text = element_text(face = "bold",
                                    color = "#2d3436"),
          plot.margin = unit(c(1, 2, 1, 1), "cm")
        ) +
        scale_y_continuous(labels = comma) +
        coord_cartesian(clip = "off") +
        guides(fill = "none")
      
      p2 <- ggplotly(p, tooltip = "text")
      
      p2 <- p2 %>%
        config(
          displayModeBar = T,
          modeBarButtonsToRemove = list(
            "zoom2d",
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
            "toggleSpikelines"
          )
        )
      
      return(p2)
    })
    
    output$textOutput1 <- renderUI({
      if (input$ResultTabs == "Graphical Results") {
        if (input$graph_tabs == "enPlotvalue") {
          HTML(paste(formatted_sentences_en, collapse = "<br>"))
        } else if (input$graph_tabs == "ecPlotvalue") {
          HTML(paste(formatted_sentences_ec, collapse = "<br>"))
        } else if (input$graph_tabs == "emPlotvalue") {
          HTML(paste(formatted_sentences_em, collapse = "<br>"))
        } else {
          HTML(paste(formatted_sentences, collapse = "<br>"))
        }
      } else {
        NULL
      }
    })
    
    
    output$intensity_table <- renderDataTable({
      if (input$selected_method == "Quantity-based" ||
          input$revenue_name == "Revenue %") {
        if (input$en == TRUE &&
            input$ec == FALSE &&
            input$em == FALSE || input$en == FALSE &&
            input$ec == FALSE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:3
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3), digits = 0) %>%
            formatRound(columns = c(4), digits = 3)
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == FALSE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:3
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatCurrency(columns = 3,
                           currency = "$",
                           digits = 0) %>%
            formatRound(columns = c(4), digits = 3)
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == FALSE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:5
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3), digits = 0) %>%
            formatRound(columns = c(4, 6), digits = 3) %>%
            formatCurrency(columns = 5,
                           currency = "$",
                           digits = 0)
          
        } else if (input$en == TRUE &&
                   input$ec == FALSE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:5
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3, 5), digits = 0) %>%
            formatRound(columns = c(4, 6), digits = 3)
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:5
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(5), digits = 0) %>%
            formatRound(columns = c(4, 6), digits = 3) %>%
            formatCurrency(columns = 3,
                           currency = "$",
                           digits = 0)
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:7
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3, 7), digits = 0) %>%
            formatRound(columns = c(4, 6, 8), digits = 3) %>%
            formatCurrency(columns = 5,
                           currency = "$",
                           digits = 0)
        }
        
      } else  {
        if (input$en == TRUE &&
            input$ec == FALSE &&
            input$em == FALSE || input$en == FALSE &&
            input$ec == FALSE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:4
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3), digits = 0) %>%
            formatRound(columns = c(4, 5), digits = 3)
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == FALSE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:4
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatCurrency(columns = 3,
                           currency = "$",
                           digits = 0) %>%
            formatRound(columns = c(4, 5), digits = 3)
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == FALSE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:7
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3), digits = 0) %>%
            formatRound(columns = c(4, 5, 7, 8), digits = 3) %>%
            formatCurrency(columns = 6,
                           currency = "$",
                           digits = 0)
          
        } else if (input$en == TRUE &&
                   input$ec == FALSE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:7
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3, 6), digits = 0) %>%
            formatRound(columns = c(4, 5, 7, 8), digits = 3)
          
        } else if (input$en == FALSE &&
                   input$ec == TRUE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:7
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(6), digits = 0) %>%
            formatRound(columns = c(4, 5, 7, 8), digits = 3) %>%
            formatCurrency(columns = 3,
                           currency = "$",
                           digits = 0)
          
        } else if (input$en == TRUE &&
                   input$ec == TRUE && input$em == TRUE) {
          all_products_summarized %>%
            DT::datatable(
              class = 'cell-border stripe hover',
              filter = "none",
              style = "default",
              fillContainer = F,
              rownames = F,
              selection = "single",
              options = list(
                columnDefs = list(list(
                  className = 'dt-center', targets = 0:10
                )),
                dom = 't',
                autoWidth = TRUE
              )
            ) %>%
            formatRound(columns = c(3, 9), digits = 0) %>%
            formatRound(columns = c(4, 5, 7, 8, 10, 11),
                        digits = 3) %>%
            formatCurrency(columns = 6,
                           currency = "$",
                           digits = 0)
        }
      }
    })
    
    output$show_dl_link <- renderUI({
      downloadLink("download_all_data",
                   "Download Underlying Data (.xlsx)",
                   style = "font-size: 14px; text-decoration: underline;")
    })
    
    
    output$download_all_data <- downloadHandler(
      filename = function() {
        "all_data.xlsx"
      },
      content = function(file) {
        # Create the custom header style
        hs1 <- createStyle(
          fontColour = "black",
          fgFill = "#e0e0e0",
          halign = "center",
          valign = "center",
          textDecoration = "bold",
          border = "TopBottomLeftRight",
          wrapText = TRUE,
          fontSize = 12
        )
        
        # Create a workbook and add a worksheet
        wb <- createWorkbook()
        addWorksheet(wb, "Sheet1")
        
        # Write data to the worksheet
        writeData(wb, "Sheet1", download_excel_file, headerStyle = hs1)
        
        # Set column widths to auto
        setColWidths(wb,
                     "Sheet1",
                     cols = 1:ncol(download_excel_file),
                     widths = "auto")
        
        # Create styles for different decimal places
        no_decimal_style <- createStyle(numFmt = "0")
        two_decimal_style <- createStyle(numFmt = "0.00")
        three_decimal_style <- createStyle(numFmt = "0.000")
        
        # Define which columns should have which decimal places
        no_decimal_cols <- c(3, 8, 9, 10, 12)
        two_decimal_cols <- c(11)
        
        
        # Apply styles to specific columns
        addStyle(
          wb,
          "Sheet1",
          no_decimal_style,
          rows = 2:(nrow(download_excel_file) + 1),
          cols = no_decimal_cols,
          gridExpand = TRUE
        )
        
        addStyle(
          wb,
          "Sheet1",
          two_decimal_style,
          rows = 2:(nrow(download_excel_file) + 1),
          cols = two_decimal_cols,
          gridExpand = TRUE
        )
        
        if (input$selected_method == "Quantity-based" ||
            input$revenue_name == "Revenue %") {
          three_decimal_cols <- c(13, 15, 17)
          
          addStyle(
            wb,
            "Sheet1",
            three_decimal_style,
            rows = 2:(nrow(download_excel_file) + 1),
            cols = three_decimal_cols,
            gridExpand = TRUE
          )
        } else {
          three_decimal_cols <- c(13, 14, 16, 17, 19, 20)
          
          addStyle(
            wb,
            "Sheet1",
            three_decimal_style,
            rows = 2:(nrow(download_excel_file) + 1),
            cols = three_decimal_cols,
            gridExpand = TRUE
          )
        }
        
        column_border_style <- createStyle(border = c("left", "right"),
                                           borderStyle = "thin")
        
        # Apply the style to each column individually
        for (col in 1:ncol(download_excel_file)) {
          addStyle(
            wb,
            "Sheet1",
            style = column_border_style,
            rows = 1:(nrow(download_excel_file) + 1),
            # Apply to all rows including headers
            cols = col,
            gridExpand = TRUE,
            stack = TRUE
          )
        }
        
        product_changes <- which(diff(download_excel_file$Product) != 0) + 1
        
        bottom_border_style <- createStyle(border = "bottom", borderStyle = "thin")
        
        # Apply the bottom border style to the identified rows
        for (row in product_changes) {
          addStyle(
            wb,
            "Sheet1",
            style = bottom_border_style,
            rows = row,
            cols = 1:ncol(download_excel_file),
            # Apply to all columns
            gridExpand = TRUE,
            stack = TRUE
          )
        }
        
        addStyle(
          wb,
          "Sheet1",
          createStyle(border = "Bottom"),
          rows = (nrow(download_excel_file) + 1),
          cols = 1:ncol(download_excel_file),
          gridExpand = TRUE,
          stack = TRUE
        )
        
        centered_style <- createStyle(halign = "center", valign = "center")
        
        # Apply the centered style to all cells
        addStyle(
          wb,
          "Sheet1",
          style = centered_style,
          rows = 1:(nrow(download_excel_file) + 1),
          # Apply to all rows including headers
          cols = 1:ncol(download_excel_file),
          # Apply to all columns
          gridExpand = TRUE,
          stack = TRUE
        )
        
        
        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
  })
  
  
}

shinyApp(ui, server)
