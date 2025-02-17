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
library(bslib)

install_phantomjs(force = T)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  includeCSS(system.file("css", "kable_extra.css", package = "kableExtra")),
  tags$head(tags$style(
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
      "
    )
  )),
  titlePanel(HTML("Facility Sankey Tool"), windowTitle = "FST"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Load Inputs",
      sidebarLayout(
        sidebarPanel(
          tags$style(HTML(
            "
      #downloadData1 {
        font-weight: bold;
        font-size: 18px;
      }
    "
          )),
          downloadLink("downloadData1", "Download Facility Sankey Tool - Input Sheet"),
          br(),
          br(),
          br(),
          fileInput("file", "Upload \'FST Input Sheet\' Excel File", accept = ".xlsx"),
          br(),
          br(),
          tags$style(HTML(
            "
      #downloadData2 {
        font-weight: bold;
        font-size: 18px;
      }
    "
          )),
          downloadLink("downloadData2", "Download Tool Documentation")
          
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
          br(),
          h3("Tab 3: Emissions Sankey"),
          tags$p("1. View and customize Emissions Sankey Diagram.", style = "font-size: 18px;"),
          tags$p("2. Download Emissions Sankey Diagram.", style = "font-size: 18px;"),
          br(),
          h3("Tab 4: Product Emissions Intensity"),
          tags$p(
            "1. Estimate Product Emissions Intensity (e.g., Product Carbon Footprint).",
            style = "font-size: 18px;"
          )
        )
      ),
      tags$div(
        style = "position: fixed; bottom: 0; width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: space-between; align-items: flex-end;",
        tags$div(
          style = "text-align: left;",
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px; "),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
        
        
      )
      
    ),
    tabPanel(
      "Energy Sankey",
      sidebarLayout(
        sidebarPanel(
          textInput("cname_e", "Enter Facility Name"),
          textOutput('move'),
          selectInput("units_e", "Select Units", c("MMBtu/yr", "MWh/yr")),
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
          div(style = "position: relative; width: 100%; max-height: 100%; preserveAspectRatio='xMinYMin meet';  background-color: #f8f8f8;", uiOutput("diagram_energy"))
        )
        
      ),
      tags$div(
        style = "width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: space-between; align-items: flex-end;",
        tags$div(
          style = "text-align: left;",
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px; "),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
        
        
      )
      
    ),
    tabPanel(
      "Emissions Sankey",
      sidebarLayout(
        sidebarPanel(
          textInput("cname", "Enter Facility Name"),
          textOutput('move'),
          selectInput("units", "Select Units", c("MT CO₂e/yr", "lbs. of CO₂e/yr")),
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
          tableOutput("table1")
          
        )
        
      ),
      tags$div(
        style = "width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: space-between; align-items: flex-end;",
        tags$div(
          style = "text-align: left;",
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px; "),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
    ),
    tabPanel(
      "Product Emissions Intensity",
      sidebarLayout(
        sidebarPanel(
          tags$style(HTML("
        font-weight: bold;
        font-size: 16px;
      ")),
          width = 3,
          selectInput(
            "products_num",
            "Enter Number of Products:",
            choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
          ),
          uiOutput("product_inputs"),
          actionButton("calc_int", "Calculate Product Intensity")
        ),
        mainPanel(
          div(
            style = "position: relative; width: 100%; padding-bottom: 5%;",
            div(style = "display: flex; justify-content: center; align-items: center;", DTOutput("intensity_table")),
            div(style = "position: relative; bottom: 30%; right: -90%;", uiOutput("show_dl_link"))
          )
        )
      ),
      tags$div(
        style = "width: 100%; background-color: #f8f8f8; text-align: center; display: flex; justify-content: space-between; align-items: flex-end;",
        tags$div(
          style = "text-align: left;",
          tags$img(src = "lbnl.png", style = "max-height: 50px; margin-left: 0px;"),
          tags$p(tags$b("Prakash Rao"), style = "margin-top: 0.5px; margin-left: 0px;"),
          tags$p("prao@lbl.gov", style = "margin-top: 0.5px; margin-left: 0px;")
        ),
        tags$div(
          style = "text-align: left;",
          
          tags$img(src = "ucdavis_logo_gold.png", style = "max-height: 50px;"),
          tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px; "),
          tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  excelFilePath <- "Facility Sankey Tool - Input Sheet.xlsx"
  
  docFilePath <- 'User Guide for Facility CO2e Flow Tool.pdf'
  
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
    
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source))
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    em.src <- tibble('Name' = unique(aa$`emission_category`))
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
  
  
  nodes_data_energy <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    aa <- aa[-1, ]
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
    nodes.h <- rbind(nodes.hh, ene.src, end.use)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>%
      mutate('No' = row_number()) %>%
      select(No, Name)
    nodes
  })
  
  end_use <- reactiveValues(measures = NULL)
  
  output$product_inputs <- renderUI({
    req(input$file)
    num <- input$products_num
    
    aa <<- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200") %>%
      clean_names()
    
    aa <- aa[-1, ]
    aa <- aa %>%
      filter(!is.na(source))
    end.use <- tibble('Name' = aa$`source`)
    end_use$measures <- end.use
    
    lapply(1:num, function(i) {
      tagList(fluidRow(
        column(width = 6, numericInput(
          paste0("mass_", i),
          paste("Enter Product ", i, " Mass (Tons/yr):"),
          value = 0,
          min = 0
        )),
        column(
          width = 6,
          autonumericInput(
            paste0("revenue_", i),
            paste("Enter Product ", i, " Revenue %:"),
            value = 0,
            decimalPlaces = 0,
            currencySymbol = "%",
            currencySymbolPlacement = "s",
            minimumValue = "0",
            maximumValue = "100",
            align = "left"
          )
        ),
        column(
          width = 12,
          selectInput(
            paste0("process_", i),
            paste("Select Product ", i, "-based Processes:"),
            choices = end_use$measures,
            multiple = TRUE
          )
        )
      ), br())
    })
    
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
    
    
    bb <- read_excel(input$file$datapath, sheet = 'Emission Factors', range = "b3:h20")
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    
    aa <- aa[-1, ]
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
    
    
    temp <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    
    temp <- temp[-1, ]
    temp <- clean_names(temp)
    temp <- temp %>%
      filter(!is.na(source))
    temp
  })
  
  temp_e <- reactive({
    req(input$file)
    temp_e <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    temp_e <- temp_e[-1, ]
    temp_e <- clean_names(temp_e)
    temp_e <- temp_e %>%
      filter(!is.na(energy_source))
    temp_e
  })
  
  
  links_data <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source))
    
    end.use <- tibble('Name' = aa$`source`)
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    em.src <- tibble('Name' = unique(aa$`emission_category`))
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
      filter(!is.na(energy_source))
    
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
      filter(is.na(energy_source))
    pr <- nodes %>%
      filter(Name == 'Process')
    pr_link_val <- numeric(0)
    if (!is_empty(pr$No)) {
      pr_link_val <- as.numeric(pr$No - 1)
    }
    fg_link_val <- numeric(0)
    fg <- nodes %>%
      filter(Name == 'Fugitive')
    if (!is_empty(fg$No)) {
      fg_link_val <- as.numeric(fg$No - 1)
    }
    
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
          if (aa.ne[o, 'emission_category'] == nodes[j, 'Name']) {
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
    
    if (!is_empty(pr_link_val) & !is_empty(fg_link_val)) {
      links.t <- links.h %>%
        filter(Source == pr_link_val |
                 Source == ene_link_val |
                 Source == fg_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    } else if (!is_empty(pr_link_val) & is_empty(fg_link_val)) {
      links.t <- links.h %>%
        filter(Source == pr_link_val | Source == ene_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    } else if (is_empty(pr_link_val) & !is_empty(fg_link_val)) {
      links.t <- links.h %>%
        filter(Source == fg_link_val | Source == ene_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    } else{
      links.t <- links.h %>%
        filter(Source == ene_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    }
    
    total_fields <- as.numeric(!is_empty(pr_link_val)) + as.numeric(!is_empty(ene_link_val)) +
      as.numeric(!is_empty(fg_link_val))
    
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
  
  
  links_data_energy <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    aa <- aa[-1, ]
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
      filter(!is.na(energy_source))
    
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
    
    
    
    
    if (!is_empty(ele_link_val) & !is_empty(fuel_link_val)) {
      links.t <- links.h %>%
        filter(Source == ele_link_val |
                 Source == fuel_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    } else if (!is_empty(ele_link_val) & is_empty(fuel_link_val)) {
      links.t <- links.h %>%
        filter(Source == ele_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
    } else {
      links.t <- links.h %>%
        filter(Source == fuel_link_val) %>%
        group_by(Source) %>%
        summarise(Value = sum(Value))
      
    }
    
    
    total_fields <- as.numeric(!is_empty(fuel_link_val)) + as.numeric(!is_empty(ele_link_val))
    
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
  
  output$output_text <- renderUI({
    req(input$file)
    if (nchar(input$cname) > 0 & input$perc != "Percentage") {
      paste0("Facility CO₂e Flow for ",
             input$cname,
             " (" ,
             input$units,
             ")")
    } else if (nchar(input$cname) > 0 &
               input$perc == "Percentage") {
      paste0("Facility CO₂e Flow for ", input$cname, " (%)")
    } else if (input$perc == "Percentage") {
      paste0("Facility CO₂e Flow ", "(%)")
    } else {
      paste0("Facility CO₂e Flow ", "(" , input$units, ")")
    }
  })
  
  
  output$output_text_e <- renderUI({
    req(input$file)
    if (nchar(input$cname_e) > 0 & input$perc_e != "Percentage") {
      paste0("Facility Energy Flow for ",
             input$cname_e,
             " (" ,
             input$units_e,
             ")")
    } else if (nchar(input$cname_e) > 0 &
               input$perc_e == "Percentage") {
      paste0("Facility Energy Flow for ", input$cname_e, " (%)")
    } else if (input$perc == "Percentage") {
      paste0("Facility Energy Flow ", "(%)")
    } else {
      paste0("Facility Energy Flow ", "(" , input$units_e, ")")
    }
  })
  
  output$downloadPNG <- downloadHandler(
    filename = "Facility CO2e Flow.png",
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
        caption <- paste0("Facility CO2e Flow for ", input$cname, "(" , un, ")")
      } else {
        caption <- paste0("Facility CO2e Flow ", "(" , un, ")")
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
    filename = "Facility Energy Flow.png",
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
        caption <- paste0("Facility Energy Flow for ",
                          input$cname_e,
                          "(" ,
                          input$units_e,
                          ")")
      } else {
        caption <- paste0("Facility Energy Flow ", "(" , input$units_e, ")")
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
    
    aa <- read_excel(input$file$datapath, sheet = 'Calculator', range = "c17:o200")
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>%
      filter(!is.na(source)) %>%
      select(source, co2e_emissions_mt_co2e_yr)
    
    product_dataframe <- function(i) {
      product_data <- aa %>%
        filter(source %in% input[[paste0("process_", i)]])
      
      return(product_data)
    }
    
    all_product_dfs <- list()
    mass_values <- numeric(num)
    revenue_values <- numeric(num)
    
    for (i in 1:num) {
      df_name <- paste0("product_data_", i, "_df")
      df <- product_dataframe(i)
      
      assign(df_name, df, envir = .GlobalEnv)
      
      all_product_dfs[[df_name]] <- df
      
      
      ##Mass
      df_mass <- paste0("product_", i, "_mass")
      mass_value <- input[[paste0("mass_", i)]]
      assign(df_mass, mass_value, envir = .GlobalEnv)
      
      ##Revenue
      df_revenue <- paste0("product_", i, "_revenue")
      rev_value <- input[[paste0("revenue_", i)]] / 100
      assign(df_revenue, rev_value , envir = .GlobalEnv)
    }
    
    assign("dataframes", all_product_dfs, envir = .GlobalEnv)
    
    mass_vector <- numeric(num)
    revenue_vector <- numeric(num)
    
    # Calculate process percentages
    for (i in 1:num) {
      df_name <- paste0("product_data_", i, "_df")
      df <- product_dataframe(i)
      
      assign(df_name, df, envir = .GlobalEnv)
      
      all_product_dfs[[df_name]] <- df
      
      
      ##Mass
      df_mass <- paste0("product_", i, "_mass")
      mass_value <- input[[paste0("mass_", i)]]
      assign(df_mass, mass_value, envir = .GlobalEnv)
      mass_vector[i] <- mass_value
      
      ##Revenue
      df_revenue <- paste0("product_", i, "_revenue")
      rev_value <- input[[paste0("revenue_", i)]] / 100
      assign(df_revenue, rev_value , envir = .GlobalEnv)
      revenue_vector[i] <- rev_value
    }
    
    # Create named vectors
    names(mass_vector) <- paste0("product_", 1:num, "_mass")
    names(revenue_vector) <- paste0("product_", 1:num, "_revenue")
    
    # Assign vectors to global environment
    assign("product_masses", mass_vector, envir = .GlobalEnv)
    assign("product_revenues", revenue_vector, envir = .GlobalEnv)
    
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
    
    # Initialize an empty list to store individual product dataframes
    product_breakdowns <- list()
    
    for (i in seq_along(dataframes)) {
      df <- dataframes[[i]]
      
      results <- sapply(df$source,
                        check_presence,
                        dataframes = dataframes,
                        current_index = i)
      
      product_df <- data.frame(
        product_number = i,
        source = df$source,
        presence = results,
        co2e_emissions_mt_co2e_yr = df$co2e_emissions_mt_co2e_yr,
        stringsAsFactors = FALSE
      )
      
      product_df <- product_df############################
      
      # Calculate mass-based and revenue-based emissions
      product_df$mass_based_emissions <- mapply(
        apply_proportions,
        product_df$presence,
        product_df$co2e_emissions_mt_co2e_yr,
        MoreArgs = list(values = product_masses)
      )
      
      
      product_df$revenue_based_emissions <- mapply(
        apply_proportions,
        product_df$presence,
        product_df$co2e_emissions_mt_co2e_yr,
        MoreArgs = list(values = product_revenues)
      )
      
      # Calculate emissions intensities
      product_df$mass_based_intensity <- product_df$mass_based_emissions / product_masses[i]
      product_df$revenue_based_intensity <- product_df$revenue_based_emissions / product_masses[i]
      
      # Store the dataframe in the list
      product_breakdowns[[i]] <- product_df
      
      # Assign individual dataframe to global environment
      assign(paste0("product_", i, "_breakdown"), product_df, envir = .GlobalEnv)
    }
    
    # Combine all product dataframes into a single dataframe
    all_products_breakdown <- do.call(rbind, product_breakdowns)
    
    # Assign the combined dataframe to the global environment
    assign("all_products_breakdown", all_products_breakdown, envir = .GlobalEnv)
    
    if (exists("all_products_breakdown")) {
      all_products_breakdown <- `rownames<-`(all_products_breakdown, NULL)
      all_products_summarized <<- all_products_breakdown %>%
        group_by(product_number) %>%
        summarise(
          total_emissions_mass_based_mtco2e_yr = sum(mass_based_emissions),
          mass_based_emission_intensity_mtco2e_ton = sum(mass_based_intensity),
          total_emissions_revenue_based_mtco2e_yr = sum(revenue_based_emissions),
          revenue_based_emission_intensity_mtco2e_ton = sum(revenue_based_intensity)
        )
      
      colnames(all_products_summarized) <-  c(
        "Product",
        "Mass-based Emissions\n(MTCO2e)",
        "Mass-based Emissions Intensity\n(MTCO2e/Ton of Product)",
        "Revenue-based Emissions\n(MTCO2e)",
        "Revenue-based Emissions Intensity\n(MTCO2e/Ton of Product)"
      )
      
      names(all_products_breakdown)
      
      download_excel_file <- all_products_breakdown %>%
        rename(
          associated_products = presence,
          all_products_co2e_emissions_mt_co2e_yr = co2e_emissions_mt_co2e_yr,
          mass_based_emissions_mt_co2e_yr = mass_based_emissions,
          mass_based_ei_mt_co2e_per_mt = mass_based_intensity,
          revenue_based_emissions_mt_co2e_yr = revenue_based_emissions,
          revenue_based_ei_mt_co2e_per_mt = revenue_based_intensity,
        ) %>%
        relocate(mass_based_ei_mt_co2e_per_mt, .after = mass_based_emissions_mt_co2e_yr)
    }
    
    output$intensity_table <- renderDataTable({
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
        formatRound(columns = c(2, 4), digits = 0) %>%
        formatRound(columns = c(3, 5), digits = 2)
    })
    
    output$show_dl_link <- renderUI({
      downloadLink("download_all_data", "Download Plot Data (.XLSX)", style = "font-size: 14px; text-decoration: underline;")
    })
    
    # output$download_all_data <- downloadHandler(
    #   filename = function() {
    #     "all_data.xlsx"
    #   },
    #   content = function(file) {
    #     write.xlsx(all_products_breakdown, file)
    #   }
    # )
    
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
        
        # Create styles for different decimal places
        style_1_decimal <- createStyle(numFmt = "0.0")
        style_3_decimals <- createStyle(numFmt = "0.00")
        
        # Define columns for different decimal places
        cols_1_decimals <- c(4, 5,7)  # columns for 1 decimal place
        cols_3_decimals <- c(6,8)    # columns for 3 decimal places
        
        # Create a workbook and add a worksheet
        wb <- createWorkbook()
        addWorksheet(wb, "Sheet1")
        
        # Write data to the worksheet
        writeData(wb, "Sheet1", download_excel_file, headerStyle = hs1)
        
        # Apply decimal formatting to specific columns
        addStyle(
          wb,
          "Sheet1",
          style_1_decimal,
          rows = 2:(nrow(download_excel_file) + 1),
          cols = cols_1_decimals,
          gridExpand = TRUE
        )
        addStyle(
          wb,
          "Sheet1",
          style_3_decimals,
          rows = 2:(nrow(download_excel_file) + 1),
          cols = cols_3_decimals,
          gridExpand = TRUE
        )
        
        # Set column widths to auto
        setColWidths(
          wb,
          "Sheet1",
          cols = 1:ncol(download_excel_file),
          widths = "auto"
        )
        
        # Add borders to all columns
        addStyle(
          wb,
          "Sheet1",
          createStyle(border = "TopBottomLeftRight"),
          rows = 1:(nrow(download_excel_file) + 1),
          cols = 1:ncol(download_excel_file),
          gridExpand = TRUE
        )
        
        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
  
  output$downloadPNG <- downloadHandler(
    filename = "Facility CO2e Flow.png",
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
        caption <- paste0("Facility CO2e Flow for ", input$cname, "(" , un, ")")
      } else {
        caption <- paste0("Facility CO2e Flow ", "(" , un, ")")
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
    filename = "Facility Energy Flow.png",
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
      )
    }
  )
  
  output$downloadPNG <- downloadHandler(
    filename = "Facility CO2e Flow.png",
    content = function(file)
    {
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
        colourScale = JS("d3.scaleSequential(d3.interpolateViridis);")
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
      
      if (nchar(input$cname) > 0)
      {
        caption <- paste0("Facility Energy Flow for ",
                          input$cname_e,
                          "(" ,
                          input$units_e,
                          ")")
      } else
      {
        caption <- paste0("Facility Energy Flow ", "(" , input$units_e, ")")
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
}

shinyApp(ui, server)
