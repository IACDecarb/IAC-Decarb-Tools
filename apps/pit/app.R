library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(scales)
library(plotly)
library(ggrepel)
library(plotly)
library(openxlsx)
library(bslib)

# UI ----

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel("Pinch Heat Integration Tool"),
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
          downloadLink("downloadData1", "Download Pinch Heat Integration Tool - Input Sheet"),
          br(),
          br(),
          br(),
          fileInput("file", "Upload PIT - Input Sheet"),
          br(),
          tags$style(HTML("
      #downloadData2 {
        font-weight: bold;
        font-size: 16px;
      }
    ")),
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
          h3("Tab 2: Main Pinch"),
          tags$p("1. View the Shifted Composite Curve (SCC) diagram.", style = "font-size: 18px;"),
          tags$p("2. Add optional overlays.", style = "font-size: 18px;"),
          tags$p("3. Download the SCC diagram.", style = "font-size: 18px;"),
          br(),
          h3("Tab 3: GCC"),
          tags$p("1. View the Grand Composite Curve (GCC) diagram.", style = "font-size: 18px;"),
          tags$p("2. Download the GCC diagram.", style = "font-size: 18px;"),
          br(),
          h3("Tab 4: Heat Exchanger"),
          tags$p("1. Select streams to match using heat exchanger (HX).", style = "font-size: 18px;"),
          tags$p("2. Specify HX effectiveness.", style = "font-size: 18px;"),
          tags$p("3. Run analysis and download results", style = "font-size: 18px;"),
          br(),
          h3("Tab 5: Heat Pump"),
          tags$p("1. Select streams to match using heat pump (HP).", style = "font-size: 18px;"),
          tags$p("2. Select Coefficient of Performance (COP) calculation methodology.", style = "font-size: 18px;"),
          tags$p("3. Run technical analysis.", style = "font-size: 18px;"),
          tags$p("4. Specify economic and emissions inputs to perform economic and emission analysis.", style = "font-size: 18px;"),
          tags$p("5. Download results.", style = "font-size: 18px;"),
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
    tabPanel("Main Pinch",
             sidebarLayout(
               sidebarPanel(
                 tags$style(HTML("
      #downloadData1 {
        font-weight: bold;
        font-size: 16px;
      }
    ")),
                 
                 textInput('title', 'Add Pinch Title'),
                 numericInput("pinchdt", "Enter Pinch Temperature", 10),
                 selectInput('pt','Show Pinch Hot/Cold Side Temperatures', c('No','Yes')),
                 selectInput('hx','Show Heat Exchanger Overlap Region', c('No','Yes'), "Yes"),
                 selectInput('hp','Show Heat Pump Source and Sink Regions', c('No','Yes'), "Yes"),
                 selectInput('bg','Show High Temperature Heating Region', c('No','Yes')),
                 selectInput('hl','Show Stream Labels', c('No','Yes'),"Yes"),
                 numericInput("cop", HTML("Specify Heat Pump COP<sub>h</sub>"), 3),
                 numericInput("hxappmain","Specify Heat Exchanger Approach Temperature for Heat Pump HXs" , 5),
                 bsCollapse(id = "collapsePanel", open = "",
                            bsCollapsePanel(title = HTML("<div style='background-color: #98A4A4; padding: 10px; border-radius: 4px; text-align: left; width: 100%; margin: 0 auto;'><span style='font-size: 16px; color: white;'>Click here to adjust axes. &#9660;</span></div>"),
                                            numericInput("xaxmin","Decrease x-axis by" , 0),
                                            numericInput("xax","Specify x-axis limit" , 0),
                                            numericInput("yax","Specify y-axis limit" , 0)
                            )
                 ),
                 
                 downloadButton("downloadPNG", "Click Here to Download plot as Image"),
                 br(),
                 br(),
                 downloadButton("downloadtab", "Click Here to Download Pinch Summary Table")
                 
               ),
               mainPanel(
                 span(textOutput("error1"), style="color:red"),
                 plotOutput("pinch_plot", height = '600px') %>% withSpinner(color="#0dc5c1"),
                 span(textOutput("pinch_notes"), style="font-size: 12px"),
                 br(),
                 textOutput('ercode'),
                 br(),
                 br(),
                 uiOutput("tit"),
                 tableOutput("myTable"),
                 
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
    tabPanel("GCC",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("downloadgcc", "Click Here to Download plot as Image")
               ),
               mainPanel(
                 fluidRow(
                 ),
                 withSpinner(plotlyOutput("gcc_plot", height = '600px'), color = "#0dc5c1"),
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
    tabPanel("Heat Exchanger",
             sidebarLayout(
               sidebarPanel(
                 selectInput("stream1", "Choose Stream 1", character(0)),
                 selectInput("stream2", "Choose Stream 2", character(0)),
                 numericInput("eff", "Enter Heat Exchanger Effectiveness", min = 0.01, max = 1, value = 0.8),
                 actionButton("run", "Run Analysis"),
                 br(),
                 br(),
                 downloadButton("downloadtabhx", "Click Here to Heat Exchanger Results Table")
               ),
               mainPanel(
                 fluidRow(
                   div(
                     style = "
                     margin-left: 2%;
                     height: 500px;
                ",
                     img(
                       src = "hx.png",
                       style =  "width: 800px; height: 500px;"
                     ),
                     span(textOutput("thin"),
                          style = "
                      color: black;
                      position: relative;
                      top: -450px;
                      left: 250px;
                      font-size: 20px;
                    "
                     ),
                     span(textOutput("thout"),
                          style = "
                      color: black;
                      position: relative;
                      top: -85px;
                      left: 250px;
                      font-size: 20px;"
                     ),
                     span(textOutput("tcout"),
                          style = "
                color: black;
                 position: relative;
                     top: -507px;
                     left: 520px;
                          font-size: 20px;"
                     ),
                     span(textOutput("tcin"),
                          style = "
                color: black;
                 position: relative;
                     top: -141px;
                     left: 520px;
                          font-size: 20px;"
                     ),
                     span(textOutput("qexch"),
                          style = "
                color: black;
                 position: relative;
                     top: -380px;
                     left: 140px;
                          font-size: 20px;"
                     )
                   ),
                 ),
                 h4("Streams Data (Inputs)", align = "left"),
                 tableOutput("hxstreams"),
                 h4("Results Table", align = "left"),
                 tableOutput("resulthx"),
                 span(textOutput("errorhx"), style="color:red")
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
    tabPanel("Heat Pump", 
             sidebarLayout(
               sidebarPanel(
                 h3("Step 1: Technical Analysis"),
                 radioButtons("t_or_s", "Perform Heat Pump Simulation using:", choices = c('Streams','Temperature Levels')),
                 conditionalPanel(
                   condition = "input.t_or_s == 'Streams'",
                   selectInput("stream111", "Choose Source Stream", character(0)),
                   selectInput("stream222", "Choose Sink Stream", character(0)),
                   radioButtons('match', 'Match Streams:', choices = c("Completely", "Partially")),
                   conditionalPanel(
                     condition = "input.match == 'Partially'",
                     numericInput("match_input", "Enter Percentage of Source Heat to use (%)", min = 1, max = 100, value = 100)
                   ),
                   numericInput("hxapp", "Specify Heat Exchanger Approach Temperature", min = 0.01, max = 100, value = 10)
                 ),
                 conditionalPanel(
                   condition = "input.t_or_s == 'Temperature Levels'",
                   h5(em("Please run GCC Tab before using Temperature Levels as inputs.")),
                   numericInput("source_temp", "Select Source Temperature Level", value = 0),
                   numericInput("sink_temp", "Select Sink Temperature Level", value = 0)
                 ),
                 radioButtons('copmet','Select COP Calculation Methodolgy', choices = c("Calculate COP from Temperatures", "Enter Heat Pump COP")),
                 bsCollapse(id = "collapsePanel2", open = "",
                            bsCollapsePanel(title = "Enter Heat Pump COP",
                                            numericInput("cop2", "", min = 1, value = 3)),
                            bsCollapsePanel(title = "Calculate COP from Temperatures",
                                            selectInput("ref", "Select Refrigerant", choices = c('Ammonia (R717)','Water (R-718)','Propane (R-290)',
                                                                                                 'Propylene (R-1270)','n-Butane (R600)','isoButane (r600a)')),
                                            tags$small("(only available for select refrigerants and temperature ranges)")
                            )
                 ),
                 
                 actionButton("run2", "Run Technical Analysis"),
                 h3("Step 2: Energy Cost Analysis"),
                 selectInput("fuel_type", "Select Baseline Fuel", c("Natural Gas", "Propane", "Petroleum Coke", "Distillate or Light Fuel Oil", 
                                                                    "Coal", "Diesel", "Motor Gasoline")),
                 numericInput("ng_cost", "Enter Fuel Cost ($/MMBtu)", min = 0.001, value = 4),
                 numericInput("elec_cost", "Enter Electricity Cost ($/kW)", min = 0.001, value = 0.10),
                 numericInput("eff_heat", "Enter Efficiency of Thermal Unit (%)", min = 0.001,max = 100, value = 80),
                 numericInput("oper_hours", "Enter Annual Operating Hours", min = 1, value = 8000),
                 actionButton("run3", "Run Economic Analysis"),
                 tooltip(
                   h3("Step 3: Emissions Analysis"),
                   "This section uses fuel and electricity emission factors to compare emissions from natural gas equipment vs. heat pump."
                 ),
                 
                 numericInput("elec_ef", "Enter Electricity Emissions Factor (kg/kWh)", min = 0.001, value = 0.20),
                 actionButton("run4", "Run Emissions Analysis"),
                 br(),
                 br(),
                 downloadButton("downloadtabhp", "Click Here to Download Heat Pump Results Table"),
                 
               ),
               mainPanel(
                 fluidRow(
                   div(
                     style = "
                     margin-left: 2%;
                     height: 500px;
                ",
                     img(
                       src = "hp.png",
                       style =  "width: 1000px; height: 480px;"
                     ),
                     span(textOutput("thin_hp"),
                          style = "
                      color: black;
                      position: relative;
                      top: -90px;
                      left: 850px;
                      font-size: 20px;
                      width:40px;
                    "
                     ),
                     span(textOutput("thout_hp"),
                          style = "
                      color: black;
                      position: relative;
                      top: -440px;
                      left: 850px;
                      font-size: 20px;"
                     ),
                     span(textOutput("tcout_hp"),
                          style = "
                color: black;
                 position: relative;
                     top: -150px;
                     left: 110px;
                          font-size: 20px;"
                     ),
                     span(textOutput("tcin_hp"),
                          style = "
                color: black;
                 position: relative;
                     top: -500px;
                     left: 110px;
                          font-size: 20px;"
                     ),
                     span(textOutput("qsource"),
                          style = "
                color: black;
                 position: relative;
                     top: -380px;
                     left: 80px;
                          font-size: 15px;
                          width: 40px;"
                     ),
                     span(textOutput("qsink"),
                          style = "
                color: black;
                 position: relative;
                     top: -400px;
                     left: 850px;
                          font-size: 15px;"
                     ),
                     span(textOutput("tsource"),
                          style = "
                color: black;
                 position: relative;
                     top: -360px;
                     left: 370px;
                          font-size: 15px;"
                     ),
                     span(textOutput("tsink"),
                          style = "
                color: black;
                 position: relative;
                     top: -380px;
                     left: 605px;
                          font-size: 15px;"
                     ),
                     span(textOutput("winhp"),
                          style = "
                color: black;
                 position: relative;
                     top: -605px;
                     left: 460px;
                          font-size: 15px;"
                     )
                   ),
                 ),
                 span(textOutput("image_notes"), style="font-size: 12px"),
                 span(textOutput("stream_data"), style="font-size: 18px"),
                 tableOutput("hptable1"),
                 span(textOutput("technical_results"), style="font-size: 18px"),
                 tableOutput("resulthp"),
                 span(textOutput("hperror"), style="color:red"),
                 span(textOutput("economic_results"), style="font-size: 18px"),
                 tableOutput("econ_table"),
                 span(textOutput("emissions_results"), style="font-size: 18px"),
                 tableOutput("em_table")
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
             ))
  )
)
# Server ----

server <- function(input, output, session) {
  
  
  excelFilePath <- 'Pinch.xlsx'
  copFilePath <- 'cop_table.xlsx'
  docFilePath <- 'User Guide for Pinch Heat Integration Tool.pdf'
  
  observeEvent(input$copmet, ({
    if (input$copmet == "Calculate COP from Temperatures") {
      updateCollapse(session, "collapsePanel2",open = "Calculate COP from Temperatures")
    } else {
      updateCollapse(session, "collapsePanel2", open = "Enter Heat Pump COP")
    }
  }))
  
  observeEvent(input$file, {
    updateTabsetPanel(session, "tabs", selected = "Main Pinch")
  })
  
  # Page 1 - Main Pinch ----
  
  hs <- reactiveValues()
  
  hs$c1l <- 0
  hs$c1h <- 1
  hs$c2l <- 0
  hs$c2h <- 1
  
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
  
  hold.h <- reactive({
    req(input$file)
    req(input$pinchdt)
    streams_data.h <- read_excel(input$file$datapath, sheet = "Streams")
    clean_names(streams_data.h)
  })
  
  hold <- reactive({
    req(input$file)
    req(input$pinchdt)
    hold.h <- hold.h()
    hold <- hold.h %>% 
      slice(-1)
    hold %>% 
      mutate(tin  = as.numeric(tin)) %>% 
      mutate(tout  = as.numeric(tout)) %>% 
      mutate(q  = as.numeric(q))
  })
  
  uni <- reactive ({
    req(input$file)
    req(input$pinchdt)
    hold.h <- hold.h()
    hold.h %>% 
      slice(1) %>% 
      select(tin,tout,q)
  })
  
  needs_cooling <- reactive({
    req(input$file)
    req(input$pinchdt)
    hold <- hold()
    hold <- hold %>% 
      filter(stream_type == "Needs Cooling")
    for (ii in  1:nrow(hold)) {
      if (hold$tin[ii] == hold$tout[ii]) {
        hold$tout[ii] = hold$tout[ii] - 0.0000001 
      }
    }
    hold
  })
  
  needs_heating <- reactive({
    req(input$file)
    req(input$pinchdt)
    hold <- hold()
    hold <- hold %>% 
      filter(stream_type == "Needs Heating")
    for (ii in  1:nrow(hold)) {
      if (hold$tin[ii] == hold$tout[ii]) {
        hold$tout[ii] = hold$tout[ii] + 0.0000001 
      }
    }
    hold
  })
  
  forPlot.c <- reactive({
    req(input$file)
    req(input$pinchdt)
    needs_cooling <- needs_cooling()
    tinc <- needs_cooling %>% 
      select('temps' = tin)%>% 
      unique()
    toutc <- needs_cooling %>% 
      select('temps' =tout)%>% 
      unique()
    df2 <- bind_rows(tinc, toutc) %>% 
      arrange(desc(temps)) 
    
    new_df1c <- df2[1:(nrow(df2) - 1), ]
    new_df2c <- df2[-1, ]
    
    cooling_comp <- bind_cols(new_df1c,new_df2c) %>% 
      rename('tin' = 'temps...1') %>% 
      rename('tout' = 'temps...2') %>% 
      arrange(tout,tin)
    for (i in 1:nrow(cooling_comp)) {
      q.tot <- 0
      for (j in 1:nrow(needs_cooling)) {
        if(cooling_comp[i, "tin"] != cooling_comp[i, "tout"]){
          if (cooling_comp[i, "tin"] <= needs_cooling[j,"tin"] & (cooling_comp[i, "tout"] >= needs_cooling[j,"tout"])){
            q.hold <- ((cooling_comp[i, "tout"]-(cooling_comp[i, "tin"]))/(needs_cooling[j,"tout"]-needs_cooling[j,"tin"]))*needs_cooling[j,"q"]
            q.tot <- q.tot+q.hold
          }
        }
        if(cooling_comp[i, "tin"] == cooling_comp[i, "tout"]){
          if(cooling_comp[i, "tin"] == needs_cooling[j, "tout"]){
            q.tot <- needs_cooling[j, "q"]
          }
        }
        cooling_comp[i, "q"] <- q.tot
      }
    }
    forPlot.c.hold <- cooling_comp
    
    forPlot.c.hold[nrow(forPlot.c.hold)+1, "tout"] <- forPlot.c.hold[nrow(forPlot.c.hold), "tin"]
    
    forPlot.c <- forPlot.c.hold %>% 
      select(-tin) %>% 
      mutate(q_cum = 0)
    
    for (i in 2:nrow(forPlot.c)) {
      forPlot.c[i, "q_cum"] <- forPlot.c[i-1, "q"] + forPlot.c[i-1, "q_cum"]
    }
    
    forPlot.c %>% 
      select(-q)
  })
  
  forPlot.h1 <- reactive({
    req(input$file)
    req(input$pinchdt)
    needs_heating <- needs_heating()
    forPlot.c <- forPlot.c()
    tin <- needs_heating %>% 
      select('temps' = tin)%>% 
      unique()
    tout <- needs_heating %>% 
      select('temps' = tout)%>% 
      unique()
    df <- bind_rows(tin, tout) %>% 
      arrange(temps)
    
    new_df1 <- df[1:(nrow(df) - 1), ]
    new_df2 <- df[-1, ]
    
    heating_comp <- bind_cols(new_df1,new_df2) %>% 
      rename('tin' = 'temps...1') %>% 
      rename('tout' = 'temps...2') %>% 
      mutate('q' = 0) %>% 
      arrange(tin, tout)
    for (i in 1:nrow(heating_comp)) {
      q.tot <- 0
      for (j in 1:nrow(needs_heating)) {
        if(heating_comp[i, "tin"] != heating_comp[i, "tout"]){
          if (heating_comp[i, "tin"] >= needs_heating[j,"tin"] & (heating_comp[i, "tout"] <= needs_heating[j,"tout"])){
            q.hold <- ((heating_comp[i, "tout"]-(heating_comp[i, "tin"]))/(needs_heating[j,"tout"]-needs_heating[j,"tin"]))*needs_heating[j,"q"]
            q.tot <- q.tot+q.hold
          }
        }
        if(heating_comp[i, "tin"] == heating_comp[i, "tout"]){
          if(heating_comp[i, "tin"] == needs_heating[j, "tout"]){
            q.tot <- needs_heating[j, "q"]
          }
        }
        heating_comp[i, "q"] <- q.tot
      }
    }
    forPlot.h.hold <- heating_comp
    
    forPlot.h.hold[nrow(forPlot.h.hold)+1, "tin"] <- forPlot.h.hold[nrow(forPlot.h.hold), "tout"]
    
    forPlot.hh <- forPlot.h.hold %>% 
      select(-tout) %>% 
      mutate(q_cum = 0)
    
    for (i in 2:nrow(forPlot.hh)) {
      forPlot.hh[i, "q_cum"] <- forPlot.hh[i-1, "q"] + forPlot.hh[i-1, "q_cum"]
    }
    heatshift <- max(forPlot.c$q_cum)
    forPlot.h1 <- forPlot.hh %>% 
      select(-q) %>% 
      mutate(q_cum = q_cum + heatshift)
  })
  
  
  observe({
    req(input$file)
    req(input$pinchdt)
    pinchdt <- input$pinchdt
    forPlot.h <- forPlot.h1()
    forPlot.c <- forPlot.c()
    uni <- uni()
    heatshift <- max(forPlot.c$q_cum)
    delta <- max(forPlot.c$q_cum) / 500
    mdiffh <- 1000
    mdiffc <- 1000
    mdiff <- 1000
    k = 0;
    
    
    
    while (mdiffc > pinchdt) {
      k = k + 1
      heatshift <- k * delta
      forPlot.shift <- forPlot.h %>% 
        mutate(q_cum = q_cum - heatshift)
      
      for (l in nrow(forPlot.c):1) {
        for (m in 1:(nrow(forPlot.shift) - 1)) {
          if (forPlot.c[[l, "q_cum"]] >= forPlot.shift[[m, "q_cum"]] & 
              forPlot.c[[l, "q_cum"]] <= forPlot.shift[[m + 1, "q_cum"]]) {
            y <- forPlot.c[[l, "tout"]]
            x <- forPlot.c[[l,'q_cum']]
            slope <- (forPlot.shift[[m + 1, "tin"]] - 
                        forPlot.shift[[m, "tin"]]) / (forPlot.shift[[m + 1, "q_cum"]] - 
                                                        forPlot.shift[[m, "q_cum"]])
            intercept <- forPlot.shift[[m, "tin"]] - slope * forPlot.shift[[m, "q_cum"]]
            if (slope == Inf) {
              y.h <- forPlot.shift[[m, "tin"]]
            } else {
              y.h <- intercept + slope * x
            }
            dist <- abs(y - y.h)
            
            if (dist < mdiffc) {
              mdiffc = dist
              if (mdiffc <= pinchdt) {
                break  # Exit the inner loop when mdiffc < pinchdt
              }
              if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
                break  # Exit the inner loop when mdiffc < pinchdt
              }
            }
          }
        }
        if (mdiffc <= pinchdt) {
          break  # Exit the outer loop when mdiffc < pinchdt
        }
        if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
          break  # Exit the inner loop when mdiffc < pinchdt
        }
      }
      heatshift_c <- heatshift
      xc_co <- x
      yc_co <- y
      if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
        break  # Exit the inner loop when mdiffc < pinchdt
      }
    }
    hs$c1l <- forPlot.c[[1, "q_cum"]]
    hs$c1h <- forPlot.shift[[nrow(forPlot.shift), "q_cum"]]
    diffc <- abs(pinchdt - mdiffc)
    hs$heatshift_c <- heatshift_c
    hs$xc_co <- x
    hs$yc_co <- y
    hs$diffc <- diffc
    
    heatshift <- max(forPlot.c$q_cum)
    mdiffh <- 1000
    k = 0;
    
    while (mdiffh > pinchdt) {
      k = k + 1
      heatshift <- k * delta
      forPlot.shift <- forPlot.h %>% 
        mutate(q_cum = q_cum - heatshift)
      
      for (l in 1:nrow(forPlot.shift)) {
        for (m in (nrow(forPlot.c) - 1):1) {
          if (forPlot.shift[[l, "q_cum"]] >= forPlot.c[[m, "q_cum"]] & forPlot.shift[[l, "q_cum"]] <= 
              forPlot.c[[m + 1, "q_cum"]]) {
            y <- forPlot.shift[[l, "tin"]]
            x <- forPlot.shift[[l,'q_cum']]
            slope <- (forPlot.c[[m + 1, "tout"]] - forPlot.c[[m, "tout"]]) / (forPlot.c[[m + 1, "q_cum"]] - 
                                                                                forPlot.c[[m, "q_cum"]])
            intercept <- forPlot.c[[m, "tout"]] - (slope * forPlot.c[[m, "q_cum"]])
            if (slope == Inf) {
              y.h <- forPlot.c[[m, "tout"]]
            } else {
              y.h <- intercept + slope * x
            }
            dist <- abs(y - y.h)
            if (dist < mdiffh) {
              mdiffh = dist
              if (mdiffh <= pinchdt) {
                break  # Exit the inner loop when mdiffc < pinchdt
              }
              if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
                break  # Exit the inner loop when mdiffc < pinchdt
              }
            }
          }
        }
        if (mdiffh <= pinchdt) {
          break  # Exit the outer loop when mdiffc < pinchdt
        }
        if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
          break  # Exit the inner loop when mdiffc < pinchdt
        }
      }
      heatshift_h <- heatshift
      xh_co <- x
      yh_co <- y
      if (forPlot.c[[1, "q_cum"]] > forPlot.shift[[nrow(forPlot.shift), "q_cum"]]) {
        break  # Exit the inner loop when mdiffc < pinchdt
      }
    }
    hs$c2l <- forPlot.c[[1, "q_cum"]]
    hs$c2h <- forPlot.shift[[nrow(forPlot.shift), "q_cum"]]
    
    heatshift_c <-  hs$heatshift_c
    
    heatshift <- ifelse(heatshift_c>heatshift_h, heatshift_h, heatshift_c)
    diffh <- abs(pinchdt - mdiffh)
    hs$heatshift_h <- heatshift_h
    hs$xh_co <- x
    hs$yh_co <- y
    hs$diffh <- diffh
    hs$heatshift <- heatshift
  })
  
  
  coor <- reactiveValues()
  
  observe({
    req(input$file)
    req(input$pinchdt)
    pinchdt <- input$pinchdt
    if (hs$heatshift == hs$heatshift_h) {
      x_coor <- hs$xh_co
      y_coor <- hs$yh_co
      p_lab <- pinchdt
    } else {
      x_coor <- hs$xc_co
      y_coor <- hs$yc_co
      p_lab <- -pinchdt
    }
    coor$x_coor <- x_coor
    coor$y_coor <- y_coor
    coor$p_lab <- p_lab
    
    if (y_coor > (y_coor+p_lab)) {
      coor$hst <- y_coor
      coor$cst <- y_coor+p_lab
    } else {
      coor$cst <- y_coor
      coor$hst <- y_coor+p_lab
    }
  }) 
  
  
  forPlot.h <- reactive({
    req(input$file)
    req(input$pinchdt)
    if (hs$c1l > hs$c1h & hs$c2l > hs$c2h){
      forPlot.h <- forPlot.h1()
      forPlot.h
    } else {
      forPlot.h <- forPlot.h1()
      heatshift <- hs$heatshift
      forPlot.h %>% 
        mutate(q_cum = q_cum - heatshift)
    }
    
  })
  
  all_q <- reactiveValues()
  
  observe({
    req(input$file)
    req(input$pinchdt)
    forPlot.c <- forPlot.c()
    forPlot.h <- forPlot.h()
    all_q$qhx <-  ifelse (min(forPlot.h$q_cum) >=0,round(abs(max(forPlot.c$q_cum)- abs(min(forPlot.h$q_cum))),1),
                          round(abs(max(forPlot.c$q_cum)) - abs(min(forPlot.c$q_cum)),1))
    
    all_q$qhp.so <-  round(abs(min(forPlot.h$q_cum) - min(forPlot.c$q_cum)),1)
    all_q$qhp.si.h <- round((all_q$qhp.so/(input$cop-1))*input$cop,1)
    all_q$qhp.poss <- max(forPlot.h$q_cum) - max(forPlot.c$q_cum)
    all_q$qhp.si <- round(min(all_q$qhp.poss,all_q$qhp.si.h),1)
    all_q$qbg <-  round(abs(max(forPlot.h$q_cum) - (max(forPlot.c$q_cum)+all_q$qhp.si)),1)
  })
  
  lb <- reactive({
    req(input$file)
    req(input$pinchdt)
    forPlot.h <- forPlot.h()
    needs_heating <- needs_heating()
    hl.hold <- tibble(
      'a' = 0L,
      's.nam' = ""
    )
    s.hold <- c()
    for(a in 1:(nrow(forPlot.h)-1)) {
      hl.hold[a,'a'] <- a
      hl.hold.h <- c()
      for(b in 1:nrow(needs_heating)){
        if(forPlot.h[[a, "tin"]] >= needs_heating[[b, "tin"]] &
           forPlot.h[[a+1, "tin"]] <= needs_heating[[b, "tout"]]){
          s.hold <- needs_heating[[b, "stream_no"]]
          hl.hold.h <- c(hl.hold.h,s.hold)
          hl.hold.h <- unique(hl.hold.h)
        }
        
      }
      name.hold <- paste0(hl.hold.h,collapse = ';')
      hl.hold[a,'s.nam'] <- name.hold
    }
    lb <- tibble(
      'pos.x' = 0,
      'pos.y' = 0,
      'lab' = ""
    )
    for (p in 1:(nrow(forPlot.h)-1)) {
      lb[p,'pos.x'] = (forPlot.h[[p,'q_cum']]+forPlot.h[[p+1,'q_cum']])/2
      lb[p,'pos.y'] = ((forPlot.h[[p,'tin']]+forPlot.h[[p+1,'tin']])/2) - 6
      lb[p,'lab'] = paste0(hl.hold[[p,'s.nam']])
    }
    lb
  })
  
  lb.c <- reactive({
    req(input$file)
    req(input$pinchdt)
    forPlot.c <- forPlot.c()
    needs_cooling <- needs_cooling()
    cl.hold <- tibble(
      'c' = 0L,
      's.nam.c' = ""
    )
    cs.hold <- c()
    for(a in 1:(nrow(forPlot.c)-1)) {
      cl.hold[a,'c'] <- a
      cl.hold.h <- c()
      for(b in 1:nrow(needs_cooling)){
        if(forPlot.c[[a, "tout"]] >= needs_cooling[[b, "tout"]]   &
           forPlot.c[[a+1, "tout"]] <= needs_cooling[[b, "tin"]]){
          cs.hold <- needs_cooling[[b, "stream_no"]]
          cl.hold.h <- c(cl.hold.h,cs.hold)
          cl.hold.h <- unique(cl.hold.h)
        }
      }
      name.hold.c <- paste0(cl.hold.h,collapse = ';')
      cl.hold[a,'s.nam.c'] <- name.hold.c
    }
    
    lb.c <- tibble(
      'pos.xc' = 0,
      'pos.yc' = 0,
      'labc' = ""
    )
    for (p in 1:(nrow(forPlot.c)-1)) {
      lb.c[p,'pos.xc'] = (forPlot.c[[p,'q_cum']]+forPlot.c[[p+1,'q_cum']])/2
      lb.c[p,'pos.yc'] = ((forPlot.c[[p,'tout']]+forPlot.c[[p+1,'tout']])/2) + 6
      lb.c[p,'labc'] = paste0(cl.hold[[p,'s.nam.c']])
    }
    lb.c
  })
  
  lb.hp.so <- reactive({
    req(input$file)
    req(input$pinchdt)
    lb.c <- lb.c()
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    lb.hp.so <- c()
    for (o in 1:nrow(lb.c)) {
      lab.hps.h <- c()
      if (forPlot.c[[o,'q_cum']] < forPlot.h[[1,'q_cum']]){
        lab.hps.h <- lb.c[o,'labc']
      }
      lb.hp.so <- c(lb.hp.so,lab.hps.h)
    }
    lb.hp.so <- paste0(lb.hp.so, collapse = ';')
    elements <- unlist(strsplit(lb.hp.so, ";"))
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hp.so <- unique(numeric_vector)
    lb.hp.so <- paste0(lb.hp.so, collapse = ';')
    lb.hp.so
  })
  
  lb.hx.c <- reactive({
    req(input$file)
    req(input$pinchdt)
    lb.c <- lb.c()
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    lb.hx.c <- c()
    for (o in 1:nrow(lb.c)) {
      lab.hx.h <- c()
      if (((forPlot.c[[o,'q_cum']] >= forPlot.h[[1,'q_cum']]) & 
           (forPlot.c[[o,'q_cum']] <= (forPlot.h[[1,'q_cum']] + all_q$qhx))) |
          ((forPlot.c[[o,'q_cum']] <= forPlot.h[[1,'q_cum']]) & 
           (forPlot.c[[o+1,'q_cum']] >= (forPlot.h[[1,'q_cum']])))
      ) {
        lab.hx.h <- lb.c[o,'labc']
      }
      lb.hx.c <- c(lb.hx.c,lab.hx.h)
    }
    lb.hx.c <- paste0(lb.hx.c, collapse = ';')
    elements <- unlist(strsplit(lb.hx.c, ";"))
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hx.c <- unique(numeric_vector)
    lb.hx.c <- paste0(lb.hx.c, collapse = ';')
    
    lb.hx.c
  })
  
  lb.hx.nh <- reactive({
    req(input$file)
    req(input$pinchdt)
    lb <- lb()
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    lb.hx.nh <- c()
    for (o in 1:nrow(lb)) {
      lab.hx.h <- c()
      if (((forPlot.h[[o,'q_cum']] <= forPlot.c[[nrow(forPlot.c),'q_cum']]) & 
           (forPlot.h[[o,'q_cum']] >= (forPlot.c[[nrow(forPlot.c),'q_cum']] - all_q$qhx))) |
          ((forPlot.h[[o,'q_cum']] < forPlot.c[[nrow(forPlot.c),'q_cum']]) & 
           (forPlot.h[[o+1,'q_cum']] > (forPlot.c[[nrow(forPlot.c),'q_cum']])))
      ){
        lab.hx.h <- lb[o,'lab']
      }
      lb.hx.nh <- c(lb.hx.nh,lab.hx.h)
    }
    lb.hx.nh <- paste0(lb.hx.nh, collapse = ';')
    elements <- unlist(strsplit(lb.hx.nh, ";"))
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hx.nh <- unique(numeric_vector)
    lb.hx.nh <- paste0(lb.hx.nh, collapse = ';')
    
    lb.hx.nh
  })
  
  lb.hp.si <- reactive({
    req(input$file)
    req(input$pinchdt)
    lb <- lb()
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    lb.hp.si <- c()
    for (o in 1:nrow(lb)) {
      lab.hx.h <- c()
      if (((forPlot.h[[o,'q_cum']] >= forPlot.c[[nrow(forPlot.c),'q_cum']]) & 
           (forPlot.h[[o,'q_cum']] <= (forPlot.c[[nrow(forPlot.c),'q_cum']] + all_q$qhp.si))) |
          ((forPlot.h[[o,'q_cum']] <= forPlot.c[[nrow(forPlot.c),'q_cum']]) & 
           (forPlot.h[[o+1,'q_cum']] >= (forPlot.c[[nrow(forPlot.c),'q_cum']])))
      ){
        lab.hx.h <- lb[o,'lab']
      }
      lb.hp.si <- c(lb.hp.si,lab.hx.h)
    }
    lb.hp.si <- paste0(lb.hp.si, collapse = ';')
    elements <- unlist(strsplit(lb.hp.si, ";"))
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hp.si <- unique(numeric_vector)
    lb.hp.si <- paste0(lb.hp.si, collapse = ';')
    lb.hp.si
  })
  
  lb.ht <- reactive({
    req(input$file)
    req(input$pinchdt)
    lb <- lb()
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    lb.ht <- c()
    for (o in 1:nrow(lb)) {
      lab.hx.h <- c()
      if (((forPlot.h[[o,'q_cum']] > (forPlot.c[[nrow(forPlot.c),'q_cum']] + all_q$qhp.si)))|
          ((forPlot.h[[o,'q_cum']] <= (forPlot.c[[nrow(forPlot.c),'q_cum']] + all_q$qhp.si))&
           (forPlot.h[[o+1,'q_cum']] > (forPlot.c[[nrow(forPlot.c),'q_cum']] + all_q$qhp.si)))
      ){
        lab.hx.h <- lb[o,'lab']
      }
      lb.ht <- c(lb.ht,lab.hx.h)
    }
    lb.ht <- paste0(lb.ht, collapse = ';')
    elements <- unlist(strsplit(lb.ht, ";"))
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.ht <- unique(numeric_vector)
    lb.ht <- paste0(lb.ht, collapse = ';')
    lb.ht
  })
  
  wavg.source <- reactive({
    req(input$file)
    req(input$pinchdt)
    forPlot.c <- forPlot.c()
    forPlot.h <- forPlot.h()
    wavg.source <- min(forPlot.c$tout)
    wavg.source <- round(wavg.source,1)-input$hxappmain
    wavg.source
  })
  
  wavg.sink <- reactive({
    req(input$file)
    req(input$pinchdt)
    forPlot.c <- forPlot.c()
    forPlot.h <- forPlot.h()
    
    wavg.h <- tibble(
      'dt' = c(),
      'q.h' = c()
    )
    t_init <- 0
    for (j in 1:(nrow(forPlot.h)-1)){
      if(forPlot.h[[j,'q_cum']]<max(forPlot.c$q_cum) & forPlot.h[[j+1,'q_cum']]>max(forPlot.c$q_cum)){
        slp <- (forPlot.h[[j+1,'tin']]-forPlot.h[[j,'tin']])/(forPlot.h[[j+1,'q_cum']]-forPlot.h[[j,'q_cum']])
        yicp <- forPlot.h[[j+1,'tin']]-(slp*forPlot.h[[j+1,'q_cum']])
        tempot <- slp*max(forPlot.c$q_cum)+yicp
        wavg.h[j,'dt'] <- forPlot.h[[j+1,'tin']] - tempot
        wavg.h[j,'q.h'] <- forPlot.h[[j+1,'q_cum']] - max(forPlot.c$q_cum)
        t_init <- tempot
      }
      else if (forPlot.h[[j,'q_cum']]>=max(forPlot.c$q_cum) & forPlot.h[[j+1,'q_cum']]<(max(forPlot.c$q_cum)+all_q$qhp.si)){
        wavg.h[j,'dt'] <- forPlot.h[[j+1,'tin']]-forPlot.h[[j,'tin']]
        wavg.h[j,'q.h'] <- forPlot.h[[j+1,'q_cum']]-forPlot.h[[j,'q_cum']]
      }
      else if (forPlot.h[[j+1,'q_cum']]>=(max(forPlot.c$q_cum)+all_q$qhp.si) & (forPlot.h[[j,'q_cum']]<(max(forPlot.c$q_cum)+all_q$qhp.si))){
        slp <- (forPlot.h[[j+1,'tin']]-forPlot.h[[j,'tin']])/(forPlot.h[[j+1,'q_cum']]-forPlot.h[[j,'q_cum']])
        yicp <- forPlot.h[[j+1,'tin']]-slp*forPlot.h[[j+1,'q_cum']]
        tempot1 <- slp*(max(forPlot.c$q_cum)+all_q$qhp.si)+yicp
        wavg.h[j,'dt'] <- tempot1 - forPlot.h[[j,'tin']]
        wavg.h[j,'q.h'] <- (max(forPlot.c$q_cum)+all_q$qhp.si) - forPlot.h[[j,'q_cum']]
      }
      else {
        wavg.h[j,'dt'] <- 0
        wavg.h[j,'q.h'] <- 0
      }
    }
    if (hs$c1l > hs$c1h & hs$c2l > hs$c2h) {
      t_init = max(forPlot.c$tout)
    }
    
    
    wavg.sink <- sum(wavg.h$dt)+t_init+input$hxappmain
    wavg.sink <- round(wavg.sink,1)
    wavg.sink
  })
  
  data <- reactive({
    req(input$file)
    req(input$pinchdt)
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    data <- tibble(
      'Metric' = c('Heat Exchange Potential','Heat Pump - Source Potential','Heat Pump - Source Temperature',
                  'Heat Pump - Sink Potential','Heat Pump - Sink Temperature','High Temperature Heating Requirement','Heat Pump Source Streams','Heat Exchange Streams - Needs Cooling',
                  'Heat Exchange Streams - Needs Heating','Heat Pump Sink Streams','High Temperature Heating Streams'),
      'Value' = c(all_q$qhx,all_q$qhp.so,wavg.source(),all_q$qhp.si,wavg.sink(),all_q$qbg, lb.hp.so(),lb.hx.c(), lb.hx.nh(), lb.hp.si(),lb.ht()),
      'Units' = c(q_uni,q_uni,t_uni,q_uni,t_uni,q_uni,'','','','','')
    )
    data
  })
  
  output$myTable <- renderTable({
    
    req(hs$c1l < hs$c1h | hs$c2l < hs$c2h)
    data()
  })
  
  output$error1 <- renderText({
    req(hs$c1l > hs$c1h & hs$c2l > hs$c2h)
    "Error: Pinch Temperature too low, can not be achieved"})
  
  pinch1 <- reactive({
    req(input$file)
    req(input$pinchdt)
    output$ercode <- renderText("")
    
    
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    needs_heating <- needs_heating()
    needs_cooling <- needs_cooling()
    uni <- uni()
    x_coor <-  coor$x_coor
    y_coor <-  coor$y_coor
    p_lab <- coor$p_lab 
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    pinchdt <- input$pinchdt
    qhx <- all_q$qhx
    cst <- coor$cst
    hst <- coor$hst
    qhp.so <- all_q$qhp.so
    qhp.si <- all_q$qhp.si
    qbg <- all_q$qbg
    qhx.l <- round(all_q$qhx,1)
    cst.l <- round(coor$cst,1)
    hst.l <- round(coor$hst,1)
    qhp.so.l <- round(all_q$qhp.so,1)
    qhp.si.l <- round(all_q$qhp.si,1)
    qbg.l <- round(all_q$qbg,1)
    lb <- lb()
    lb.c <- lb.c()
    wavg.source.l <- wavg.source()
    wavg.sink.l <- wavg.sink()
    if (input$yax == 0){
      y_max_limit <- max(max(forPlot.h$tin),max(forPlot.c$tout))+30
    } else {
      y_max_limit <- input$yax
    }
    
    if (input$xax == 0){
      x_max_limit <- max(max(forPlot.h$q_cum),max(forPlot.c$q_cum))+(max(max(forPlot.h$q_cum),max(forPlot.c$q_cum)))*0.05
    } else {
      x_max_limit <- input$xax
    }
    y_min = min(0,min(forPlot.c$tout) - 5,min(forPlot.h$tin) - 5)
    
    tit <- paste0('Pinch Analysis for ',input$Metric)
    pinch <- ggplot() +
      geom_line(data = forPlot.c, aes(x = q_cum, y = tout, color = "Needs Cooling"),arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
      geom_line(data = forPlot.h, aes(x = q_cum, y = tin, color = "Needs Heating"), arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
      xlab(paste0('Q (', q_uni, ')')) +
      ylab(paste0('Temperature (', t_uni, ')')) +
      labs(color = "Stream Type") +  # Set the legend title
      theme_bw() +
      theme(
        panel.grid.minor = element_line(color = "gray", linewidth = 0.1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size=16), 
        legend.text = element_text(size = 15), 
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      scale_y_continuous(limits = c(y_min, y_max_limit)) +
      scale_x_continuous(limits = c(0, x_max_limit)) +
      scale_color_manual(values = c("Needs Cooling" = "red", "Needs Heating" = "blue"))+
      ggtitle(tit)# Set line colors and legend labels
    
    if (hs$c1l < hs$c1h | hs$c2l < hs$c2h) {
      pinch <- pinch +
        geom_segment(aes(x = x_coor, xend = x_coor, y = y_coor, yend = y_coor + p_lab))}
    
    if (input$pt == 'Yes') {
      
      pinch <- pinch +  
        geom_hline(yintercept = y_coor, linetype = "dashed", color = "steelblue")+
        geom_hline(yintercept = y_coor+p_lab, linetype = "dashed", color = "steelblue")+
        geom_text(
          aes(
            x = 5,
            y = hst,
            label = paste0('Pinch - Hot Side T = ',hst.l)
          ),
          vjust = -0.1,
          hjust = 0
        )+
        geom_text(
          aes(
            x = 5,
            y = cst,
            label = paste0('Pinch - Cold Side T = ',cst.l)
          ),
          vjust = 1.2,
          hjust = 0
        )
    }
    
    if (input$hx == 'Yes') {
      t1 <- paste0("Qhx\n",comma(qhx.l)," ", q_uni)
      
      ifelse (min(forPlot.h$q_cum) >=0, 
              xint1 <- min(forPlot.h$q_cum),
              xint1 <- min(forPlot.c$q_cum)
      )
      
      
      ifelse (min(forPlot.h$q_cum) >=0, xint2 <- max(forPlot.c$q_cum),xint2 <- max(forPlot.c$q_cum))
      pinch <- pinch +  
        geom_vline(xintercept = xint1, linetype = "dashed", color = "steelblue")+
        geom_vline(xintercept = xint2, linetype = "dashed", color = "steelblue")+
        
        geom_segment(aes(x=xint1,
                         xend=xint2,
                         y=y_max_limit-20,
                         yend=y_max_limit-20),
                     color = 'steelblue',
                     arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "both", angle = 15))+
        geom_text(
          aes(
            x = (xint1 + xint2) / 2,
            y = y_max_limit-20,
            label = t1
          ),
          vjust = -0.2
        )
    }
    if (input$hp == 'Yes') {
      
      t2 <- paste0("Qhp Source\n",comma(qhp.so.l)," ", q_uni)
      
      t4 <- paste0("Qhp Sink\n",comma(qhp.si.l)," ", q_uni)
      
      
      t3 <- paste0("T Source\n",wavg.source.l, t_uni)
      t5 <- paste0("T Sink\n",wavg.sink.l, t_uni)
      
      pinch <- pinch +  
        geom_vline(xintercept = min(forPlot.h$q_cum), linetype = "dashed", color = "green")+
        geom_vline(xintercept = min(forPlot.c$q_cum), linetype = "dashed", color = "green")+
        geom_segment(aes(x=min(forPlot.h$q_cum),
                         xend=min(forPlot.c$q_cum),
                         y=y_max_limit-20,
                         yend=y_max_limit-20),
                     color = 'green',
                     arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "both", angle = 15))+
        geom_vline(xintercept = max(forPlot.c$q_cum), linetype = "dashed", color = "green")+
        geom_vline(xintercept = max(forPlot.c$q_cum)+qhp.si, linetype = "dashed", color = "green")+
        geom_text(
          aes(
            x = (min(forPlot.h$q_cum) + min(forPlot.c$q_cum)) / 2,
            y = y_max_limit-20,
            label = t2
          ),
          vjust = -0.2
        )+
        geom_text(
          aes(
            x = (min(forPlot.h$q_cum) + min(forPlot.c$q_cum)) / 2,
            y = y_max_limit-20,
            label = t3
          ),
          vjust = 1.2
        )+
        geom_segment(aes(x=max(forPlot.c$q_cum),
                         xend=max(forPlot.c$q_cum)+qhp.si,
                         y=y_max_limit-20,
                         yend=y_max_limit-20),
                     color = 'palegreen',
                     arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "both", angle = 15))+
        geom_text(
          aes(
            x = (max(forPlot.c$q_cum) + (max(forPlot.c$q_cum)+qhp.si)) / 2,
            y = y_max_limit-20,
            label = t4
          ),
          vjust = -0.2
        )+
        geom_text(
          aes(
            x = (max(forPlot.c$q_cum) + (max(forPlot.c$q_cum)+qhp.si)) / 2,
            y = y_max_limit-20,
            label = t5
          ),
          vjust = 1.2
        )
    }
    if (input$bg == 'Yes') {
      
      if(qbg == 0) {
        output$ercode <- renderText("External Heating not required, demand can be fulfilled using Heat Pumps")
      }
      else 
      {
        t6 <- paste0("Qht\n",comma(qbg.l)," ", q_uni)
        pinch <- pinch +  
          geom_vline(xintercept = max(forPlot.h$q_cum), linetype = "dashed", color = "blue")+
          geom_vline(xintercept = max(forPlot.c$q_cum)+qhp.si, linetype = "dashed", color = "blue")+
          geom_segment(aes(x=max(forPlot.h$q_cum),
                           xend=max(forPlot.c$q_cum)+qhp.si,
                           y=y_max_limit-20,
                           yend=y_max_limit-20),
                       color = 'blue',
                       arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "both", angle = 15))+
          geom_text(
            aes(
              x = (max(forPlot.h$q_cum) + (max(forPlot.c$q_cum)+qhp.si)) / 2,
              y = y_max_limit-20,
              label = t6
            ),
            vjust = -0.2
          )
      }
      
    }
    
    if (input$hl == 'Yes') {
      
      pinch <- pinch +
        geom_text_repel(data = lb, aes(x = pos.x, y = pos.y, label = lab)) +
        geom_text_repel(data = lb.c, aes(x = pos.xc, y = pos.yc, label = labc))
      
    }
    
    
    
    print(pinch)
  })
  
  output$pinch_plot <- renderPlot({
    pinch1()
  })
  
  
  output$pinch_notes <- renderText({
    req(input$file)
    req(input$pinchdt)
    "Notes: 1. Qhp source: Q Heat Pump Source. 2; Qhx: Q Heat Exchanger; 3. Qhp Sink: Q Heat Pump Sink;
                                  4. Qht: Q High Temperature Heating; \n
                                  5. T source: Source Temperature for Heat Pump; 6: T Sink: Sink Temperature for Heat Pump"})
  output$tit <- renderUI({
    req(input$file)
    req(input$pinchdt)
    # Create a div with inline CSS for bold, larger text, and left indent
    if (hs$c1l < hs$c1h | hs$c2l < hs$c2h)  {
      HTML("<div style='font-weight: bold; font-size: 24px; margin-left: 12px;'>Pinch Heat Integration Tool - Summary Table</div>")
    }
  })
  
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste("pinch_", input$title, ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = pinch1(), device = "png", height = 25, width = 37.5,
             units = "cm", dpi= 400)
    })
  output$downloadtab <- downloadHandler(
    filename = "Pinch Summary.csv",
    content = function(file) {
      write.csv(data(), file,fileEncoding="Windows-1252")
    })
  
  # Page 2 - GCC ----
  
  gcc_data <- reactiveValues()
  
  gcc_plot <- reactive({
    req(input$file)
    req(input$pinchdt)
    output$ercode <- renderText("")
    
    forPlot.h <- forPlot.h()
    forPlot.c <- forPlot.c()
    needs_heating <- needs_heating()
    needs_cooling <- needs_cooling()
    pinchdt = input$pinchdt
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    
    forPlot.c.shift <- forPlot.c %>% 
      mutate(tout = tout - (0.5*pinchdt))
    
    forPlot.h.shift <- forPlot.h %>% 
      mutate(tin = tin + (0.5*pinchdt))
    
    min_temp <- min(min(forPlot.h.shift$tin),min(forPlot.c.shift$tout))
    max_temp <- max(max(forPlot.h.shift$tin),max(forPlot.c.shift$tout))
    
    
    line_seg_fc<- tibble(
      'startx' = 0,
      'endx' = 0,
      'starty' = 0,
      'endy' = 0
    )
    for (g in 1:(nrow(forPlot.c.shift)-1)) {
      line_seg_fc[g,'startx'] <- forPlot.c.shift[g, "q_cum"]
      line_seg_fc[g,'endx'] <- forPlot.c.shift[g+1, "q_cum"]
      line_seg_fc[g,'starty'] <- forPlot.c.shift[g, "tout"]
      line_seg_fc[g,'endy'] <- forPlot.c.shift[g+1, "tout"]
    }
    
    line_seg_fc <- line_seg_fc %>% 
      mutate(slope = (endy-starty)/(endx-startx)) %>% 
      mutate(intercept = starty - (slope*startx))
    
    line_seg_fh<- tibble(
      'startx' = 0,
      'endx' = 0,
      'starty' = 0,
      'endy' = 0
    )
    for (g in 1:(nrow(forPlot.h.shift)-1)) {
      line_seg_fh[g,'startx'] <- forPlot.h.shift[g, "q_cum"]
      line_seg_fh[g,'endx'] <- forPlot.h.shift[g+1, "q_cum"]
      line_seg_fh[g,'starty'] <- forPlot.h.shift[g, "tin"]
      line_seg_fh[g,'endy'] <- forPlot.h.shift[g+1, "tin"]
    }
    
    line_seg_fh <- line_seg_fh %>% 
      mutate(slope = (endy-starty)/(endx-startx)) %>% 
      mutate(intercept = starty - (slope*startx))
    
    
    
    gcc <- tibble(
      'temp' = seq(min_temp,max_temp, by=0.1),
      'slopefh' = 0,
      'interceptfh' = 0,
      'slopefc' = 0,
      'interceptfc' = 0
    )
    
    for(p in 1:nrow(gcc)){
      for (q in 1:nrow(line_seg_fh)) {
        if(gcc[[p,'temp']]>=line_seg_fh[[q,'starty']] & gcc[[p,'temp']]<=line_seg_fh[[q,'endy']]){
          gcc[[p,'slopefh']] <- line_seg_fh[[q,'slope']]
          gcc[[p,'interceptfh']] <- line_seg_fh[[q,'intercept']]
        }
      }
    }
    
    for(p in 1:nrow(gcc)){
      for (q in 1:nrow(line_seg_fc)) {
        if(gcc[[p,'temp']]>=line_seg_fc[[q,'starty']] & gcc[[p,'temp']]<=line_seg_fc[[q,'endy']]){
          gcc[[p,'slopefc']] <- line_seg_fc[[q,'slope']]
          gcc[[p,'interceptfc']] <- line_seg_fc[[q,'intercept']]
        }
      }
    }
    
    gcc <- gcc %>% 
      mutate(xc = (temp-interceptfc)/slopefc) %>% 
      mutate(xh = (temp-interceptfh)/slopefh)
    
    idx_fc = line_seg_fc$slope == Inf
    idx_fh =  line_seg_fh$slope == Inf
    
    inf_line_fc = line_seg_fc[idx_fc,]
    inf_line_fh = line_seg_fh[idx_fh,]
    
    if (nrow(inf_line_fc) > 0){
      for (i in 1:nrow(inf_line_fc)) {
        st_y = inf_line_fc$starty[i]
        end_y = inf_line_fc$endy[i]
        gcc_idx = gcc$temp >= st_y & gcc$temp <= end_y
        gcc$xc[gcc_idx] = inf_line_fc$endx[i]
      }
    }
    
    if (nrow(inf_line_fh) > 0){
      for (i in 1:nrow(inf_line_fh)) {
        st_y = inf_line_fh$starty[i]
        end_y = inf_line_fh$endy[i]
        gcc_idx = gcc$temp >= st_y & gcc$temp <= end_y
        gcc$xh[gcc_idx] = inf_line_fh$endx[i]
      }
    }
    
    
    gcc <- gcc %>% 
      mutate(xc = ifelse(temp<min(forPlot.c.shift$tout), min(forPlot.c.shift$q_cum), xc)) %>% 
      mutate(xc = ifelse(temp>max(forPlot.c.shift$tout), max(forPlot.c.shift$q_cum), xc)) %>% 
      mutate(xh = ifelse(temp<min(forPlot.h.shift$tin), min(forPlot.h.shift$q_cum), xh)) %>% 
      mutate(xh = ifelse(temp>max(forPlot.h.shift$tin), max(forPlot.h.shift$q_cum), xh)) %>% 
      mutate(x = abs(xh-xc))
    gcc_data$gcc_table <- gcc
    
    y_max_limit = max(gcc$x)
    x_max_limit = max(gcc$temp)
    
    p <- ggplot(gcc, aes(x = temp, y = x)) +
      geom_line() +  # main line, no hover text here
      geom_point(aes(
        text = paste0("Temperature: ", round(temp, 1), " ", t_uni, 
                      "<br>Q: ", round(x, 1), " ", q_uni)),
        alpha = 0  # invisible points just for hover text
      ) +
      theme_bw() +
      ylab(paste0('Q (', q_uni, ')')) +
      xlab(paste0('Temperature (', t_uni, ')')) +
      theme(
        panel.grid.minor = element_line(color = "gray", linewidth = 0.1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 24)
      ) +
      scale_y_continuous(limits = c(0, y_max_limit)) +
      scale_x_continuous(limits = c(0, x_max_limit)) +
      ggtitle("Grand Composite Curve")
    
    gcc_plot <- p + coord_flip() 
    
  })
  
  
  
  output$gcc_plot <- renderPlotly({
    ggplotly(gcc_plot(), tooltip = "text")
  })
  
  output$downloadgcc <- downloadHandler(
    filename = function() {
      paste("GCC_", input$title, ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = gcc_plot(), device = "png", height = 25, width = 37.5,
             units = "cm", dpi= 400)
    })
  
  
  # Page 3 - Heat Exchanger ----
  
  observeEvent(input$file, {
    updateSelectInput(inputId = 'stream1', choices = unique(hold()$stream_no))}
  )
  observeEvent(input$file, {
    updateSelectInput(inputId = 'stream2', choices = unique(hold()$stream_no))}
  )
  
  
  
  stream1 <- reactive({
    req(input$stream1)
    input$stream1})
  stream2 <- reactive({
    req(input$stream2)
    input$stream2})
  
  
  
  observeEvent(input$run,{ 
    hx <- reactive({
      streams <- hold()
      stream1 <- stream1()
      stream2 <- stream2()
      streams <- streams %>%
        filter(stream_no == stream1 | stream_no == stream2) %>% 
        mutate(mcp = abs(q/(tout - tin))) 
      streams
    })
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    output$errorhx <- renderText ("")
    all_hx <- reactiveValues()
    hx <- hx()
    if (grepl("/hr", q_uni)){
      mcp_uni_df = paste(q_uni,t_uni)
    } else {
      mcp_uni_df <- paste(q_uni,"/",t_uni)
    }
    hx1 <- hx %>% 
      mutate(tin = round(tin,0)) %>% 
      mutate(tout = round(tout,0)) %>% 
      mutate(q = round(q,2)) %>% 
      mutate(mcp = round(mcp,2))
    hx1$stream_no <- formatC(hx1$stream_no, digits = 0)  
    hx1$stream_no <- as.character(hx1$stream_no)
    hx1$stream_name <- as.character(hx1$stream_name)
    
    hx1$tin <- as.character(hx1$tin)
    hx1$tout <- as.character(hx1$tout)
    hx1$q <- as.character(hx1$q)
    hx1$stream_type <- as.character(hx1$stream_type)
    hx1$mcp <- as.character(hx1$mcp)
    
    hx1 <- hx1 %>% 
      add_row(stream_no = '(-)', stream_name = '(-)', tin = paste0("(",t_uni,")"),
              tout = paste0("(",t_uni,")"),q = paste0("(",q_uni,")"), stream_type = '(-)', mcp = paste0("(",mcp_uni_df,")"),.before = 1) 
    
    hx1 <- hx1 %>% 
      rename(
        'S.No.' = stream_no,
        'Stream Name' = stream_name,
        'Temp,in' = tin,
        'Temp,out' = tout,
        'Q' = q,
        'Stream Type' = stream_type,
        'Mass Capacitance' = mcp
      )
    
    
    output$hxstreams <- renderTable({
      
      hx1
    })
    hx <- hx %>% 
      arrange(mcp)
    mcp_min <- hx[[1, "mcp"]]
    hot_stream <- hx %>% 
      filter(stream_type == "Needs Cooling")
    cold_stream <- hx %>% 
      filter(stream_type == "Needs Heating")
    
    output$tcin <- renderText({
      ""
    })
    output$thin <- renderText({
      ""
    })
    output$thout <- renderText({
      ""
    })
    output$tcout <- renderText({
      ""
    })
    output$qexch <- renderText({
      ""
    })
    
    results_hx <- reactive({
      req(input$file)
      req(input$pinchdt)
      results_hx <- tibble(
        'Metric' = c(),
        'Value' = c(),
        'Units' = c()
      )
      results_hx
    })
    
    
    if (nrow(hot_stream) > 0 && nrow(cold_stream) > 0) {
      th_in <- hot_stream[[1,"tin"]]
      mcp_h <- hot_stream[[1, "mcp"]]
      q_h <- hot_stream[[1, "q"]]
      mcp_c <- cold_stream[[1, "mcp"]]
      tc_in <- cold_stream[[1, "tin"]]
      q_c <- cold_stream[[1, "q"]]
      eff <- input$eff
      all_hx$th_in <- th_in 
      all_hx$tc_in <- tc_in
      
      q_hx_eff <- mcp_min*eff*(th_in - tc_in)
      q_hx <- min(q_c,q_h,q_hx_eff)
      act_eff <- q_hx/(mcp_min*(th_in - tc_in))
      all_hx$q_hx <- q_hx 
      
      th_out <- th_in-(q_hx/mcp_h)
      all_hx$th_out <- th_out
      tc_out <- (q_hx/mcp_c)+tc_in
      all_hx$tc_out <- tc_out
      
      
      results_hx <- reactive({
        req(input$file)
        req(input$pinchdt)
        results_hx <- tibble(
          'Metric' = c('Th,in','Th,out','Tc,in',
                      'Tc,out','Q,hx'),
          'Value' = c(all_hx$th_in,all_hx$th_out,all_hx$tc_in,all_hx$tc_out,all_hx$q_hx),
          'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni)
        )
        results_hx$Value <- formatC(results_hx$Value, digits = 3, format = "g") 
        results_hx
      })
      
      if (q_hx < 0 ) {
        results_hx <- reactive({
          req(input$file)
          req(input$pinchdt)
          results_hx <- tibble(
            'Metric' = c(),
            'Value' = c(),
            'Units' = c()
          )
          results_hx
        })
        output$errorhx <- renderText ("error: Temperatures for the streams do not overlap")
      }
      output$tcin <- renderText({
        t <- round(tc_in,0)
        val <- paste0(t, " ",t_uni)
        val
      })
      output$thin <- renderText({
        t <- round(th_in,0)
        val <- paste0(t, " ",t_uni)
        val
      })
      output$thout <- renderText({
        t <- round(th_out,0)
        val <- paste0(t, " ",t_uni)
        val
      })
      output$tcout <- renderText({
        t <- round(tc_out,0)
        val <- paste0(t, " ",t_uni)
        val
      })
      output$qexch <- renderText({
        t <- round(q_hx,1)
        val <- paste0(t, " ",q_uni)
        val
      })
      
      
    } else {
      output$errorhx <- renderText ("error: Please verify that there is a Needs Heating and Needs Cooling Stream ")
    }
    output$resulthx <- renderTable(results_hx())
    output$downloadtabhx <- downloadHandler(
      filename = "Heat Exchanger Results.csv",
      content = function(file) {
        results_hx <- results_hx()
        write.csv(results_hx, file,fileEncoding="Windows-1252")
      })
    
    
  })
  
  
  
  # Page 4 - Heat Pump ----
  
  observeEvent(input$file, {
    updateSelectInput(inputId = 'stream111', choices = unique(hold()$stream_no))}
  )
  observeEvent(input$file, {
    updateSelectInput(inputId = 'stream222', choices = unique(hold()$stream_no))}
  )
  
  cop_table <- reactive({
    ref_name <- as.character(input$ref)
    read_excel("cop_table.xlsx",sheet = ref_name)
  })
  
  results_table <- reactiveValues()
  
  observeEvent(input$run2,{
    cop_table <- cop_table()
    output$hperror <- renderText ("")
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    output$tcin_hp <- renderText({
      "    "
    })
    output$thin_hp  <- renderText({
      "    "
    })
    
    output$thout_hp  <- renderText({
      "    "
    })
    output$tcout_hp  <- renderText({
      "    "
    })
    output$qsource <- renderText({
      "   "
    })
    output$qsink <- renderText({
      "   "
    })
    output$tsink <- renderText({
      "   "
    })
    output$tsource <- renderText({
      "   "
    })
    output$winhp <- renderText({
      "   "
    })
    # Stream Inputs ----
    if (input$t_or_s == "Streams"){
      
      
      stream11 <- as.numeric(input$stream111)
      stream22 <- as.numeric(input$stream222)
      output$stream_data <- renderText("Selected Stream Data")
      output$technical_results <- renderText("Technical Results Table")
      
      streams_hp <- hold()
      hxapp <- input$hxapp
      hp <- streams_hp  %>% 
        mutate(mcp = abs(q/(tout - tin)))
      hp1 <- streams_hp %>% 
        filter(stream_no == stream11 | stream_no == stream22) %>% 
        mutate(mcp = round(abs(q/(tout - tin)),2)) %>% 
        mutate(tin = round(tin,0)) %>% 
        mutate(tout = round(tout,0)) %>% 
        mutate(q = round(q,1))
      
      output$image_notes <- renderText("Notes: 1. Qevap: Q evaporator. 2. Qcond: Q Condenser")
      hp1$stream_no <- formatC(hp1$stream_no, digits = 0)
      hp1$stream_no <- as.character(hp1$stream_no)
      hp1$stream_name <- as.character(hp1$stream_name) 
      hp1$tin <- as.character(hp1$tin)
      hp1$tout <- as.character(hp1$tout)
      hp1$q <- as.character(hp1$q)
      hp1$stream_type <- as.character(hp1$stream_type)
      hp1$mcp <- as.character(hp1$mcp)
      
      if (grepl("/hr", q_uni)){
        mcp_uni_df = paste(q_uni,t_uni)
      } else {
        mcp_uni_df <- paste(q_uni,"/",t_uni)
      }
      hp1 <- hp1 %>% 
        add_row(stream_no = '(-)', stream_name = '(-)', tin = paste("(",t_uni,")"),
                tout = paste("(",t_uni,")"),q = paste("(",q_uni,")"), stream_type = '(-)', mcp = paste("(",mcp_uni_df,")"),.before = 1)  %>% 
        rename(
          'S.No.' = stream_no,
          'Stream Name' = stream_name,
          'Temp,in' = tin,
          'Temp,out' = tout,
          'Q' = q,
          'Stream Type' = stream_type,
          'Mass Capacitance' = mcp
        )
      output$hptable1 <- renderTable({
        
        hp1
      })
      tsource_in <- hp[[stream11, "tin"]]
      tsource_out <- hp[[stream11, "tout"]]
      if (input$match == 'Partially') {
        qsource_avail <- hp[[stream11, "q"]]*input$match_input/100
      } else {
        qsource_avail <- hp[[stream11, "q"]]
      }
      
      
      mcp_source <- hp[[stream11, "mcp"]]
      tsink_in <- hp[[stream22, "tin"]]
      tsink_out <- hp[[stream22, "tout"]]
      qsink_req <- hp[[stream22, "q"]]
      mcp_sink <- hp[[stream22, "mcp"]]
      
      
      output$tcin_hp <- renderText({
        ""
      })
      output$thin_hp  <- renderText({
        ""
      })
      output$thout_hp  <- renderText({
        ""
      })
      output$tcout_hp  <- renderText({
        ""
      })
      output$qsource <- renderText({
        ""
      })
      output$qsink <- renderText({
        ""
      })
      output$tsink <- renderText({
        ""
      })
      output$tsource <- renderText({
        ""
      })
      output$winhp <- renderText({
        ""
      })
      results_hp <- tibble(
        'Metric' = c(),
        'Value' = c(),
        'Units' = c()
      )
      
      if (hp[[stream22, "stream_type"]] == "Needs Heating" & 
          hp[[stream11, "stream_type"]] == "Needs Cooling") {
        
        if (input$copmet == 'Enter Heat Pump COP') {
          cop <- input$cop2
          qsink_avail <- qsource_avail*(cop/(cop-1))
          qsink_act <- min(qsink_avail, qsink_req)
          qsource_act <- qsink_act*((cop-1)/cop)
          tsource_out_act <- tsource_in - qsource_act/mcp_source
          tsink_out_act <- (qsink_act/mcp_sink)+tsink_in
          w_in <- qsink_act/cop
          
          
        } else {
          cop_g = 4
          dc <- 1e10
          cop_table <- cop_table %>% 
            mutate(t_cond.t_evap = t_cond*t_evap) %>% 
            mutate(t_cond_2 = t_cond^2) %>% 
            mutate(t_evap_2 = t_evap^2)
          
          model1 <- lm(df~ t_cond + t_evap + t_cond.t_evap + t_cond_2 + t_evap_2, data = cop_table)
          while (dc>0.1) {
            cop <- cop_g
            cop_h <- cop_g
            
            if (t_uni == '°F'){ 
              t_evap_g = (tsource_out-32-hxapp)*(5/9)
              t_cond_g = (tsink_out-32+hxapp)*(5/9)
            } else {
              t_evap_g = tsource_out-hxapp
              t_cond_g = tsink_out+hxapp
            }
            
            carnot_cop = (t_cond_g+273)/(t_cond_g-t_evap_g)
            
            lookup_vals <- tibble(
              "t_cond" = t_cond_g,
              "t_evap" = t_evap_g,
              "t_cond.t_evap" = t_cond_g*t_evap_g,
              "t_cond_2" = t_cond_g^2,
              "t_evap_2" = t_evap_g^2
            )
            
            lookup_vals$df <- as_tibble(predict(model1,lookup_vals))
            
            # lookup_table <- inner_join(lookup_vals,cop_table, by = c("t_cond","t_evap"))
            
            df <- as.numeric(lookup_vals$df)
            
            cop <- df*carnot_cop
            
            qsink_avail <- qsource_avail*(cop/(cop-1))
            qsink_act <- min(qsink_avail,qsink_req)
            qsource_act <- qsink_act*((cop-1)/cop)
            tsource_out <- tsource_in - qsource_act/mcp_source
            tsink_out <- (qsink_act/mcp_sink)+tsink_in
            w_in <- qsink_act/cop
            
            if (t_uni == '°F'){ 
              tsource_out_c = (tsource_out-32-hxapp)*(5/9)
              dc <- abs(t_evap_g - tsource_out_c)
            } else {
              dc <- abs(t_evap_g - tsource_out +hxapp)
            }
            if(is.na(dc)){
              tsource_out_act <- NA
              tsink_out_act <- NA
              break
            }
            cop_g <- cop
          }
          tsource_out_act <- tsource_out
          tsink_out_act <- tsink_out
          
          if (t_evap_g < min(cop_table$t_evap) | t_evap_g > max(cop_table$t_evap)|
              t_cond_g < min(cop_table$t_cond) | t_evap_g > max(cop_table$t_cond)) {
            output$hperror <- renderText("Warning: COP calculation is extrapolated and may not be accurate.")
          }
        }
        
        if (t_uni == '°F'){ 
          te <- tsource_out_act - hxapp
          tc <- tsink_out_act + hxapp
        } else {
          te <- tsource_out_act - hxapp
          tc <- tsink_out_act + hxapp
        }
        
        cop_c <- cop - 1
        cop_combined <- cop_c + cop
        
        win_uni <- "KW"
        
        if (q_uni == "MMBtu/hr") {
          win = w_in*293.07107
        } else if (q_uni == "kJ/hr") {
          win = w_in*0.0002777778
        } else if (q_uni == "kW") {
          win = w_in
        } else if (q_uni == "MW") {
          win = w_in*1000
        } else if (q_uni == "MJ/hr") {
          win = w_in*0.277777778
        } else {
          win = w_in
          win_uni = q_uni
        }
        
        if (is.na(tsource_out_act) | is.na(tsink_out_act)) {
          output$hperror <- renderText("Error: COP calculation not available for this temperature range. Consider manually enterting COP.")
        } else if (tsink_out_act < tsource_out_act & cop>1) {
          output$hperror <- renderText("Error: Source Temperature is higher than sink temperature. Consider using a Heat Exchanger between streams.")
        } else if (tsink_out_act < tsource_out_act & cop<1) {
          output$hperror <- renderText("Error: Calculated COP is less than 1. Lift may not be achievable.")
        } else if (tc > 212){
          output$hperror <- renderText("Warning: High Temperature at Condenser, a suitable Heat Pump may not be available.")
          
          output$tcin_hp <- renderText({
            t <- round(tsource_in,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$thin_hp  <- renderText({
            t <- round(tsink_in,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$thout_hp  <- renderText({
            t <- round(tsink_out_act,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tcout_hp  <- renderText({
            t <- round(tsource_out_act,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$qsource <- renderText({
            t <- round(qsource_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$qsink <- renderText({
            t <- round(qsink_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$tsink <- renderText({
            t <- round(tc,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tsource <- renderText({
            t <- round(te,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$winhp <- renderText({
            
            t <- round(win,1)
            val <- paste0(t, " ",win_uni)
            val
          })
          results_hp <- tibble(
            'Metric' = c('Tso,in','Tso,out','Tsi,in',
                        'Tsi,out','Q,source','Q,sink','W,in',"COP,h", "COP,c","COP,combined"),
            'Value' = c(tsource_in,tsource_out_act,tsink_in,tsink_out_act,qsource_act,qsink_act, win,cop,cop_c,cop_combined),
            'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni,q_uni,win_uni," "," "," ")
          )
        }
        else {
          
          output$tcin_hp <- renderText({
            t <- round(tsource_in,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$thin_hp  <- renderText({
            t <- round(tsink_in,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$thout_hp  <- renderText({
            t <- round(tsink_out_act,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tcout_hp  <- renderText({
            t <- round(tsource_out_act,0)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$qsource <- renderText({
            t <- round(qsource_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$qsink <- renderText({
            t <- round(qsink_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$tsink <- renderText({
            t <- round(tc,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tsource <- renderText({
            t <- round(te,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$winhp <- renderText({
            
            
            t <- round(win,1)
            val <- paste0(t, " ",win_uni)
            val
          })
          results_hp <- tibble(
            'Metric' = c('Tso,in','Tso,out','Tsi,in',
                        'Tsi,out','Q,source','Q,sink','W,in',"COP,h", "COP,c","COP,combined"),
            'Value' = c(tsource_in,tsource_out_act,tsink_in,tsink_out_act,qsource_act,qsink_act, win,cop,cop_c,cop_combined),
            'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni,q_uni,win_uni," "," "," ")
          )
        }
        
        
      } else {
        
        
        output$hperror <- renderText("error: Please verify that Source Stream is a Needs Cooling and 
                                   Sink Stream is a Needs Heating Stream")
      }
      output$resulthp <- renderTable(results_hp)
      results_table$results_hp_rv = results_hp
    } else {
      # Temperature Input ----
      results_hp <- tibble(
        'Metric' = c('T,source','T,sink','Q,source','Q,sink','W,in',"COP,h", "COP,c","COP,combined"),
        'Value' = c("","","","", "","","",""),
        'Units' = c("","","","",""," "," "," ")
      )
      gcc_table <- gcc_data$gcc_table
      
      source_temp <- input$source_temp
      sink_temp <-input$sink_temp
      pinch_temp <- coor$cst + input$pinchdt/2
      source_table <- gcc_table[gcc_table[["temp"]] <= pinch_temp, ]
      sink_table <- gcc_table[gcc_table[["temp"]] >= pinch_temp, ]
      qsource_avail <- source_table[source_table$temp == source_temp, "x"]
      qsink_req <- sink_table[sink_table$temp == sink_temp, "x"]
      if (sink_temp > pinch_temp && sink_temp < max(gcc_table$temp) && source_temp < pinch_temp && source_temp > min(gcc_table$temp) ){
        source_table_poss <- source_table[source_table[["temp"]] >= source_temp, ]
        sink_table_poss <- sink_table[sink_table[["temp"]] <= sink_temp, ]
          
        if (input$copmet == 'Enter Heat Pump COP') {
          cop <- input$cop2
          qsink_avail <- qsource_avail*(cop/(cop-1))
          qsink_act <- min(qsink_avail, qsink_req)
          qsource_act <- qsink_act*((cop-1)/cop)
          tsource_out_idx <- which.min(abs(source_table_poss$x - qsource_act))
          tsource_out_act <- source_table_poss[tsource_out_idx, "temp"]
          tsink_out_idx <- which.min(abs(sink_table_poss$x - qsink_act))
          tsink_out_act <- sink_table_poss[tsink_out_idx, "temp"]
          w_in <- qsink_act/cop
          
          
          
        } else {
          cop_table <- cop_table %>% 
            mutate(t_cond.t_evap = t_cond*t_evap) %>% 
            mutate(t_cond_2 = t_cond^2) %>% 
            mutate(t_evap_2 = t_evap^2)
          
          model1 <- lm(df~ t_cond + t_evap + t_cond.t_evap + t_cond_2 + t_evap_2, data = cop_table)
          
          if (t_uni == '°F'){ 
            t_evap_g = (source_temp-32)*(5/9)
            t_cond_g = (sink_temp-32)*(5/9)
          } else {
            t_evap_g = source_temp
            t_cond_g = sink_temp
          }
          
          carnot_cop = (t_cond_g+273)/(t_cond_g-t_evap_g)
          
          lookup_vals <- tibble(
            "t_cond" = t_cond_g,
            "t_evap" = t_evap_g,
            "t_cond.t_evap" = t_cond_g*t_evap_g,
            "t_cond_2" = t_cond_g^2,
            "t_evap_2" = t_evap_g^2
          )
          
          lookup_vals$df <- as_tibble(predict(model1,lookup_vals))
          
          
          df <- as.numeric(lookup_vals$df)
          
          cop <- df*carnot_cop
          
          qsink_avail <- qsource_avail*(cop/(cop-1))
          qsink_act <- min(qsink_avail, qsink_req)
          qsource_act <- qsink_act*((cop-1)/cop)
          tsource_out_idx <- which.min(abs(source_table_poss$x - qsource_act))
          tsource_out_act <- source_table_poss[tsource_out_idx, "temp"] + input$pinchdt/2
          tsink_out_idx <- which.min(abs(sink_table_poss$x - qsink_act))
          tsink_out_act <- sink_table_poss[tsink_out_idx, "temp"] - input$pinchdt/2
          w_in <- qsink_act/cop

          if (t_evap_g < min(cop_table$t_evap) | t_evap_g > max(cop_table$t_evap)|
              t_cond_g < min(cop_table$t_cond) | t_evap_g > max(cop_table$t_cond)) {
            output$hperror <- renderText("Warning: COP calculation is extrapolated and may not be accurate.")
          }
        }
 
          te <- tsource_out_act - input$pinchdt/2
          tc <- tsink_out_act + input$pinchdt/2

        
        cop_c <- cop - 1
        cop_combined <- cop_c + cop
        
        win_uni <- "KW"
        
        if (q_uni == "MMBtu/hr") {
          win = w_in*293.07107
        } else if (q_uni == "kJ/hr") {
          win = w_in*0.0002777778
        } else if (q_uni == "kW") {
          win = w_in
        } else if (q_uni == "MW") {
          win = w_in*1000
        } else if (q_uni == "MJ/hr") {
          win = w_in*0.277777778
        } else {
          win = w_in
          win_uni = q_uni
        }
        
        if (is.na(tsource_out_act) | is.na(tsink_out_act)) {
          output$hperror <- renderText("Error: COP calculation not available for this temperature range. Consider manually enterting COP.")
        } else if (tsink_out_act < tsource_out_act & cop>1) {
          output$hperror <- renderText("Error: Source Temperature is higher than sink temperature. Consider using a Heat Exchanger between streams.")
        } else if (tsink_out_act < tsource_out_act & cop<1) {
          output$hperror <- renderText("Error: Calculated COP is less than 1. Lift may not be achievable.")
        } else if (tc > 212){
          output$hperror <- renderText("Warning: High Temperature at Condenser, a suitable Heat Pump may not be available.")
          
          output$tcin_hp <- renderText({
            ""
          })
          output$thin_hp  <- renderText({
            ""
          })
          
          output$thout_hp  <- renderText({
            ""
          })
          output$tcout_hp  <- renderText({
            ""
          })
          output$qsource <- renderText({
            t <- round(qsource_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$qsink <- renderText({
            t <- round(qsink_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$tsink <- renderText({
            t <- round(tc,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tsource <- renderText({
            t <- round(te,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$winhp <- renderText({
            
            t <- round(win,1)
            val <- paste0(t, " ",win_uni)
            val
          })
          results_hp <- tibble(
            'Metric' = c('T,source','T,sink','Q,source','Q,sink','W,in',"COP,h", "COP,c","COP,combined"),
            'Value' = c(te,tc,qsource_act,qsink_act, win,cop,cop_c,cop_combined),
            'Units' = c(t_uni,t_uni,q_uni,q_uni,win_uni," "," "," ")
          )
        }
        else {
          output$tcin_hp <- renderText({
            "-"
          })
          output$thin_hp  <- renderText({
            "-"
          })

          output$thout_hp  <- renderText({
            "-"
          })
          output$tcout_hp  <- renderText({
            "-"
          })
          output$qsource <- renderText({
            t <- round(qsource_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$qsink <- renderText({
            t <- round(qsink_act,1)
            val <- paste0(t, " ",q_uni)
            val
          })
          output$tsink <- renderText({
            t <- round(tc,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$tsource <- renderText({
            t <- round(te,1)
            val <- paste0(t, " ",t_uni)
            val
          })
          output$winhp <- renderText({
            
            
            t <- round(win,1)
            val <- paste0(t, " ",win_uni)
            val
          })
          results_hp <- tibble(
            'Metric' = c('T,source','T,sink','Q,source','Q,sink','W,in',"COP,h", "COP,c","COP,combined"),
            'Value' = c(te,tc,qsource_act,qsink_act, win,cop,cop_c,cop_combined),
            'Units' = c(t_uni,t_uni,q_uni,q_uni,win_uni," "," "," ")
          )
        }
      }
      

      
      else {
        output$hperror <- renderText("error: Integration is not across the pinch point or the selected temperature levels are not available.")
      }
      output$resulthp <- renderTable(results_hp)
      results_table$results_hp_rv = results_hp
      }
  })
  
  observeEvent(input$run3, {
    results_hp = results_table$results_hp_rv 
    hp_cost = input$hp_cost
    ng_price = input$ng_cost
    elec_price = input$elec_cost
    oper_hours = input$oper_hours
    output$economic_results <- renderText("Economic Results Table")
    
    q_cond_value <- results_hp$Value[results_hp$Metric == "Q,sink"][[1]]
    w_in_value <- results_hp$Value[results_hp$Metric == "W,in"][[1]]
    
    fuel_cost = q_cond_value*ng_price*oper_hours/(input$eff_heat/100)
    elec_cost = w_in_value*elec_price*oper_hours
    energy_savings = fuel_cost - elec_cost
    
    hp_cost = q_cond_value*293.07107*1.2*hp_cost
    
    simple_payback = max(0,hp_cost/energy_savings)
    
    econ_results = tibble(
      'Metric' = c("Fuel Cost", "Electricity Cost", "Energy Cost Savings"),
      'Value' = c(fuel_cost, elec_cost, energy_savings),
      'Units' = c("($)","($)","($)"))
    
    output$econ_table <- renderTable({
      econ_results
    }, digits = 0, # Number of decimal places
    format.args = list(big.mark = ","))
    
    results_table$econ_table <- econ_results
  })
  
  observeEvent(input$run4, {
    results_hp = results_table$results_hp_rv 
    elec_ef = input$elec_ef
    oper_hours = input$oper_hours
    
    q_cond_value <- results_hp$Value[results_hp$Metric == "Q,sink"][[1]]
    q_cond_units <- results_hp$Units[results_hp$Metric == "Q,sink"][[1]]
    w_in_value <- results_hp$Value[results_hp$Metric == "W,in"][[1]]
    
    output$emissions_results <- renderText("Emissions Results Table")
    
    emissions_data <- data.frame(
      'Metric'= c("Natural Gas", "Propane", "Petroleum Coke", "Distillate or Light Fuel Oil", 
                 "Coal", "Diesel", "Motor Gasoline")
    )
    emissions_data <- emissions_data %>% 
      mutate(      'kJ/hr' = c(5.0E-08, 6.0E-08, 9.7E-08, 7.0E-08, 9.1E-08, 7.0E-08, 6.7E-08),
                   'kW' = c(1.8E-04, 2.1E-04, 3.5E-04, 2.5E-04, 3.3E-04, 2.5E-04, 2.4E-04),
                   'MMBtu/hr' = c(5.3E-02, 6.3E-02, 1.0E-01, 7.4E-02, 9.6E-02, 7.4E-02, 7.1E-02),
                   'MW' = kW*1000,
                   'MJ/hr' = `kJ/hr`*1000)
    
    fuel_ef = emissions_data[emissions_data$Metric == input$fuel_type, q_cond_units]*1000 #convert from MT to kg.
    
    
    fuel_emissions = q_cond_value*fuel_ef*oper_hours/(input$eff_heat/100)
    elec_emisions = w_in_value*elec_ef*oper_hours
    emissions_savings = fuel_emissions - elec_emisions
    
    emission_results = tibble(
      'Metric' = c("Fuel Emissions", "Electricity Emissions", "Emissions Savings"),
      'Value' = c(fuel_emissions, elec_emisions, emissions_savings),
      'Units' = c("(kg CO2)","(kg CO2)","(kg CO2)"))
    
    emission_results$Value <- round(emission_results$Value / 10) * 10      
    output$em_table <- renderTable({
      emission_results
    }, digits = 0, # Number of decimal places
    format.args = list(big.mark = ","))
    results_table$em_table <- emission_results
    
  })
  
  output$downloadtabhp <- downloadHandler(
    filename = function() {
      "Heat_Pump_Results.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook()
      results_hp <- results_table$results_hp_rv
      econ_table <- results_table$econ_table
      em_table <- results_table$em_table
      
      # Add each table to a separate worksheet
      addWorksheet(wb, "Technical Results")
      writeData(wb, "Technical Results", results_hp)
      
      addWorksheet(wb, "Economic Results")
      writeData(wb, "Economic Results", econ_table)
      
      addWorksheet(wb, "Emissions Results")
      writeData(wb, "Emissions Results", em_table)
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)

