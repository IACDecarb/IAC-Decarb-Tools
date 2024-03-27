## 
#Delete this chuck after running once
install.packages("tidyverse")
install.packages("shiny")
install.packages("janitor")
install.packages("readxl")
install.packages("shinycssloaders")
install.packages("shinythemes")
install.packages("shinyjs")
install.packages("shinyBS")
#


##Application starts here
library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(shinyBS)



ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel("Process Integration Tool"),
  tabsetPanel(
    tabPanel("Main Pinch",
             sidebarLayout(
               sidebarPanel(
                 tags$style(HTML("
      #downloadData1 {
        font-weight: bold;
        font-size: 18px;
      }
    ")),
                 downloadLink("downloadData1", "Download Process Integration Tool - Input Sheet"),
                 br(),
                 br(),
                 fileInput("file", "Upload Excel File"),
                 textInput('title', 'Add Pinch Title'),
                 numericInput("pinchdt", "Enter Pinch Temperature", 10),
                 selectInput('pt','Show Pinch Hot/Cold Side Temperatures', c('No','Yes')),
                 selectInput('hx','Show Heat Exchanger Overlap Region', c('No','Yes'), "Yes"),
                 selectInput('hp','Show Heat Pump Source and Sink Regions', c('No','Yes')),
                 numericInput("cop", HTML("Specify Heat Pump COP<sub>h</sub>"), 3),
                 selectInput('bg','Show High Temperature Heating Region', c('No','Yes')),
                 selectInput('hl','Show Stream Labels', c('No','Yes'),"Yes"),
                 downloadButton("downloadPNG", "Click Here to Download plot as Image"),
                 br(),
                 br(),
                 downloadButton("downloadtab", "Click Here to Download Pinch Summary Table")
               ),
               mainPanel(
                 span(textOutput("error1"), style="color:red"),
                 plotOutput("pinch_plot", height = '600px') %>% withSpinner(color="#0dc5c1"),
                 br(),
                 textOutput('ercode'),
                 br(),
                 br(),
                 uiOutput("tit"),
                 tableOutput("myTable"),
                 
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
                     display: inline-block;
                    position: relative;
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
                 tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
                 tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
               )
             )
    ),
    tabPanel("Heat Pump", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("stream111", "Choose Source Stream", character(0)),
                 selectInput("stream222", "Choose Sink Stream", character(0)),
                 radioButtons('copmet','Select COP Calculation Methodolgy', choices = c("Enter COP Value", "Use System Defined COP")),
                 
                 bsCollapse(id = "collapsePanel", open = "",
                            bsCollapsePanel(title = "Enter Heat Pump COP",
                                            numericInput("cop2", "", min = 1, value = 4)),
                            bsCollapsePanel(title = "Use System Defined COP",
                                            selectInput("ref", "Select Refrigerant", choices = c('Ammonia (R717)','Water (R-718)','Propane (R-290)',
                                                                                                 'Propylene (R-1270)','n-Butane (R600)','isoButane (r600a)')),
                                            tags$small("(only available for select refrigerants and temperature ranges)")
                            )
                 ),
                 
                 actionButton("run2", "Run Analysis"),
                 br(),
                 br(),
                 downloadButton("downloadtabhp", "Click Here to Heat Pump Results Table")
                 
               ),
               mainPanel(
                 fluidRow(
                   div(
                     style = "
                     margin-left: 2%;
                     display: inline-block;
                    position: relative;
                ",
                     img(
                       src = "hp.png",
                       style =  "width: 1000px; height: 500px;"
                     ),
                     span(textOutput("thin_hp"),
                          style = "
                      color: black;
                      position: relative;
                      top: -110px;
                      left: 820px;
                      font-size: 20px;
                      width:40px;
                    "
                     ),
                     span(textOutput("thout_hp"),
                          style = "
                      color: black;
                      position: relative;
                      top: -460px;
                      left: 820px;
                      font-size: 20px;"
                     ),
                     span(textOutput("tcout_hp"),
                          style = "
                color: black;
                 position: relative;
                     top: -170px;
                     left: 100px;
                          font-size: 20px;"
                     ),
                     span(textOutput("tcin_hp"),
                          style = "
                color: black;
                 position: relative;
                     top: -518px;
                     left: 100px;
                          font-size: 20px;"
                     ),
                     span(textOutput("qsource"),
                          style = "
                color: black;
                 position: relative;
                     top: -400px;
                     left: 75px;
                          font-size: 15px;
                          width: 40px;"
                     ),
                     span(textOutput("qsink"),
                          style = "
                color: black;
                 position: relative;
                     top: -420px;
                     left: 835px;
                          font-size: 15px;"
                     ),
                     span(textOutput("tsource"),
                          style = "
                color: black;
                 position: relative;
                     top: -382px;
                     left: 355px;
                          font-size: 15px;"
                     ),
                     span(textOutput("tsink"),
                          style = "
                color: black;
                 position: relative;
                     top: -405px;
                     left: 595px;
                          font-size: 15px;"
                     ),
                     span(textOutput("winhp"),
                          style = "
                color: black;
                 position: relative;
                     top: -625px;
                     left: 442px;
                          font-size: 15px;"
                     )
                   ),
                 ),
                 h4("Streams Data", align = "left"),
                 tableOutput("hptable1"),
                 h4("Results Table", align = "left"),
                 tableOutput("resulthp"),
                 span(textOutput("hperror"), style="color:red")
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
                 tags$p(tags$b("Kelly Kissock"), style = "margin-top: 0.5px;"),
                 tags$p("jkissock@ucdavis.edu", style = "margin-top: 0.5px;")
               )
             ))
  )
)

server <- function(input, output,session) {
  
  
  
  excelFilePath <- 'Pinch.xlsx'
  copFilePath <- 'cop_table.xlsx'
  
  observeEvent(input$copmet, ({
    if (input$copmet == "Use System Defined COP") {
      updateCollapse(session, "collapsePanel",open = "Use System Defined COP")
    } else {
      updateCollapse(session, "collapsePanel", open = "Enter Heat Pump COP")
    }
  }))
  
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
  
  hold.h <- reactive({
    req(input$file)
    streams_data.h <- read_excel(input$file$datapath, sheet = "Streams")
    clean_names(streams_data.h)
  })
  
  hold <- reactive({
    req(input$file)
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
    hold.h <- hold.h()
    hold.h %>% 
      slice(1) %>% 
      select(tin,tout,q)
  })
  
  needs_cooling <- reactive({
    req(input$file)
    hold <- hold()
    hold %>% 
      filter(stream_type == "Needs Cooling")
  })
  
  needs_heating <- reactive({
    req(input$file)
    hold <- hold()
    hold %>% 
      filter(stream_type == "Needs Heating")
  })
  
  forPlot.c <- reactive({
    req(input$file)
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
            y.h <- intercept + slope * x
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
            y.h <- intercept + slope * x
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
    heatshift <- hs$heatshift
    forPlot.h <- forPlot.h1()
    forPlot.h %>% 
      mutate(q_cum = q_cum - heatshift)
  })
  
  all_q <- reactiveValues()
  
  observe({
    req(input$file)
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
    
    # Convert the elements to a numeric vector
    numeric_vector <- as.numeric(elements)
    
    numeric_vector <- na.omit(numeric_vector)
    
    lb.hp.so <- unique(numeric_vector)
    lb.hp.so <- paste0(lb.hp.so, collapse = ';')
    
    lb.hp.so
  })
  
  lb.hx.c <- reactive({
    req(input$file)
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
    
    # Convert the elements to a numeric vector
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hx.c <- unique(numeric_vector)
    lb.hx.c <- paste0(lb.hx.c, collapse = ';')
    
    lb.hx.c
  })
  
  lb.hx.nh <- reactive({
    req(input$file)
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
    
    # Convert the elements to a numeric vector
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hx.nh <- unique(numeric_vector)
    lb.hx.nh <- paste0(lb.hx.nh, collapse = ';')
    
    lb.hx.nh
  })
  
  lb.hp.si <- reactive({
    req(input$file)
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
    
    # Convert the elements to a numeric vector
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.hp.si <- unique(numeric_vector)
    lb.hp.si <- paste0(lb.hp.si, collapse = ';')
    
    lb.hp.si
  })
  
  lb.ht <- reactive({
    req(input$file)
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
    
    # Convert the elements to a numeric vector
    numeric_vector <- as.numeric(elements)
    numeric_vector <- na.omit(numeric_vector)
    lb.ht <- unique(numeric_vector)
    lb.ht <- paste0(lb.ht, collapse = ';')
    
    lb.ht
  })
  
  wavg.source <- reactive({
    req(input$file)
    forPlot.c <- forPlot.c()
    forPlot.h <- forPlot.h()
    wavg.c <- tibble(
      'dt' = 0,
      'q.h' = 0
    )
    for (j in 1:(nrow(forPlot.c)-1)){
      if(forPlot.c[[j+1,'q_cum']]<min(forPlot.h$q_cum)){
        wavg.c[j,'dt'] <- forPlot.c[[j+1,'tout']]-forPlot.c[[j,'tout']]
        wavg.c[j,'q.h'] <- forPlot.c[[j+1,'q_cum']]-forPlot.c[[j,'q_cum']]
      }
      else if(forPlot.c[[j+1,'q_cum']]>min(forPlot.h$q_cum) & 
              (forPlot.c[[j,'q_cum']]<min(forPlot.h$q_cum))){
        slp <- (forPlot.c[[j+1,'tout']]-forPlot.c[[j,'tout']])/(forPlot.c[[j+1,'q_cum']]-forPlot.c[[j,'q_cum']])
        yicp <- forPlot.c[[j+1,'tout']]-slp*forPlot.c[[j+1,'q_cum']]
        tempot <- slp*min(forPlot.h$q_cum)+yicp
        wavg.c[j,'dt'] <- tempot - forPlot.c[[j,'tout']]
        wavg.c[j,'q.h'] <- min(forPlot.h$q_cum) - forPlot.c[[j,'q_cum']]
      }
      else {
        wavg.c[j,'dt'] <- 0
        wavg.c[j,'q.h'] <- 0
      }
    }
    wavg.c <- wavg.c %>% 
      mutate(pr = dt*q.h)
    wavg.source <- (sum(wavg.c$pr)/sum(wavg.c$q.h))+min(forPlot.c$tout)
    wavg.source <- round(wavg.source,1)
    wavg.source
  })
  
  wavg.sink <- reactive({
    req(input$file)
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
      else if (forPlot.h[[j,'q_cum']]>max(forPlot.c$q_cum) & forPlot.h[[j+1,'q_cum']]<(max(forPlot.c$q_cum)+all_q$qhp.si)){
        wavg.h[j,'dt'] <- forPlot.h[[j+1,'tin']]-forPlot.h[[j,'tin']]
        wavg.h[j,'q.h'] <- forPlot.h[[j+1,'q_cum']]-forPlot.h[[j,'q_cum']]
      }
      else if (forPlot.h[[j+1,'q_cum']]>(max(forPlot.c$q_cum)+all_q$qhp.si) & (forPlot.h[[j,'q_cum']]<(max(forPlot.c$q_cum)+all_q$qhp.si))){
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
    wavg.h <- wavg.h %>% 
      mutate(pr = dt*q.h)
    
    wavg.sink <- (sum(wavg.h$pr)/sum(wavg.h$q.h))+t_init
    wavg.sink <- round(wavg.sink,1)
    wavg.sink
  })
  
  
  data <- reactive({
    req(input$file)
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    data <- tibble(
      'Title' = c('Heat Exchange Potential','Heat Pump - Source Potential','Heat Pump - Source Temperature',
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
    cst <- round(coor$cst,0)
    hst <- round(coor$hst,0)
    qhp.so <- all_q$qhp.so
    qhp.si <- all_q$qhp.si
    qbg <- all_q$qbg
    lb <- lb()
    lb.c <- lb.c()
    wavg.source <- wavg.source()
    wavg.sink <- wavg.sink()
    y_max_limit <- max(max(forPlot.h$tin),max(forPlot.c$tout))+30
    tit <- paste0('Pinch Analysis for ',input$title)
    pinch <- ggplot() +
      geom_line(data = forPlot.c, aes(x = q_cum, y = tout, color = "Needs Cooling"),arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
      geom_line(data = forPlot.h, aes(x = q_cum, y = tin, color = "Needs Heating"), arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
      geom_segment(aes(x = x_coor, xend = x_coor, y = y_coor, yend = y_coor + p_lab)) +
      geom_text(aes(x = x_coor, y = y_coor + (p_lab/2), label = paste0('pinch \n', pinchdt,t_uni), hjust = -0.05), size = 3) +
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
      scale_y_continuous(limits = c(0, y_max_limit)) +
      scale_color_manual(values = c("Needs Cooling" = "blue", "Needs Heating" = "red"))+
      ggtitle(tit)# Set line colors and legend labels
    
    
    if (input$pt == 'Yes') {
      
      pinch <- pinch +  
        geom_hline(yintercept = y_coor, linetype = "dashed", color = "steelblue")+
        geom_hline(yintercept = y_coor+p_lab, linetype = "dashed", color = "steelblue")+
        geom_text(
          aes(
            x = 5,
            y = hst,
            label = paste0('Pinch - Hot Side T = ',hst)
          ),
          vjust = -0.1,
          hjust = 0
        )+
        geom_text(
          aes(
            x = 5,
            y = cst,
            label = paste0('Pinch - Cold Side T = ',cst)
          ),
          vjust = 1.2,
          hjust = 0
        )
    }
    
    if (input$hx == 'Yes') {
      t1 <- paste0("Qhx\n",qhx, ' MMBtu/hr')
      
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
      
      t2 <- paste0("Qhp Source\n",qhp.so, ' MMBtu/hr')
      
      t4 <- paste0("Qhp Sink\n",qhp.si, ' MMBtu/hr')
      
      
      t3 <- paste0("T Source (avg.)\n",wavg.source, t_uni)
      t5 <- paste0("T Sink (avg.)\n",wavg.sink, t_uni)
      
      pinch <- pinch +  
        geom_vline(xintercept = min(forPlot.h$q_cum), linetype = "dashed", color = "palegreen")+
        geom_vline(xintercept = min(forPlot.c$q_cum), linetype = "dashed", color = "palegreen")+
        geom_segment(aes(x=min(forPlot.h$q_cum),
                         xend=min(forPlot.c$q_cum),
                         y=y_max_limit-20,
                         yend=y_max_limit-20),
                     color = 'palegreen',
                     arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "both", angle = 15))+
        geom_vline(xintercept = max(forPlot.c$q_cum), linetype = "dashed", color = "palegreen")+
        geom_vline(xintercept = max(forPlot.c$q_cum)+qhp.si, linetype = "dashed", color = "palegreen")+
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
        t6 <- paste0("Qht\n",qbg, ' MMBtu/hr')
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
        annotate('text',x = lb$pos.x, y = lb$pos.y, label = lb$lab)+
        annotate('text',x = lb.c$pos.xc, y = lb.c$pos.yc, label = lb.c$labc)
    }
    
    
    
    print(pinch)
  })
  
  output$pinch_plot <- renderPlot({
    req(hs$c1l < hs$c1h | hs$c2l < hs$c2h)
    pinch1()})
  
 
  
  output$tit <- renderUI({
    req(input$file)
    # Create a div with inline CSS for bold, larger text, and left indent
    if (hs$c1l < hs$c1h | hs$c2l < hs$c2h)  {
    HTML("<div style='font-weight: bold; font-size: 24px; margin-left: 12px;'>Process Integration Tool - Summary Table</div>")
    }
      })
  
  output$downloadPNG <- downloadHandler(
    filename = "Pinch.png",
    content = function(file) {
      ggsave(file, plot = pinch1(), device = "png", height = 25, width = 37.5,
             units = "cm", dpi= 400)
    })
  output$downloadtab <- downloadHandler(
    filename = "Pinch Summary.csv",
    content = function(file) {
      write.csv(data(), file,fileEncoding="Windows-1252")
    })
  
  #Page 2
  
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
    
    output$errorhx <- renderText ("")
    all_hx <- reactiveValues()
    hx <- hx()
    hx1 <- hx %>% 
      mutate(stream_no = round(stream_no,0)) %>% 
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
      hx1$S.No. <- formatC(hx1$S.No., digits = 0)
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
      
      results_hx <- tibble(
        'Title' = c(),
        'Value' = c(),
        'Units' = c()
      )
      results_hx
    })
    
    
    if (nrow(hot_stream) > 0 && nrow(cold_stream) > 0) {
      uni <- uni()
      t_uni <- uni[[1, "tin"]]
      q_uni <- uni[[1, "q"]]
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
        
        results_hx <- tibble(
          'Title' = c('Th,in','Th,out','Tc,in',
                      'Tc,out','Q,hx'),
          'Value' = c(all_hx$th_in,all_hx$th_out,all_hx$tc_in,all_hx$tc_out,all_hx$q_hx),
          'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni)
        )
        results_hx
      })
      
      if (q_hx < 0 ) {
        results_hx <- reactive({
          req(input$file)
          
          results_hx <- tibble(
            'Title' = c(),
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
  
  
  
  #Page 3
  
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
  
  observeEvent(input$run2,{
    stream11 <- as.numeric(input$stream111)
    stream22 <- as.numeric(input$stream222)
    output$hperror <- renderText ("")
    uni <- uni()
    t_uni <- uni[[1, "tin"]]
    q_uni <- uni[[1, "q"]]
    streams_hp <- hold()
    hp <- streams_hp  %>% 
      mutate(mcp = abs(q/(tout - tin)))
    hp1 <- streams_hp %>% 
      filter(stream_no == stream11 | stream_no == stream22) %>% 
      mutate(mcp = abs(q/(tout - tin)))
    hp1 <- hp1 %>% 
      mutate(stream_no = round(stream_no,0)) %>% 
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
      hp1$S.No. <- formatC(hp1$S.No., digits = 0)
      hp1
    })
    tsource_in <- hp[[stream11, "tin"]]
    tsource_out <- hp[[stream11, "tout"]]
    qsource_avail <- hp[[stream11, "q"]]
    mcp_source <- hp[[stream11, "mcp"]]
    tsink_in <- hp[[stream22, "tin"]]
    tsink_out <- hp[[stream22, "tout"]]
    qsink_req <- hp[[stream22, "q"]]
    mcp_sink <- hp[[stream22, "mcp"]]
    cop_table <- cop_table()
    
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
      'Title' = c(),
      'Value' = c(),
      'Units' = c()
    )
    
    if (hp[[stream22, "stream_type"]] == "Needs Heating" & 
        hp[[stream11, "stream_type"]] == "Needs Cooling") {
      
      if (input$copmet == 'Enter COP Value') {
        cop <- input$cop2
        qsink_avail <- qsource_avail*(cop/(cop-1))
        qsink_act <- ifelse(qsink_avail<qsink_req, qsink_avail, qsink_req)
        qsource_act <- qsink_act*((cop-1)/cop)
        tsource_out_act <- tsource_in - qsource_act/mcp_source
        tsink_out_act <- (qsink_act/mcp_sink)+tsink_in
        w_in <- qsink_act/cop
        
        
      } else {
        cop_g = 4
        dc <- 1e10
        while (dc>0.1) {
          cop <- cop_g
          cop_h <- cop_g
          
          if (t_uni == '°F'){ 
            t_evap_g = floor((tsource_out-32-5)*(5/9))
            t_cond_g = ceiling(((tsink_out-32+5)*(5/9))/10)*10
          } else {
            t_evap_g = floor(tsource_out-3)
            t_cond_g = ceiling(((tsink_out+3)/10)*10)
          }
          
          carnot_cop = (t_cond_g+273)/(t_cond_g-t_evap_g)
          
          lookup_vals <- tibble(
            "t_cond" = t_cond_g,
            "t_evap" = t_evap_g
          )
          
          lookup_table <- inner_join(lookup_vals,cop_table, by = c("t_cond","t_evap"))
          
          a <- as.numeric(lookup_table[1, "a"])
          b <- as.numeric(lookup_table[1, "b"])
          
          n = a + b/carnot_cop
          
          cop <- n*carnot_cop
          
          qsink_avail <- qsource_avail*(cop/(cop-1))
          qsink_act <- ifelse(qsink_avail<qsink_req, qsink_avail, qsink_req)
          qsource_act <- qsink_act*((cop-1)/cop)
          tsource_out <- tsource_in - qsource_act/mcp_source
          tsink_out <- (qsink_act/mcp_sink)+tsink_in
          w_in <- qsink_act/cop
          
          dc <- abs(cop_h - cop)
          if(is.na(dc)){
            tsource_out_act <- NA
            tsink_out_act <- NA
            break
          }
          cop_g <- cop
        }
        tsource_out_act <- tsource_out
        tsink_out_act <- tsink_out
      }
      
      te <- tsource_out_act - 5
      tc <- tsink_out_act + 5
      
      if (is.na(tsource_out_act) | is.na(tsink_out_act)) {
        output$hperror <- renderText("error: COP calculation not available for this temperature range. Consider manually enterting COP.")
      } else if (tsink_out_act < tsource_out_act) {
        output$hperror <- renderText("error: Source Temperature is higher than sink temperature. Consider using a Heat Exchanger between streams.")
      } else if (tc > 212){
        output$hperror <- renderText("Warning: High Temperature at Condenser, a suitable Heat Pump may not be available.")
        results_hp <- tibble(
          'Title' = c('Tso,in','Tso,out','Tsi,in',
                      'Tsi,out','Q,source','Q,sink','W,in',"COP"),
          'Value' = c(tsource_in,tsource_out_act,tsink_in,tsink_out_act,qsource_act,qsink_act, w_in,cop),
          'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni,q_uni,q_uni," ")
        )
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
          t <- round(w_in,1)
          val <- paste0(t, " ",q_uni)
          val
        })
      }
      else {
        results_hp <- tibble(
          'Title' = c('Tso,in','Tso,out','Tsi,in',
                      'Tsi,out','Q,source','Q,sink','W,in',"COP"),
          'Value' = c(tsource_in,tsource_out_act,tsink_in,tsink_out_act,qsource_act,qsink_act, w_in,cop),
          'Units' = c(t_uni,t_uni,t_uni,t_uni,q_uni,q_uni,q_uni," ")
        )
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
          t <- round(w_in,1)
          val <- paste0(t, " ",q_uni)
          val
        })
      }
      
      
    } else {
      
      
      output$hperror <- renderText("error: Please verify that Source Stream is a Needs Cooling and 
                                   Sink Stream is a Needs Heating Stream")
    }
    output$resulthp <- renderTable(results_hp)
    
    output$downloadtabhp <- downloadHandler(
      filename = "Heat Pump Results.csv",
      content = function(file) {
        write.csv(results_hp, file,fileEncoding="Windows-1252")
      })
    
  })
  
}

shinyApp(ui = ui, server = server)

