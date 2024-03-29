setwd("C:/Users/jkissock/Box/UCD-LBL Decarbonization Group/IAC Decarbonization/github repo/apps/fcft")

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


webshot::install_phantomjs()

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(
      HTML(".output-text {
             font-size: 20px;
             color: black;
             text-align: center;
           }")
    )
  ),
  titlePanel(HTML("Facility CO<sub>2</sub>e Flow Tool"), 
             windowTitle = "FCF Tool"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("
      #downloadData1 {
        font-weight: bold;
        font-size: 16px;
      }
    ")),
      downloadLink("downloadData1", "Download Facility CO₂e Flow Tool - Input Sheet"),
      br(),
      br(),
      textInput("cname", "Enter Facility Name"),
      fileInput("file", "Upload \'FCFT Input Sheet\' Excel File", accept = ".xlsx"),
      textOutput('move'),
      selectInput("units", "Select Units", c("MT CO₂e/yr", "lbs. of CO₂e/yr")),
      numericInput("precision", "Choose precision level of numeric values", 0, -20, 20, 1),
      sliderInput("vsc", "Adjust vertical scaling of the Sankey Diagram", 1, 100, 50),
      numericInput("height", "Adjust height of downloaded image (px)", 500, 500, 20000, 250),
      numericInput("width", "Adjust width of downloaded image (px)", 1000, 750, 20000, 250),
      downloadButton("downloadPNG", "Click Here to Download plot as Image"),
      br(),
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
      div(
        uiOutput("output_text"),
        class = "output-text"
      ),
      tags$div(
        style = "position: relative; width: 100%; background-color: #f8f8f8;",
        uiOutput("diagram")
      ),
      br(),
      span(textOutput("titleef"), style="font-size: 21px; margin-left: 10px;"),
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
  
)


server <- function(input, output, session) {
  
  excelFilePath <- "Facility CO2e Flow Tool - Input Sheet.xlsx"
  docFilePath <- 'User Guide for Facility CO2e Flow Tool.pdf'
  
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
    aa <- read_excel(input$file$datapath,sheet = 'Emissions Calculator', range = "c17:k200")
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>% 
      filter(!is.na(emission_source))
    end.use <- tibble('Name' = aa$`emission_source`)
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
    nodes.h <- rbind(nodes.hh,ene.src,end.use,em.src)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>% 
      mutate('No' = row_number()) %>% 
      select(No,Name)
    nodes
  })
  
  units_conversion <- reactive({
    if (input$units == "MT CO₂e/yr") {
      1 # Conversion factor 
    } else {
      2204.6226218
    }
  })
  
  # Read the uploaded links Excel file
  ef <- reactive({
    req(input$file)
    bb <- read_excel(input$file$datapath,sheet = 'Emission Factors', range = "b3:h20")
    aa <- read_excel(input$file$datapath,sheet = 'Emissions Calculator', range = "c17:k200")
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>% 
      filter(!is.na(emission_source))
    ene.src <- tibble('Name' = unique(aa$`energy_source`))
    bb1 <- bb %>% 
      filter(!is.na(Title))
    fortable <- aa %>% 
      mutate(efta = paste0(energy_source,units))%>% 
      select(efta) %>% 
      filter(!is.na(efta))
    
    result <- merge(bb1, ene.src, by.x = "Title", by.y = "Name", all = FALSE)
    
    long_data <- melt(result, id.vars = c("Title", "Source"), variable.name = "Units", value.name = "Factors")
    
    display_data <- merge(bb1, ene.src, by.x = "Title", by.y = "Name", all = FALSE)
    
    dd1 <- long_data %>% 
      mutate(efta = paste0(Title,Units)) 
    
    dd2 <- tibble('efta' = unique(fortable$`efta`))
    
    result2 <- merge(dd1, dd2, by = "efta", all = FALSE)
    result2 <- result2 %>% 
      select(-efta) %>% 
      select(Title, Factors,Units,Source) %>% 
      mutate(Units = paste0("MTCO₂e/",Units))
    result2$Factors <- signif(result2$Factors,3)
    result2$Factors <- format(result2$Factors, scientific = T)
    
    result2
  })
  
  
  
  tef <- reactive({
    ef <- ef()
    if (is_empty(ef$Factors)){
      ""
    }else{
      "Emission Factors Used"
    }
  })
  
  output$titleef <- renderText({
    text <- tef()
  })
  
  output$table1<- renderTable({
    # Set to 0 to always display in scientific notation
     ef()
  })
  
  temp <- reactive({
    req(input$file)
    temp <- read_excel(input$file$datapath,sheet = 'Emissions Calculator', range = "c17:k200")
    temp <- temp[-1, ]
    temp <- clean_names(temp)
    temp <- temp %>% 
      filter(!is.na(emission_source))
    temp
  })
  links_data <- reactive({
    req(input$file)
    aa <- read_excel(input$file$datapath,sheet = 'Emissions Calculator', range = "c17:k200")
    aa <- aa[-1, ]
    aa <- clean_names(aa)
    aa <- aa %>% 
      filter(!is.na(emission_source))
    
    end.use <- tibble('Name' = aa$`emission_source`)
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
    
    nodes.h <- rbind(nodes.hh,ene.src,end.use,em.src)
    
    nodes <- nodes.h %>%
      filter(!is.na(Name)) %>% 
      mutate('No' = row_number()) %>% 
      select(No,Name)
    
    links.h <- tibble(
      'No' = 0,
      'Source' = 0,
      'Target' = 0,
      'Value' = 0
    )
    
    aa.e <- aa %>% 
      filter(!is.na(energy_source))
    
    for (i in 1:nrow(aa.e)) {
      links.h[i,'No'] <- i
      for (j in 1:nrow(nodes)) {
        if (aa.e[i,'emission_source'] == nodes[j,'Name']){
          links.h[i,'Target'] <- nodes[[j,'No']] - 1
        }
      }
      for (j in 1:nrow(nodes)) {
        if (aa.e[i,'energy_source'] == nodes[j,'Name']){
          links.h[i,'Source'] <- nodes[[j,'No']] - 1
        }
      }
      links.h[i,'Value'] <- aa.e[i,'co2e_emissions_mt_co2e_yr']
    }
    
    links.hh <- links.h %>% 
      group_by(Source) %>% 
      summarise(Value = sum(Value))
    ctr <- 0
    
    links.hh2 <- tibble( Source = c(),
                         Value = c()
    )
    ele_link <- tibble( Source = c(),
                       Value = c()
    )
    ele <- nodes %>% 
      filter(Name == 'Electricity')
    
    if(!is_empty(ele$No)) {
    ele_link_val <- as.numeric(ele$No - 1)
    ele_link <- links.hh %>% 
      filter(Source == ele_link_val)
    
    links.hh <- links.hh %>% 
      filter(Source != ele_link_val)
    ctr <- ctr+1
    }


    if(!is_empty(links.hh$Source)) {
      if (!is_empty(ele$Name)){
        ctr2 <- n_src - 1
      } else {
        ctr2 <- n_src
      }
    l <- 0
    for (k in (nrow(links.h)+1):(nrow(links.h)+ctr2)) {
      l <- l+1
      links.h[k,'No'] <- k
      links.h[k,'Target'] <- links.hh[l,'Source']
      links.h[k,'Source'] <- 1
      links.h[k,'Value'] <- links.hh[l,'Value']
    }
    
    links.hh2 <- links.h %>% 
      group_by(Source) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Source == 1)
    ctr <- ctr+1
    }
    
    links.fe <- rbind(links.hh2, ele_link)
    
    ene <- nodes %>% 
      filter(Name == 'Energy')
    
    ene_link_val <- as.numeric(ene$No - 1)
    
    p <- 0
    
    
    for (m in (nrow(links.h)+1):(nrow(links.h)+ctr)) {
      p <- p+1
      links.h[m,'No'] <- m
      links.h[m,'Target'] <- links.fe[p,'Source']
      links.h[m,'Source'] <- ene_link_val 
      links.h[m,'Value'] <- links.fe[p,'Value']
    }
    
    aa.ne <- aa %>% 
      filter(is.na(energy_source))
    pr <- nodes %>% 
      filter(Name == 'Process')
    pr_link_val <- numeric(0)
    if(!is_empty(pr$No)) {
    pr_link_val <- as.numeric(pr$No - 1)
    }
    fg_link_val <- numeric(0)
    fg <- nodes %>% 
      filter(Name == 'Fugitive')
    if(!is_empty(fg$No)) {
    fg_link_val <- as.numeric(fg$No - 1)
    }
    
    if (!is_empty(aa.ne$s_no)){
    o <- 0
    for (q in (nrow(links.h)+1):(nrow(links.h)+nrow(aa.ne))) {
      o <- o+1
      links.h[q,'No'] <- q
      for (j in 1:nrow(nodes)) {
        if (aa.ne[o,'emission_source'] == nodes[j,'Name']){
          links.h[q,'Target'] <- nodes[[j,'No']] - 1
        }
      }
      for (j in 1:nrow(nodes)) {
        if (aa.ne[o,'emission_category'] == nodes[j,'Name']){
          links.h[q,'Source'] <- nodes[[j,'No']] - 1
        }
      }
      links.h[q,'Value'] <- aa.ne[o,'co2e_emissions_mt_co2e_yr']
    }
    }
    
    if (!is_empty(pr_link_val) & !is_empty(fg_link_val)) {
    links.t <- links.h %>% 
      filter(Source == pr_link_val | Source == ene_link_val | Source == fg_link_val) %>% 
      group_by(Source) %>% 
      summarise(Value = sum(Value))
    } else if (!is_empty(pr_link_val) & is_empty(fg_link_val)){
      links.t <- links.h %>% 
        filter(Source == pr_link_val | Source == ene_link_val) %>% 
        group_by(Source) %>% 
        summarise(Value = sum(Value))
    } else if (is_empty(pr_link_val) & !is_empty(fg_link_val)){
      links.t <- links.h %>% 
        filter(Source == fg_link_val | Source == ene_link_val) %>% 
        group_by(Source) %>% 
        summarise(Value = sum(Value))
    } else{
      links.t <- links.h %>% 
        filter( Source == ene_link_val) %>% 
        group_by(Source) %>% 
        summarise(Value = sum(Value))
    }
    
    total_fields <- as.numeric(!is_empty(pr_link_val))+as.numeric(!is_empty(ene_link_val))+as.numeric(!is_empty(fg_link_val))
    
    v <- 0
    for (m in (nrow(links.h)+1):(nrow(links.h)+total_fields)) {
      v <- v+1
      links.h[m,'No'] <- m
      links.h[m,'Target'] <- links.t[v,'Source']
      links.h[m,'Source'] <- 0
      links.h[m,'Value'] <- links.t[v,'Value']
    }
    
    
    links <- links.h
    links <- links %>%
      mutate(Value = round(Value * units_conversion(), input$precision),
             label = paste0(Source, " → ", Target, ": ", Value)) %>% 
      arrange(Source)
    links
  })
  
  
  output$move <- renderText("Note: click and drag each node to 
                            customize the chart \n")
  
  # Create the Sankey diagram
  
  
  s1 <- reactive({
    nodes <- nodes_data() 
    links <- links_data()
    names(nodes) <- c('SN',"Name")
    names(links) <- c('SN',"Source", "Target", "Value","label")
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
      'function(el, x){
    d3.select(el).selectAll(".node text")
      .text(function(d) { 
        var value_str = d.value.toLocaleString();
        if (d.dx < 20) {
          return d.name;
        } else {
          return d.name + " (" + value_str + ")";
        }
      });
  }'
    
    htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
  })
  
  output$sankey <- renderSankeyNetwork(s1())
  
  output$diagram <- renderUI({
    temp <- temp ()
    nr <- nrow(temp)*input$vsc
    ht <- paste0(nr,"px")
    sankeyNetworkOutput("sankey", height = ht)
  })
  
  output$output_text <- renderUI({
    req(input$file)
    if (nchar(input$cname) > 0) {
      paste0("Facility CO₂e Flow for ", input$cname, " (" , input$units, ")")
    } else {
      paste0("Facility CO₂e Flow ","(" ,input$units,")")
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
      names(links) <- c("SN","Source", "Target", "Value","label")
      sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "Source",
                              Target = "Target", Value = "Value", NodeID = "Name",
                              LinkGroup = "label", sinksRight = F, fontSize = 14, nodeWidth = 30, 
                              colourScale = JS("d3.scaleSequential(d3.interpolateViridis);"))
      
      
      javascript_string <- '
function(el, x) {
  var format = d3.format(",");
  d3.select(el).selectAll(".node text")
    .text(function(d) { 
      var value_str = format(d.value);
      if (d.dx < 20) {
        return d.name;
      } else {
        return d.name + " (" + value_str + ")";
      }
    });
}'
      sankey <- htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
      # Save the widget to the temporary HTML file
      saveWidget(sankey, tmp_file)
      
      # Take a screenshot of the HTML file and save it to the output file
      x <- tempfile(fileext = ".png")
      webshot(tmp_file, x,
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
        caption <- paste0("Facility CO2e Flow for ", input$cname,"(" ,un,")")
      } else {
        caption <- paste0("Facility CO2e Flow ","(" ,un,")")
      }
      img <- image_annotate(img1, caption, 
                            size = 100, color = "black", gravity = "North", 
                            location = "+0+10%")
      # write the annotated image to file
      image_write(img, path = file)
      # Delete the temporary file
      unlink(tmp_file)
      unlink(x)
    }
  )
}


shinyApp(ui, server)



