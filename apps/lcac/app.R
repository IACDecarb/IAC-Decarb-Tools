##Application starts here
library(ggmacc)
library(readxl)
library(janitor)
library(ggplot2)
library(scales)
library(ggrepel)
library(stringr)
library(dplyr)
library(pals)
library(gridGraphics)
library(gridExtra)
library(ggpubr)
library(shiny)
library(viridis)
library(shinythemes)
library(shinyBS)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(HTML("Levelized Cost of Avoided CO<sub>2</sub>e"),
             windowTitle = "LCAC Calculator"),
  #  downloadButton("download", "Download"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("
      #downloadData1 {
        font-weight: bold;
        font-size: 18px;
      }
    ")),
      downloadLink("downloadData1", "Download LCAC - Input Sheet"),
      br(),
      br(),
      textInput("cname", "Enter Facility Name"),
      fileInput("file", "Upload input sheet"),
      numericInput("pem", "Enter Total Plant CO₂e Emissions ", 0),
      selectInput("type", "Choose Level of Detail", c("Individual Measures", "Summarized")),
      selectInput('tc','Show Total Avoided CO₂e', c('Yes','No'),'Yes'),
      selectInput('anc','Show Annualized Expenditure', c('Yes','No'),'Yes'),
      selectInput('avc','Show Average Cost of Abatement', c('Yes','No'),'Yes'),
      selectInput('tpc','Show Total Plant CO₂e', c('Yes','No'),'No'),
      numericInput("yaxmax", "Increase y-axis upper limit by", 0, -20000, 25000, 10),
      numericInput("yaxmin", "Decrease y-axis lower limit by", 0, -20000, 25000, 10),
      numericInput("xaxmax", "Increase x-axis upper limit by", 0, -20000, 25000, 10),
      numericInput("rleg", "Specify Number of rows for Legend", 4, 1, 100, 1),
      bsCollapse(id = "collapsePanel", open = "",
                 bsCollapsePanel(title = HTML("<div style='background-color: #98A4A4; padding: 10px; border-radius: 4px; text-align: left; width: 100%; margin: 0 auto;'><span style='font-size: 16px; color: white;'>Click here to adjust positioning of text on plot &#9660;</span></div>"),
                                 numericInput("acoh", "Adjust horizontal position of \'Average Cost\' label", 0, -200000, 250000, 10),
                                 numericInput("aco", "Adjust vertical position of \'Average Cost\' label", 0, -200000, 250000, 10),
                                 numericInput("ancosth", "Adjust horizontal position of \'Annualized Expenditure\' label", 0, -200000, 250000, 10),
                                 numericInput("ancost", "Adjust vertical position of \'Annualized Expenditure\' label", 0, -200000, 250000, 10),
                                 numericInput("totch", "Adjust horizontal position of \'Total Avoided CO₂e\' label", 0, -200000, 250000, 10),
                                 numericInput("totc", "Adjust vertical position of \'Total Avoided CO₂e\' label", 0, -200000, 250000, 10),
                                 numericInput("cprh", "Adjust horizontal position of \'CO₂ Cost\' label", 0, -200000, 250000, 10),
                                 numericInput("cpr", "Adjust vertical position of \'CO₂ Cost\' label", 0, -200000, 250000, 10),
                                 numericInput("pemh", "Adjust horizontal position of \'Total Plant CO₂e\' label", 0, -200000, 250000, 10),
                                 numericInput("pemv", "Adjust vertical position of \'Total Plant CO₂e\' label", 0, -200000, 250000, 10)
                 )
      ),
      downloadButton("downloadPNG", "Click Here to Download plot as Image"),
      br(),
      br(),
      tags$style(HTML("
      #downloadData2 {
        font-weight: bold;
        font-size: 18px;
      }
    ")),
      downloadLink("downloadData2", "Download Tool Documentation")
    ),
    mainPanel(
      
      tags$div(
        style = "width: 100%;position: sticky; top: 0;",
        plotOutput("curve1", width = "100%", height = '600px')
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
server <- function(input, output, session){
  
  excelFilePath <- "LCAC Input Sheet.xlsx"
  
  observeEvent(input$toggleBtn, ({
    updateCollapse(session, "collapsePanel")
  }))
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      # Set the filename of the downloaded file
      basename(excelFilePath)
    },
    content = function(file) {
      # Copy the file to the specified download location
      file.copy(excelFilePath, file)
    }
  )
  
  docFilePath <- "User Guide for LCAC Tool.pdf"
  output$downloadData2 <- downloadHandler(
    filename = function() {
      # Set the filename of the downloaded file
      basename(docFilePath)
    },
    content = function(file) {
      # Copy the file to the specified download location
      file.copy(docFilePath, file)
    }
  )
  
  sen1 <- reactive({
    req(input$file)
    s1 <- read_excel(input$file$datapath, sheet = "Assessment Recommendations", range = "d29:an200")
    s1 <- clean_names(s1) 
    s1 <- s1[!is.na(s1$assessment_recommendation) & s1$assessment_recommendation != "", ]
    s1 <- s1 %>%
      mutate(annualized_avoided_co2e = as.numeric(annualized_avoided_co2e)) %>% 
      mutate(annualized_total_costs = as.numeric(annualized_total_costs)) %>%
      mutate(levelized_cost_of_avoided_co2e = as.numeric(levelized_cost_of_avoided_co2e)) %>% 
      mutate(total_cost = levelized_cost_of_avoided_co2e*annualized_avoided_co2e)
    
    if (input$type == "Summarized") {
      s1 <- s1 %>% 
        group_by(decarbonization_pillar) %>% 
        summarise(
          total_cost = sum(annualized_total_costs),
          annualized_avoided_co2e = sum(annualized_avoided_co2e),
          levelized_cost_of_avoided_co2e = total_cost/annualized_avoided_co2e
        )
      s1 <- s1 %>% 
        rename(assessment_recommendation=decarbonization_pillar)
    }
    else {
      s1 <- s1
    }
    
    carbon_price <- as.numeric(read_excel(input$file$datapath, 
                                          sheet = "Assessment Recommendations", 
                                          range = "e23:e23",col_names = FALSE))
    
    
    
    
    
    
    total_cost_s1 <- as.numeric(format(round(sum(s1$total_cost))))
    
    
    macc_average <- as.numeric(format(round(sum(s1$total_cost)/sum(s1$annualized_avoided_co2e),0)), nsmall=0) ## Calculating the average Marginal abatement cost to illustrate in the diagram later
    
    s1_carbon_abated <- as.numeric(format(round(sum(s1$annualized_avoided_co2e),0)), nsmall=0) ##Total carbon needed to abate to reach netzero
    
    options(scipen=999) ## This code remove scientific notation from the graph
    ## Color Coding
    num_colors <- length(s1$assessment_recommendation)
    
    # Generate a color palette with the specified number of colors
    color_palette <- viridis(num_colors)
    y_max <- 1.05*(max(max(s1$levelized_cost_of_avoided_co2e), carbon_price))
    y_min <- 1.05*(min(min(s1$levelized_cost_of_avoided_co2e), macc_average,0))
    s1_fig <- s1 %>% 
      mutate(assessment_recommendation = factor(assessment_recommendation, levels = assessment_recommendation[order(levelized_cost_of_avoided_co2e)])) %>% 
      ggmacc(mac = levelized_cost_of_avoided_co2e, abatement = annualized_avoided_co2e, fill=assessment_recommendation) +
      
      theme_bw() +
      scale_x_continuous(labels = comma,limits = c(0,(max(s1_carbon_abated)+input$xaxmax)+20), name = 
                           "MT CO₂e/yr") +
      scale_y_continuous(name = "Levelized Cost of Avoided CO₂e ($/MT CO₂e Avoided)",
                         limits = c(y_min-50-input$yaxmin,y_max+50+input$yaxmax))  +
      theme(axis.title = element_text(size = 15),
            plot.subtitle = element_text(size = 15), 
            axis.text = element_text(size = 15), 
            legend.title = element_text(size=12), 
            legend.text = element_text(size = 10), 
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 20))+
      scale_fill_manual(values = color_palette)+
      ggtitle(paste0("Levelized Cost of Avoided CO₂e Curve"))+
      guides(fill=guide_legend(title="Decarbonization \nMeasures",nrow = input$rleg))
    
    if(input$tc == 'Yes' & input$pem == 0){
    s1_fig <- s1_fig +
      geom_vline(xintercept = s1_carbon_abated,linetype = c("dashed"), color = c("black")) +
    geom_text(data=data.frame((x=(9.2*s1_carbon_abated/10)+input$totch),y=min(s1$levelized_cost_of_avoided_co2e)+input$totc), aes(x, y), label=str_wrap(paste(
      "Total Avoided CO₂e = ",comma(s1_carbon_abated)," MT CO₂e/yr", sep = ""), width = 11)
      ,size = 4.5, color = "black")
    }
    
    if(input$anc == 'Yes'){
      s1_fig <- s1_fig +
        geom_text(data=data.frame(x=(s1_carbon_abated/20)+input$ancosth,y=max(s1$levelized_cost_of_avoided_co2e)+input$ancost), aes(x, y), label=str_wrap(paste(
          'Annualized Expenditure = ',"$",comma(total_cost_s1),"/yr",sep=""), width = 11)
          ,size = 4.5, color = "black")
    }
    
    if(input$avc == 'Yes'){
      s1_fig <- s1_fig +
        geom_hline(yintercept = c(0,macc_average), linetype = c("solid", "dashed"), 
                   color = c("black","black")) +
        geom_text(data=data.frame(x=(s1_carbon_abated/2)+input$acoh,y=macc_average+input$aco), aes(x, y), 
                  label=str_wrap(paste("Average Cost = ","$",round(macc_average,digits = 1),
                                       "/MT CO₂e",sep=""), width = 15), size =4, color = "black")
    }
    
    if(carbon_price != 0){
      s1_fig <- s1_fig +
        geom_hline(yintercept = carbon_price, linetype = "dashed") +
        geom_text(data=data.frame(x=(s1_carbon_abated/4)+input$cprh,y=carbon_price+input$cpr), aes(x, y), label=str_wrap(paste(
          "CO₂ Cost = ","$",carbon_price,"/MT CO₂", sep = ""), width=11), size =4)
    }
    
    if(input$cname != ""){
      s1_fig <- s1_fig +
        ggtitle(paste0("Levelized Cost of Avoided CO₂e Curve for ", input$cname))
    }
    
    if(input$pem != 0) {
      calc_percent <-  round(s1_carbon_abated/input$pem,3)*100
      s1_fig <- s1_fig +
        geom_text(data=data.frame((x=(9.2*s1_carbon_abated/10)+input$totch),y=min(s1$levelized_cost_of_avoided_co2e)+input$totc), aes(x, y), label=str_wrap(paste(
          "Total Avoided CO₂e = ",comma(s1_carbon_abated)," MT CO₂e/yr\n","(",calc_percent,"%)", sep = ""), width = 11)
          ,size = 4.5, color = "black")
    }
    
    if(input$tpc == 'Yes'){
      s1_fig <- s1_fig +
        geom_vline(xintercept = input$pem,linetype = c("dashed"), color = c("black"))+
        scale_x_continuous(labels = comma,limits = c(0,(max(input$pem,s1_carbon_abated)+input$xaxmax)+20), name = 
                             "MT CO₂e/yr")+
        geom_text(data=data.frame((u=(9.2*input$pem/10)+input$pemh),v=min(s1$levelized_cost_of_avoided_co2e)+input$pemv), aes(u, v), label=str_wrap(paste(
          "Total Plant CO₂e = ",comma(input$pem)," MT CO₂e/yr", sep = ""), width = 11)
          ,size = 4.25, color = "black")
    }
    
    s1_fig
    
  })
  output$curve1 <- renderPlot(sen1())
  output$downloadPNG <- downloadHandler(
    filename = "LCAC.png",
    content = function(file) {
      ggsave(file, plot = sen1(), device = "png", height = 30, width = 45,
             units = "cm", dpi= 400)
    })
  
  
}

shinyApp(ui = ui, server = server)


