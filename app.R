#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Shiny-App for hydroTS
# build by FS | 07.2024

library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse)
library(scales)
library(plotly)
library(shinythemes)

# Data specific functions to read data
# readGRDC <- function(path) {
#   df <- read.csv(path, sep = ";", dec = ".", header = TRUE, na.strings = "-999.000", col.names = c("date","time","Q")) %>% dplyr::select(date,Q)
#   df$date <- as.POSIXct(df$date, format = "%Y-%m-%d", tz="UTC")
#   
#   return(df)
# }

readTS <- function(path) {
  df <- read.csv(path, sep = ";", dec = ".", )
}



# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = h2("hydroTS"),
  theme = shinytheme(theme = "flatly"),
  # tabPanel("Plot", "one"),
  # tabPanel("Summary","two"),
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Format of dataset
    radioButtons("dataFormat", label = "1) TS-Format",
                 choices = list("date-time | value"="dv",
                                "date | time | value"="dtv"),
                 selected = "dv"),
    
    # Insert date-format 
    textInput("dateLabel", label = "2) Date format", value = "%Y-%m-%d %H:%M:%S"),
    # fluidRow(column(3, verbatimTextOutput("dateLabel"))),
    
    # Choose data separate format
    selectInput("sepFormat", label = "3) Separate",
                 choices = list("Semicolon"=";",
                                "Comma"=",",
                                "Blank space" =" ",
                                "Tab"="\t"),
                 selected = ";"),
    
    # Choose dec-separator
    selectInput("decFormat", label = "4) Decimal separator",
                choices = list("." = ".", "," = ","),
                selected = "."),
    
    # Header TRUE/FALSE
    checkboxInput("header", label = "First row is header", value = TRUE),
    
    # Path to data
    fileInput("dataset", label = "5) File input"),
    # Summary from Input
    # fluidRow(column(12, verbatimTextOutput("summary"))),
    
    
    
    # Choose date range
    # dateRangeInput("dates", label = "Date range",
    #                start = ),
    # fluidRow(column(12, verbatimTextOutput("value")))
  ),
  
  # Main panel for visualizations ----
  navset_card_tab(
    nav_panel("Plot", plotlyOutput("tsPlot")),
    nav_panel("Summary", fluidRow(column(12, verbatimTextOutput("summary")))),
    nav_panel("Extremes", plotlyOutput("extremePlot"), 
              fluidRow(column(12,verbatimTextOutput("extremeSummary"))))
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Upload-Limit 50 Mb
  options(shiny.maxRequestSize=110*1024^2)
  
  
  datasetInput <- eventReactive(input$dataset, {
    if(is.null(datasetInput)) return(NULL)
    req(input$dataFormat)
    req(input$dateLabel)
    req(input$sepFormat)
    req(input$decFormat)
    req(input$header)
    
    if(input$dataFormat=="dv") {
      df <- read.csv(input$dataset$datapath, 
                     sep = as.character(input$sepFormat), 
                     dec = as.character(input$decFormat), 
                     header = input$header,
                     col.names = c("date","value","test"))
      df$date <- as.POSIXct(df$date, format = as.character(input$dateLabel))
      return(df)
    } else if(input$dataFormat=="dtv") {
      df <- read.csv(input$dataset$datapath, 
                     sep = as.character(input$sepFormat), 
                     dec = as.character(input$decFormat), 
                     header = input$header,
                     col.names = c("date","time","value"))
      df$date <- as.POSIXct(df$date, format = as.character(input$dateLabel))
      return(df)
    }
    
    # df[1] <- as.POSIXct(df[1], format = input$dateLabel)
  })
  
  amaxInput <- reactive({
    df.AMAX <- datasetInput()
    df.AMAX <- df.AMAX %>% dplyr::mutate(year=lubridate::year(date)) %>% dplyr::group_by(year) %>% dplyr::summarise(amax = max(value,na.rm = TRUE))
    df.AMAX$year <- lubridate::ymd(df.AMAX$year, truncated = 2L)
    df.AMAX <- as.data.frame(df.AMAX)
  })

  # datasetInput <- reactive({
  #   df <- readGRDC(input$dataset$datapath)
  # })
  
  # Summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Timeseries plot
  output$tsPlot <- renderPlotly({
    dataset <- datasetInput()
      ggplot(dataset, aes(date,value)) + 
        geom_line(color="steelblue4") +
        scale_x_datetime(name = NULL,
                         date_breaks = "5 years",
                         date_labels = "%Y") +
        labs(y="Value",x=NULL) + 
        theme_bw()
  })
  
  # Extremes plot
  output$extremePlot <- renderPlotly({
    df.AMAX <- amaxInput()
    
    ggplot(df.AMAX, aes(year, amax)) +
      geom_col(fill = "steelblue4") +
      scale_x_date(name = "Year",
                   date_breaks = "5 years",
                   date_labels = "%Y") +
      labs(y="Value") +
      theme_bw()
  })
  
  # Extremes summary
  output$extremeSummary <- renderPrint({
    df.AMAX <- amaxInput()
    summary(df.AMAX)
  })
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
