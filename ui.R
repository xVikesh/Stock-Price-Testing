library(shiny)

shinyUI(fluidPage(
  titlePanel("Stock Price Testing"),
  
  verticalLayout(
    fluidRow(sidebarPanel(
      
      list(tags$head(tags$style("body {background-color: #E0F2F7; }"))),
      
  
      helpText("Choose a stock ticker to examine, For example
        ^HSI - Hang Seng, 
        ^N225 - Nikkei 225 and 
        ^FTSE - FTSE 100. 
        Information will be collected from yahoo finance."),
    
      textInput("symb", "Symbol", "^FTSE"),
      bsAlert(inputId = "alert_anchor"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2015-01-01", 
        end = as.character(Sys.Date())),
      textOutput("DateRange"),
      div(style="display:inline-block;left-align:",submitButton("Analysis")),
      div(style="display:inline-block;right-align:",downloadButton('downloadData', 'Download Data')),width=6
    ),
       
    
    helpText( "Criteria for tests: Shapiro Wilk Test - Must choose between 3 and 5000 trading days in your range. ",column(5,htmlOutput("logs")),
    tags$style(type="text/css", "#logs th, td {border: 2px solid #000000;text-align:center}"),
    tags$style(type="text/css", "#logs td {border: 2px solid #000000;text-align:center}"),width=6
  )),
    
   fluidRow(column(6,plotOutput("plot")),column(6,plotOutput("ret.graph"))),
    
   fluidRow(column(4,plotOutput("qq.plot")),column(4,plotOutput("histogram")),column(4,plotOutput("qq.line")))  
    
  
)))

