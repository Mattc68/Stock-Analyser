library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

load("returns_long.RData")
load("performance_summary.RData")
load("Sp500.RData")

ui <- dashboardPage(
  dashboardHeader(title = "Stock Analyser"),
  dashboardSidebar(
    selectInput("ticker_select", "Ticker", sp500$Ticker)
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      
      #name_out {
        font-size: 24px;
        font-weight: bold;
        color: #000000;
      }
      
      #location_out {
        font-size: 18px;
        color: #000000;
      }
      
      #sector_out {
        font-size: 18px;
        color: #000000;
        margin-bottom: 20px;
      }
    
    '))),
    fluidRow(
      column(
        width = 5,
        textOutput("name_out"),
        textOutput("location_out"),
        textOutput("sector_out")
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(width = '100%',
            solidHeader = TRUE,
            style = "background-color: #17202A;",
            plotOutput('price_chart')
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 8,
          solidHeader = TRUE,
          style = "background-color: #17202A;",
          plotOutput('industry')
        ),
        box(
          width = 4,
          solidHeader = TRUE,
          style = "background-color: #17202A;",
          plotOutput('performance')
        )
      )
    )
  )
)