library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

server <- function(input, output){
  # Generate Output Variables
  
  output$price_chart <- renderPlot({
    price_data <- returns_long %>% filter(Ticker == input$ticker_select, Date >= as.Date("2019-02-28"), Series == "Close")
    price_data$Date <- as.Date(price_data$Date)
    
    price_chart <- ggplot(price_data) + 
      geom_line(aes(Date, Value), color = "#0066ff") +
      xlab("Date") + 
      ylab("USD") +
      labs(
        title = "Price per share",
        caption = "Source: Yahoo! Finance"
      ) +
      scale_y_continuous(labels = scales::dollar) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#000000"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill = "#17202A", color = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#333333"),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 9),
        legend.position = "none"
      )
    
    price_chart
  })
  
  output$industry <- renderPlot({
    sector <- sp500 %>% filter(Ticker == input$ticker_select) %>% select(Sector) %>% as.character()
    industry <- sp500 %>% filter(Ticker == input$ticker_select) %>% select(Industry) %>% as.character()
    
    industry_summary_charting <- performance_summary %>% 
      filter(Sector == sector) %>% 
      mutate(
        isIndustry = ifelse(Industry == industry, "Industry", "Non_Industry")
      )
    
    Industry_plot <- ggplot(industry_summary_charting) + 
      geom_bar(aes(Industry, One_year, fill = isIndustry), stat = "summary", fun.y = "mean") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values= c(Industry = "#ffff00", Non_Industry = "#0066ff")) +
      ylab("Annual Returns") +
      labs(
        title = "Industry Returns",
        caption = "Source: Yahoo! Finance"
      ) +
      theme(
        plot.background = element_rect(fill = "#17202A", color = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, color = '#ffffff'),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#273746"),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
      )
    
    Industry_plot
  })
  
  output$performance <- renderPlot({performance_summary_charting <- performance_summary %>% 
    filter(Ticker == input$ticker_select) %>% 
    select(Thirty_days, Ninety_days, One_year, Three_year, Five_Year, Ten_Year)
  
  performance_summary_charting <- performance_summary_charting %>% gather("Period", "Return")
  
  performance_summary_charting <- performance_summary_charting %>% mutate(
    Period = case_when(
      Period == 'Thirty_days' ~ "30 Days",
      Period == 'Ninety_days' ~ "90 Days",
      Period == 'One_year' ~ "1 Year",
      Period == 'Three_year' ~ "3 Years",
      Period == 'Five_Year' ~ "5 Years",
      Period == 'Ten_Year' ~ "10 Years"
    )
  )
  
  performance_summary_charting$Period <- factor(performance_summary_charting$Period, levels = c("30 Days", "90 Days", "1 Year", "3 Years", "5 Years", "10 Years"))
  
  Performance_plot <- ggplot(performance_summary_charting) + 
    geom_bar(aes(Period, Return), stat = "identity", fill = "#0066ff") +
    # scale_fill_manual(values = c(`30 Days` = "#0066ff", `90 Days` = "#0066ff", `1 Year` = "#ffff00", `3 Years` = "#0066ff", `5 Years` = "#0066ff", `10 Years` = "#0066ff")) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Returns") +
    labs(
      title = "Returns by Time Period",
      caption = "Source: Yahoo! Finance"
    ) +
    theme(
      plot.background = element_rect(fill = "#17202A", color = "#17202A"),
      panel.background = element_rect(fill = "#17202A"),
      axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, color = '#ffffff'),
      axis.text.y = element_text(color = "#ffffff"),
      axis.title.x = element_text(color = "#ffffff"),
      axis.title.y = element_text(color = "#ffffff"),
      plot.title = element_text(color = "#ffffff"),
      plot.subtitle = element_text(color = "#ffffff"),
      plot.caption = element_text(color = "#ffffff", face = "italic", size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "#273746"),
      panel.grid.minor.y = element_blank(),
      legend.position = "none"
    )
  
  Performance_plot
  })
  
  name <- reactive({sp500 %>% filter(Ticker == input$ticker_select) %>% select(Name) %>% as.character()})
  hq_city <- reactive({sp500 %>% filter(Ticker == input$ticker_select) %>% select(HQ_City) %>% as.character()})
  hq_location <- reactive({sp500 %>% filter(Ticker == input$ticker_select) %>% select(HQ_Location) %>% as.character()})
  sector <- reactive({sp500 %>% filter(Ticker == input$ticker_select) %>% select(Sector) %>% as.character()})
  industry <- reactive({sp500 %>% filter(Ticker == input$ticker_select) %>% select(Industry) %>% as.character()})
  
  output$ticker_out <- renderText({input$ticker_select})
  output$name_out <- renderText({name()})
  output$location_out <- renderText({paste0(hq_city(), ", ", hq_location())})
  output$sector_out <- renderText({sector()})
  output$industry_out <- renderText({industry()})
}

shinyApp(ui, server)

# library(rsconnect)
# setAccountInfo(name='mmcleary6',
#                 token='E75B795F3AF4182EE1D8599A8C91BDA5',
#                 secret='tDg4hmBjmgE7L2cFWXyWcisVk+sg3np4kHx3GXtn')
# deployApp()



