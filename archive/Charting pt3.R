library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

setwd("C:/Users/clear/Workspace/Project/Stock Analyser")

load("returns_long.RData")

ticker <- "AMZN"

###

charting_data <- returns_long %>% filter(Ticker == ticker, Date >= as.Date("2020-03-06"))
charting_data$Date <- as.Date(charting_data$Date)

stock_name <- paste0(charting_data$Name[1], " (", ticker, ")")
stock_sector <- charting_data$Sector[1]
stock_industry <- charting_data$Industry[1]

candlestick <- ggplot(charting_data) + 
  geom_boxplot(aes(as.factor(Date), Value, fill = Movement), color = "#D0D3D4", width = 0.2) +
  scale_fill_manual(values= c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Stock Price") +
  labs(
    # title = paste0(charting_data$Name[1], " (", ticker, ")"),
    # subtitle = charting_data$Sector[1],
    title = "60 Day Price",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, color = "#000000", size = 10),
    axis.text.y = element_text(color = "#000000"),
    axis.title.x = element_text(color = "#000000"),
    axis.title.y = element_text(color = "#000000"),
    plot.title = element_text(color = "#000000"),
    plot.subtitle = element_text(color = "#000000"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "#17202A", color = "#17202A"),
    panel.background = element_rect(fill = "#17202A"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#333333"),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(color = "#999999", face = "italic", size = 6),
    legend.position = "none"
  )

# candlestick

###

price_data <- returns_long %>% filter(Ticker == ticker, Date >= as.Date("2010-03-06"), Series == "Close")
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
    axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, color = "#ffffff", size = 10),
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
    plot.caption = element_text(color = "#999999", face = "italic", size = 6),
    legend.position = "none"
  )

price_chart

###

load("performance.RData")

rolling_avg_chart_data <- performance %>% filter(Ticker == ticker, Date >= as.Date("2019-03-01"))

rolling_avg_chart <- ggplot(rolling_avg_chart_data) + geom_line(aes(Date, Returns, color = Period))
rolling_avg_chart

###

load("performance_summary.RData")
load("Sp500.RData")

sector <- sp500 %>% filter(Ticker == ticker) %>% select(Sector) %>% as.character()
industry <- sp500 %>% filter(Ticker == ticker) %>% select(Industry) %>% as.character()

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
    plot.caption = element_text(color = "#ffffff", face = "italic", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#273746"),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

# Industry_plot

###

performance_summary_charting <- performance_summary %>% 
  filter(Ticker == ticker) %>% 
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
    plot.caption = element_text(color = "#ffffff", face = "italic", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#273746"),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

# Performance_plot

