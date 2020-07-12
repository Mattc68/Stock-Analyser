library(ggplot2)
library(dplyr)

setwd("C:/Users/clear/Workspace/Project/Stock Analyser")

load("returns_long.RData")

ticker <- "AMZN"
charting_data <- returns_long %>% filter(Ticker == ticker, Date >= as.Date("2020-05-06"))
charting_data$Date <- as.Date(charting_data$Date)

candlestick <- ggplot(charting_data) + 
  geom_boxplot(aes(as.factor(Date), Value, fill = Movement), color = "#D0D3D4", width = 0.2) +
  scale_fill_manual(values= c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Stock Price") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, color = "#ffffff"),
    axis.text.y = element_text(color = "#ffffff"),
    axis.title.x = element_text(color = "#ffffff"),
    axis.title.y = element_text(color = "#ffffff"),
    plot.title = element_text(color = "#ffffff"),
    plot.subtitle = element_text(color = "#ffffff"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "#17202A"),
    panel.background = element_rect(fill = "#17202A"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#273746"),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(color = "#999999", face = "italic", size = 6),
    legend.position = "none"
  )

candlestick
