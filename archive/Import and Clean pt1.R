#1
library(dplyr)
library(ggplot2)
library(rvest)
library(tidyr)
library(stringr)
library(readr)
library(plotly)

#2
setwd("C:/Users/clear/Workspace/Project/Stock Analyser")

## Get S&P 500 Tickers
#3
sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

#4
sp500 <- sp500 %>% 
  html_nodes("#constituents") %>% 
  html_table()

sp500 <- sp500[[1]]

#5
sp500 <- sp500 %>% select(Symbol, Security, `GICS Sector`, `GICS Sub Industry`, `Headquarters Location`)
names(sp500) <- c("Ticker", "Name", "Sector", "Industry", "HQ_Location")


# First fix rows 28, 281, 294
sp500 <- sp500 %>% separate(HQ_Location, c("HQ_City", "HQ_Location"), sep = ", ")

save(sp500, file = "Sp500.RData")
# COMMENT OUT THE REST OF THE SP500 Code


# Get Returns
returns <- as.data.frame(matrix(NA, nrow = 0, ncol = 8))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj Close ", "Volume", "Ticker")

#aapl <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=345427200&period2=1591488000&interval=1d&events=history"))
#ret <- try(suppressMessages(read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/BRK.B?period1=345427200&period2=1591488000&interval=1d&events=history"))), silent = T)

# https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1434412800&period2=1592265600&interval=1d&events=history
# https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1560643200&period2=1591920000&interval=1d&events=history

# WIP Incorporate at the end to make dynamic up
# end_date <- as.numeric(difftime(Sys.Date(), as.Date("1970-01-01"), "minutes"))*24*60*60

i = 0
t0 <- Sys.time()
for(symbol in sp500$Ticker){
  
  i = i + 1
  #symbol = str_replace_all(symbol, "[.]", "")
  print(paste(i, symbol))
    ret <- try(suppressMessages(read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol,"?period1=345427200&period2=1591488000&interval=1d&events=history"))), silent = T)
    if(mode(ret) != "character"){
      ret$Ticker <- symbol
      returns <- rbind(returns, ret)
    }
}
t1 <- Sys.time()
t1 - t0

names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close ", "Volume", "Ticker")
returns <- returns %>% select("Date", "Ticker", "Open", "High", "Low", "Close")
returns$Movement <- ifelse(returns$Close > returns$Open, "Up", "Down")

save(returns, file = "returns.RData")
# Comment out previous code

returns_long <- returns %>% gather("Series", "Value", -Date, -Ticker, -Movement)
returns_long <- returns_long %>% left_join(sp500 %>% select(Ticker, Name, Sector, Industry), by = c("Ticker" = "Ticker"))
returns_long$Value <- as.numeric(returns_long$Value)


save(returns_long, file = "returns_long.RData")