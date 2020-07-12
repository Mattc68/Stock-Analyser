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
returns_long <- returns_long %>% filter(Date >= "2009-01-01")

save(returns_long, file = "returns_long.RData")



# CALCULATE PERFORMANCE STATISTICS
returns_long$thirty_day <- NA
returns_long$ninety_day <- NA
returns_long$one_year <- NA
returns_long$three_year <- NA
returns_long$five_year <- NA
returns_long$ten_year <- NA

performance <- as.data.frame(matrix(NA, ncol = 14, nrow = 0))


for(ticker in unique(returns_long$Ticker)){
  print(ticker)

    # filter returns long by symbol
    ticker_returns_long <- returns_long %>% filter(Ticker == ticker, Series == "Close") %>% arrange(desc(Date))
    
    for(i in 1:nrow(ticker_returns_long)){
      if(i + 21 < nrow(ticker_returns_long)){
        thirty_day <- (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+21])/ticker_returns_long$Value[i+21]
      }
      
      if(i + 63 < nrow(ticker_returns_long)){
        ninety_day <- (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+63])/ticker_returns_long$Value[i+63]
      }
      
      if(i + 253 < nrow(ticker_returns_long)){
        one_year <- (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+253])/ticker_returns_long$Value[i+253]
      }
      
      if(i + 759 < nrow(ticker_returns_long)){
        three_year <- (1 + (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+759])/ticker_returns_long$Value[i+759])^(1/3) - 1
      }
      
      if(i + 1265 < nrow(ticker_returns_long)){
        five_year <- (1 + (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+1265])/ticker_returns_long$Value[i+1265])^(1/5) - 1
      }
      
      if(i + 2530 < nrow(ticker_returns_long)){
        ten_year <- (1 + (ticker_returns_long$Value[i] - ticker_returns_long$Value[i+2530])/ticker_returns_long$Value[i+2530])^(1/10) - 1
      }
      
      ticker_returns_long[i, 9] <- thirty_day
      ticker_returns_long[i, 10] <- ninety_day
      ticker_returns_long[i, 11] <- one_year
      ticker_returns_long[i, 12] <- three_year
      ticker_returns_long[i, 13] <- five_year
      ticker_returns_long[i, 14] <- ten_year
    }
  
    performance <- rbind(performance, ticker_returns_long)
    
}

performance <- performance %>% select(-Series, -Value, -Movement, -Name, -Sector, -Industry) %>% gather("Period", "Returns", -Date, -Ticker)

performance <- performance %>% mutate(
  Period = case_when(
    Period == "thirty_day" ~ "30 Day",
    Period == "ninety_day" ~ "90 Day",
    Period == "one_year" ~ "1 Year",
    Period == "five_year" ~ "5 Year",
    Period == "ten_year" ~ "10 Year",
  )
)

save(performance, file = "performance.RData")

###

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_year", "Five_Year", "Ten_Year")

i = 1

for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  # filter returns long by symbol
  ticker_returns_long <- returns_long %>% filter(Ticker == ticker, Series == "Close") %>% arrange(desc(Date))
  
  thirty_day <- (ticker_returns_long$Value[1] - ticker_returns_long$Value[21])/ticker_returns_long$Value[21]
  ninety_day <- (ticker_returns_long$Value[1] - ticker_returns_long$Value[63])/ticker_returns_long$Value[63]
  one_year <- (ticker_returns_long$Value[1] - ticker_returns_long$Value[253])/ticker_returns_long$Value[253]
  three_year <- (1 + (ticker_returns_long$Value[1] - ticker_returns_long$Value[759])/ticker_returns_long$Value[759])^(1/3) - 1
  five_year <- (1 + (ticker_returns_long$Value[1] - ticker_returns_long$Value[1265])/ticker_returns_long$Value[1265])^(1/5) - 1
  ten_year <- (1 + (ticker_returns_long$Value[1] - ticker_returns_long$Value[2530])/ticker_returns_long$Value[2530])^(1/10) - 1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thirty_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i = i + 1
  
}

performance_summary <- performance_summary %>% left_join(sp500, by = c('Ticker' = 'Ticker'))
save(performance_summary, file = "performance_summary.RData")
