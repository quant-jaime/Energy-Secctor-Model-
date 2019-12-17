# Energy-Secctor-Model-


library(quantmod)
library(tseries)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(knitr)
options(scipen=999) # -- To disable scientific notation in R

stock1 <- getSymbols.yahoo("CVX", auto.assign=F)
stock2 <- getSymbols.yahoo("EOG", auto.assign=F)

stock1 <- stock1[, grep("Adjusted", colnames(stock1))]

stock2 <- stock2[, grep("Adjusted", colnames(stock2))]

cut_off_date <- as.Date("2018-01-01")
stock1 <- stock1[index(stock1) >= cut_off_date]
stock2 <- stock2[index(stock2) >= cut_off_date]

dim(stock1[is.na(stock1),])
dim(stock2[is.na(stock2),])

stock1 <- na.locf(stock1)
stock2 <- na.locf(stock2)

ret_stock1 <- Delt(stock1)
ret_stock2 <- Delt(stock2)


data <- data.frame(matrix(NA, dim(ret_stock1)[1], 2))
data[, 1] <- ret_stock1
data[, 2] <- ret_stock2
data <- xts(data, index(ret_stock1))
head(data)

correlation <- function(x) {
  result <- cor(x[, 1], x[, 2])
  return(result)
}
corr <- rollapply(data, 478, correlation, by.column = FALSE)
plot(corr)


hedge_ratio <- stock1/stock2
(head(hedge_ratio))


n_period <- 30
roll_me <- rollapply(hedge_ratio, n_period, mean)
roll_std <- rollapply(hedge_ratio, n_period, sd)


n <- 1
roll_ub <- roll_me + roll_std*n
roll_lb <- roll_me - roll_std *n

plot(cbind(hedge_ratio, roll_ub, roll_lb))


signal <- NULL
signal <- ifelse(
  hedge_ratio > roll_ub, -1, ifelse(
    hedge_ratio < roll_lb, 1, 0
  )
)



signal <- lag(signal, 1)


spread_return <- ret_stock1 - ret_stock2*hedge_ratio
trade_return <- spread_return*signal




charts.PerformanceSummary(trade_return)


##### QUANT METRICS 
print(paste0("Cumulative Returns -- ", Return.cumulative(trade_return)))

print(paste0("Annualized Returns -- ", Return.annualized(trade_return)))

print(paste0("Maximum Drawdown -- ", maxDrawdown(trade_return)))

print(paste0("Sharpe Ratio -- ", SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")))

