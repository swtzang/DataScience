# https://books.google.com.tw/books?id=GVxnDwAAQBAJ&pg=PA218&lpg=PA218&dq=getSymbols(Symbols+%3D+symbols,+src+%3D+%22google%22,+from%3Dfrom,+to%3Dto,+adjust+%3D+TRUE)&source=bl&ots=SmwaG2NQLU&sig=ACfU3U19mE9TsxsEksvCqxaRZxg5XdOadw&hl=zh-TW&sa=X&ved=2ahUKEwjY8_KEw47nAhWIHKYKHa-iCHkQ6AEwAHoECAoQAQ#v=onepage&q=getSymbols(Symbols%20%3D%20symbols%2C%20src%20%3D%20%22google%22%2C%20from%3Dfrom%2C%20to%3Dto%2C%20adjust%20%3D%20TRUE)&f=false
# Applied Financial Economics -- programming Excel, VBA and R , Chiu Yu Ko.
#
rm(list=ls())
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)

library(blotter)
library(quantstrat)

from  = "2003-01-01"
to = "2012-12-31"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from  = from , to = to)

stock(symbols, currency = "USD", multiplier = 1)

filter <- function(price){
          lagprice <- lag(price, 1)
          temp <- lagprice/price
          colnames(temp) <- "filter"
          return(temp)
}

strategy.st <- "filter"
portfolio.st <- "filter"
account.st <- "filter"
#
rm.strat(strategy.st)
rm.strat(account.st)

initEq = 100000
initDate = "1990-01-01"

initPortf(name = portfolio.st, 
          symbols = symbols, 
          initDate = initDate, 
          currency = 'USD')
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = initDate,
         currency = 'USD',
         initEq = initEq)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = initDate)

strategy(strategy.st, store = TRUE)

add.indicator(strategy = strategy.st,
              name = "filter", 
              arguments = list(price = quote(Cl(mktdata))), 
              label = "filter")

test <- try(applyIndicators(strategy.st, mktdata = OHLC(AAPL)))

head(test, n = 4)
# enter when filter > 1 + delta
add.signal(strategy.st,
           name = "sigThreshold",
           arguments = list(threshold = 1.05, 
                            column = "filter", 
                            relationship = "gt", 
                            cross = TRUE), 
           label = "filter.gt.0")
# exit when filter < 1 - delta
add.signal(strategy.st, 
           name = "sigThreshold", 
           arguments = list(threshold = 0.95, 
                            column = "filter", 
                            relationship = "lt", 
                            cross = TRUE), 
           label = "filter.lt.0")
# Buy rule
add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = "filter.gt.0", 
                          sigval = TRUE, 
                          orderqty = 1000, 
                          ordertype = 'market', 
                          orderside = 'long', 
                          pricemethod = 'market', 
                          replace = FALSE), 
         type = 'enter', 
         path.dep = TRUE)
# Sell rule
add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = "filter.lt.0", 
                          sigval = TRUE, 
                          orderqty = -1000, 
                          ordertype = 'market', 
                          orderside = 'long', 
                          pricemethod = 'market', 
                          replace = FALSE), 
         type = 'enter', 
         path.dep = TRUE)
# Apply strategy
start_t <- Sys.time()

out <- try(applyStrategy(strategy = strategy.st, 
                         portfolios = portfolio.st))
end_t <- Sys.time()
print("Strategy Loop:")
print(end_t - start_t)

# Update portfolio
updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)

# Charting results
for (symbol in symbols) {
  chart.Posn(Portfolio = portfolio.st,
             Symbol = symbols, 
             log = TRUE)
}

# trade statistics
tstats <- tradeStats(portfolio.st)
t(tstats)

# Performance statistics
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab.perf <- table.Arbitrary(rets, 
                            metrics = c(
                              "Return.cumulative", 
                              "Return.annualized", 
                              "SharpeRatio.annualized", 
                              "CalmarRatio"), 
                            metricsNames = c(
                              "Cumulative Return", 
                              "Annualized Return", 
                              "Annualized Sharperatio", 
                              "Calmar Ratio"))
# Performance summary
charts.PerformanceSummary(rets, colorset = bluefocus)

# Buy and hold strategy
# Get SPY
rm.strat("buyHold")
getSymbols("SPY", from = from, to = to)
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)

# Initial Setup
initPortf("buyHold", "SPY", initDate = initDate)
initAcct("buyHold", portfolios = "buyHold", 
         initDate = initDate, initEq = initEq)

# Since buy and sell are not given by trading rule, we 
# directly add transaction to it. We first add the transaction to buy at the beginning.










                            

















