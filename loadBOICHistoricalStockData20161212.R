# loadHistoricalStockData.R

# see; http://www.r-bloggers.com/loading-historical-stock-data/
# June 1, 2013, by systematicinvestor
# Modified by Paul Leiby 6/9/2014, 8/14/2016

rm(list = ls())

# setInternet2(TRUE)  # Set or disable the use of Windows internal functions for Internet access.
# con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
# source(con)
# close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(quantmod)

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Alternative sets of tickers to be fetched
#tickers = spl('EMC,AAPL,QCOM')
indexSymbols = c("^DJI","^GSPC","^RUT", "^IXIC", "^IRX")
indexNames = c("DowJones", "S&P500", "Russell2000", "NASDAQ", "TBill3Mon")
BOICtickers = c("AAPL", "CMI",  "CP",  "DY",  "GIL",  "HAR",
                "IRBT", "JAZZ", "LKQ", "MSM", "QCOM", "R" )

#tickers = indexSymbols
tickers = BOICtickers

stockData <- new.env() # create environment in which xts dataset for each stock symbol will be created

# load historical data, getSymbols from quantmod
getSymbols(Symbols=tickers, src = 'yahoo', from = '1940-01-01', env = stockData, auto.assign = T)    
showSymbols(stockData)  # each member of stockData is an object of class xts (time-series) data
ls(stockData)
tckrs = gsub("\\^","",tickers)  # drop the goofy "^" symbols needed for index symbols
tckrs

head(get(tckrs,envir=stockData))
class(stockData)  # class is "environment," members are xts (multiple) time series
class(stockData[[tckrs[1]]])  # members can be accessed like list or dataframe series

# barChart(AAPL)
barChart(stockData[[tckrs[1]]])

# get closing prices for each series, looping and merging
sxts = get(tckrs[1],envir=stockData)[,paste0(tckrs[1],".Close")]
for (i in 2:length(tckrs)) { # loop, merging each series for closing price
  sxts = merge(sxts, 
        get(tckrs[i],envir=stockData)[,paste0(tckrs[i],".Close")]) 
}

names(sxts) = gsub(".Close", "", names(sxts)) # drop '.Close' from the names
# plot(sxts)

# convert xts-class object to dataframe, and add series with dates
sdf = data.frame(sxts)
sdf$date = as.Date(row.names(sdf))

head(sdf)
summary(sdf)

# melt (reform) data to date, ticker, and closing price
sdf = gather(sdf, key=ticker, value=close, -date) %>% filter(!is.na(close))

# plot the index series
# ggplot(data=sdf, aes(x=date, y=close)) + geom_line(aes(col=ticker))
sdf %>% filter(!is.na(close)) %>% 
  ggplot(aes(x=date, y=close)) + geom_line(aes(col=ticker))

# create growth rates (log-first-differences)
sdf$lclose = log(sdf$close)
sdf = sdf %>% group_by(ticker) %>% mutate(dlclose= lclose-lag(lclose))
head(sdf)

# plot the daily growth rates
sdf %>% filter(!is.na(dlclose)) %>% 
  ggplot(aes(x=date, y=dlclose)) + geom_line(aes(col=ticker)) +
  ggtitle("Daily Growth rates")

# create wide-form data base for daily price growth rates for each ticker (stock or index) price
spg_daily = sdf %>% ungroup() %>% select(-c(close, lclose)) %>% 
  filter(!is.na(dlclose)) %>% spread(key=ticker, value=dlclose)
head(spg_daily)
plot(select(spg_daily,-date)) # scatter plot with correlations
summary(spg_daily)
# spg_daily %>% select(-date) %>% summarise_each(funs(mean,na.rm=TRUE))

# convert to monthly growths (by summing daily growths) and plot
sdf_monthly = sdf %>% 
  mutate(my = floor_date(date, "month")) %>% group_by(ticker, my) %>% 
  summarise(dlclose = sum(dlclose)) %>% ungroup() %>% 
  rename(date=my) %>% filter(!is.na(dlclose))
sdf_monthly %>% filter(!is.na(dlclose)) %>% 
  ggplot(aes(x=date, y=dlclose)) + geom_line(aes(col=ticker)) +
  ggtitle("Monthly Growth Rates")

# Plot a single series for Monthly Growth Rate
sdf_monthly %>% filter(ticker==tckrs[1]) %>% 
  ggplot(aes(x=date, y=dlclose)) + geom_line(aes(col=ticker)) + 
  ggtitle(paste("Monthly Growth Rate:",tckrs[1]))

# create wide-form data base for monthly growth rates of 4 stock indices
spg_monthly = sdf_monthly %>% ungroup() %>% spread(key=ticker, value=dlclose)
head(spg_monthly)
tail(spg_monthly)
plot(select(spg_monthly,-date)) # scatter plot with correlations

# =======================================================

# look for maximum prices
max(get(tckrs[1],envir=stockData)[,paste0(tckrs[1],".Close")])
                                    

# prepare data for back test
# adjust all columns of OHLC (Open, High, Low, Close) object for split and dividend (to get adjusted prices)
for(i in ls(stockData)) stockData[[i]] = adjustOHLC(stockData[[i]], use.Adjusted=T) 
bt.prep(stockData, align='remove.na')  # backtesting prep

ls(stockData)# bt.prep added dates and prices

# 13 WEEK TREASURY BILL (^IRX)
# Chicago Options - Chicago Options Delayed Price. Currency in USD
#   IRX index is annualized yield for the short-term (three month) treasuries.
#   So by adding 1, raising it to the 252nd root, and taking the cumulative product,
#   we can actually get the “price” of the risk-free rate, and from that, 
#   compute daily returns 
getSymbols("^IRX", from="1990-01-01")
daily3MoTYield <- (1+(Cl(IRX)/100))^(1/252) - 1
threeMoTPrice <- cumprod(1+dailyYield)
threeMoTPrice <- threeMoTPrice["1997-03::"]
threeMoTPrice <- threeMoTPrice[endpoints(threeMoPrice, "months"),]
