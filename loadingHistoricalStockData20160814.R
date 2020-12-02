# loadingHistoricalStockData.R

# see; http://www.r-bloggers.com/loading-historical-stock-data/
# June 1, 2013, by systematicinvestor
# Modified by Paul Leiby 6/9/2014, 8/14/2016

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list = ls())

# setInternet2(TRUE)  # Set or disable the use of Windows internal functions for Internet access.
# con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
# source(con)
# close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#tickers = spl('EMC,AAPL,QCOM')
tickers = c("^DJI","^GSPC","^RUT", "^IXIC")
tickerNames = c("DowJones", "S&P500", "Russell2000", "NASDAQ")

stockData <- new.env() # create environment in which xts dataset for each stock symbol will be created

# load historical data, getSymbols from quantmod
getSymbols(Symbols=tickers, src = 'yahoo', from = '1940-01-01', env = stockData, auto.assign = T)    
showSymbols(stockData)  # each member of stockData is an object of class xts (time-series) data
ls(stockData)
tckrs = gsub("\\^","",tickers)  # drop the goofy "^" symbols
tckrs

head(get(tckrs,envir=stockData))
class(stockData)  # class is "environment," members are xts (multiple) time series
class(stockData$DJI)  # members can be accessed like list or dataframe series

# get closing prices for each series
sxts = merge(stockData$DJI$DJI.Close, 
            stockData$GSPC$GSPC.Close, 
            stockData$RUT$RUT.Close, 
            stockData$IXIC$IXIC.Close)  # merging of xts objects by date
names(sxts) = gsub(".Close", "", names(sxts))
plot(sxts)

sdf = data.frame(sxts)
sdf$date = as.Date(row.names(sdf))

head(sdf)
summary(sdf)

sdf = gather(sdf, key=ticker, value=close, DJI:IXIC)

# plot the index series
# ggplot(data=sdf, aes(x=date, y=close)) + geom_line(aes(col=ticker))
sdf %>% filter(!is.na(close)) %>% 
  ggplot(aes(x=date, y=close)) + geom_line(aes(col=ticker))

# create growth rates (log-first-differences)
sdf$lclose = log(sdf$close)
sdf = sdf %>% group_by(ticker) %>% mutate(dlclose= lclose-lag(lclose))

# plot the daily growth rates
sdf %>% filter(!is.na(dlclose)) %>% 
  ggplot(aes(x=date, y=dlclose)) + geom_line(aes(col=ticker))

# create wide-form data base for daily price growth rates for 4 stock indices
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

sdf_monthly %>% filter(ticker=="GSPC") %>% 
  ggplot(aes(x=date, y=dlclose)) + geom_line(aes(col=ticker)) + 
  ggtitle(paste("Monthly Growth Rate:","GSPC"))

# create wide-form data base for monthly growth rates of 4 stock indices
spg_monthly = sdf_monthly %>% ungroup() %>% spread(key=ticker, value=dlclose)
head(spg_monthly)
tail(spg_monthly)
plot(select(spg_monthly,-date)) # scatter plot with correlations

# =======================================================

# look for maximum prices
stockData$DJI[which.max(stockData$DJI$DJI.Close),]
max(get(tckrs[1],envir=stockData)[,paste0(tckrs[1],".Close")])
                                    

# prepare data for back test
# adjust all columns of OHLC (Open, High, Low, Close) object for split and dividend (to get adjusted prices)
for(i in ls(stockData)) stockData[[i]] = adjustOHLC(stockData[[i]], use.Adjusted=T) 
bt.prep(stockData, align='remove.na')  # backtesting prep

ls(stockData)# bt.prep added dates and prices
summary(stockData$prices)
head(index(stockData$prices))
head(time(stockData$prices))

#*****************************************************************
# Code Strategies
#******************************************************************
obj = portfolio.allocation.helper(stockData$prices, periodicity = 'months', lookback.len = 250, 
                                  min.risk.fns = list(EW=equal.weight.portfolio,
                                                      RP=risk.parity.portfolio,
                                                      MV=min.var.portfolio,
                                                      MC=min.corr.portfolio)
) 

models = create.strategies(obj, stockData)$models

#*****************************************************************
# Create Report
#******************************************************************        
strategy.performance.snapshoot(models, T)



#*****************************************************************
# Following are 3 examples to load historical prices from files saved on your computer:
#   
#   1. Load Historical Stock data from the csv files you saved from Yahoo Fiance
#   2. Load Historical Stock data from your custom files
#   3. Load Historical Stock from one csv file, where each column represents one stock
#   # Just substitute any of the samples below for following line of code in my original back-test example:
#   # load historical data, getSymbols from quantmod
#   getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)    

# For the first method I created a getSymbols.sit function that loads historical stock prices from csv files you saved from Yahoo Fiance. Here is an example:
stock.folder = 'c:\\Stocks\\Data\\'
# if you saved yahoo historical price files localy
getSymbols.sit(tickers, src = 'yahoo', from = '1980-01-01', env = stockData, auto.assign = T, stock.folder = stock.folder)

# For the second method, please load your historical price files with read.xts function. You will need to make sure that that one of the column headers is labeled "Close". Here is an example:
stock.folder = 'c:\\Stocks\\Data\\'
# custom format historical price files
for(n in tickers) {
  stockData[[n]] = read.xts(paste(stock.folder, n, '.csv', sep=''), format='%m/%d/%Y')
} 

# For the third method, please load your historical prices from one file. (i.e. each column is one stock) Here is an example:
stock.folder = 'c:\\Stocks\\Data\\'
# read from one csv file, column headers are tickers
filename = 'histdata.csv'
all.stockData = read.xts(paste(stock.folder, filename, sep=''), format='%m/%d/%Y')
for(n in names(all.stockData)) {
  stockData[[n]] = all.stockData[,n]
  colnames(stockData[[n]]) = 'Close'
  stockData[[n]]$Adjusted = stockData[[n]]$Open = stockData[[n]]$High = stockData[[n]]$Low = stockData[[n]]$Close
}
