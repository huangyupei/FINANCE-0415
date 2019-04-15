rm(list = ls())
install.packages("quantmod")
library(quantmod)
tickers<-c("1101.TW","1216.TW","1301.TW","1402.TW","2002.TW","2105.TW","2912.TW",
           "9904.TW","6505.TW","2303.TW","2330.TW","2408.TW","2357.TW","2412.TW",
           "3008.TW","3481.TW","2327.TW","2308.TW","2317.TW","2474.TW")
getSymbols(tickers,from='2014-01-01',to='2018-12-31',auto.assign=T)
getSymbols(tickers,from='2014-01-01',to='2018-12-31',tw20_adj_close=new.env(),auto.assign=T)
names(tw20_adj_close)
tw20_adj_close$`1101.TW`

#1
ifelse(!require(quantmod),install.packages('quantmod'),library(quantmod))
tw20_adj_close<-read.csv("2018Q4_20.csv")

#2
ifelse(!require(readr),install.packages('readr'),library(readr))
tw20.txt<-read.table("tw20.txt",header = T)

#3
ifelse(!require(reshape2),install.packages('reshape2'),library(reshape2))
colnames(tw20.txt)<-c("id","", "date", "price")
tw20.xts=dcast(tw20.txt,date~id)

tw20.xts$date<-as.Date(as.character(tw20.xts$date), "%Y%m%d") 
library(xts)
tw20.xts<-xts(tw20.xts[,-1], order.by = tw20.xts$date)
saveRDS(tw20.xts, "tw20_xts_all")
head(tw20.xts)
tail(tw20.xts)
str(tw20.xts)

#4
library(quantmod)
tw20.mon.ret <- to.monthly(tw20.xts, indexAt = "lastof", OHLC=FALSE)
head( tw20.mon.ret)

library(PerformanceAnalytics)
library(magrittr)
tw20.day.ret<-Return.calculate(tw20.xts, method = "log")
head(tw20.day.ret)

#6
#1101
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
load.packages('quantmod')
bt.simple <- function(data, signal)
{
  # lag serial
  signal <- lag(signal,1)
  # back fill
  signal <- na.locf(signal, na.rm = FALSE)
  signal[is.na(signal)] = 0
  # calculate close-to-close returns
  # ROC() : Calculate the (rate of) change of a series over n periods.
  ret <- ROC(Cl(data), type="discrete")
  ret[1] = 0
  # compute stats
  bt <- list()
  bt$ret <- ret * signal 
  bt$equity <- cumprod(1 + bt$ret)
  return(bt)}

tw20.all<-readRDS("c:/test0415/TEST0415/tw20_xts_all")
head(tw20.all)
str(tw20.all)
tw20.all.1<-tw20.all[complete.cases(tw20.all),]
head(tw20.all.1)
tail(tw20.all.1)

data1<-new.env()
data1$prices<-tw20.all.1$`1101`
prices<-data1$prices
prices

sma90<-SMA(prices, 90)
head(sma90, 91)

# buy and hold for 1101
bt.prep(data1, align='keep.all')
names(data1)
data1$dates
data1$prices
data1$prices<-prices

class(data1$dates)
data1$weight
data1$execution.price = prices

data1$weight[] = 1
buy.hold.1101 <- bt.run.share(data1, clean.signal=F, trade.summary = TRUE)
buy.hold.1101 <-bt.run(data1)
# sma 90 for 1101
sma90<-SMA(prices, 90)
head(sma90, 91)
data1$weight[] <- iif(prices >= sma90, 1, 0)
sma90.1101 <- bt.run(data1, trade.summary=T)
# sma 50 for 0056, short allowed
data1$weight[] <- iif(prices >= sma90, 1, -1)
sma90.1101.short <- bt.run(data1, trade.summary=T)
# summary of investment
models<-list("SMA90"= sma90.1101, 
             "SMA90_short" = sma90.1101.short, 
             "BH 1101" = buy.hold.1101)
strategy.performance.snapshoot(models, T)
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=T)
plotbt.strategy.sidebyside(models, return.table=T)
#
library(ggplot2)
all.1101<-merge.xts(sma90.1101$equity, 
                    sma90.1101.short$equity, 
                    buy.hold.1101$equity)
colnames(all.1101)<-c("sma90", "sma90 short", "BH")
head(all.1101)
all.1101.long<-fortify(all.1101, melt=T)
head(all.1101.long)
#
title = "Cumulative returns of 1101s"
p = ggplot(all.1101.long, aes(x = Index, y = Value)) +
  geom_line(aes(linetype = Series, color = Series)) +
  #geom_point(aes(shape = Series))+
  xlab("year") + ylab("cumulative returns")+
  ggtitle(title)
p

#1216
data2<-new.env()
data2$prices<-tw20.all.1$`1216`
prices<-data2$prices
prices

sma90<-SMA(prices, 90)
head(sma90, 91)

# buy and hold for 1216
bt.prep(data2, align='keep.all')
names(data2)
data2$dates
data2$prices
data2$prices<-prices

class(data2$dates)
data2$weight
data2$execution.price = prices

data2$weight[] = 1
buy.hold.1216 <- bt.run.share(data2, clean.signal=F, trade.summary = TRUE)
buy.hold.1216 <-bt.run(data2)
# sma 90 for 1216
sma90<-SMA(prices, 90)
head(sma90, 91)
data2$weight[] <- iif(prices >= sma90, 1, 0)
sma90.1216 <- bt.run(data2, trade.summary=T)
# sma 90 for 1216, short allowed
data2$weight[] <- iif(prices >= sma90, 1, -1)
sma90.1216.short <- bt.run(data2, trade.summary=T)
# summary of investment
models<-list("SMA90"= sma90.1216, 
             "SMA90_short" = sma90.1216.short, 
             "BH 1216" = buy.hold.1216)
strategy.performance.snapshoot(models, T)
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=T)
plotbt.strategy.sidebyside(models, return.table=T)
#
library(ggplot2)
all.1216<-merge.xts(sma90.1216$equity, 
                    sma90.1216.short$equity, 
                    buy.hold.1216$equity)
colnames(all.1216)<-c("sma90", "sma90 short", "BH")
head(all.1216)
all.1216.long<-fortify(all.1216, melt=T)
head(all.1216.long)
#
title = "Cumulative returns of 1216s"
p = ggplot(all.1216.long, aes(x = Index, y = Value)) +
  geom_line(aes(linetype = Series, color = Series)) +
  #geom_point(aes(shape = Series))+
  xlab("year") + ylab("cumulative returns")+
  ggtitle(title)
p
#1301
data3<-new.env()
data3$prices<-tw20.all.1$`1301`
prices<-data3$prices
prices

sma90<-SMA(prices, 90)
head(sma90, 91)

# buy and hold for 1301
bt.prep(data3, align='keep.all')
names(data3)
data3$dates
data3$prices
data3$prices<-prices

class(data3$dates)
data3$weight
data3$execution.price = prices

data3$weight[] = 1
buy.hold.1301 <- bt.run.share(data3, clean.signal=F, trade.summary = TRUE)
buy.hold.1301 <-bt.run(data3)
# sma 90 for 1301
sma90<-SMA(prices, 90)
head(sma90, 91)
data3$weight[] <- iif(prices >= sma90, 1, 0)
sma90.1301 <- bt.run(data3, trade.summary=T)
# sma 90 for 1301, short allowed
data3$weight[] <- iif(prices >= sma90, 1, -1)
sma90.1301.short <- bt.run(data3, trade.summary=T)
# summary of investment
models<-list("SMA90"= sma90.1301, 
             "SMA90_short" = sma90.1301.short, 
             "BH 1301" = buy.hold.1301)
strategy.performance.snapshoot(models, T)
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=T)
plotbt.strategy.sidebyside(models, return.table=T)
#
library(ggplot2)
all.1301<-merge.xts(sma90.1301$equity, 
                    sma90.1301.short$equity, 
                    buy.hold.1301$equity)
colnames(all.1301)<-c("sma90", "sma90 short", "BH")
head(all.1301)
all.1301.long<-fortify(all.1301, melt=T)
head(all.1301.long)
#
title = "Cumulative returns of 1301s"
p = ggplot(all.1301.long, aes(x = Index, y = Value)) +
  geom_line(aes(linetype = Series, color = Series)) +
  #geom_point(aes(shape = Series))+
  xlab("year") + ylab("cumulative returns")+
  ggtitle(title)
p
