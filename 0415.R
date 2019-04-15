rm(list = ls())
install.packages("quantmod")
library(quantmod)
tickers<-c("1101.TW","1216.TW","1301.TW","1402.TW","2002.TW","2105.TW","2912.TW",
           "9904.TW","6505.TW","2303.TW","2330.TW","2408.TW","2357.TW","2412.TW",
           "3008.TW","3481.TW","2327.TW","2308.TW","2317.TW","2474.TW")
getSymbols(tickers,from='2014-01-01',to='2018-12-31',auto.assign=T)
tw20_adj_close=new.env()
getSymbols(tickers,from='2014-01-01',to='2018-12-31',auto.assign=T)
names(tw20_adj_close)

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

tw20.xts<-xts(tw20.xts[,-1], order.by = tw20.xts$date)
head(tw20.xts)
tail(tw20.xts)
str(tw20.xts)

#4
library(xts)
library(quantmod)
tw20.mon.ret <- to.monthly(tw20.xts, indexAt = "lastof", OHLC=FALSE)
head( tw20.mon.ret)

library(PerformanceAnalytics)
library(magrittr)
tw20.day.ret<-Return.calculate(tw20.xts, method = "log")
head(tw20.day.ret)

#5
