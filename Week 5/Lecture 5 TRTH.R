# Agenda: 
# 1. Package quantmod
# 2. Intro to Bloomberg API
# 3. Intro Thomson Reuters Tick History
# 4. TRTH R API


# =============== quantmod ================

rm(list = ls())

library(quantmod)

??getSymbols

getSymbols(Symbols = "MSFT", src = "yahoo")
MSFT <- data.frame(MSFT)
head(MSFT)

# set time interval
getSymbols(Symbols = "INTC", from = "2001-01-01", to = "2001-12-31")
INTC <- data.frame(INTC)
head(INTC)    
tail(INTC)


# assign data to a custom name
getSymbols("AAPL", auto.assign = F)
aapl <- getSymbols("AAPL", auto.assign = F)


# by default 
# src = "yahoo"
# from = "2007-01-01"
# to = today
# auto.assign = T


chartSeries(MSFT)
chartSeries(MSFT,theme=chartTheme('white'))
chartSeries(MSFT,TA=NULL)   #no volume
chartSeries(MSFT,TA=c(addBBands()))  #add volume and Bollinger Bands from TTR
chartSeries(MSFT,TA=c(addVo()))

# download option data with quantmod
getOptionChain("AAPL")

aapl_opt <- getOptionChain("AAPL")
names(aapl_opt) 
# [1] "calls" "puts"
is.data.frame(aapl_opt)
is.data.frame(aapl_opt$calls)

head(aapl_opt$calls)

# ======================== Bloomberg API =============================

# Prerequisite for Rbbg:
#     
# -latest Java
# -R version consistent with Java version (32 bit or 64 bit)
# -latest R package rJava
# -package Rbbg
# -Bloomberg APIv3

# sample 1
conn <- blpConnect()
bdp(conn, "AMZN US Equity", "NAME")

# sample 2
securities <- c("AMZN US Equity", "OCN US Equity")
fields <- c("NAME", "PX_LAST", "TIME", "SETTLE_DT", "HAS_CONVERTIBLES") # Demo different return data types.
bdp(conn, securities, fields)




# =============== Thomson Reuters Tick History ================

# a. Google "trth"
# b. Two manuals uploaded to Canvas


# ======================== TRTH API ===========================
# prerequisite packages:
# xml
# base64
# Rcurl


library(THAPI)
rdth <- createCredential(user = "", password = "")
??rdth




# ======================= RIC ================================

# look up:  http://www.reuters.com/finance/stocks/lookup
# GOOG.O  Google  Nasdaq
# AAPL.O  Apple   Nasdaq
# T.N     AT&T    NYSE
# IBM.N   IBM     NYSE
# ...




# RIC on API

# get functions

# domain
getAssetDomains(rdth)

# result:
# > getAssetDomains(rdth)
# value               longName
# 1    BON           Fixed Income
# 2    COM Commodities and Energy
# 3    ECI    Economic Indicators
# 4    EQU               Equities
# 5    FOR       Foreign Exchange
# 6    FUN                  Funds
# 7    FUT                Futures
# 8    IND                Indices
# 9    MON           Money Market
# 10   OPT                Options


# exchange
getExchanges(rdth, domains="EQU")




# searchRIC
?searchRICs

searchRICs(rdth, "2013-11-11", "2013-11-12", "EQU", "ASX", "^BHP.*")

searchRICs(rdth, "2013-11-11", "2013-11-12", "EQU", "NYS", "IBM.*")
searchRICs(rdth, "2013-11-11", "2013-11-12", "EQU", "NYS", "IB.*")
searchRICs(rdth, "2013-11-11", "2013-11-12", "EQU", "NYS", "^IB.*")

# a preferred way...
# http://www.reuters.com/finance/stocks/lookup

# ======================= submit request ================================

submitRequest(...)      # request data for 1 day
submitFTPRequest(...)   # request data for multiple days


?submitFTPRequest


# to download tick data
submitFTPRequest(r=rdth, 
                 friendlyname="test", 
                 instrumentList="GOOG.O", 
                 startdate="2011-11-08", enddate="2011-12-12", 
                 starttime="00:00:00", endtime="23:59:59.999", 
                 reqtype="TimeAndSales", 
                 mktdepth="0", 
                 messagetypelist="Trade:Price,Volume;Quote:Bid Price,Ask Price", 
                 reqInGMT="false", disInGMT="false")


# "Your request id is: zzhao6@stevens.edu-test-N82868653"


# request type and message type list

?submitFTPRequest

# Request Type:

?getMessageTypes

# "TimeAndSales"
# "MarketDepth"
# "NasdaqLevel2"
# "Intraday"
# "EndOfDay"
# "CorporateActions"
# "Any"

# message type list

getMessageTypes(rdth, c("EQU"), "TimeAndSales")

# Message Types -->> Field List

getMessageTypes(rdth, c("EQU"), "Intraday")


# format of messageTypeList:
# messagetypelist="reqtype1:messagetype1,messagetype2;
#                  reqtype2:messagetype1,messagetype2;
#                  ..."



# to download minute data
submitFTPRequest (rdth, "test", instrumentList ="GOOG.O", 
                  "2011-11-08", "2011-11-12 ", "00:00:00", "23:59:59.999", 
                  reqtype="Intraday", 
                  mktdepth="0",
                  messagetypelist ="Intraday 1Min: Open, High, Low, Last, Volume",
                  reqInGMT="false", disInGMT="false")

submitFTPRequest (rdth , "test", instrumentList ="AAPL .O", "2011-11-08", 
                  "2013-11-12 ", "00:00:00", "23:59:59.999", "Intraday", 
                  mktdepth ="0",
                  messagetypelist="Intraday 1Sec: Open, High, Low, Last ,Volume",
                  reqInGMT="false", disInGMT="false")





# get request result
getRequestResult(rdth, "zzhao6@stevens.edu-test-N82868653")



# where to find the data:
# https://tickhistory.thomsonreuters.com/HttpPull/





getInFlightStatus(rdth)

submitFTPRequest(r=rdth, 
                 friendlyname="test", 
                 instrumentList="GOOG.O", 
                 startdate="2010-11-08", enddate="2013-11-12", 
                 starttime="00:00:00", endtime="23:59:59.999", 
                 reqtype="TimeAndSales", 
                 mktdepth="0", 
                 messagetypelist="Trade:Price,Volume;Quote:Bid Price,Ask Price", 
                 reqInGMT="false", disInGMT="false")


getInFlightStatus(rdth)




?cancelRequest
# This operations cancels and removes the results file for a request. 
# This can be either a small API request or an FTP request.

cancelRequest(rdth, friendlyname ="zzhao6@stevens.edu-test-N64418915")
getInFlightStatus(rdth)



