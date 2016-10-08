
#
# Process the S&P Index list (which includes the S&P 500 and other S&P indices) to build a file
# of S&P Index stocks for a particular set of quarters.
# 
# Processing the Compustat S&P 500 data involves finding the shares that make up the index
# for a particular year. To do this the Compustat North America Index Constituents file is read 
# from the starting data.  Each year is built up of stocks that have no end in the index and stocks
# whose end date is beyond that year.
# 
# This result is stored in a file.
# 
# In order to support reading the data from the Compustat corporate factors, the GEVKEY Compustat
# key code is output.
#
# Under the conm column:
#
#   S&P 500 Comp-Ltd         
#   S&P 500/Barra Growth Index
#   S&P 500/Barra Value Index
#   S&P 500 Growth          
#   S&P 500 Value
# 
# There also appears to be sections and groups. For example:
#   
# [1] "S&P 500 Comp-Ltd"                                       "S&P 500/Barra Growth Index"                            
# [3] "S&P 500/Barra Value Index"                              "SP500 Energy .S"                                       
# [5] "SP500 Materials .S"                                     "SP500 Industrials .S"                                  
# [7] "SP500 Consumr Discretion .S"                            "SP500 Consumer Staples .S"                             
# [9] "SP500 Health Care .S"                                   "SP500 Financials .S"                                   
# [11] "SP500 Information Tech .S"                              "SP500 Telecom Services .S"                             
# [13] "SP500 Utilities .S"                                     "SP500 Energy .G"                                       
# [15] "SP500 Materials .G"                                     "SP500 Capital Goods .G"                                
# [17] "SP500 CMMRCL&PRFSSNL SVC.G"                             "SP500 Transportation .G"                               
# [19] "SP500 Auto & Components .G"                             "SP500 Cnsmr Durbl&Apprel .G
# 

library(tseries)
library(zoo)
library(gridExtra)

printTable = function( tab )
{
  grid.newpage()
  par(mfrow=c(1,1))
  grid.table(round(tab, 6), gpar.rowtext=gpar(fontsize = 16), show.box=T, 
             show.hlines=T, show.vlines=T, separator = "black")
  par(mfrow=c(1,1))
}

pathRoot = "/Users/gliao/Documents/sec"
#
# INPUT Files
#
# This is the file of compustat data
indexInfo = "wrdsindex.csv"

#
# OUTPUT Files
#
# All Filtered S&P 500 values, by year
allSP500 = "sp500ByQtr_all.csv"
# S&P 500 GVKEY index values for WRDS Compustat lookup
sp500GEVKEY = "sp500GEVKEY.txt"

sp500CUSIP = "sp500CUSIP.txt"

sp500TICKER = "sp500TICKER.txt"

path = paste(pathRoot, indexInfo, sep="/")
allSP500Path = paste(pathRoot, allSP500, sep="/")
gevkeyPath = paste(pathRoot, sp500GEVKEY, sep="/")
cusipPath = paste(pathRoot, sp500CUSIP, sep="/")
tickerPath = paste(pathRoot, sp500TICKER, sep="/")

TICKER = "co_tic"
FROM = "from"
TO = "thru"
NAME = "co_conm"
KEY = "gvkey"
CUSIP = "co_cusip"

startDate = as.Date("2004-01-01")
endDate = as.Date("2016-09-30")
quarters = as.Date(seq(from=startDate, to=endDate, by="quarter"))
# Quarters should be year-03-31, year-06-30, year-09-30, year-12-31
# We have:           year-03-30, year-06-30, year-09-30, year-12-30
# So the May and December quarterly dates need to be fixed
qtrs = format(quarters, format="%m-%d")
years = format(quarters, format="%Y")
qtrs[qtrs == "03-30"] = "03-31"
qtrs[qtrs == "12-30"] = "12-31"
quarters = as.Date(paste(years, qtrs, sep="-"))

wrdsind<-fread('wrdsindex.csv',sep=',')
sp500<-wrdsind[conm=='S&P 500 Comp-Ltd']
sp500[,from.date:=ymd(from)]
sp500[,thru.date:=ymd(thru)]

sp500Qtr = data.table()
for (i in 1:length(quarters)) {
  qtr = quarters[i]
  startBlock = sp500[from.date <= qtr,]
  startToDates = as.Date(strptime(as.vector(startBlock[,TO]), format="%m/%d/%Y", tz="EST"))
  startEndIx = sort(c(which( is.na(startToDates) ), which(startToDates >= qtr)))
  qtrSAndP = startBlock[startEndIx, c(KEY, CUSIP, NAME, TICKER)]
  qtrVec = rep(qtr, times = nrow(qtrSAndP))
  qtrSAndP = data.frame(qtrVec, qtrSAndP)
  colnames(qtrSAndP) = c("qtr", KEY, CUSIP, NAME, TICKER)
  sp500Qtr = rbind(sp500Qtr, qtrSAndP)
}

# Check that all the row lengths are 500 for each quarter
lengths = rep(0, length(quarters))
for (i in 1:length(quarters)) {
  qtr = quarters[i]
  block = sp500Qtr[sp500Qtr[,"qtr"] == qtr,]
  lengths[i] = nrow(block)
}


sp500q<-sp500Qtr %>% as.data.table()
sp500q %>% summary

# Check against the Wikipedia list. Unfortunately this is as of Nov. 2013, so it's not the
# same as the compustat list.
#
sp500_2013 = read.csv(paste(pathRoot, "sp500_2013.csv", sep="/"))
sp500_2013.v = as.vector(sp500_2013[,"tic"])
q2013 = as.Date("2013-09-30")
syms = as.vector(sp500Qtr[sp500Qtr[,"qtr"] == q2013,TICKER])
same = syms %in% sp500_2013.v

write.csv(sp500Qtr, file=allSP500Path, row.names=F)

# get all of the GVKEY values
gevkey = as.matrix(unique(as.vector(sp500Qtr[,KEY])))
write(gevkey, file=gevkeyPath,ncolumns=1)

cusip = as.matrix(unique(as.vector(sp500Qtr[,CUSIP])))
# What's strange is that the co_cusip, which is what is in this data, has an extra
# low order digit compared to the CUSIP. So remvoe this digit.
cusipLen = nchar(cusip)
cusipTrunc = substr(cusip, start=1, stop=(cusipLen-1))
write(cusipTrunc, file=cusipPath,ncolumns=1)

tickers = as.matrix(unique(as.vector(sp500Qtr[,TICKER])))
write(tickers, file=tickerPath,ncolumns=1)