rm(list=ls(all=TRUE))
library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)

df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
genlastdateinmonth<-function(df){
  df$lastdate<-last(df$date)
  dfout<-subset(df,date==lastdate)
  return(dfout)
}

pickonlylastdateofmonth<-function(df_in){
  df_in$yrmo<-factor(format(df_in$date,'%Y%m')) #must be factor to use by() later
  df<-do.call(rbind,by(df_in,df_in$yrmo,genlastdateinmonth))
  df$lastdate<-NULL
  df
}

setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")




# load('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices3.RData')
# # extract single
# ticker_extract<-'EUSA2'
# i<-index(tickerraw)[tickerraw$Ticker==str_c(ticker_extract,' Curncy')]
# print(i)
# dfout<-price[[i]]
# colnames(dfout)<-c('datestr',str_to_lower(ticker_extract))
# write.dta(dfout,str_c('bbg_',str_to_lower(ticker_extract),'.dta'))

# extract multiple
extractbbg<-function(filein){
  load(filein)
  i=1
  dfout<-price[[i]]
  colnames(dfout)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
  
  for (i in 2:nrow(tickerraw)){
    dfnew<-price[[i]]
    colnames(dfnew)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
    dfout=merge(dfout, dfnew, all=TRUE)
  }
  dfout
}

#### Monthly
dfout1<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices_151005.RData')
dfout2<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices2.RData')
dfout3<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices3.RData')
dfout4<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices4.RData')
dfout4<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices4.RData')
dfout5<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices_monthlyCM_160204.RData')

dfout5<-dfout5 %>% 
  mutate(date=as.Date(datestr)) %>% 
  pickonlylastdateofmonth() %>%
  mutate(date=NULL,yrmo=NULL)

dfout<-dfout1
dfout<-merge(dfout,dfout2,all=TRUE)
dfout<-merge(dfout,dfout3,all=TRUE)
dfout<-merge(dfout,dfout4,all=TRUE)
dfout<-merge(dfout,dfout5,all=TRUE)
write.dta(dfout,str_c('bbg_prices.dta'))

### Daily
dfout1<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgpricesall151207_daily.RData')
dfout2<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices_151210_daily_emccy.RData')
dfout3<-extractbbg('/Users/gliao/Dropbox/Research/ccy basis/data/bloomberg/bbgprices_additional_daily_151210.RData')
dfout<-dfout1
dfout<-merge(dfout,dfout2,all=TRUE,suffixes=c("",".z"))
dfout<-merge(dfout,dfout3,all=TRUE,by="datestr",suffixes=c("",".z"))
write.dta(dfout,str_c('bbg_prices_daily.dta'))
