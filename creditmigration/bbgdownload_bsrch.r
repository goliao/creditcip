
rm(list=ls())
setwd("J:/data")
setwd("E:/data")

require(Rblpapi)
require(lubridate)
# install.packages(c('lubridate'))

startdate<-ymd('2005-01-01')
load('bbgdownload_042616.rdata')


# batch 1 monthly for IGs previously not yet downloaded -------------------
field<-c("OAS_SPREAD_BID","YLD_YTM_MID")
opt <- c("periodicitySelection"="MONTHLY")
tickers<-split(as.character(bdownload_batch1_mo$parsekeyable),bdownload_batch1_mo$fac)
con <- Rblpapi::blpConnect()
prices<-list()
for (i in 1:length(tickers)) {
  ptm<-proc.time()
  prices[[i]]<-bdh(tickers[[i]], field, start.date=startdate, options=opt)  
  save.image('bbg_gbonds_160426_temp.RData')
  print(paste('i:',i,'time:',(proc.time() - ptm)[[3]],'length:',length(tickers[[1]])))
  flush.console()
}
save.image('bbg_gbonds_160426_mo_batch1.RData')
blpDisconnect(con)

# batch 2 daily download for all IG ---------------------------------------
load('bbg_gbonds_160426_temp_daily.RData')

field<-c("OAS_SPREAD_BID","YLD_YTM_MID")
opt <- c("periodicitySelection"="DAILY")
startdate<-ymd('2004-01-01')
tickers<-split(as.character(bdownload_batch2_daily$parsekeyable),bdownload_batch2_daily$fac)
con <- Rblpapi::blpConnect()
for (i in 9:length(tickers)) {
  ptm<-proc.time()
  prices[[i]]<-bdh(tickers[[i]], field, start.date=startdate, options=opt)  
  save.image('bbg_gbonds_160426_temp_daily.RData')
  print(paste('i:',i,'time:',(proc.time() - ptm)[[3]],'length:',length(tickers[[1]])))
  flush.console()
}
save.image('bbg_gbonds_160426_daily_batch2.RData')
blpDisconnect(con)


# Consider other fields in monthly data: ASW ---------------------------------------------------
rm(list=ls())
setwd("E:/data")
require(Rblpapi)
require(lubridate)
startdate<-ymd('2004-01-01')
load('bbgdownload_042616.rdata')
field<-c("ASW")
opt <- c("periodicitySelection"="MONTHLY")
tickers<-split(as.character(bdownload_batch2_daily$parsekeyable),bdownload_batch2_daily$fac)
con <- Rblpapi::blpConnect()
prices<-list()
for (i in 1:length(tickers)) {
  ptm<-proc.time()
  prices[[i]]<-bdh(tickers[[i]], field, start.date=startdate, options=opt)  
  save.image('bbg_gbonds_160426_aswtemp.RData')
  print(paste('i:',i,'time:',(proc.time() - ptm)[[3]],'length:',length(tickers[[1]])))
  flush.console()
}
save.image('bbg_gbonds_160426_mo_batch2_asw.RData')
blpDisconnect(con)