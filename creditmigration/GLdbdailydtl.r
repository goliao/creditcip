## Construct daily dtl bond data file
# for only bond yield
rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
#setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
load('dailyprices.rdata')

dtl.daily<-df_p_daily %>% as.data.table()
setnames(dtl.daily,c('YLD_YTM_MID','ticker'),c('value','pk'))
dtladd<-loadBBGdownload2df('bbg_dailybondsadd_160624.RData')
dtl.daily<-update.dtl.mo(dtl.daily,dtladd)

# add daily bond data:
dtladd<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_daily_batch2.RData')
dtl.daily<-update.dtl.mo(dtl.daily,dtladd)

load('../data/bloomberg/dtchfdaily.RData')
dtladd<-dtchfdaily
dtl.daily<-update.dtl.mo(dtl.daily,dtladd)

dtladd<-loadBBGdownload2df('dtl160628GBP.RData')
dtl.daily<-update.dtl.mo(dtl.daily,dtladd)

load('dtl160726_daily_addition.RData')
dtladd<-dtladd.daily
temp<-update.dtl.mo(dtl.daily,dtladd,overridein=T,diagret=T)
dtl.daily<-temp$dtout
recheck.daily<-temp$dtret[[1]][,.N,pk][,.(pk)]

#save(dtl.daily,recheck.daily,file='dtldaily.RData')


# load('dtldaily.RData')
# load('bondref.RData')
# br<-bondref[!is.na(pk),.(ccy,mat2,nrating,upcusip,pk,ytofm,sicfac,sic1)]
# setkey(dtl.daily,pk,date)
# setkey(br,pk)

# cdrus<-RQuantLib::getHolidayList('UnitedStates',from=ymd('2004-01-01'),to=ymd('2016-07-01'))
# cdreu<-RQuantLib::getHolidayList('Germany',from=ymd('2004-01-01'),to=ymd('2016-07-01'))
# holidays<-data.table('date'=unique(c(cdrus,cdreu)))
# setkey(holidays,date)

# dtl<-dtl.daily[br,nomatch=0]
# dtl
# dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy) %>% ggplotw()
# dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy)  %>% View