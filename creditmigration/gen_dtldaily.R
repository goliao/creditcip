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
recheck.daily<-unique(temp$dtconflicts[abs(diff_pk)>.1,.(pk)])

# load new issuance data from april to july 31, 2016
load(file='dtl160731newiss.RData')
dtltemp<-update.dtl.mo(dtl.daily,dtladd.daily,overridein = T,diagret = T)
dtl.daily<-dtltemp[[1]]
recheck.daily<-rbind(recheck.daily,unique(temp$dtconflicts[abs(diff_pk)>.1,.(pk)]))
#save(dtl.daily,recheck.daily,file='db/archieve/dtldaily160731.RData')


######
#### what's the equivalent to dtlmoadd20160731???? ######
### probably need to redownload at some point
############

load('db/archieve/dtldaily160731.RData')

#loading new data from 160803
	load('dtl160803_yr04-06add.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd1.daily,overridein = T)
	load('dtl160803_1-2yrallccynewadd.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd2.daily,overridein=T)
	load('dtl160803_otherccy.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd3.daily,overridein=T)
	load('dtl160803_completesdc.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd4.daily,overridein=T)
	load('dtl160803_recheck1add.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,recheck1.daily,overridein=T)
	load('dtl160804_completesdc.RData')
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd5.daily,overridein=T)
	#save(dtl.daily,file='db/dtldaily.RData')

load('db/dtldaily.RData')

### Diagnostics that's used to get rid of dates with few observations
# merging with bondref
	load('db/bondref.RData')
	br<-bondref[!is.na(pk),.(ccy,mat2,nrating,upcusip,pk,ytofm,sicfac,sic1)]
	br[,ccy:=tolower(ccy)]
	compare.dt(dtl.daily,br,bykey.='pk')
	setkey(dtl.daily,pk,date)
	setkey(br,pk)
	dtl<-dtl.daily[br]

# getting rid of holidays for eur and usd
	load('db/holidaycalendar.RData')
	holidays<-data.table('date'=unique(c(cdrus,cdreu)))
	setkey(holidays,date)
	compare.dt(dtl.daily,holidays,bykey.='date')


# Looks terribly choppy in observations for USD
	dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy) %>% ggplotw()


	dtl %>% setkey(date,pk)
# much better with holidays taken out 
	dtl<-dtl[!holidays][date<'2016-07-26']
	dtl[ccy %in% c('aud','jpy','gbp')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy) %>% ggplotw()
	dtl.count<-(dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy))[order(date)]  
# even better after taking out sharp drops in dates
	dtl.count[,usd.L:=lag(usd)]
	dtl.count[usd<.90*usd.L,sharpobsdrop:=1][is.na(sharpobsdrop),sharpobsdrop:=0]
	dtl.count[sharpobsdrop==1] %>% View
	dtl.count<-dtl.count[sharpobsdrop==0]
	dtl.count[date %between% c('2014-09-01','2015-06-01'),.(date,usd,eur)] %>% ggplotw()
	dtl.count[date %between% c('2014-09-01','2015-06-01'),.(date,usd,eur)] %>% View

# need to cut off at 7/26 for now. 
