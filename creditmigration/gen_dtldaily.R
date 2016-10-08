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


# 16/9/7 add new bond downloads from Sept 4th using figi derived bbg ticker, to complete SDC dataset
	rm(list=ls(all=TRUE))
	setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	source('util.r')
	load('db/archive/dtldaily160806.RData')	
	dtladd.daily<-loadBBGdownload2df('dbin/sdcticker_wossa_daily_160904.RData')	
	dtladd2.daily<-loadBBGdownload2df('dbin/sdcticker_ssa_daily_160904.RData')	
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd.daily)
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd2.daily)
	dtl.daily %>% setkey(pk,date)
	dtl.daily<-unique(dtl.daily)
	#save(dtl.daily,file='db/dtldaily.RData')	

# 16/9/7 replace all pk with tickers 
	load('db/dtlmo.rdata')
	load('db/pktickerlookup.RData')		
	dtl.mo %>% setkey(pk)
	pk.ticker.lookup %>% setkey(pk)
		# check which ones are just not there from bondref/sdc; these can be enhanced if I can get BBG information: ccy, mat, rating, firm
		# load('db/bondrefall.RData')
		# dtl.mo[,.N,.(pk)]
		# missing<-dtl.mo[!pk.ticker.lookup][,.N,.(pk)]
		# missing %>% setkey(pk)
		# bondrefall %>% setkey(pk)
		# bondrefall[missing]
		# bondrefall[missing]
	dtl.mo[pk.ticker.lookup,pk:=ticker]
	dtl.mo %>% setkey(pk,date)
	dtl.mo<-unique(dtl.mo)
	
	# dtl.mo[,.N,pk] %>% View
	#save(dtl.mo,recheck,file='db/dtlmo.rdata')	

# 16/09/27 update daily time series
	rm(list=ls(all=TRUE));setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration"); source('util.r')
	load('db/archive/dtldaily160907.RData')	
	dtladd.daily<-loadBBGdownload2df('dbin/daily_cspp_up_160927.RData')	
	dtladd.daily2<-loadBBGdownload2df('dbin/daily_update_globalonly_160927.RData')	
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd.daily,overridein = T)
	dtl.daily<-update.dtl.mo(dtl.daily,dtladd.daily2,overridein = T)
	dtl.daily %>% setkey(pk,date)
	dtl.daily<-unique(dtl.daily)
	#save(dtl.daily,file='db/dtldaily.RData')	


### Diagnostics that's used to get rid of dates with few observations
# merging with bondref
	load('db/archive/bondref160806.RData')
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
