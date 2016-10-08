
# script used to genertes daily firm-level credit spread
setwd('/mnt/disks/xccy/creditmigration/')
rm(list=ls(all=TRUE));load('db/bondref.RData');
	print('start')
	load('db/dtldaily.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	# nmdates<-nonmarket.dates(dtl.daily,bondref)
	# dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	# dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	 load('dailyregrun.RData')
	dtregdata<-copy(dtmd$dtl4)
	dtregdata[,year:=year(date)]
	regres<-list()
	 # k=2007
	# dtregdata
	for(k in 2007:2016){
		print(Sys.time())
		print(k)
		tic()
		regres[[k-2003]]<-estimate.crddiff.firm(dtregdata[year==k],parallel.core=30)	
		save(regres,file='firmlevel_daily_temp.rdata')	
		toc()
	}
	save(regres,file='firmlevel_daily.rdata')	



	# out<-estimate.crddiff.firm(dtregdata[date>ymd('2015-08-14') & date<ymd('2015-08-30')],parallel.core=1)	