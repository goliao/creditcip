## This file is dedicated to maintaining rdata database. all modifications are recordered here
# this is only for construction of dtl monthly bond data
# 7/26/16 Reconstruct Gldb monthly only; only merging on yld
rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration");#setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# Bloomberg: combine all price data --------------------------------------------------
#load the old sprd data
dtl<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413.rdata')  %>% mutate(batch=0)  %>% as.data.table()
dtl_batch1<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData') %>% mutate(batch=1) %>% as.data.table()
dtl_batch1<-dtl_batch1[field=='YLD_YTM_MID']
dtl_batch2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY.RData') %>% mutate(batch=2) %>% as.data.table()
dtl_batch2<-dtl_batch2[field=='YLD_YTM_MID']
dtl[,field:=NULL]
dtl_batch1[,field:=NULL]
dtl_batch2[,field:=NULL]
setkey(dtl,date,pk)
dtl<-update.dtl.mo(dtl,dtl_batch1)
dtltemp<-update.dtl.mo(dtl,dtl_batch2,override=T,diagret=T)
dtl<-dtltemp[[1]]
recheck<-dtltemp[[2]][[1]][,.N,pk][,.(pk)]

# merge monthly price data on GBP bonds
dtlgbp<-loadBBGdownload2df('../data/bloomberg/bbg_yld_gbp_bonds_160610.RData')  %>% mutate(batch=3) %>% as.data.table()
#check thta the field is YLD_YTM_MID only and the dates are monthly only!
dtlgbp[,.N,field];dtlgbp[,.N,date]
dtl<-update.dtl.mo(dtl,dtlgbp)
# Additional JPY downloads
dtladd.mo<-loadBBGdownload2df('bbg_jpybonds_160613.RData')
dtladd.mo[,.N,field];dtladd.mo[,.N,date]
dtl<-update.dtl.mo(dtl,dtladd.mo)

#Aud monthly bond yield
dtladd.mo<-loadBBGdownload2df('bbg_audbonds_160613.RData')
dtladd.mo[,.N,field];dtladd.mo[,.N,date]
dtl<-update.dtl.mo(dtl,dtladd.mo)

# download eur/gbp bonds till end of may:
dtladd.mo<-loadBBGdownload2df('bbg_eurusdbonds_160614.RData')
dtladd.mo[,.N,field];dtladd.mo[,.N,date]
dtladd.mo<-dtladd.mo[date!='2016-04-30']
dtl<-update.dtl.mo(dtl,dtladd.mo)

load('bbg_cadchfbonds_160623_final.RData')
dtladd.mo<-cadchfbonds
dtladd.mo[,.N,field];dtladd.mo[,.N,date]
setnames(dtladd.mo,old = 'parsekeyable','pk')
dtl<-update.dtl.mo(dtl,dtladd.mo)
#save(dtl,recheck,file='dtlmo.rdata')

tickerblacklist<-tolower(c('SS102100 Corp','EF306087 Corp'))
dtl<-dtl[pk %ni% tickerblacklist]

load('../data/bloomberg/smallIssuance.RData')
dtladd.mo<-dtsmalliss
dtladd.mo[,.N,field];dtladd.mo[,.N,date]
dtltemp<-update.dtl.mo(dtl,dtladd.mo,overridein = T,diagret = T)
dtl<-dtltemp[[1]]
recheck<-rbind(recheck,dtltemp[[2]][[1]][,.N,pk][,.(pk)])
dtl.mo<-dtl

#Update old bonds until july 31,2016
load('dtlmoadd20160731.RData')
dtltemp<-update.dtl.mo(dtl.mo,dtladd.monthly,overridein = T,diagret = T)
dtl.mo<-dtltemp[[1]]
recheck<-rbind(recheck,dtltemp[[2]][[1]][,.N,pk][,.(pk)])

#Update with new issuance until july 31,2016
load(file='dtl160731newiss.RData')
dtltemp<-update.dtl.mo(dtl.mo,dtladd.monthly,overridein = T,diagret = T)
dtl.mo<-dtltemp[[1]]
recheck<-rbind(recheck,dtltemp[[2]][[1]][,.N,pk][,.(pk)])
#save(dtl.mo,recheck,file='dtlmo.rdata')


# load again
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration");#setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE))
load('dtlmo.rdata')
source('util.r')


# load('gldb.RData')
# bondstatus<-get.dtl.status.mo(dtl.mo,gracewindow=30,bondref)
# dttoget<-bondstatus[matured==0 & monthlyfilled==0 & !is.na(ccy)]
# save(dttoget,file='bonds2update160726.rdata')
# 
# dtladd.mo<-loadBBGdownload2df('dtl160726.RData')
# update.dtl.mo(dtl.mo,dtladd.mo)

