## This file is dedicated to maintaining rdata database. all modifications are recordered here


rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# Bloomberg: combine all price data --------------------------------------------------
#load the old sprd data
dtl_spd<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')  %>% mutate(batch=0) 
dtl_yld2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413.rdata')  %>% mutate(batch=0) 
dtl_batch1<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData') %>% mutate(batch=1) 
dtl_batch2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch2_asw.RData')  %>% mutate(batch=1) 
dtl_batch3<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY.RData') %>% mutate(batch=2)
dtl_batch4<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY_asw.RData') %>% mutate(batch=2)
dtl<-rbind(dtl_spd,dtl_yld2,dtl_batch1,dtl_batch2,dtl_batch3,dtl_batch4)
# get rid of duplicates, keep more recent downloads
setkey(dtl,date,parsekeyable,field)
dtl<-unique(dtl, fromLast = TRUE)
tickersloaded<-dtl[,parsekeyable,by=parsekeyable][,.(parsekeyable)]
save(dtl,tickersloaded,file='bbg_bond_prices.rdata')


# mySQL -------------------------------------------------------------------
source('util.r')
load('sdc.rdata')
sdc[ccy=='USD',ccy:='usd']
sdc[ccy=='EUR',ccy:='eur']
DBwrite('BondRef',sdc)

load('bbg_bond_prices.rdata')
DBwrite('glsecval',dtl)

load(file='pfi.rdata')
pi2<-fread('parsekeyable_isin_match_bbg.csv')[isin!='#N/A Requesting Data...'] # new bbg downloaded parsekeyable <-> isin
pi2 %<>% anti_join(pfi,by='isin') %>% distinct()
figiadd<-requestfigibyisin(pi2[,.(isin)])[[1]] %>% as.data.table()
pifigiadd<-merge(figiadd ,pi2,by='isin') %>% as.data.table()
pifigi<-bind_rows(pfi,pifigiadd) %>% data.table()
DBwrite('pifigi',pifigi)

save(sdc,dtl,pifigi,file='gldb.RData')

priceraw<-read.dta13('prices_extended.dta') %>% data.table()
resave(priceraw,file='gldb.RData')


## add these two at some point
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RDATA')
refadd<-fread('bondrefadd.csv')
refadd2<-fread('bondrefadd2.csv')

refadd %>% anti_join(sdc,by='isin')
refadd %>% semi_join(pifigi,by='isin')
refadd2 %>% semi_join(pifigi,by='isin')
refadd2 %>% anti_join(sdc,by='isin')
refadd2 %>% semi_join(sdc,by='isin')
