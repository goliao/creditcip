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
load('bbg_bond_prices.rdata')
resave(dtl,file='gldb.RData')
# merge monthly price data on GBP bonds
rm(list=ls(all=T))
load('gldb.RData')
source('util.r')
dtlgbp<-loadBBGdownload2df('../data/bloomberg/bbg_yld_gbp_bonds_160610.RData')  %>% mutate(batch=3) 
dtl<-rbind(dtl,dtlgbp)
resave(dtl,file='gldb.RData')
priceraw<-read.dta13('prices_extended.dta') %>% data.table()
resave(priceraw,file='gldb.RData')



# pfi data ----------------------------------------------------------------
load(file='pfi.rdata')
pi2<-fread('parsekeyable_isin_match_bbg.csv')[isin!='#N/A Requesting Data...'] # new bbg downloaded parsekeyable <-> isin
pi2 %<>% anti_join(pfi,by='isin') %>% distinct()
figiadd<-requestfigibyisin(pi2[,.(isin)])[[1]] %>% as.data.table()
pifigiadd<-merge(figiadd ,pi2,by='isin') %>% as.data.table()
pifigi<-bind_rows(pfi,pifigiadd) %>% data.table()

# additional GBP bond info and price data
load('gldb.RData')
gbpadd<-fread('gbpbondref_temp.csv')
figiadd<-gbpadd[,.(isin)] %>% requestfigibyisin(.)
pfiadd<-merge(figiadd[[1]],gbpadd,by='isin') %>% as.data.table()
pfiadd<-pfiadd[isin!='#N/A Field Not Applicable'] 
pifigi<-rbind(pifigi,pfiadd,fill=T)
setnames(pifigi,'ccy','bbgccy')
resave(pifigi,file='gldb.RData')

# BondRefData
# generate sdc comprehensive with isin and parsekeyables ------------------
rm(list=ls())
source('util.r')

dt_sdc0<-read.dta13('sdc96_clean2.dta') %>% as.data.table()
setkey(dt_sdc0,isin)
dt_sdc0<-dt_sdc0[isin!='-',.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp)][order(isin,-amt)]

load(file='sdcnew.rdata')
df_sdcnew %<>% filter(isin!='-')
df_sdcnew %<>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,PackageID=id_package_sdc,upnames=upco,issue_type_desc=typesec,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% mutate(PackageID=as.numeric(PackageID))

dt_sdcnew<-data.table(df_sdcnew)
setkey(dt_sdcnew,isin)
dt_sdcall<-rbind(dt_sdc0,dt_sdcnew,fill=TRUE)
dt_sdcall <- dt_sdcall[order(-amt)] %>% distinct(isin)
save(dt_sdcall,file='sdcall.RData')




# merge pfigi data
rm(list=ls())
source('util.r')
load('sdcall.RData')
load('gldb.RData')
bondref<-merge(dt_sdcall,pifigi,by='isin',all.x=T)
#bondref[!is.na(parsekeyable)]
setkey(bondref,parsekeyable,isin)
# get rid of non-matches between sdc and bloomberg based on coupon and mat date
bondref[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")),couponsdc=as.numeric(str_extract(descr,"^\\d+.\\d*(?=\\%)")))]
bondref[,matdiff:=as.numeric((matbbg-mat2)/365)]
#bondref<-bondref[matdiff==0 | couponbbg==couponsdc] 
bondref[,ccy:=tolower(ccy)]
# insert rating for new bondref data
bondref[is.na(nrating),nrating:=nrating_sp]
bondref[nrating==0,nrating:=nrating_mdy]
bondref[,nrating_sp:=NULL]
bondref[,nrating_mdy:=NULL]
resave(bondref,file='gldb.RData')

# guide: use issfilter function to get rid of common filters
bondref[!is.na(parsekeyable),.N]
bondref2<-issfilter(bondref)
bondref2[!is.na(parsekeyable),.N]



# Additional downloads

# a function to access what to download

tic<-priceraw %>% ds('eubs')
prl<-melt(priceraw[!is.na(date)],id.vars='date',variable.name='ticker')[!is.na(value)]
ticdt<-prl[,.(firstdt=min(date),lastdt=max(date)),by=ticker]


ticdt[ticker %like% 'eubs']
ticdt[ticker %like% 'bpbs']
ticdt[ticker %like% 'jybs']
ticdt[ticker %like% 'adbs']
ticdt[ticker %like% 'eusa']
ticdt[ticker %like% 'bpsw']
ticdt[ticker %like% 'ussw']
ticdt[ticker %like% 'jysw']
ticdt[ticker %like% 'adsw']

ticdt[,.N,lastdt]


priceraw %>% ds('eubs')

# todo --------------------------------------------------------------------


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


