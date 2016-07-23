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
#tickersloaded<-dtl[,parsekeyable,by=parsekeyable][,.(parsekeyable)]
save(dtl,tickersloaded,file='bbg_bond_prices.rdata')
load('bbg_bond_prices.rdata')
#resave(dtl,file='gldb.RData')
# merge monthly price data on GBP bonds
rm(list=ls(all=T))
load('gldb.RData')
source('util.r')
dtlgbp<-loadBBGdownload2df('../data/bloomberg/bbg_yld_gbp_bonds_160610.RData')  %>% mutate(batch=3) 
dtl<-rbind(dtl,dtlgbp)
#resave(dtl,file='gldb.RData')
priceraw<-read.dta13('prices_extended.dta') %>% data.table()
#resave(priceraw,file='gldb.RData')



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
#resave(pifigi,file='gldb.RData')

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
#dt_sdcall <- dt_sdcall[order(-amt)] %>% distinct(isin)
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
#resave(bondref,file='gldb.RData')

# guide: use issfilter function to get rid of common filters
bondref[!is.na(parsekeyable),.N]
bondref2<-issfilter(bondref)
bondref2[!is.na(parsekeyable),.N]



# Additional downloads
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RData')
# a function to access what to download
prl<-melt(priceraw[!is.na(date)],id.vars='date',variable.name='ticker')[!is.na(value)]
ticdt<-prl[,.(firstdt=min(date),lastdt=max(date)),by=ticker]
tic2download<-rbind(ticdt[ticker %like% 'eubsv'], #3s6s basis
ticdt[ticker %like% 'eubs'],
ticdt[ticker %like% 'bpbs'],
ticdt[ticker %like% 'jybs'],
ticdt[ticker %like% 'adbs'],
ticdt[ticker %like% 'eusa'],
ticdt[ticker %like% 'bpsw'],
ticdt[ticker %like% 'ussw'],
ticdt[ticker %like% 'jysw'],
ticdt[ticker %like% 'adsw'])
tic2download<-tic2download[,startdt:=lastdt][,parsekeyable:=str_c(ticker, ' Curncy')][,.(parsekeyable,startdt)]
#downloadbbg(tic2download,startdt = ymd('2016-02-25'))
bbgadd<-loadBBGdownload2df('bbg_2016-06-13.RData')
prl[,parsekeyable:=str_c(ticker, ' Curncy')]
prl<-rbind(prl,bbgadd,fill=T)
prl[,value:=as.numeric(value)]
prl<-prl[!is.na(value)]

##save(prl,bondref,dtl,pifigi,priceraw,sdc,file='gldb.RData')
# add jpy bonds
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RData')
jpybond<-fread('temp_jpy_bond.csv')[isin!='#N/A Field Not Applicable']
setkey(jpybond,isin)
jpybondref<-bondref %>% semi_join(jpybond,by='isin')
priceloadedupcusip<-bondref %>% semi_join(dtl,by='parsekeyable') %>% select(upcusip) %>% distinct(.)
jpyref2<-jpybondref %>% semi_join(priceloadedupcusip,by='upcusip')
jpytodownload<-jpybond %>% semi_join(jpyref2,by='isin')
#downloadbbg(jpytodownload$parsekeyable,filestr='bbg_jpybonds_160613.RData',fieldstr = 'YLD_YTM_MID',startdt =ymd('2002-01-01'),splitN = 5)
jpybondpricesadd<-loadBBGdownload2df('bbg_jpybonds_160613.RData')


#aud bonds
rm(list=ls(all=TRUE))
setwd("E:/")
source('util.r')
load('gldb.RData')
audbond<-fread('bbg_isin_aud_bonds.csv')
setkey(audbond,isin)
audbond2download<-(audbond %>% semi_join(bondref,by='isin'))[,.(parsekeyable,isin)]
#downloadbbg(audbond2download$parsekeyable,filestr='bbg_audbonds_160613.RData',fieldstr = 'YLD_YTM_MID',startdt =ymd('2002-01-01'),splitN = 1)
audbondpricesadd<-loadBBGdownload2df('bbg_audbonds_160613.RData')
audbondpricesadd

# actually add the bonds to database:
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RData')
#add bond ref
jpybond<-fread('temp_jpy_bond.csv')[isin!='#N/A Field Not Applicable'][,ccy:='jpy']
bondref<-update.dt(bondref,jpybond)
audbond<-fread('bbg_isin_aud_bonds.csv')[isin!='#N/A Field Not Applicable'][,.(parsekeyable,isin,ccy='aud')]
bondref<-update.dt(bondref,audbond)

#add bond price
jpybondpricesadd<-loadBBGdownload2df('bbg_jpybonds_160613.RData')
jpybondpricesadd[,batch:=4]
setkey(dtl,date,parsekeyable,field)
dtl<-update.dt(dtl,jpybondpricesadd)
audbondpricesadd<-loadBBGdownload2df('bbg_audbonds_160613.RData')
audbondpricesadd[,batch:=4]
dtl<-update.dt(dtl,audbondpricesadd)

##save(bondref,dtl,pifigi,priceraw,prl,sdc,file='gldb.RData')

## Add some more old referecing data 160614
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RData')
refadd<-fread('bondrefadd.csv',integer64='numeric')[isin!='#N/A Field Not Applicable']
refadd2<-fread('bondrefadd2.csv',integer64 = 'numeric')[isin!='#N/A Field Not Applicable']
setnames(refadd,'issue_dt','d')
refadd[,amt:=amt/10^6]
refadd[,ccy:=tolower(ccy)][,d:=mdy(d)]
refadd2[,ccy:=tolower(ccy)]
bondref<-update.dt(bondref,refadd,keyfield = 'isin',override = TRUE)
bondref<-update.dt(bondref,refadd2,keyfield = 'isin')
bondref<-update.dt(bondref,pifigi,keyfield = 'isin')

prl[is.na(field),field:='PX_LAST']
prl[is.na(ticker), ticker:=str_extract(parsekeyable,regex('.*(?= Curncy)'))]
prl[ticker %like% '_',parsekeyable:=NA]
setkey(prl,date,ticker,parsekeyable,field)
##save(bondref,dtl,prl,file='gldb.RData')

# download eur/gbp bonds till end of may:
rm(list=ls(all=TRUE))
source('util.r')
load(file='us_eu_bonddownloadtickers.RData')
#downloadbbg(bondtodownload$parsekeyable,filestr='bbg_eurusdbonds_160614.RData',fieldstr = 'YLD_YTM_MID',startdt =ymd('2016-04-01'),splitN = 20)
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RData')
bondaddeurusd<-loadBBGdownload2df('bbg_eurusdbonds_160614.RData')
bondaddeurusd[,batch:=5]
setkey(dtl,date,parsekeyable,field)
dtl<-update.dt(dtl,bondaddeurusd)
#resave(dtl,file='gldb.RData')

# try to squeeze more out of existing data from SDC and add to bond ref
rm(list=ls(all=TRUE)); source('util.r'); load('gldb.RData')
dt_sdc0<-read.dta13('sdc96_clean2.dta') %>% as.data.table()
setkey(dt_sdc0,isin)
dt_sdc1<-dt_sdc0[isin!='-',.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,deal_no,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp,upnat,sp,mdy,settlement2,modnat,modupnat,rank_domicile_nation,upnames,tf_macro_desc,mkt,exch,pub,cusip9,num_packageid,main_tranche)][order(isin,-amt)]
cn1<-dt_sdc1 %>% ds
cn0<-dt_sdc0 %>% ds
cn1[cn1 %ni% cn0]
cn0[cn0 %ni% cn1]
load(file='sdcnew.rdata')
dt_sdc2<-df_sdcnew %>% filter(isin!='-') %>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,upnames=upco,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% as.data.table()
dt_sdc2[,id_package_sdc:=NULL]
cn2<-dt_sdc2 %>% ds
cn2[cn2 %ni% cn1]
cn1[cn1 %ni% cn2]
setkey(dt_sdc1,isin,deal_no)
setkey(dt_sdc2,isin,deal_no)
dt_sdc1<-dt_sdc1[order(-isin,-deal_no, num_packageid, -amt)] %>% distinct(isin,deal_no)
dt_sdc2<-dt_sdc2[order(-isin,-deal_no, -amt)] %>% distinct(isin,deal_no)
dt_sdc1[cusip9=='-',cusip9:=NA]
dt_sdc2[cusip9=='-',cusip9:=NA]
dt_sdc1[tic=='-',tic:=NA]
dt_sdc2[tic=='-',tic:=NA]
dt_sdc1[upsicp=='',upsicp:=NA]
dt_sdc2[upsicp=='',upsicp:=NA]
dt_sdc1[mdy=='' | mdy=='-',mdy:=NA]
dt_sdc2[mdy=='' | mdy=='-',mdy:=NA]
dt_sdc1[sp=='' | sp=='-',sp:=NA]
dt_sdc2[sp=='' | sp=='-',sp:=NA]
dt_sdc1[,ccy:=tolower(ccy)]
dt_sdc2[,ccy:=tolower(ccy)]
setkey(dt_sdc1,isin,deal_no)
setkey(dt_sdc2,isin,deal_no)
dt_sdcall2<-update.dt(dt_sdc1,dt_sdc2)
#dt_sdcall3<-dt_sdcall2[order(num_packageid,-main_tranche,-amt)] %>% distinct(isin)
save(dt_sdcall3,file='sdcall_160614.RData')

rm(list=ls(all=TRUE)); source('util.r'); load('gldb.RData')
load(file='sdcall_160614.RData')
bondref2<-update.dt(bondref,dt_sdcall3,keyfield = 'isin',override = T)

# test how many NAs were eliminated/introdueced in the process
brn<-bondref %>% ds()
brn2<-bondref2 %>% ds()
brnu<-brn2[brn2 %in% brn]
print('na eliminated: (ngeatives only/introduced)')
for (cn in brnu){
  naelim<-bondref[is.na(eval(exparse(cn))),.N]-bondref2[is.na(eval(exparse(cn))),.N]
  if (naelim<0)  print(str_c(cn,':  ',naelim))
}

# bondref<-copy(bondref2)
## resave(bondref,file='gldb.RData')



#### script to generate ticks used for updating bond data
dtl3[,.N,date][order(date)]
yldsprd2[[2]][date=='2016-02-29',.N,ccy][order(ccy)]
yldsprd2[[2]][date=='2016-03-31',.N,ccy][order(ccy)]
yldsprd2[[2]][date=='2016-04-29',.N,ccy][order(ccy)]

temp<-dtl3[date=='2016-02-31']
temp<-dtl3[date=='2016-03-31']
tempf<-filterglobaluponly(temp)
temp2<-dtl3[date=='2016-04-29']
temp3<-tempf %>% anti_join(temp2,by='parsekeyable')

#bondtodownload<-temp3[,.(parsekeyable)]
save(bondtodownload,file='us_eu_bonddownloadtickers.RData')

# correction of prl scaling
rm(list=ls(all=TRUE));load('gldb.RDATA');source('util.r')
bbgadd<-loadBBGdownload2df('bbg_2016-06-13.RData')
bbgadd[parsekeyable %like% '^..sw',value:=100*value]
prl<-update.dt(prl,bbgadd,keyfield = c('date','parsekeyable','field'),override = T)
# prl<-prl[date!='2016-02-25'] # need to add a column to indicate monthly price for picking out monthly price among daily price
setkey(prl,date,ticker,parsekeyable,field)
prl<-prl %>% distinct() %>% as.data.table()
#resave(prl,file='gldb.RData')


 # download additional swap data
# setwd('E:/')
rm(list=ls(all=TRUE));source('util.r');load('gldb.RData')
tickers2download<-data.table(parsekeyable=c('jysw1 Curncy','bpsw1 Curncy','adsw12 Curncy','adsw15 Curncy','adsw20 Curncy','adsw30 Curncy','eusw1 Curncy'))
# downloadbbg(tickers2download$parsekeyable,filestr='bbg_swapadd_160615.RData',fieldstr = 'PX_LAST',startdt =ymd('1996-01-01'),splitN = 1)
swapadd<-loadBBGdownload2df('bbg_swapadd_160615.RData')
swapadd[,value:=value*100]
prl<-update.dt(prl,swapadd,keyfield = c('date','parsekeyable','field'),override = T)
#resave(prl,file='gldb.RData')

# setwd('E:/')
rm(list=ls(all=TRUE));source('util.r');
tickers2download<-data.table(parsekeyable=c('eusa1 Curncy','eusa50 Curncy','ussw50 Curncy','jysw50 Curncy','bpsw50 Curncy','adsw50 Curncy'))
#downloadbbg(tickers2download$parsekeyable,filestr='bbg_swapadd_160615b.RData',fieldstr = 'PX_LAST',startdt =ymd('1996-01-01'),splitN = 1)
swapadd<-loadBBGdownload2df('bbg_swapadd_160615b.RData')
swapadd[,value:=value*100]
load('gldb.RData')
prl<-update.dt(prl,swapadd,keyfield = c('date','parsekeyable','field'),override = T)
prl[is.na(ticker), ticker:=str_extract(parsekeyable,regex('.*(?= Curncy)'))]
#resave(prl,file='gldb.RData')

# create adjusted swaps and spreads
rm(list=ls(all=TRUE));source('util.r');load('gldb.RData')
# # gen eusw=eusa-eubsv
# # gen eusz=eusw
# prl[ticker %like% '^eusa',.(ticker)] %>% distinct(ticker)
# prl[ticker %like% '^eubsv',.(ticker)] %>% distinct(ticker)
bradd1<-fread('bondrefadd.csv',integer64='numeric')
checkcnexist(bondref,bradd1)
setnames(bradd1,'issue_dt','d')
bondref<-update.dt(bondref,bradd1,keyfield='parsekeyable')

# get figi data
source('util.r')
charSummary(bondref)
figis2request<-bondref[is.na(figi) & !is.na(isin),.(isin)]
figiaddinfo<-figis2request %>% sample_frac(1) %>% requestfigibyisin(.)
#load(file='temp_dfisinfigi.rdata')
dtadd<-figiaddinfo[[1]] %>% as.data.table()
bondref<-update.dt(bondref,dtadd, keyfield = 'isin')
##resave(bondref,file='gldb.RData')


# bondref data maintainance -----------------------------------------------
# run everytime there is new additions
rm(list=ls(all=TRUE));source('util.r');load('gldb.RData')
bondref[,sic1:=as.numeric(str_sub(as.character(sicp),1,1))]
bondref[,upsic1:=as.numeric(str_sub(as.character(upsicp),1,1))]
bondref[is.na(sic1),sic1:=upsic1]
bondref[,sicfac:=factor(sic1)]
setkey(bondref,parsekeyable,isin)
# get rid of non-matches between sdc and bloomberg based on coupon and mat date
bondref[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")),couponsdc=as.numeric(str_extract(descr,"^\\d+.\\d*(?=\\%)")))]
bondref[,matdiff:=as.numeric((matbbg-mat2)/365)]
#bondref<-bondref[matdiff==0 | couponbbg==couponsdc] 
bondref[,ccy:=tolower(ccy)]
bondref[,bbgccy:=tolower(bbgccy)]
warning(print_and_capture(bondref[ccy!=bbgccy,.(ccy,bbgccy)]))
bondref[ccy!=bbgccy,ccy:=bbgccy]
bondref[rating=='NR' | rating=='',rating:=NA]
bondref[mdy=='NR' | mdy=='',mdy:=NA]
bondref[sp=='NR' | sp=='',sp:=NA]
bondref[is.na(rating),rating:=sp]
bondref[is.na(rating),rating:=mdy]
# insert rating for new bondref data
ratinglu<-fread('rating.csv')
# bondref[is.na(nrating),nrating:=nrating_sp]
# bondref[nrating==0,nrating:=nrating_mdy]
bondref<-update.dt(bondref,ratinglu,keyfield = 'rating',override = T)
bondref[,nrating_sp:=NULL]
bondref[,nrating_mdy:=NULL]
setkey(bondref,isin,parsekeyable)
#resave(bondref,file='gldb.RData')


# way too messy to try to get upcusip information based on name identifier, due to mergers, etc
# charSummary(bondref)
# br2_upmiss<-br2[is.na(upcusip)]
# br2_upmiss[!is.na(name)]
# brupadd<-br2_upmiss[,.(name)] %>% distinct()
# uplookup<-bondref[,.(name,upcusip)] %>% distinct()
# m2<-update.dt(brupadd,uplookup,keyfield = c('name')) 
# nameupcusip<-m2[!is.na(name) & !is.na(upcusip)]
# aa<-update.dt(br2,nameupcusip,keyfield = 'name',insertnewrow = FALSE,diagnostic_rt = TRUE)
# 
# aa[[2]]
# 
# bb<-bondref[,.(cu,name)] %>% distinct(cu)
# cc<-merge(aa[[2]],bb,by.x='upcusip',by.y='cu')
# cc %>% View

#setwd('E:/')
rm(list=ls(all=TRUE));source('util.r');load('gldb.RData')
# br<-issfilter(bondref %>% as.data.frame())
# [!is.na(amt),.(amtT=sum(amt)/1000000),by=ccy][order(-amtT)]
#cadchfparkey<-fread('cadchfbonds2download160623.csv',header=T,sep=',')
#some download script
##save(cadchfbonds,file='bbg_cadchfbonds_160623_final.RData')
load('bbg_cadchfbonds_160623_final.RData')
cadchfbonds[,batch:=6]
dtl<-update.dt(dtl,cadchfbonds,keyfield = c('date','parsekeyable','field'))
##resave(dtl,file='gldb.RData')
pfigiadd<-fread('parsekeyablemap2figi_160624.csv')
setnames(pfigiadd,'FIGI','figi')
bondref[!is.na(deal_no),sdcloaded:=1]
bondref[!is.na(figi),figiloaded:=1]
bondref<-update.dt(bondref,pfigiadd,keyfield = c('figi'))
pisinadd<-fread('tempisinmap.csv')[isin!='#N/A Field Not Applicable']
bondref<-update.dt(bondref,pisinadd,keyfield = 'parsekeyable')
##resave(bondref,file='gldb.RData')

### major updates to database 160624
rm(list=ls(all=TRUE));source('util.r');load('gldb160624.RData')
prl[ticker %like% '^ussw' | ticker %like% '^bpsw'| ticker %like% 'eusa' | ticker %like% '^adsw' | ticker %like% '^jysw',value:=value/100]
setnames(prl,'parsekeyable','pk')
setkey(prl,date,pk,ticker,field)
setnames(dtl,'parsekeyable','pk')
setkey(dtl,date,pk,field)
setnames(bondref,'parsekeyable','pk')
tickerblacklist<-c('SS102100 Corp','EF306087 Corp')
dtl<-dtl[pk %ni% tickerblacklist]
prl[,pk:=tolower(pk)]
dtl[,pk:=tolower(pk)]
bondref[,pk:=tolower(pk)]
#downloadbbg(ticker$parsekeyable,filestr='bbg_swapadd_160624_2.RData',fieldstr = 'PX_LAST',startdt =ymd('1996-01-01'),splitN = 1)
prladd<-loadBBGdownload2df('bbg_swapadd_160624.RData') 
prladd2<-loadBBGdownload2df('bbg_swapadd_160624_2.RData')
prl<-update.prl(prl,prladd)
prl<-update.prl(prl,prladd2)
dtl[,monthend:=1]
dtbondadd<-loadBBGdownload2df('bbg_dailybondsadd_160624.RData')
# need to remove override later, and use downloaded monthly
dtl<-update.dtl(dtl,dtbondadd,overridein = TRUE)
##save(dtl,prl,bondref,file='gldb.RData')



rm(list=ls(all=TRUE));source('util.r');load('gldb.RData')
bondref<-distinct(bondref)
# figireq<-bondref[is.na(figiloaded) & !is.na(figi)][,.(figi)]
# setnames(figireq,'figi','id')
# figiadd<-requestfigiinfo(figireq,'ID_BB_GLOBAL')
# dtadd<-figiadd[[1]] %>% as.data.table()
load('temp.rdata')
bondref<-update.br(bondref,dtadd)
##resave(bondref,file='gldb.RData')
bondref<-bondref[!is.na(figiloaded) | !is.na(sdcloaded)]
setkey(bondref,pk,isin)
#bondref<-unique(bondref)
##resave(bondref,file='gldb.RData')
showdups(bondref,c('pk','isin')) %>% View
showdups(bondref[!is.na(pk)],'pk') %>% View
showdups(bondref[!is.na(figi)],'figi')
showdups(bondref[!is.na(isin)],'isin')
showdups(bondref[!is.na(deal_no)],'deal_no')


#merge in daily bond data
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');

# add daily bond data:
load('dailyprices.rdata')
dtadd<-df_p_daily %>% as.data.table() %>% melt(id.vars=c('date','ticker'),measure.vars=c('OAS_SPREAD_BID','YLD_YTM_MID'),variable.name='field') %>% rename(pk=ticker)
dtadd<-dtadd[!is.na(value)]
temp<-update.dtl(dtl,dtadd,diagret=T)
dtl<-temp[[1]]
monthlydownloadagain<-temp[[2]][[1]][,.N,pk]

dtadd2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_daily_batch2.RData')
temp<-update.dtl(dtl,dtadd2,diagret=T)
dtl<-temp[[1]]

#resave(dtl,file='gldb.RData')
# monthlydownloadagain<-rbind(monthlydownloadagain,temp[[2]][[1]][,.N,pk])
## save(monthlydownloadagain,file='get_mo_160627.RData')



#merge daily prl data
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');
prl[is.na(pk),pk:='IND']
prd1<-loadBBGdownload2df_older('../data/bloomberg/bbgpricesall151207_daily.RData')
prd2<-loadBBGdownload2df_older('../data/bloomberg/bbgprices_additional_daily_151210.RData')
prd3<-loadBBGdownload2df_older('../data/bloomberg/bbgprices_dailyCM_160204.RData')
prd4<-loadBBGdownload2df_older('../data/bloomberg/bbgprices_151210_daily_emccy.RData')

prl<-update.prl(prl,prd1)
prl<-update.prl(prl,prd2)
prl<-update.prl(prl,prd3)
prl<-update.prl(prl,prd4)

baml<-prl[pk=='IND']
prl<-prl[pk!='IND']

##resave(prl,baml,file='gldb.RData')

rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');

prl.status<-get.prl.status(prl)
prl.status[DailyMax<'2016-02-25'] # do not need to get
prl.status[is.na(DailyMax)] # get long term times series
prl.status[DailyMax>='2016-02-25'] #get starting 25th

dtl.status<-get.dtl.status(dtl)
dtl.status[matured==0,.N,.(ccy,DailyMax)][order(ccy,-N)] %>% View
#to download daily these
togetdaily<-dtl.status[matured==0 & ccy %in% c('eur','usd') & (DailyMax<'2016-06-01' | is.na(DailyMax))]

togetdaily[,.N,DailyMax]


#resolve maturity difference
bb<-merge(bondref[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],dtl[,.N,pk],by='pk',all.y=TRUE)
bb %>% ds
bb[is.na(ccy)]
bb[,.(pk,i,descr,d,settlement2,mat2,matbbg,diff=as.numeric(matbbg-mat2))][abs(diff)>10]


#7/5/16
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');
load('../data/bloomberg/smallIssuance.RData')
load('../data/bloomberg/dtchfdaily.RData')
dtl<-update.dtl(dtl,dtsmalliss,override=TRUE)
dtl<-update.dtl(dtl,dtchfdaily,override=TRUE)
dtl<-dtl %>% distinct(date,pk,field)
#resave(dtl,file='gldb.RData')

rm(list=ls(all=TRUE));load('gldb.RData')
dtl<-dtl[field=='YLD_YTM_MID'][,field:=NULL]
prl[,field:=NULL]
#save(dtl,prl,bondref,monthenddates,file='gldbsmall.RData')
# 
# 
# #7/6/16 does not make sense to use sqlite given the large size
# rm(list=ls(all=TRUE));load('gldb.RData')
# source('util.r');
# dtl[,fieldfac:=factor(field)]
# dtl[,pkfac:=factor(pk)]
# dtl %>% ds
# dtl2<-dtl[,.(date,as.integer(pkfac),as.integer(fieldfac),as.double(value))]
# source('dbutil.r')
# gdbwrite('glsecval',dtl2,dbname='temp.sqlite',bool_overwrite = TRUE)



