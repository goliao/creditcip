## This file is dedicated to maintaining rdata database. all modifications are recordered here

rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
#setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# getting a list of pk isin figi full info mapping
# load('gldb_160614.RData')
# save(pifigi,file='pifigi.RData') # this has all the pfi additions until 160614, including all of pfi.rdata file

rm(list=ls(all=TRUE));source('util.r')

# pfi data ----------------------------------------------------------------
load('pifigi.RData')

load('bondref.RData')
bondref160727<-copy(bondref)
save(bondref160727,file='bondref160727.RData')

filestr_sdcclean='sdc96_clean2.dta'
filestr_bondref='bondref_160801_test.RData'


# BondRefData
# generate sdc comprehensive with isin and parsekeyables ------------------
dt_sdc0<-readstata13::read.dta13(filestr_sdcclean) %>% as.data.table()
setkey(dt_sdc0,isin)
dt_sdc0<-dt_sdc0[isin!='-',.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp)][order(isin,-amt)]

load(file='sdcnew.rdata')
df_sdcnew %<>% filter(isin!='-')
df_sdcnew %<>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,PackageID=id_package_sdc,upnames=upco,issue_type_desc=typesec,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% mutate(PackageID=as.numeric(PackageID))

dt_sdcnew<-data.table(df_sdcnew)
setkey(dt_sdcnew,isin)
dt_sdcall<-rbind(dt_sdc0,dt_sdcnew,fill=TRUE)
dt_sdcall <- dt_sdcall[order(-amt)] %>% distinct(isin,.keep_all=T)
# save(dt_sdcall,file='sdcall_160801_test.RData')


# # merge pfigi data
# rm(list=ls())
# source('util.r')
# load('sdcall_160801_test.RData')

load('pifigi.RData')
bondref<-merge(dt_sdcall,pifigi,by='isin',all.x=T)
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


#add bond ref
jpybond<-fread('temp_jpy_bond.csv')[isin!='#N/A Field Not Applicable'][,ccy:='jpy']
bondref<-update.dt(bondref,jpybond)
audbond<-fread('bbg_isin_aud_bonds.csv')[isin!='#N/A Field Not Applicable'][,.(parsekeyable,isin,ccy='aud')]
bondref<-update.dt(bondref,audbond)

## Add some more old referecing data 160614
refadd<-fread('bondrefadd.csv',integer64='numeric')[isin!='#N/A Field Not Applicable']
refadd2<-fread('bondrefadd2.csv',integer64 = 'numeric')[isin!='#N/A Field Not Applicable']
setnames(refadd,'issue_dt','d')
refadd[,amt:=amt/10^6]
refadd[,ccy:=tolower(ccy)][,d:=mdy(d)]
refadd2[,ccy:=tolower(ccy)]
bondref<-update.dt(bondref,refadd,keyfield = 'isin',override = TRUE)
bondref<-update.dt(bondref,refadd2,keyfield = 'isin')
load('pifigi.RData')
bondref<-update.dt(bondref,pifigi,keyfield = 'isin')
save(bondref,file=filestr_bondref)



# try to squeeze more out of existing data from SDC and add to bond ref
rm(list=ls(all=TRUE)); source('util.r'); load(filestr_bondref)
dt_sdc0<-readstata13::read.dta13(filestr_sdcclean) %>% as.data.table()
setkey(dt_sdc0,isin)
dt_sdc1<-dt_sdc0[isin!='-',.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,deal_no,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp,upnat,sp,mdy,settlement2,modnat,modupnat,rank_domicile_nation,upnames,tf_macro_desc,mkt,exch,pub,cusip9,num_packageid,main_tranche)][order(isin,-amt)]
cn1<-dt_sdc1 %>% ds
cn0<-dt_sdc0 %>% ds
cn1[cn1 %ni% cn0];cn0[cn0 %ni% cn1]
load(file='sdcnew.rdata')
dt_sdc2<-df_sdcnew %>% filter(isin!='-') %>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,upnames=upco,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% as.data.table()
dt_sdc2[,id_package_sdc:=NULL]
cn2<-dt_sdc2 %>% ds
cn2[cn2 %ni% cn1];cn1[cn1 %ni% cn2]
setkey(dt_sdc1,isin,deal_no)
setkey(dt_sdc2,isin,deal_no)
dt_sdc1<-dt_sdc1[order(-isin,-deal_no, num_packageid, -amt)] %>% distinct(isin,deal_no,.keep_all=T)
dt_sdc2<-dt_sdc2[order(-isin,-deal_no, -amt)] %>% distinct(isin,deal_no,.keep_all=T)
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
dt_sdcall3<-dt_sdcall2[order(num_packageid,-main_tranche,-amt)] %>% distinct(isin,.keep_all=T)
save(dt_sdcall3,file='sdcall_160801.RData')

rm(list=ls(all=TRUE)); source('util.r'); load(filestr_bondref)
load(file='sdcall_160801.RData')
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

bondref<-copy(bondref2)
save(bondref,file=filestr_bondref)


# get figi data
source('util.r')
figis2request<-bondref[is.na(figi) & !is.na(isin),.(isin)]
figiaddinfo<-figis2request %>% sample_frac(1) %>% requestfigibyisin(.)
#load(file='temp_dfisinfigi.rdata')
dtadd<-figiaddinfo[[1]] %>% as.data.table()
bondref<-update.dt(bondref,dtadd, keyfield = 'isin')
save(bondref,file=filestr_bondref)


# bondref data maintainance -----------------------------------------------
# run everytime there is new additions
rm(list=ls(all=TRUE));source('util.r');load(filestr_bondref)
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
bondref<-update.dt(bondref,ratinglu,keyfield = 'rating',override = T,insertnewrow=F)
bondref[,nrating_sp:=NULL]
bondref[,nrating_mdy:=NULL]
setkey(bondref,isin,parsekeyable)


#rm(list=ls(all=TRUE));source('util.r');load(filestr_bondref)
pfigiadd<-fread('parsekeyablemap2figi_160624.csv')
setnames(pfigiadd,'FIGI','figi')
bondref[!is.na(deal_no),sdcloaded:=1]
bondref[!is.na(figi),figiloaded:=1]
bondref<-update.dt(bondref,pfigiadd,keyfield = c('figi'))
pisinadd<-fread('tempisinmap.csv')[isin!='#N/A Field Not Applicable']
bondref<-update.dt(bondref,pisinadd,keyfield = 'parsekeyable')
bondref<-distinct(bondref)
figireq<-bondref[is.na(figiloaded) & !is.na(figi)][,.(figi)]
setnames(figireq,'figi','id')
figiadd<-requestfigiinfo(figireq,'ID_BB_GLOBAL')
dtadd<-figiadd[[1]] %>% as.data.table()
bondref<-update.br(bondref,dtadd)
save(bondref,file=filestr_bondref)

pkfigiadd<-fread('parsekeyable2figi_08012016.csv') 
setnames(pkfigiadd, c('FIGI','Name','Ticker','Exchange Code' ,'Security Type' ,'Market Sector','FIGI Composite','Share Class','Unique ID'  ),c('figi','name','ticker','exchCode','securityType'  ,'marketSector', 'compositeFIGI' , 'shareClassFIGI' , 'uniqueID'   ))
pkfigiadd[compositeFIGI=='#N/A Field Not Applicable',compositeFIGI:=NA];pkfigiadd[shareClassFIGI=='#N/A Field Not Applicable',shareClassFIGI:=NA]
bondref<-update.br(bondref,pkfigiadd)
setnames(bondref,'parsekeyable','pk')
bondref<-bondref[!is.na(figiloaded) | !is.na(sdcloaded)]
bondref<-distinct(bondref)
save(bondref,file=filestr_bondref)

load('bondref160727.RData')

bondref160727[is.na(pk),.N]
bondref[is.na(pk),.N]

bondref160727[is.na(isin),.N]
bondref[is.na(isin),.N]

bondref160727[is.na(figi) & !is.na(pk),.N]
bondref[is.na(figi) & !is.na(pk),.N]

bondref160727[is.na(figi),.N]
bondref160727[is.na(deal_no),.N]
bondref160727[is.na(deal_no) & is.na(figi),.N]
showdups(bondref160727[!is.na(deal_no)],'deal_no')

bondref<-bondref[order(isin,figi,-amt)] %>% distinct(figi,.keep_all=T)


bondrefA<-bondref[!is.na(figi)]
bondrefB<-bondref160727[!is.na(figi)]
aa<-update.dt(bondrefA,bondrefB,keyfield='figi',diagnostic_rt=T)

aa[[2]][[1]]
aa[[2]][[2]]
ch

# bondref[is.na(figiloaded) & is.na(sdcloaded)]
# bondref160727[is.na(figiloaded) & is.na(sdcloaded)]
# setkey(bondref,pk,isin)

# showdups(bondref,c('pk','isin')) 
# showdups(bondref,c('figi','deal_no')) 

# showdups(bondref[!is.na(pk)],'pk') 
# showdups(bondref[!is.na(figi)],'figi')
# showdups(bondref[!is.na(isin)],'isin')
# showdups(bondref[!is.na(deal_no)],'deal_no')

