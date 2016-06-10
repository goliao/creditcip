# generate global bond tickers/isins
rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# BBG generated -----------------------------------------------------------
# takes in bbg tiker and ultimate parent info and spits out global bonds tickers
# this is the earliest batch of downloads
raw<-read.dta13('sdc96_clean2.dta')
bondinfo<-read.csv("../data/bloomberg/bbg_bsrch_all-usd-eur-ig-small.csv")
colnames(bondinfo)<-c('ticker','isin','id_bb_co','id_bb_ultimate_co','crncy')
up_global<-bondinfo %>% group_by(id_bb_ultimate_co,crncy) %>% dplyr::summarise(ct=length(ticker)) %>% 
  reshape2::dcast(.,id_bb_ultimate_co~crncy,value.var='ct',fill=0) %>% mutate(isglobal=(EUR & USD)) %>% filter(isglobal==TRUE)
#break into 20 chuncks
up_global$fac=sample(1:20,nrow(up_global),replace=TRUE)
globalbonds<-bondinfo %>% inner_join(.,up_global %>% select(id_bb_ultimate_co,fac),by='id_bb_ultimate_co') %>% arrange(fac,id_bb_ultimate_co) 
save(globalbonds,file='globalbondtickers.rdata')
save(globalbonds,file='J:/data/globalbondtickers.rdata')


# SDC generated isin ------------------------------------------------------
# this is generated more or less for query from figi, so that sdc can be matched to figi to parsekeyables
df_sdc_raw<-read.dta13('sdc96_clean2.dta') %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
df_sdc <- df_sdc_raw %>% sample_frac(1)
#find issuances that are global in EU and USD
up_global<-df_sdc %>% group_by(upcusip,ccy) %>% filter(ccy %in% c('USD','EUR','GBP','JPY','CHF','AUD','CAD')) %>% 
  dplyr::summarise(ct=length(upcusip)) %>% tbl_df %>% 
  tabulate('upcusip') %>% filter(Freq>1) %>% rename(upcusip=Var1) %>%  filter(upcusip!=0) %>% select(upcusip)
up_global$fac=sample(1:20,nrow(up_global),replace=TRUE)
# keeping only global issuer bonds
df_sdc %<>% inner_join(.,up_global %>% select(upcusip,fac),by='upcusip') %>% 
  arrange(fac,upcusip)

# generous filtering, for sending to bloomberg to get bbg tickers 
df_sdc_generous <- df_sdc %>% filter(
  amt >= 50,
  ytofm >= 1,
  ytofm <= 99999,
  mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
  secur %ni% c(
    "Cum Red Pfd Shs",
    "Non-Cum Pref Sh" ,
    "Preferred Shs" ,
    "Pfd Stk,Com Stk"
  ),
  tf_mid_desc != 'Government Sponsored Enterprises',
  !grepl('Flt', secur),
  !grepl('Zero Cpn', secur),
  !grepl('Float', secur),
  !grepl('Fl', descr),
  !grepl('Zero Cpn', descr),
  !grepl('Mortgage-backed',issue_type_desc),
  !grepl('Asset-backed',issue_type_desc),
  !grepl('Federal Credit Agency',issue_type_desc),
  !grepl('Loan',descr)
) %>% print
bbg_isin_req <- df_sdc_generous %>% filter(isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print
# send this file to bloomberg to request parskey
bbg_isin_req %>% write.csv(file='isin_sdc.csv')

# # only EUR USD
# bbg_isin_req_euus <- df_sdc_generous %>% 
#   filter(ccy %in% c('EUR','USD') ,isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print
# # eur usd small
bbg_isin_req_euus <- df_sdc_generous %>%
  filter(amt>=100, ytofm>=2,pub!='Govt.',ccy %in% c('EUR', 'USD') , isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print
save('sdc_generated_isin.rdata')

# isin to figi ------------------------------------------------------------
#let's be very bold and get all isin figi mappings
sdc0_isins<-df_sdc %>% distinct(isin) %>% filter(isin != '-') %>% select(isin)
load(file='sdcnew.rdata')
# new additions of sdc data
newadditions<-df_sdcnew %>% filter(isin!='-') %>% distinct(isin) %>% anti_join(sdc0_isins,by='isin') %>% select(isin)
sdc_isins<-sdc0_isins %>% full_join(df_sdcnew %>% filter(isin!='-') %>% distinct(isin) %>% select(isin),by='isin')
isin2figi<-sdc_isins %>% requestfigibyisin(.)
isin2figi2<-sdc_isins %>% sample_frac(1) %>% requestfigibyisin(.)
save(isin2figi,isin2figi2,file='isin2figi_examine.rdata')
# save(isin2figi,file='isin2figi.rdata')
load(file='isin2figi.rdata')

isin2figi
load(file='temp_dfisinfigi.rdata')
df_isin2figi_all %>% filter(!is.na(figi))

# something is wrong in the retreival process
isin2figi %>% countdups('isin')
isin2figi %>% countdups('figi')
ifdup<-isin2figi %>% showdups('figi') 
reqdup<-sdc_isins %>% mutate(rowN=row_number()) %>% semi_join((isin2figi %>% showdups('figi')),by='isin') %>% arrange(rowN) 
reqdup %>% filter((isin=='US12548TAG58' | isin=='US140420NE62'))
rectified<-reqdup %>% select(isin) %>% requestfigibyisin(.) %<>% filter(!is.na(figi))

rectified %>% showdups('figi')
rectified %>% filter(figi=='BBG005M0YQR9') %>% View

isin2figi<-isin2figi %>% anti_join(reqdup,by='isin') %>% bind_rows(rectified)
isin2figi %>% showdups('figi')
isin2figi %>% showdups('isin')
isin2figi %>% nrow()
isin2figi %>% filter(isin=='US312923XE00')
isin2figinew %>% filter(isin=='US312923XE00')


## new with datatable
rm(list=ls())
source('util.r')
load(file='isin2figi.rdata')
ifold<-data.table(isin2figi)
rm(isin2figi)
load(file='isin2figi_examine.rdata')
if1<-data.table(isin2figi[[1]])
if2<-data.table(isin2figi2[[1]])

# check that the nonduplicated if1 and if2 mappings are the same, by figi
setkey(if1,figi)
setkey(if2,figi)
#no dup versions
if1_nd<-if1[if1[,.N,figi][N==1,figi]]
if2_nd<-if2[if2[,.N,figi][N==1,figi]]
if12_nd<-if1_nd[if2_nd,nomatch=0] #inner merge
if12_nd[isin!=i.isin] # good, at least they agree when there are no dups

# bind 1 and 2 together, take all verified mappings
if12<-rbind(if1,if2)
setkey(if12,figi,isin)
if12[,N:=.N,by=.(figi,isin)]
if12<-unique(if12[N==2])
if12<-if12[!is.na(figi)][,N:=NULL]
# bind 1 & 2 request with old mapping
ifnew<-rbind(if12,ifold)
setkey(ifnew,figi,isin)
ifnew[,N:=.N,by=.(figi,isin)]
ifnew<-unique(ifnew[N==2])[!is.na(figi)][,N:=NULL]
ifnew<-ifnew[!is.na(isin)]
# what's still missing from sdc
isinmiss<-sdc_isins %>% anti_join(ifnew,by='isin')
ifsup1<-requestfigibyisin(isinmiss)
ifsup2<-isinmiss %>% sample_frac(1) %>% requestfigibyisin()
#check supplemental similarity
dt.ifsup1<-data.table(ifsup1[[1]])
dt.ifsup2<-data.table(ifsup2[[1]])
dt.ifsup<-rbind(dt.ifsup1,dt.ifsup2)
setkey(dt.ifsup,figi,isin)
dt.ifsup[,N:=.N,by=.(figi,isin)]
dt.ifsup<-unique(dt.ifsup[N==2])[,N:=NULL]
dt.ifsup<-dt.ifsup[!is.na(figi)][!is.na(isin)]
setkey(ifnew,isin,figi)
setkey(dt.ifsup,isin,figi)
ifnew<-rbind(ifnew,dt.ifsup)
isin2figi<-ifnew
save(isin2figi,file='isin2figiNew.rdata')

parsekeyfigi<-read.csv(file='parsekeyablefigi.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% select(parsekeyable,figi) %>% data.table()
showdups.dt(parsekeyfigi,'figi')
showdups.dt(isin2figi,'figi')
setkey(parsekeyfigi,figi)
setkey(isin2figi,figi)
pfi<-parsekeyfigi[isin2figi][!is.na(parsekeyable)]
save(pfi,file='pfi.rdata')


# generate sdc comprehensive with isin and parsekeyables ------------------
rm(list=ls())
source('util.r')
load(file='pfi.rdata')

df_sdc0<-read.dta13('sdc96_clean2.dta') 
dt_sdc0<- data.table(df_sdc0) 
setkey(dt_sdc0,isin)
dt_sdc0<-dt_sdc0[isin!='-',.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp)][order(isin,-amt)]
dt_sdc0 %<>% filter(
  amt >= 50,
  ytofm >= 1,
  ytofm <= 99999,
  mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
  secur %ni% c(
    "Cum Red Pfd Shs",
    "Non-Cum Pref Sh" ,
    "Preferred Shs" ,
    "Pfd Stk,Com Stk"
  ),
  tf_mid_desc != 'Government Sponsored Enterprises',!grepl('Flt', secur),!grepl('Zero Cpn', secur),!grepl('Float', secur),!grepl('Fl', descr),!grepl('Zero Cpn', descr),!grepl('Mortgage-backed', issue_type_desc),!grepl('Asset-backed', issue_type_desc),!grepl('Federal Credit Agency', issue_type_desc),!grepl('Loan', descr)
) 
load(file='sdcnew.rdata')
df_sdcnew %<>% filter(isin!='-')
df_sdcnew %<>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,PackageID=id_package_sdc,upnames=upco,issue_type_desc=typesec,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% mutate(PackageID=as.numeric(PackageID))
df_sdcnew %<>% filter(
  amt >= 50,
  ytofm >= 1,
  ytofm <= 99999,
  mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
  !grepl('Fl', descr),!grepl('Zero Cpn', descr),!grepl('Mortgage-backed', issue_type_desc),!grepl('Asset-backed', issue_type_desc),!grepl('Federal Credit Agency', issue_type_desc),!grepl('Loan', descr)
) 
dt_sdcnew<-data.table(df_sdcnew)
setkey(dt_sdcnew,isin)
dt_sdcall<-rbind(dt_sdc0,dt_sdcnew,fill=TRUE)
setkey(pfi,isin)
setkey(dt_sdcall,isin)
dt_sdc2<-dt_sdcall[pfi][order(parsekeyable,-amt)]
setkey(dt_sdc2,parsekeyable,isin)
# get rid of non-matches between sdc and bloomberg based on coupon and mat date
dt_sdc2[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")),couponsdc=as.numeric(str_extract(descr,"^\\d+.\\d*(?=\\%)")))]
dt_sdc2[,matdiff:=(matbbg-mat2)/365]
dt_sdc2[matdiff!=0 & couponbbg!=couponsdc,.(isin,matdiff,ytofm,d,mat2,matbbg,couponsdc,couponbbg,descr,i,name,tic,ticker)] %>% View

dt_sdc2<-dt_sdc2[matdiff==0 | couponbbg==couponsdc] 
sdc<-dt_sdc2[!is.na(ccy)] %>% distinct(parsekeyable,isin)
save(sdc,file='sdc.rdata')

save.image(file='temp160608.rdata')
##check for and add additional mapping with bbg derived isin to parsekeyable
pi2<-fread('parsekeyable_isin_match_bbg.csv')[isin!='#N/A Requesting Data...'] # new bbg downloaded parsekeyable <-> isin
pi1<-pfi[,.(isin,parsekeyable)]
pi2 %<>% anti_join(pi1,by='isin')
# check how many downloaded prices there are from bbg and in the SDC database
dt_sdcall %>% semi_join(pi2,by='isin') # only 2!!!
# which ones exists on bbg but not SDC???
figidatamissing<-pi2 %>% anti_join(dt_sdc0,by='isin') %>% anti_join(dt_sdcnew,by='isin')
figiadd<-figidatamissing[,.(isin)] %>% requestfigibyisin()
todownload<-figiadd[[1]] %>% as.data.table()
todownload %>% write.csv('todownloadbbg.csv') # good to download ccy,rating, upcusip for additional use with existing data

# Test comparing isins directly downloaded from bloomberg with those matched via isin. no errors made, but missing a few observations
# dt_bbgisin<-fread("temp_bbgisindirectmapping.csv")
# dt_bbgisin[,source:=1]
# sdc[,source:=0]
# setkey(sdc,isin)
# setkey(dt_bbgisin,isin)
# comp<-sdc[dt_bbgisin][!is.na(parsekeyable),.(isin,parsekeyable,source,i.parsekeyable,i.source)]
# comp[parsekeyable!=i.parsekeyable]
# addmapping<-dt_bbgisin %>% anti_join(sdc,by=c('isin'))
# dt_sdcall %>% semi_join(addmapping,by='isin')

# # 
# df_sdc0 %<>% distinct(isin)
# 
# load(file='sdcnew.rdata')
# df_sdcnew %<>% filter(isin!='-')
# df_sdcnew %<>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,PackageID=id_package_sdc,upnames=upco,issue_type_desc=typesec,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% mutate(PackageID=as.numeric(PackageID))
# df_sdcnew %<>% distinct(isin)
# #merge the two sdc files together
# df_sdc_full<- df_sdc0 %>% full_join((df_sdcnew %>% anti_join(df_sdc0,by='isin')))
# parsekeyfigi<-read.csv(file='parsekeyablefigi.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% select(parsekeyable,figi)
# parsekeyfigiisin<-parsekeyfigi %>% left_join(isin2figi,by='figi') %>% select(parsekeyable,figi,isin,everything())
# df_sdc_all<-df_sdc_full %>% left_join(isin2figi,by='isin') %>% left_join(parsekeyfigi,by='figi')
# 
# parsekeyfigiisin %>% countdups('parsekeyable')
# parsekeyfigiisin %>% countdups('figi')
# 
# df_sdc_bbg<-parsekeyfigiisin %>% filter(!is.na(isin)) %>% left_join(df_sdc_full,by='isin')
# save(df_sdc_all,df_sdc_bbg,file='sdc_all.rdata')
# 


# sdc_bbg_matched ---------------------------------------------------------
load('isin2figi.rdata')
# use figi paraskeyable matching
parsekeyfigi<-read.csv(file='parsekeyablefigi.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% select(parsekeyable,figi)

# how many that's in bbg but not in sdc  
parsekeyfigi %>% anti_join(isin2figi,by='figi')

# how many that's in sdc parent but not in bbg eur usd
df_isin2figi_all %>% anti_join(parsekeyfigi,by='figi')

# union
sdc_bbg<-df_isin2figi_all %>% distinct(figi) %>% inner_join(parsekeyfigi,by='figi')

# from sdc eur us small, get paraskeyable that can be used for download, this include Non-IG
bbg_ticker_download<-bbg_isin_req_euus %>% inner_join(df_isin2figi_all,by='isin') %>% distinct(figi) %>% inner_join(parsekeyfigi,by='figi') %>% print

# # from bbg search on ig, get ones that has overlap with sdc
bbgig<-read.csv(file='bbgIGbsrch.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% print
    # filter down to overlap with sdc global issuance
bbgsrch<-sdc_bbg %>% semi_join(bbgig,by='parsekeyable') %>% print

# exclude ones I have already downloaded monthly data and formulate batch 1
load('globalbondtickers.rdata')
downloaded_mo<-globalbonds %>% tbl_df() %>% mutate(parsekeyable=as.character(ticker)) %>% select(parsekeyable)
bdownload_batch1_mo<-bbgsrch %>% anti_join(downloaded_mo,by='parsekeyable') %>% select(parsekeyable) %>% print
bdownload_batch2_daily<-bbgsrch %>% select(parsekeyable) %>% print
bdownload_batch1_mo %<>% mutate(fac=sample(1:20,nrow(.),replace=TRUE)) %>% print
bdownload_batch2_daily %<>% mutate(fac=sample(1:20,nrow(.),replace=TRUE)) %>% print
save(bdownload_batch1_mo,bdownload_batch2_daily,file='bbgdownload_042616.rdata')

## download "HY" that's really not HY
bbgall<-read.csv(file='bsrch_eurusd_all.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% print
bbgHY<-sdc_bbg %>% semi_join(bbgall %>% anti_join(bbgig,by='parsekeyable'),by='parsekeyable') 
bbgHY_sdc<-bbgHY %>% left_join(df_sdc,by='isin')
bbgHY_sdc %>% distinct(isin) %>% filter(nrating==1) %>% View
bbgHY_sdc %>% distinct(isin) %>% tabulate('i') %>% View
bdownload_batch3hy<-bbgHY  %>% select(parsekeyable) %>% mutate(fac=sample(1:20,nrow(.),replace=TRUE)) %>%  print
save(bdownload_batch3hy,file='bbgdownload_042616B.rdata')



## download what's remaining in bbg but not in sdc again
load('bbg_bond_prices.rdata')
downloaded<-df_p %>% distinct(parsekeyable) %>% select(parsekeyable)
bbgall<-read.csv(file='bsrch_eurusd_all.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% print
undownloaded<-bbgall %>% anti_join(downloaded)
load('sdc_all.rdata')
undownloaded_unmatched<-undownloaded %>% anti_join(df_sdc_bbg,by='parsekeyable')
downloaded_unmatched<-downloaded %>% anti_join(df_sdc_bbg,by='parsekeyable')
undownloaded_matched<-undownloaded %>% semi_join(df_sdc_bbg,by='parsekeyable')
# get isin for downloaded unmatched
downloaded_unmatched %>% write.csv(file='downloaded_unmatched.csv')
#get isin, ccy, upparent, issue date, amt, etc
undownloaded_unmatched %>% write.csv(file='undownloaded_unmatched.csv')

# what information we are missing in sdc ----------------------------------
# very little information is missing in sdc
df_sdc_raw<-read.dta13('sdc96_clean2.dta') %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
df_sdc <- df_sdc_raw %>% sample_frac(1)
bbgsrch_sdc<-bbgsrch %>% left_join(df_sdc,by='isin')

bbgsrch_sdc %>% ds
bbgsrch_sdc %>% distinct(isin) %>% tabulate('upnat') %>% View
bbgsrch_sdc %>% distinct(isin) %>% tabulate('mdy') %>% View
