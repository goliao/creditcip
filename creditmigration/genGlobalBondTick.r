# Generate global bond tickers/isins
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
# new additions
newadditions<-df_sdcnew %>% filter(isin!='-') %>% distinct(isin) %>% anti_join(sdc0_isins,by='isin') %>% select(isin)
sdc_isins<-sdc0_isins %>% full_join(df_sdcnew %>% filter(isin!='-') %>% distinct(isin) %>% select(isin),by='isin')
isin2figi<-sdc_isins %>% requestfigibyisin(.)
save(isin2figi,file='isin2figi.rdata')


# generate sdc comprehensive with isin and parsekeyables ------------------
load(file='isin2figi.rdata')
df_sdc0<-read.dta13('sdc96_clean2.dta') %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d) %>% filter(isin!='-')
df_sdc0 %<>% select(.,-starts_with("E",ignore.case=FALSE))
df_sdc0 %<>% distinct(deal_no)
load(file='sdcnew.rdata')
df_sdcnew %<>% filter(isin!='-')
df_sdcnew %<>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,PackageID=id_package_sdc,upnames=upco,issue_type_desc=typesec,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber)
df_sdcnew %<>% mutate(PackageID=as.numeric(PackageID))
parsekeyfigi<-read.csv(file='parsekeyablefigi.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% select(parsekeyable,figi)
parsekeyfigiisin<-parsekeyfigi %>% left_join(isin2figi,by='figi') %>% select(parsekeyable,figi,isin,everything())
#merge the two sdc files together
df_sdc0 %>% ds %>% sort()
(df_sdcnew %>% ds)[(df_sdcnew %>% ds) %ni% (df_sdc0 %>% ds)] %>% sort
df_sdcnew_add<-df_sdcnew %>% anti_join(df_sdc0,by='deal_no')
df_sdc_full<- df_sdc0 %>% full_join(df_sdcnew_add)

df_sdc_all<-df_sdc_full %>% left_join(isin2figi,by='isin') %>% left_join(parsekeyfigi,by='figi')
# there are some dups since isin to figi has dupes
(isin2figi %>% nrow)-(isin2figi %>% distinct(isin) %>% nrow)
# but this can be easily removed
df_sdc_all %>% distinct(deal_no)
save(df_sdc_all,file='sdc_all.rdata')

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

# what information we are missing in sdc ----------------------------------
# very little information is missing in sdc
df_sdc_raw<-read.dta13('sdc96_clean2.dta') %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
df_sdc <- df_sdc_raw %>% sample_frac(1)
bbgsrch_sdc<-bbgsrch %>% left_join(df_sdc,by='isin')

bbgsrch_sdc %>% ds
bbgsrch_sdc %>% distinct(isin) %>% tabulate('upnat') %>% View
bbgsrch_sdc %>% distinct(isin) %>% tabulate('mdy') %>% View
