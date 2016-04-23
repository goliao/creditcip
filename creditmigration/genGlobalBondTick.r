# Generate global bond tickers/isins
rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# BBG generated -----------------------------------------------------------
# takes in bbg tiker and ultimate parent info and spits out global bonds tickers
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
# bbg_isin_req_euus <- df_sdc_generous %>%
#   filter(amt>=100, ytofm>=2,pub!='Govt.',ccy %in% c('EUR', 'USD') , isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print


