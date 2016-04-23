rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
require(sqldf)
# load bbg bond sprd, unpack ----------------------------------------------
load('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')
a0 <- unlist(prices, recursive = FALSE)
tickernames <- names(a0)
df_prices <- data.frame() %>% tbl_df()
for (i in 1:length(tickernames)) {
  temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
  if (nrow(temp_new) == 0)
    print (str_c('empty:#', i, ' name:', tickernames[i]))
  df_prices <- df_prices %>% dplyr::bind_rows(., temp_new)
}
df_yld<-df_prices

# df_yld_long<-df_yld %>% select(-BLP_CDS_BASIS_MID) %>%  gather(key = 'field',value='value',-date,-ticker) %>% dplyr::tbl_df()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

# add ccy
df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
# df_yld_long<-df_yld2 %>% select(-BLP_CDS_BASIS_MID) %>%  
#   melt(.,id.vars=c('date','ticker','crncy'),
#        measure.vars=c("OAS_SPREAD_BID","BLP_Z_SPRD_MID","BLP_ASW_SPREAD_MID","BLP_I_SPRD_MID","BLP_Z_SPRD_LAST","BLP_ASW_SPREAD_LAST","BLP_I_SPRD_LAST"),
#        variable.name='field') %>% 
#   dplyr::tbl_df()
# #plot available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()

#View available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(OAS_SPREAD_BID))) 
# plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(BLP_I_SPRD_MID))) 
# # plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# # plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()


# Merge with sdc data main ------------------------------------------------

raw<-read.dta13('sdc96_clean2.dta')
df_sdc<-raw %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
# filter and augment with expected number of monthly observations
df_sdc2<-df_sdc[!duplicated(df_sdc$isin),] %>% filter(isin!='-')  %>%
  arrange(isin)  %>% 
  filter(mat2>'2005-01-01') %>% 
  mutate(expmonthlyobs=ceiling((pmin(as.numeric(ymd('2016-04-01')),as.numeric(mat2))-
                                  pmax(as.numeric(settlement2),as.numeric(ymd('2005-01-01'))))/30.5)) 
  
# count number of observations by isin
df_obs<-df_yld2 %>% group_by(isin) %>% summarise(.,ct=length(isin)) %>% arrange(desc(ct))
# compare to number of expected obs by isin
df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
# df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View

# merging
df_bond<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.d,B.mdealtype,B.amt,B.pub,B.issue_type_desc from df_yld2 as A, df_sdc2 as B where A.isin=B.isin')
df_bond2<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.mdealtype from df_yld2 as A left join df_sdc2 as B on A.isin=B.isin')

# merge with sec new ------------------------------------------------------
# matching to newly downloaded sdc isins
sdc_raw<-read.csv('../data/sdc/sdc_bbgmatch.csv',stringsAsFactors = FALSE)
rating_sp<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_sp=rating,nrating_sp=nrating)
rating_mdy<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_mdy=rating,nrating_mdy=nrating)
df_sdcnew<-sdc_raw %>% mutate(d=mdy(d),mat2=mdy(mat2),amt=as.numeric(amt),ytofm=as.numeric(ytofm)) %>% tbl_df() %>% 
  left_join(.,rating_sp, by='rating_sp') %>% left_join(.,rating_mdy,by='rating_mdy') %>% 
  select(issname,ticker_sdc,isin,cusip,d,nat,amt,descr,ccy,mat2,ytofm,everything()) %>% arrange(d)
df_sdcn2<-df_sdcnew[!duplicated(df_sdcnew$isin),] %>% filter(isin!='-')  %>% arrange(isin)

df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2 from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')
df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2, B.issname, B.mdealtype from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')


# explore what's missing in which dataset ---------------------------------
# what's missing compare to earlier merge
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>%  group_by(i) %>% top_n(.,1,desc(date)) %>% dplyr::arrange(pub) %>% select(1,10:26) %>% View
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>%  xtabs(~pub+mdealtype+issue_type_desc,data=.)
  # so a lot of bonds are excluded due to dealtypes, gov, R144D. but still has some that's unclear why was excluded
   anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>% filter(issue_type_desc=='Investment Grade Corporate',mdealtype!='R144D') %>% View

# what's added compare to earlier merge
anti_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's the joint between new and old merge
semi_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's not captured in the sdc data but showed up in bloomberg bsrch
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% View
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% select(ticker:crncy) %>% write.csv(.,file='GetBBGBondData.csv')

#what's covered in sdc but not in bloomberg?
bbgmiss<-anti_join(df_sdcn2,df_yld2,by='isin') %>% filter(ccy!='gbp',nrating_sp<=10,nrating_mdy<=10,nrating_sp!=0,nrating_mdy!=0,amt>=100,ytofm>2,ytofm<50,
                                                 !grepl('Flt',typesec),!grepl('Zero Cpn',typesec),!grepl('Float',typesec),!grepl('Fl',descr),!grepl('Zero Cpn',descr)) 
bbgmiss 
bbgmiss  %>% xtabs(~typesec,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~mktplace,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~upsic,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss %>% View
bbgmiss %>% tabulate(.,'issname') %>% View
bbgmiss %>% write.csv(.,file='GetBBGTickerPrice.csv')

#######
df_bond %>% xtabs(~ccy+mdealtype,data=.)
df_bond %>% tabulate(.,'isin')
Hmisc::describe(df_sdcnew$mktplace)

# out of the ones that are matched, what's the data coverage?
# very good price coverage

df_bond %>% mutate(datestr=as.character(date),matstr=as.character(mat2)) %>% select(-date,-mat2,-fac) %>% 
  write.dta(.,'bondsprdpanel.dta')

# openfigi ----------------------------------------------------------------


# curl -v -X POST 'https://api.openfigi.com/v1/mapping'                 \
# --header 'Content-Type: text/json'                           \
# --header 'X-OPENFIGI-APIKEY: abcdefghijklmnopqrstuvwxyz'     \
# --data '[{"idType":"ID_WERTPAPIER","idValue":"851399"}]'
# 
# curl -v -X POST 'https://api.openfigi.com/v1/mapping'   \
# --header 'Content-Type: text/json'             \
# --data '[{"idType":"ID_WERTPAPIER","idValue":"851399","exchCode":"US"}]'

require('magrittr')
require('httr')

figireq<-'[{"idType":"ID_WERTPAPIER","idValue":"851399","exchCode":"US"},
          {"idType":"ID_BB_UNIQUE","idValue":"JK354407"},
          {"idType":"ID_BB","idValue":"JK354407"},
          {"idType":"COMPOSITE_ID_BB_GLOBAL","idValue":"JK354407"},
          {"idType":"TICKER","idValue":"JK354407 Corp"}]'

r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
r
content(r)
content(r)[[1]][[1]][[1]] %>% as.data.frame() %>% View
unlist(content(r)[[1]],recursive = FALSE) 

t1<-as.data.frame(content(r)[[1]][[1]][[1]])
for (i in 2:134){
  t1<-rbind(t1,content(r)[[1]][[1]][[i]] %>% as.data.frame())
}
t1 %>% View
  
content_type()
content_type_json()
add_headers(`Content-Type`='text/json')

content_type_json() %>% str
content_type_json()[1]

# explore yield -----------------------------------------------------------

load('../data/bloomberg/bbg_gbonds_160413.rdata')

df_yld<-unlist(prices, recursive=FALSE) %>% do.call(rbind.data.frame,.) %>%
  mutate(ticker=str_extract(rownames(.),'^.*(?=(\\.))')) %>% filter(ticker!="NA")

df_yld2<-left_join(df_yld,globalbonds,by='ticker')

agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(YLD_YTM_MID))) 

# plot yields for eur, usd
agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
  gather(.,key='type',value='yield',-date) %>% 
  filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

df_yldw<-df_yld %>% dcast(.,date~ticker,value.var="yield")
df_yldw %>% summarise_each(.,funs(length(is.na())))

# describe the number of data points missing
missings<-colSums(is.na(df_yldw[,-1])) %>% as.data.frame() 
colnames(missings)<-'Nmissings'
missings$ticker=rownames(missings)
summary(missings)
missings %>% ggplot(.,aes(x=Nmissings))+geom_density()
# describe number of data points avaialbe for each currency at each time
df_yld2 %>% group_by(date, crncy) %>% summarise(Ndp=length(ticker)) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=crncy))+geom_line()



# Demeaned spread ---------------------------------------------------------

df_bond2<-df_bond %>% rename(oas=OAS_SPREAD_BID,upco=id_bb_ultimate_co) %>% select(-fac)
df_oas_issmean<-df_bond2 %>% group_by(date,upco) %>% summarise(oas_issmean=mean(oas))
# demeaned oas spread for each bond; demeaning issuer*time specific means
df_bond_de<-sqldf('select A.*, B.oas_issmean from df_bond2 as A left join df_oas_issmean as B on (A.date==B.date and A.upco==B.upco)') %>%
  tbl_df() %>% mutate(oas_res=oas-oas_issmean) %>% select(date,oas, oas_issmean, oas_res,upco,ccy) %>% arrange(date) 
# residual oas spread between eur and usd, issuer matched
df_ccyres<-df_bond_de %>% group_by(date,ccy) %>% summarise(mean_oas_res=mean(na.omit(oas_res))) %>% mutate(ccy=tolower(ccy)) %>% 
  dcast(.,date~ccy,value.var='mean_oas_res') %>% tbl_df() %>% mutate(euus_sprd=eur-usd)
df_ccyres %>% melt(.,id.vars='date') %>% filter(variable=='euus_sprd') %>% 
  ggplot(.,aes(x=date,y=value)) +geom_line()

priceraw<-read.dta13('prices_extended.dta')

# adjusting for xccy
df_price<-df_ccyres %>% full_join(.,priceraw,by='date') %>% mutate(oas_res_eff=euus_sprd-eubs5) %>% 
  select(date,oas_res_eff,euus_sprd) %>% filter(date>'2006-01-01') %>% wgplot(.)
# filter issuance 
df_reg_data<- df_sdc %>% as.data.frame() %>%  issfilter(.) %>% icollapse_all(.) %>% full_join(.,df_price,by='date')
df_reg_data %>% tail()

df_reg_data %>% 
  lm(I_net_euus~oas_res_eff,data=.)

regtemp<-function(dfreg){
  reg10_1<- dfreg %>% lm(I_net_euus~Cdif_euus_30,data=.)
  stargazer(reg10_1,type='text',report="vct*")
}



df_bond_de %>% filter(date=='2005-12-30') %>% View  
nrow(df_bond2)
nrow(df_oas_issmean)
nrow(df_bond_de)

