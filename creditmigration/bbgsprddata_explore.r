rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
require(sqldf)

load('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')
df_yld<-unlist(prices, recursive=FALSE) %>% do.call(rbind.data.frame,.) %>%  
  mutate(ticker=str_extract(rownames(.),'^.*(?=(\\.))')) %>% filter(ticker!="NA")
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

### MERGE WITH SDC
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
df_obs %>% View
# compare to number of expected obs by isin
df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View

# merging
df_bond<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.d,B.mdealtype,B.amt,B.pub,B.issue_type_desc from df_yld2 as A, df_sdc2 as B where A.isin=B.isin')
df_bond2<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.mdealtype from df_yld2 as A left join df_sdc2 as B on A.isin=B.isin')

nrow(df_yld2)
nrow(df_bond)
df_bond %>% tabulate(.,'isin') %>% View

# matching to newly downloaded sdc isins
sdc_raw<-read.csv('../data/sdc/sdc_bbgmatch.csv',stringsAsFactors = FALSE)
df_sdcnew<-sdc_raw %>% mutate(d=mdy(d),mat2=mdy(mat2),amt=as.numeric(amt),ytofm=as.numeric(ytofm)) %>% tbl_df() %>% 
  select(issname,ticker_sdc,isin,cusip,d,nat,amt,descr,ccy,mat2,ytofm,everything()) %>% arrange(d)
df_sdcn2<-df_sdcnew[!duplicated(df_sdcnew$isin),] %>% filter(isin!='-')  %>% arrange(isin)

df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2 from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')
df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2, B.issname, B.mdealtype from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')

df_sdcn2 %>% ds

nrow(df_yld2)
nrow(df_bond)
nrow(df_bond3)
df_bond3 %>% tabulate(.,'isin') %>% View

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
 # need to merge with nrating and filter on it
anti_join(df_sdcn2,df_yld2,by='isin') %>% filter(ccy!='gbp',rating_sp!="NR",rating_mdy!='NR') %>% xtabs(~typesec,data=.) %>% as.data.frame() %>% View
df_sdcn2 %>% xtabs(~typesec,data=.) %>% as.data.frame() %>% View
 # explore on bbg why these are missing




#######
df_bond %>% xtabs(~ccy+mdealtype,data=.)
df_bond %>% tabulate(.,'isin')

Hmisc::describe(df_sdcnew$mktplace)

# out of the ones that are matched, what's the data coverage?
# very good price coverage










df_bond %>% ds

df




df_bond %>% mutate(datestr=as.character(date),matstr=as.character(mat2)) %>% select(-date,-mat2,-fac) %>% 
  write.dta(.,'bondsprdpanel.dta')




summary(df_bond)

df_yld2 %>% View
df_bond %>% View

df_sdc %>% ds(.,'mat')
df_sdc %>% filter(isin=='1312630') %>% View
df_sdc2 %>% filter(isin=='1312630') %>% View






###### explore yields
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


