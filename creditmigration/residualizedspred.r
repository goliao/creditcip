rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
require(sqldf)

# explore yields
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





rm(list=ls(all=TRUE))
source('util.r')
load('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')
df_yld<-unlist(prices, recursive=FALSE) %>% do.call(rbind.data.frame,.) %>%  
  mutate(ticker=str_extract(rownames(.),'^.*(?=(\\.))')) %>% filter(ticker!="NA")
df_yld %>% tbl_df()
df_yld_long<-df_yld %>% select(-BLP_CDS_BASIS_MID) %>%  gather(key = 'field',value='value',-date,-ticker) %>% dplyr::tbl_df()
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

# add ccy
df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
df_yld_long<-df_yld2 %>% select(-BLP_CDS_BASIS_MID) %>%  
  melt(.,id.vars=c('date','ticker','crncy'),
  measure.vars=c("OAS_SPREAD_BID","BLP_Z_SPRD_MID","BLP_ASW_SPREAD_MID","BLP_I_SPRD_MID","BLP_Z_SPRD_LAST","BLP_ASW_SPREAD_LAST","BLP_I_SPRD_LAST"),
  variable.name='field') %>% 
      dplyr::tbl_df()
#plot available data pionts for US and EU
df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()

#View available data pionts for US and EU
df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View






agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(OAS_SPREAD_BID))) 
# plot yields for eur, usd
agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
  gather(.,key='type',value='yield',-date) %>% 
  filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()


agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(BLP_I_SPRD_MID))) 
# plot yields for eur, usd
agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
  gather(.,key='type',value='yield',-date) %>% 
  filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

### MERGE WITH SDC
raw<-read.dta13('sdc96_clean2.dta')
df_sdc<-raw %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
df_sdc2<-df_sdc[!duplicated(df_sdc$isin),] %>% filter(isin!='-')  %>%
  arrange(isin) 


df_bond<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm from df_yld2 as A, df_sdc2 as B where A.isin=B.isin')
require(foreign)

df_bond %>% mutate(datestr=as.character(date),matstr=as.character(mat2)) %>% select(-date,-mat2,-fac) %>% 
  write.dta(.,'bondsprdpanel.dta')




summary(df_bond)

df_yld2 %>% View
df_bond %>% View

df_sdc %>% ds(.,'mat')
df_sdc %>% filter(isin=='1312630') %>% View
df_sdc2 %>% filter(isin=='1312630') %>% View
