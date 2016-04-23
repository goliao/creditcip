setwd('/Users/gliao/Dropbox/Research/ccy basis/')
require(tidyr)
require(dplyr)
require(xts)

load('data/bloomberg/bbg_T_all.RData')

bondinfo<-bondinfo %>% mutate(ticker=rownames(.))
df1<-left_join(price,bondinfo,by="ticker") %>% dplyr::tbl_df(.)

df1 %>% View()

dfmedian <- df1 %>% group_by(date,CRNCY) %>% 
  summarise(yield=median(na.omit(YLD_YTM_MID))) %>% 
  mutate(d=as.Date(date,format="%Y-%m-%d")) 

df_usd<-dfmedian %>% filter(CRNCY=="USD")
df_eur<-dfmedian %>% filter(CRNCY=="EUR") 

xdf_usd<-as.xts(df_usd$yield,order.by=df_usd$d)
xdf_eur<-as.xts(df_eur$yield,order.by=df_eur$d)

xdf<-merge(xdf_usd,xdf_eur)

xdf %>% View(.)

plot(xdf)



xdfmedian




df_usd <- df1  %>% filter(CRNCY=="USD") %>% 
  select(date,ticker,YLD_YTM_MID) %>% 
  tidyr::spread(.,ticker,YLD_YTM_MID)  

df_eur <- df1  %>% filter(CRNCY=="EUR") %>% 
  select(date,ticker,YLD_YTM_MID) %>% 
  tidyr::spread(.,ticker,YLD_YTM_MID)  

