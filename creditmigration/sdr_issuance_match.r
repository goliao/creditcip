rm(list=ls())
setwd("/Users/gliao/Dropbox/Research/ccy basis/data/bbg0417_sdr")
load(file='sdr.rdata')
require(stringr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)

df_sdr_eur<-
  df_sdr %>% dplyr::tbl_df() %>%
  mutate(ytofm=as.numeric(str_sub(T,1,str_length(T)-1)),Tfwd=as.numeric((effective-tradedate)/365)) %>% 
  dplyr::filter(Curr.1=="EUR",ytofm>=1) 

#proxy arb activities by fwd starting currency swaps
df_sdr_eur %>% View
dfarb<-df_sdr_eur %>% mutate(fwdstart=(Tfwd>.5), year=lubridate::year(tradedate), month=lubridate::month(tradedate)) %>% 
  group_by(year, month,fwdstart) %>% dplyr::summarise(tdcount=length(usdnot2)) %>% 
  mutate(date=lubridate::ymd(str_c(year,'-',month,'-',1))) %>% ungroup() %>%  select(fwdstart,tdcount,date) %>% mutate(fwdstart=tolower(fwdstart)) %>% 
  reshape2::dcast(.,date~fwdstart,value.var = "tdcount") %>% rename(spot=false,fwd=true) %>% mutate(arbratio=fwd/(spot+fwd))
# graph arb ratio
dfarb %>% ggplot(.,aes(x=date,y=arbratio))+geom_line()


df_sdr_eur %>% 
  filter(tradedate<=ymd('2014-11-30'),tradedate>=ymd('2014-11-15')) %>% 
  filter(ytofm>=14,ytofm<=16) %>% 
  filter(not1plus==1 | usdnot2>900) %>% 
  arrange(desc(tradedate)) %>% 
  View

df_sdr_eur %>% ggplot(.,aes(x=usdnot2))+geom_density()
df_sdr_eur %>% mutate(not1plus=as.character(not1plus)) %>%  ggplot(.,aes(x=usdnot2,colour=not1plus))+geom_density()
df_sdr_eur %>% ggplot(.,aes(x=usdnot2,colour=as.character(not1plus)))+stat_ecdf()

df_sdr_eur %>% View
df_sdr_eur %>% ggplot(.,aes(x=usdnot2,colour=Type))+stat_ecdf()

summary(df_sdr_eur$usdnot2)
df_sdr_eur %>% group_by(not1plus) %>% summarise(mx=max(usdnot2),mn=min(usdnot2))


ishedged<-function(date=ymd('2014-11-20'),mat=15,notl=1000,ccy="EUR",daterange=5,notlrange=.1,matrange=.5){
  swaps<-df_sdr_eur %>%  filter(tradedate<=date+daterange,tradedate>=date-daterange,
                         ytofm>=(mat-matrange), ytofm<=(mat+matrange),
                         not1plus==1 | (usdnot2>=notl*(1-notlrange) & usdnot2<=notl*(1+notlrange)),
                         Tfwd<=.02) 
  swaps %>% View
  nrow(swaps)
}
ishedged()


ishedged_exact<-function(date=ymd('2014-11-20'),mat=ymd('2019-06-04'),notl=1000,ccy="EUR",daterange=5,notlrange=.1){
  swaps<-df_sdr_eur %>%  filter(tradedate<=date+daterange,tradedate>=date-daterange,
                                maturity==mat,
                                not1plus==1 | (usdnot2>=notl*(1-notlrange) & usdnot2<=notl*(1+notlrange)),
                                Tfwd<=.5) 
  #swaps %>% View
  nrow(swaps)
}
ishedged_exact()

setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
raw<-read.dta13('sdc96_clean3.dta')
df1<-raw[!is.na(raw$amt),] %>% dplyr::tbl_df()
colnames(df1)

# exact match based on maturity date
dfius_test<-df1 %>% filter(modnat=="United States",ccy=="EUR") %>% select(i,ytofm,amt,d,secur,descr,mkt,mat2) %>% filter(d>ymd('2014-06-01'),d<ymd('2015-03-01')) %>% filter(amt>=75)
dfhedge<-dfius_test %>% rowwise() %>%  mutate(hedges=ishedged_exact(date=d,mat=mat2,notl=amt,daterange=2)) %>% ungroup()
dfhedge %>% ggplot(.,aes(x=hedges))+stat_ecdf()
dfhedge %>% group_by(hedges) %>% summarise(ct=length(hedges)) %>% mutate(freq=ct/nrow(dfhedge))

dfhedge %>% View
# graph hedging ratio by quarter; shows a flury of activities in total amt of swap traded in 2014q4, which corresponds to swap volume
dfhedge %>% mutate(hedged=(hedges>0),quarter=lubridate::quarter(d,TRUE)) %>% group_by(hedged,quarter) %>% summarise(amtsum=sum(amt)) %>% ungroup() %>% 
  mutate(hedged=tolower(as.character(hedged))) %>% dcast(.,quarter~hedged,value.var='amtsum') %>% rename(hamt=true,unhamt=false) %>% mutate(hratio=hamt/(hamt+unhamt)) 
%>% ggplot(.,aes(x=quarter,y=hratio))+geom_line()
  

#fake exact match
dfius_test<-df1 %>% filter(modnat=="Eurozone",ccy=="EUR") %>% select(i,ytofm,amt,d,secur,descr,mkt,mat2) %>% filter(d>ymd('2014-06-01'),d<ymd('2015-03-01')) %>% filter(amt>=75)
dfhedge<-dfius_test %>% rowwise() %>%  mutate(hedges=ishedged_exact(date=d,mat=mat2,notl=amt,daterange=2))
dfhedge %>% ggplot(.,aes(x=hedges))+stat_ecdf()
dfhedge %>% group_by(hedges) %>% summarise(ct=length(hedges)) %>% mutate(freq=ct/nrow(dfhedge))


# rough match based on ytofm
dfius_test<-df1 %>% filter(modnat=="United States",ccy=="EUR") %>% select(i,ytofm,amt,d,secur,descr,mkt,maturity) %>% filter(d>ymd('2014-06-01'),d<ymd('2015-03-01')) %>% filter(amt>=75)
dfhedge<-dfius_test %>% rowwise() %>%  mutate(hedges=ishedged(date=d,mat=ytofm,notl=amt,matrange=.25,daterange=5))
dfhedge %>% ggplot(.,aes(x=hedges))+stat_ecdf()

dfius_test %>% filter(d>ymd('2014-11-19'),d<=ymd('2014-11-30')) %>% View

# fake
dfius_test2<-df1 %>% filter(modnat=="Japan",ccy=="JPY") %>% select(i,ytofm,amt,d,secur,descr,mkt) %>% filter(d>ymd('2014-06-01'),d<ymd('2015-03-01')) %>% filter(amt>=75)
dfius_test2 %>% rowwise() %>%  mutate(hedges=ishedged(date=d,mat=ytofm,notl=amt,matrange=.2,daterange=1)) %>% ggplot(.,aes(x=hedges))+stat_ecdf()





dfhedge %>% View
