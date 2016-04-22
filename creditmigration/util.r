library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)
require('readstata13')
require('ggfortify')
require('doBy')
require(lubridate)
require(ggplot2)
require(sandwich)
require(stargazer)
require(reshape2)
require(sqldf)
require(magrittr)
'%ni%' <- Negate('%in%')
df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))

icollapse <- function(dfin,ccyA="EUR",natA="Eurozone"){
  # dfin=df2
  # ccyA="EUR"
  # natA="Eurozone"
  # 
  # collapse into monthly aggregate flows
  df_fUSD<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat==natA, ccy=='USD') %>% 
    mutate(yrmo=as.numeric(format(d,'%Y%m')),ct=1) %>% 
    group_by(yrmo) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, IN_fUSD=sum(ct), nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_fUSD=amt,nrating_fUSD=nrating,ytofm_fUSD=ytofm)
  
  df_usF<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat=='United States', ccy==ccyA) %>% 
    mutate(yrmo=as.numeric(format(d,'%Y%m')),ct=1) %>% 
    group_by(yrmo) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, IN_usF=sum(ct),nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_usF=amt,nrating_usF=nrating,ytofm_usF=ytofm)
  
  dfjoint<-df_fUSD %>% full_join(.,df_usF,by='yrmo') 
  dfjoint[,c('I_usF','I_fUSD','IN_fUSD','IN_usF')]<-apply(dfjoint[,c('I_usF','I_fUSD','IN_fUSD','IN_usF')],2, function(x){replace(x,is.na(x),0)})
  dfout<-dfjoint %>% 
    mutate(I_net_fus=I_fUSD-I_usF,IN_fus=IN_fUSD-IN_usF) %>% 
    mutate(date=as.Date(str_c(yrmo,"01"),format="%Y%m%d"))
    # tidyr::gather(.,'type','value',-year) %>% 
    # filter(type %in% c('nrating_fUSD','nrating_usF')) %>%
    # filter(type %in% c('ytofm_fUSD','ytofm_usF')) %>%
    # filter(type %in% c('I_fUSD','I_usEUR','I_net_fus')) %>%   
   if (ccyA=="EUR") dfout<-filter(dfout,yrmo>200112)
   dfout
}
icollapsedaily <- function(dfin,ccyA="EUR",natA="Eurozone"){
  # collapse into daily aggregate flows
  df_fUSD<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat==natA, ccy=='USD') %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_fUSD=amt,nrating_fUSD=nrating,ytofm_fUSD=ytofm,date=d)
  
  df_usF<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat=='United States', ccy==ccyA) %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_usF=amt,nrating_usF=nrating,ytofm_usF=ytofm,date=d)
  
  dfout<-df_fUSD %>% full_join(.,df_usF,by='date') %>% 
    replace_na(list(I_fUSD=0,I_usF=0)) %>%     
    mutate(I_net_fus=I_fUSD-I_usF) %>% 
    select(date,I_net_fus,I_usF,I_fUSD)
  if (ccyA=="EUR") dfout<-filter(dfout,date>'2001-12-31')
  dfout
}
expandfulldates<-function(dfin){
  date.max<-max(dfin$date)
  date.min<-min(dfin$date)
  all.dates <- seq(date.min, date.max, by="month")
  all.dates.frame <- data.frame(list(date=all.dates))
  dfout<-dfin %>% full_join(.,all.dates.frame,by='date') %>% 
    replace(is.na(.),0) %>% 
    arrange(date)
  dfout
  #dfin
}

icollapse_all <- function(dfin){
df_euus<- dfin %>% icollapse(.,ccyA = "EUR",natA="Eurozone") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_euus=I_net_fus,IN_euus=IN_fus) %>% 
  expandfulldates(.)
df_gbus<- dfin %>% icollapse(.,ccyA = "GBP",natA="United Kingdom") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_gbus=I_net_fus,IN_gbus=IN_fus) %>% 
  expandfulldates(.)
df_jpus<- dfin %>% icollapse(.,ccyA = "JPY",natA="Japan") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_jpus=I_net_fus,IN_jpus=IN_fus) %>% 
  expandfulldates(.)
df_auus<- dfin %>% icollapse(.,ccyA = "AUD",natA="Australia") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_auus=I_net_fus,IN_auus=IN_fus) %>% 
  expandfulldates(.)

df_fus<- df_euus %>% 
  full_join(.,df_gbus,by='date') %>% 
  full_join(.,df_jpus,by='date') %>% 
  full_join(.,df_auus,by='date') %>% 
  arrange(date)
df_fus
}

plotgl <- function(dfin,fields=c('I_net_euus'),afteryr=2002){
  dfin %>% 
  filter(year>=afteryr) %>%
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()

}


ds<-function(dfin,matchpattern='.'){
  require(stringr)
  cn<-colnames(dfin) 
  try(cn[str_locate(as.character(cn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}

wgplot <- function(dfwide,idvar='date'){
  dfwide %>% 
  reshape2::melt(.,id.vars=idvar) %>%
  #filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line()+geom_point()
}


issfilter <- function(df_issraw){

  df_issraw  %>% 
  filter(amt>=50,ytofm>=2, ytofm<=99999,nrating<=16, (pub=="Public" | pub=="Sub."), 
         mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
         secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
         tf_mid_desc!='Government Sponsored Enterprises',
         nrating>1)
}


tabulate <- function(dfin,byvar='variable'){
  # dfin %>% group_by_(byvar) %>% dplyr::summarise_(count=length(byvar)) %>% arrange(desc(count))
  table(dfin[byvar]) %>% as.data.frame() %>%  tbl_df() %>% arrange(desc(Freq))
}