rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

raw<-read.dta13('sdc96_clean2.dta')
priceraw<-read.dta13('prices_extended.dta')

df1<-raw[!is.na(raw$amt),]
df1<-filter(df1,ccy %in% c('USD',"EUR",'AUD',"JPY",'GBP')) %>% dplyr::tbl_df()

# current 
df1  %>% 
  filter(amt>=50,ytofm>=2, ytofm<=99999,nrating<=16, (pub=="Public" | pub=="Sub."), 
         mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
         secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
         tf_mid_desc!='Government Sponsored Enterprises',
         nrating>1) %>% 
  icollapse_all(.) %>% full_join(.,priceraw,by='date') %>% regtemp(.)


regtemp<-function(dfreg){
  reg1<- dfreg %>% lm(diff(eubs10)~I_net_euus[-1],data=.)
  reg2<- dfreg %>% lm(diff(bpbs10)~I_net_gbus[-1],data=.)
  reg3<- dfreg %>% lm(diff(jybs10)~I_net_jpus[-1],data=.)
  reg4<- dfreg %>% lm(diff(adbs10)~I_net_auus[-1],data=.)
  stargazer(reg1,reg2,reg3,reg4,type='text',report="vct*")
}

df1 %>% dplyr::tbl_df() %>% 
  filter(amt>=50,ytofm>=5, ytofm<=15,nrating<=16, (pub=="Public" | pub=="Sub."), 
       mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
       secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
       tf_mid_desc!='Government Sponsored Enterprises',
       nrating>1) %>% 
  icollapse_all(.) %>% full_join(.,priceraw,by='date') %>% regtemp(.)






df2 %>% filter(modnat=='Eurozone', ccy=="USD",year==2002,month==9) %>% 
  select(i,amt, ccy) %>% 
  glimpse()

df2 %>% filter(modnat %in% c("Eurozone","United States"), ccy %in% c("EUR","USD"), foreign==1) %>% 
  select(amt,nrating,ytofm) %>% 
  summary()

##########
## seperate out by credit rating
################
df_euus<- icollapse(df2,ccyA = "EUR",natA="Eurozone") %>% 
  select(date,I_net_fus) %>% 
  expandfulldates(.)
df_euus %>% 
  full_join(.,priceraw,by='date') %>% 
  lm(lead(I_net_fus) ~ Cdif_euus_30_eff,data=.) %>%
  NeweyWest(.,lag=6) %>% 
  summary()


ratingdiv<-6
df_euus_HY<- df2 %>% filter(nrating>ratingdiv) %>%  icollapse(.,ccyA = "EUR",natA="Eurozone") %>% 
  select(date,I_net_fus) %>% rename(I_net_fus_HY=I_net_fus) 
df_euus_IG<- df2 %>% filter(nrating<=ratingdiv) %>%  icollapse(.,ccyA = "EUR",natA="Eurozone") %>% 
  select(date,I_net_fus) %>% rename(I_net_fus_IG=I_net_fus) 


df_euus %>% full_join(.,df_euus_HY,by='date') %>% 
  full_join(.,df_euus_IG,by='date') %>% 
  replace_na(list(I_net_fus_HY=0,I_net_fus_IG=0)) %>% 
  mutate(qrt=lubridate::quarter(date),year=year(date),month=month(date)) %>% 
  group_by(year,qrt) %>% 
  summarise(I_net_fus_HY=sum(I_net_fus_HY),I_net_fus_IG=sum(I_net_fus_IG)) %>% 
  mutate(date=as.Date(str_c(year,'-',3*qrt,'-','01'),format="%Y-%m-%d")) %>% 
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% c('I_net_fus_HY','I_net_fus_IG')) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()

#############
## regression
###
dfreg<-df_euus %>% full_join(.,priceraw,by='date') 


lm(I_net_fus ~ Cdif_euus_30_eff,data=as.data.frame(dfreg)) %>%
  # NeweyWest(.,lag=6) %>% 
  summary()


priceraw %>% filter(year>2001) %>% 
  select(date,Cdif_euus_30_eff,Cdif_euus_20_eff) %>% gggraph()
  

View(df_euus)

##################
## construct daily issuance flow data
## export to CSV for QE bootstrap
#################
df_euus<- icollapsedaily(df2,ccyA = "EUR",natA="Eurozone") 

df_euus %>% 
  replace_na(list(I_net_fus=0,I_fUSD=0,I_usF=0)) %>% 
  mutate(yyyymmdd=str_replace_all(as.character(date),"-","")) %>% 
  select(yyyymmdd,I_net_fus,I_fUSD,I_usF) %>% 
  write.csv(.,file='../QE/I_euus.csv')


############
# ploting rating changes
###########

df_euus %>% tidyr::gather(.,'type','value',-yrmo) %>% 
  mutate(date=as.Date(str_c(yrmo,"01"),format="%Y%m%d")) %>% 
  filter(type %in% c('nrating_fUSD','nrating_usF')) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()


gggraph<-function(dfin){
  dfin %>% tidyr::gather(.,'type','value',-date) %>% 
    # filter(type %in% c('nrating_fUSD','nrating_usF')) %>% 
    ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()
}

ggplot(data=.,aes(x=year,y=value,colour=type))+geom_line()+geom_point()
  

df_euUSD %>% 
    ggplot(data=.,aes(x=year,y=nrating_euUSD))+geom_line()
  
  
df_usEUR %>% 
  ggplot(data=.,aes(x=year,y=nrating))+geom_line()

  filter((ccy=="USD" | ccy=="EUR"),(modnat==nata | modnat==natb)) %>% 
  group_by(monthly,year, month, qrt,ccy,modnat) %>% 
  summarise(amt=sum(amt)) %>% 
  ungroup() %>% 
  mutate()
  glimpse()






raw<-read.dta13('sdc96_clean3.dta')
df1<-raw[!is.na(raw$amt),]
df1<-filter(df1,ccy %in% c('USD',"EUR",'AUD',"JPY"))




print ('U.S. Inflow')
df1 %>% dplyr::tbl_df() %>% 
  filter(modnat=='Eurozone', ccy=='USD',year==2014, ytofm<=99) %>% 
  select(i, amt, nrating,ytofm) %>% 
  mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
  summarise(amt=sum(amt), nrating=sum(nrating*weight),ytofm=sum(ytofm*weight))

print ('U.S. Outflow')
df1 %>% dplyr::tbl_df() %>% 
  filter(modnat=='United States', ccy=='EUR',year==2014, ytofm<=99) %>% 
  select(i, amt, nrating,ytofm) %>% 
  mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
  summarise(amt=sum(amt), nrating=sum(nrating*weight),ytofm=sum(ytofm*weight))


  

df1 %>% dplyr::tbl_df() %>% 
  filter(ytofm>50) %>% 
  group_by(modnat) %>% 
  summarise(amt=sum(amt)) %>% 
  orderBy(~-amt,.)


# globalissuers<-unique(filter(df1,foreign==1)$i)
# dfgi<-data.frame(i=globalissuers,gi=1)
# df2<-merge(df1,dfgi,by='i',all=TRUE)
# df2[is.na(df2$gi),]$gi=0
# write.dta(df2,'sdc96_clean2.dta')

dfs<-summaryBy(amt~ccy+foreign,data=df1,FUN=sum)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~modupnat+foreign,data=df1,FUN=sum)
orderBy(~-foreign-amt.sum,dfs)

dfs5<-summaryBy(amt~foreign+year,data=df1,FUN=sum)
dfs6<-reshape(dfs5,timevar='foreign',idvar='year',direction='wide')
dfs6$total<-(dfs6$amt.sum.0+dfs6$amt.sum.1)
dfs6$foreignp<-dfs6$amt.sum.1/(dfs6$amt.sum.0+dfs6$amt.sum.1)
dfs6

# foreign share by secur type
dfs<-summaryBy(amt~secur+foreign,data=filter(df1),FUN=c(sum,length),order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
dfs3<-reshape(dfs2,timevar='foreign',idvar='secur',direction='wide')
str(dfs3)
dfs4<-orderBy(~-amt.sum.0-amt.sum.1,dfs3)
dfs4$amtforeignP<-dfs4$amt.sum.1/(dfs4$amt.sum.1+dfs4$amt.sum.0)
dfs4$nforeignP<-dfs4$amt.length.1/(dfs4$amt.length.1+dfs4$amt.length.0)
df2clip(dfs4)




dfs<-summaryBy(amt~foreign+secur,data=filter(df1),FUN=c(sum,function(x) quantile(x,c(.1,.5,.9),na.rm=TRUE)),order=TRUE)
dfs
dfs2<-orderBy(~foreign,dfs)
dfs2



dfs<-summaryBy(amt~foreign+ccy,data=filter(df1),FUN=c(sum,function(x) quantile(x,c(.1,.5,.9),na.rm=TRUE)),order=TRUE)
dfs
dfs2<-orderBy(~ccy,dfs)
dfs2



dfs<-summaryBy(nrating~foreign+ccy,data=filter(df1),FUN=c(mean,median, function(x) quantile(x,c(.1,.9))),order=TRUE)
dfs
dfs2<-orderBy(~foreign+ccy,dfs)
dfs2

dfs<-summaryBy(amt~nrating+ccy,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~ccy,dfs)
dfs2
df2clip(dfs2)

dfs<-summaryBy(amt~nrating+foreign,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~foreign+nrating-amt.sum,dfs)

dfs<-summaryBy(amt~nrating+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~nrating-amt.sum,dfs)

dfs<-summaryBy(amt~mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-amt.sum,dfs)

dfs<-summaryBy(amt~mdy+sp,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
df2clip(dfs2)



dfs<-summaryBy(ytofm~sp,data=filter(df1,!(is.na(df1$ytofm))),FUN=c(mean,median),order=TRUE)
dfs

dfs2<-orderBy(~-amt.sum,dfs)
dfs2


summary(filter(df1,!(is.na(df1$ytofm)))$ytofm)


dfs<-summaryBy(amt~sp,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
df2clip(dfs2)


dfs<-summaryBy(amt~foreign+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)


dfs<-summaryBy(amt~foreign+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~foreign+btypc,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~foreign+gi,data=filter(df2),FUN=sum,order=TRUE)
dfs



dfs<-summaryBy(amt~i,data=filter(df1,pub=="Govt." & foreign==1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)




View(dfs2)
dfs<-summaryBy(amt~pub+sic1,data=df1,FUN=sum,order=TRUE)
dfs

dfs<-summaryBy(amt~pub+secur+foreign,data=df1,FUN=sum,order=TRUE)
dfs2<-orderBy(~pub-amt.sum,data=dfs)

filter(dfs2,pub=='Public',foreign==1)


dfs<-summaryBy(amt~pub+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~pub,data=dfs)

dfs<-summaryBy(amt~ccy,data=df1,FUN=sum,order=TRUE)
orderBy(~-amt.sum,data=dfs)
sum(dplyr::filter(dfs,ccy %in% c('USD',"EUR",'AUD',"JPY"))$amt.sum)/sum(dfs$amt.sum)


dfs<-summaryBy(amt~ccy+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,data=dfs)


dfs<-summaryBy(amt~modnat,data=df1,FUN=sum,order=TRUE)
orderBy(~-amt.sum,data=dfs)


dfs<-summaryBy(amt~modnat+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,data=dfs)


dfs<-summaryBy(amt~sp,data=df1,FUN=sum,order=TRUE)
as.numeric(filter(dfs,sp==""))/sum(dfs$amt.sum)

dfs<-summaryBy(amt~mdy,data=df1,FUN=sum,order=TRUE)
as.numeric(filter(dfs,mdy==""))/sum(dfs$amt.sum)

dfs<-summaryBy(amt~sp+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign,data=dfs)
as.numeric(filter(dfs,sp=="" & foreign==1)[,3])/sum(filter(dfs,foreign==1)$amt.sum)

orderBy(~-amt.sum,data=dfs)

# what's going on with aud
df2<-


