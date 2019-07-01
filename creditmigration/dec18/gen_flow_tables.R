source('../util.r')
require(xlsx)
#credit.cip.exact[,ccy:=factor(ccy)] %>% write.dta('temp.dta')
dtcreditcip.m<-copy(credit.cip.exact)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]

dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg

# Monthly 6 month avg forward
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)

dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]



# issuance flow and net dev -----------------------------------------------
# still need to floor month to get date at the begining of the month
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtcreditcip.m[date<'2016-08-01']
dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
# dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()

# Collapse Monthly 6 month avg forward
  tmp<-list()
  for(iccy in c('eur','gbp','jpy','aud','chf','cad')) {
    tmp[[length(tmp)+1]]<-dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
  }; 
  dtiss.collapse.m <- tmp %>% rbindlist()
  dtiss.collapse.m %>% setkey(date,ccy)
  
  dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
  dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
  
  ## make issuance lead by one period
  dtreg.m[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
  dtreg.m[,i_netflow6mf:=(shift(i_netflow,n=1,type='lead')+shift(i_netflow,n=2,type='lead')+shift(i_netflow,n=3,type='lead')+shift(i_netflow,n=4,type='lead')+shift(i_netflow,n=5,type='lead')+shift(i_netflow,n=6,type='lead'))/6,ccy]
  
  dtreg.m[,F.I_netflow:=shift(I_netflow,n=1,type='lead'),ccy]
  dtreg.m[,I_netflow6mf:=(shift(I_netflow,n=1,type='lead')+shift(I_netflow,n=2,type='lead')+shift(I_netflow,n=3,type='lead')+shift(I_netflow,n=4,type='lead')+shift(I_netflow,n=5,type='lead')+shift(I_netflow,n=6,type='lead'))/6,ccy]

# Monthly regression with 6 month forwards
  reg_issnetdev<-list()
  for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
    reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~netmisp,value.name=iccy)
  }
  reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip
  reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %>% write.xlsx(file='tables.xlsx',sheetName='flow_netdev1',append=TRUE)
  
  ## credit and cip seperated effect
  dtreg.m[,cip:=credit-netmisp]
  reg_issnetdev<-list()
  for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
    reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~credit+cip,value.name=iccy)
  }
  reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip
  reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %>% write.xlsx(file='tables.xlsx',sheetName='flow_devs',append=TRUE)
  
  
  # adding swap rate control
  swap.rate<-prl[monthend==1 & date>=ymd('2004-01-01')][ticker %like% '^\\w\\wsw5$' | ticker %like% '^eusa5$',.(date,ticker,value=value*100)]
  swap.rate[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf']
  swap.rate<-swap.rate[ccy %in% c('eur','gbp','jpy','aud','chf','cad','usd'),.(date,ccy,swaprate=value)]
  swap.rate[,swapraterel:=swaprate-.SD[ccy=='usd',swaprate],date]
  swap.rate<-swap.rate[ccy!='usd'][,date:=floor_date(date,'month')][,.(date,ccy,swapraterel)] %>% setkey(date,ccy)
  
  dtreg.m<-swap.rate[dtreg.m]
  reg_issnetdev<-list()
  for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
    reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~netmisp+swapraterel,value.name=iccy)
  }
  reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip

# pooling monthly 6 month avg forward 
## Pooling by AGGREGATING issuance of all debt in EUR, GBP, JPY, AUD
  CCY4=c('eur','gbp','jpy','chf','aud','cad')
  dtin3<-dtin2 %>% copy()
  dtin3[ccy %in% CCY4,ccyA:='eur']
  dtiss.collapse.m<-dtin3 %>% icollapse4(.,'eur',collapse.freq = 'month',filter=0)
  dtiss.collapse.m %>% setkey(date,ccy)
  dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
  dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
  ## make issuance lead by one period
  dtreg.m[,.N,ccy]
  dtreg.m[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
  dtreg.m[,i_netflow6mf:=(shift(i_netflow,n=1,type='lead')+shift(i_netflow,n=2,type='lead')+shift(i_netflow,n=3,type='lead')+shift(i_netflow,n=4,type='lead')+shift(i_netflow,n=5,type='lead')+shift(i_netflow,n=6,type='lead'))/6,ccy]
  dtreg.m[,F.I_netflow:=shift(I_netflow,n=1,type='lead'),ccy]
  dtreg.m[,I_netflow6mf:=(shift(I_netflow,n=1,type='lead')+shift(I_netflow,n=2,type='lead')+shift(I_netflow,n=3,type='lead')+shift(I_netflow,n=4,type='lead')+shift(I_netflow,n=5,type='lead')+shift(I_netflow,n=6,type='lead'))/6,ccy]
  # adding swap rate control
  swap.rate<-prl[monthend==1 & date>=ymd('2004-01-01')][ticker %like% '^\\w\\wsw5$' | ticker %like% '^eusa5$',.(date,ticker,value=value*100)]
  swap.rate[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf']
  swap.rate<-swap.rate[ccy %in% c('eur','gbp','jpy','aud','chf','cad','usd'),.(date,ccy,swaprate=value)]
  swap.rate[,swapraterel:=swaprate-.SD[ccy=='usd',swaprate],date]
  swap.rate<-swap.rate[ccy!='usd'][,date:=floor_date(date,'month')][,.(date,ccy,swapraterel)] %>% setkey(date,ccy)
  dtreg.m<-swap.rate[dtreg.m]

### Monthly regression with 6 month forwards, need to average net deviation
  dtreg.m[,.N,ccy]
  est_issnetdev<-dtreg.m[ccy %in% CCY4] %>% neweymod(i_netflow6mf~netmisp,value.name='full')
  est_issnetdevpre<-dtreg.m[ccy %in% CCY4 & date<ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp,value.name='pre')
  est_issnetdevpost<-dtreg.m[ccy %in% CCY4 &date>=ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp,value.name='post')
  estnet=rbindlist(list(est_issnetdev,est_issnetdevpre,est_issnetdevpost))
  estw=(estnet %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  estw
  ## seperated
  est_issnetdev<-dtreg.m[ccy %in% CCY4] %>% neweymod(i_netflow6mf~cip+credit,value.name='full')
  est_issnetdevpre<-dtreg.m[ccy %in% CCY4 & date<ymd('2009-01-01')] %>% neweymod(i_netflow6mf~cip+credit,value.name='pre')
  est_issnetdevpost<-dtreg.m[ccy %in% CCY4 & date>=ymd('2009-01-01')] %>% neweymod(i_netflow6mf~cip+credit,value.name='post')
  est=rbindlist(list(est_issnetdev,est_issnetdevpre,est_issnetdevpost))
  estw=(est %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  estw
  
  estnet[,colorder:=1]
  est[,colorder:=2]
  estprepost=(rbindlist(list(estnet,est)) %>% dcast(order+rn+variable~varname+colorder))[order(order,rn,variable)][,.(rn,full_1,pre_1,post_1,full_2,pre_2,post_2)]
  estprepost %>% write.xlsx(file='tables.xlsx',sheetName='flow_devs_prepost',append=TRUE)
  
  ####
  
########################################################################################################
### exploratory
########################################################################################################
# I
est_issnetdev<-dtreg.m[ccy %in% CCY4] %>% neweymod(I_netflow6mf~netmisp,value.name='full')
est_issnetdev
est_issnetdevpre<-dtreg.m[ccy %in% CCY4 & date<ymd('2009-01-01')] %>% neweymod(I_netflow6mf~netmisp,value.name='pre')
est_issnetdevpre
est_issnetdevpost<-dtreg.m[ccy %in% CCY4 &date>=ymd('2009-01-01')] %>% neweymod(I_netflow6mf~netmisp,value.name='post')
est_issnetdevpost

# with rate
est_issnetdev<-dtreg.m[ccy %in% CCY4] %>% neweymod(i_netflow6mf~netmisp+swapraterel,value.name='full')
est_issnetdev
est_issnetdevpre<-dtreg.m[ccy %in% CCY4 & date<ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp+swapraterel,value.name='pre')
est_issnetdevpre
est_issnetdevpost<-dtreg.m[ccy %in% CCY4 &date>=ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp+swapraterel,value.name='post')
est_issnetdevpost

###########
## pre-post 2008 seperated 4 ccy
# Monthly regression with 6 month forwards
reg_issnetdev<-list()
CCY4=c('eur','gbp','jpy','chf')
for (iccy in CCY4){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy][date<ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev[,.(rn,eur,gbp,jpy,chf)] %T>% dt2clip

# post
reg_issnetdev<-list()
for (iccy in CCY4){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy][date>ymd('2009-01-01')] %>% neweymod(i_netflow6mf~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev[,.(rn,eur,gbp,jpy,chf)] %T>% dt2clip


## panel  # not panel newey
require(lfe)
dtreg.m[,.(date,i_netflow6mf,netmisp,ccy)] %>% save.dta13('issuanceflow.dta')
dtreg.m %>% felm(i_netflow6mf~netmisp|ccy,data = .) %>% summary()
dtreg.m[date<ymd('2008-01-01')] %>% felm(i_netflow6mf~netmisp|ccy,data = .) %>% summary()
dtreg.m[date>ymd('2008-01-01')] %>% felm(i_netflow6mf~netmisp|ccy,data = .) %>% summary()

regpanel=dtreg.m %>% felm(i_netflow6mf~netmisp|ccy,data = .)
alpha <- getfe(regpanel)
btrap(alpha,regpanel)


# Quarterly graph ---------------------------------------------------------

#Quarterly
# collapsing each ccy pair
registerDoParallel(1)
dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=2)
} %>% rbindlist()
dtiss.collapse.q %>% setkey(date,ccy)
dtiss.collapse.q[ccy=='eur'] %>% dt2clip()
## merging issuance
# construct quarterly using creditcip from begining of the month, merge with quarterly issuance
#dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[1],.(date,ccy)]
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
# dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
dtcreditcip.q %>% setkey(date,ccy); dtiss.collapse.q %>% setkey(date,ccy)
dtreg.q<-dtcreditcip.q[dtiss.collapse.q,nomatch=0]

## make issuance lead by one period
dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]

# lead issue by 1 period: reg issflow on netmisp
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.q[ccy==iccy] %>% neweymod(F.i_netflow~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev %T>% dt2clip


dtreg.q[ccy=='eur',.(date,i_netflow,netmisp)] %>% ggplotw()
