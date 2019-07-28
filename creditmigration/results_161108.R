#' ---
#' title: "Sidetracked!"
#' output: html_document
#' params:
#'  regversion: 5
#'  run.var: FALSE 
#'  run.stata: FALSE
#'  show.credit.cip: TRUE # graph credit cip, run regressoin
#'  individual.resid: FALSE # individually generated residualized spread or generated together
#'  sdc.filter: '6ccyv3' # set iss filter options: bondrefall, 6ccyv1 6ccyv2 # this is first step filter
#'  sdc.restrict.to.dtl: TRUE
#'  icollapse.filter: 1 # issfiltertype:# collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance
#'  sdc.filter.iv.restrict.to.dtl: TRUE
#'  sdc.filter.iv.collapse: 1
#'  gov: 'wogov'
#'  firmlevel: 'upcusip'
#'  mcore:  1
#'  quickstart: TRUE # to use previously saved results in the begining
#'  figfile:  '../paper/figures/161108/'
#' ---
#+ setup, include=FALSE
library(knitr)
opts_chunk$set(echo=FALSE,include=TRUE,cache=FALSE)

if (interactive()){
  print('interactive')
  setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")  
  source('util.r'); library(yaml)
  #strparams<-readr::read_lines('results_161108.R',skip = 4,n_max=21);strparams<-strparams[1:grep(pattern="#' ---",strparams)-1];params<-yaml.load(str_c(strparams,collapse='\n') %>% str_replace_all("#'",""))
  }else{
    source('util.r')    
}
if (Sys.info()['sysname'][[1]]=='Windows') mcore=1 else mcore=4

#+ warning=FALSE
(params %>% as.data.table())[,rn:=(1:2)] %>% melt('rn') %>% distinct(variable,value)

# use original bond data or current expanded

# quick start -------------------------------------------------------------
if(params$quickstart){
  load('preprocessedtemp.RData');load('db/sdc.RData');load('db/prl.RData');load('db/monthenddates.RData');load('db/bondref.RData');load('db/bondrefall.RData');source('util.r')
} else{
  # load data ---------------------------------------------------------------
  load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
  load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
  load('db/sdc.RData')
  source('util.r')
  # preprocessing -----------------------------------------------------------
  
    load('dbin/bdp_moody_augment_161110.RData')
    for (i in 1:length(prices))
      prices[[i]]<-prices[[i]] %>% as.data.table(keep.rownames=T)
    dtp<-rbindlist(prices) %>% setnames('rn','pk')
    bondref<-merge(bondref,dtp,by='pk',all.x = T)
    bondref[PAYMENT_RANK %in% c('Secured','1st lien','2nd lien'),senior:='SS']
    bondref[PAYMENT_RANK %in% c('SR Unsecured','Unsecured'),senior:='SU']
    bondref[PAYMENT_RANK %like% 'Subord',senior:='SB']
    bondref[,ccy:=str_to_lower(ccy)]
    exchrate<-prl[ticker %in% c('eur','aud','gbp','jpy','chf','cad'),.(ISSUE_DT=date,ccy=ticker,exchrate=value)][ccy %in% c('chf','cad','jpy'),exchrate:=1/exchrate][ccy=='usd',exchrate:=1]
    bondref<-merge(bondref,exchrate,by=c('ISSUE_DT','ccy'),all.x=T,all.y=F)
  bondref[(is.na(amt) & !is.na(AMT_ISSUED)), amt:=exchrate*AMT_ISSUED/10^6]
  dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
  save(dtm,file='preprocessedtemp.RData')
}

# residualize credit spread -----------------------------------------------------------
bondref <- (bondref %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6]
bondref[,ccy:=str_to_lower(ccy)]
dtl<-(dtm$dtl4 %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6]
bondrefall <- (bondrefall %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6]
bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]

#+ include=F
# if(params$gov=='woregional'){
#   dtl<-dtl[tf_mid_desc %nlk% '^[Reg|City|Other].+Gov']  
# } else if (params$gov=='wogov'){
#   dtl<-dtl[tf_mid_desc %nlk% 'Gov']  
# } else if (params$gov=='onlygov'){
#   dtl<-dtl[tf_mid_desc %like% 'Gov']  
# } else if (params$gov=='onlynatgov'){
#   dtl<-dtl[tf_mid_desc %like% 'National Gov'] 
# }

# 
dtl<- dtm$dtl4[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<99][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][str_to_lower(secur) %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik|pfd|zero|fr|fl|var|stk'][issue_type_desc %nlk% 'backed'][nrating<=16][nrating!=0]

dtm$br[,.N]
dtm$br[senior=='UN',.N]
dtm$br[is.na(amt),.N]




ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)

# reshape to long and merge with cip
dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)
# cip as credit-net
credit.cip.exact<-(ys1m$regresult[ys1meff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]

#'
dtcreditcip[!is.na(credit),.(cor(credit,cip)),ccy]
credit.cip.exact[,.(cor(credit,cip)),ccy]

dtcreditcip<-credit.cip.exact

# filter iss --------------------------------------------------------------

#modified
# b1 <- sdc[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<99][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][str_to_lower(secur) %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik|pfd|zero|fr|fl|var|stk'][issue_type_desc %nlk% 'backed'][nrating<=16][nrating!=0]
dtissraw <- sdc[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<99][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][str_to_lower(secur) %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik|pfd|zero|fr|fl|var|stk'][issue_type_desc %nlk% 'backed'][nrating<=16][nrating!=0]
dtissraw <- sdc[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<99][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][secur %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik'][nrating<=16][nrating!=0]
# compare.dt(b1,dtissraw,'deal_no')
# b1 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# dtissraw<-sdc %>% onefilter('v1') 
dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtissraw<-(dtissraw %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6] %>% add.earlist.iss.in.ccy()

# b1 <- sdc %>%  onefilter('v1')
# b1 <- sdc[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<=99999][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][str_to_lower(secur) %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik|pfd|zero|fr|fl|var|stk'][issue_type_desc %nlk% 'backed']
# b2 <- sdc %>% filter.sdc('6ccyv3')
 # b2 <- sdc[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][amt>=50][ytofm>=1][ytofm<99][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][secur %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik'][nrating<=16][nrating!=0]
# b2 %>% showdups('deal_no')
# compare.dt(b1,b2,'deal_no')

#' **Summary of bond data**

# #'## Data summary ------------------------------------------------------------
dtl.bonds<-dtl[,.SD[1],pk]
dtl.bonds.global<-(dtl %>% filterglobaluponly())[,.SD[1],pk]
invisible({dtl.bonds[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds[sic1==6,sicind:=6];dtl.bonds[sic1==9,sicind:=9] })
dtl.bonds<-dtl.bonds %>% bucketytofm()
lefthalf<-dtl.bonds %>% table.summary()
invisible({dtl.bonds.global[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds.global[sic1==6,sicind:=6];dtl.bonds.global[sic1==9,sicind:=9]})
dtl.bonds.global<-dtl.bonds.global %>% bucketytofm()
righthalf <- dtl.bonds.global %>% table.summary()
cbind(lefthalf,righthalf)



#' Monthly Level Reg
#+ eval=params$show.credit.cip
reg_creditcip<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
 reg_creditcip[[length(reg_creditcip)+1]]<-dtcreditcip[ccy==iccy] %>% neweymod('credit~cip',value.name=iccy)
}
regtable1<-(rbindlist(reg_creditcip) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]

#'
# panel with correct standard errors
dtin<-dtcreditcip
# dtin %>% write.dta(file = 'temp.dta')
require(plm)
# pooled 
reg1<-dtin %>% plm(credit~cip,.,index=c('ccy','date'),model='pooling') 
res1<-coeftest(reg1,vcov=vcovSCC(reg1,type='sss',maxlag=12))
regadd1 <- regformatcoef(dtin,reg1,res1,'Pooled',lags=12)
# regadd1

# FE on date
reg2<-dtin %>% plm(credit~cip+as.factor(date),.,index=c('ccy','date'),model='pooling') 
res2<-coeftest(reg2,vcovSCC(reg2,type='sss',maxlag = 12))
regadd2 <- regformatcoef(dtin,reg2,res2,'FEdate',lags=12)[rn=='const',FEdate:='']
# regadd2

#FE on firm
reg3<-dtin %>% plm(credit~cip,.,index=c('ccy','date'),model='within') 
# res3<-coeftest(reg1,vcovNW(reg3,type="HC3",maxlag=12)) 
res3<-coeftest(reg3,vcovSCC(reg3,type='sss',maxlag = 12))
regadd3 <- regformatcoef(dtin,reg3,res3,'FEfirm',lags=12)
# regadd3

tableout<-cbind(regadd1,regadd2,regadd3,regtable1)[,.(rn,Pooled,FEdate,FEfirm,eur,gbp,jpy,aud,chf,cad)];tableout

try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'creditcipMolevel',showNA=F)) #no append here


# issuance flow and net dev -----------------------------------------------
#' ## issuance flow and net dev
#+ swaprate calc m & q, include=F
swap.rate<-prl[monthend==1 & date>=ymd('2004-01-01')][ticker %like% '^\\w\\wsw5$' | ticker %like% '^eusa5$',.(date,ticker,value=value*100)]
swap.rate[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf']
swap.rate<-swap.rate[ccy %in% c('eur','gbp','jpy','aud','chf','cad','usd'),.(date,ccy,swaprate=value)][order(date)]
swap.rate[,swapraterel:=swaprate-.SD[ccy=='usd',swaprate],date] #define relative swap rate
swap.rate.q<-swap.rate[ccy!='usd'][,date:=floor_date(date,'quarter')][,.SD[3],.(date,ccy)][,.(date,ccy,swapraterel)] %>% setkey(date,ccy)
swap.rate.m<-swap.rate[ccy!='usd'][,date:=floor_date(date,'month')][,.(date,ccy,swapraterel)] %>% setkey(date,ccy)

# set date to beginning of month/quarter
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
# construct quarterly using creditcip monthly data, picking the last of the month
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]

#issuance calc
if (params$sdc.restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
 dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
} else {dtiss.in<-dtissraw}
#+ Monthly issuance calc,include=F
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=params$icollapse.filter)
} %>% rbindlist() %>% setkey(date,ccy)
dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
dtreg.m[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.m[,i_netflow6mf:=(shift(i_netflow,n=1,type='lead')+shift(i_netflow,n=2,type='lead')+shift(i_netflow,n=3,type='lead')+shift(i_netflow,n=4,type='lead')+shift(i_netflow,n=5,type='lead')+shift(i_netflow,n=6,type='lead'))/6,ccy]
dtreg.m[,I_netflow6mf:=(shift(I_netflow,n=1,type='lead')+shift(I_netflow,n=2,type='lead')+shift(I_netflow,n=3,type='lead')+shift(I_netflow,n=4,type='lead')+shift(I_netflow,n=5,type='lead')+shift(I_netflow,n=6,type='lead'))/6,ccy]
dtreg.m[,F.i_netflow.smooth:=shift(i_netflow.smooth,n=1,type='lead'),ccy]
dtreg.m[,i_netflow.smooth6mf:=(shift(i_netflow.smooth,n=1,type='lead')+shift(i_netflow.smooth,n=2,type='lead')+shift(i_netflow.smooth,n=3,type='lead')+shift(i_netflow.smooth,n=4,type='lead')+shift(i_netflow.smooth,n=5,type='lead')+shift(i_netflow.smooth,n=6,type='lead'))/6,ccy]
dtreg.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
dtreg.m[,D3.credit:=credit-shift(credit,n=3,type='lag'),.(ccy)]
dtreg.m<-swap.rate.m[dtreg.m] # add swap rate
#+ Quarterly issunace calc,include=F
#Issuance quarterly collapsing each ccy pair
dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=params$icollapse.filter)
} %>% rbindlist() %>% setkey(date,ccy)
dtcreditcip.q %>% setkey(date,ccy);
dtreg.q<-dtcreditcip.q[dtiss.collapse.q,nomatch=0]

## make issuance lead by one period
dtreg.q[,F.I_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]
dtreg.q[,D.credit:=credit-shift(credit,n=1,type='lag'),ccy]
dtreg.q[,D.cip:=cip-shift(cip,n=1,type='lag'),ccy]
dtreg.q<-swap.rate.q[dtreg.q] # add swap rate

#' Quick summary of issuance flow
#+ echo=T,include=F, eval=F
dtreg.m[,.(iss_mean=mean(i_netflow),iss_sd=sd(i_netflow),iss_min=min(i_netflow),iss_max=max(i_netflow)),ccy]
dtreg.m[,.(Iss_mean=mean(I_netflow),Iss_sd=sd(I_netflow),Iss_min=min(I_netflow),Iss_max=max(I_netflow)),ccy]
dtreg.q[,.(iss_mean=mean(i_netflow),iss_sd=sd(i_netflow),iss_min=min(i_netflow),iss_max=max(i_netflow)),ccy]
dtreg.q[,.(Iss_mean=mean(I_netflow),Iss_sd=sd(I_netflow),Iss_min=min(I_netflow),Iss_max=max(I_netflow)),ccy]
dtreg.q[,.(date,i_netflow,ccy)] %>% ggplot(aes(date,i_netflow,colour=ccy))+geom_line()
dtreg.q[ccy=='eur',.(date,i_netflow,ccy)] %>% ggplot(aes(date,i_netflow,colour=ccy))+geom_line()
dtreg.q[ccy=='eur',.(date,i_netflow,netmisp)] %>% melt(id.vars='date') %>% ggplot(aes(date,value,colour=variable))+geom_line()
dtreg.q[ccy=='eur',.(date,100*(mu-mean(mu)),netmisp)] %>% melt(id.vars='date') %>% ggplot(aes(date,value,colour=variable))+geom_line()
dtreg.q[ccy=='eur'][,.(date,netmisp)] %>% ggplotw()
dtreg.q[ccy=='eur'][,.(date,i_netflow)] %>% ggplotw()
dtreg.q[ccy=='eur'][date<'2016-01-01'][,.(date,netmisp,i_netflow)] %>% ggplotw()
dtreg.q[ccy=='eur'][date<'2016-01-01'][,.(date,netmisp,i_netflow)] %>% felm(i_netflow~netmisp,.) %>% stargazer(type='text',report='vct*')


#' ## Monthly regressions of issuance on deviations
#+ echo=T,include=T
#dtreg.m[date<ymd('2016-08-01')] %>% reg.newey.all(i_netflow6mf~netmisp)
#dtreg.m %>% reg.newey.all(i_netflow6mf~netmisp+swapraterel)
#dtreg.m[date<ymd('2016-08-01')] %>% reg.newey.all(i_netflow6mf~netmisp+swapraterel)
tableout<-dtreg.m %>% reg.newey.all(i_netflow6mf~netmisp+swapraterel); tableout;
try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'issnetdev',showNA=F,append=T))
#tableout<-dtreg.m %>% reg.newey.all(I_netflow6mf~netmisp+swapraterel); tableout;try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'ISSnetdev',showNA=F,append=T))
#dtreg.m %>% reg.newey.all(i_netflow.smooth6mf~netmisp)
#dtreg.m %>% reg.newey.all(i_netflow.smooth6mf~netmisp+swapraterel)
#dtreg.m %>% reg.newey.all(i_netflow6mf~credit+cip)
tableout<-dtreg.m %>% reg.newey.all(i_netflow6mf~credit+cip+swapraterel);tableout
try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'isscreditcip',showNA=F,append=T))


#' ### regression of issuance on chg in credit
#+ include=F, echo=T,eval=F
reg.credit.chg<-list()
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(F.i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow.smooth~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow6mf~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(I_netflow~D.credit|ccy|0|0,.)
stargazer::stargazer(reg.credit.chg,report='vct*',type='text')


#' ## Quarterly regressions of issuance on deviations
#+ echo=T,include=T
#dtreg.q %>% reg.newey.all(F.i_netflow~netmisp)
tableout<-dtreg.q %>% reg.newey.all(F.i_netflow~netmisp+swapraterel);tableout;try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'issnetdevQ',showNA=F,append=T))
#dtreg.q %>% reg.newey.all(F.i_netflow~credit+cip)
tableout<-dtreg.q %>% reg.newey.all(F.i_netflow~credit+cip+swapraterel);tableout;try(tableout %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'isscreditcipQ',showNA=F,append=T))
#dtreg.q %>% reg.newey.all(F.I_netflow~netmisp)
#dtreg.q %>% reg.newey.all(F.I_netflow~netmisp+swapraterel)
#dtreg.q %>% reg.newey.all(F.I_netflow~credit+cip)
#dtreg.q %>% reg.newey.all(F.I_netflow~credit+cip+swapraterel)
#dtreg.q %>% reg.newey.all(i_netflow~netmisp)
dtreg.q %>% reg.newey.all(i_netflow~netmisp+swapraterel)

#' #### reg flow on credit chg alone
#+ echo=F,include=F,eval=F
reg.credit.chg<-list()
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.q %>% felm(i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.q %>% felm(F.i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.q %>% felm(D.mu~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.q %>% felm(I_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.q %>% felm(F.I_netflow~D.credit|ccy|0|0,.)
stargazer::stargazer(reg.credit.chg,report='vct*',type='text')

####################################################################################################################################
## abs net dev and D, all iss ----------------------------------------------
#' ## abs net dev and D, all iss ----------------------------------------------
#+ collapse each,include=F
if (params$sdc.filter.iv.restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
 dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
} else {dtiss.in<-dtissraw}
# Monthly
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=params$sdc.filter.iv.collapse)
} %>% rbindlist() %>% setkey(date,ccy)


#+ merging issuance,include=F
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
dtreg.m[,month:=month(date)]
dtreg.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
dtreg.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
dtreg.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
dtreg.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreg.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreg.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]



#' ## debt maturity aggregate
#+ Monthly agg, include=F
registerDoParallel(1)
dtmat.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4.mature(iccy,collapse.freq = 'month',filter=params$sdc.filter.iv.collapse)
} %>% rbindlist() %>% setkey(date,ccy)
#+ merging matured with credit cip,include=F
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtcreditcip.m %>% setkey(date,ccy); dtmat.collapse.m %>% setkey(date,ccy)
dtreg.mat.m<-dtcreditcip.m[dtmat.collapse.m,nomatch=0]
dtreg.mat.m[,month:=month(date)]
dtreg.mat.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.I_both:=I_both-shift(I_both,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreg.mat.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreg.mat.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]
dtreg.mat.m[,L.D.abs.netmisp:=shift(D.abs.netmisp,n=1,type='lag'),.(ccy)]
dtreg.mat.m %>% setnames('I_both','I_both.mat')
dt.iss.mat.m<-update.dt(dtreg.m,dtreg.mat.m,keyfield = c('date','ccy'),override = T)
dt.iss.mat.m %>% setnames(c('I_both'),c('I_both.iss'))
dt.iss.mat.m[is.na(I_both.iss),I_both.iss:=0]


#' ### IV just EUR
dtreg<-dt.iss.mat.m[date<ymd('2016-07-30')][ccy %in% c('eur')]
invisible({dtreg[,rn:=rownames(dtreg)]}) # add a unique variable to report robust SE
regres<-list()
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss+L.D.abs.netmisp |0| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat+L.D.abs.netmisp |0| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |0| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |0| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))


#' ### Panel IV eur gbp chf 
dtreg<-dt.iss.mat.m[date<ymd('2016-07-30')][ccy %in% c('eur','gbp','chf')]
invisible({dtreg[,rn:=rownames(dtreg)]}) # add a unique variable to report robust SE
regres<-list()
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |ccy| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))


#' ### Panel IV eur gbp chf jpy
dtreg<-dt.iss.mat.m[date<ymd('2016-07-30')][ccy %in% c('eur','gbp','chf','jpy')]
invisible({dtreg[,rn:=rownames(dtreg)]}) # add a unique variable to report robust SE
regres<-list()
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |ccy| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))


dtreg<-dt.iss.mat.m[date<ymd('2016-07-30')][ccy %in% c('eur','gbp','chf','jpy')]
invisible({dtreg[,rn:=rownames(dtreg)]}) # add a unique variable to report robust SE
regres<-list()
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |ccy| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))

#dtreg[,.(sd(I_both.iss),sd(I_both.mat))] 
#saving to excel
regres.ls<-list();rsq.ls<-list();n.ls<-list()
for (iregres in regres){
 regres.ls[[length(regres.ls)+1]]<-((iregres %>% summary)$coef %>% as.data.table(keep.rownames=T))[,reg:=length(regres.ls)+1]
 rsq.ls[[length(rsq.ls)+1]]<-as.data.table((iregres %>% summary)$r.squared)[,reg:=length(rsq.ls)+1]
 n.ls[[length(n.ls)+1]]<-as.data.table((iregres %>% summary)$df[1:2] %>% sum())[,reg:=length(n.ls)+1]
}
dtout<-rbindlist(regres.ls)
invisible(dtout[,`t value`:=sprintf("[%.2f]",`t value`)])
invisible(dtout[,Estimate:=sprintf('%.3g',Estimate)])
dtout2<-(dtout %>% melt(id.vars=c('rn','reg'),measure.vars=c('Estimate','t value')))[order(-rn)] 
rsq.dt<-(rsq.ls %>% rbindlist)[,.(rn='zRsq',reg,variable='Rsq',value=sprintf('%.2f',V1))]
n.dt<-(n.ls %>% rbindlist)[,.(rn='zzN',reg,variable='N',value=sprintf('%.2f',V1))]
dtout3<-rbind(dtout2,rsq.dt)
dtout4<-rbind(dtout3,n.dt)
dtout5<-(dtout4 %>% dcast(rn+variable~reg))[order(rn,variable)]
dtout5 %>% xlsx::write.xlsx(file=str_c(params$figfile,'activetables.xlsx'),sheetName = 'IVreg',showNA=F,append=T)


#' #### control for rates?
#' ### Panel IV All ccy
dtreg<-dt.iss.mat.m[date<ymd('2016-07-30')]#[ccy %in% c('eur','gbp','chf','cad')]
invisible({dtreg[,rn:=rownames(dtreg)]}) # add a unique variable to report robust SE
regres<-list()
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |ccy| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))


# dt.iss.mat.m[date<ymd('2016-07-30')] %>% write.dta('temp.dta')
# system('rm temp.csv')
#' Stata output
#+ eval=params$run.stata
stata('format date %tm
     gen year=year(date)
     gen monthly=ym(year,month)
     format monthly %tm
     tsset ccy monthly
     eststo clear
     quietly eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp i.ccy,robust
     quietly eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy,robust
     quietly eststo: xi: reg I_both_iss I_both_mat i.ccy,robust cluster(monthly)
     quietly eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust
     esttab, order(I_both*) bracket r2 nostar nogaps replace', data.in=dtreg)
#dtout<-fread('temp.csv',sep=',');
#system('open temp.csv')}


# Credit CIP -------------------------------------------------------------------
#' ## Graphs
#' Extension
#+ eval=params$show.credit.cip
ys1mb<-resyldsprdv4(dtl[!is.na(DEBT_CLASS_CD)],dtm$prl,regversion=5,returndt=F,parallel.core. = mcore)
figext<-ggplotw.comp2(ys1m$regcoef,ys1mb,c('residualized credit spread diff.','with additional controls'))
ggsave(plot=figext,file=str_c(params$figfile,'extensioncomp.pdf'),width=10.5,height=6.5)

#' credit deviations
#+ eval=params$show.credit.cip
dtcreditcip[ccy %in% c('eur','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))
ggsave(file=str_c(params$figfile,'active_credit.pdf'),width=9,height=6)

#' CIP
#+ eval=params$show.credit.cip
dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))
# ggsave(file=str_c(params$figfile,'fig1_cip_160830.pdf'),width=9,height=6)
#' 1y cip
#+ eval=params$show.credit.cip
dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs1,bpbs1,jybs1,adbs1)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 1-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))
# ggsave(file=str_c(params$figfile,'fig2_cip_1y_160929.pdf'),width=9,height=6)

#' Graphing CIP and credit mispriing overlay
#+ eval=params$show.credit.cip, fig.width=8
#'
creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename=str_c(params$figfile,'active_panel_usd.pdf'),yrstr.='5',wide=T) 

#' scatter
#+ eval=params$show.credit.cip
aa<-creditcip.result$dt.credit.cip[,.(ccy,cip,credit)]#[credit>-130]
aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Credit Spread Diff. in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=-80,y=40,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))
ggsave(file=str_c(params$figfile,'active_creditcipscatter.pdf'),width=9,height=6)

#' Net deviation
#+ eval=params$show.credit.cip
#'
ys1meff$regresult %>% plot.netdev(fileout=str_c(params$figfile,'active_netdev.pdf'))

#' EUR credit cip overlay
#+ eval=params$show.credit.cip
#creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
plot.eur.credit.cip<-creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
ggsave(plot.eur.credit.cip,file=str_c(params$figfile,'active_eurcreditcip.pdf'),width=9,height=6)
plot.eur.swap.rate.diff<-((dtm$prl[ticker %in% c('eusw5','ussw5'),.(date,ticker,value)][date>=ymd('2004-01-01')] %>% dcast.data.table(date~ticker))[,.(date,swap.rate.diff=eusw5-ussw5)] %>% setkey(date))[monthenddates,nomatch=0] %>% ggplot(aes(date,swap.rate.diff,colour='black'))+geom_line(colour='black') +xlab('')+ylab('5-yr swap rate diff. (EU-US)')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('5-yr swap rate differential'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')
require('gridExtra')
plot.eur.cc.grid<-grid.arrange(plot.eur.credit.cip+theme(legend.position=c(1,0),legend.justification=c(1,0)),plot.eur.swap.rate.diff+theme(legend.position=c(1,0),legend.justification=c(1,0)),ncol=1,nrow=2,layout_matrix=rbind(c(1),c(2)),heights=c(4,3))
ggsave(plot.eur.cc.grid,file=str_c(params$figfile,'appendix_eurusdratediff.pdf'),width=9,height=6)

# popular
#plot.eur.credit.cip<-creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr (EURUSD cross-currency basis)','Credit Spread Diff. (EU-US) controling for other bond characteristics'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr (EURUSD cross-currency basis)','Credit Spread Diff. (EU-US) controling for other bond characteristics'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+
  geom_rect(aes(xmin = as.Date('2007-08-01'), ymin = -Inf, xmax = as.Date('2008-08-01'), ymax = Inf),fill = "grey89",alpha=.2)+
  geom_rect(aes(xmin = as.Date('2008-09-01'), ymin = -Inf, xmax = as.Date('2009-08-01'), ymax = Inf),fill = "grey89",alpha=.2)+
  geom_rect(aes(xmin = as.Date('2011-05-01'), ymin = -Inf, xmax = as.Date('2012-06-01'), ymax = Inf),fill = "grey89",alpha=.2)+
  geom_rect(aes(xmin = as.Date('2014-09-01'), ymin = -Inf, xmax = as.Date('2016-10-01'), ymax = Inf),fill = "grey89",alpha=.2)+
  geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr (FX-implied - actual euro funding rate)','Credit Spread Diff. (EU-US) controling for other bond characteristics'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr (FX-implied - actual euro funding rate)','Credit Spread Diff. (EU-US) controling for other bond characteristics'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+
  annotate('text',size=3.1,x=ymd('2005-01-01'),y=10,label=str_c('correlation=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))+
  annotate('text',size=3.1,x=ymd('2011-11-01'),y=9,label='Bank dollar\nshortage')+
  annotate('text',size=3.1,x=ymd('2011-11-01'),y=-90,label='CIP\nspills over\n to credit sprd')+
  annotate('text',size=3.1,x=ymd('2015-09-01'),y=10,label='ECB QE')+
  annotate('text',size=3.1,x=ymd('2015-09-01'),y=-90,label='credit sprd\nspills over\n to CIP')+
  annotate('text',size=3.1,x=ymd('2008-02-01'),y=9,label='U.S. credit\ncrunch')+
  annotate('text',size=3.1,x=ymd('2008-02-01'),y=-90,label='credit sprd\nspills over\n to CIP')+
  annotate('text',size=3.1,x=ymd('2009-02-15'),y=9,label='Lehman,\nfinancial\ncrisis')+
  annotate('text',size=3.1,x=ymd('2009-02-15'),y=-90,label='liquidity\ncontraction\n in both\nmarkets')
ggsave(file=str_c(params$figfile,'credit_cip_spillover.pdf'),width=9,height=6)


#' HG LG
#+ eval=params$show.credit.cip
dthlgrade<-dtl[ccy %in% c('usd','eur')]#[pub != 'Govt']#[date>ymd('2006-01-01')]
ys_hy<-dthlgrade[nrating>6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
ys_hg<-dthlgrade[nrating<=6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
setnames(ys_hy$regcoef,'eur','highyield')
setnames(ys_hg$regcoef,'eur','highgrade')
ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit deviation (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))
ggsave(file=str_c(params$figfile,'active_hghy_eur.pdf'),width=9,height=6)

#' HG/LG for all ccy together
#+ eval=params$show.credit.cip, warning=F
dtrating<-copy(dtl) %>% filterglobaluponly()
invisible(yshg<-resyldsprdv4(dtrating[nrating %between% c(1,5)],prl,regversion=3,returndt=T,parallel.core.=mcore))
invisible(yshy<-resyldsprdv4(dtrating[nrating>=6],prl,regversion=3,returndt=T,parallel.core.=mcore))
dtplot.rating<-yshg$regresult[yshy$regresult,on=c('date','ccy')]
dtplot.rating %>% setnames(c('est','i.est'),c('hg','lg'))
dtplot.ratingl<-dtplot.rating %>% melt(id.vars=c('date','ccy'),measure.vars=c('hg','lg'))
dtplot.ratingl %>% plot.panel.creditrating(filename='',wide=T)#../paper/figures/HGLG.pdf
ggsave(file=str_c(params$figfile,'active_HGLG_all.pdf'),width=10.5,height=6.5)


# Credit cip against other ccys -------------------------------------------
#' ### Credit cip against EUR
#+ eval=params$show.credit.cip
ys.eur<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = 'eur')
ys.eur.eff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'eur')
credit.cip.exact.eur<-(ys.eur$regresult[ys.eur.eff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
credit.cip.exact.eur[ccy %in% c('usd','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to EUR in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))
zz<-plot.panel.creditcip.any.ccy(ys.eur,ys.eur.eff,wide=T,filename=str_c(params$figfile,'active_panel_eur.pdf'))
#' Without USD bonds in the calculation
#+ eval=F
ys.eur2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,returndt=T,mainccyin = 'eur')
ys.eur.eff2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'eur')
zz2<-plot.panel.creditcip.any.ccy(ys.eur2,ys.eur.eff2,wide=T)
#' ### Credit cip against GBP
#+ eval=params$show.credit.cip
ys.gbp<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = 'gbp')
ys.gbp.eff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'gbp')
credit.cip.exact.gbp<-(ys.gbp$regresult[ys.gbp.eff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
credit.cip.exact.gbp[ccy %in% c('usd','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to gbp in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))
zz<-plot.panel.creditcip.any.ccy(ys.gbp,ys.gbp.eff,wide=T,filename=str_c(params$figfile,'active_panel_gbp.pdf'))
#' Without USD bonds in the calculation
#+ eval=F
ys.gbp2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,returndt=T,mainccyin = 'gbp')
ys.gbp.eff2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'gbp')
zz2<-plot.panel.creditcip.any.ccy(ys.gbp2,ys.gbp.eff2,wide=T)



#'
# VAR  ---------------------------------------------------------
#+ VAR calc, include=FALSE,eval=params$run.var
version.var.data<-'pairwise,exact'
if (version.var.data=='original'){
  dtcreditcip.m<-copy(credit.cip.exact)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
} else if (version.var.data %like% 'original_approx'){
  # the non-exact version works etter with ordering of flow credit cip
  #dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
} else if (version.var.data %like% 'pairwise'){
  # just pairwise eurusd
  iccy <- 'eur'
  yseurusd <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 1))$regresult
  yseurusdeff <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1))$regresult
  creditsingle<-create.dev.long2(prwin = dtm$prw,creditmispin = yseurusd,netmispin = yseurusdeff)
  # use exact
  if (version.var.data %like% 'exact') creditsingle[,cip:=credit-netmisp]
  dtcreditcip.m<-copy(creditsingle)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
}

if (params$sdc.restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
  dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
} else {dtiss.in<-dtissraw}
dtiss.collapse.m <- foreach(iccy=c('eur')) %do% {
  dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=params$icollapse.filter)
} %>% rbindlist() %>% setkey(date,ccy)
dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]; 
# dtreg.m  %>% write.dta('vardatain_1019.dta')
#' ### VAR using R: Flow Credit CIP
#+ eval=params$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,credit,cip)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95) 
resirf %>% plot.irf() %>% ggsave(file=str_c(params$figfile,'active_oirf_icb.pdf'),.,width=8,height=5)

#' ### VAR using R: Flow CIP Credit
#+ eval=params$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,cip,credit)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95) 
resirf %>% plot.irf() %>% ggsave(file=str_c(params$figfile,'active_oirf_ibc.pdf'),.,width=8,height=5)
#' ### SVAR using R with partial identification
#+ eval=params$run.var
amat <- diag(3)
diag(amat) <- 1
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat[2,3] <- NA
## A matrix
res2<-vars::SVAR(res1,Amat=amat,estmethod = 'direct')
resirf<-vars::irf(res2,runs=500) 
resirf %>% plot.irf() %>% ggsave(file=str_c(params$figfile,'active_oirf_partial.pdf'),.,width=8,height=5)

#+ eval=params$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,netmisp)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95,impulse='netmisp',response='i_netflow') 
resirf %>% plot.irf.single() %>%  ggsave(file=str_c(params$figfile,'active_oirf_inetdev.pdf'),.,width=8,height=5)

# resirf<-vars::irf(res1,ci=.95,impulse='netmisp',response='netmisp') 
# resirf %>% plot.irf.single() 

#+ statacalc,include=FALSE,eval=(params$run.var & params$run.stata)
statacmd <- str_c('clear matrix
                  set more off
                  set matsize 10000
                  format date %tm
                  gen year=year(date)
                  gen month=month(date)
                  gen monthly=ym(year,month)
                  format monthly %tm
                  decode ccy, gen(strccy)
                  xtset ccy monthly
                  drop if date>=date("20160901","YMD")
                  local graphopt "title("") subtitle("") note("") legend(off) xtitle(month) graphregion(color(white)) bgcolor(white)"
                  
                  * A: irf
                  *var i_netflow credit cip if strccy=="eur", lags(1) 
                  *irf create eur_irf, set(irf1,replace) step(10) 
                  *irf describe eur_irf
                  *irf cgraph  (eur_irf credit credit irf,`graphopt\')  (eur_irf credit cip irf,`graphopt\')  (eur_irf credit i_netflow irf,`graphopt\') (eur_irf cip credit irf,`graphopt\') (eur_irf cip cip irf,`graphopt\')  (eur_irf cip i_netflow irf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
                  *graph export "',params$figfile,'VAR_irfeur_A.png",replace
                  * A
                  var i_netflow credit cip if strccy=="eur", lags(1)
                  irf create eur_oirf, set(irf1,replace) step(10)
                  irf describe eur_oirf
                  irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
                  graph export "',params$figfile,'VAR_oirfeur_A.png",replace
                  
                  * reorder
                  var i_netflow cip credit if strccy=="eur", lags(1)
                  irf create eur_oirf, set(irf1,replace) step(10)
                  irf describe eur_oirf
                  irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
                  graph export "',params$figfile,'VAR_oirfeur_B.png",replace
                  
                  * C: lags=4 according to AIC
                  varsoc  i_netflow credit cip  if strccy=="eur"
                  var i_netflow credit cip if strccy=="eur", lags(1/4)
                  irf create eur_oirf, set(irf1,replace) step(10)
                  irf describe eur_oirf
                  irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
                  graph export "',params$figfile,'VAR_oirfeur_4lags.png",replace
                  
                  * D: c-b and iss
                  gen netdev=netmisp
                  var i_netflow netdev if strccy=="eur", lags(1)
                  irf create oirf_eur, set(irf1,replace) step(10)
                  irf describe oirf_eur
                  irf graph oirf, irf(oirf_eur) impulse(netdev) response(i_netflow) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
                  graph export "',params$figfile, 'VAR_oirfeur_netdeviss.png",replace
                  xtvar i_netflow credit cip , lags(1) mc step(10) stirf ssaving(',params$figfile,'panelvar)
                  xtvar i_netflow cip credit , lags(1) mc step(10) stirf ssaving(',params$figfile,'panelvarrev)')
stata(statacmd,data.in=dtreg.m) 
#dtreg.m %>% write.dta('temp.dta')
# Panel VAR with all 6 ccys
plot.irf.panel(fname=str_c(params$figfile,'panelvar.dta'),filename = str_c(params$figfile,'panelvar.pdf'))

#+ echo=FALSE,include=TRUE,eval=params$run.var
#print('EUR IRF')
#include_graphics('',params$figfile,'VAR_irfeur_A.png') 
print('EUR OIRF order: Issuance Credit CIP')
include_graphics(str_c(params$figfile,'VAR_oirfeur_A.png') )
print('EUR OIRF order: Issuance CIP Credi')
include_graphics(str_c(params$figfile,'VAR_oirfeur_B.png') )
print('EUR OIRF 4 lags order: Issuance Credit CI')
include_graphics(str_c(params$figfile,'VAR_oirfeur_4lags.png') )
print('EUR OIRF order: Issuance Net deviation (credit-cip)')
include_graphics(str_c(params$figfile,'VAR_oirfeur_netdeviss.png'))