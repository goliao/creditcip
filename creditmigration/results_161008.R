#' ---
#' title: "Results"
#' output: html_document
#' ---
#+ setup, include=FALSE
library(knitr)
opts_chunk$set(echo=FALSE,include=TRUE,cache=FALSE)
#setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
if (Sys.info()['sysname'][[1]]=='Windows') mcore=1 else mcore=4
#' Options -----------------------------------------------------------------
gl <- list('run.var'=TRUE, 
           'run.stata'=TRUE,
           'show.credit.cip'=TRUE, # graph credit cip, run regressoin
           'individual.resid'=FALSE,# individually generated residualized spread or generated together
           'sdc.filter'='6ccyv3',# set iss filter options: bondrefall, 6ccyv1 6ccyv2 # this is first step filter
           'sdc.filter.var'=list('restrict.to.dtl'=TRUE,'collapse.filter'=1), # issfiltertype:# collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance
           'sdc.filter.iss.reg'=list('restrict.to.dtl'=TRUE,'collapse.filter'=0),
           'sdc.filter.iv.reg'=list('restrict.to.dtl'=TRUE,'collapse.filter'=1),
           'gov'='wogov',
           'firmlevel'='upcusip',
           'mcore'= mcore,
           'quickstart'=FALSE # to use previously saved results in the begining
           )
options(gl=gl)
#+ warning=FALSE

(getOption('gl') %>% as.data.table())[,rn:=(1:2)] %>% melt('rn') %>% distinct(variable,value)
# getOption('gl')$sdc.filter.var[[1]]
# use original bond data or current expanded
# set firm level to either upcusip or cu
firmlevel <- getOption('gl')$firmlevel
# individually constructing pairwise credit spread or construct all at the same time
# taking quarterly credit/cip to be last date, or average of 3 end of month, or first month
#last

# quick start -------------------------------------------------------------
if(getOption('gl')$quickstart){
  load('preprocessedtemp.RData');load('db/sdc.RData');load('db/prl.RData');load('db/monthenddates.RData');load('db/bondref.RData');load('db/bondrefall.RData');source('util.r')
} else{
  # load data ---------------------------------------------------------------
  load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
  load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
  source('util.r')
  load('db/sdc.RData')
  # preprocessing -----------------------------------------------------------
  dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
  save(dtm,file='preprocessedtemp.RData')
}
# residualize credit spread -----------------------------------------------------------
bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
dtl<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]

#+ include=F
  if(getOption('gl')$gov=='woregional'){
    dtl<-dtl[tf_mid_desc %nlk% '^[Reg|City|Other].+Gov']  
  } else if (getOption('gl')$gov=='wogov'){
    dtl<-dtl[tf_mid_desc %nlk% 'Gov']  
  } else if (getOption('gl')$gov=='onlygov'){
    dtl<-dtl[tf_mid_desc %like% 'Gov']  
  } else if (getOption('gl')$gov=='onlynatgov'){
    dtl<-dtl[tf_mid_desc %like% 'National Gov'] 
  }
  
  ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
  
  ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)
  # reshape to long and merge with cip
  dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)
  # cip as credit-net
  credit.cip.exact<-(ys1m$regresult[ys1meff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
  
  # individually constructing pairwise credit spread
  if (getOption('gl')$individual.resid){
    ys2m<-list(); ys2meff<-list()
    for(iccy in c('eur','gbp','jpy','aud','chf','cad')){
      print(iccy)
      ys2m[[length(ys2m)+1]] <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore))$regresult
      ys2meff[[length(ys2meff)+1]] <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore))$regresult
    }
    ys2m.res<-rbindlist(ys2m)
    ys2meff.res<-rbindlist(ys2meff)
    dtcreditcip2<-create.dev.long2(prwin = dtm$prw,creditmispin = ys2m.res,netmispin = ys2meff.res)
    print(dtcreditcip[,cor(credit,cip)])
    print(dtcreditcip2[,cor(credit,cip)])
    creditcip.result<-plot.panel.creditcip(dtm$prw,ys2m.res,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
    dtcreditcip<-dtcreditcip2
  }
  

#' Gov vs non-gov
#+ eval=F
dtl[,.(.N,sum(na.omit(amt))),tf_mid_desc][order(-V2)][tf_mid_desc %like% 'Gov']
dtl[,.(.N,sum(na.omit(amt))),tf_mid_desc][order(-V2)][tf_mid_desc %like% '^[Reg|City|Other].+Gov']
#dtl.gov.all<-dtl[tf_mid_desc %like% 'Gov']
dtl.gov.all<-dtl[tf_mid_desc %like% 'Gov']
ys.gov.all<-resyldsprdv4(dtl.gov.all,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
yseff.gov.all<-resyldsprdv4(dtl.gov.all,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)
dtcreditcip.gov.all<-create.dev.long2(prwin = dtm$prw,creditmispin = ys.gov.all$regresult,netmispin = yseff.gov.all$regresult)
creditcip.result<-plot.panel.creditcip(dtm$prw,ys.gov.all$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
creditcip.result$dt.corr

dtl.gov.nat<-dtl[tf_mid_desc %like% 'National Gov']
ys.gov.nat<-resyldsprdv4(dtl.gov.nat,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
yseff.gov.nat<-resyldsprdv4(dtl.gov.nat,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)
dtcreditcip.gov.nat<-create.dev.long2(prwin = dtm$prw,creditmispin = ys.gov.nat$regresult,netmispin = yseff.gov.nat$regresult)
creditcip.result<-plot.panel.creditcip(dtm$prw,ys.gov.nat$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf

dtl.wo.gov<-dtl[tf_mid_desc %nlk% '^[Reg|City|Other].+Gov']
dtl.wo.gov<-dtl[tf_mid_desc %nlk% 'Gov']
ys.wo.gov<-resyldsprdv4(dtl.wo.gov,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
yseff.wo.gov<-resyldsprdv4(dtl.wo.gov,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)
dtcreditcip.wo.gov<-create.dev.long2(prwin = dtm$prw,creditmispin = ys.wo.gov$regresult,netmispin = yseff.wo.gov$regresult)
creditcip.result<-plot.panel.creditcip(dtm$prw,ys.wo.gov$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf


# try out new things ------------------------------------------------------
if (FALSE) {
  #' alternate ccy pairs
  bb<-(dtl[ccy %in% c('eur','gbp')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore))$regresult
  
  bb[ccy=='gbp'][,.(date,est)] %>% ggplotw()
  #' Additional FE
  
  aa<-bondref[str_to_lower(ccy) %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
  aa[,secur:=str_to_lower(secur)]
  #' floating notes
  aa[,f.floating:=ifelse(str_to_lower(secur) %like% 'fl|fr' | str_to_lower(descr) %like% 'flt|fl ',TRUE,FALSE)]
  aa[,.N,f.floating]
  #' 144a
  sdc[,.N,.(rule144a)]
  
  # senior vs sub
  aa[,.N,secur %like% 'senior|sr']
  aa[secur %like% 'senior|sr'][,.N,.(secur)][order(-N)]
  aa[,.N,secur %like% 'sub']
  aa[secur %like% 'sub'][,.N,.(secur)][order(-N)]
  #
  # collateral, secured vs unsecured
  
  #' private placement
  aa[,.N,str_to_lower(securityType) %like% 'priv']
  #' amt
  
  
  
  # step up
  aa[,.N,secur %like% 'step'| str_to_lower(descr) %like% 'step']
  
  #
  
  #
  aa %>% ds()
  aa[,.N,name] %>% head(20)
  
  aa %>% ds
  aa[,.N,secur %like% 'pri']
  
  aa[,.N,str_to_lower(typesec)==str_to_lower(secur)]
  
  aa [,.N,.(tf_mid_desc)][order(-N)] %>% head(50) %T>% dt2clip()
  aa [,.(.N,sum(na.omit(amt))),.(tf_mid_desc)][order(-V2)] %>% head(50) %T>% dt2clip()
  
  
  aa[str_to_lower(tf_mid_desc) %like% 'govern'][,.N,.(tf_mid_desc)][order(-N)]
  aa[,.N,secur %like% 'step'| str_to_lower(descr) %like% 'step']
  aa[,.N,secur %like% 'step']
  
  
  aa %>% ds
  aa[,.N,securityType]
  aa %>% ds
  aa[,.N,.(rule144a)][order(-N)] %T>% dt2clip()
  sdc[,.N,.(rule144a)]
  sdc[str_to_lower(ccy) %in% c('usd','eur','gbp','jpy','aud','chf','cad')][!is.na(isin) | !is.na(cu)][!is.na(amt)][,.N]
}
# filter iss --------------------------------------------------------------
#'
if (getOption('gl')$sdc.filter=='bondrefall'){
  dtissraw<-bondrefall %>% issfilter(type=6) 
} else{
  dtissraw<-sdc %>% filter.sdc()
}
dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtissraw<-(dtissraw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6] %>% add.earlist.iss.in.ccy()
#'
# VAR  ---------------------------------------------------------
#+ VAR calc, include=FALSE,eval=getOption('gl')$run.var
  
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
  
  if (getOption('gl')$sdc.filter.var$restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
    dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
  } else {dtiss.in<-dtissraw}
  dtiss.collapse.m <- foreach(iccy=c('eur')) %do% {
    dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=getOption('gl')$sdc.filter.var$collapse.filter)
  } %>% rbindlist() %>% setkey(date,ccy)
  dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
  dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]; # dtreg.m  %>% write.dta('vardatain_1007.dta')
#' ### VAR using R: Flow Credit CIP
#+ eval=getOption('gl')$run.var
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,credit,cip)]
  res1<-vars::VAR(dtvar,1)
  resirf<-vars::irf(res1,ci=.95) 
  resirf %>% plot.irf() %>% ggsave(file='../paper/figures/active_oirf_icb.pdf',.,width=8,height=5)
  
#' ### VAR using R: Flow CIP Credit
#+ eval=getOption('gl')$run.var
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,cip,credit)]
  res1<-vars::VAR(dtvar,1)
  resirf<-vars::irf(res1,ci=.95) 
  resirf %>% plot.irf() %>% ggsave(file='../paper/figures/active_oirf_ibc.pdf',.,width=8,height=5)
#' ### SVAR using R with partial identification
#+ eval=getOption('gl')$run.var
  amat <- diag(3)
  diag(amat) <- 1
  amat[2,1] <- NA
  amat[3,1] <- NA
  amat[3,2] <- NA
  amat[2,3] <- NA
## A matrix
  res2<-vars::SVAR(res1,Amat=amat,estmethod = 'direct')
  resirf<-vars::irf(res2,runs=500) 
  resirf %>% plot.irf() %>% ggsave(file='../paper/figures/active_oirf_partial.pdf',.,width=8,height=5)

#+ eval=getOption('gl')$run.var
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,netmisp)]
  res1<-vars::VAR(dtvar,1)
  resirf<-vars::irf(res1,ci=.95,impulse='netmisp',response='i_netflow') 
  resirf %>% plot.irf.single() %>%  ggsave(file='../paper/figures/active_oirf_inetdev.pdf',.,width=8,height=5)
  
#+ statacalc,include=FALSE,eval=(getOption('gl')$run.var & getOption('gl')$run.stata)
  stata('clear matrix
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
      *graph export "../paper/figures/VAR_irfeur_A.png",replace
      * A
      var i_netflow credit cip if strccy=="eur", lags(1)
      irf create eur_oirf, set(irf1,replace) step(10)
      irf describe eur_oirf
      irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
      graph export "../paper/figures/VAR_oirfeur_A.png",replace
      
      * reorder
      var i_netflow cip credit if strccy=="eur", lags(1)
      irf create eur_oirf, set(irf1,replace) step(10)
      irf describe eur_oirf
      irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
      graph export "../paper/figures/VAR_oirfeur_B.png",replace
      
      * C: lags=4 according to AIC
      varsoc  i_netflow credit cip  if strccy=="eur"
      var i_netflow credit cip if strccy=="eur", lags(1/4)
      irf create eur_oirf, set(irf1,replace) step(10)
      irf describe eur_oirf
      irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
      graph export "../paper/figures/VAR_oirfeur_4lags.png",replace
      
      * D: c-b and iss
      gen netdev=netmisp
      var i_netflow netdev if strccy=="eur", lags(1)
      irf create oirf_eur, set(irf1,replace) step(10)
      irf describe oirf_eur
      irf graph oirf, irf(oirf_eur) impulse(netdev) response(i_netflow) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
      graph export "../paper/figures/VAR_oirfeur_netdeviss.png",replace
      ',data.in=dtreg.m)

dtreg.m %>% write.dta('temp.dta')
#+ echo=FALSE,include=TRUE,eval=getOption('gl')$run.var
#print('EUR IRF')
#include_graphics('../paper/figures/VAR_irfeur_A.png') 
print('EUR OIRF order: Issuance Credit CIP')
include_graphics('../paper/figures/VAR_oirfeur_A.png') 
print('EUR OIRF order: Issuance CIP Credi')
include_graphics('../paper/figures/VAR_oirfeur_B.png') 
print('EUR OIRF 4 lags order: Issuance Credit CI')
include_graphics('../paper/figures/VAR_oirfeur_4lags.png') 
print('EUR OIRF order: Issuance Net deviation (credit-cip)')
include_graphics('../paper/figures/VAR_oirfeur_netdeviss.png')


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


# Credit CIP -------------------------------------------------------------------
#' ## Graphs
#' credit deviations
#+ eval=getOption('gl')$show.credit.cip
dtcreditcip[ccy %in% c('eur','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))
ggsave(file='../paper/figures/active_credit.pdf',width=9,height=6)

#' CIP
#+ eval=getOption('gl')$show.credit.cip
dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))
# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)
#' 1y cip
#+ eval=getOption('gl')$show.credit.cip
dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs1,bpbs1,jybs1,adbs1)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 1-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))
# ggsave(file='../paper/figures/fig2_cip_1y_160929.pdf',width=9,height=6)

#' Graphing CIP and credit mispriing overlay
#+ eval=getOption('gl')$show.credit.cip, fig.width=8
creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename='../paper/figures/active_panel_usd.pdf',yrstr.='5',wide=T) 

#' scatter
#+ eval=getOption('gl')$show.credit.cip
aa<-creditcip.result$dt.credit.cip[,.(ccy,cip,credit)]#[credit>-130]
aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Credit Spread Diff. in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=-80,y=40,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))
ggsave(file='../paper/figures/active_creditcipscatter.pdf',width=9,height=6)

#' Net deviation
#+ eval=getOption('gl')$show.credit.cip
ys1meff$regresult %>% plot.netdev(fileout='../paper/figures/active_netdev.pdf')

#' EUR credit cip overlay
#+ eval=getOption('gl')$show.credit.cip
creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
ggsave(file='../paper/figures/active_eurcreditcip.pdf',width=9,height=6)

#' HG LG
#+ eval=getOption('gl')$show.credit.cip
  dthlgrade<-dtl[ccy %in% c('usd','eur')]#[pub != 'Govt']#[date>ymd('2006-01-01')]
  ys_hy<-dthlgrade[nrating>6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
  ys_hg<-dthlgrade[nrating<=6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
  setnames(ys_hy$regcoef,'eur','highyield')
  setnames(ys_hg$regcoef,'eur','highgrade')
  ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit deviation (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))
  ggsave(file='../paper/figures/active_hghy_eur.pdf',width=9,height=6)

#' HG/LG for all ccy together
#+ eval=getOption('gl')$show.credit.cip, warning=F
  dtrating<-copy(dtl) %>% filterglobaluponly()
  invisible(yshg<-resyldsprdv4(dtrating[nrating %between% c(1,5)],prl,regversion=3,returndt=T,parallel.core.=mcore))
  invisible(yshy<-resyldsprdv4(dtrating[nrating>=6],prl,regversion=3,returndt=T,parallel.core.=mcore))
  dtplot.rating<-yshg$regresult[yshy$regresult,on=c('date','ccy')]
  dtplot.rating %>% setnames(c('est','i.est'),c('hg','lg'))
  dtplot.ratingl<-dtplot.rating %>% melt(id.vars=c('date','ccy'),measure.vars=c('hg','lg'))
  dtplot.ratingl %>% plot.panel.creditrating(filename='',wide=T)#../paper/figures/HGLG.pdf
  ggsave(file='../paper/figures/active_HGLG_all.pdf',width=10.5,height=6.5)

#' Monthly Level Reg
#+ eval=getOption('gl')$show.credit.cip
reg_creditcip<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcip[[length(reg_creditcip)+1]]<-creditcip.result$dt.credit.cip[ccy==iccy] %>% neweymod('credit~cip',value.name=iccy)
}
regtable1<-(rbindlist(reg_creditcip) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
tableout<-regtable1[,.(rn,eur,gbp,jpy,aud,chf,cad)];tableout
try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'creditcipMolevel',showNA=F)) #no append here

credit.cip<-copy(creditcip.result$dt.credit.cip)
#+ eval=getOption('gl')$show.credit.cip,include=F
invisible({credit.cip[,D.cip:=cip-shift(cip,n=1,type='lag'),ccy]})
invisible({credit.cip[,D.credit:=credit-shift(credit,n=1,type='lag'),ccy]})
# individual diff regressions with robust S.E.
reg_creditcipb<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcipb[[length(reg_creditcipb)+1]]<-credit.cip[ccy==iccy] %>% regformat(formula='D.credit~D.cip',value.name=iccy)
}
regtable1b<-(rbindlist(reg_creditcipb) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
#' Monthly diff
#+ eval=getOption('gl')$show.credit.cip
regtable1b

#' ### quarterly diff
#+ eval=getOption('gl')$show.credit.cip,include=F
credit.cip.q<-copy(creditcip.result$dt.credit.cip)[order(date,ccy)][,date.qrt:=floor_date(date,'quarter')][,`:=`(cip.q=first(cip),credit.q=first(credit)),.(ccy,date.qrt)][,.(date.qrt,ccy,cip.q,credit.q)] %>% unique()
credit.cip.q[,D.cip:=cip.q-shift(cip.q,n=1,type='lag'),ccy]
credit.cip.q[,D.credit:=credit.q-shift(credit.q,n=1,type='lag'),ccy]
#' panel
#+ eval=getOption('gl')$show.credit.cip
credit.cip.q %>% felm(D.cip~D.credit|ccy|0|date.qrt,.) %>% stargazer(report='vct*',type='text')
#' individual diff regressions with robust S.E.
#+ eval=getOption('gl')$show.credit.cip
reg_creditcipb<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcipb[[length(reg_creditcipb)+1]]<-credit.cip.q[ccy==iccy] %>% regformat(formula='D.credit~D.cip',value.name=iccy)
}
regtable1b<-(rbindlist(reg_creditcipb) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1b


# Credit cip against other ccys -------------------------------------------
#' ### Credit cip against EUR
#+ eval=getOption('gl')$show.credit.cip
ys.eur<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = 'eur')
ys.eur.eff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'eur')
credit.cip.exact.eur<-(ys.eur$regresult[ys.eur.eff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
credit.cip.exact.eur[ccy %in% c('usd','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to EUR in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))
zz<-plot.panel.creditcip.any.ccy(ys.eur,ys.eur.eff,wide=T,filename='../paper/figures/active_panel_eur.pdf')
#' Without USD bonds in the calculation
#+ eval=F
ys.eur2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,returndt=T,mainccyin = 'eur')
ys.eur.eff2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'eur')
zz2<-plot.panel.creditcip.any.ccy(ys.eur2,ys.eur.eff2,wide=T)
#' ### Credit cip against GBP
#+ eval=getOption('gl')$show.credit.cip
ys.gbp<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = 'gbp')
ys.gbp.eff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'gbp')
credit.cip.exact.gbp<-(ys.gbp$regresult[ys.gbp.eff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
credit.cip.exact.gbp[ccy %in% c('usd','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to gbp in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'USD','JPY'),breaks=c('aud','gbp','usd','jpy'))
zz<-plot.panel.creditcip.any.ccy(ys.gbp,ys.gbp.eff,wide=T,filename='../paper/figures/active_panel_gbp.pdf')
#' Without USD bonds in the calculation
#+ eval=F
ys.gbp2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,returndt=T,mainccyin = 'gbp')
ys.gbp.eff2<-resyldsprdv4(dtl[ccy!='usd'],dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = 'gbp')
zz2<-plot.panel.creditcip.any.ccy(ys.gbp2,ys.gbp.eff2,wide=T)


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
  if (getOption('gl')$sdc.filter.iss.reg$restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
    dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
  } else {dtiss.in<-dtissraw}
#+ Monthly issuance calc,include=F
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=getOption('gl')$sdc.filter.iss.reg$collapse.filter)
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
  dtiss.in %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=getOption('gl')$sdc.filter.iss.reg$collapse.filter)
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

###########
#' Quick summary of issuance flow
#+ echo=T,include=T
dtreg.m[,.(iss_mean=mean(i_netflow),iss_sd=sd(i_netflow),iss_min=min(i_netflow),iss_max=max(i_netflow)),ccy]
dtreg.m[,.(Iss_mean=mean(I_netflow),Iss_sd=sd(I_netflow),Iss_min=min(I_netflow),Iss_max=max(I_netflow)),ccy]
dtreg.q[,.(iss_mean=mean(i_netflow),iss_sd=sd(i_netflow),iss_min=min(i_netflow),iss_max=max(i_netflow)),ccy]
dtreg.q[,.(Iss_mean=mean(I_netflow),Iss_sd=sd(I_netflow),Iss_min=min(I_netflow),Iss_max=max(I_netflow)),ccy]
dtreg.q[,.(date,i_netflow,ccy)] %>% ggplot(aes(date,i_netflow,colour=ccy))+geom_line()
dtreg.q[ccy=='eur',.(date,i_netflow,ccy)] %>% ggplot(aes(date,i_netflow,colour=ccy))+geom_line()
dtreg.q[ccy=='eur',.(date,i_netflow,netmisp)] %>% melt(id.vars='date') %>% ggplot(aes(date,value,colour=variable))+geom_line()
dtreg.q[ccy=='eur',.(date,100*(mu-mean(mu)),netmisp)] %>% melt(id.vars='date') %>% ggplot(aes(date,value,colour=variable))+geom_line()


###################################################################################################################################################
#' ## Monthly regressions of issuance on deviations
#+ echo=T,include=T
dtreg.m[date<ymd('2016-08-01')] %>% reg.newey.all(i_netflow6mf~netmisp)
dtreg.m %>% reg.newey.all(i_netflow6mf~netmisp+swapraterel)
tableout<-dtreg.m %>% reg.newey.all(i_netflow6mf~netmisp+swapraterel); tableout;try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'issnetdev',showNA=F,append=T))
tableout<-dtreg.m %>% reg.newey.all(I_netflow6mf~netmisp+swapraterel); tableout;try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'ISSnetdev',showNA=F,append=T))
#dtreg.m %>% reg.newey.all(i_netflow.smooth6mf~netmisp)
#dtreg.m %>% reg.newey.all(i_netflow.smooth6mf~netmisp+swapraterel)
#dtreg.m %>% reg.newey.all(i_netflow6mf~credit+cip)
tableout<-dtreg.m %>% reg.newey.all(i_netflow6mf~credit+cip+swapraterel);tableout
try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'isscreditcip',showNA=F,append=T))




#' ### regression of issuance on chg in credit
#+ include=T, echo=T,eval=F
reg.credit.chg<-list()
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(F.i_netflow~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow.smooth~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(i_netflow6mf~D.credit|ccy|0|0,.)
reg.credit.chg[[length(reg.credit.chg)+1]]<-dtreg.m %>% felm(I_netflow~D.credit|ccy|0|0,.)
stargazer::stargazer(reg.credit.chg,report='vct*',type='text')


#' ## Quarterly regressions of issuance on deviations
#+ echo=T,include=T
dtreg.q %>% reg.newey.all(F.i_netflow~netmisp)
tableout<-dtreg.q %>% reg.newey.all(F.i_netflow~netmisp+swapraterel);tableout;try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'issnetdevQ',showNA=F,append=T))
#dtreg.q %>% reg.newey.all(F.i_netflow~credit+cip)
tableout<-dtreg.q %>% reg.newey.all(F.i_netflow~credit+cip+swapraterel);tableout;try(tableout %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'isscreditcipQ',showNA=F,append=T))
#dtreg.q %>% reg.newey.all(F.I_netflow~netmisp)
#dtreg.q %>% reg.newey.all(F.I_netflow~netmisp+swapraterel)
#dtreg.q %>% reg.newey.all(F.I_netflow~credit+cip)
#dtreg.q %>% reg.newey.all(F.I_netflow~credit+cip+swapraterel)
dtreg.q %>% reg.newey.all(i_netflow~netmisp)
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
if (getOption('gl')$sdc.filter.iv.reg$restrict.to.dtl){ # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
    dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
  } else {dtiss.in<-dtissraw}
# Monthly
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtiss.in %>% icollapse4(iccy,collapse.freq = 'month',filter=getOption('gl')$sdc.filter.iv.reg$collapse.filter)
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
  dtiss.in %>% icollapse4.mature(iccy,collapse.freq = 'month',filter=getOption('gl')$sdc.filter.iv.reg$collapse.filter)
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
dtreg[,.(sd(I_both.iss),sd(I_both.mat))] 
regres.ls<-list();rsq.ls<-list();n.ls<-list()
for (iregres in regres){
 regres.ls[[length(regres.ls)+1]]<-((iregres %>% summary)$coef %>% as.data.table(keep.rownames=T))[,reg:=length(regres.ls)+1]
 rsq.ls[[length(rsq.ls)+1]]<-as.data.table((iregres %>% summary)$r.squared)[,reg:=length(rsq.ls)+1]
 n.ls[[length(n.ls)+1]]<-as.data.table((iregres %>% summary)$df[1:2] %>% sum())[,reg:=length(n.ls)+1]
}
dtout<-rbindlist(regres.ls)
dtout[,`t value`:=sprintf("[%.2f]",`t value`)]
dtout[,Estimate:=sprintf('%.3g',Estimate)]
dtout2<-(dtout %>% melt(id.vars=c('rn','reg'),measure.vars=c('Estimate','t value')))[order(-rn)] 
rsq.dt<-(rsq.ls %>% rbindlist)[,.(rn='zRsq',reg,variable='Rsq',value=sprintf('%.2f',V1))]
n.dt<-(n.ls %>% rbindlist)[,.(rn='zzN',reg,variable='N',value=sprintf('%.2f',V1))]
dtout3<-rbind(dtout2,rsq.dt)
dtout4<-rbind(dtout3,n.dt)
dtout5<-(dtout4 %>% dcast(rn+variable~reg))[order(rn,variable)]

#dtout5 %>% xlsx::write.xlsx(file='../paper/tables/activetables.xlsx',sheetName = 'IVreg',showNA=F,append=T)




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
#+ eval=getOption('gl')$run.stata
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



##########################################################

# test --------------------------------------------------------------------
#' ## redefining mu 
#+ eval=FALSE
## issuance
dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
lsiss<-list()
for(iccy in c('eur','usd','jpy','aud','cad','chf','gbp')){
  lsiss[[length(lsiss)+1]] <- dtiss.in %>% icollapse.againstall(iccy,collapse.freq = 'month')
};
dtiss<-rbindlist(lsiss) %>% setkey(date,ccy)
iss.weight<-dtiss[,sum(I_total),ccy][order(ccy)]

### credit cip
ysls <- list(); yseffls <- list(); cc.exact.ls <- list()
for (iccy in c('eur','usd','jpy','aud','cad','chf','gbp')){
  ysiccy<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = iccy)
  ysiccyeff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = iccy)
  cc.exact.ls[[length(cc.exact.ls)+1]] <- (ysiccy$regresult[ysiccyeff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff][,mainccy:=iccy]
}
dt.cc.exact<-rbindlist(cc.exact.ls) %>% setkey(mainccy,ccy,date)
# save(dt.cc.exact,file='temp_dtccexact.RData')
# geometric avg
# cc.avg<-dt.cc.exact[,.(credit.avg=mean(credit),cip.avg=mean(cip),netdev.avg=mean(crediteff)),.(date,mainccy)][,date:=floor_date(date,'month')] %>% setnames('mainccy','ccy') %>% setkey(date,ccy)
# weighted avg
cc.avg<-dt.cc.exact[order(date,mainccy,ccy)][,.(netdev.avg=weighted.mean(crediteff, iss.weight[ccy!=.BY$mainccy,V1])),.(date,mainccy)][,date:=floor_date(date,'month')] %>% setnames('mainccy','ccy') %>% setkey(date,ccy)
dtreg<-cc.avg[dtiss]

dtreg[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg[,mu_3mf:=(shift(mu,n=1,type='lead')+shift(mu,n=2,type='lead')+shift(mu,n=3,type='lead'))/3,ccy]
dtreg[,mu_6mf:=(shift(mu,n=1,type='lead')+shift(mu,n=2,type='lead')+shift(mu,n=3,type='lead')+shift(mu,n=4,type='lead')+shift(mu,n=5,type='lead')+shift(mu,n=6,type='lead'))/6,ccy]
dtreg[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg[,FD.mu:=shift(D.mu,n=1,type='lead'),ccy]
dtreg[,D.netdev.avg:=netdev.avg-shift(netdev.avg,n=1,type='lag'),ccy]

res1 <- list()
res1[[length(res1)+1]] <- dtreg %>% felm(D.mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(D.mu~D.netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(F.mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(FD.mu~netdev.avg|ccy,.) 
res1 %>% stargazer(type='text',report='vct*')

dtreg %>% reg.newey.all2(mu~netdev.avg)
dtreg %>% reg.newey.all2(F.mu~netdev.avg)
dtreg %>% reg.newey.all2(F.mu~netdev.avg)
dtreg %>% reg.newey.all2(mu_3mf~netdev.avg)
dtreg %>% reg.newey.all2(mu_6mf~netdev.avg)


dtreg[ccy=='usd',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='eur',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='jpy',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='aud',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='cad',.(date,mu_3mf,netdev.avg)] %>% ggplotw
#dtreg[ccy=='usd',.(date,mu_3mf,netdev.avg)] %>% ggplotw
# dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% ggplotw
# dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% lm(mu~netdev.avg,.) %>% summary
# zz<-dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% neweymod(mu~netdev.avg);zz;zz



#' ## Impact of large issuance on basis


icollapse4.daily<-function(dtin.,ccyA=dtin.[str_to_lower(ccy) %nlk% 'usd'][1,str_to_lower(ccy)],collapse.freq='month',filter=0){
  # newer version of collapsing 
  # todo: construct and use modupccy
  #ccyA="eur";dtin.<-dtin
  
  if (ccyA=='eur') natA<-'eurozone'
  
  
  if (filter==1){ # issued in both ccy before
    dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][d>=earlist.usd & d>=eval(exparse(str_c('earlist.',ccyA)))]
  } else if (filter==2){ # issued in both ccy ever (before and after)
    dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
  }
  
  #print(str_c('collapsing using: ', ccyA, ' ',natA))
  dtin<-copy(dtin.[d>=ymd('2002-01-01')])
  dtin[,ccy:=str_to_lower(ccy)]
  dtin[,modnat:=str_to_lower(modnat)]
  dtin<-dtin[modnat %in% c(natA,'united states') & ccy %in% c('usd','1usd',ccyA)]
  natA=str_to_lower(natA)
  dtin[,date:=d]
  
  # basic summation in this section: cannot yet do add/subract/divid/multiple since rows are different
  # Yankee isuance
  dtin[modnat==natA & ccy %like% 'usd|1usd',I_fUSD:=sum(na.omit(amt))/1000,by=date]
  # reverse yankee
  dtin[modnat=='united states' & ccy==ccyA,I_usF:=sum(na.omit(amt))/1000,by=date]
  # issuance from both countries in either currencies
  dtin[,I_both:=sum(na.omit(amt))/1000,by=date]
  # mu: issuance only in usd/total issuance
  dtin[ccy %like% 'usd|1usd',I_usd_tot:=sum(na.omit(amt))/1000,by=date]
  
  # first collapse into unique values by all columns, but each of the variables appear on a different row
  dt2<-dtin[,.(date,I_fUSD,I_usF,I_both,I_usd_tot)] %>% unique()    
  # get rid of NAs by combining rows of identical yrmo, first melt into long, get rid of NAs, then cast back to wide
  dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')))[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})
  
  # when there are no flow for a certain month, use zero
  dtout[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][is.na(I_usd_tot),I_usd_tot:=0]
  
  # Calculations based on earlier summations: net flow are net Yankee flow
  dtout[,I_netflow:=I_fUSD-I_usF][,i_netflow:=I_netflow/I_both*100][,mu:=I_usd_tot/I_both]
  
  # cacluate a smooth version
  dtout[,I_both_12m.L.avg:=rowMeans(dtout[,shift(I_both,n=0:11,type='lag')])]
  dtout[,i_netflow.smooth:=I_netflow/I_both_12m.L.avg*100] 
  
  # check to make sure there's no dates missing in case there is a month without any issuance at all!
  # dtout<-dtout %>% expandfulldates(.,freq='daily') %>% as.data.table()
  # if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');browser()}
  dtout[,ccy:=ccyA]
  dtout %>% setkey(date,ccy)
  dtout
}

dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
dtiss.in<-dtissraw
dtiss.collapse.d <- dtiss.in %>% icollapse4.daily('eur',collapse.freq = 'daily',filter=getOption('gl')$sdc.filter.var$collapse.filter) %>% setkey(date,ccy)

#dtin.=dtiss.in;ccyA='eur';collapse.freq='daily';filter=0


#' #### add 3s6s basis as well later
eubs5<-prl[ticker=='eubs5',.(date,value)] %>% setnames('value','cip') %>% setkey(date)
dtiss.collapse.d %>% setkey(date)
dtreg<-dtiss.collapse.d[eubs5]
#+ include=F
dtreg[is.na(I_netflow),I_netflow:=0]
dtreg[,D.cip:=cip-shift(cip,n=1,type='lag')]
dtreg[,D1.cip:=shift(cip,n=1,type='lead')-shift(cip,n=1,type='lag')]
dtreg[,D2.cip:=shift(cip,n=2,type='lead')-shift(cip,n=2,type='lag')]
dtreg[,D3.cip:=shift(cip,n=3,type='lead')-shift(cip,n=3,type='lag')]
dtreg[,D4.cip:=shift(cip,n=4,type='lead')-shift(cip,n=4,type='lag')]
dtreg[,D5.cip:=shift(cip,n=5,type='lead')-shift(cip,n=5,type='lag')]
dtreg[,D6.cip:=shift(cip,n=6,type='lead')-shift(cip,n=6,type='lag')]

#+ include=T,echo=T
resls <- list()
resls[[length(resls)+1]] <- dtreg %>% felm(D.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D1.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D2.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D3.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D4.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D5.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D6.cip~I_netflow,.)
stargazer(resls,type='text',report='vct*')

resls <- list()
resls[[length(resls)+1]] <- dtreg %>% felm(D.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D1.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D2.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D3.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D4.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D5.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D6.cip~i_netflow,.)
stargazer(resls,type='text',report='vct*')
save.image(file='results161010.RData')