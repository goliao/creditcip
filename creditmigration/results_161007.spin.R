#' ---
#' title: "Results"
#' date: "Oct 07,2016"
#' output: html_document
#' ---
#+ setup, include=FALSE
library(knitr)
opts_chunk$set(echo=FALSE,include=TRUE,cache=TRUE)
#setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
if (Sys.info()['sysname'][[1]]=='Windows') mcore=1 else mcore=4
#' Options -----------------------------------------------------------------
gl <- list('run.var'=TRUE, 
           'run.stata'=TRUE,
           'show.credit.cip'=FALSE, # graph credit cip, run regressoin
           'individual.resid'=FALSE,# individually generated residualized spread or generated together
           'sdc.filter'='6ccyv3',# set iss filter options: bondrefall, 6ccyv1 6ccyv2
           'firmlevel'='upcusip',
           'mcore'= mcore,
           'quickstart'=TRUE # to use previously saved results in the begining
           )
options(gl=gl)
(getOption('gl') %>% as.data.table())[,rn:=1] %>% melt('rn')

# getOption('gl')$mcore

# use original bond data or current expanded

# set firm level to either upcusip or cu
firmlevel <- getOption('gl')$firmlevel

# individually constructing pairwise credit spread or construct all at the same time

# issfiltertype

# collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance

# taking quarterly credit/cip to be last date, or average of 3 end of month, or first month
#last

# quick start -------------------------------------------------------------
if(getOption('gl')$quickstart){
  load('preprocessedtemp.RData');load('db/sdc.RData');load('db/prl.RData');load('db/monthenddates.RData');source('util.r')
} else{
  # load data ---------------------------------------------------------------
  load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
  load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
  source('util.r')
  load('db/sdc.RData')

  
  # preprocessing -----------------------------------------------------------
  
  bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
  dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
  
  dtl<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
  
  bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
  bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
  
  
  # residualize credit spread -----------------------------------------------------------
  
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
  save(bondref,bondrefall,dtm,dtl,ys1m,ys1meff,dtcreditcip,credit.cip.exact,file='preprocessedtemp.RData')
}

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
#'
# VAR  ---------------------------------------------------------
#+ VAR calc, include=FALSE,eval=getOption('gl')$run.var
  dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
  dtissraw<-(dtissraw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
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
  dtin2<-dtissraw %>% add.earlist.iss.in.ccy(dtissraw)
  dtin2 %>% setkey(upcusip,ccy)
  # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
  # dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
  registerDoParallel(1)#,'gbp','jpy','aud','chf','cad'
  dtiss.collapse.m <- foreach(iccy=c('eur')) %do% {
    dtin2 %>% icollapse4(iccy,collapse.freq = 'month',filter=1)
  } %>% rbindlist()
  dtiss.collapse.m %>% setkey(date,ccy)
  dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
  dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]; # dtreg.m  %>% write.dta('vardatain_1007.dta')
#' ### VAR using R: Flow Credit CIP
#+ eval=getOption('gl')$run.var
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,credit,cip)]
  res1<-vars::VAR(dtvar,1)
  resirf<-vars::irf(res1,ci=.95) 
  # res1 %>% summary
  resirf %>% plot.irf()
#' ### VAR using R: Flow CIP Credit
#+ eval=getOption('gl')$run.var
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,cip,credit)]
  res1<-vars::VAR(dtvar,1)
  resirf<-vars::irf(res1,ci=.95) 
  # res1 %>% summary
  resirf %>% plot.irf()
#' #### SVAR using R with partial identification
#+ eval=getOption('gl')$run.var
  amat <- diag(3)
  diag(amat) <- 1
  amat[2,1] <- NA
  amat[3,1] <- NA
  amat[3,2] <- NA
  amat[2,3] <- NA
## A matrix
  amat
  res2<-vars::SVAR(res1,Amat=amat,estmethod = 'direct')
#  summary(res2)
  resirf<-vars::irf(res2,runs=100) 
  resirf %>% plot.irf()

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


#+ echo=FALSE,include=TRUE,eval=getOption('gl')$run.var
print('EUR IRF')
include_graphics('../paper/figures/VAR_irfeur_A.png') 
print('EUR OIRF order: Issuance Credit CIP')
include_graphics('../paper/figures/VAR_oirfeur_A.png') 
print('EUR OIRF order: Issuance CIP Credi')
include_graphics('../paper/figures/VAR_oirfeur_B.png') 
print('EUR OIRF 4 lags order: Issuance Credit CI')
include_graphics('../paper/figures/VAR_oirfeur_4lags.png') 
print('EUR OIRF order: Issuance Net deviation (credit-cip)')
include_graphics('../paper/figures/VAR_oirfeur_netdeviss.png')



# #'## Data summary ------------------------------------------------------------
dtl.bonds<-dtl[,.SD[1],pk]
dtl.bonds.global<-(dtl %>% filterglobaluponly())[,.SD[1],pk]
invisible({dtl.bonds[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds[sic1==6,sicind:=6];dtl.bonds[sic1==9,sicind:=9] })
dtl.bonds<-dtl.bonds %>% bucketytofm()
lefthalf<-dtl.bonds %>% table.summary()
invisible({dtl.bonds.global[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds.global[sic1==6,sicind:=6];dtl.bonds.global[sic1==9,sicind:=9]})
dtl.bonds.global<-dtl.bonds.global %>% bucketytofm()
righthalf <- dtl.bonds.global %>% table.summary()
#' **Summary of bond data**
cbind(lefthalf,righthalf)


# Credit CIP -------------------------------------------------------------------
#' ## Graphs
#' credit deviations
#+ eval=getOption('gl')$show.credit.cip
dtcreditcip[ccy %in% c('eur','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))
# ggsave(file='../paper/figures/fig1_creditmisprice_160908.pdf',width=9,height=6)

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
creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf

#' scatter
#+ eval=getOption('gl')$show.credit.cip
aa<-creditcip.result$dt.credit.cip[,.(ccy,cip,credit)]#[credit>-130]
aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Credit Spread Diff. in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=-80,y=40,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))
#ggsave(file='../paper/figures/creditcipscatter.pdf',width=9,height=6)

#' Net deviation
#+ eval=getOption('gl')$show.credit.cip
ys1meff$regresult %>% plot.netdev()

#' EUR credit cip overlay
#+ eval=getOption('gl')$show.credit.cip
creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
# ggsave(file='../paper/figures/EURcreditcip160925.pdf',width=9,height=6)

#' HG LG
#+ eval=getOption('gl')$show.credit.cip
  dthlgrade<-dtl[ccy %in% c('usd','eur')]#[pub != 'Govt']#[date>ymd('2006-01-01')]
  ys_hy<-dthlgrade[nrating>6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
  ys_hg<-dthlgrade[nrating<=6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = mcore)
  setnames(ys_hy$regcoef,'eur','highyield')
  setnames(ys_hg$regcoef,'eur','highgrade')
  ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit deviation (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))
  # ggsave(file='../paper/figures/hghy_eur_160908.pdf',width=9,height=6)

#' HG/LG for all ccy together
#+ eval=getOption('gl')$show.credit.cip
  dtrating<-copy(dtl) %>% filterglobaluponly()
  yshg<-resyldsprdv4(dtrating[nrating %between% c(1,5)],prl,regversion=3,returndt=T,parallel.core.=mcore)
  yshy<-resyldsprdv4(dtrating[nrating>=6],prl,regversion=3,returndt=T,parallel.core.=mcore)
  dtplot.rating<-yshg$regresult[yshy$regresult,on=c('date','ccy')]
  dtplot.rating %>% setnames(c('est','i.est'),c('hg','lg'))
  dtplot.ratingl<-dtplot.rating %>% melt(id.vars=c('date','ccy'),measure.vars=c('hg','lg'))
  dtplot.ratingl %>% plot.panel.creditrating(filename='',wide=T)#../paper/figures/HGLG.pdf
  #ggsave(file='../paper/figures/HGLG.pdf',width=10.5,height=6.5)
#' Monthly Level Reg
#+ eval=getOption('gl')$show.credit.cip
reg_creditcip<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcip[[length(reg_creditcip)+1]]<-creditcip.result$dt.credit.cip[ccy==iccy] %>% neweymod('credit~cip',value.name=iccy)
}
regtable1<-(rbindlist(reg_creditcip) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1[,.(rn,eur,gbp,jpy,aud,chf,cad)] #%T>% dt2clip

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
#+ eval=getOption('gl')$show.credit.cip
credit.cip.q<-copy(creditcip.result$dt.credit.cip)[order(date,ccy)][,date.qrt:=floor_date(date,'quarter')][,`:=`(cip.q=first(cip),credit.q=first(credit)),.(ccy,date.qrt)][,.(date.qrt,ccy,cip.q,credit.q)] %>% unique()
#+ include=F
credit.cip.q[,D.cip:=cip.q-shift(cip.q,n=1,type='lag'),ccy]
credit.cip.q[,D.credit:=credit.q-shift(credit.q,n=1,type='lag'),ccy]
#' panel
#+ eval=getOption('gl')$show.credit.cip
credit.cip.q %>% felm(D.cip~D.credit|ccy|0|0,.) %>% summary
#' individual diff regressions with robust S.E.
#+ eval=getOption('gl')$show.credit.cip
reg_creditcipb<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcipb[[length(reg_creditcipb)+1]]<-credit.cip.q[ccy==iccy] %>% regformat(formula='D.credit~D.cip',value.name=iccy)
}
regtable1b<-(rbindlist(reg_creditcipb) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1b




# issuance flow and net dev -----------------------------------------------
#' ## issuance flow and net dev
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtin2<-dtissraw %>% add.earlist.iss.in.ccy(dtissraw)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
# dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()

#+ Monthly 6 month avg forward,include=F
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)
dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
# make issuance lead by one period
dtreg.m[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.m[,i_netflow6mf:=(shift(i_netflow,n=1,type='lead')+shift(i_netflow,n=2,type='lead')+shift(i_netflow,n=3,type='lead')+shift(i_netflow,n=4,type='lead')+shift(i_netflow,n=5,type='lead')+shift(i_netflow,n=6,type='lead'))/6,ccy]

#' ### Monthly regression with 6 month forwards
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] #%T>% dt2clip


#+ Baseline 6m fwd with swap rate control,include=F
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
#' #### reg iss on net dev with swap control
reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] #%T>% dt2clip

#+ Quarterlycalc,include=F
# collapsing each ccy pair
registerDoParallel(1)
dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
} %>% rbindlist()
dtiss.collapse.q %>% setkey(date,ccy)

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

#' #### reg F.issflow (next Qrt) netmisp: need at add rates control
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.q[ccy==iccy] %>% neweymod(F.i_netflow~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev #%T>% dt2clip


## abs net dev and D, all iss ----------------------------------------------
#' ## abs net dev and D, all iss ----------------------------------------------
#+ collapse each,include=F
dtin2<-dtissraw %>% add.earlist.iss.in.ccy(dtissraw)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# Monthly
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)

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
  dtin2 %>% icollapse4.mature(iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtmat.collapse.m %>% setkey(date,ccy)
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
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.iss+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ I_both.mat+L.D.abs.netmisp |ccy| 0 | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(I_both.iss~I_both.mat |ccy| 0 | date,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | rn,.)
regres[[length(regres)+1]]<-dtreg %>% felm(D.abs.netmisp ~ L.D.abs.netmisp |ccy| (I_both.iss~I_both.mat) | date,.)
stargazer(regres,type='text',report='*vct*',order=c('I_both.iss','I_both.mat','`I_both.iss(fit)`','L.D.abs.netmisp'),column.labels=c('reducedform','ols','1ststg','iv','ivtclust'))

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
