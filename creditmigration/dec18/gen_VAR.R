setwd("C:/Users/Gordon/Dropbox/Research/ccy basis/creditmigration/dec18")
source('../util.r')
load('img_calc.RData')


dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtissraw<-(dtissraw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6] %>% add.earlist.iss.in.ccy()
#'
# VAR  ---------------------------------------------------------
#+ VAR calc, include=FALSE,eval=getOption('gl')$run.var
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
           'mcore'= 4,
           'quickstart'=FALSE # to use previously saved results in the begining
)
options(gl=gl)



plot.irf<-function(resirf,v1='credit',v2='cip',v3='i_netflow',filename='',naturalarrange=F){
  require(latex2exp)
  labelv1<-'$\\kappa$'
  labelv2<-'$b$'
  labelv3<-'$\\mu$'
  dttemp<-list()
  dttemp[[1]]<-(resirf$irf %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="irf"]
  dttemp[[2]]<-(resirf$Upper %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Upper"]
  dttemp[[3]]<-(resirf$Lower %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Lower"]
  dtirf<-(dttemp %>% rbindlist %>% melt(id.vars=c('rn','type')) %>% dcast(rn+variable~type))[,rn:=as.numeric(rn)][order(rn)]  
  par(ask=F)  
  require('gridExtra')
  figirf<-list()
  figirf[[1]]<-dtirf[variable==str_c(v1,'.',v1)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv1)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  figirf[[3]]<-dtirf[variable==str_c(v1,'.',v2)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv2)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  figirf[[2]]<-dtirf[variable==str_c(v1,'.',v3)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv3)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  figirf[[6]]<-dtirf[variable==str_c(v2,'.',v1)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv1)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  figirf[[4]]<-dtirf[variable==str_c(v2,'.',v2)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv2)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  figirf[[5]]<-dtirf[variable==str_c(v2,'.',v3)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv3)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  
  if (!naturalarrange){
    figirf.all<-grid.arrange(figirf[[1]],figirf[[2]],figirf[[3]],figirf[[4]],figirf[[5]],figirf[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
  } else {
    figirf.all<-grid.arrange(figirf[[1]],figirf[[3]],figirf[[2]],figirf[[5]],figirf[[6]],figirf[[4]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
  }
  #if(filename!='') ggsave(file=filename,figirf.all,width=8,height=5)
  figirf.all
} 

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
#' ### Single currency VAR using R: Flow Credit CIP
#+ eval=getOption('gl')$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,credit,cip)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95) 
resirf %>% plot.irf()+ggitle('EUR VAR flow credit cip')# %>% ggsave(file='../../figures/EUR_oirf_icb.pdf',.,width=8,height=5)

#' ### EUR VAR using R: Flow CIP Credit
#+ eval=getOption('gl')$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,cip,credit)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95) 
resirf %>% plot.irf() # %>% ggsave(file='../../figures/EUR_oirf_ibc.pdf',.,width=8,height=5)
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
resirf %>% plot.irf() #%>% ggsave(file='../../figures/active_oirf_partial.pdf',.,width=8,height=5)

#+ eval=getOption('gl')$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,netmisp)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95,impulse='netmisp',response='i_netflow') 
resirf %>% plot.irf.single() #%>%  ggsave(file='../paper/figures/active_oirf_inetdev.pdf',.,width=8,height=5)

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
      *graph export "VAR_irfeur_A.png",replace
      * A
      var i_netflow credit cip if strccy=="eur", lags(1)
      irf create eur_oirf, set(irf1,replace) step(10)
      irf describe eur_oirf
      irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
      graph export "VAR_oirfeur_A.png",replace
      
      * reorder
      var i_netflow cip credit if strccy=="eur", lags(1)
      irf create eur_oirf, set(irf1,replace) step(10)
      irf describe eur_oirf
      irf cgraph  (eur_oirf credit credit oirf,`graphopt\')  (eur_oirf credit cip oirf,`graphopt\')  (eur_oirf credit i_netflow oirf,`graphopt\') (eur_oirf cip credit oirf,`graphopt\') (eur_oirf cip cip oirf,`graphopt\')  (eur_oirf cip i_netflow oirf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
      graph export ".VAR_oirfeur_B.png",replace
      
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
      graph export "VAR_oirfeur_netdeviss.png",replace
      ',data.in=dtreg.m)

dtreg.m %>% write.dta('temp.dta')
# #+ echo=FALSE,include=TRUE,eval=getOption('gl')$run.var
# #print('EUR IRF')
# #include_graphics('../paper/figures/VAR_irfeur_A.png') 
# print('EUR OIRF order: Issuance Credit CIP')
# include_graphics('../paper/figures/VAR_oirfeur_A.png') 
# print('EUR OIRF order: Issuance CIP Credi')
# include_graphics('../paper/figures/VAR_oirfeur_B.png') 
# print('EUR OIRF 4 lags order: Issuance Credit CI')
# include_graphics('../paper/figures/VAR_oirfeur_4lags.png') 
# print('EUR OIRF order: Issuance Net deviation (credit-cip)')
# include_graphics('../paper/figures/VAR_oirfeur_netdeviss.png')
