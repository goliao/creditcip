# load data ---------------------------------------------------------------
rm(list=ls(all=TRUE));
load('../db/dtlmo.rdata');load('../db/bondref.RData');load('../db/prl.RData');load('../db/monthenddates.RData');
load('../db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('../util.r')


# Options -----------------------------------------------------------------
# redo preprocessing
preprocess=0
# set dtissraw to bondrefall %>% issfilter()

# use original bond data or current expanded

# set firm level to either upcusip or cu
firmlevel <- 'upcusip'

# individually constructing pairwise credit spread or construct all at the same time

# issfiltertype

# collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance

# taking quarterly credit/cip to be last date, or average of 3 end of month, or first month
#last

# individually generated residualized spread or generated together
bool.ind.resid <- 0

# preprocess data for pricing calculations -----------------------------------------------------------
if (preprocess==1){
  bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
  dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
} else {
  load('../preprocessedtemp.RData')
}
dtl<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
glaw<-merge(bondref[,.(pk,deal_no)],dtissraw[!is.na(Eglaw) & Eglaw!='NA',.(deal_no,Eglaw)],by='deal_no',all.x=T)
dtllaw<-merge(dtl,glaw[,.(pk,Eglaw)],by='pk')
dtllaw[,.N,.(Eglaw)][order(-N)]
# preprocess data for issuance flow -----------------------------------------------------------
dtissraw<-read.dta13('../sdc96_clean3.dta')	%>% as.data.table()
# dtissraw<-bondrefall %>% issfilter(type=6) 
dtissraw[,ccy:=str_to_lower(ccy)]
dtiss1<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtiss1[,sum(amt)/1000,ccy]
dtissraw<-dtissraw[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
dtissraw %>% tocusip6(field=firmlevel)
dtissraw[,upcusip:=cusip6]
bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]


# calculate residualize credit spread -----------------------------------------------------------
ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = 1)
#extension
ysglaw<-resyldsprdv4(dtllaw[Eglaw!='NA'],dtm$prl,regversion=4.5,returndt=T,parallel.core. = 1)
figext<-ggplotw.comp2(ys1m$regcoef,ysglaw$regcoef,c('residualized credit spread diff.','with additional controls'))

ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
# reshape to long and merge with cip
dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)
# cip as credit-net
credit.cip.exact<-(ys1m$regresult[ys1meff$regresult] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]

save.image(file = 'img_calc.RData')