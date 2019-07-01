






## this file uses CMT treasuries in EUR, JPY, CHF and GBP to generate credit spread against treasury and compares it against cip deviaion



rm(list=ls(all=TRUE));
load('../db/dtlmo.rdata');load('../db/bondref.RData');load('../db/prl.RData');load('../db/monthenddates.RData');
load('../db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('../util.r')

cmt <- fread('../cmtyld.csv',header = T)
cmt[,date:=mdy(date)]
cmt[,ccy:=str_sub(ccy,4)]
cmtl <- cmt %>% melt(id.vars=c('date','ccy'),variable.name='tenor',variable.factor=F)
cmtl[,tenor:=as.numeric(tenor)]



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

preprocess<-function(bondref,dtl,prl,monthlyonly=TRUE,issfiltertype=2,ccyfilter=c('usd','eur','jpy','gbp','aud','cad','chf')){
  tic()
  dtl<-copy(dtl)
  
  # prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
  br<-bondref[!is.na(pk)] %>% issfilter(.,type=issfiltertype)
  br[,ccy:=tolower(ccy)]
  br <- br[ccy %in% ccyfilter]
  br <- br %>% semi_join(dtl,by='pk') %>% as.data.table()
  setkey(br,pk)
  if (nrow(showdups(br,'pk'))!=0){
    print('error: duplicate pks; dedupe based on amt')
    br<-br[order(pk,-amt)]
    setkey(br,pk)
    br<-unique(br)
  }
  # MERGE RATING AND ADD MATURITY BUCKETS -----------------------------------
  setkey(dtl,pk);setkey(br,pk)
  if ('monthend' %in% ds(dtl)){
    if (monthlyonly) {
      dtl<-dtl[monthend==1]
      prl<-prl[monthend==1]
    } else{
      print('daily data')
      pk_daily0<-unique(dtl[monthend==0,.(pk)])
      # alternatively, count pk as daily obs if there are more than three times as many daily obs as monthly obs
      pkcount<-dtl[,.N,.(pk,monthend)] %>% dcast.data.table(pk~monthend)
      pk_daily<-pkcount[`0`>3*`1`,.(pk)]
      setkey(pk_daily,pk)
      dtl<-dtl[pk_daily]
    }
  }
  
  br[DEBT_SENR_CD=='SU',senior:='SU']
  br[DEBT_SENR_CD=='SS',senior:='SS']
  br[DEBT_SENR_CD %in% c('SB','SR') ,senior:='SB']
  br[is.na(senior),senior:='UN']
  
  br[is.na(amt) & !is.na(FACE_US_AMNT),amt:=FACE_US_AMNT]
  br[,amt_bucket:=as.numeric(NA)]
  br[amt>=100,amt_bucket:=ntile(amt,4)]
  br[amt<100,amt_bucket:=0]
  br[is.na(amt),amt_bucket:=-1]
  
  dtl2<-dtl[br[,.(ccy,mat2,nrating,upcusip,cu,pk,ytofm,mdealtype,secur,issue_type_desc,pub,tf_mid_desc,sic1,amt,amt_bucket,DEBT_CLASS_CD,MKT_TYP_CD,senior)],nomatch=0]
  dtl2[,ytm:=as.numeric((mat2-date)/365)]
  dtl3<-dtl2[ytm >.05]
  dtl3[is.na(nrating),nrating:=0]
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='YLD_YTM_MID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.1,1.0)]
  dtl3[liq<.25,liq_bucket:=0] # more illiq
  dtl3[liq>=.25 & liq<.5,liq_bucket:=1] # more illiq
  dtl3[liq>=.5 & liq<.75,liq_bucket:=2] # liq
  dtl3[liq>=.75 & liq<=1,liq_bucket:=3] # liq
  
  #browser()
  #dtl4<-dtl.addswapsprd(dtl3,prl)
  toc()
  list('dtl'=dtl3,'br'=br)
}
bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)

## interpolate to each bond maturity
dtl <- dtm$dtl[ccy %ni% c('aud','cad')] %>% copy()

setkey(cmtl,date,ccy,tenor)  
setkey(dtl,date,ccy)

# find out what treasury prices are missing
dates2interp<-dtl[!is.na(ytm) & wday(date) %between% c(2,6),.N,by=.(date,ccy)]
setkey(dates2interp,date,ccy)
sp2interp<-cmtl[,.N,.(date,ccy)][str_length(ccy)==3]
missingswap<-sp2interp[dates2interp][is.na(N) | N<3]
setkey(missingswap,date,ccy)
print('missing swap prices on these dates for these ccys:')
print(missingswap)

dtl <- dtl %>% anti_join(missingswap,by=c('date','ccy')) %>% as.data.table()
setkey(dtl,date,pk,value)

dtl<-dtl[!is.na(ytm) & wday(date) %between% c(2,6)]

cmtl<-cmtl %>% na.omit()
dtl[,treasyld:=intrwrap(.SD,cmtl,.BY,interprule=2),by=.(date,ccy)][treasyld==0,treasyld:=NA]
dtl[,treassprd:=100*(value-treasyld)]
dtl<-dtl[!is.na(treassprd)] #get rid of ones that can't be interpolated for one reason or another


dtl<-(dtl %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]

# calculate residualize credit spread -----------------------------------------------------------
resyldsprdv4<-function(dtlin,regversion=2,globaluponly=1,returndt=T,fld='treassprd',winsor.=.025,parallel.core.=1,mainccyin='usd'){
  # a wrapper for FE regression
  tic()
  dtl<-copy(dtlin)
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]
  
  if (globaluponly){ # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  
  lsout<-getccyFE2(dtl,fieldstr=fld,version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)

  toc()
  if (returndt)
    lsout
  else
    lsout$regcoef
}

dtltreasury<-dtl %>% copy()
ys1m<-resyldsprdv4(dtltreasury,regversion=4,returndt=T,parallel.core. = 1)

rcl <- ys1m$regcoef %>% melt(id.vars='date')
rcl[value<40 & value>-150] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()



# compare with swap sprd --------------------------------------------------

load('img_calc.RData')
ss <- ys1m$regcoef %>% melt(id.vars='date')

stcomp <- merge(rcl,ss,by=c('date','variable'),suffixes = c('TS','SS')) %>% melt(id.vars=c('date','variable'),variable.name='type')
stcomp[,type:=str_sub(type,6)]
stcomp %>% setnames('variable','ccy')

stcomp[ccy=='eur'][value<40 & value>-200] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()
stcomp[ccy=='jpy'][value<40 & value>-200] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()
stcomp[ccy=='gbp'][value<40 & value>-200] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()
stcomp[ccy=='chf'][value<40 & value>-200] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()

