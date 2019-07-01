rm(list=ls(all=TRUE));
load('../db/dtlmo.rdata');load('../db/bondref.RData');load('../db/prl.RData');load('../db/monthenddates.RData');
load('../db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('../util.r')
load('../gldb.RDATA');load('../db/bondref.RData')
dtl<-dtl[monthend==1]
dtl %>% setkey('pk')

#dtloas<-dtl %>% copy()
#dtl2<-dtl[bondref[,.(ccy,mat2,rating,nrating,upcusip,pk,isin,ytofm,sicfac,sic1)],nomatch=0]
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

preprocess<-function(bondref,dtl,prl,monthlyonly=TRUE,issfiltertype=2,ccyfilter=c('usd','eur','jpy','gbp','aud','cad','chf')){
  tic()
  dtl<-copy(dtl)
  #prl<-copy(prl)
  #prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
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
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='OAS_SPREAD_BID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
  prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
  prw<-prl %>% distinct() %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value')
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.1,1.0)]
  dtl3[liq<.25,liq_bucket:=0] # more illiq
  dtl3[liq>=.25 & liq<.5,liq_bucket:=1] # more illiq
  dtl3[liq>=.5 & liq<.75,liq_bucket:=2] # liq
  dtl3[liq>=.75 & liq<=1,liq_bucket:=3] # liq
  
  # 
  # prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
  # prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
  # prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
  # prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
  # prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
  # prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz5=cdsw5+cdbs5,cdsz7=cdsw7+cdbs7,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz30=cdsw30+cdbs30)]
  # prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz5=sfsw5+sfbs5,sfsz7=sfsw7+sfbs7,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz30=sfsw30+sfbs30)]
  # # prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz3=cdsw3+cdbs3,cdsz4=cdsw4+cdbs4,cdsz5=cdsw5+cdbs5,cdsz6=cdsw6+cdbs6,cdsz7=cdsw7+cdbs7,cdsz8=cdsw8+cdbs8,cdsz9=cdsw9+cdbs9,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz25=cdsw25+cdbs25,cdsz30=cdsw30+cdbs30)]
  # # prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz3=sfsw3+sfbs3,sfsz4=sfsw4+sfbs4,sfsz5=sfsw5+sfbs5,sfsz6=sfsw6+sfbs6,sfsz7=sfsw7+sfbs7,sfsz8=sfsw8+sfbs8,sfsz9=sfsw9+sfbs9,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz25=sfsw25+sfbs25,sfsz30=sfsw30+sfbs30)]
  # # prw[,approx_eubs1:=(eur/(eur+eur12m/10000)*(1+ussw1/10000)-(1+eusw1/10000))*10000]
  # # prw[,approx_eubs5:=((eur/(eur+eur5y/10000))^(1/5)*(1+ussw5/10000)-(1+eusw5/10000))*10000]
  # # transform prw back to prl
  # prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
  # prl<-prl[!is.na(value)]
  # #browser()
  # dtl4<-dtl.addswapsprd(dtl3,prl)
  toc()
  list('dtl4'=dtl3,'br'=br)
}

#####################
dtl<-copy(dtloas)
br<-bondref[!is.na(pk)]# %>% issfilter(.,type=4)
br[,ccy:=tolower(ccy)]
br <- br[ccy %in% c('usd','eur','jpy','gbp','aud','cad','chf')]
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
if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='OAS_SPREAD_BID']
dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
prw<-prl %>% distinct() %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value')
dtl3<-dtl3[date>'2004-01-01']
dtl3[,liq:=ytm/ytofm]
dtl3<-dtl3[liq %between% c(0.1,1.0)]
dtl3[liq<.25,liq_bucket:=0] # more illiq
##########################
# preprocess data for pricing calculations -----------------------------------------------------------
bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
dtm<-preprocess(bondref,dtloas,prl,issfiltertype =4)

tocusip6<-function(dtin, field='upcusip'){
  dtin[,cusip6:=str_sub(upcusip,1,6)]  
  dtin[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
}
dtloas2<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6] %>% copy()

# preprocess data for issuance flow -----------------------------------------------------------
dtissraw<-read.dta13('sdc96_clean3.dta')	%>% as.data.table()
# dtissraw<-bondrefall %>% issfilter(type=6) 
dtissraw[,ccy:=str_to_lower(ccy)]
dtiss1<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtiss1[,sum(amt)/1000,ccy]
dtissraw<-dtissraw[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
dtissraw %>% tocusip6(field=firmlevel)
dtissraw[,upcusip:=cusip6]
bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
load('img_calc.RData')


resyldsprdv4<-function(dtlin,pricein,regversion=2,fld='OAS_SPREAD_BID',globaluponly=1,returndt=T,adjccybs=0,winsor.=.025,parallel.core.=1,mainccyin='usd'){
  # a wrapper for FE regression
  tic()
  dtl<-copy(dtlin)
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]
  
  if (globaluponly){ # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  if (adjccybs==1)
    lsout<-getccyFE2(dtl,fieldstr='swapsprdadj',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  else
    lsout<-getccyFE2(dtl,fieldstr=fld,version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  toc()
  if (returndt)
    lsout
  else
    lsout$regcoef
}
# calculate residualize credit spread -----------------------------------------------------------
ys1m<-resyldsprdv4(dtloas2,dtm$prl,regversion=4,fld='OAS_SPREAD_BID',returndt=T,parallel.core. = 1)

#dtl.new<-dtl %>% copy()
dtl<-dtloas2 %>% as.data.table() %>% copy()
setkey(dtl,date)
dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

if (globaluponly){ # get rid of up where up doesn't have bonds in both ccys for each date
  dtl<-filterglobaluponly(dtl)
}
if (adjccybs==1)
  lsout<-getccyFE2(dtl,fieldstr='swapsprdadj',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
else
  lsout<-getccyFE2(dtl,fieldstr='',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)





ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
# reshape to long and merge with cip


############

resyldsprdv4<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=T,adjccybs=0,winsor.=.025,parallel.core.=1,mainccyin='usd'){
  # a wrapper for FE regression
  tic()
  dtl<-copy(dtlin)
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]
  
  if (globaluponly){ # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  if (adjccybs==1)
    lsout<-getccyFE2(dtl,fieldstr='swapsprdadj',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  else
    lsout<-getccyFE2(dtl,fieldstr='swapsprd',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  toc()
  if (returndt)
    lsout
  else
    lsout$regcoef
}