require(foreign)
require(stringr)
# require(xts)
require(tidyr)
require(dplyr)
#require('readstata13')
# require('haven')
# require('ggfortify')
# require('doBy')
require(lubridate)
require(ggplot2)
require(sandwich)
#require(stargazer)
require(reshape2)
# require(sqldf)
require(magrittr)
require(xda)
require(beepr)
require(data.table)
require(ggthemes)
'%ni%' <- Negate('%in%')
'%nlk%' <- Negate('%like%')
'%lk%' <- '%like%'
#source('dbutil.r')

df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")){
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}
toc <- function(){
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  beep()
  invisible(toc)
}
preprocess<-function(bondref,dtl,prl,monthlyonly=TRUE,issfiltertype=2,ccyfilter=c('usd','eur','jpy','gbp','aud','cad','chf')){
  tic()
  dtl<-copy(dtl)
  prl<-copy(prl)
  prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
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
      pkcount<-dtl[,.N,.(pk,monthend)] %>% dcast(pk~monthend)
      pk_daily<-pkcount[`0`>3*`1`,.(pk)]
      setkey(pk_daily,pk)
      dtl<-dtl[pk_daily]
    }
  }
  dtl2<-dtl[br[,.(ccy,mat2,nrating,upcusip,pk,ytofm,sicfac,sic1)],nomatch=0]
  dtl2[,ytm:=as.numeric((mat2-date)/365)]
  dtl3<-dtl2[ytm >.05]
  dtl3[is.na(nrating),nrating:=0]
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='YLD_YTM_MID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
  prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
  prw<-prl %>% distinct() %>% data.table::dcast(.,date~ticker,value.var = 'value')
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.05,1.0)]
  dtl3[liq<.5,liq_bucket:=0] # more illiq
  dtl3[liq>=.5,liq_bucket:=1] # liq
  # gen eusw=eusa-eubsv
  # gen eusz=eusw+eubs
  prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
  prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
  prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
  prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
  prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
  prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz3=cdsw3+cdbs3,cdsz4=cdsw4+cdbs4,cdsz5=cdsw5+cdbs5,cdsz6=cdsw6+cdbs6,cdsz7=cdsw7+cdbs7,cdsz8=cdsw8+cdbs8,cdsz9=cdsw9+cdbs9,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz25=cdsw25+cdbs25,cdsz30=cdsw30+cdbs30)]
  prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz3=sfsw3+sfbs3,sfsz4=sfsw4+sfbs4,sfsz5=sfsw5+sfbs5,sfsz6=sfsw6+sfbs6,sfsz7=sfsw7+sfbs7,sfsz8=sfsw8+sfbs8,sfsz9=sfsw9+sfbs9,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz25=sfsw25+sfbs25,sfsz30=sfsw30+sfbs30)]
  # prw[,approx_eubs1:=(eur/(eur+eur12m/10000)*(1+ussw1/10000)-(1+eusw1/10000))*10000]
  # prw[,approx_eubs5:=((eur/(eur+eur5y/10000))^(1/5)*(1+ussw5/10000)-(1+eusw5/10000))*10000]
  # transform prw back to prl
  prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
  prl<-prl[!is.na(value)]
  #browser()
  dtl4<-dtl.addswapsprd(dtl3,prl)
  toc()
  list('prw'=prw,'prl'=prl,'dtl4'=dtl4,'br'=br)
}
resyldsprdv4<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=T,adjccybs=0,winsor.=.025){
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
    lsout<-getccyFE2(dtl,fieldstr='swapsprdadj',version=regversion,winsor=winsor.)
  else
    lsout<-getccyFE2(dtl,fieldstr='swapsprd',version=regversion,winsor=winsor.)
  
  toc()
  if (returndt==1)
    lsout
  else
    lsout$regcoef
}

getccyFE2<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.025){
  #  Generalized function for calculating FE 
  print(str_c('FE on field: ',fieldstr))
    if ('field' %in% ds(dfin)) { # if dfin is in the long format with a field called 'field'
      df2<-dfin[field==fieldstr]
      lhs<-'value'
    } else { # if dfin is in the semi-wide format with a column called fieldstr
      df2<-copy(dfin)
      lhs<-fieldstr
    }
    setkey(df2,date,upcusip,ccy)

  #winsorize each date
    if (winsor!=0){
      df2[,pctl:=percent_rank(eval(exparse(lhs))),by=.(date,ccy)]
      df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
    }
      #get rid of days with only single observation and ones with only one ccy
      #df2<-df2[date %ni% df2[,.N,by=c('date','ccy')][N==1,date]]
      #df2<-df2[date %ni% df2[,.N,.(date,ccy)][,.N,date][N==1,date]]

  # set alphabetical order such that dummies are on foreign ccys
    df2[ccy=='usd',ccy:='1usd']
          
  # introduce liquidity measure based on bond age
      df2[,liq:=ytm/ytofm]
      df2<-df2[liq %between% c(0,1.1)]
      df2[liq<.5,liq_bucket:=0] # more illiq
      df2[liq>=.5,liq_bucket:=1] # liq
    regfun<-function(dt,ccylist,regversion=1,bylist){
      tryCatch({
          if (regversion==1){
            # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
            reg<-lm(eval(exparse(lhs))~ccy+upcusip,data=dt)
          } else if (regversion==3){
            # regversion 3: like regversion 2 but also adds maturity considerations in regression
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket,data=dt)
          } else if (regversion==4){
            # regversion 4: regversion 3+ 3 rating buckets as dummies
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket,data=dt)
          } else if (regversion==5){
           # regversion 5: regversion 3+ 3 rating buckets as dummies
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
          } else if (regversion==6){
           # regversion 6, add illiqudity index
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
          } else if (regversion==7){
           # regversion 7, like 6 but w/o sicfac
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+liq_bucket,data=dt)
          } else if (regversion==8){
           # regversion 8, like 7 but only focus on liq
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+liq_bucket,data=dt)
          }
      }, error=function(err){
        print(err)
        print(bylist)
        reg<-data.table(coefficients=as.numeric(NA))
        browser()
      })
        # dtcoef<-data.frame(as.list(reg$coefficients)) %>% select(starts_with('ccy')) %>%  as.data.table()
        # missccy<-ccylist[str_c('ccy',ccylist) %ni% c('ccy1usd',ds(dtcoef,'ccy'))]
        # if (length(missccy)>0){ # if missing ccy on a particular date
        #   for (iccy in missccy){
        #     dtcoef[,str_c('ccy',iccy):=as.numeric(NA)]
        #   }
        # }
        dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Std. Error`)]
        dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
        dtcoef2 %>% setkey(ccy)
        dtcoef2 <- dtcoef2[dtccy[ccy!='1usd']]
        # browser()
        dtcoef2
    }
    ccylist<-(df2 %>% distinct(ccy) %>% select(ccy))[[1]]
    regresult<-df2[,regfun(.SD,ccylist,version,.BY),by='date']
    regcoef <- regresult %>% dcast(date~ccy,value.var='est'); regcoef %>% setkey(date)
    regse <- regresult %>% dcast(date~ccy,value.var='se'); regse %>% setkey(date)
    lsout<-list('regcoef'=regcoef,'regse'=regse,'regresult'=regresult,'dtreg'=df2)
    lsout
}
intrwrap<-function(dfin,sp,bylist,interprule=1){
 #wrapper function for interpolation
  splocal<-sp[date==bylist$date & ccy==bylist$ccy]
  if (nrow(splocal)<3) {
    if (bylist$date %between% c(2,6)) 
      print(str_c('No swap data; NAs on ',bylist$date,bylist$ccy))
    #browser()
    rep(0,nrow(dfin))
  } else{
    tryCatch(dfout<-approx(x=splocal$tenor,y=splocal$value,xout=dfin$ytm,rule=interprule),
             error=function(err){
               print(err)
               print(bylist)
               browser()
             })
    dfout$y
  }
}

backfillNAs.prl<-function(prl,tickerpattern='^eubsv\\d',dates2fill.='eu', roll.=10){
  # 7/8/16 Self-rolling join to fill missing 3s6s basis swap data
  # this function back-fills NANs for the datatable prl (price in long form); it fills tickers matching tickerpattern; new dates are either given by dates2fill.
  # roll. determines how far to lookback before giving up and filling NANs
  prl<-copy(prl)
  prw<-prl %>% distinct() %>% data.table::dcast(.,date~ticker,value.var = 'value')
  if (dates2fill.=='eu'){
    dates2fill<-prw[!is.na(eusa5) & !is.na(eubs5),.(date)]
  } else {
    dates2fill<-dates2fill.
  }
  prlrolldata<-prl[ticker %like% tickerpattern,.(date,ticker,value)]
  setkey(prlrolldata,ticker,date)
  fields2fill<-dsl(prl,tickerpattern)
  blankslate<-data.table()
  for (f2f in fields2fill){
    blankslate<-rbind(blankslate,dates2fill[,.(date,ticker=f2f)])
  }
  setkey(blankslate,ticker,date)
  dtfill<-prlrolldata[blankslate,roll=roll.]
  #dtfill %>% data.table::dcast(.,date~ticker,value.var = 'value') %>%  View
  dtfill<-dtfill[!is.na(value)]
  prlnew<-update.prl(prl,dtfill)
  setkey(prlnew,date,ticker,pk)
  prlnew
}
get.dtl.status.mo<-function(dtl,gracewindow=60,bondref.=bondref){
  # get a sense of what data needs to be updated at the monthly and daily frequency
  # gracewindow is the number of days that are allowed to elapse to be considered as filled back starting today
  dtl<-copy(dtl)
  aa<-dtl[,.(MonthlyMin=min(date),MonthlyMax=max(date)),.(pk)] 
  #setnames(aa,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  bb<-merge(bondref.[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],aa,by='pk',all.y=TRUE)
  bb[!is.na(mat2),mat:=mat2][is.na(mat) & !is.na(matbbg),mat:=matbbg]
  bb[mat<=today(),matured:=1][is.na(matured),matured:=0]
  bb[MonthlyMax>mat-50 | MonthlyMax>today()-gracewindow,monthlyfilled:=1][is.na(monthlyfilled),monthlyfilled:=0]
  print(bb[monthlyfilled==0,.N,MonthlyMax][order(-N)])
  bb
}

get.dtl.status<-function(dtl,gracewindow=60,bondref.=bondref){
  # get a sense of what data needs to be updated at the monthly and daily frequency
  # gracewindow is the number of days that are allowed to elapse to be considered as filled back starting today
  dtl<-copy(dtl)
  aa<-dtl[,.(mindt=min(date),maxdt=max(date)),.(pk,monthend)] %>% dcast(pk~monthend,value.var=c('maxdt','mindt'))
  setnames(aa,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  bb<-merge(bondref.[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],aa,by='pk',all.y=TRUE)
  bb[!is.na(mat2),mat:=mat2][is.na(mat) & !is.na(matbbg),mat:=matbbg]
  bb[mat<=today(),matured:=1][is.na(matured),matured:=0]
  bb[DailyMax>mat-50 | DailyMax>today()-gracewindow,dailyfilled:=1][is.na(dailyfilled),dailyfilled:=0]
  bb[MonthlyMax>mat-50 | MonthlyMax>today()-gracewindow,monthlyfilled:=1][is.na(monthlyfilled),monthlyfilled:=0]
  print(bb[dailyfilled==0,.N,DailyMax][order(-N)])
  print(bb[monthlyfilled==0,.N,MonthlyMax][order(-N)])
  bb
}
get.prl.status<-function(prl){
  prl.status<-prl[,.(mindt=min(date),maxdt=max(date)),.(ticker,monthend)] %>% dcast(ticker~monthend,value.var=c('maxdt','mindt'))
  setnames(prl.status,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  print(prl.status[,.N,DailyMax][order(-N)])
  print(prl.status[,.N,MonthlyMax][order(-N)])
  prl.status
}

dtl.addswapsprd<-function(dtl,prl){
  ######## calculate interpolated swap spread for each and every single bond
  ####################Move this entire part to preprossing part!!!
  dtl<-copy(dtl)
  prl<-copy(prl)
  
  swappricesl<-prl[ticker %like% '^\\w\\wsw\\d+',.(date,ticker,value)]   
  swappricesladj<-prl[ticker %like% '^\\w\\wsz\\d+' | ticker %like% '^ussw\\d+',.(date,ticker,value)] 
  
  swappricesl[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(ticker,regex('\\d+')))]
  swappricesladj[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(ticker,regex('\\d+')))]
  
  swappricesl<-swappricesl[str_length(ccy)==3]
  swappricesladj<-swappricesladj[str_length(ccy)==3]

  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')  
  if (swappricesladj[is.na(tenor),.N]!=0) warning('swappricesladj has tenor not parsed')  
  
  setkey(swappricesl,date,ccy,tenor)  
  setkey(swappricesladj,date,ccy,tenor)  
  setkey(dtl,date,ccy)
  
  # find out what swap prices are missing
    dates2interp<-dtl[!is.na(ytm) & wday(date) %between% c(2,6),.N,by=.(date,ccy)]
    setkey(dates2interp,date,ccy)
    sp2interp<-swappricesl[,.N,.(date,ccy)][str_length(ccy)==3]
    missingswap<-sp2interp[dates2interp][is.na(N) | N<3]
    setkey(missingswap,date,ccy)
    print('missing swap prices on these dates for these ccys:')
    print(missingswap)
    dtl <- dtl %>% anti_join(missingswap,by=c('date','ccy')) %>% as.data.table()
    setkey(dtl,date,pk,value)
  #dtl[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY,interprule=1),by=.(date,ccy)]
  dtl[!is.na(ytm) & wday(date) %between% c(2,6),swapyld:=intrwrap(.SD,swappricesl,.BY,interprule=1),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[!is.na(ytm) & wday(date) %between% c(2,6),swapyldadj:=intrwrap(.SD,swappricesladj,.BY,interprule=1),by=.(date,ccy)][swapyldadj==0,swapyldadj:=NA]
    
  dtl[,swapsprdadj:=value*100-swapyldadj]
  dtl[,swapsprd:=value*100-swapyld]
  dtl[,swapyld:=NULL][,swapyldadj:=NULL]
  # dtl.na<-dtl[is.na(swapsprd)] 
  # dtladj.na<-dtl[is.na(swapsprdadj)] 
  # dtladj.na[!is.na(swapsprd),.N,ccy] # mainly missing GBP 30 yr+, need xccb for gbp 30
  dtl<-dtl[!is.na(swapsprd)] #get rid of ones that can't be interpolated for one reason or another
  #dtl<-dtl[!is.na(swapsprdadj)] #get rid of ones that can't be interpolated for one reason or another
  dtl
}


icollapse3<-function(dtin,ccyA="eur",natA="Eurozone"){
  # newer version of collapsing 
  # todo: construct and use modupccy
  dtin<-copy(dtin)
  dtin[,yrmo:=as.numeric(format(d,'%Y%m'))]
  df_fUSD<-dtin[modnat==natA & ccy=='usd',.(I_fUSD=sum(amt)/1000),by=yrmo]
  df_usF<-dtin[modnat=='United States' & ccy==ccyA,.(I_usF=sum(amt)/1000),by=yrmo]
  df_both<-dtin[modnat %in% c(natA,'United States') & ccy %in% c(ccyA,'usd'),.(I_both=sum(amt)/1000),by=yrmo]
  setkey(df_fUSD,yrmo)
  setkey(df_usF,yrmo)
  setkey(df_both,yrmo)
  df_all<-df_both %>% merge(df_fUSD,by='yrmo',all=TRUE) %>% merge(df_usF,by='yrmo',all=TRUE)
  df_all[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  df_all[,I_net_fus:=I_fUSD-I_usF][,i_net_fus:=I_net_fus/I_both]
  df_all %>% expandfulldates(.) %>%  rename(I_net_euus=I_net_fus,i_net_euus=i_net_fus) %>% as.data.table()
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
ggplotw<-function(dfin, fields=ds(dfin),x11.=FALSE){
  if (x11.) X11(width=15,height=9)
  dfin %>% 
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()

}
ggplotl<-function(dfin){
  dfin %>% ggplot(data=.,aes(x=date,y=value,colour=ticker))+geom_line()+geom_point()
}
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
ds<-function(dfin,matchpattern='.'){
  require(stringr)
  cn<-colnames(dfin) 
  try(cn[str_locate(as.character(cn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}
dsl<-function(dtlin,matchpattern='.',field='ticker'){

  #fn<-(dtlin[,ticker] %>% unique())[[1]]
   dtlin<-copy(dtlin)
   setkeyv(dtlin,field)
   fn<-unique(dtlin)[,field,with=F][[1]]
  
  try(fn[str_locate(as.character(fn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}


wgplot <- function(dfwide,idvar='date'){
  dfwide %>% 
  reshape2::melt(.,id.vars=idvar) %>%
  #filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line()+geom_point()
}


issfilter<-function(dtin,type=1){
  dtout<-dtin
  dtout %<>% filter(
     amt >= 50,
     ytofm >= 1,
     ytofm <= 99999,
     mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
     secur %ni% c("Cum Red Pfd Shs","Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk"),
     !grepl('Government Sponsored Enterprises',tf_mid_desc),!grepl('Flt', secur),!grepl('Zero Cpn', secur),!grepl('Float', secur),!grepl('Fl', descr),
     !grepl('Zero Cpn', descr),!grepl('Mortgage-backed', issue_type_desc),!grepl('Asset-backed', issue_type_desc),!grepl('Federal Credit Agency', issue_type_desc),!grepl('Loan', descr)
   ) 
  if (type==2){
    dtout %<>% filter(amt>=100)
  }
  if (type==3){
    dtout  %<>%  filter(!grepl('Mtg',typesec),
          !grepl('FRN',typesec), issue_type_desc!='Agency, Supranational, Sovereign',
          !grepl('Mortgage',secur),!grepl('Mtg',secur)
        )
  }
  dtout %>% as.data.table()
}



tabulate <- function(dfin,byvar='variable'){
  # dfin %>% group_by_(byvar) %>% dplyr::summarise_(count=length(byvar)) %>% arrange(desc(count))
  table(dfin[byvar]) %>% as.data.frame() %>%  tbl_df() %>% arrange(desc(Freq))
}

unpackbbgprices<-function(prices){
  # use when prices are batched in a list of say 20 groups
  a0 <- unlist(prices, recursive = FALSE)
  tickernames <- names(a0)
  df_prices <- data.frame() %>% tbl_df()
  for (i in 1:length(tickernames)) {
    temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
    if (nrow(temp_new) == 0)
      print (str_c('empty:#', i, ' name:', tickernames[i]))
    df_prices %<>% dplyr::bind_rows(., temp_new)
  }
  df_prices
}

requestfigibyisin<-function(df_isins){
 # given a dataframe of isins, get a dataframe of isin and figi mappings 
  require('magrittr')
  require('httr')
  require('jsonlite')
  tic()
  #require('tidyjson')
  # figireq<-'[{"idType":"ID_ISIN","idValue":"XS1033736890"},
  # {"idType":"ID_BB_UNIQUE","idValue":"JK354407"},
  # {"idType":"ID_BB","idValue":"JK354407"},
  # {"idType":"COMPOSITE_ID_BB_GLOBAL","idValue":"JK354407"},
  # {"idType":"TICKER","idValue":"JK354407 Corp"},
  # {"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]' ## FIGI code
  # # figireq<-'[{"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'
  print(str_c('min est:',nrow(df_isins)/10000))
    ptm <- proc.time()
    counter <- 0 # count request up to 100, for figi limit of 100 request per minute
    df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
    diag_response<-list()
    for (j in 1:ceiling(nrow(df_isins)/100)){
      counter <- counter+1
      if (counter==100){
       save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
         print(str_c("row (j):",j*100))
         # if ((proc.time() - ptm)[[3]]<=60){ # let it sleep to a full minute if it hasn't been a full minute
           print(str_c('last cycle:',(proc.time() - ptm)[[3]]))
           print(str_c('sleeping max of full 60 sec or next min:',Sys.time()))
           Sys.sleep(max(61-(Sys.time() %>% second()),61-((proc.time() - ptm)[[3]])))
           print(str_c('awake:',Sys.time()))
           #counter %>% print
           ptm <- proc.time()
           counter <- 0
      #    } else { # continue and reset counters and time
      #   ## let it sleep till the next minute regardless
      #     print(str_c('sleeping till next min:',proc.time()[[3]]))
      #     Sys.sleep(60-Sys.time() %>% second())
      #     ptm <- proc.time()
      #     print(str_c('awake:',proc.time()[[3]]))
      #     counter <- 0
      # } 
      }
      tempreq <- df_isins %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
      figireq<- tempreq %>%  mutate(idType='ID_ISIN',idValue=isin) %>% select(-isin)  %>% jsonlite::toJSON() 
      r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
      # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
      tryCatch({
        responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
      }, error = function(err) {
        print(r %>% content(as = "text") %>% str_sub(.,1,50))
        
        counter <- 0
        print(str_c('sleeping on error:',Sys.time()))
        Sys.sleep(60)
        ptm <- proc.time()
        print(str_c('awake:',Sys.time()))
        r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
        tryCatch({
          responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
        }, error=function(err){
          error('repeated request error, stopping')
          stop()
        })
      })
      
     if (colnames(responsejson)==c('error')){
        next
      }
      diag_response[j]<-responsejson
      # extract 100x100 results at a time
      temp_isin2figi<-as.data.frame(list()) %>% tbl_df()
      if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
      for  (i in 1:nrow(responsejson)){
        if (ncol(responsejson)==1) { # only data column
          
          temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
        } else{ # data column and error column #####something is not right
          if (is.na(responsejson$error[i][[1]]))
            temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
          else
            temp_isin2figi %<>% bind_rows(.,data_frame(isin=tempreq$isin[i][[1]]))
        }
      }
      if (nrow(temp_isin2figi)<nrow(tempreq)) browser()
      df_isin2figi_all %<>% bind_rows(.,temp_isin2figi)
    }
    toc()
    #this is the isin to figi mapping that contains 
    list(df_isin2figi_all,diag_response)
}

requestfigibyID2<-function(dtid_in,idtypein='isin',diagnostics=F,uselocalfirst=T){
 # given a dataframe of ID, get a dataframe of isin and figi mappings 




  require('httr');  require('jsonlite');  
  tic()
  ##########
  getresult100<-function(id2req){
    # this function would return a data.table of 100 rows if it's good, return 1 if bad
     json2req <- id2req %>% jsonlite::toJSON() 
      tryCatch({
      result100<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = json2req, encode = "json")
        result100_json <-   result100 %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
        

        if (!is.data.frame(result100_json)) browser()
        dt100temp<-result100_json %>% as.data.table()
        dt100temp$idValue<-id2req$idValue
        
        dtout<-data.table()
        for (m in 1:nrow(dt100temp)){
          singlerow<-dt100temp[,.(data)][[1]][[m]] %>% as.data.table()
          if (nrow(singlerow)>0){
            singlerow$idValue=dt100temp[,.(idValue)][[1]][[m]]
            dtout<-rbind(dtout,singlerow,fill=T)
          } else if (dt100temp[,.(error)][[1]][[m]]=='No identifier found.'){
            # dtout<-rbind(dtout,data.table('idValue'=dt100temp[,.(idValue)][[1]][[m]],'figi'=NA),fill=T)
          } else {
            browser()
          }
        }
                
        #dtout <- unlist(result100_json,recursive=F) %>% rbindlist(.,idcol='Nid')
        # dtout[,Nid:=as.numeric(str_sub(Nid,5))]
        # id2req[,Nid:=.I]
        # dtout<-merge(dtout,id2req,by='Nid')
        # browser()
        dtout
      }, error = function(err) {
        errstr<-result100 %>% content(as = "text") %>% str_sub(.,1,50)
        #browser()
        #result100 %>% content(as = "text") %>% str_replace_all(.,'{\"error\":\"No identifier found.\"}','')
        if (length(grep('error|Too Many',errstr))==0) browser()
        # if (length(grep('Too Many',errstr))==0) print(errstr)
        if (length(grep('Too Many',errstr))==1) {
          Sys.sleep(60); counter <<- 0
          print(str_c("Sleep: row (j):",j*100))
        }
        return(errstr)
      })
    }

    print(str_c('min est:',nrow(dtid_in)/10000))

    dtid_in<-unique(copy(dtid_in[!is.na(eval(exparse(idtypein))),.(eval(exparse(idtypein)))]))
    dtid<-copy(dtid_in) # create a copy so that the original can be compared to get missing at the end
    
    # first check the existance of the data in local database
    if ((idtypein=='isin' | idtypein=='figi') & uselocalfirst){
      load(file='db/dt_isin_figi.RData')
      setkeyv(dt.isin.figi,idtypein)
      setkeyv(dtid,idtypein)
      if ( idtypein=='isin'){ # if requesting by isin, can check against known missing list
        setkeyv(isin.no.figi,idtypein)
        dtid <- dtid[!isin.no.figi] #rid of missing figi requests
      }
      dtfigiout<-dt.isin.figi[dtid,nomatch=0] # matched requests output
      dtid <- dtid[!dtfigiout] #rid of matched requests
      if (nrow(dtid)==0) {
        print ('no new request made, all requests are from local DB')
        if (diagnostics)
          return(list('dtout'=dtfigiout))
        else
          return(dtfigiout)
      }
    } else if ((idtypein=='cusip9' | idtypein=='cusip8') & uselocalfirst){
      load(file='db/cusip_figi.RData')
      setkeyv(cusip.figi,idtypein)
      setkeyv(dtid,idtypein)
      dtfigiout<-cusip.figi[dtid,nomatch=0] # matched requests output
      dtid <- dtid[!dtfigiout] #rid of matched requests
      if (nrow(dtid)==0) {
        print ('no new request made, all requests are from local DB')
        if (diagnostics)
          return(list('dtout'=dtfigiout))
        else
          return(dtfigiout)
      }
    } else { #starting from scratch
      print('starting from scratch')
      dtfigiout<-data.table()
    }

    print(str_c('requesting total of :',nrow(dtid)))
    splitN<-ceiling(nrow(dtid)/100)
    dtid[,grp:=ceiling(.I/100)]


    if (idtypein=='isin') dtid$idType='ID_ISIN'
    if (idtypein=='figi') dtid$idType='ID_BB_GLOBAL'
    if (idtypein=='cusip8' | idtypein=='cusip9') dtid$idType='ID_CUSIP'
    

    counter <- 0 # count request up to 100, for figi limit of 100 request per minute
    
    
    #####Loop
    for (j in 1:ceiling(nrow(dtid)/100)){
      counter <- counter+1
      # if (counter==100){
         # print(str_c("Sleep: row (j):",j*100))
         # Sys.sleep(60); counter <- 0
      # }
      
      idreqtemp <- dtid[grp==j,.(idType,idValue=eval(exparse(idtypein)))]
      result100 <- getresult100(idreqtemp)

      if (!is.data.table(result100)){
        # Sys.sleep(60); counter=0
        result100 <- getresult100(idreqtemp)
      }
      if (!is.data.table(result100)){
        print('sleep for a second time due to error')
        # Sys.sleep(60); counter=0
        result100 <- getresult100(idreqtemp)
      }
      if (!is.data.table(result100)){
        print(str_c('give up due to error on grp: ',j))
        print(result100)
      } else { # this is the good scenario
        result100[,grp:=j]
        if (idtypein=='isin') setnames(result100,'idValue','isin')
        if (idtypein=='cusip8' | idtypein=='cusip9') setnames(result100,'idValue',idtypein)
        if (idtypein=='figi'){
           setnames(result100, 'figi','figi_old')
           setnames(result100,'idValue','figi')
          }

        dtfigiout<-rbind(dtfigiout,result100,fill=T)
      }
    }
    toc()
    setkeyv(dtfigiout,idtypein)
    setkeyv(dtid_in,idtypein)
    missing <- dtid_in[!dtfigiout]
    if (nrow(missing)>0){ 
      print('missing:')
      print(missing)
    }

    dups<-dtfigiout[,.N,eval(exparse(idtypein))][N>1]
    if (nrow(dups)>0){
      print('dups:')
      print(dups)
      print('marketsector of dups:')
      print(dups[dtfigiout,nomatch=0][,.N,marketSector])
    }

    if (diagnostics)
      list('dtout'=dtfigiout[,c(idtypein,'figi', 'marketSector',  'ticker', 'name', 'securityType', 'exchCode'),with=F], 'dtfull'=dtfigiout, 'missing'=missing, 'dups'=dups)
    else 
      dtfigiout[,c(idtypein,'figi', 'marketSector',  'ticker', 'name', 'securityType', 'exchCode'),with=F]
}


requestfigiinfo<-function(df_id,idstr='ID_ISIN'){
 # given a dataframe of ids, get info from openfigi isin
  tic()
  require('magrittr'); require('httr'); require('jsonlite')
  print(str_c('min est:',nrow(df_id)/10000))
  ptm <- proc.time()
  counter <- 0 # count request up to 100, for figi limit of 100 request per minute
  df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
  diag_response<-list()
  for (j in 1:ceiling(nrow(df_id)/100)){
    counter <- counter+1
    if (counter==100){
     save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
       print(str_c("row (j):",j*100))
         print(str_c('last cycle:',(proc.time() - ptm)[[3]]))
         print(str_c('sleeping max of full 60 sec or next min:',Sys.time()))
         Sys.sleep(max(61-(Sys.time() %>% second()),61-((proc.time() - ptm)[[3]])))
         print(str_c('awake:',Sys.time()))
         #counter %>% print
         ptm <- proc.time()
         counter <- 0
    }
    tempreq <- df_id %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
    figireq<- tempreq %>%  mutate(idType=idstr,idValue=id) %>% select(-id)  %>% jsonlite::toJSON() 
    r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
    # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
    tryCatch({
      responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
    }, error = function(err) {
      print(r %>% content(as = "text") %>% str_sub(.,1,50))  
      counter <- 0
      print(str_c('sleeping on error:',Sys.time()))
      Sys.sleep(60)
      ptm <- proc.time()
      print(str_c('awake:',Sys.time()))
      r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
      tryCatch({
        responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
      }, error=function(err){
        error('repeated request error, stopping')
        stop()
      })
    })
    
   if (colnames(responsejson)==c('error')){
      next
    }
    diag_response[j]<-responsejson
    # extract 100x100 results at a time
    temp_isin2figi<-as.data.frame(list()) %>% tbl_df()
    if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
    for  (i in 1:nrow(responsejson)){
      if (ncol(responsejson)==1) { # only data column
        
        temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(id=tempreq$id[i][[1]]))
      } else{ # data column and error column #####something is not right
        if (is.na(responsejson$error[i][[1]]))
          temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(id=tempreq$id[i][[1]]))
        else
          temp_isin2figi %<>% bind_rows(.,data_frame(id=tempreq$id[i][[1]]))
      }
    }
    if (nrow(temp_isin2figi)<nrow(tempreq)) browser()
    df_isin2figi_all %<>% bind_rows(.,temp_isin2figi)
  }
  toc()
  #this is the isin to figi mapping that contains 
  list(df_isin2figi_all,diag_response)
}

countdups<-function(dfin,field='isin'){
  (dfin %>% nrow)-(dfin %>% distinct_(field) %>% nrow)
}


loadBBGdownload2df<-function(filename='../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData'){
  # takes in a filename associated with bbg download; spits out price dataframe with field as colnames and a variable type
  load(filename)
  a0 <- unlist(prices, recursive = FALSE)
  dtw<-data.table::rbindlist(a0,use.names=TRUE,idcol=TRUE)
  dtl<-melt(dtw,id.vars=c('date','.id'),variable.name='field')[!is.na(value)]
  setnames(dtl,'.id','pk')
  setkey(dtl,date,pk)
  dtl
}

loadBBGdownload2df_older<-function(filein='../data/bloomberg/bbgpricesall151207_daily.RData'){
  load(filein)
  i=1
  dfout<-price[[i]]
  colnames(dfout)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
  
  for (i in 2:nrow(tickerraw)){
    dfnew<-price[[i]]
    colnames(dfnew)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
    dfout=merge(dfout, dfnew, all=TRUE)
  }
  dtout<- dfout %>% as.data.table() %>% melt(iv.vars='datestr',variable.name='ticker')
  dtout[,date:=ymd(datestr)][,datestr:=NULL]
  dtout<-dtout[!is.na(value)]
  setkey(dtout,date,ticker)
  dtout
}
melt.gl<-function(dfin,idvars=c('date','pk','batch')){
  #Deprecated: not useful: a wrapper for melt that sets measure.vars to be anything other than idvars
  setkey_(dfin,idvars)
  dtl_out<-melt(dfin,id.vars=idvars,measure.vars=((dfin %>% ds)[(dfin %>% ds) %ni% idvars]),
              variable.name='field')[!is.na(value)]
}

assessDataCoverage<-function(bondinfo,bondprices,field='YLD_YTM_MID',lastdate='lastobs',startdate='firstobs'){
  # check monthly data coverage; bondinfo is essentially sdc infomation on maturity etc, bondprice is from bbg download
  # 
  #   bondprices<-df_p %>% filter(date>='2005-01-01',date<='2016-04-01') 
  #   bondinfo<-df_sdc_all
  #   field="YLD_YTM_MID"
  # field="BLP_Z_SPRD_MID"
  # lastdate='lastobs'
  # startdate='firstobs'
  # #   
  #count unique bond tickers
  if (startdate=='firstobs') {
    startdate = min(bondprices$date)
    str_c('startdate: ', startdate) %>% print
  }
  if (lastdate == 'lastobs') {
    lastdate = max(bondprices$date)
    str_c('lastdate: ', lastdate) %>% print
  }
  
  
  if ('pk' %ni% (bondprices %>% ds)) bondprices %<>% rename(pk=ticker) 
  str_c('unique securities: ',bondprices %>% tabulate('pk') %>% nrow) %>% print
  
  # add expected number of months
  bondinfo %<>% distinct(pk) %>%  mutate(expmonthlyobs=ceiling((pmin(as.numeric(lastdate),as.numeric(mat2))-pmax(as.numeric(d),as.numeric(startdate)))/30.5)) 
  # count number of observations by isin # compare to number of expected obs by isin
  bondtickers <- bondprices %>% group_by(pk) %>% summarise(obs_allflds=length(pk))
  fldfreq<-bondprices[!is.na(bondprices[field]),] %>% select_('date','pk',field) %>% group_by(pk) %>% summarize(obs_specfld=length(date))
  bondtickers %<>% left_join(fldfreq,by='pk')
  bondtickers %<>% mutate(obs_specfld=ifelse(is.na(obs_specfld),0,obs_specfld))
  df_obs<-bondtickers %>% inner_join(bondinfo,by='pk') %>% mutate(obsdiff=expmonthlyobs-obs_specfld, obscoverage=ifelse(expmonthlyobs>=12,obs_specfld/expmonthlyobs,1)) %>% select(pk,isin,obsdiff,obscoverage,obs_allflds,obs_specfld,expmonthlyobs,mat2,d,settlement2)
  print('coverage:')
  str_c('# obs matching sdc:',df_obs %>% nrow()) %>% print
  df_obs$obscoverage %>% summary %>% print
  ggplot(df_obs,aes(x=obscoverage))+stat_ecdf()
  print('all fld obs diff from expected number of obs')
  df_obs %>% mutate(allflddiff=expmonthlyobs-obs_allflds) %$%summary(allflddiff) %>% print
  df_obs
  # df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
  # df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View 
}


showdups<-function(dfin,vars=key(dfin)){
  print('showing duplicates in:')
  print(vars)
  oldkey<-key(dfin)
  setkeyv(dfin,vars)
  dupkey<-dfin[,.N,by=vars][N>1,vars,with=FALSE]
  dfout<-dfin[dupkey]
  if(length(oldkey)>0) {
    if (!identical(oldkey,vars)) setkeyv(dfin,oldkey)
  }
  else {
    warning('set key(s) to vars:',vars)
  }
  dfout
}



openfigi.response2DT<-function(res){
  # not used
  aa<-list()
  # i=1
  for (i in 1:length(res)){
    aa[[i]]<-data.table()
    # tryCatch(aa[[i]]<-rbindlist(res[[i]]))
     for (j in 1:length(res[[i]])){
      aa[[i]]<-rbind(aa[[i]],data.table(res[[i]][[j]]))
    }
    print(i)
  }
  bb<-rbindlist(aa,fill=TRUE)
  bb
}


mergebymonth<-function(dfin1,dfin2){
  dfin1<-as.data.table(dfin1)
  dfin2<-as.data.table(dfin2)
  dfin1[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfin2[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfjoin<-merge(dfin1,dfin2,by='yrmo')
  dfjoin[,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  dfjoin
}
winsorize<-function(dfin){
  dfin[,pctl:=percent_rank(value)][pctl>=.01 & pctl<=.99]
  # tempdf<-dti %>% mutate(pctl=percent_rank(value))
  # tempdf[pctl>=.01 & pctl<=.99]
}

filterglobaluponly<-function(dtlin){
  dtl<-copy(dtlin)
  tokeep<-dtl[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N!=1][,.(date,upcusip)]
  setkey(tokeep,date,upcusip)
  setkey(dtl,date,upcusip)
  dtl[tokeep]
}

bucketrating<-function(dtlin){
  # creates new column called rating bucket as factor with 4 levels 
  #dtlout<-dtlin
  dtlin[nrating %between% c(1,4),rating_bucket:=3]
  dtlin[nrating %between% c(5,10),rating_bucket:=2]
  dtlin[nrating>10,rating_bucket:=1]
  dtlin[nrating==0,rating_bucket:=0]
  dtlin[is.na(nrating),rating_bucket:=0]
  dtlin[,rating_bucket:=factor(rating_bucket)]
  dtlin
}
bucketytm<-function(dtlin){
  dtlin<-dtlin[!is.na(ytm)]
  dtlin[ytm %between% c(0,3),ytm_bucket:=1]
  dtlin[ytm %between% c(3,7),ytm_bucket:=2]
  dtlin[ytm %between% c(7,10),ytm_bucket:=3]
  dtlin[ytm >10,ytm_bucket:=4]
  dtlin[,ytm_bucket:=factor(ytm_bucket)]
  dtlin
}

downloadbbg<-function(tickers,filestr=str_c('bbg_',today(),'.RData'),startdt=ymd('1996-01-01'),periodstr='MONTHLY',fieldstr='PX_LAST',splitN=1){
  require(Rblpapi); require(data.table); require(lubridate); require(dplyr)
  if (is.data.table(tickers)) {
    tickers<-tickers[,.(pk)]
  } else{
    tickers<-data.table('pk'=tickers)
  }


  if (tickers[1]=='restart'){ # if ticker=='restart', then restart using temp_bbgdownload_restart.RData file
    if (filestr==str_c('bbg_',today(),'.RData')){ 
      print('restarting from temp_bbgdownload_restart.RData')
      load('temp_bbgdownload_restart.RData')
    } else{
      print(str_c('restarting from ',filestr))
      load(filestr)
    }
    istart<-i
    message('restarting from batch ',i, ' out of total ',splitN)
  } else{ # regular new download
    tickers$batchfactor<-sample(1:splitN,size=nrow(tickers),replace=T)
    tickerslist<-split(as.character(tickers$pk),tickers$batchfactor)
    opt <- c("periodicitySelection"=periodstr)
    istart<-1
    prices<-list()
  }

  con <- Rblpapi::blpConnect()     # automatic if option("blpAutoConnect") is TRUE
  for (i in istart:splitN) {
    print(i)
    if (length(tickerslist[[i]])==0) {error('no tickers error'); browser()}
    tryCatch({
      #if (startdt=='BDP'){
      #  prices[[i]]<-bdp(tickerslist[[i]], fieldstr)  
      #} else{
        prices[[i]]<-bdh(tickerslist[[i]], fieldstr, start.date=startdt, options=opt)
      #}

    }, error=function(err){
        print(err)
        message('Limit Hit on ', i, ' consider restarting download on another machine or another day')
        save(prices,tickers,tickerslist,i,fieldstr,startdt,opt,splitN,filestr,file='temp_bbgdownload_restart.RData')
        #browser()
        blpDisconnect(con)      
        stop(err)
    }
    )
    # will remove this following line later
    save(prices,tickers,tickerslist,i,fieldstr,startdt,opt,splitN,filestr,file=str_c('temp_',filestr))
  }
  blpDisconnect(con)
  save(prices,file=filestr)
  loadBBGdownload2df(filestr)
}

update.prl<-function(prlin,prladd,overridein=FALSE,monthenddates.=monthenddates,diagret=FALSE){
  dtout<-copy(prlin)
  prladd<-copy(prladd)
  if ('pk' %ni% colnames(prladd)) prladd[,pk:=str_c(ticker,' curncy')]
  if ('field' %in% colnames(prlin)) {
    if ('field' %ni% colnames(prladd)) prladd[,field:='PX_LAST']
    keyfield.=c('date','pk','field')
  } else{
    keyfield.=c('date','pk')
  }
  prladd[,pk:=tolower(pk)]
  prladd[!is.na(pk),ticker:=str_extract(tolower(pk),regex('.*(?= curncy)'))]
  prladd<-fixmonthend(monthenddates.,prladd)
  # nextbatchN<-max(na.omit(prl[,batch]))+1;  # prladd[,batch:=nextbatchN]
  dtout<-update.dt(dtout,prladd,keyfield = keyfield.,override=overridein,diagnostic_rt=diagret)
  dtout
}

update.dtl<-function(dtlin,dtladd,overridein=FALSE,diagret=FALSE,monthenddates.=monthenddates){
  dtout<-copy(dtlin)
  dtladd<-copy(dtladd)
  if ('batch' %in% ds(dtlin)){
    nextbatchN<-max(na.omit(dtout[,batch]))+1
    dtladd[,batch:=nextbatchN]
  }
  dtladd[,pk:=tolower(pk)]
  dtladd<-fixmonthend(monthenddates.,dtladd)
  dtout<-update.dt(dtout,dtladd,keyfield = c('date','pk','field'),override=overridein,diagnostic_rt=diagret)
  dtout
}

update.dtl.mo<-function(dtlin,dtladd,overridein=FALSE,diagret=FALSE){
  # a condensed version for only YLD_YTM_MID and w/o regard for monthend
  # can also be applied to daily data
  dtout<-copy(dtlin)
  dtladd<-copy(dtladd)
  if ('field' %in% ds(dtladd)){
    message('resticting field to be only YLD_YTM_MID')
    dtladd<-dtladd[field=='YLD_YTM_MID']
  }
  dtout[,pk:=tolower(pk)]
  dtladd[,pk:=tolower(pk)]
  dtout<-dtout[,.(date,pk,value)]
  dtladd<-dtladd[,.(date,pk,value)]
  
  setkey(dtout,date,pk); setkey(dtladd,date,pk)
  dtout<-update.dt(dtout,dtladd,keyfield = c('date','pk'),override=overridein,diagnostic_rt=diagret)
  dtout
}
update.br<-function(bondref,dtadd,keystr='figi'){
  bondrefout<-copy(bondref)
  dtadd<-copy(as.data.table(dtadd))
  dtadd[!is.na(figi),figiloaded:=1]
  checkcn<-checkcnexist(bondrefout,dtadd)
  if (length(checkcn)!=0) {
    message('no matching col in bondref, not inserted: '); print(checkcn)
    dtadd<-dtadd[,!checkcn,with=F]
  }
  
  dtadd[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")))]
  bondrefout<-update.dt(bondrefout,dtadd,keyfield = keystr )
  bondrefout[,matdiff:=as.numeric((matbbg-mat2)/365)]
  bondrefout
}
update.br2<-function(bondref,dtadd,keystr='figi',diagnostics=F){
  bondrefout<-copy(bondref)
  dtadd<-copy(as.data.table(dtadd))

  dtadd[!is.na(figi),figiloaded:=1]
  checkcn<-checkcnexist(bondrefout,dtadd)
  if (length(checkcn)!=0) {
    message('no matching col in bondref, not inserted: '); print(checkcn)
    dtadd<-dtadd[,!checkcn,with=F]
  }
  
  dtadd[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")))]
  bondrefout<-update.dt(bondrefout,dtadd,keyfield = keystr )
  bondrefout[,matdiff:=as.numeric((matbbg-mat2)/365)]
  bondrefout
}
fixmonthend<-function(dtby=monthenddates,dtfix=dtby){
  #fix monthend date field of dtfix with monthend dates of dtby; return dtfix
  dtby<-copy(dtby)
  dtfix<-copy(dtfix)
  keyoriginal<-key(dtfix)
  if (colnames(dtby)[1]==c('date') & ncol(dtby)==1) 
    monthenddate<-dtby 
  else 
    monthenddate<-dtby[monthend==1,.N,date][N>100,.(date)]
  setkey(monthenddate,date)
  setkey(dtfix,date)
  dtfix[monthenddate,monthend:=1]
  dtfix[!monthenddate,monthend:=0]
  setkeyv(dtfix,keyoriginal)
  dtfix
}

update.dt<-function(dtA,dtB,keyfield='auto',updatefield='auto',insertnewrow=TRUE,override=FALSE,diagnostic_rt=FALSE){
  # this function allows easy addition and update of columns; adding new rows require insertnewrow=TRUE
  # update will override existing values currently; ideally want update on NA, do nothing on records where update is the same as original, and split out differentiated records where original value!=update value
  
  dtA<-copy(dtA)
  dtB<-copy(dtB)
  keyoriginal<-key(dtA)
  conflist<-list()
  if (keyfield[1]=='auto') keyfield<-keyoriginal
  print(str_c('Keyfield: ',paste(keyfield,collapse=' ')))
  setkeyv(dtA,keyfield)
  setkeyv(dtB,keyfield)
  if (updatefield=='auto') {
    fieldA<-ds(dtA)[ds(dtA) %ni% keyfield]
    fieldB<-ds(dtB)[ds(dtB) %ni% keyfield]
    updatefield<-fieldB[fieldB %in% fieldA]
    newfield<-fieldB[fieldB %ni% fieldA]
    lhs<-c(updatefield,newfield)
    rhs<-c(str_c('i.',updatefield),newfield)
    rhs<-rhs[rhs %ni% 'i.']
  }else{
    lhs<-updatefield
    rhs<-str_c('i.',updatefield)
  }
  if (length(lhs)!=length(rhs)){ print('len(lhs)!=len(rhs)');browser()}
  for (j in 1:length(lhs)){
    message(str_c('inserting/updating: lhs: ',lhs[j],' rhs: ',rhs[j]))
    #show warning message first
    problemdt<-dtA[dtB,nomatch=0][eval(exparse(lhs[j]))!=eval(exparse(rhs[j]))]
    if (nrow(problemdt)!=0){
      message('matched but existing value conflicts for field: ',lhs[j])
      #if (override) warning('overriding:') else warning('skiping:')
      #print(problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE])
      
      # if (lhs[j]=='value') { # additionally create a value diff if field is 'value'
      gettypefrom<-problemdt[,lhs[j],with=FALSE][[1]]
      if (is.numeric(gettypefrom) | is.Date(gettypefrom) | is.character(gettypefrom)){
        if (is.numeric(gettypefrom)) 
          problemdt[,valdiff:=eval(exparse(lhs[j]))-eval(exparse(rhs[j]))]
        else if (is.Date(gettypefrom))
          problemdt[,valdiff:=(as.numeric(eval(exparse(lhs[j]))-eval(exparse(rhs[j]))))/365]
        else if (is.character(gettypefrom))
          problemdt[,valdiff:=adist(eval(exparse(lhs[j])),eval(exparse(rhs[j])),ignore.case=TRUE)/str_length(eval(exparse(lhs[j]))),by=1:nrow(problemdt)]
        else
          message('ERROR type of problemdt column')

        confdt<-problemdt[,c(keyfield,lhs[j],rhs[j],'valdiff'),with=FALSE]
        message('\n val diff summary: \n',print_and_capture(summary(confdt$valdiff)))
      } else {
        confdt<-problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE]
      }
      message('\n',print_and_capture(confdt))
      conflist[[length(conflist)+1]]<-confdt
    }

    if (override){
      # override when there are existing value
      tempdt<-dtA[dtB,nomatch=0]
      message(str_c('updated/inserted: ',tempdt[is.na(eval(exparse(lhs[j]))),.N]))
      message(str_c('override: ',tempdt[!is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=eval(exparse(rhs[j])),nomatch=0]
    } else{
      # update/insert if na /override 
      message(str_c('updated/inserted (no override) on ',dtA[dtB,nomatch=0][is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=ifelse(is.na(eval(exparse(lhs[j]))),eval(exparse(rhs[j])),eval(exparse(lhs[j]))),nomatch=0]
    }
    
    
  }
  if (insertnewrow){
    #insert row
    rowinsert<-dtB[!dtA]
    # dtA<-rbind(dtA,rowinsert,fill=TRUE)
    dtA<-bind_rows(dtA,rowinsert) %>% as.data.table()
    message(str_c('Rows inserted: ',rowinsert[,.N]))
  }
  setkeyv(dtA,keyoriginal)
  if (diagnostic_rt){
    # merging the list of conflicting problematic data.tables all together into one single database
    if (length(conflist)>0) dtconflict<-data.table(conflist[[1]]) else dtconflict<-data.table()
    tryCatch({setnames(dtconflict,'valdiff',str_c('diff_',colnames(conflist[[1]])[2]))},error=function(e){})
    if (length(conflist)>1){
      sink("/dev/null")
      print('Constructing conflict table:')
      for (k in 2:length(conflist)){
         dtconflictadd<-as.data.table(conflist[[k]])
         tryCatch({setnames(dtconflictadd,'valdiff',str_c('diff_',colnames(dtconflictadd)[2]))},error=function(e){})
         dtconflict<-update.dt(dtconflict,dtconflictadd,keyfield=keyfield,diagnostic_rt=F)
      }



      sink();sink();
        dtconflict[,Nconf:=(sum(!is.na(.SD))-1)/3,by=1:nrow(dtconflict)]
    }

    list('dtout'=dtA,'dtconflicts'=dtconflict)
  } else {
    dtA  
  }
  
}

exparse<-function(strin){parse(text=strin)}
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

checkcnexist<-function(dtold,dtadd){
# check which colnames in dtadd do not exists in dtold
  cnexist<-dtold %>% ds()
  cnadd<-dtadd %>% ds()
  cnadd[cnadd %ni% cnexist]
}
missSummary<-function(dtin){
  cn<-dtin %>% ds()
  misslist<-list()
  for (cni in cn) 
    misslist[[length(misslist)+1]]<-data.table(vname=cni,missN=dtin[is.na(eval(exparse(cni))),.N])
  missdt<-rbindlist(misslist)
  missdt[,total:=dtin[,.N]][,misspct:=missN/total]
  print(missdt)
  missdt
}
tsdiff<-function(dtin){
  # simply generate diff=i.var-var for all i.var
  dtin<-copy(dtin)
  cn<-dtin %>% ds()
  cn<-cn[cn %like% '^i\\.']
  for (cni in cn){
    cnis<-str_sub(cni,3)
    dtin %<>% mutate_(str_c('diff_',cnis,'=',cni,'-',cnis))
  }
  dtin
}
readkey <- function(){
  cat ("Press [enter] to continue")
  line <- readline()
}
readkeygraph <- function(prompt){
  getGraphicsEvent(prompt = prompt, 
                   onMouseDown = NULL, onMouseMove = NULL,
                   onMouseUp = NULL, onKeybd = onKeybd,
                   consolePrompt = "[click on graph then follow top prompt to continue]")
  Sys.sleep(0.01)
  return(keyPressed)
}

onKeybd <- function(key){
  keyPressed <<- key
}

ggplotw.comp<-function(dtin){
  #X11()
  X11(width=15,height = 9)
  print(dtin[,.(date,ccyeur,i.ccyeur)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,ccygbp,i.ccygbp)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,ccyjpy,i.ccyjpy)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,ccyaud,i.ccyaud)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,ccychf,i.ccychf)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,ccycad,i.ccycad)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
}

dt2clip = function(x,sep="\t",col.names=T,...) { 
  write.table(x
             ,file = pipe("pbcopy")
             ,sep=sep
             ,col.names = col.names
             ,row.names = F
             ,quote = F,...)
}
get.dt.class<-function(dtin){
  data.table('columnname'=ds(dtin),'classtype'=unlist(lapply(dtin,class)))
}

cleanup.sdc.by.col<-function(dtsdc){
  # this function cleans up dtsdc by column, replaceing '-'  with NA and get rid of factor columns
  dtsdc<-copy(dtsdc)
  dtclass<-get.dt.class(dtsdc)
  dtsdc<-subset(dtsdc,select=dtclass[classtype!='factor',columnname])
  dtclass<-get.dt.class(dtsdc)
  dtclass[,rowN:=.I]
  for (k in 1:nrow(dtclass)){
    if(dtclass[rowN==k,classtype=='character']){
      cname<-dtclass[rowN==k,columnname]
      dtsdc[,eval(exparse(cname)):=str_trim(eval(exparse(cname)))]
      dtsdc[eval(exparse(cname)) %in% c('','-','N/A','TBA','NA'),eval(exparse(cname)):=NA]
    }
  }
  dtsdc
}

CUSIPcheck <- function(x){
  #check cusip number
  # cusip9.to.check<-cusip9.to.get[str_length(cusip9)==9]
  # cusip9.to.check[,digit9:=CUSIPcheck(str_sub(.SD[,cusip9],0,8)),by=1:nrow(cusip9.to.check)]
  # cusip9.to.check[str_sub(.SD[,cusip9],9)!=digit9]
  if(nchar(x)!=8){stop("Provided CUSIP does not have length 8")}
  v <- unlist(strsplit(x,""))
  if(any(v %in% LETTERS)){
  v[which(v %in% LETTERS)] <- which(LETTERS %in% v[which(v %in% LETTERS)])+9
  v <- as.numeric(v)
  }else{v <- as.numeric(v)}
  out <- 0
  for(i in 1:8){
  if(i%%2==0){v[i]<-v[i]*2}
  out <- out + v[i]%/%10 + v[i]%%10
  }
  (10-(out%%10))%%10
}

show.dupe.col<-function(dtin){
  # iterate through the columns of dtin and show only duplicated asOneSidedFormula
  dup.col<-data.table('col'=1:ncol(dtin),'dupe'=0)
  for (colq in 1:ncol(dtin))  {
    # if(anyDuplicated(dtin[,colq,with=F])==0) # colq is not the same
    if((dtin[,colq,with=F] %>% duplicated() %>% not()  %>% sum())>1)
      dup.col[col==colq,dupe:=1]
  }
  dtin[,dup.col[dupe==1,col],with=F]
}

compare.dt<-function(A,B, bykey.=key(A),mask=T,unique.=T){
  A<-copy(A)
  B<-copy(B)
  A %>% setkeyv(bykey.); B %>% setkeyv(bykey.)
  if (unique.){
    A<-unique(A)
    B<-unique(B)
  }
  Acount<-A[,.N,bykey.][,.N]; print(str_c('A N.= ',Acount))
  Bcount<-B[,.N,bykey.][,.N]; print(str_c('B N.= ',Bcount))
  AB.intersect<-A[B,nomatch=0]; print(str_c('AB intersect N.= ',AB.intersect[,.N]))
  AnotB <- A[!B]; print(str_c('A not in B N.= ',AnotB[,.N]))
  BnotA <- B[!A]; print(str_c('B not in A N.= ',BnotA[,.N]))
  if (mask) 
    return(NULL)
  else
    list('AB'=AB.intersect,'AniB'=AnotB,'BniA'=BnotA,'A.N'=Acount,'B.N'=Bcount,'AB.N'=AB.intersect[,.N],'AniB.N'=AnotB[,.N],'BniA.N'=BnotA[,.N])
}

fread.xls<-function(path,sheet.=excel_sheets(path)){
  require('readxl')
  dt.out<-rbindlist(lapply(sheet., function(x,path){
    dt.sheet<-(read_excel(path,x) %>% as.data.table())
    dt.sheet[,sheet:=x]
    dt.sheet
  },path),fill=T) 
  dt.out
}

nonmarket.dates<-function(dtl,bondref){
  #retrieve nonmarket dates for daily time series based on holiday schedule and consecutive observation drop
  dtl<-copy(dtl)
  br<-copy(bondref[!is.na(pk),.(ccy,mat2,nrating,upcusip,pk,ytofm,sicfac,sic1)])
  br[,ccy:=tolower(ccy)]
  setkey(dtl,pk); setkey(br,pk)
  dtl<-dtl[br]

  load('db/holidaycalendar.RData')
  holidays<-data.table('date'=unique(c(cdrus,cdreu)))
  setkey(holidays,date)
  setkey(dtl,date)
  dtl<-dtl[!holidays][date<'2016-07-26']
 # consecutive obs drop 
  dtl.count<-(dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast(date~ccy,value.var='N'))[order(date)]   
  dtl.count[,usd.L:=lag(usd)]
  # dtl.count[,usd.L:=mean(lag(usd),lag(usd),lag(usd),lag(usd),lag(usd))]
  dtl.count[usd<.90*usd.L,sharpobsdrop:=1][is.na(sharpobsdrop),sharpobsdrop:=0]
  drop.dates<-rbind(dtl.count[sharpobsdrop==1,.(date)],holidays)
  drop.dates %>% setkey(date)
  drop.dates<-drop.dates %>% unique(.)
  #dtl.count<-dtl.count[sharpobsdrop==0]
  drop.dates
}