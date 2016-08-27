resyldsprdv3<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=0,approxrule=1,adjccybs=0){
  # v3 improvement: using swap data
  # Residualize yld sprd ----------------------------------------------------
  #create yield spread for aggregate 
  dtl<-copy(dtlin[field=='YLD_YTM_MID'])
  dtl[,ytm:=as.numeric((mat2-date)/365)]
  #winsorize by value a little
  #[,pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99]
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

  if (globaluponly){
  # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  
  # next step, try to generate yield sprd at the individual bond level instead of taking avg 
  # bring in the bbg prices
  
  if (adjccybs==1){
    message('adj. for ccy basis')
    swappricesl<-pricein[ticker %like% '^ussw' | ticker %like% '^eusz' | ticker %like% '^bpsz' | ticker %like% '^jysz' | ticker %like% '^adsz',.(date,ticker,value)] 
  } else{ # just getting swap spread
    swappricesl<-pricein[ticker %like% '^\\w\\wsw\\d+'][ticker %like% '^ussw' | ticker %like% '^eusw' | ticker %like% '^bpsw' | ticker %like% '^jysw' | ticker %like% '^adsw',.(date,ticker,value)]   
  }
  swappricesl<-swappricesl[ticker!='euswec' & ticker!='bpswsc'] # get rid of 3 month
  setnames(swappricesl,'ticker','field')
  swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
  #swappricesl[,.N,ticker][,.(field,tictenor=str_sub(ticker,5))] 
  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')
  setkey(swappricesl,date,ccy,tenor,field)
  setkey(dtl,date,ccy)
  
  dtl[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY,interprule=approxrule),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[,value:=value*100-swapyld][,field:='yldsprd']
  setkey(dtl,date,upcusip)

  dtl<-dtl[value!='NA'] #get rid of ones that can't be interpolated for one reason or another
  lsout<-getccyFE2(dtl,fieldstr='yldsprd',version=regversion)
  if (returndt==1)
    lsout
  else
    lsout[[1]]
}

getccyFE2<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.01){
  #  dfin<-dtl2
  print(str_c('FE on field: ',fieldstr))
  # df2<-dfin[field==fieldstr,.(date,ccy,value,upcusip,ytm,rating_bucket)]
  df2<-dfin[field==fieldstr]
  setkey(df2,date,upcusip,ccy)

  #winsorize each date
  if (winsor!=0){
    df2[,pctl:=percent_rank(value),by=date]
    df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
  }
    #get rid of days with only single observation
  df2<-df2[date %ni% df2[,.N,by=c('date','ccy')][N==1,date]]

  # set alphabetical order such that dummies are on foreign ccys
  df2[ccy=='usd',ccy:='1usd']
        
  # introduce liquidity measure based on bond age
    df2[,liq:=ytm/ytofm]
    df2<-df2[liq %between% c(0,1.1)]
    df2[liq<.5,liq_bucket:=0] # more illiq
    df2[liq>=.5,liq_bucket:=1] # liq
  regfun<-function(dt,regversion=1){
      if (regversion==1){
        # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
        reg<-lm(value~ccy+upcusip,data=dt)
      } else if (regversion==3){
        # regversion 3: like regversion 2 but also adds maturity considerations in regression
        reg<-lm(value~ccy+upcusip+ytm_bucket,data=dt)
      } else if (regversion==4){
        # regversion 4: regversion 3+ 3 rating buckets as dummies
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket,data=dt)
      } else if (regversion==5){
       # regversion 5: regversion 3+ 3 rating buckets as dummies
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
      } else if (regversion==6){
       # regversion 6, add illiqudity index
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
      } else if (regversion==7){
       # regversion 7, like 6 but w/o sicfac
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+liq_bucket,data=dt)
      } else if (regversion==8){
       # regversion 8, like 7 but only focus on liq
        reg<-lm(value~ccy+upcusip+ytm_bucket+liq_bucket,data=dt)
      }

      data.table(ccyeur=reg$coefficients['ccyeur'],
        ccygbp=reg$coefficients['ccygbp'],
        ccyjpy=reg$coefficients['ccyjpy'],
        ccyaud=reg$coefficients['ccyaud'],
        liquid=reg$coefficients['liq_bucket'])
  }

  regcoef<-df2[,regfun(.SD,version),by='date']
  setkey(regcoef,date)
  lsout<-list(regcoef,df2)
  beep()
  lsout
}