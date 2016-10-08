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
# this version of resyldsprdv4 is dated 2016-08-11
resyldsprdv4<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=T,adjccybs=0,winsor.=.025){
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
    # set alphabetical order such that dummies are on foreign ccys
      df2[ccy=='usd',ccy:='1usd']
            
    # introduce liquidity measure based on bond age
        df2[,liq:=ytm/ytofm]
        df2<-df2[liq %between% c(0,1.1)]
        df2[liq<.5,liq_bucket:=0] # more illiq
        df2[liq>=.5,liq_bucket:=1] # liq
      regfun<-function(dt,ccylist,regversion=1,bylist){
        tc.ret<-tryCatch({
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
          #reg<-data.table(coefficients=as.numeric(NA))
          browser()
          return(data.table('ccy'='eur','est'=NA,se=NA))
        })
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
      lsout<-getccyFE2(dtl,fieldstr='YLD_YTM_MID',version=regversion,winsor=winsor.)    
    toc()
    if (returndt==1)
      lsout
    else
      lsout$regcoef
}

ys_hy4<-resyldsprdv4(dtl3[ccy %in% c('usd','eur') & nrating>6 & date>'2004-09-01'],prl,regversion=3)
ys_hy4$regcoef %>% ggplotw
