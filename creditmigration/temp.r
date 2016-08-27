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
