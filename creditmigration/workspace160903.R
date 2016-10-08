

#testing high grade low grade
	setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	rm(list=ls(all=TRUE));
	load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
	
	source('util.r')
	ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3,returndt=T)
	ys_hg<-resyldsprdv3new(dtl3[ccy %in% c('usd','eur') & nrating<=6],prl,regversion=3)	

	ys_hy$regdt
	# dtltemp<-dtm$dtl4[ccy %in% c('usd','eur') & nrating>6]
	dtltemp<-filterglobaluponly(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6])
	dtltemp[,pctl:=percent_rank(swapsprd),by=.(date,ccy)]
	winsor=.025
    dtltemp<-dtltemp[pctl>=winsor & pctl<=(1-winsor)]
     dtltemp[,liq:=ytm/ytofm]
     dtltemp<-dtltemp[liq %between% c(0,1.1)]
     dtltemp[liq<.5,liq_bucket:=0] # more illiq
     dtltemp[liq>=.5,liq_bucket:=1] # liq
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
          } else if (regversion==5){ # makes no sense!!!! deprecated
            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
          } else if (regversion==6){
            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
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
      })
        if (exists('reg')){
          dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Std. Error`)]
          dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
          dtcoef2 %>% setkey(ccy)
          dtcoef2 <- dtcoef2[dtccy[ccy!='1usd']]
          dtcoef2
        } else {
          return(data.table('ccy'='eur','est'=as.numeric(NA),se=as.numeric(NA)))
        }
        
  }
  	lhs='swapsprd'
  	dtltemp[date=='2004-06-30'][ccy %in% c('eur','usd')]
    regfun(dtltemp,c('eur','usd'),4,'2004-06-30')

	# '2004-06-30' has only 3 observations; all in usd
    merge(ys_hy$dtreg[,.N,date],ys_hg$dtreg[,.N,date],by='date')  %>% dt2clip()
    merge(ys_hy$dtreg[,.N,date],ys_hg$dtreg[,.N,date],by='date') %>% ggplotw()
    

    # what does it look like for old data?
    setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	rm(list=ls())
	load('dtclean20160725.RData')
	source('old/util_20160620.r')

	ys_hy<-resyldsprdv3new(dtl3[ccy %in% c('usd','eur') & nrating>6],prl,regversion=3,returndt=T)

	source('util.r')
	# ys_hy$dtreg[,.N,date] %>% dt2clip(.)

	# old data with new code
		dtltemp<-filterglobaluponly(dtl3[ccy %in% c('usd','eur') & nrating>6])
		dtltemp[,pctl:=percent_rank(value),by=.(date,ccy)]
		winsor=.025
	    dtltemp<-dtltemp[pctl>=winsor & pctl<=(1-winsor)]
	     dtltemp[,liq:=ytm/ytofm]
	     dtltemp<-dtltemp[liq %between% c(0,1.1)]
	     dtltemp[liq<.5,liq_bucket:=0] # more illiq
	     dtltemp[liq>=.5,liq_bucket:=1] # liq
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
	     		} else if (regversion==5){ # makes no sense!!!! deprecated
	            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
	     	} else if (regversion==6){
	            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
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
	        # return(data.table('ccy'='eur','est'=NA,se=NA))

	     })

	     	if (exists('reg')){
	     		dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Std. Error`)]
	     		dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
	     		dtcoef2 %>% setkey(ccy)
	     		dtcoef2 <- dtcoef2[dtccy[ccy!='1usd']]
	     		dtcoef2
	     	} else {
	     		return(data.table('ccy'='eur','est'=as.numeric(NA),se=as.numeric(NA)))
	     	}

	     }
	     lhs='value'
	  	dtregtemp<-dtltemp[date=='2004-06-30'][ccy %in% c('eur','usd')]
	  	dtregtemp[ccy=='usd',ccy:='1usd']
	  	reg<-lm(value~ccy+upcusip+ytm_bucket,data=dtregtemp);
	  	reg %>% summary
	    regfun(dtlregtemp,c('eur','usd'),3,'2004-06-30')

	# what data is missing in new?
	    dtregtemp[ccy=='eur'] %>% save(file='tempmissing.rdata')
	    dtregtemp[ccy=='eur'] %>% dt2clip

		load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
		source('util.r')
		dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
		dtltempnew<-filterglobaluponly(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6])
		dtltempnew
		# it's not really missing in the database, so it must be filtered out somehow by preprocessing
		pkmissing<-dtltemp[,.N,pk] %>% anti_join(dtltempnew[,.N,pk],by=c('pk')) %>% as.data.table
		dtltemp[,.N,pk] %>% anti_join(dtl.mo[,.N,pk],by=c('pk'))
		dtltemp[,.N,pk] %>% anti_join(br[,.N,pk],by=c('pk'))

		

# figure out what's going on in preprocessing that's making the observations disappear :)
		# do this later!!!!!
		dtltemp %>% issfilter(4)
		pkmissing %>% setkey(pk)
		bondref %>% setkey(pk)
		bondref[pkmissing,nomatch=0]
		br[pkmissing,nomatch=0]
		zz<-bondref %>% semi_join(pkmissing,by='pk') %>% as.data.table %>% issfilter(4,desc=T)

		zz<-bondref %>% issfilter(4,desc=T)

# what to do with SSA		
		aa<-bondref[issue_type_desc=='Agency, Supranational, Sovereign'] 

		tokeep<-aa[,.N,by=c('upcusip','ccy')][,.N,by=c('upcusip')][N!=1][,.(upcusip)]
	    setkey(tokeep,upcusip)
	    setkey(aa,upcusip)
	    aa[tokeep][,.N,upnames][order(-N)]
	    aa[tokeep][,.N,upnames][order(-N)] %>% dt2clip

	    aa[upnames %in% c('Germany (Federal Republic Of)')][,.(pk,i,name, ccy,tic,ticker,cu,cusip9,nat,amt,descr,rating)] %>% dt2clip
	    aa[upnames %in% c('The Netherlands','Sweden','Malaysia','Russia')][order(upnames)][,.(pk,i,name, ccy,tic,ticker,cu,cusip9,nat,amt,descr,rating)] %>% dt2clip

# untrim upcusip #e.g. 001957 vs 1957
		bondref[,.N,str_length(upcusip)]
		bondrefall[cu %like% '78387G'][,.(i,name,upnames,cu,upcusip)]
		# maybe better to use bbg mapping of up

	# getting a good firm example: first try AT&T
	load('db/bondrefall.RData')	
	att<-bondrefall[i %like% 'AT&T' | upnames %like% 'AT&T' | upcusip=='00206R' | ticker %like% '^T\\s'][,.(i,name,upnames,ticker,upcusip,d,ccy,mat2,matbbg,rating,ytofm,nrating,pk,amt)][order(-d)] 
	# att %>% dt2clip
	att %>% showdups('ticker') %>% dt2clip
	att %>% setkey(ticker)
	att<-unique(att)

	att[,quarterly:=ceiling_date(d,'quarter')]
	require('ggTimeSeries')
	att[,.N,.(ccy,quarterly)][quarterly>ymd('2008-01-01')] %>% ggplot(aes(x=quarterly,y=N,colour=ccy))+stat_occurrence()
	
	# how does att's credit mispricing look like
	
	bbg.mo<-loadBBGdownload2df('dbin/att_monthly_160903.RData')
	aa<-bbg.mo[,.N,pk][,.(pk=str_trim(str_replace_all(pk,' Corp','')))]
	# merge(aa, att,by.x='pk',by.y='ticker',all.x=T) %>% dt2clip

	att.dtl<-merge(bbg.mo[,.(date,pk=str_replace_all(pk,' Corp',''),field,value)],att,by.x='pk',by.y='ticker')
	att.dtl %>% setkey(pk,date,field)
	att.dtl<-att.dtl %>% unique

	att.dtl[,.N,upcusip]
	att.dtl[,.N,field]

	att.dtl[,ytm:=as.numeric((mat2-date)/365)]
    att.dtl<-att.dtl[ytm >.05]

    att.dtl[,ccy:=tolower(ccy)]

    att.dtl<-att.dtl[upcusip=='00206R']
    # att.dtl<-att.dtl[,upcusip:='1']
    att.dtl.yld<-att.dtl[field=='YLD_YTM_MID' & date<ymd('2016-08-01')]

	# preprocessing
		prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
		prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
		prw<-prl %>% distinct() %>% data.table::dcast(.,date~ticker,value.var = 'value')
		  prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
		prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
		prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
		prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
		prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
		prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz5=cdsw5+cdbs5,cdsz7=cdsw7+cdbs7,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz30=cdsw30+cdbs30)]
		prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz5=sfsw5+sfbs5,sfsz7=sfsw7+sfbs7,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz30=sfsw30+sfbs30)]
		prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
		prl<-prl[!is.na(value)]
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
		    
		    # dtl <- dtl %>% anti_join(missingswap,by=c('date','ccy')) %>% as.data.table()
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
	att.dtl.ys<-dtl.addswapsprd(att.dtl.yld,prl)
	att.dtl.ys<-att.dtl.ys %>% bucketytm()
	att.dtl.ys<- att.dtl.ys %>% filterglobaluponly
	att.dtl.ys[ccy=='usd',ccy:='1usd']
		 regfun.local<-function(dt,ccylist,regversion=1,lhs='swapsprd',bylist){
				      tc.ret<-tryCatch({
				          if (regversion==1){
				            # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
				            reg<-lm(eval(exparse(lhs))~ccy,data=dt)
				          } else if (regversion==3){
				            # regversion 3: like regversion 2 but also adds maturity considerations in regression
				            reg<-lm(eval(exparse(lhs))~ccy+ytm_bucket,data=dt)
				          }
				      }, error=function(err){
				        print(err)
				        print(bylist)
			        
			      })
			      
			        if (exists('reg')){
			          dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Std. Error`)]
			          dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
			          dtcoef2 %>% setkey(ccy)
			          dtcoef2 <- dtcoef2[dtccy[ccy!='1usd']]
			          dtcoef2
			        } else {
			          return(data.table('ccy'='eur','est'=as.numeric(NA),se=as.numeric(NA)))
			        }	        
			    }

	winsor=.01
	att.dtl.ys[,pctl:=percent_rank(eval(exparse(lhs))),by=.(date,ccy)]
	dtlattwin<-att.dtl.ys[pctl>=winsor & pctl<=(1-winsor)]

	ys_att<-dtlattwin[,regfun.local(.SD,c('eur','usd'),1,lhs='swapsprd',.BY),by='date'][ccy!='usd']
	oas_att<-att.dtl[field=='OAS_SPREAD_BID'][,regfun.local(.SD,c('eur','usd'),1,lhs='value',.BY),by='date'][ccy!='usd']


	ys_att.w<-ys_att %>% dcast(date~ccy,value.var='est')
	ys_att.w %>% ggplotw()	  
	ys_att[date==ymd('2014-11-28')]


	oas_att.w<-oas_att %>% dcast(date~ccy,value.var='est')
	oas_att.w %>% ggplotw()	  
	oas_att[date==ymd('2014-11-28')]

	lm(swapsprd~ccy+ytm_bucket,data=dtlattwin[date==ymd('2014-11-28')][ccy %in% c('1usd','eur')])

	dtlattwin[date==ymd('2014-11-28')][,mean(swapsprd),ccy]
	 
	dtlattwin[,mean(swapsprd),.(ccy,date)]  %>% dcast(date~ccy) %>% ggplotw
	(dtlattwin[,mean(swapsprd),.(ccy,date)]  %>% dcast(date~ccy))[date==ymd('2014-11-28')]

	dtlattwin[date==ymd('2014-11-28')][ytm_bucket=='3'][,mean(swapsprd),.(ccy,date)]
	dtlattwin[date==ymd('2014-11-28')][ytm_bucket=='4'][,mean(swapsprd),.(ccy,date)]
	#what's the interpolated 10 yr # dosn't matter

# is there a one to one mapping between ticker filed and pk??? yes!
	bondref[!is.na(ticker) & !is.na(pk)][,.N,pk]
	bondref[!is.na(ticker) & !is.na(pk)][,.N,ticker]
	pkticker_lookup<-bondref[!is.na(ticker) & !is.na(pk)][,.N,.(pk,ticker)][,.(pk,ticker)]
	# so I can download all the ones I haven't downloaded!
	tdd<-bondrefall[!is.na(ticker)] %>% anti_join(dtl.mo,by='pk') %>% issfilter(5)
	tdd2<-tdd[ccy %in% c('USD','EUR','JPY','CAD','CHF','GBP','AUD')] %>% issfilter(5,T)
	


		
