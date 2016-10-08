# workspace160831.R

	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')

	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	ys1<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T)
	ys2<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T,adjccybs=T)
	# save.image('dailyts_eu_clean_831_isstype4.RData')
	
	ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

### all ccy daily
	ys3<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=6,returndt = T)
	# save(ys3,file='dailyts_allccy_clean_831_isstype4.RData')
	beep()
	ys1$regresult %>% ds
	cip<-dtmd$prw[,.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)]
	cip %>% setnames(c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'),c('eur','gbp','jpy','aud','chf','cad'))
	cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
	cipl %>% setkey(date,ccy)
	ys3$regresult %>% setkey(date,ccy)
	dt.merged<-cipl[ys3$regresult]
	dt.merged %>% setnames('est','credit')
	dt.merged[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]
	dt.merged[,.N,ccy]

# merge issuance data
	dtiss<-read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]

	plot.event.study<-function(dtin, event.dates,type.=1){
		if (type.==1)
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
	 	else if(type.==2) # plot cipest as well
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','cipest','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.1) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','cipest','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','cipest','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
		else if(type.==3) # use ribbon instead
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))	+geom_ribbon(aes(ymin=cimin,ymax=cimax),alpha=.3,colour=NA) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
	}

	plot.event.study.moiss<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1){
		# dtin.in=dt.merged;dtiss.in=dtiss;ccy.='eur';date.range=c('2007-01-01','2016-08-01');event.dates.in=ecbqe;type.in=1
		figa<-dt.merged[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)
		fignetmisprice<-dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='orange'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))

		figb<-dtiss[date %between% date.range,.(date,eval(exparse(str_c('i_net_USD',toupper(ccy.)))),eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='dodge')+theme_few()+theme(legend.position='bottom')
		require('gridExtra')
		figab<-grid.arrange(figa,	fignetmisprice,figb,ncol=1,nrow=3,heights=c(3,2,2))
		figab
	}
	
	plot.event.study.moiss2<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1){
		# dtin.in=dt.merged;dtiss.in=dtiss;ccy.='eur';date.range=c('2007-01-01','2016-08-01');event.dates.in=ecbqe;type.in=1
		figa<-dt.merged[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)
		fignetmisprice<-dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_color_discrete('',labels='Net Mispricing (Credit-CIP)')

		figb<-dtiss[date %between% date.range,.(date,eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='identity')+xlab('')+ylab('bps')+scale_fill_discrete('',labels='Monthly Net Issuance Flow (EU to US)')+theme_few()+theme(legend.position='bottom')
		# figb<-figb+fignetmisprice
		# figb<-dtiss[date %between% date.range,.(date,eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='identity')+xlab('')+(dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line())	
		require('gridExtra')
		figab<-grid.arrange(figa,	fignetmisprice,figb,ncol=1,nrow=3,heights=c(3,2,2))
		figab
	}

	plot.event.study.moiss2<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1,filepathhead='',datetics=10){
		# dtin.in=dt.merged;dtiss.in=dtiss;ccy.='eur';date.range=c('2007-01-01','2008-08-01');event.dates.in=ecbqe;type.in=1;filepathhead='../paper/figures/eventstudytemp'
		figa<-dt.merged[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
		
# dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_color_discrete('',labels='Net Mispricing (Credit-CIP)')+scale_x_date(breaks=scales::pretty_breaks(n=15))

		figb<-dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_color_discrete('',labels='Net Mispricing (Credit-CIP)')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))

		figc<-dtiss[date %between% date.range,.(date,eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='identity')+xlab('')+ylab('$billions')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_fill_discrete('',labels='Monthly Net Issuance Flow (EU to US)')+theme_few()+theme(legend.position='bottom')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
		require('gridExtra')
		figab<-grid.arrange(figa, figb,figc,ncol=1,nrow=3,heights=c(3,2,2))
		if (filepathhead!=''){
			ggsave(filename=str_c(filepathhead,'.pdf'),plot=figab)
			ggsave(filename=str_c(filepathhead,'_A.pdf'),width=9,height=5,plot=figa)
			ggsave(filename=str_c(filepathhead,'_B.pdf'),width=9,height=3,plot=figb)
			ggsave(filename=str_c(filepathhead,'_C.pdf'),width=9,height=3,plot=figc)
		}
		figab
	}
	# US credit crisis
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='../paper/figures/eventstudy_creditcrunch',datetics=7)
	# slightly longer history and equally interesting:
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-07-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
	
	
	# figa<-dt.merged[ccy=='eur'][date %between% c('2007-09-01','2008-12-01')] %>% plot.event.study(.,event.dates=c(ymd('2008-03-16'),ymd('2008-09-15')),type.=1)
	# # figb<-dtiss[date %between% c('2007-09-01','2008-12-01'),.(date,i_net_USDEUR,I_net_USDEUR)] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='dodge')+theme_few()+theme(legend.position='bottom')
	# # require('gridExtra')
	# # figab<-grid.arrange(figa,figb,ncol=1,nrow=2,heights=c(3,2))

	# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='../paper/figures/eventstudy_eusovereigncrisis')
	# dt.merged[ccy=='eur'][date %between% c('2011-03-01','2012-10-01')]  %>% plot.event.study(.,event.dates=c(ymd('2011-05-01'),ymd('2012-07-01')),type.=1)

##################
## Other events not used
##################
# # ECB around 2014 # worth exploring maybe this can be made nicer with monthly issuance residualized for seaonal effect(? but as a percentage, there shouldn't be seaonal effect);
	plot.event.study.moiss(date.range=c('2014-03-01','2015-07-01'))

	# ECB around 2016
	plot.event.study.moiss(date.range=c('2016-01-01','2016-07-26'))
	# draghi whatever it takes
	plot.event.study.moiss(date.range=c('2012-03-01','2012-12-01'))


	# chf depeg # should really estimate eur/chf directly rather than usd/chf usd/eur
	plot.event.study.moiss(ccy.='chf',date.range=c('2014-01-01','2015-12-31'),event.dates.in=c(ymd('2015-01-15')))

	dt.merged[ccy=='chf'][date %between% c('2014-01-01','2015-12-31')]  %>% plot.event.study(.,event.dates=c(ymd('2015-01-15')),type.=1)
	dt.merged[ccy=='eur'][date %between% c('2014-01-01','2015-12-31')]  %>% plot.event.study(.,event.dates=c(ymd('2015-01-15')),type.=1)


	# gbp brexit
	plot.event.study.moiss(ccy.='gbp',date.range=c('2016-01-01','2016-08-31'),event.dates.in=c(ymd('2016-06-23')))	
	dt.merged[ccy=='gbp'][date %between% c('2016-01-01','2016-08-31')]  %>% plot.event.study(.,event.dates=c(ymd('2016-06-23')),type.=1)

	# jpy earthquak?
	plot.event.study.moiss(ccy.='jpy',date.range=c('2011-01-01','2011-08-31'),event.dates.in=c(ymd('2011-03-11')))	

	# jpy circa 2010
	plot.event.study.moiss(ccy.='jpy',date.range=c('2007-01-01','2015-08-31'),event.dates.in=c(ymd('2011-03-11')))	
	dt.merged[ccy=='jpy',.(date,credit,cip)] %>% ggplotw()



### quick look at what happened with Yen mispricing circa 2010
	setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	rm(list=ls(all=TRUE));
	load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	source('util.r')
	dtm_4<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
	ys1m_4<-resyldsprdv4(dtm_4$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm_4$prl,regversion=6,returndt=T)


	cip<-dtm_4$prw[,.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)]
	cip %>% setnames(c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'),c('eur','gbp','jpy','aud','chf','cad'))
	cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
	cipl %>% setkey(date,ccy)
	ys1m_4$regresult %>% setkey(date,ccy)
	dt.merged<-cipl[ys1m_4$regresult]
	dt.merged %>% setnames('est','credit')
	dt.merged[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]

	# get issuance data
		dtiss<-read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
		monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
		dtiss %>% setkey(year,month)
		monthend2 %>% setkey(year,month)
		# dtiss<-dtiss[monthend2][,date:=i.date]
		dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]

		plot.event.study.moiss3<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1,filepathhead='',datetics=10){
			# slight chg of legend label on iss flow compare to version 2 
			# dtin.in=dt.merged;dtiss.in=dtiss;ccy.='eur';date.range=c('2007-01-01','2008-08-01');event.dates.in=ecbqe;type.in=1;filepathhead='../paper/figures/eventstudytemp'
			figa<-dt.merged[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
			
			figb<-dt.merged[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_color_discrete('',labels='Net Mispricing (Credit-CIP)')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))

			figc<-dtiss[date %between% date.range,.(date,eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='identity')+xlab('')+ylab('$billions')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_fill_discrete('',labels='Monthly Net Issuance Flow (into US)')+theme_few()+theme(legend.position='bottom')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
			require('gridExtra')
			figab<-grid.arrange(figa, figb,figc,ncol=1,nrow=3,heights=c(3,2,2))
			if (filepathhead!=''){
				ggsave(filename=str_c(filepathhead,'.pdf'),plot=figab)
				ggsave(filename=str_c(filepathhead,'_A.pdf'),width=9,height=5,plot=figa)
				ggsave(filename=str_c(filepathhead,'_B.pdf'),width=9,height=3,plot=figb)
				ggsave(filename=str_c(filepathhead,'_C.pdf'),width=9,height=3,plot=figc)
			}
			figab
		}
	plot.event.study.moiss3(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='jpy',date.range=c('2004-01-01','2016-07-01'),event.dates.in='',type.in=1)





###### text analysis
	# redownload as much as possible
	# run on R console
		rm(list=ls())
		# setwd("/Users/gliao/Dropbox/sec")
		source('secutil.r')
		load('sp500v2/temp_sp500_dt_index.rdata')
		for (i in 1:nrow(dtind)){
		  if (dtind[i,downloaded]==0){
		    print(i)
		    try({
		      htmlout<-url2txt(dtind[i,path])
		      write(htmlout$htmltxtfull,file=str_c('sp500v2/htmlrec',i,'.html'))
		      if (length(htmlout$paratxt)>0){
		        save(htmlout,file=str_c('sp500v2/rec',i,'.rdata'))
		        dtind[i,downloaded:=1]
		        save(dtind,file='sp500v2/temp_sp500_dt_index.rdata')
		        rm(htmlout)
		      }
		    })}
		}
		        # ind1<-grep('hedg',htmlout$paratxt)
		        # ind2<-grep('debt|issu|borrow',htmlout$paratxt)
		        # ind3<-grep('currenc|exchang|fx',htmlout$paratxt)
		        # indleft<-Reduce(intersect,list(ind1,ind2,ind3))
		        # if (length(indleft)>0){
		        #   dtind[i,hedge:=length(indleft)]
		        #   print('hedge: e.g.:')
		        #   print(htmlout$paratxt[indleft[1]])
		        # }


	# 1. create pdf of hedging examples
	rm(list=ls())
	setwd("/Users/gliao/Dropbox/sec")
	source('secutil.r')
	load('sp500sec/temp_sp500_dt_index.rdata')

	dtdl<-dtind[downloaded==1]
	dtdl[,.N,cik]
	dtdl[hedge==1,.N,cik]
	dtind
	dtdl[,date:=ymd(date)]
	dtdl[,yrqrt:=str_c(year(date),quarter(date))]

	#plot yr qrt mention of hedging by firm
	dtdl %>% setkey(yrqrt)
	dtdl2<-dtdl[,.(hedge.N.yq=sum(hedge)),.(yrqrt,cik)][,hedged:=as.numeric(hedge.N.yq>0)]
	dtdl3<-dtdl2[,.(cik.N=.N,hedged=sum(hedged)),.(yrqrt)][,hfrac:=hedged/cik.N]
	dtdl3[,date:=yrqrt2date(yrqrt)]
	ggplot(dtdl3,aes(x=date,y=hfrac)) + geom_line()

	for (i in 1:nrow(dtind)){

	}

	get.para.relevant<-function(txtin){
		ind1<-grep('hedg',txtin)
        ind2<-grep('debt|issu|borrow',txtin)
        ind3<-grep('currenc|exchang|fx',txtin)
        indleft<-Reduce(intersect,list(ind1,ind2,ind3))
        if (length(indleft)>0){
	         txtin[indleft]
	    }
    }


    dtdl
    
    file.remove('test.txt')
    for (i in 1:nrow(dtdl)){
		# i=1
		tryCatch({
		load(str_c('sp500sec/rec',i,'.rdata'))
		if (exists('txt1'))	para.in<-txt1 else para.in<-htmlout$paratxt
		para.relavent<-para.in %>% get.para.relevant() %>% unlist()
			if (length(para.relavent)>0){
				print(i)
				sout<-paste(dtdl[i,.(i,conm,type,date,'')],collapse='\n')
				sout<-c(sout,paste(para.relavent,collapse = '\n...\n '))
				sout<-c(sout,'\n******\n')
				write.table(sout,file='test.txt',append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE)
			}
		}, error=function(e){print(e)})
	}



	# just download the files  
		rm(list=ls())
		setwd("/Users/gliao/Documents/sec")
		source('secutil.r')
			
		# get a comprehensive list of tickers for sp500
			sp500<-fread('sp500.csv',sep=',')[!is.na(CIK),.(CIK)]
			setnames(sp500,'CIK','cik')
			setkey(sp500,cik)
			sp500<-unique(sp500)
		# create path from query
			con <- dbConnect(drv=SQLite(), dbname="edgar_idx.db")
			res<-dbGetQuery(con,"select * from idx where (type='10-K' or type='10-Q') and date>='2007-01-01' and date<='2016-12-31'")
			dbDisconnect(con)
			dtind<-as.data.table(res)
			# merge with sp500
			dtind[,cik:=as.numeric(cik)]
			setkey(dtind,cik)
			dtind<-dtind[sp500]
			dtind[,downloaded:=0]
			dtind[,hedge:=0]
			# save(dtind,file='index160902.RData')		
		rm(list=ls())
		setwd("/Users/gliao/Documents/sec")
		source('secutil.r')
			download.file.wrap<-function(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_replace_all(dtind[1,path],'/','_')){
			  	tryCatch({download.file(urlstr,destfile = fileout)},error=function(e){print('skipped');return(NA)})
			}
		dtind[,.N]
		load('index160902.RData')
		for (i in 1:nrow(dtind)){
			if (!file.exists(str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))){
				print(i)
				download.file.wrap(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))
			}
		}

		# going backwards
		rm(list=ls())
		# setwd("/Users/gliao/Documents/sec")
		source('secutil.r')
			download.file.wrap<-function(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_replace_all(dtind[1,path],'/','_')){
			  	tryCatch({download.file(urlstr,destfile = fileout)},error=function(e){print('skipped');return(NA)})
			}
		load('index160902.RData')
		for (i in nrow(dtind):1){
			if (!file.exists(str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))){
				print(i)
				download.file.wrap(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))
			}
		}

		# starting from middle
		rm(list=ls())
		setwd("/Users/gliao/Documents/sec")
		source('secutil.r')
			download.file.wrap<-function(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_replace_all(dtind[1,path],'/','_')){
			  	tryCatch({download.file(urlstr,destfile = fileout)},error=function(e){print('skipped');return(NA)})
			}
		load('index160902.RData')
		for (i in 7000:16000){
			if (!file.exists(str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))){
				print(i)
				download.file.wrap(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')))
			}
		}		
# download.file('ftp://ftp.sec.gov/edgar/data/1800/0001104659-07-013496.txt',destfile = 'test.txt')
# download.file('http://www.google.com',destfile = 'test.html')

