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

	setnames(ys2$regcoef,'eur','euradj')
	dt.merged2<-ys1$regcoef[ys2$regcoef][,cipest:=eur-euradj]
	dt.merged2 %>% ggplotw()
	dt.merged2[,.(date,cipest,eubs5)] %>% ggplotw()

		
	
	dt.merged<-dtmd$prw[,.(date,eubs5)][ys1$regresult,nomatch=0][dt.merged2[,.(date,cipest)]]
	setnames(dt.merged,c('est','eubs5'),c('credit','cip'))
	dt.merged[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]

	plot.event.study<-function(dtin, event.dates,type.=1){
		if (type.==1)
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
	 	else if(type.==2) # plot cipest as well
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','cipest','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.1) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','cipest','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','cipest','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
		else if(type.==3) # use ribbon instead
			dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))	+geom_ribbon(aes(ymin=cimin,ymax=cimax),alpha=.3,colour=NA) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
	}


# US credit crisis
dt.merged[date %between% c('2007-09-01','2008-12-01')] %>% plot.event.study(.,event.dates=c(ymd('2008-03-16'),ymd('2008-09-15')),type.=1)
# EUR soverign crisis
dt.merged[date %between% c('2011-03-01','2012-10-01')]  %>% plot.event.study(.,event.dates=c(ymd('2011-05-01'),ymd('2012-07-01')),type.=1)
# ECB around 2014
dt.merged[date %between% c('2014-03-01','2015-07-01')] %>% plot.event.study(.,event.dates=c(ecbqe),type.=1)
# ECB around 2016
dt.merged[date %between% c('2016-01-01','2016-07-26')]  %>% plot.event.study(.,event.dates=c(ecbqe),type.=1)
# draghi whatever it takes
dt.merged[date %between% c('2012-03-01','2012-09-01')]






### all ccy 
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

	# US credit crisis
	dt.merged[ccy=='eur'][date %between% c('2007-09-01','2008-12-01')] %>% plot.event.study(.,event.dates=c(ymd('2008-03-16'),ymd('2008-09-15')),type.=1)
	# EUR soverign crisis
	dt.merged[ccy=='eur'][date %between% c('2011-03-01','2012-10-01')]  %>% plot.event.study(.,event.dates=c(ymd('2011-05-01'),ymd('2012-07-01')),type.=1)
	# ECB around 2014
	dt.merged[ccy=='eur'][date %between% c('2014-03-01','2015-07-01')] %>% plot.event.study(.,event.dates=c(ecbqe),type.=1)
	# ECB around 2016
	dt.merged[ccy=='eur'][date %between% c('2016-01-01','2016-07-26')]  %>% plot.event.study(.,event.dates=c(ecbqe),type.=1)
	# draghi whatever it takes
	dt.merged[ccy=='eur'][date %between% c('2012-03-01','2012-09-01')]  %>% plot.event.study(.,event.dates=c(ecbqe),type.=1)


	# chf depeg # should really estimate eur/chf directly rather than usd/chf usd/eur
	dt.merged[ccy=='chf'][date %between% c('2014-01-01','2015-12-31')]  %>% plot.event.study(.,event.dates=c(ymd('2015-01-15')),type.=1)
	dt.merged[ccy=='eur'][date %between% c('2014-01-01','2015-12-31')]  %>% plot.event.study(.,event.dates=c(ymd('2015-01-15')),type.=1)


	# gbp brexit
	dt.merged[ccy=='gbp'][date %between% c('2016-01-01','2016-08-31')]  %>% plot.event.study(.,event.dates=c(ymd('2016-06-23')),type.=1)