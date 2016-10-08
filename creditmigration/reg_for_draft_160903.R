
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,returndt=T)
ys1meff<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,adjccybs=1,returndt=T)

ys1m$regcoef %>% write.dta('dta/creditmisp_160903.dta')
ys1meff$regcoef %>% write.dta('dta/netmisp_160903.dta')
dt.mispl<-create.misp.long2(dtm$prw,ys1m$regresult,ys1meff$regresult)
dt.mispl[,.(date,ccy=as.character(ccy),credit,netmisp,cip)] %>% write.dta('dta/misplong_160903.dta')

# ys1meff %>% write.dta('dta/creditmispri.dta') # ('effresys_160826_retro_0625.dta')
# Fig: 1 credit mispricings
	ys1m$regcoef[,.(date,eur,gbp,aud,jpy)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_creditmisprice_160903.pdf',width=9,height=6)
	
# fig: 4 CIP 
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)

	

# HG LG
	source('util.r')
	ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3,returndt=T)
	ys_hg<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating<=6],dtm$prl,regversion=3,returndt=T)
	# fread('rating.csv')
	# setnames(ys_hy,'ccyeur','Low Grade')
	# setnames(ys_hg,'ccyeur','High Grade (Single A or better)')

	ys_hy$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='lowgrade']
	ys_hg$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='highgrade']
	ys_byrating<-rbind(ys_hy$regresult,ys_hg$regresult)

	ys_byrating %>% ggplot(aes(x=date,y=est,colour=rating))+geom_line()+geom_errorbar(aes(ymin=cimin,ymax=cimax,colour=rating))+theme_few()

	setnames(ys_hy$regcoef,'eur','highyield')
	setnames(ys_hg$regcoef,'eur','highgrade')
	ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit mispricing (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/hghy_eur_160903.pdf',width=9,height=6)

# Graphing CIP and credit mispriing overlay
	setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	rm(list=ls(all=TRUE));
	load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	dtm_4<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
	ys1m_4<-resyldsprdv4(dtm_4$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm_4$prl,regversion=4,returndt=T)
	ys1m_4$regcoef[,.(date,eur)] %>% ggplotw()
	ys1<-ys1m_4$regcoef
	
	cip<-dtm_4$prw[,.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)]
	cip %>% setnames(c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'),c('eur','gbp','jpy','aud','chf','cad'))
	cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
	cipl %>% setkey(date,ccy)

	ys1m_4$regresult %>% setkey(date,ccy)
	dt.merged<-cipl[ys1m_4$regresult]
	dt.merged %>% setnames('est','credit')
	dt.merged[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]

	# dt.merged<-create.misp.long(dtm_4$prw,)
	dtcreditcip.plot<-dt.merged %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) 
	dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+theme_classic()+theme(legend.position='bottom')
	dt.corr.ccy<-dt.merged[,.(corr=cor(cip,credit)),ccy]



	get_legend<-function(myggplot){
	  tmp <- ggplot_gtable(ggplot_build(myggplot))
	  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	  legend <- tmp$grobs[[leg]]
	  return(legend)
	}
	require('gridExtra')
	fig6<-list()
	X11(width=7,height=9)
	fig6[[1]]<-dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='eur',corr],2))))
	legendcommon<-get_legend(fig6[[1]])
	fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
	fig6[[2]]<-dtcreditcip.plot[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='gbp',corr],2))))
	fig6[[3]]<-dtcreditcip.plot[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='jpy',corr],2))))
	fig6[[4]]<-dtcreditcip.plot[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='aud',corr],2))))
	fig6[[5]]<-dtcreditcip.plot[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='chf',corr],2))))
	fig6[[6]]<-dtcreditcip.plot[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Mispricing'))+scale_linetype_discrete('',labels = c('CIP','Credit Mispricing'))+ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='cad',corr],2))))
	fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
	fig6all	

	# ggsave(file='../paper/figures/creditcip160903.pdf',fig6all,width=7,height=9)

# plot figure 3 EUR credit cip
	dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='eur',corr],2))))
	# ggsave(file='../paper/figures/EURcreditcip160903.pdf',width=9,height=6)


# export price data for regression in stata
	load('db/prl.RData'); load('db/monthenddates.RData')
	p2dta<-prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^\\w\\wsz\\d+' | ticker %like% '^ussw\\d+',.(date,ticker,value)]
	p2dta %>% setkey(date)
	p2dta<-p2dta[monthenddates] 
	p2dta[,value:=value*100]
	p2dta %>% dcast(.,date~ticker) %>% write.dta('workspace826/pricesv1.dta')


# Event study using daily data
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


	

	plot.event.study.moiss2<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1,filepathhead='',datetics=10){
		
		figa<-dt.merged[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
		
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
	
	
	# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='../paper/figures/eventstudy_eusovereigncrisis')
	


