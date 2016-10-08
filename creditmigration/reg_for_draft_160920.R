
# no major difference other than expanding the dataset
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
# setwd("~/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
# dtm$dtl4 %>% tocusip6(field='upcusip')
dtm$dtl4 %>% tocusip6(field='cu')
dtm$dtl4[,upcusip:=cusip6]
if(nrow(dtm$dtl4[str_length(upcusip)!=6])) print('error')
ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,returndt=T,parallel.core. = 8)
ys1meff<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=8)
dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)


# graph -------------------------------------------------------------------
# Export to stata
  # ys1m$regcoef %>% write.dta('dta/creditmisp_160908.dta')
  # ys1meff$regcoef %>% write.dta('dta/netmisp_160908.dta')
  # dt.mispl<-create.misp.long2(dtm$prw,ys1m$regresult,ys1meff$regresult)
  # dt.mispl[,.(date,ccy=as.character(ccy),credit,netmisp,cip)] %>% write.dta('dta/misplong_160908.dta')


# credit deviations
	ys1m$regcoef[,.(date,eur,gbp,aud,jpy)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_creditmisprice_160908.pdf',width=9,height=6)

# CIP 
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)

# Graphing CIP and credit mispriing overlay
	creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename='',yrstr.='10') 
	creditcip.result$dt.credit.cip

	# plot figure 3 EUR credit cip
	creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
	
	# ggsave(file='../paper/figures/EURcreditcip160908.pdf',width=9,height=6)
	
	# HG LG
	dtm$dtl4[,.N,pub]
	
	ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3,returndt=T,parallel.core. = 6)
	ys_hg<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating<=6],dtm$prl,regversion=3,returndt=T,parallel.core. = 6)
	# fread('rating.csv')
	# setnames(ys_hy,'ccyeur','Low Grade')
	# setnames(ys_hg,'ccyeur','High Grade (Single A or better)')
	
	ys_hy$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='lowgrade']
	ys_hg$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='highgrade']
	ys_byrating<-rbind(ys_hy$regresult,ys_hg$regresult)
	
	ys_byrating %>% ggplot(aes(x=date,y=est,colour=rating))+geom_line()+geom_errorbar(aes(ymin=cimin,ymax=cimax,colour=rating))+theme_few()
	
	setnames(ys_hy$regcoef,'eur','highyield')
	setnames(ys_hg$regcoef,'eur','highgrade')
	ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit deviation (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/hghy_eur_160908.pdf',width=9,height=6)
	
	
# Table -------------------------------------------------------------------
	require(sandwich);require(lmtest);require(texreg)
	
	## helper function  
	regtable_creditcip<-function(dtin){
  	reg_creditcip<-list()
  	reg_creditcip[[1]]<-dtin[ccy=='eur'] %>% neweymod('credit~cip')
  	reg_creditcip[[2]]<-dtin[ccy=='gbp'] %>% neweymod('credit~cip')
  	reg_creditcip[[3]]<-dtin[ccy=='jpy'] %>% neweymod('credit~cip')
  	reg_creditcip[[4]]<-dtin[ccy=='aud'] %>% neweymod('credit~cip')
  	reg_creditcip[[5]]<-dtin[ccy=='chf'] %>% neweymod('credit~cip')
  	reg_creditcip[[6]]<-dtin[ccy=='cad'] %>% neweymod('credit~cip')
    	stargazer::stargazer(reg_creditcip,type='text',report = "vct*")
    	dtout<-list()
    	dtout[[1]]<-as.data.frame.matrix(reg_creditcip[[1]]) %>% stack_card_shuffle() %>% as.data.table
  	dtout[[2]]<-as.data.frame.matrix(reg_creditcip[[2]]) %>% stack_card_shuffle() %>% as.data.table
  	dtout[[3]]<-as.data.frame.matrix(reg_creditcip[[3]]) %>% stack_card_shuffle() %>% as.data.table
  	dtout[[4]]<-as.data.frame.matrix(reg_creditcip[[4]]) %>% stack_card_shuffle() %>% as.data.table
  	dtout[[5]]<-as.data.frame.matrix(reg_creditcip[[5]]) %>% stack_card_shuffle() %>% as.data.table
  	dtout[[6]]<-as.data.frame.matrix(reg_creditcip[[6]]) %>% stack_card_shuffle() %>% as.data.table
  	rbindlist(dtout)
  }
  stack_card_shuffle <- function (mat_in){
    # convert mat_in from nx2 to 2nx1
    # stack by shuffling every other
    mat_out <- matrix(0,2*nrow(mat_in),1)
    for (i in 1:nrow(mat_in)){
      mat_out[(i*2-1):(i*2)] <- mat_in[i,1:2]
    }
    mat_out
  }
  
  creditcip.result$dt.credit.cip %>% regtable_creditcip()


  # better comparison(?) with  same maturity cip
  # ys1m$dtreg[,mean(ytm),ccy]
  # ys1m$dtreg[,median(ytm),ccy]
  # ys1m$dtreg[ccy=='eur',median(ytm),date] %>% ggplotw()
  # ys1m$dtreg[,median(ytm),date] %>% ggplotw()
# cip as credit-net
  # aa<-ys1m$regresult %>% copy()
  # bb<-ys1meff$regresult %>% copy()
  # cc<-aa[bb] %>% setnames(c('est','i.est'),c('credit','crediteff'))
  # cc[,cip:=credit-crediteff]
  # cc %>%   regtable_creditcip
  # cc[,cor(credit,cip)]
  # cc[,cor(credit,cip),ccy]
  # creditcip.result$dt.credit.cip[,cor(credit,cip)]
  # creditcip.result$dt.credit.cip[,cor(credit,cip),ccy]


## collapsing issuance:
	
	dtin<-read.dta13('sdc96_clean3.dta')	%>% as.data.table()
	dtin<-dtin[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
	dtin %>% tocusip6(field='upcusip')
	# dtin %>% tocusip6(field='cu')
	dtin[,upcusip:=cusip6]
	
	# collapse each
	source('util.r')
	load('db/bondrefall.RData')
	
	add.earlist.iss.in.ccy<-function(dtin,bondrefall){
	  bondrefall %>% tocusip6(field='upcusip')
	  bondrefall[,upcusip:=cusip6]
	  bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
	  refdt<-rbind(bondrefall,dtin,fill=T)
	  dtearlistforeign<-refdt[order(d)][,first(d),.(upcusip,ccy)] %>% dcast(upcusip~ccy,value.var='V1') #%>% melt(id.vars='upcusip',variable.name='ccy',value.name='earliest.iss.in.ccy')
	  dtearlistforeign %>% setnames(c('aud','cad','chf','eur','gbp','jpy','usd'),str_c('earlist.',c('aud','cad','chf','eur','gbp','jpy','usd')))
	  dtearlistforeign %>% setkey(upcusip)
	  dtin %>% setkey(upcusip)
	  dtin<-dtearlistforeign[dtin]
	  # dtin %>% anti_join(bondrefall,by='upcusip')
	  dtin
	  # dtearlistforeign[ccy=='usd'][is.na(earliest.iss.in.ccy)]
	}
	

	dtin2<-dtin %>% add.earlist.iss.in.ccy(.,dtin)
	dtin2 %>% setkey(upcusip,ccy)
	
	# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
	# dtin2<-dtin %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
	

	registerDoParallel(6)
	dtiss <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
	  dtin2 %>% icollapse4(.,iccy,filter=1)
	} %>% rbindlist()
	dtiss %>% setkey(date,ccy)
  
	registerDoParallel(6)
	dtiss.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
	  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
	} %>% rbindlist()
	dtiss.q %>% setkey(date,ccy)
	
	
		dtiss[,median(mu),ccy]
		dtiss[,mean(mu),ccy]
		dtiss[ccy=='eur',.(date,mu)][date>ymd('2007-01-01')] %>% ggplotw()
		dtiss[ccy=='eur',.(date,i_netflow)][date>ymd('2007-01-01')] %>% ggplotw()
		
## merging issuance
# revise the date in creditcip to begining of the month for merging with issuance
  dtcreditcip[,date:=lubridate::floor_date(date,'month')]
# merge monthly issuance with credit cip
  dtreg<-dtiss[date>=ymd('2004-01-01')][dtcreditcip,nomatch=0]
# construct quarterly using creditcip from begining of the month, merge with quarterly issuance
  dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
  dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
  dtcreditcip.q %>% setkey(date,ccy); dtiss.q %>% setkey(date,ccy)
  dtreg.q<-dtcreditcip.q[dtiss.q,nomatch=0]


reg1<-dtreg %>% lm(i_netflow~netmisp+factor(ccy),data=.); summary(reg1)
reg1<-dtreg[ccy=='eur'] %>% lm(i_netflow~netmisp,data=.); summary(reg1)
reg1<-dtreg[ccy=='eur'] %>% lm(i_netflow~credit+cip,data=.); summary(reg1)

reg1<-dtreg.q %>% lm(i_netflow~netmisp+factor(ccy),data=.); summary(reg1)
reg1<-dtreg.q[ccy=='eur'] %>% lm(i_netflow~netmisp,data=.); summary(reg1)
reg1<-dtreg.q[ccy=='eur'] %>% lm(i_netflow~credit+cip,data=.); summary(reg1)

reg2newey<-list()
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='eur'] %>% neweymod(.,i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='gbp'] %>% neweymod(.,i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='jpy'] %>% neweymod(.,i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='aud'] %>% neweymod(.,i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='chf'] %>% neweymod(.,i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='cad'] %>% neweymod(.,i_netflow~netmisp)
stargazer::stargazer(reg2newey,type='text',report = "vct*")

## make issuance lead by one period
bb<-dtreg.q[,.(date,F.i_netflow=shift(i_netflow,n=1,type='lead'),i_netflow,netmisp,credit,cip),.(ccy)]#[ date>ymd('2007-01-01')]
reg2newey<-list()
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='eur'] %>% neweymod(.,F.i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='gbp'] %>% neweymod(.,F.i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='jpy'] %>% neweymod(.,F.i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='aud'] %>% neweymod(.,F.i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='chf'] %>% neweymod(.,F.i_netflow~netmisp)
reg2newey[[length(reg2newey)+1]]<-bb[ccy=='cad'] %>% neweymod(.,F.i_netflow~netmisp)
stargazer::stargazer(reg2newey,type='text',report = "vct*")


# Event study using daily data -------------------------------------------------------------

	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')



	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	ys1<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=4,returndt = T,parallel.core.=30)
	save.image('dailyregrun.RData')
	save(dtmd,file='dtmd160912.RData')

	# ys2<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T,adjccybs=T)
	# save.image('dailyts_eu_clean_831_isstype4.RData')
	
	ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

	### all ccy daily
	ys3<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=4,returndt = T)
	# save(ys3,file='dailyts_allccy_clean_831_isstype4.RData')

	dt.mispl<-create.misp.long(dtmd$prw,ys1$regresult)
# merge issuance data

	dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]

	# US credit crisis
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='figs/eventstudy_creditcrunch',datetics=7)
	# plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='../paper/figures/eventstudy_creditcrunch',datetics=7)
	# slightly longer history and equally interesting:
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-07-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
	
	# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='../paper/figures/eventstudy_eusovereigncrisis')
	


