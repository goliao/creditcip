
# workspace160904.R
export PATH="~/miniconda2/bin:$PATH"
bsub -app generic-5g -q interactive -Is R
require(ggplot2)
require(data.table)
ggplot(data=data.table(x=1:10),aes(x=x))+geom_bar()

setwd("~/xccy/creditmigration")
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')

dtl.mo.new<-dtl.mo
load('db/archive/dtlmo160804.rdata');
dtl.mo.old<-dtl.mo

load('db/bondrefall.RData')
# do this better
pk.old<-dtl.mo.old[,.N,pk][,.(pk)]
pk.old %>% setkey(pk)
bondrefall %>% setkey(pk)
bondrefall[!pk.old,pk:=NA]
bondrefall %>% setkey(pk)
bondrefall[!pk.old][!is.na(pk)]
bondrefall[is.na(pk),pk:=str_c(tolower(ticker), ' corp')]

dtm.new<-preprocess(bondrefall,dtl.mo.new,prl,issfiltertype =4)
dtm.old<-preprocess(bondref,dtl.mo.old,prl,issfiltertype =4)

dtm.new$dtl4[,.N,pk][,.N]
dtm.old$dtl4[,.N,pk][,.N]

merge(dtm.new$dtl4, bondrefall[!is.na(pk) & is.na(ticker)][,.(pk)],by='pk')

aa<-merge(dtl.mo.new[,.N,pk], bondrefall,by='pk')
aa %>% setkey(pk)
unique(aa)[,.N,cu]
unique(aa)[mat2>ymd('2016-06-01'),sum(na.omit(amt))]

### duplicates!!!!
dtm.new$dtl4 %>% setkey(date,pk)
(dtm.new$dtl4 %>% showdups())[,.N,.(pk,ccy)]
(dtm.new$dtl4 %>% showdups())[,.N,.(pk,ccy)][,.N,ccy]

bb<-merge(dtm.new$dtl4[,.N,pk], bondrefall,by='pk') %>% unique()
bb[,.N,.(nrating)]
bb[,median(nrating),.(date,ccy)]
bb[,mean(na.omit(ytofm)),ccy]
bb[,median(na.omit(ytofm))]
bb[,summary(na.omit(ytofm))]
bb[ytofm<=30,median(na.omit(ytofm)),ccy]
bb[ytofm<=30,mean(na.omit(ytofm))]
bb[,.N,pub]

bb[pub=='Govt.'][,.N,issue_type_desc]
bb[pub=='Govt.'][,.N,tf_mid_desc]
bb[pub=='Govt.'][,.N,tf_macro_desc]
bb[pub=='Govt.'][tf_mid_desc=='National Government'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='Regional Government'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='Supranational'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,name]
bb[pub=='Govt.'][,.N,sicp]
bb[issue_type_desc=='Agency, Supranational, Sovereign'][,.N,pub]
bb[issue_type_desc=='Agency, Supranational, Sovereign'][pub=='Priv.'][,.N,name]
bb %>% ds()
bb[upcusip=='52537'][,.N,ccy]
(dtm.new$dtl4[upcusip=='52537'] %>% filterglobaluponly())[,.N,pk]

dtm.new$dtl4 %>% setkey(pk,date)


# number of firms-dates that has multiple ratings
ratingdistwithinfirm<-unique(dtm.new$dtl4)[nrating!=0,.N,.(date,upcusip,nrating)][,.(count=.N),.(upcusip,date)][,.N,count]
ratingdistwithinfirm[,pct:=N/sum(N)]
ratingdistwithinfirm


dt.rating.ts<-unique(dtm.new$dtl4)[nrating!=0,.(rating=mean(as.numeric(nrating))),.(date,ccy)]
dt.rating.tsw<-dt.rating.ts %>% dcast(date~ccy)
dt.rating.tsw[,`:=`(aud=aud-usd,cad=cad-usd,chf=chf-usd,eur=eur-usd,gbp=gbp-usd,jpy=jpy-usd)]

dt.rating.ts %>% ggplot(aes(x=date,y=rating,colour=ccy))+geom_line()
dt.rating.tsw[,.(date,aud,cad,eur,gbp,jpy,chf)] %>% ggplotw()
fread('rating.csv')

ys1m.new<-resyldsprdv4(dtm.new$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm.new$prl,regversion=4,returndt=T)
ys1m.old<-resyldsprdv4(dtm.old$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm.old$prl,regversion=4,returndt=T)


ys1m.new$regcoef[,.(date,eur,gbp,aud,jpy)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ys1m.new$regresult %>% ggplot(aes(x=date,y=se,colour=ccy))+geom_line()
ys1m.old$regresult %>% ggplot(aes(x=date,y=se,colour=ccy))+geom_line()

ys1m.new$regresult %>% setkey(date,ccy)
ys1m.old$regresult %>% setkey(date,ccy)

ys1m.new$regresult[ys1m.old$regresult][,.(date,ccy,sediff=se-i.se)][] %>% ggplot(aes(x=date,y=sediff,colour=ccy))+geom_line()

dtm.new$dtl4
dtm.old$dtl4


	cip<-dtm.new$prw[,.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)]
	cip %>% setnames(c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'),c('eur','gbp','jpy','aud','chf','cad'))
	cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
	cipl %>% setkey(date,ccy)

	ys1m.new$regresult %>% setkey(date,ccy)
	dt.merged<-cipl[ys1m.new$regresult]
	dt.merged %>% setnames('est','credit')
	dt.merged[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]

	# dt.merged<-create.misp.long(dtm.new$prw,)
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
	# X11(width=7,height=9)
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

	dt.merged[,.(corr=cor(cip,credit))]



# workspace160904.R

# net misp
	setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
	rm(list=ls(all=TRUE));
	load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	dtl.mo.new<-dtl.mo
	load('db/archieve/dtlmo160804.rdata');
	dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
	ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,returndt=T)
	ys1meff<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,adjccybs=1,returndt=T)

	ys1meff$regcoef[,.(date,eur,gbp,aud,jpy)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	ys1meff$regcoef %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))


	dt.netmisp<-ys1meff$regresult

	dt.netmisp[,cimax:=est+1.96*se][,cimin:=est-1.96*se]
	get_legend<-function(myggplot){
	  tmp <- ggplot_gtable(ggplot_build(myggplot))
	  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	  legend <- tmp$grobs[[leg]]
	  return(legend)
	}
	require('gridExtra')
	fig7<-list()
	# X11(width=7,height=9)
	fig7[[1]]<-dt.netmisp[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[2]]<-dt.netmisp[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[3]]<-dt.netmisp[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[4]]<-dt.netmisp[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[5]]<-dt.netmisp[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[6]]<-dt.netmisp[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7all<-grid.arrange(fig7[[1]],fig7[[2]],fig7[[3]],fig7[[4]],fig7[[5]],fig7[[6]],ncol=2,nrow=3,layout_matrix=rbind(c(1,2),c(3,4),c(5,6)),heights=c(2,2,2))
	fig7all	
	ggsave(file='../paper/figures/netmispall160906.pdf',fig7all,width=7,height=9)

	dtall<-create.misp.long2(dtm$prw,ys1m$regresult,ys1meff$regresult)
	dtall[,cor(cip,credit)]

	dtcreditcip<-copy(dtall[ccy=='eur',.(date,cip,credit)])
	dtcreditcip[,cipchg:=cip-lag(cip)][,creditchg:=credit-lag(credit)]
	width=24
	for (i in width+1:nrow(dtcreditcip)){
		dtcreditcip[i,regcoef:=lm(data=dtcreditcip[(i-width-1):i],creditchg~cipchg)$coefficients[['cipchg']]]
	}
	dtcreditcip %>% lm(data=.,creditchg~cipchg)$Summary
	dtcreditcip[,.(date,credit,cip)] %>% ggplotw() + geom_hline(yintercept=1)

