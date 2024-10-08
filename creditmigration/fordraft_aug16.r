
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
# load('dtclean160624.RData')

# load('tempmots.RData')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =3)
ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=6,returndt=T)
ys1m$regcoef[,.(date,eur)] %>% ggplotw()


dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =2)
ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=6,returndt=T)
ys1m$regcoef[,.(date,eur)] %>% ggplotw()

# type 1
dtm_1<-preprocess(bondref,dtl.mo,prl,issfiltertype =1)
ys1m_1<-resyldsprdv4(dtm_1$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm_1$prl,regversion=6,returndt=T)
ys1m_1$regcoef[,.(date,eur)] %>% ggplotw()

source('util.r')
dtm_4<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
ys1m_4<-resyldsprdv4(dtm_4$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm_4$prl,regversion=6,returndt=T)
ys1m_4$regcoef[,.(date,eur)] %>% ggplotw()
ys1<-ys1m_4$regcoef
prw<-dtm_4$prw


ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy')],dtm$prl,regversion=6,returndt=T)
ys1mallccy<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6,returndt=T)
ys1m$regcoef %>% write.dta('dta/resys_160825.dta')
ys2m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy')],dtm$prl,regversion=6,adjccybs=TRUE);
ys2mallccy<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6,returndt=T,adjccybs=TRUE)
ys2m$regcoef %>% write.dta('dta/effresys_160825.dta')
ys2mallccy$regcoef %>% write.dta('dta/effresys_160825b.dta')
# save(dtm,ys1m,ys2m, ys1mallccy,ys2mallccy,file='tempmots.RData')

ys1<-ys1mallccy$regcoef
prw<-dtm$prw

# Graphing CIP and credit mispriing overlay
	require('gridExtra')
	fig6<-list()
	X11(width=7,height=9)
	ys1.prw<-ys1[prw,nomatch=0][date>='2004-01-01']

	fig6[[1]]<-ys1.prw[,.(date,c=eur,b=eubs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+ scale_color_discrete('',labels = c('CIP deviations','Credit mispricing'))+scale_linetype_discrete('',labels = c('CIP deviations','Credit mispricing'))+ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	legendcommon<-get_legend(fig6[[1]])
	fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
	fig6[[2]]<-ys1.prw[,.(date,c=gbp,b=bpbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	fig6[[3]]<-ys1.prw[,.(date,c=jpy,b=jybs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+theme(legend.position='none')+ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	fig6[[4]]<-ys1.prw[,.(date,c=aud,b=adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+theme(legend.position='none')+ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	fig6[[5]]<-ys1.prw[,.(date,c=chf,b=sfbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+theme(legend.position='none')+ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	fig6[[6]]<-ys1.prw[,.(date,c=cad,b=cdbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='grey')+theme(legend.position='none')+ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.text.x = element_text(angle = 30, hjust = 1))
	fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
ggsave(file='../paper/figures/creditcipwithSSA160831.pdf',fig6all,width=7,height=9)
	#ggsave(file='../paper/figures/creditcipmispricings160825.pdf',fig6all,width=7,height=9)
	ys1.prw %>% write.dta('dta/cbmispricings160825.dta')


	ys1l<-ys1[,.(date,eur,gbp,jpy,aud,chf,cad)] %>% melt(id.vars='date')
	setnames(ys1l,c('variable','value'),c('ccy','credit'))
	setkey(ys1l,date,ccy)
	cip<-prw[date>'2004-01-01',.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)] %>% melt(id.vars='date')
	setnames(cip,c('variable','value'),c('ccy','cip'))
	cip[ccy=='eubs5',ccy:='eur'][ccy=='jybs5',ccy:='jpy'][ccy=='bpbs5',ccy:='gbp'][ccy=='adbs5',ccy:='aud'][ccy=='cdbs5',ccy:='cad'][ccy=='sfbs5',ccy:='chf']
	cip<-cip[!is.na(cip)]
	setkey(cip,date,ccy)
	dt.panel<-ys1l[cip,nomatch=0]
	#write.dta(dt.panel,file='dta/mispricings_long_160825.dta')





	dt.merged2<-dtm$prw[,.(date,eubs5,ussw5,eusa5,eusw5)][ys1m,nomatch=0][ys2m,nomatch=0]
	dt.merged2[,cipadj:=ccyeur-ccyeuradj]
	dt.merged2[,.(date,eubs5,cipadj)] %>% ggplotw(x11.=T)
	dt.merged2[,.(date,eubs5,cipadj,ccyeur)] %>% ggplotw()

ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur')],dtm$prl,regversion=1,returndt=F)
ys2m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur')],dtm$prl,regversion=1,adjccybs=TRUE);setnames(ys2m,'ccyeur','ccyeuradj')
	dt.merged2<-dtm$prw[,.(date,eubs5,ussw5,eusa5,eusw5)][ys1m,nomatch=0][ys2m,nomatch=0]
	dt.merged2[,cipadj:=ccyeur-ccyeuradj]
	dt.merged2[,.(date,eubs5,cipadj)] %>% ggplotw(x11.=T)
	dt.merged2[,.(date,eubs5,cipadj,ccyeur)] %>% ggplotw()

	#CI 
	ys1m$regresult[ccy=='eur'] %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),colour='black')+geom_line()+geom_point(size=1)
	ys1m$regresult %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se))+geom_line()+geom_point(size=1)

	ys1m$regresult[ccy=='aud'] %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),alpha=.3,colour=NA)+geom_line()

	ys1m$regresult[ccy=='aud'] %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),alpha=.3,colour=NA)+geom_line()


# Figure 1 CIP deviations
dfg<-prw[,.(date,eubs5,bpbs5,jybs5,adbs5)] 
setnames(dfg,old = c('eubs5','bpbs5','jybs5','adbs5'),new=c('EUR','GBP','JPY','AUD'))
fig1<-dfg %>% ggplotw()+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "EUR",'GBP','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig1
#ggsave(file='../paper/figures/fig1_cip.pdf',fig1,width=9,height=6)

# Figure 2 Credit mispring
fig2<-ys1[,.(date,ccyeur,ccygbp,ccyjpy,ccyaud)] %>% ggplotw(x11.=F)+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "EUR",'GBP','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig2




#ggsave(file='../paper/figures/fig2_creditmisprice.pdf',fig2,width=9,height=6)

# Figure 3 Credit mispring and CIP for EUR
fig3<-ys1[prw,nomatch=0][date>'2004-01-01',.(date,ccyeur,eubs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (EU-US)','CIP deviations 5yr (implied - actual euro funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig3
# dtm$prw %>% key(date) 

### construct credit mispricings with 95% CI
ys1m$regresult %>% setkey(date)
dtplot<-ys1m$regresult[ccy=='eur'][dtm$prw[,.(date,eubs5)],nomatch=0][date>'2004-01-01']
dtplot2<-melt(dtplot,id.vars=c('date','ccy','se'));dtplot2[variable=='eubs5',se:=0];
dtplot2[variable=='eubs5',variable:='cip deviation'][variable=='est',variable:='credit mispricing']
fig3b<-dtplot2 %>% ggplot(aes(x=date,y=value,colour=variable))+geom_errorbar(aes(ymin=value-1.96*se,ymax=value+1.96*se))+geom_line()+geom_point(size=1)+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('CIP deviations 5yr (implied - actual euro funding rate)','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
ggsave(file='../paper/figures/meetingwSam160810/fig3_creditCIPeur.pdf',fig3b,width=9,height=6)

dtplot2[variable=='credit mispricing'] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_errorbar(aes(ymin=value-1.96*se,ymax=value+1.96*se,colour='grey'))+geom_line()+geom_point(size=1)+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Credit mispricing (Residualized credit spread diff EU-US)','95% CI'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
ggsave(file='../paper/figures/fig_crediteur_CI.pdf',width=9,height=6)



dtplot2[date %between% c('2012-03-01','2012-09-01')] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_errorbar(aes(ymin=value-1.96*se,ymax=value+1.96*se))+geom_line()+geom_point(size=1)+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('CIP deviations 5yr (implied - actual euro funding rate)','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))

#ggsave(file='../paper/figures/fig3_creditCIPeur.pdf',fig3,width=9,height=6)

# Figure 4 Credit mispring and CIP for JPY
fig4<-ys1[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (JP-US)','CIP deviations 5yr (implied - actual yen funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig4
#ggsave(file='../paper/figures/fig4_creditCIPjpy.pdf',fig4,width=9,height=6)

# Figure ... : rating
#fread('rating.csv')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =3)
ys_hg<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating %between% c(1,6)],dtm$prl,regversion=3)
dtm$dtl4[,.N,date] 
dtm$dtl4[nrating>6,.N,date] %>% head(10)
ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6 & date>'2004-09-01'],dtm$prl,regversion=3)
fig5<-ys_hg$regcoef[ys_hy$regcoef][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('HG','HY'))+theme_stata(base_size = 12)+theme(axis.title.y = element_text(margin =margin(0, 0, 0, 0)))
fig5



# getting the correlation of CIP and credit mispricing by ccy
	ys1l<-ys1[,.(date,ccyeur,ccygbp,ccyjpy,ccyaud,ccychf,ccycad)] %>% melt(id.vars='date')
	ys1l[,variable:=str_sub(variable,4,6)]
	setnames(ys1l,c('variable','value'),c('ccy','credit'))
	setkey(ys1l,date,ccy)
	cip<-prw[date>'2004-01-01',.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)] %>% melt(id.vars='date')
	setnames(cip,c('variable','value'),c('ccy','cip'))
	cip[ccy=='eubs5',ccy:='eur'][ccy=='jybs5',ccy:='jpy'][ccy=='bpbs5',ccy:='gbp'][ccy=='adbs5',ccy:='aud'][ccy=='cdbs5',ccy:='cad'][ccy=='sfbs5',ccy:='chf']
	cip<-cip[!is.na(cip)]
	setkey(cip,date,ccy)
	dt.panel<-ys1l[cip,nomatch=0]
	#write.dta(dt.panel,file='mispricings_long_160730.dta')
	cor(dt.panel$credit,dt.panel$cip)
	dt.panel[,cor(credit,cip),by=ccy]

	# reg<-lm(credit~cip+factor(ccy)-1,data=dt.panel)
	# summary(reg)
	# summary(reg)$coefficients %>% write.csv('temp.csv')
	# table1<-stargazer::stargazer(reg,type='text')
	# reg<-lm(cip~credit+factor(ccy)-1,data=dt.panel)
	# summary(reg)

# Graphing CIP and credit mispriing overlay
	require('gridExtra')
	fig6<-list()
	X11(width=7,height=9)
	ys1.prw<-ys1[prw,nomatch=0][date>='2004-01-01']

	fig6[[1]]<-ys1.prw[,.(date,ccyeur,eubs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Credit mispricing','CIP deviations'))+ggtitle('EUR')+theme_stata()
	legendcommon<-get_legend(fig6[[1]])
	fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
	fig6[[2]]<-ys1.prw[,.(date,ccygbp,bpbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('GBP')
	fig6[[3]]<-ys1.prw[,.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('JPY')
	fig6[[4]]<-ys1.prw[,.(date,aud=ccyaud,basis_aud=adbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('AUD')
	fig6[[5]]<-ys1.prw[,.(date,ccychf,sfbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('CHF')
	fig6[[6]]<-ys1.prw[,.(date,ccycad,cdbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('CAD')
	fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
	#ggsave(file='../paper/figures/fig6_creditmispricings.pdf',fig6all,width=7,height=9)



# daily time series -------------------------------------------------------
	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')

	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	ys1<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T)
	ys2<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T,adjccybs=T)
	
	# save.image('tempdailyts.RData')
	load('tempdailyts.RData')
		#CI 
		ys1$regresult[ccy=='eur'] %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),colour='black')+geom_line()+geom_point(size=1)
		ys1$regresult %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se))+geom_line()+geom_point(size=1)

		ys1$regresult[ccy=='eur'][date %between% c('2011-05-01','2012-06-01')]  %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),alpha=.3,colour=NA)+geom_line()

		ys1$regresult[ccy=='aud'] %>% ggplot(aes(x=date,y=est,colour=ccy))+geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),alpha=.3,colour=NA)+geom_line()


		# (ys1$dtreg[,.N,.(date,ccy)] %>% dcast(date~ccy))[date %between% c('2014-12-15','2015-01-15')]
		# ys1$dtreg[,.N,.(date,ccy)] %>% dcast(date~ccy) %>% ggplotw()
		# dtregbr<-compare.dt(ys1$dtreg,bondref,'pk',mask=F)
		# dtregbr$AB[,sum(amt),ccy]
	ys1$regcoef %>% ggplotw()

	dt.merged<-dtmd$prw[,.(date,eubs5)][ys1$regcoef,nomatch=0]


	dt.merged2<-dtmd$prw[,.(date,eubs5,ussw5,eusa5,eusw5)][ys1$regcoef,nomatch=0][ys2$regcoef,nomatch=0]
		setnames(dt.merged2,'i.ccyeur','ccyeuradj')
		dt.merged2[,yyyymmdd:=str_replace_all(as.character,'-','')]
		dt.merged2 %>% write.csv('../QE/creditmispricing.csv')

	dt.merged2[,cipadj:=ccyeur-ccyeuradj]
	dt.merged2[,.(date,eubs5,cipadj)] %>% ggplotw(x11.=T)
	dt.merged2[,.(date,eubs5,cipadj,ccyeur)] %>% ggplotw()
	
	# ECB QE announcements
	ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))
	
	
	load('tempdailyts.RData')
	# dt.merged %>% ggplotw(x11. = T)
	dt.merged[date %between% c('2012-03-01','2016-07-01')] %>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))

	ys1m$regcoef[,.(date,ccyeur)][dt.merged[date %between% c('2012-03-01','2012-10-26')],nomatch=0]	%>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))
	ys1m$regcoef[,.(date,ccyeur)][dt.merged[date %between% c('2016-01-01','2016-07-26')]]  %>% ggplotw()
	dt.merged[date %between% c('2012-06-01','2012-10-01')] %>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))

	# credit crunch
	setnames(dt.merged,c('eur','eubs5'),c('crd_misprice','cip'))
	dt.merged[date %between% c('2007-09-01','2008-09-01')] %>% ggplotw()+geom_vline(xintercept = as.numeric(c(ymd('2008-03-16'),ymd('2008-09-15'))))+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('CIP deviations','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
	# ggsave(file='../paper/figures/meetingwSam160810/eventstudy_creditcrunch.pdf',width=9,height=6)

	# US QE 08-11
		qe.events<-fread.xls('EventStudy-eventdates.xlsx','QE')
		us.qe.dates<-qe.events[CB=='FedQE',ymd(yyyymmdd)];
		ust10<-prl[ticker=='cmtusd10',.(date,ust10=value,ust10.ret=value-lag(value))]; ust10 %>% setkey(date)
		sig.us.qe.dates<-ust10[date %in% us.qe.dates][abs(ust10.ret)>.1,date]
	dt.merged[date %between% c('2008-09-01','2013-02-01')] %>% ggplotw(x11.=T)+geom_vline(xintercept = as.numeric(sig.us.qe.dates))	
	dt.merged[date %between% c('2008-11-01','2009-04-01')] %>% ggplotw(x11.=T)+geom_vline(xintercept = as.numeric(sig.us.qe.dates))	
	dt.merged[date %between% c('2010-07-01','2011-01-01')] %>% ggplotw(x11.=T)+geom_vline(xintercept = as.numeric(sig.us.qe.dates))	
	dt.merged[date %between% c('2012-07-01','2012-10-01')] %>% ggplotw(x11.=T)+geom_vline(xintercept = as.numeric(sig.us.qe.dates))	


	# EUR soverign crisis 2011
	dt.merged[date %between% c('2011-05-01','2012-06-01')] %>% ggplotw(x11.=F)+xlab('')+ylab('bps')+ scale_color_discrete('',labels = c('CIP deviations','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
	ggsave(file='../paper/figures/meetingwSam160810/eventstudy_eurocrisis11.pdf',width=9,height=6)

	# ECB around 2012 # Draghi whatever it takes
	dt.merged[date %between% c('2012-03-01','2012-09-01')] %>% ggplotw()+geom_vline(xintercept = as.numeric(ecbqe))
	dtmd$prw[,.(date,ussw5,eusw5)][date %between% c('2012-03-01','2012-09-01')] %>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))

	# ECB around 2014
	dt.merged[date %between% c('2014-03-01','2015-07-01')] %>% ggplotw(x11. = F)+geom_vline(xintercept = as.numeric(ecbqe))+xlab('')+ylab('bps')+ scale_color_discrete('',labels = c('CIP deviations','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
	ggsave(file='../paper/figures/meetingwSam160810/eventstudy_ecb2014.pdf',width=9,height=6)
	# ECB around 2016
	dt.merged[date %between% c('2016-01-01','2016-07-26')] %>% ggplotw(x11. = F)+geom_vline(xintercept = as.numeric(ymd(c('2016-06-08','2016-03-10'))))+xlab('')+ylab('bps')+ scale_color_discrete('',labels = c('CIP deviations','Credit mispricing (EU-US)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
	ggsave(file='../paper/figures/meetingwSam160810/eventstudy_ecb2016.pdf',width=9,height=6)











	dt.merged[,diff(ccyeur)] %>% sd()
	dt.merged[,diff(ccyeur)] %>% sd()
	dtmd$prw[,.(date,eubs5)][ys1$regcoef][!obsdiscard][!holidays][date>'2008-01-01',diff(ccyeur)] %>% sd()
	dt.merged[,diff(eubs5)] %>% sd(na.rm = T)
	beep(sound=2)

	#plot a rolling covariance graph!!! somehow showing lead lag would be ideal


# quarter end dates   
credit.mispricing<-ys1$regcoef
quarterend<-credit.mispricing[,.(date,yrq=str_c(year(date),quarter(date)))][,.(date=max(date)),yrq]
quarterend %>% setkey(date)
xccy.mispricing<-dtmd$prw[date>='2004-01-01',.(date,eubs5)][!is.na(eubs5)]
quarterend.xccy<-xccy.mispricing[,.(date,yrq=str_c(year(date),quarter(date)))][,.(date=max(date)),yrq]
quarterend.xccy %>% setkey(date)
# get rid of year end dates that are not credit market quarter end dates in the xccy data
date.rid.xccy<-c(merge(quarterend,quarterend.xccy,all=TRUE)[is.na(yrq.x),date],ymd('2004-12-30'))
#redo find quarterend dates for xccy; after taking out the year end dates
xccy.mispricing<-xccy.mispricing[date %ni% date.rid.xccy]
quarterend.xccy<-xccy.mispricing[,.(date,yrq=str_c(year(date),quarter(date)))][,.(date=max(date)),yrq]
quarterend.xccy %>% setkey(date)
quarterend<-quarterend.xccy[date!='2016-07-28']
dt2mispricings<-xccy.mispricing[credit.mispricing,nomatch=0]
dt2mispricings[,`:=`(credit.chg1d=eur-lag(eur),xccy.chg1d=eubs5-lag(eubs5))]
dt2mispricings[quarterend,is.quarterend:=1][is.na(is.quarterend),is.quarterend:=0]
dt2mispricings[!is.na(credit.chg1d),.(credit.meanchg1d=mean(credit.chg1d),credit.medianchg1d=median(credit.chg1d),credit.sdchg1d=sd(credit.chg1d),xccy.meanchg1d=mean(xccy.chg1d),xccy.medianchg1d=median(xccy.chg1d),xccy.sdchg1d=sd(xccy.chg1d)),is.quarterend]
lm(xccy.chg1d~is.quarterend,data=dt2mispricings) %>% summary()
lm(credit.chg1d~is.quarterend,data=dt2mispricings) %>% summary()
dt2mispricings %>% ggplot(aes(x=xccy.chg1d,y=credit.chg1d,colour=factor(is.quarterend)))+geom_point()
lm(xccy.chg1d~credit.chg1d,data=dt2mispricings) %>% summary()

# ecb qe
ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))
dt2mispricings[date %in% ecbqe,is.ecbqe:=1];dt2mispricings[date %ni% ecbqe,is.ecbqe:=0];
dt2mispricings[date>=min(ecbqe)-10 & date<=max(ecbqe)+10][!is.na(credit.chg1d),.(.N,credit.meanchg1d=mean(credit.chg1d),credit.medianchg1d=median(credit.chg1d),credit.sdchg1d=sd(credit.chg1d),xccy.meanchg1d=mean(xccy.chg1d),xccy.medianchg1d=median(xccy.chg1d),xccy.sdchg1d=sd(xccy.chg1d)),is.ecbqe]
lm(xccy.chg1d~is.ecbqe,data=dt2mispricings[date>=min(ecbqe)-10 & date<=max(ecbqe)+10]) %>% summary()

# Fed qe?


