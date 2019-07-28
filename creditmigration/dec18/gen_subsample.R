source('../util.r')
require(xlsx)

dtl[,.N,ytm_bucket]
dtl[,.N,rating_bucket]

#############################################
## HG LG split for EUR

ysHG<-resyldsprdv4(dtl[rating_bucket==3],dtm$prl,regversion=3,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_HG=ysHG$regresult[ccy=='eur',.(date,`High Grade`=est)]

ysLG<-resyldsprdv4(dtl[(rating_bucket==2 | rating_bucket==1)],dtm$prl,regversion=3,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_LG=ysLG$regresult[ccy=='eur',.(date,`Low Grade`=est)]


psi_rating <- rbindlist(list('High Grade'=ysHG$regresult[,.(date,ccy,est)],'Low Grade'=ysLG$regresult[,.(date,ccy,est)]),idcol = 'type')

psi_rating[ccy=='eur'] %>% ggplot(aes(x=date,y=est,colour=type))+geom_line()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+ scale_color_manual(values=Lcolor)

#ggsave('ratingcomp.pdf',width=4, height=4,units='in')


corpbasis_eur_hglg=merge(corpbasis_HG,corpbasis_LG,by='date')
Lcolor=c('red','dark blue')
corpbasis_eur_hglg %>% ggplotw()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+ scale_color_manual(values=Lcolor)
ggsave('ratingcomp.pdf',width=4, height=4,units='in')

## Long vs Short split for EUR
ysShort<-resyldsprdv4(dtl[ytm_bucket==1],dtm$prl,regversion=3.5,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_short=ysShort$regresult[ccy=='eur',.(date,`Short Maturity`=est)]

ysLong<-resyldsprdv4(dtl[(ytm_bucket!=1)],dtm$prl,regversion=3.5,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_long=ysLong$regresult[ccy=='eur',.(date,`Long Maturity`=est)]

corpbasis_eur_shortlong=merge(corpbasis_short,corpbasis_long,by='date')
corpbasis_eur_shortlong %>% ggplotw()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+ scale_color_manual(values=Lcolor)
ggsave('maturitycomp.pdf',width=4, height=4,units='in')
###
#########################
## financial vs non-financial
#############################
dtl %>% ds
dtl[,.N,sic1][order(-N)]

dtl[,.N,.(sic1==6)]
dtl[,.N,issue_type_desc]

yslist <- list();
psi <- list()

#dtl<-dtl[ccy %in% c('usd','eur')]

yslist[['fin']]<-resyldsprdv4(dtl[sic1==6],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Financials']]=yslist[['fin']]$regresult[ccy=='eur',.(date,est)]

yslist[['nonfin']]<-resyldsprdv4(dtl[(sic1!=6)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Non-Finacials']]=yslist[['nonfin']]$regresult[ccy=='eur',.(date,est)]
# 
yslist[['supr']]<-resyldsprdv4(dtl[(sic1==9)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Supras']]=yslist[['supr']]$regresult[ccy=='eur',.(date,est)]


yslist[['tele']]<-resyldsprdv4(dtl[(sic1==4)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Telecomm']]=yslist[['tele']]$regresult[ccy=='eur',.(date,est)]


yslist[['manu']]<-resyldsprdv4(dtl[(sic1==2 | sic1==3)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Manufacturing']]=yslist[['manu']]$regresult[ccy=='eur',.(date,est)]



yslist[['others']]<-resyldsprdv4(dtl[(sic1 %ni% c(2,3,4,6,9))],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
psi[['Others']]=yslist[['others']]$regresult[ccy=='eur',.(date,est)]



#psim=merge(psi[['fin']],psi[['nonfin']],by='date')
psim=rbindlist(psi,idcol = 'type')
psim[,.N,type]
Lcolor=c('red','dark blue')
psim[type %in% c('Financials','Supras','Telecomm','Manufacturing','Others')] %>% ggplot(aes(x=date,y=est,color=type))+geom_line()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')#+ scale_color_manual(values=Lcolor)
ggsave('psi_4sectors.pdf',width=4, height=4,units='in')

Lcolor=c('red','dark blue')
psim[type %in% c('Financials','Non-Finacials')] %>% ggplot(aes(x=date,y=est,color=type))+geom_line()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+ scale_color_manual(values=Lcolor)
ggsave('psi_by_sector.pdf',width=4, height=4,units='in')




####################

###
shortHG<-resyldsprdv4(dtl[rating_bucket==3 & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
shortHG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')
corpbasis<-shortHG$regresult[,.(`Corporate Basis`=mean(na.omit(est))),date] 
corpbasis %>% ggplotw() +theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')
# corp basis 2 1-7 yr
shortHG2<-resyldsprdv4(dtl[(rating_bucket==3 | rating_bucket==2) & (ytm_bucket==1 | ytm_bucket==2)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis2<-shortHG2$regresult[,.(`Corporate Basis`=mean(na.omit(est))),date]; corpbasis2=corpbasis2[,.(date=floor_date(date,'month'),`1-7Y Corp`=`Corporate Basis`)]


du<-fread('Dudata.csv')
du[,date:=dmy(date)]
du[,.N,tenor]
tb<-du[group=='g10' & date>ymd('2004-01-01') & tenor=='1y',.(`Treasury Basis`=mean(cip_govt)),date]
tb<-tb[date %in% tb[,max(date),floor_date(date,'month')]$V1]

tbjkl<-fread('jkldata.csv')[,.(date=mdy(date),basisjkl=-10000*basis)][date>=ymd('2004-01-01')]

#merge(tb,tbjkl,by='date',rolling=T) %>% ggplotw()

bases_corp_trea<-merge(corpbasis[,.(date=floor_date(date,'month'),`1-3Y Corp`=`Corporate Basis`)],tbjkl[,.(date=floor_date(date,'month'),`Treasury`=basisjkl)],by='date',all=TRUE);bases_corp_trea<-merge(bases_corp_trea,corpbasis2,by='date',all=TRUE)
baseslong<-bases_corp_trea %>% melt(id.var='date') %>% na.omit()
baseslong$variable<-factor(baseslong$variable,levels=c( "Treasury","1-3Y Corp", "1-7Y Corp"))

baseslong[date<=ymd('2016-12-01')] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='right',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')

pdt_convyld<-baseslong[date<=ymd('2016-12-01')]
pdt_convyld %>% fwrite('plotdata_convyld.csv')
# bases_corp_trea<-merge(corpbasis[,.(date=floor_date(date,'month'),`Corporate Basis`)],tb[,.(date=floor_date(date,'month'),`Treasury Basis`)],by='date')[,.(`Corporate Basis`,`Treasury Basis`)] 
# bases_corp_trea %>% cor()



ggsave('treasuryvscorpbasis.pdf',width=6*3/4, height=3,units='in')
ggsave('../../paper/figures/treasuryvscorpbasis.pdf',width=6, height=4,units='in')


cip10 <- prl[ticker %in% c('eubs1','jybs1','adbs1','sfbs1','cdbs1','bpbs1','dkbs1','skbs1','nkbs1','ndbs1') & monthend==1,.(cip=mean(value)),.(date)]

cip6 <- prl[ticker %in% c('eubs1','jybs1','adbs1','sfbs1','cdbs1','bpbs1') & monthend==1,.(cip=mean(value)),.(date)]

merge(cip10,cip6,by='date') %>% ggplotw()

baseslong[date<=ymd('2016-12-01')] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()+scale_fill_manual(values=c('blue','blue','grey','red'))

dtA <-  baseslong[date<=ymd('2016-12-01')]
dtB <- cip6[year(date)>2003,.(date,variable='CIP deviation',value=-cip)]

save(dtA,dtB,file='data_treasury_corp_basis.RData')
plt1 <-dtA %>% ggplot(aes(x=date,y=value,colour=variable))+geom_area(data=dtB,aes(x=date,y=value),alpha=.25,fill='gray',linetype='blank')+xlab('date')+ylab('basis points')+ labs(fill='white')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+geom_line()+theme_few()+theme(legend.position='right',legend.title=element_text(colour='white'))
plt1
plt1 %>% ggsave(filename = 'treasurycorp2.pdf',plot=.,width=6, height=4,units='in')

plt1+ scale_color_manual(values=c('green','blue','white','red'))+scale_fill_manual(values=c('white','white','grey','white'))

merge(baseslong[date<=ymd('2016-12-01')],cip6[year(date)>2003,.(date,cip=-cip)],by='date',all=T) %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()+theme_few()+xlab('date')+ylab('basis points')+theme(legend.position='right',legend.title=element_text(colour='white'))+ labs(fill='')+scale_x_date(breaks=scales::pretty_breaks(n=7))+geom_hline(yintercept = 0,colour='grey')+geom_area(aes(x=date,y=value),alpha=.05)

############################
############################
############################
######### Others
############################
############################

# this is corpbasis 1 sent to JLK
shortHG<-resyldsprdv4(dtl[rating_bucket==3 & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
shortHG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')

shortLG<-resyldsprdv4(dtl[rating_bucket %in% c(1,2) & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
shortLG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')


longHG<-resyldsprdv4(dtl[rating_bucket==3 & ytm_bucket!=1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
longHG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')

longLG<-resyldsprdv4(dtl[rating_bucket  %in% c(1,2) & ytm_bucket!=1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
longLG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')

combined<-rbindlist(list(shortHG$regresult[,.(mean_corp_basis=mean(na.omit(est)),type='sHG'),date],
                         shortLG$regresult[,.(mean_corp_basis=mean(na.omit(est)),type='sLG'),date],
                         longHG$regresult[,.(mean_corp_basis=mean(na.omit(est)),type='lHG'),date],
                         longLG$regresult[,.(mean_corp_basis=mean(na.omit(est)),type='lLG'),date]))

combined %>% dcast(date~type,value.var='mean_corp_basis') %>% summary()
combined %>% ggplot(aes(x=date,y=mean_corp_basis,colour=type))+geom_line()

############################

## Aggregating
dtl2<-dtl %>% copy()
dtl2[ccy!='usd',ccy:='eur']
shortHG_agg<-resyldsprdv4(dtl2[rating_bucket==3 & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
shortLG_agg<-resyldsprdv4(dtl2[rating_bucket %in% c(1,2) & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
longHG_agg<-resyldsprdv4(dtl2[rating_bucket==3 & ytm_bucket!=1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
longLG_agg<-resyldsprdv4(dtl2[rating_bucket  %in% c(1,2) & ytm_bucket!=1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
combined<-rbindlist(list(shortHG_agg$regresult[,.(date,est,type='sHG')],
                         shortLG_agg$regresult[,.(date,est,type='sLG')],
                         longHG_agg$regresult[,.(date,est,type='lHG')],
                         longLG_agg$regresult[,.(date,est,type='lLG')]))
combined %>% ggplot(aes(x=date,y=est,colour=type))+geom_line()


aa<-shortHG$regresult[,.(mean_corp_basis=mean(na.omit(est))),date]
bb<-shortHG_agg$regresult[,.(date,aggcorpbasis=est)]

merge(aa,bb,by='date') %>% ggplotw()
[,.(mean_corp_basis=mean(na.omit(est))),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')





ggsave(file='../paper/figures/corpbasis.pdf',width=9,height=6)
dtl[rating_bucket==3 & ytm_bucket==1][,.N,nrating]
ys1meff$regresult %>% write.csv('corpbasis1.csv')

# this is corpbasis 2 sent to JLK
ys1meff<-resyldsprdv4(dtl[(rating_bucket==3 | rating_bucket==2) & (ytm_bucket==1 | ytm_bucket==2)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
ys1meff$regresult[,.(mean_corpbasis=mean(est),median_corpbasis=-median(est)),date] %>% ggplotw()
ys1meff$regresult %>% write.csv('corpbasis2.csv')


#############################################

###################
ys1meff<-NULL

dtl[(rating_bucket==2 | rating_bucket==1) & ytm_bucket==1,.N]
dtl[(rating_bucket==3) & ytm_bucket==1,.N]

ys1meff<-resyldsprdv4(dtl[(rating_bucket!=3) & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)

ys1meff<-resyldsprdv4(dtl[(rating_bucket==1 | rating_bucket==2) & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
ys1meff$regresult[,.(mean_corpbasis=-mean(na.omit(est)),median_corpbasis=-median(na.omit(est))),date] %>% ggplotw()


ys1meff$regcoef %>% ggplotw()

################################################
# ultimate parent == US vs not equal to US
#############################################

upnationlookup=bondrefall[,.N,.(upcusip,upnat)]

dtl2<-merge(dtl,upnationlookup[,.(upcusip,upnat)],by = 'upcusip')

nonUS<-resyldsprdv4(dtl2[upnat!='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
#nonUS<-resyldsprdv4(dtl2[upnat!='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_nonUS_eur<-nonUS$regresult[ccy=='eur',.(date,corpbasis_nonUS=est)]

US<-resyldsprdv4(dtl2[upnat=='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
#US<-resyldsprdv4(dtl2[upnat=='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_US_eur<-US$regresult[ccy=='eur',.(date,corpbasis_US=est)]

corpbasis_eur=merge(corpbasis_US_eur,corpbasis_nonUS_eur,by='date')
corpbasis_eur  %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')
ggsave(file='../paper/figures/corpbasis_USnonUS.pdf',width=9,height=6)

corpbasis_nonUS_eur_se<-nonUS$regresult[ccy=='eur',.(date,corpbasis_nonUS=se)]
corpbasis_US_eur_se<-US$regresult[ccy=='eur',.(date,corpbasis_US=se)]
corpbasis_eur_se=merge(corpbasis_US_eur_se,corpbasis_nonUS_eur_se,by='date')
corpbasis_eur_se %>% ggplotw()

#+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) 

################################################
# ultimate parent == US vs not equal to US, USD vs NonUSD
#############################################
upnationlookup=bondrefall[,.N,.(upcusip,upnat)]
dtl-,.N,ccy]
dtl2<-merge(dtl,upnationlookup[,.(upcusip,upnat)],by = 'upcusip')

dtl2[rating_bucket==3 & ytm_bucket==1,.N,.(ccy,upnat=='United States')]

dtl2[ccy!='usd',ccy:='eur']
nonUS<-resyldsprdv4(dtl2[rating_bucket==3 & ytm_bucket==1 & upnat!='United States'],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
#nonUS<-resyldsprdv4(dtl2[upnat!='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_nonUS_eur<-nonUS$regresult[ccy=='eur',.(date,corpbasis_nonUS=-est)]

US<-resyldsprdv4(dtl2[rating_bucket==3 & ytm_bucket==1 & upnat=='United States'],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
#US<-resyldsprdv4(dtl2[upnat=='United States'],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_US_eur<-US$regresult[ccy=='eur',.(date,corpbasis_US=-est)]

corpbasis_eur=merge(corpbasis_US_eur,corpbasis_nonUS_eur,by='date')
corpbasis_eur  %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')
ggsave(file='../paper/figures/corpbasis_USnonUS.pdf',width=9,height=6)

corpbasis_nonUS_eur_se<-nonUS$regresult[ccy=='eur',.(date,corpbasis_nonUS=se)]
corpbasis_US_eur_se<-US$regresult[ccy=='eur',.(date,corpbasis_US=se)]
corpbasis_eur_se=merge(corpbasis_US_eur_se,corpbasis_nonUS_eur_se,by='date')
corpbasis_eur_se %>% ggplotw()

#+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) 

#############################################
### short maturity, high grade/low grade
#############################################

ysHG<-resyldsprdv4(dtl[rating_bucket==3 & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_HG=ysHG$regresult[ccy=='eur',.(date,corpbasis_HG=-est)]

ysLG<-resyldsprdv4(dtl[(rating_bucket==2 | rating_bucket==1) & ytm_bucket==1],dtm$prl,regversion=1,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_LG=ysLG$regresult[ccy=='eur',.(date,corpbasis_LG=-est)]

corpbasis_eur_hglg=merge(corpbasis_HG,corpbasis_LG,by='date')
corpbasis_eur_hglg %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')


corpbasis_HG_se=ysHG$regresult[ccy=='eur',.(date,corpbasis_HG=se)]
corpbasis_LG_se=ysLG$regresult[ccy=='eur',.(date,corpbasis_LG=se)]
corpbasis_eur_hglg_se=merge(corpbasis_HG_se,corpbasis_LG_se,by='date')
corpbasis_eur_hglg_se %>% ggplotw()
ggsave(file='../paper/figures/corpbasis_HGLG.pdf',width=9,height=6)


corpbasis_eur_hglg %>% melt(id.vars='date')
dtplot=merge(corpbasis_eur_hglg %>% melt(id.vars='date'),corpbasis_eur_hglg_se %>% melt(id.vars='date'),by=c('date','variable'),suffixes = c('','_se'))

dtplot[,cimax:=value+2*value_se];
dtplot[,cimin:=value-2*value_se]

dtplot %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_ribbon(aes(ymin=cimin,ymax=cimax),color=c('blue','green'),alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme(axis.title.y=element_blank())

#############################################
### all maturity, high grade/low grade NET DEVIATION
#############################################

ysHG<-resyldsprdv4(dtl[rating_bucket==3],dtm$prl,regversion=3,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_HG=ysHG$regresult[ccy=='eur',.(date,corpbasis_HG=est)]

ysLG<-resyldsprdv4(dtl[(rating_bucket==2 | rating_bucket==1)],dtm$prl,regversion=3,adjccybs=1,returndt=T,parallel.core.=1)
corpbasis_LG=ysLG$regresult[ccy=='eur',.(date,corpbasis_LG=est)]

corpbasis_eur_hglg=merge(corpbasis_HG,corpbasis_LG,by='date')
corpbasis_eur_hglg %>% ggplotw()


corpbasis_HG_se=ysHG$regresult[ccy=='eur',.(date,corpbasis_HG=se)]
corpbasis_LG_se=ysLG$regresult[ccy=='eur',.(date,corpbasis_LG=se)]
corpbasis_eur_hglg_se=merge(corpbasis_HG_se,corpbasis_LG_se,by='date')
corpbasis_eur_hglg_se %>% ggplotw()


########################
#############################################
#############################################
#############################################
#############################################

#ys1meff$regresult[,.(mean_corpbasis=-mean(est),median_corpbasis=-median(est)),date] %>% ggplotw()




ys1meff<-NULL
ys1meff<-resyldsprdv4(dtl[(rating_bucket==1 | rating_bucket==2) & (ytm_bucket==1 | ytm_bucket==2)],dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=1)
ys1meff$regresult[,.(mean_corpbasis=-mean(est),median_corpbasis=-median(est)),date] %>% ggplotw() +theme_few()+geom_hline(yintercept = 0)+xlab('')+ylab('basis points')+theme(legend.position='bottom',legend.title=element_text(colour='white'))+ labs(fill='')
ggsave(file='../paper/figures/corpbasis.pdf',width=9,height=6)
bondref

dtl[ytm_bucket==1,.N,rating_bucket]
