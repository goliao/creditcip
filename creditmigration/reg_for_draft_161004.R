# load data ---------------------------------------------------------------
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('util.r')
load('db/sdc.RData')


# Options -----------------------------------------------------------------
# set dtissraw to bondrefall %>% issfilter()

# use original bond data or current expanded

# set firm level to either upcusip or cu
firmlevel <- 'upcusip'

# individually constructing pairwise credit spread or construct all at the same time

# issfiltertype

# collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance

# taking quarterly credit/cip to be last date, or average of 3 end of month, or first month
#last

# individually generated residualized spread or generated together
bool.ind.resid <- 0

# preprocessing -----------------------------------------------------------

bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)

dtl<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]

bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
bondrefall<-bondrefall[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]


# residualize credit spread -----------------------------------------------------------

  ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = 8)

  ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=8)
    # reshape to long and merge with cip
  dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)
  
  ys1meff$regresult
    # cip as credit-net
  credit.cip.exact<-(ys1m$regresult[ys1meff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]

  dtcreditcip %>% write.dta('temp.dta')
  dtcreditcip %>% str
  credit.cip.exact[,ccy:=factor(ccy)] %>% write.dta('temp.dta')
  credit.cip.exact %>% str
# individually constructing pairwise credit spread
if (bool.ind.resid){
  ys2m<-list(); ys2meff<-list()
  for(iccy in c('eur','gbp','jpy','aud','chf','cad')){
    print(iccy)
    ys2m[[length(ys2m)+1]] <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 8))$regresult
    ys2meff[[length(ys2meff)+1]] <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=8))$regresult
  }
  ys2m.res<-rbindlist(ys2m)
  ys2meff.res<-rbindlist(ys2meff)
  dtcreditcip2<-create.dev.long2(prwin = dtm$prw,creditmispin = ys2m.res,netmispin = ys2meff.res)
  
  print(dtcreditcip[,cor(credit,cip)])
  print(dtcreditcip2[,cor(credit,cip)])

  # ys2meff.res %>% setkey(date,ccy)
  # aa<-ys1meff$regresult[ys2meff.res]
  # aa[ccy=='eur',.(date,est,i.est)] %>% ggplotw()
  # aa[ccy=='gbp',.(date,est,i.est)] %>% ggplotw()
  # aa[ccy=='jpy',.(date,est,i.est)] %>% ggplotw()
  # aa[ccy=='aud',.(date,est,i.est)] %>% ggplotw()
  # aa[ccy=='chf',.(date,est,i.est)] %>% ggplotw()
  # aa[ccy=='cad',.(date,est,i.est)] %>% ggplotw()
  creditcip.result<-plot.panel.creditcip(dtm$prw,ys2m.res,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
  dtcreditcip<-dtcreditcip2
}
  
  # aa<-(dtl[ccy %in% c('eur','usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 8,globaluponly=0))$regresult
  # aa  
  # bb<-create.dev.long2(prwin = dtm$prw,creditmispin = aa,netmispin = ys2meff.res)
  # bb[,.(date,cip,credit)] %>% ggplotw()

  
# VAR data export ---------------------------------------------------------
  dtissraw<-sdc %>% filter.sdc('6ccy,v2')
  # dtissraw<-bondrefall %>% issfilter(type=6) 
  dtissraw[,ccy:=str_to_lower(ccy)]
  dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
  dtissraw<-(dtissraw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
  
  
  
  version.var.data<-'pairwise,exact'

  if (version.var.data=='original'){
    dtcreditcip.m<-copy(credit.cip.exact)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
  } else if (version.var.data %like% 'original_approx'){
    # the non-exact version works etter with ordering of flow credit cip
    #dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
  } else if (version.var.data %like% 'pairwise'){
  # just pairwise eurusd
      iccy <- 'eur'
      yseurusd <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 8))$regresult
      yseurusdeff <- (dtl[ccy %in% c(iccy,'usd','1usd')] %>% resyldsprdv4(.,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=8))$regresult
      creditsingle<-create.dev.long2(prwin = dtm$prw,creditmispin = yseurusd,netmispin = yseurusdeff)
      # use exact
      if (version.var.data %like% 'exact') creditsingle[,cip:=credit-netmisp]
      dtcreditcip.m<-copy(creditsingle)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
  }

  dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.,dtissraw)
  dtin2 %>% setkey(upcusip,ccy)
  # limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
  # dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
  
  
  registerDoParallel(1)#,'gbp','jpy','aud','chf','cad'
  dtiss.collapse.m <- foreach(iccy=c('eur')) %dopar% {
    dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
  } %>% rbindlist()
  dtiss.collapse.m %>% setkey(date,ccy)
  
  dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
  dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
  # dtreg.m  %>% write.dta('vardatain_1007.dta')

# run a basic version
  dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,cip,credit)]
  res1<-vars::VAR(dtvar,1)
  res1 %>% summary
  resirf<-vars::irf(res1,ci=.95,runs=300) 
  resirf %>% plot.irf()

  * smooth i_netflow version
  dtvar<-dtreg.m[date<'2016-09-01'][date>='2005-01-01'][ccy=='eur'][,.(i_netflow.smooth,credit,cip)]
  res1<-vars::VAR(dtvar,1)
  res1 %>% summary
  resirf<-vars::irf(res1,ci=.95,runs=100) 
  resirf %>% plot.irf(v3='i_netflow.smooth')
  
  
# explore individual data points
  res1 %>% plot(names='i_netflow')
  res1 %>% plot(names='credit')
  res1 %>% plot(names='cip')
  res1$varresult$i_netflow$residuals
  dtreg.m[date<'2016-09-01'][ccy=='eur'][50:60][,.(date,i_netflow)] %T>% ggplotw()
  dtin2 %>% str
  dtin2[monthly==ymd('2008-07-01')][ccy=='eur'][modnat=='United States'][,.(i,amt,modnat,uop,ytofm)]#[,sum(amt)]

#could scale i_netflow it by rolling past avg


require(vars)
VARselect(dtvar,type='both',season = 3)
res1<-vars::VAR(dtvar,1,lag.max = 12,type ='both',season = 3)
res1 %>% summary
dtirf<-vars::irf(res1,runs=100) 
dtirf %>% plot

dtvar<-dtreg.m[ccy=='eur',.(credit,cip,lnmu=log(mu))]
res1<-vars::VAR(dtvar,1,lag.max = 12,type ='both',season = 3)
res1 %>% summary
dtirf<-vars::irf(res1,runs=100) 
dtirf %>% plot


dtvar<-dtreg.m[ccy=='eur',.(credit,cip,I_netflow)]
res1<-vars::VAR(dtvar,1)
res1 %>% summary
dtirf<-vars::irf(res1,runs=100) 
dtirf %>% plot

dtvar<-dtreg.m[ccy=='eur',.(credit,cip,i_netflow)]
res1<-vars::VAR(dtvar,1)
res1 %>% summary
dtirf<-vars::irf(res1,runs=100) 
dtirf %>% plot



require(urca)
summary(ur.df(diff(dtreg.m[ccy=='eur',credit]),type='drift',lags=1))

# Data summary ------------------------------------------------------------
  bucketytofm<-function(dtlin){
    dtlin<-dtlin[!is.na(ytofm)]
    dtlin[ytofm %between% c(0,3),ytofm_bucket:=1]
    dtlin[ytofm %between% c(3,7),ytofm_bucket:=2]
    dtlin[ytofm %between% c(7,10),ytofm_bucket:=3]
    dtlin[ytofm >10,ytofm_bucket:=4]
    #dtlin[,ytofm_bucket:=factor(ytofm_bucket)]
    dtlin
  }
  
dtl.bonds<-dtl[,.SD[1],pk]
dtl.bonds.global<-(dtl %>% filterglobaluponly())[,.SD[1],pk]
dtl.bonds[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds[sic1==6,sicind:=6];dtl.bonds[sic1==9,sicind:=9]


dtl.bonds<-dtl.bonds %>% bucketytofm(.)

dtl.bonds[,.(ytofm)] %>% ggplot(aes(x=ytofm))+geom_density()

summarylist<-list()
summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000)][,.(field='all',cat='',nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)][order(-notional)]
summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][order(-rating_bucket)][,.(field='rating',cat=rating_bucket,nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][order(ytofm_bucket)][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),sicind][order(sicind)][,.(field='ind',cat=sicind,nbonds,notional)]
sum1<-rbindlist(summarylist)
sum1 %T>% dt2clip()


dtl.bonds.global[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds.global[sic1==6,sicind:=6];dtl.bonds.global[sic1==9,sicind:=9]
dtl.bonds.global<-dtl.bonds.global %>% bucketytofm(.)
dtin.sum<-dtl.bonds.global
summarylist<-list()
summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000)][,.(field='all',cat='',nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)][order(-notional)]
summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][order(-rating_bucket)][,.(field='rating',cat=rating_bucket,nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][order(ytofm_bucket)][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),sicind][order(sicind)][,.(field='ind',cat=sicind,nbonds,notional)]
sum2<-rbindlist(summarylist)
sum2 %T>% dt2clip()
    
dtl.mo[,.N,pk]
  
  
# graph -------------------------------------------------------------------
# credit deviations
	dtcreditcip[ccy %in% c('eur','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_creditmisprice_160908.pdf',width=9,height=6)


# CIP 
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)
	
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs1,bpbs1,jybs1,adbs1)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 1-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig2_cip_1y_160929.pdf',width=9,height=6)
	#dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubsc,bpbsc,jybsc)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 1-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs1','bpbs1','eubs1','jybs1'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# Graphing CIP and credit mispriing overlay
	creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
	# creditcip.result<-plot.panel.creditcip(dtm$prw,ys2m.res,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
	
  aa<-creditcip.result$dt.credit.cip[,.(ccy,cip,credit)]#[credit>-130]
	aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
	ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Credit Spread Diff. in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=-80,y=40,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))
	#ggsave(file='../paper/figures/creditcipscatter.pdf',width=9,height=6)
	
# Net deviation
	dt.netmisp[ccy=='eur'][,.(date,est)] %>% ggplotw()
	dt.netmisp<-ys1meff$regresult
	#save(dt.netmisp,file='temp.RData')
	#load('temp.RData')
	dt.netmisp[,cimax:=est+1.96*se][,cimin:=est-1.96*se]
	get_legend<-function(myggplot){
	  tmp <- ggplot_gtable(ggplot_build(myggplot))
	  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	  legend <- tmp$grobs[[leg]]
	  return(legend)
	}
	require('gridExtra')
	fig7<-list()
	fig7[[1]]<-dt.netmisp[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+scale_linetype_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+ ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[2]]<-dt.netmisp[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[3]]<-dt.netmisp[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[4]]<-dt.netmisp[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[5]]<-dt.netmisp[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7[[6]]<-dt.netmisp[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
	fig7all<-grid.arrange(fig7[[1]],fig7[[2]],fig7[[3]],fig7[[4]],fig7[[5]],fig7[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
	fig7all	
	ggsave(file='../paper/figures/netmispall160925.pdf',fig7all,width=10.5,height=6.5)
	
	
 # plot figure 3 EUR credit cip
	creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
	# ggsave(file='../paper/figures/EURcreditcip160925.pdf',width=9,height=6)
	creditcip.result[[2]][ccy=='eur'][date>ymd('2015-01-01')] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
	
	
	
# HG LG
	dtl[,.N,pub]
	dthlgrade<-dtl[ccy %in% c('usd','eur')]#[pub != 'Govt']#[date>ymd('2006-01-01')]
	ys_hy<-dthlgrade[nrating>6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = 6)
	ys_hg<-dthlgrade[nrating<=6] %>% resyldsprdv4(.,dtm$prl,regversion=3,returndt=T,parallel.core. = 6)
	setnames(ys_hy$regcoef,'eur','highyield')
	setnames(ys_hg$regcoef,'eur','highgrade')
	ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit deviation (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/hghy_eur_160908.pdf',width=9,height=6)
	
	
	# fread('rating.csv')
	# setnames(ys_hy,'ccyeur','Low Grade')
	# setnames(ys_hg,'ccyeur','High Grade (Single A or better)')
	# ys_hy$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='lowgrade']
	# ys_hg$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='highgrade']
	# ys_byrating<-rbind(ys_hy$regresult,ys_hg$regresult)
	# ys_byrating %>% ggplot(aes(x=date,y=est,colour=rating))+geom_line()+geom_errorbar(aes(ymin=cimin,ymax=cimax,colour=rating))+theme_few()

	dtreg<-copy(dtl) %>% filterglobaluponly()
	# dtreg[,rating_bucket:=as.character(rating_bucket)]
	# dtreg[,ytm_bucket:=as.character(ytm_bucket)]
	dtreg[ccy=='usd',ccy:='1usd']
	dtreg[,ccy:=factor(ccy)]
	dtreg[,date:=factor(date)]
	
# plot HG/LG for all ccy
	# regressing every ccy together
	dtrating<-copy(dtl) %>% filterglobaluponly()
	yshg<-resyldsprdv4(dtrating[nrating %between% c(1,5)],prl,regversion=3,returndt=T,parallel.core.=6)
	yshy<-resyldsprdv4(dtrating[nrating>=6],prl,regversion=3,returndt=T,parallel.core.=6)
	dtplot.rating<-yshg$regresult[yshy$regresult] 
	dtplot.rating %>% setnames(c('est','i.est'),c('hg','lg'))
	dtplot.ratingl<-dtplot.rating %>% melt(id.vars=c('date','ccy'),measure.vars=c('hg','lg')) 
	dtplot.ratingl %>% plot.panel.creditrating(.,file='',wide=T)#../paper/figures/HGLG.pdf
  #ggsave(file='../paper/figures/HGLG.pdf',width=10.5,height=6.5)

	dtreg.hglg<-(dtplot.ratingl %>% dcast(date+ccy~variable))[,diff:=abs(lg)-abs(hg)] %>% setkey(date,ccy)
	dtreg.hglg[,mean(na.omit(diff))]
	dtreg.hglg %>% felm(diff~ccy+factor(date)|0,.)
	
# plot HG/LG for each ccy
	dtrating<-copy(dtl)#[pub!='Govt.'][pub=='Public'] 
  dtrating[,.N,pub]
	rating.list<-list()
	rating.plot.l<-list()
for(iccy in c('eur','gbp','jpy','aud','chf','cad')){
  print(iccy)
	dtrating.<-dtrating[ccy %in% c(iccy,'usd','1usd')][nrating!=0] %>% filterglobaluponly()
	ratingcutoff<-dtrating.[,median(nrating)]
	print(ratingcutoff)
  dtreghg<-dtrating.[nrating<=ratingcutoff]
  dtreglg<-dtrating.[nrating>ratingcutoff]
  yshg<-dtreghg %>% resyldsprdv4(.,prl,regversion=3,returndt=T,parallel.core.=6)
  yslg<-dtreglg %>% resyldsprdv4(.,prl,regversion=3,returndt=T,parallel.core.=6)
  dt.rating.i<-yshg$regresult[yslg$regresult] 
  dt.rating.i %>% setnames(c('est','i.est'),c('hg','lg'))
  dtout<-dt.rating.i %>% melt(id.vars=c('date','ccy'),measure.vars=c('hg','lg')) 
  dtout[,cutoff:=ratingcutoff]
  # rating.plot.l[[length(rating.plot.l)+1]]<-dtout %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
  rating.list[[length(rating.list)+1]]<-dtout
}

	dtratingresult<-rbindlist(rating.list)
  dtratingresult %>% plot.panel.creditrating(.,wide=T)
	

# Credit cip -------------------------------------------------------------------
	

# level
reg_creditcip<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcip[[length(reg_creditcip)+1]]<-creditcip.result$dt.credit.cip[ccy==iccy] %>% neweymod('credit~cip',value.name=iccy)
}
regtable1<-(rbindlist(reg_creditcip) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip 


credit.cip<-copy(creditcip.result$dt.credit.cip)
# monthly diff
credit.cip[,D.cip:=cip-shift(cip,n=1,type='lag'),ccy]
credit.cip[,D.credit:=credit-shift(credit,n=1,type='lag'),ccy]
# panel
credit.cip %>% felm(D.cip~D.credit|ccy|0|0,.) %>% summary
# individual diff regressions with robust S.E.
reg_creditcipb<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcipb[[length(reg_creditcipb)+1]]<-credit.cip[ccy==iccy] %>% regformat(formula='D.credit~D.cip',value.name=iccy)
}
regtable1b<-(rbindlist(reg_creditcipb) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1b


# quarterly diff
credit.cip.q<-copy(creditcip.result$dt.credit.cip)[order(date,ccy)][,date.qrt:=floor_date(date,'quarter')][,`:=`(cip.q=first(cip),credit.q=first(credit)),.(ccy,date.qrt)][,.(date.qrt,ccy,cip.q,credit.q)] %>% unique()
credit.cip.q[,D.cip:=cip.q-shift(cip.q,n=1,type='lag'),ccy]
credit.cip.q[,D.credit:=credit.q-shift(credit.q,n=1,type='lag'),ccy]
# panel
credit.cip.q %>% felm(D.cip~D.credit|ccy|0|0,.) %>% summary
# individual diff regressions with robust S.E.
reg_creditcipb<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_creditcipb[[length(reg_creditcipb)+1]]<-credit.cip.q[ccy==iccy] %>% regformat(formula='D.credit~D.cip',value.name=iccy)
}
regtable1b<-(rbindlist(reg_creditcipb) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regtable1b



# issuance flow and net dev -----------------------------------------------
# still need to floor month to get date at the begining of the month
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]

dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.,dtissraw)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
# dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()

# Monthly 6 month avg forward
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)

dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]

## make issuance lead by one period
dtreg.m[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.m[,i_netflow6mf:=(shift(i_netflow,n=1,type='lead')+shift(i_netflow,n=2,type='lead')+shift(i_netflow,n=3,type='lead')+shift(i_netflow,n=4,type='lead')+shift(i_netflow,n=5,type='lead')+shift(i_netflow,n=6,type='lead'))/6,ccy]

# Monthly regression with 6 month forwards
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip


# adding swap rate control
swap.rate<-prl[monthend==1 & date>=ymd('2004-01-01')][ticker %like% '^\\w\\wsw5$' | ticker %like% '^eusa5$',.(date,ticker,value=value*100)]
swap.rate[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf']
swap.rate<-swap.rate[ccy %in% c('eur','gbp','jpy','aud','chf','cad','usd'),.(date,ccy,swaprate=value)]
swap.rate[,swapraterel:=swaprate-.SD[ccy=='usd',swaprate],date]
swap.rate<-swap.rate[ccy!='usd'][,date:=floor_date(date,'month')][,.(date,ccy,swapraterel)] %>% setkey(date,ccy)

dtreg.m<-swap.rate[dtreg.m]
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.m[ccy==iccy] %>% neweymod(i_netflow6mf~netmisp+swapraterel,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev[,.(rn,eur,gbp,jpy,aud,chf,cad)] %T>% dt2clip



#Quarterly
# collapsing each ccy pair
registerDoParallel(6)
dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
} %>% rbindlist()
dtiss.collapse.q %>% setkey(date,ccy)

## merging issuance
# construct quarterly using creditcip from begining of the month, merge with quarterly issuance
#dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[1],.(date,ccy)]
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
# dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
dtcreditcip.q %>% setkey(date,ccy); dtiss.collapse.q %>% setkey(date,ccy)
dtreg.q<-dtcreditcip.q[dtiss.collapse.q,nomatch=0]

## make issuance lead by one period
dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]

# lead issue by 1 period: reg issflow on netmisp
reg_issnetdev<-list()
for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
  reg_issnetdev[[length(reg_issnetdev)+1]]<-dtreg.q[ccy==iccy] %>% neweymod(F.i_netflow~netmisp,value.name=iccy)
}
reg_issnetdev<-(rbindlist(reg_issnetdev) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
reg_issnetdev %T>% dt2clip



# abs net dev and D, all iss ----------------------------------------------

# collapse each
	
	dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.,dtissraw)
	dtin2 %>% setkey(upcusip,ccy)
	# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
	 dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# Monthly
	registerDoParallel(1)
	dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
	  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
	} %>% rbindlist()
	dtiss.collapse.m %>% setkey(date,ccy)
	
	## merging issuance

	dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
	dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
	dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]
	
	dtreg.m[,month:=month(date)]
  dtreg.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
  dtreg.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
  dtreg.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
  dtreg.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
  dtreg.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
  dtreg.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]
  
  # intial result on all issuance and net deviation reduction 
  dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|ccy,.) %>% summary()
  dtreg.m[ccy=='eur'] %>% lm(D.abs.cip~I_both,.) %>% summary()
  # dtreg.m %>% ggplot(aes(I_both,D.abs.credit))+geom_point(aes(colour=ccy))+stat_smooth(method = 'lm')
  # dtreg.m[ccy=='eur'] %>% ggplot(aes(I_both,D.abs.credit))+geom_point()+stat_smooth(method = 'lm')   

  
# debt maturity aggregate

	
# Monthly
	registerDoParallel(1)
	dtmat.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
	  	dtin2 %>% icollapse4.mature(.,iccy,collapse.freq = 'month',filter=1)
	} %>% rbindlist()
	dtmat.collapse.m %>% setkey(date,ccy)
	
	## merging matured with credit cip
	dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
	dtcreditcip.m %>% setkey(date,ccy); dtmat.collapse.m %>% setkey(date,ccy)
	dtreg.mat.m<-dtcreditcip.m[dtmat.collapse.m,nomatch=0]
	
	dtreg.mat.m[,month:=month(date)]
	dtreg.mat.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
	dtreg.mat.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
	dtreg.mat.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
	dtreg.mat.m[,D.I_both:=I_both-shift(I_both,n=1,type='lag'),.(ccy)]
	dtreg.mat.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
	dtreg.mat.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
	dtreg.mat.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]
	

	dtreg.mat.m %>% setnames('I_both','I_both.mat')
	dt.iss.mat.m<-update.dt(dtreg.m,dtreg.mat.m,keyfield = c('date','ccy'),override = T)
	dt.iss.mat.m %>% setnames(c('I_both'),c('I_both.iss'))
	dt.iss.mat.m[is.na(I_both.iss),I_both.iss:=0]
	

# properly do IV
  dt.iss.mat.m %>% ds
  regres<-list()
  regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | 0,.) 
  regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | ccy,.) 
  stargazer(regres,type='text',report='*vct*')
  regres[[2]] %>% summary(robust=T)
  regres[[2]] %>% summary(robust=F)
  
  # dt.iss.mat.m %>% write.dta('temp.dta')
  system('rm temp.csv')
  stata('format date %tm
  gen year=year(date)
  gen monthly=ym(year,month)
  format monthly %tm
  tsset ccy monthly
  eststo clear
  *eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust 
  *eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust 
  eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp i.ccy,robust 
  eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy,robust 
  *eststo: xi: reg I_both_iss I_both_mat i.ccy,robust 
  eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust 
  esttab using temp.csv, order(I_both*) bracket r2 nostar nogaps replace', data.in=dt.iss.mat.m[date<ymd('2016-07-30')])#[ccy %in% c('eur','gbp','chf','cad')]#dtout<-fread('temp.csv',sep=',')
  # dtout<-fread('temp.csv',sep=',')
  system('open temp.csv')
  # dtout<-read.csv('temp.csv',sep=',')
  
  
  # leverage ----------------------------------------------------------------
  
  dtlev<-read.dta13(file='bdlevq.dta') %>% as.data.table()
  dtlev<-dtlev[,month:=1+(quarter-1)*3][,date:=ymd(str_c(year,'-',month,'-01'))]
  
  # dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
  dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
  dtlev %>% setkey(date)
  dtreglev<-dtlev[dtcreditcip.q]
  dtreglev[,D.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
  dtreglev[,D.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
  dtreglev[,D.bdleverage2:=bdleverage2-shift(bdleverage2,n=1,type='lag'),.(ccy)]
  dtreglev[,D.levfac2:=levfac2-shift(levfac2,n=1,type='lag'),.(ccy)]
  dtreglev[,D.levfac2adj:=levfac2adj-shift(levfac2adj,n=1,type='lag'),.(ccy)]
  dtreglev[,D.lnbdleverage2:=log(bdleverage2)-log(shift(bdleverage2,n=1,type='lag')),.(ccy)]
  dtreglev[,D.lnbdleverage:=log(bdleverage)-log(shift(bdleverage,n=1,type='lag')),.(ccy)]
  # 
 
  #### this is what works::
  dtreglev <- dtreglev[,abscip:=abs(cip)][,abscredit:=abs(credit)]
  # dtreglev %>% write.dta('temp.dta')
  stata_src <- '
  format quarterly %tq
  eststo clear
  *eststo: xi:reg abscip levfac2adj i.ccy,robust
  *eststo: xi:reg abscredit levfac2adj i.ccy,robust
  tsset ccy quarterly 
  eststo: newey abscip levfac2adj,lag(4) force
  eststo: newey abscredit levfac2adj,lag(4) force
  esttab using temp.csv, bracket r2 nostar nogaps replace
  '
  stata(stata_src,data.in=dtreglev)
  system('open temp.csv')
  
  
  
  # Vix ---------------------------------------------------------------------
  # require(Quandl)
  # vix<-Quandl("CBOE/VIX") %>% as.data.table()
  # save(vix,file='db/vixraw.RData')
  load('db/vixraw.RData')
  vix<-vix[,date:=floor_date(Date,'month')][order(Date),.(vix=last(`VIX Close`)),date]
  vix %>% setkey(date)  
  
  dtcreditcip.<-copy(dtcreditcip)[,date:=floor_date(date,'month')]
  dtcreditcip. %>% setkey(date)
  dtregvix<-vix[dtcreditcip.]
  
  # 
  # regvixnewey<-list()
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='eur'] %>% neweymod(.,abs(credit)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='gbp'] %>% neweymod(.,abs(credit)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='jpy'] %>% neweymod(.,abs(credit)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='aud'] %>% neweymod(.,abs(credit)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='chf'] %>% neweymod(.,abs(credit)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='cad'] %>% neweymod(.,abs(credit)~vix)
  # stargazer::stargazer(regvixnewey,type='text',report = "vct*")
  # 
  # regvixnewey<-list()
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='eur'] %>% neweymod(.,abs(cip)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='gbp'] %>% neweymod(.,abs(cip)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='jpy'] %>% neweymod(.,abs(cip)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='aud'] %>% neweymod(.,abs(cip)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='chf'] %>% neweymod(.,abs(cip)~vix)
  # regvixnewey[[length(regvixnewey)+1]]<-dtregvix[ccy=='cad'] %>% neweymod(.,abs(cip)~vix)
  # stargazer::stargazer(regvixnewey,type='text',report = "vct*")
  # 
  # dtregvix[,cor(vix,abs(cip)),ccy]
  # dtregvix[,cor(vix,abs(credit)),ccy]
  
  # level result with VIX; but need to do with panel newey
  # regvix<-list()
  # regvix[[length(regvix)+1]]<-dtregvix %>% felm(abs(credit)~vix|ccy|0|ccy,data=.)
  # regvix[[length(regvix)+1]]<-dtregvix %>% felm(abs(cip)~vix|ccy|0|ccy,data=.)
  # stargazer(regvix,type='text',report = "vct*")
  
  
  #### this is what works::
  dtregvix <- dtregvix[,year:=year(date)][,month:=month(date)]
  dtregvix <- dtregvix[,abscip:=abs(cip)][,abscredit:=abs(credit)]
  stata_src <- '
  gen monthly=ym(year,month)
  format monthly %tm
  *eststo clear
  tsset ccy monthly 
  eststo: newey abscip vix,lag(12) force
  eststo: newey abscredit vix,lag(12) force
  esttab using temp.csv, bracket r2 nostar nogaps replace
  '
  stata(stata_src,data.in=dtregvix)
  system('open temp.csv')
  
  
  
  stata_src <- '
  gen monthly=ym(year,month)
  format monthly %tm
  *eststo clear
  eststo: reg abscredit vix, robust
  eststo: xi:reg abscredit vix i.ccy, robust
  eststo: xi:reg abscredit vix, robust
  tsset ccy monthly 
  eststo: newey abscredit vix,lag(0) force
  esttab
  esttab using temp.csv, bracket r2 nostar nogaps replace
  '
  stata(stata_src,data.in=dtregvix)
  
dtregvix %>% lm(cip~vix,.)
dtregvix %>% lm(cip~vix+factor(ccy),.)

dtregvix %>% lm(cip~vix+factor(ccy),.)


  
  
  
  
#  why Japan is the way it is   -------------------------------------------
## maybe workth reading dealer reports
  
  #Japan explore # by pub
  dtl[ccy %in% c('usd','1usd','jpy'),.N,pub]
  iccy='jpy'
  ys2m<-list(); 
  for(ipub in dtl[ccy %in% c('usd','1usd','jpy'),.N,pub][order(-N)]$pub){
    print(ipub)
    ys2m[[length(ys2m)+1]] <- (dtl[ccy %in% c(iccy,'usd','1usd')][pub==ipub] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 8))$regresult[,pub:=ipub]
    
  }
  ys2m.res<-rbindlist(ys2m)
  ys2m.res %>% dcast(date~pub,value.var='est') %>% ggplotw()
  
  
  # narrow within pub
  
  
  dtinspect<-dtl[ccy %in% c('usd','1usd','jpy') & pub=='Public'] %>% copy()
  
  dtinspect %>% ds
  
  category='rating_bucket'
  dtinspect[,.N,eval(exparse(category))][order(-N)]
  categorylist<-dtinspect[,.N,eval(exparse(category))][order(-N)][,exparse]
  
  ys2m<-list(); 
  for(icategory in categorylist){
    print(icategory)
    try({
      ys2m[[length(ys2m)+1]] <- (dtinspect[eval(exparse(category))==icategory] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = 8))$regresult[,eval(exparse(category)):=icategory]
    })
  }
  
	dtiss.collapse.q[ccy=='jpy',.(date,I_both,ccy)] %>% ggplot(aes(x=date,y=I_both,colour=ccy))+geom_line()
	dtiss.collapse.q[ccy=='jpy',.(date,i_netflow,ccy)] %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()
	dtregiss.q[ccy=='jpy',.(date,i_netflow,netmisp)] %>% ggplotw()
	dtregiss.q[ccy=='jpy',.(date,mu*100,netmisp)] %>% ggplotw()
	dtregiss.q[ccy=='jpy',.(date,i_netflow,netmisp)]
	
	
	
	ys2m.res<-rbindlist(ys2m)
	ys2m.res %>% dcast(date~eval(exparse(category)),value.var='est') %>% ggplotw()
	ys2m.res %>% dt2clip()
	# 2/27/09
	
	aa<-(dtinspect[date==mdy('6/30/09')] %>% filterglobaluponly())
	aa<-(dtinspect[date==mdy('2/27/09')] %>% filterglobaluponly())
	aa<-(dtinspect[date==mdy('5/31/10')] %>% filterglobaluponly())
	aa<-(dtinspect[date==mdy('12/31/15')] %>% filterglobaluponly())
	
	
	aa<-dtinspect[date==mdy('6/30/09')] # %>% filterglobaluponly())
	
	
	aa[ccy=='usd',ccy:='1usd']
	aa %>% lm(swapsprd~ccy+upcusip+rating_bucket+ytm_bucket,data=.)
	aa[,.(median(swapsprd),mean(swapsprd)),ccy]
	getccyFE2(aa,fieldstr='swapsprd',version=4,winsor=.025,parallel.core=1)
	
	df2<-copy(aa);lhs='swapsprd'; winsor=.025
	df2[,pctl:=percent_rank(eval(exparse(lhs))),by=.(date,ccy)]
	df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
	# set alphabetical order such that dummies are on foreign ccys
	df2[ccy=='usd',ccy:='1usd']
	# introduce liquidity measure based on bond age
	df2[,liq:=ytm/ytofm]
	df2<-df2[liq %between% c(.0,1.1)]
	df2[liq<.5,liq_bucket:=0] # more illiq
	df2[liq>=.5,liq_bucket:=1] # liq
	felm(eval(exparse(lhs))~ccy | upcusip+ytm_bucket+rating_bucket | 0 | 0, data=df2)
	df2[,mean(swapsprd),ccy]
	bb<-(df2[,mean(swapsprd),.(ccy,upcusip)] %>% dcast(upcusip~ccy))[,crd:=jpy-`1usd`]
	cc<-bondref[,.(upcusip,name,upnames,tf_macro_desc)][bb,mult='first',on='upcusip']
	cc[order(crd)]
	
	
	
	
  
  

		

  # Event study using daily data -------------------------------------------------------------

	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	
	firmlevel <- 'cu'
	bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
	
	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily %>% setkey(date)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	
	#ys1<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
	# save.image('dailyregrun.RData')
	# save(dtmd,file='dtmd160912.RData')

	# ys2<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T,adjccybs=T)
	# save.image('dailyts_eu_clean_831_isstype4.RData')
	
	ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

	
	
	
	### all ccy daily
	ys3<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=4,returndt = T)
	# save(ys3,file='dailyts_allccy_clean_831_isstype4.RData')

	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)

	# merge issuance data
	dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]

	
	# US credit crisis
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
	# plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='../paper/figures/eventstudy_creditcrunch',datetics=7)
	# slightly longer history and equally interesting:
	plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-07-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
	
	# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	
	
  # get the most common modnat, i, tf_mid, sic1 for each upcusip
  	upcusip.modnat<-bondref %>% Mode(.,'upcusip','modnat')
  	upcusip.i<-bondref %>% Mode(.,'upcusip','i')
  	upcusip.tf_mid_desc<-bondref %>% Mode(.,'upcusip','tf_mid_desc')
  	upcusip.sic1<-bondref %>% Mode(.,'upcusip','sic1')
  	dtl<-copy(dtmd$dtl4)
  
  	
	# custom define eurozone banks according to Ivashina, Scharfstein, Stein
  	# many of these banks have multiple upcusips, so need to combine the upcusips togetehr by picking the first one 
  	eurobank<-upcusip.i[,i:=str_to_lower(i)][i %like% 'credit ag'][,upcusipnew:=.SD[1,upcusip]]
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'societe generale'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'bnp'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'deutsche bank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'deutsche bank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'commerzbank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'unicredit'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'rabobank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'fortis bank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'banco bilbao vizcaya'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'banco santander'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'intesa sanpaolo'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'ing bank'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'westlb'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'bayernlb'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'landesbank baden-wuerttemberg'][,upcusipnew:=.SD[1,upcusip]])
  	eurobank<-rbind(eurobank,upcusip.i[,i:=str_to_lower(i)][i %like% 'landesbank'][,upcusipnew:=.SD[1,upcusip]])
	  # upcusip.i[,i:=str_to_lower(i)][i %like% 'landesbank']
  	eurobank %>% setkey('upcusip')
  	eurobank[,eurobank:=1]
  
  # update dtl to include new fields at the upcusip level
  	dtl %>% setkey('upcusip')
  	dtl<-merge(upcusip.modnat,dtl,by='upcusip',all.y = T)
  	dtl<-merge(upcusip.sic1,dtl,by='upcusip',all.y = T)
  	dtl<-merge(eurobank,dtl,by='upcusip',all.y = T)
  
  # set eurobank, combine upcusips for certain banks that has multiple upcusips listed
	  dtl[!is.na(upcusipnew),upcusip:=upcusipnew]
	  dtl[is.na(eurobank),eurobank:=0]
	
	# define date
	dtl2<-dtl[date %between% c(ymd('2010-06-06'),ymd('2012-12-30'))][ccy %in% c('usd','eur')]
	
	# eurozone bansk vs everyone else: using custom defintion
  	rm(ys1a,ys1b,dtp)
  	ys1a<-resyldsprdv4(dtl2[eurobank==1],dtmd$prl,regversion=4,returndt = T,parallel.core.=1)
  	ys1b<-resyldsprdv4(dtl2[eurobank==0],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	dtp<-ys1a$regcoef[ys1b$regcoef] %>% setnames(c('eur','i.eur'),c('banks','non-banks'))
  	dtp %>% ggplotw()
  	ys1b$regcoef %>% ggplotw()
  	
	
	
	# eurozone bansk vs everyone else
  	rm(ys1a,ys1b,dtp)
  	ys1a<-resyldsprdv4(dtl2[modnat=='Eurozone' & sic1==6],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	ys1b<-resyldsprdv4(dtl2[!(modnat=='Eurozone' & sic1==6)],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	dtp<-ys1a$regcoef[ys1b$regcoef] %>% setnames(c('eur','i.eur'),c('banks','non-banks'))
  	dtp %>% ggplotw()
  	
  	ys1a$regcoef[abs(eur)<500] %>% ggplotw
  	ys1b$regcoef[abs(eur)<500] %>% ggplotw
  	
	
	# eurozone bansk vs everyone else: by tfmid
  	dtl[,.N,tf_mid_desc][order(tf_mid_desc)]
  	ys1a<-resyldsprdv4(dtl2[modnat=='Eurozone' & tf_mid_desc=='Banks'],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	ys1b<-resyldsprdv4(dtl2[!(modnat=='Eurozone' & tf_mid_desc=='Banks')],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	ys1a$regcoef[abs(eur)<500][ys1b$regcoef] %>% ggplotw()
  	
	
	# eurozone bansk vs otehr eurzone firms
  	ys1a<-resyldsprdv4(dtl2[modnat=='Eurozone'  & sic1==6],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	ys1b<-resyldsprdv4(dtl2[(modnat=='Eurozone' & sic1!=6)],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
  	ys1a$regcoef[ys1b$regcoef] %>% setnames(c('eur','i.eur'),c('banks','non-banks')) %>% ggplotw()
  	
	
	
	ys1a<-resyldsprdv4(dtl2[modnat=='Eurozone'],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
	ys1b<-resyldsprdv4(dtl2[(modnat!='Eurozone')],dtmd$prl,regversion=4,returndt = T,parallel.core.=8)
	ys1a$regcoef[ys1b$regcoef] %>% setnames(c('eur','i.eur'),c('eurozone firms','us.firms')) %>% ggplotw()
	
	
	
# ECB QE ------------------------------------------------------------------

	
	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
  ecbeligible<-fread('ecbeligible.csv',sep=',')	
  load('cspp_tickers_download_160927.RData')
  load('db/pk_lookup.RData')
  load('db/bondrefall.RData')
  load('db/sdc_raw.RData')
  setkey(dtl.daily,date)
  nmdates<-nonmarket.dates(dtl.daily,bondref)
  dtl.daily<-dtl.daily[!nmdates][date>'2015-06-01']
  dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
  
  dtl<-copy(dtmd$dtl4)
  ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))
  
# clean up upcusip code
  firmlevel <- 'upcusip'
	bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
	bondrefall <- (bondrefall %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
	dt.sdc.raw <- (dt.sdc.raw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
	dtl<-(dtl %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
	
	
	
## using all data
	# merge issuance data: need to use better issuance data
	dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]
	
	
	ys1<-resyldsprdv4(dtl,dtmd$prl,regversion=1,returndt = T,parallel.core.=8,globaluponly = 0)
	ys1$regresult[,se:=0]
	ys1$regresult
	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2015-01-01','2016-10-01'),event.dates.in=ecbqe,type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	# maybe prehaps people figured out the game
	
# trying to match monthly
	# merge issuance data: need to use better issuance data
	dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]
	
	dtl %>% setkey(pk); mo.pk<-dtl.mo[,.(pk)] %>% setkey(pk) %>% unique()
	dtl. <- dtl[mo.pk,nomatch=0] ### NO DIFF AT ALL!!!????? WEIRD
	
	ys1<-resyldsprdv4(dtl.,dtmd$prl,regversion=1,returndt = T,parallel.core.=8,globaluponly = 1)
	ys1<-resyldsprdv4(dtl.,dtmd$prl,regversion=4,returndt = T,parallel.core.=8,globaluponly = 1)
	ys1$regresult[,se:=0]
	ys1$regresult
	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2016-03-01','2016-10-01'),event.dates.in=ecbqe,type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	
	
	### only CSPP upparents
	dtl %>% setkey(pk);cspp.tickers2download %>% setkey(pk); cspp.tickers2download[,pk:=str_to_lower(pk)]
	dtl.<-dtl[cspp.tickers2download,nomatch=0]
	dtl.[,.N,ccy]
	ys1<-resyldsprdv4(dtl.[ccy %in% c('eur','usd')],dtmd$prl,regversion=1,returndt = T,parallel.core.=8,globaluponly = 1)
	ys1$regresult[,se:=0]
	ys1$regresult
	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2016-01-01','2016-10-01'),event.dates.in=ecbqe,type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	
	### w/o CSPP upparents
	dtl %>% setkey(pk);cspp.tickers2download %>% setkey(pk); cspp.tickers2download[,pk:=str_to_lower(pk)]
	dtl.<-dtl[!cspp.tickers2download]
	dtl.[,.N,ccy]
	ys1<-resyldsprdv4(dtl.,dtmd$prl,regversion=1,returndt = T,parallel.core.=8,globaluponly = 0)
	ys1$regresult[,se:=0]
	ys1$regresult
	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2016-01-01','2016-10-01'),event.dates.in=ecbqe,type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	
	
	
	# try to match ecb elgibile bonds from bloomberg to bondref
  	ecbeligible %>% setkey(pk); ecbeligible[,pk:=str_to_lower(pk)]
  	pk.figi %>% setkey(pk)
  	pk.isin %>% setkey(pk); pk.isin[,pk:=str_to_lower(pk)]
  	
  	pk.figi[ecbeligible,nomatch=0]
  	pk.isin %>% setkey(pk)
  	ecb.isin<-pk.isin[ecbeligible,nomatch=0]
  	
  	bondref %>% setkey(isin)
  	ecb.isin %>% setkey(isin)
  	
  	bondrefall %>% setkey(isin); dt.sdc.raw %>% setkey(isin)
  	ecb.eligible<-bondref[ecb.isin,nomatch=0] 
  	bondrefall[ecb.isin,nomatch=0][,.N,upcusip]
  	dt.sdc.raw[ecb.isin,nomatch=0][,.N,upcusip]
  	
  	# might be best to match using bondrfall and by pk
  	bondrefall %>% setkey(pk); ecbeligible
  	ecb.eligible2<-bondrefall[ecbeligible,nomatch=0]
  	
  	ecb.eligible[,.N,upcusip]
  	ecb.eligible2[,.N,upcusip]
  	ecb.eligible.upcusip<-rbind(ecb.eligible[,.N,upcusip][,.(upcusip)], ecb.eligible2[,.N,upcusip][,.(upcusip)]) %>% unique
  	
	# try to match ecb csbb isins
  	cspp<-fread('ecbcsppisin.csv',sep=',')[isin!=''];cspp %>% setkey(isin)
  	bondrefall %>% setkey(isin)
  	cspp.upcusip<-bondrefall[cspp,nomatch=0][,.N,upcusip][,.(upcusip)] %>% unique
  	# cspp.cu<-bondrefall[cspp,nomatch=0][,.N,cu][,.(cu)] %>% unique
  	cspp.upcusip %>% setkey(upcusip)
  	bondrefall %>% setkey(upcusip)
    bondrefall[cspp.upcusip,nomatch=0][,.N,pk]
    cspp.tickers2download<-bondrefall[cspp.upcusip,nomatch=0][!is.na(ticker),.(pk=str_c(ticker,' Corp'))] 
    save(cspp.tickers2download, file='cspp_tickers_download_160927.RData')
  
      	  
	# dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
	# 
  	

	upcusip.modnat<-bondref %>% Mode(.,'upcusip','modnat')
	upcusip.i<-bondref %>% Mode(.,'upcusip','i')
	upcusip.tf_mid_desc<-bondref %>% Mode(.,'upcusip','tf_mid_desc')
	upcusip.sic1<-bondref %>% Mode(.,'upcusip','sic1')
	dtl2<-copy(dtl)
	dtl2 %>% setkey('upcusip')
	dtl2<-merge(upcusip.modnat,dtl,by='upcusip',all.y = T)
	dtl2<-merge(upcusip.sic1,dtl,by='upcusip',all.y = T)
	

	 
	dtl3<-dtl2[date %between% c(ymd('2015-01-01'),ymd('2016-12-30'))]#[ccy %in% c('usd','eur')]#[sic1!=6][pub!='Govt.'][issue_type_desc=='Investment Grade Corporate'][str_to_lower(tf_mid_desc) %like% 'power|oil|gas']#[pub=='Public']
	dtl3[,.N,tf_mid_desc]
	
	
	ecb.eligible.upcusip %>% setkey(upcusip)
	dtl4<-dtl3[ecb.eligible.upcusip,nomatch=0]
	
	cspp.upcusip %>% setkey(upcusip); dtl3 %>% setkey(upcusip)
	dtl4<-dtl3[cspp.upcusip,nomatch=0]
	
	dtl4[,.N,modnat]
	
	# dtl3[,.N,upcusip]
	# dtl4[,.N,upcusip]
	dtl4<-dtl4[ccy=='usd',ccy:='1usd'] %>% filterglobaluponly()
	reg1<-dtl4 %>% felm(swapsprdadj~ccy+factor(date)|upcusip+ytm_bucket+rating_bucket,.); #stargazer(reg1,type='text')
	regcoef<-reg1$coefficients %>% as.data.table(keep.rowname=T)
	regcoef[rn %like% '^factor',`:=`(eur=swapsprdadj+regcoef[rn %like% 'ccyeur',swapsprdadj],date=ymd(str_sub(rn,-10)))]
	ys2<-regcoef[!is.na(date)]
	ys2[,.(date,eur)][date>ymd('2016-02-01')] %>% ggplotw()+geom_vline(xintercept = as.numeric(c(mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))))
	
	
	## ECB AND GBP
	dtl3<-dtl2[date %between% c(ymd('2015-01-01'),ymd('2016-12-30'))][ccy %in% c('gbp','eur')][sic1!=6][pub!='Govt.'][issue_type_desc=='Investment Grade Corporate']#[str_to_lower(tf_mid_desc) %like% 'power|oil|gas']#[pub=='Public']
	dtl4<-dtl3[ccy=='gbp',ccy:='1gbp'] #%>% filterglobaluponly()
	dtl4[ccy=='gbp',ccy:='1gbp'] #%>% filterglobaluponly()
	dtl4[,.N,ccy]
	reg1<-dtl4 %>% felm(swapsprdadj~ccy+factor(date)|upcusip+ytm_bucket+rating_bucket,.); #stargazer(reg1,type='text')
	regcoef<-reg1$coefficients %>% as.data.table(keep.rowname=T)
	regcoef[rn %like% '^factor',`:=`(eur=swapsprdadj+regcoef[rn %like% 'ccyeur',swapsprdadj],date=ymd(str_sub(rn,-10)))]
	ys2<-regcoef[!is.na(date)]
	ys2[,.(date,eur)][date>ymd('2016-02-01')] %>% ggplotw()+geom_vline(xintercept = as.numeric(c(mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))))
	
	
	
	dtl3[,.N,sic1]
	dtl3[,.N,sic1]
	dtl3[,.N,pub]
	
	
	ys1<-resyldsprdv4(dtl3,dtmd$prl,regversion=1,returndt = T,parallel.core.=8,globaluponly = 0)
	ys1$regresult[,se:=0]
	ys1$regresult
	dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
  plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2016-01-01','2016-10-01'),event.dates.in=ecbqe,type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis
	
	
	