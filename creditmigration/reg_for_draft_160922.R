

# load data ---------------------------------------------------------------
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('util.r')

dtissraw<-read.dta13('sdc96_clean3.dta')	%>% as.data.table()
# dtissraw<-bondrefall %>% issfilter(type=6) 


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

dtissraw<-dtissraw[,ccy:=str_to_lower(ccy)][ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
dtissraw %>% tocusip6(field=firmlevel)
dtissraw[,upcusip:=cusip6]


# residualize credit spread -----------------------------------------------------------
  ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = 8)
  ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=8)
  # reshape to long and merge with cip
  dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)
  # cip as credit-net
  credit.cip.exact<-(ys1m$regresult[ys1meff$regresult] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]

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
  dtcreditcip<-dtcreditcip2
}

  

# graph -------------------------------------------------------------------
# credit deviations
	dtcreditcip[ccy %in% c('eur','gbp','jpy','aud')] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_creditmisprice_160908.pdf',width=9,height=6)

# CIP 
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)

# Graphing CIP and credit mispriing overlay
	creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
	# creditcip.result<-plot.panel.creditcip(dtm$prw,ys2m.res,filename='',yrstr.='5',wide=T) #../paper/figures/slides_panel2dev.pdf
	
  aa<-creditcip.result$dt.credit.cip[,.(ccy,cip,credit)]#[credit>-130]
	aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
	ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Credit Spread Diff. in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=-80,y=40,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))
	#ggsave(file='../paper/figures/creditcipscatter.pdf',width=9,height=6)
	
	ggplot(aa,aes(ccy,credit))+geom_point(colour=ccy)
	ggplot(data=aa,aes(x=cip,y=credit)) %>% geom_point()
	ggplot(data=aa,aes(x=cip,y=credit,colour=ccy)) %>% geom_point()
	aa
	
	
	creditcip.result$dt.credit.cip %>% geom_point(data=.,aes(x=cip,y=credit))
	
# plot figure 3 EUR credit cip
	creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Spread Diff. (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))
	# ggsave(file='../paper/figures/EURcreditcip160908.pdf',width=9,height=6)
	
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
	dtplot.ratingl %>% plot.panel.creditrating(.,file='../paper/figures/HGLG.pdf',wide=T)
  #ggsave(file='../paper/figures/HGLG.pdf',width=10.5,height=6.5)

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
	

	regtable_creditcip<-function(dtin){
	## helper function  
  	reg_creditcip<-list()
  	reg_creditcip[[1]]<-dtin[ccy=='eur'] %>% neweymod('credit~cip')
  	reg_creditcip[[2]]<-dtin[ccy=='gbp'] %>% neweymod('credit~cip')
  	reg_creditcip[[3]]<-dtin[ccy=='jpy'] %>% neweymod('credit~cip')
  	reg_creditcip[[4]]<-dtin[ccy=='aud'] %>% neweymod('credit~cip')
  	reg_creditcip[[5]]<-dtin[ccy=='chf'] %>% neweymod('credit~cip')
  	reg_creditcip[[6]]<-dtin[ccy=='cad'] %>% neweymod('credit~cip')
    	stargazer::stargazer(reg_creditcip,type='text',report = "vct*")
    reg_creditcip
  #   	dtout<-list()
  #   dtout[[1]]<-as.data.frame.matrix(reg_creditcip[[1]]) %>% stack_card_shuffle() %>% as.data.table
  # 	dtout[[2]]<-as.data.frame.matrix(reg_creditcip[[2]]) %>% stack_card_shuffle() %>% as.data.table
  # 	dtout[[3]]<-as.data.frame.matrix(reg_creditcip[[3]]) %>% stack_card_shuffle() %>% as.data.table
  # 	dtout[[4]]<-as.data.frame.matrix(reg_creditcip[[4]]) %>% stack_card_shuffle() %>% as.data.table
  # 	dtout[[5]]<-as.data.frame.matrix(reg_creditcip[[5]]) %>% stack_card_shuffle() %>% as.data.table
  # 	dtout[[6]]<-as.data.frame.matrix(reg_creditcip[[6]]) %>% stack_card_shuffle() %>% as.data.table
  # 	rbindlist(dtout)
  }

table.credit.cip<-  creditcip.result$dt.credit.cip %>% regtable_creditcip()
zz <- credit.cip.exact %>%   regtable_creditcip


 
	
# issuance flow and net dev -----------------------------------------------

	# collapse each
	
	dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.,dtissraw)
	dtin2 %>% setkey(upcusip,ccy)
	# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
	dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# Monthly
	registerDoParallel(6)
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
  
  regres<-list()
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.netmisp~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.credit~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.cip~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.netmisp~I_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.credit~I_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.cip~I_netflow|ccy,data=.)
  stargazer(regres,type='text',report='*vct*')
	
  dtreg.m %>% ds
  regres<-list()
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.netmisp~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.credit~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.cip~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.netmisp~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.credit~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.cip~I_both|ccy,data=.)
  stargazer(regres,type='text',report='*vct*')
  
# better clustering
  
  regres<-list()
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.netmisp~I_both|ccy|0|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.credit~I_both|ccy|0|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.cip~I_both|ccy|0|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.credit~I_both|ccy|0|ccy,data=.)
  regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.cip~I_both|ccy|0|ccy,data=.)
  stargazer(regres,type='text',report='*vct*')

  # dtreg.m %>% write.dta('temp.dta')
  stata('* stata code
        format date %tm
        gen year=year(date)
        gen monthly=ym(year,month)
        format monthly %tm
        eststo clear
        eststo: reg D_abs_netmisp I_both,robust
        eststo: xi: reg D_abs_netmisp I_both i.ccy,robust
        eststo: xi: reg D_abs_netmisp I_both i.ccy,robust cluster(ccy)
        eststo: cluster2 D_abs_netmisp I_both, fcluster(ccy)  tcluster(monthly)
        eststo: xi:cluster2 D_abs_netmisp I_both i.ccy, fcluster(ccy)  tcluster(monthly)
        eststo: xi:cluster2 D_abs_netmisp I_both i.ccy i.monthly, fcluster(ccy)  tcluster(monthly)
        tsset ccy monthly
        eststo: newey D_abs_netmisp I_both,lag(6) force
        *bootstrap "regress  D_abs_netmisp I_both" _b, reps(1000) cluster(ccy)
        esttab
        ', data.in=dtreg.m)
  
  dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|ccy,.) %>% summary()
  dtreg.m[ccy=='eur'] %>% lm(D.abs.cip~I_both,.) %>% summary()
  dtreg.m %>% ggplot(aes(I_both,D.abs.credit))+geom_point(aes(colour=ccy))+stat_smooth(method = 'lm')
  dtreg.m[ccy=='eur'] %>% ggplot(aes(I_both,D.abs.credit))+geom_point()+stat_smooth(method = 'lm')   

  
#12 month collapse only
  aa<-dtreg.m[,lapply(.SD,function(x){mean(na.omit(x))}),.(month),.SDcols=c('D.netmisp','D.credit','D.cip','D.abs.netmisp','D.abs.credit','D.abs.cip','I_both','I_netflow','i_netflow')]
  aa[ccy=='eur',.(month,D.netmisp,I_netflow)] %>% melt(id.vars='month') %>% ggplot(aes(x=month,y=value,colour=variable))+geom_line()
  aa[ccy=='eur',.(month,D.netmisp,I_netflow)] %>% lm(D.netmisp~I_netflow,data=.) %>% summary()
  
  regres<-list()
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~i_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~I_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~I_netflow|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~I_netflow|ccy,data=.)
  stargazer(regres,type='text',report='*vct*')
  regres<-list()
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.netmisp~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.credit~I_both|ccy,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.cip~I_both|ccy,data=.)
  stargazer(regres,type='text',report='*vct*')
  
  regres<-list()
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~i_netflow,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~i_netflow,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~i_netflow,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~I_netflow,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~I_netflow,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~I_netflow,data=.)
  stargazer(regres,type='text',report='*vct*')
  regres<-list()
  regres[[length(regres)+1]]<-aa %>% felm(D.netmisp~I_both,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.credit~I_both,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.cip~I_both,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.netmisp~I_both,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.credit~I_both,data=.)
  regres[[length(regres)+1]]<-aa %>% felm(D.abs.cip~I_both,data=.)
  stargazer(regres,type='text',report='*vct*')
  
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


# current period: reg issflow on netmisp
	reg2newey<-list()
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='eur'] %>% neweymod(.,i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='gbp'] %>% neweymod(.,i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='jpy'] %>% neweymod(.,i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='aud'] %>% neweymod(.,i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='chf'] %>% neweymod(.,i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtreg.q[ccy=='cad'] %>% neweymod(.,i_netflow~netmisp)
	stargazer::stargazer(reg2newey,type='text',report = "vct*")

## make issuance lead by one period
	dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
	dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
	dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
	dtreg.q[,D4.mu:=mu-shift(mu,n=4,type='lag'),ccy]
	dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]
	dtreg.q[,D4.netmisp:=netmisp-shift(netmisp,n=4,type='lag'),ccy]
	
	dtregiss.q<-dtreg.q#[date>ymd('2007-01-01')]

	reg2newey<-list()
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='eur'] %>% neweymod(.,F.i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='gbp'] %>% neweymod(.,F.i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='jpy'] %>% neweymod(.,F.i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='aud'] %>% neweymod(.,F.i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='chf'] %>% neweymod(.,F.i_netflow~netmisp)
	reg2newey[[length(reg2newey)+1]]<-dtregiss.q[ccy=='cad'] %>% neweymod(.,F.i_netflow~netmisp)
	stargazer::stargazer(reg2newey,type='text',report = "vct*")


# debt maturity aggregate
	# dtiss.collapse.q[,.(date,I_both,ccy)] %>% ggplot(aes(x=date,y=I_both,colour=ccy))+geom_line()
	# dtiss.collapse.q[,.(date,i_netflow,ccy)] %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()

	source('util.r')
	
# Monthly
	registerDoParallel(6)
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
	
	## old
	# stata('* stata code
	#       format date %tm
	#       gen year=year(date)
	#       gen monthly=ym(year,month)
	#       format monthly %tm
	#       eststo clear
	#       eststo: reg D_abs_netmisp I_both,robust
	#       eststo: xi: reg D_abs_netmisp I_both i.ccy,robust
	#       eststo: xi: reg D_abs_netmisp I_both i.ccy,robust cluster(ccy)
	#       *eststo: cluster2 D_abs_netmisp I_both, fcluster(ccy)  tcluster(monthly)
	#       eststo: xi:cluster2 D_abs_netmisp I_both i.ccy, fcluster(ccy)  tcluster(monthly)
	#       *eststo: xi:cluster2 D_abs_netmisp I_both i.ccy i.monthly, fcluster(ccy)  tcluster(monthly)
	#       *tsset ccy monthly
	#       *eststo: newey D_abs_netmisp I_both,lag(0) force
	#       *bootstrap "regress  D_abs_netmisp I_both" _b, reps(1000) cluster(ccy)
	#       esttab
	#       ', data.in=dtreg.mat.m)
	
	dtreg.mat.m %>% setnames('I_both','I_both.mat')
  
  #  merge  iss mature
	# dt.iss.mat.m<-merge(dtreg.mat.m[,.(date,ccy,I_both,D.abs.netmisp,D.abs.cip,D.abs.credit)],dtreg.m,keyby=c('date','ccy'),all=T)
	# dt.iss.mat.m %>% setnames(c('I_both','i.I_both'),c('I_both.mat','I_both.iss'))
	# 
	dt.iss.mat.m<-update.dt(dtreg.m,dtreg.mat.m,keyfield = c('date','ccy'),override = T)
	dt.iss.mat.m %>% setnames(c('I_both'),c('I_both.iss'))
	dt.iss.mat.m[is.na(I_both.iss),I_both.iss:=0]
	
	# dt.iss.mat.m[date==mdy('11/1/08') & ccy==	'cad']
	# dtreg.mat.m[date==mdy('11/1/08') & ccy==	'cad']
	#  
  # dtreg.m[is.na(D.abs.netmisp)]
  # dt.iss.mat.m[is.na(D.abs.netmisp)]
  # dtreg.mat.m[is.na(D.abs.netmisp)]
  # 
  #   aa<-dt.iss.mat.m[,.(date,ccy,D.abs.netmisp,I_both.mat)] %>% setkey(date,ccy) 
  # bb<-dtreg.mat.m[,.(date,ccy,D.abs.netmisp),I_both.mat] %>% setkey(date,ccy)
  # aa[bb] %>% dt2clip()
  # 
  # identical(aa[order(date,ccy)],bb[order(date,ccy)])
  # 

  ## correlation of first stage 
  # dt.iss.mat.m[!is.na(I_both.iss) & !is.na(I_both.mat)][,.(cor(I_both.iss,I_both.mat))]
  # dt.iss.mat.m[!is.na(I_both.iss) & !is.na(I_both.mat)][,.(cor(I_both.iss,I_both.mat))]
  # dt.iss.mat.m[!is.na(I_both.iss) & !is.na(I_both.mat)] %>% lm(I_both.iss~I_both.mat,.) %>% summary()
  # dtreg.m  
  # 
  # OLS and loose IV
  # new vs old version of iss/ OLS
  # regres<-list()
  # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.iss|ccy|0|0,data=.)
  # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.iss|ccy|0|ccy,data=.)
  # regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|0,data=.)
  # regres[[length(regres)+1]]<-dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|ccy,data=.)
  # stargazer(regres,type='text',report='*vct*')
  # 
  # 
  # regres<-list()
  # # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.iss|ccy|0|0,data=.)
  # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.iss|ccy|0|ccy,data=.)
  # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.mat|ccy|0|0,data=.)
  # regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.mat|ccy|0|ccy,data=.)
  # stargazer(regres,type='text',report='*vct*')
  # 
  # dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.mat|ccy|0|0,data=.) %>% summary(robust=T)
  # dt.iss.mat.m %>% felm(D.abs.netmisp~I_both.mat|ccy|0|ccy,data=.) %>% summary(robust=T)

# properly do IV
  dt.iss.mat.m %>% ds
  regres<-list()
  regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | 0,.) 
  regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | ccy,.) 
  stargazer(regres,type='text',report='*vct*')
  regres[[2]] %>% summary(robust=T)
  regres[[2]] %>% summary(robust=F)
  
  # dt.iss.mat.m %>% write.dta('temp.dta')
  
  stata('format date %tm
  gen year=year(date)
  gen monthly=ym(year,month)
  format monthly %tm
  tsset ccy monthly
  * with cluster
  eststo clear
  eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust cluster(ccy)
  eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust cluster(ccy)
  eststo: xi: reg I_both_iss I_both_mat i.ccy,robust cluster(ccy)
  eststo:xtivreg2 D_abs_netmisp  (I_both_iss=I_both_mat),fe robust cluster(ccy)
  esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps
  * w/o cluster
  eststo clear
  eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust 
  eststo: xi: reg I_both_iss I_both_mat i.ccy,robust 
  eststo:xtivreg2 D_abs_netmisp  I_both_mat,fe robust 
  eststo:xtivreg2 D_abs_netmisp  (I_both_iss=I_both_mat),fe robust 
  esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps', data.in=dt.iss.mat.m)
  
  
  
  # require(plm)
  # dt.iss.mat.m %>% plm(D.abs.netmisp ~ I_both.iss | I_both.mat,index=c('ccy'),data=.,model='within') %>% summary
  
  ## regresss chg in abs net dev on Issuance
  stata('* stata code
        format date %tm
        gen year=year(date)
        capture(gen month=month(date))
        gen monthly=ym(year,month)
        format monthly %tm
        eststo clear
        eststo: reg D_abs_netmisp I_both_iss,robust
        eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust
        eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust cluster(ccy)
        *eststo: cluster2 D_abs_netmisp I_both_iss, fcluster(ccy)  tcluster(monthly)
        eststo: xi:cluster2 D_abs_netmisp I_both_iss i.ccy, fcluster(ccy)  tcluster(monthly)
        *eststo: xi:cluster2 D_abs_netmisp I_both_iss i.ccy i.monthly, fcluster(ccy)  tcluster(monthly)
        *tsset ccy monthly
        *eststo: newey D_abs_netmisp I_both_iss,lag(0) force
        *bootstrap "regress  D_abs_netmisp I_both_iss" _b, reps(1000) cluster(ccy)
        esttab
        ', data.in=dt.iss.mat.m)
 ## regresss chg in abs net dev on matured
 stata('* stata code
        format date %tm
        gen year=year(date)
        capture(gen month=month(date))
        gen monthly=ym(year,month)
        format monthly %tm
        eststo clear
        eststo: reg D_abs_netmisp I_both_mat,robust
        eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust
        eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust cluster(ccy)
        *eststo: cluster2 D_abs_netmisp I_both_mat, fcluster(ccy)  tcluster(monthly)
        eststo: xi:cluster2 D_abs_netmisp I_both_mat i.ccy, fcluster(ccy)  tcluster(monthly)
        *eststo: xi:cluster2 D_abs_netmisp I_both_mat i.ccy i.monthly, fcluster(ccy)  tcluster(monthly)
        *tsset ccy monthly
        *eststo: newey D_abs_netmisp I_both_mat,lag(0) force
        *bootstrap "regress  D_abs_netmisp I_both_mat" _b, reps(1000) cluster(ccy)
        esttab
        ', data.in=dt.iss.mat.m)

 
  # Quarterly collapsing each ccy pair
# 	registerDoParallel(6)
# 	dtmat.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
# 	  dtin2 %>% icollapse4.mature(.,iccy,collapse.freq = 'quarter',filter=1)
# 	} %>% rbindlist()
# 	dtmat.collapse.q %>% setkey(date,ccy)
# 	dtmat.collapse.q <- dtmat.collapse.q[date %between% c(ymd('2004-01-01'),ymd('2016-09-01'))]
#   dtmat.collapse.q[,.(date,I_both,ccy)] %>% ggplot(aes(x=date,y=I_both,colour=ccy))+geom_line()
# 
#   dtmat.collapse.q %>% setkey(date,ccy)
#   dtregmat.q<-dtcreditcip.q[dtmat.collapse.q,nomatch=0]
#   
#   dtregmat.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]
#   dtregmat.q[,D4.netmisp:=netmisp-shift(netmisp,n=4,type='lag'),ccy]
#   dtregmat.q[,quarter:=factor(quarter(date))]
#   dtregmat.q %>% felm(netmisp~I_both|ccy+quarter,data=.) %>% stargazer(.,type='text',report = "vct*")
#   
#   reg2newey<-list()
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='eur'] %>% neweymod(.,D.netmisp~I_both)
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='gbp'] %>% neweymod(.,D.netmisp~I_both)
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='jpy'] %>% neweymod(.,D.netmisp~I_both)
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='aud'] %>% neweymod(.,D.netmisp~I_both)
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='chf'] %>% neweymod(.,D.netmisp~I_both)
#   reg2newey[[length(reg2newey)+1]]<-dtregmat.q[ccy=='cad'] %>% neweymod(.,D.netmisp~I_both)
#   stargazer::stargazer(reg2newey,type='text',report = "vct*")
  
  
  
  
  
  
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
	
	
	
	
  
  

		
# leverage ----------------------------------------------------------------

dtlev<-read.dta13(file='bdlevq.dta') %>% as.data.table()
dtlev<-dtlev[,month:=1+(quarter-1)*3][,date:=ymd(str_c(year,'-',month,'-01'))]

# dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]


dtreglev<-dtlev[dtcreditcip.q]
dtreglev[,D.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreglev[,D.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreglev[,D.bdleverage2:=bdleverage2-shift(bdleverage2,n=1,type='lag'),.(ccy)]
dtreglev[,D.levfac2:=levfac2-shift(levfac2,n=1,type='lag'),.(ccy)]
dtreglev[,D.levfac2adj:=levfac2adj-shift(levfac2adj,n=1,type='lag'),.(ccy)]
dtreglev[,D.lnbdleverage2:=log(bdleverage2)-log(shift(bdleverage2,n=1,type='lag')),.(ccy)]
dtreglev[,D.lnbdleverage:=log(bdleverage)-log(shift(bdleverage,n=1,type='lag')),.(ccy)]
# 
# # regression of change on change
#   # reg1<-dtreglev %>% lm(D.credit~D.bdleverage2+ccy,data=.);summary(reg1)## good
#   # # reg1<-dtreglev %>% lm(D.credit~D.bdleverage2,data=.);summary(reg1) ## good
#   # reg1<-dtreglev %>% felm(D.credit~D.bdleverage2|ccy|0|ccy,data=.);summary(reg1)
#   # # reg1<-dtreglev %>% felm(D.credit~D.bdleverage2|0|0|ccy,data=.);summary(reg1)
#   # 
#   # reg1<-dtreglev %>% lm(D.credit~D.levfac2+ccy,data=.);summary(reg1)## good
#   # # can't get clustering at ccy level right
#   # reg1<-dtreglev %>% felm(D.credit~D.levfac2|ccy|0|ccy,data=.);summary(reg1)
#   # 
#   ## use this one
#   reg1chg<-list()
#   reg1chg[[length(reg1chg)+1]]<-dtreglev %>% lm(D.credit~D.levfac2adj+ccy,data=.);#summary(reg1)## good
#   reg1chg[[length(reg1chg)+1]]<-dtreglev %>% lm(D.credit~D.levfac2adj+ccy,data=.);#summary(reg1)## good
#   # still cant' cluster
#   reg1chg[[length(reg1chg)+1]]<-dtreglev %>% felm(D.credit~D.levfac2adj|ccy|0|ccy,data=.);#summary(reg1)
#   reg1chg[[length(reg1chg)+1]]<-dtreglev %>% felm(D.credit~D.levfac2adj|ccy|0|ccy,data=.);#summary(reg1)
#   stargazer::stargazer(reg1chg,type='text',report = "vct*")
# 
#   # for each currency individually 
#   registerDoParallel(6)
#   outlist<-foreach (iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar%{
#   	dticcy<-dtreglev[ccy==iccy] %>% lm(D.credit~D.levfac2adj,data=.)
#   	dtout<-summary(dticcy,robust=T)$coef %>% as.data.table(keep.rownames=T)
#   	dtout[,ccy:=iccy]
#   	dtout
#   }
#   dtreglev1<-rbindlist(outlist,use.names = T)
#   dtreglev1.<-(dtreglev1 %>% melt(id.vars=c('rn','ccy'),measure.vars=c('Estimate','t value')))[order(ccy,rn,variable)] %>% dcast(rn+variable~ccy)
#   dtreglev1.
# 
#   registerDoParallel(6)
#   outlist<-foreach (iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar%{
#   	dticcy<-dtreglev[ccy==iccy] %>% lm(D.cip~D.levfac2adj,data=.)
#   	dtout<-summary(dticcy,robust=T)$coef %>% as.data.table(keep.rownames=T)
#   	dtout[,ccy:=iccy]
#   	dtout
#   }
#   dtreglev1<-rbindlist(outlist,use.names = T)
#   dtreglev1.<-(dtreglev1 %>% melt(id.vars=c('rn','ccy'),measure.vars=c('Estimate','t value')))[order(ccy,rn,variable)] %>% dcast(rn+variable~ccy)
#   dtreglev1.
# 
# # regression of level on level
# 
#   reg2newey<-list()
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='eur'] %>% neweymod(.,abs(credit)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='gbp'] %>% neweymod(.,abs(credit)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='jpy'] %>% neweymod(.,abs(credit)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='aud'] %>% neweymod(.,abs(credit)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='chf'] %>% neweymod(.,abs(credit)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='cad'] %>% neweymod(.,abs(credit)~levfac2adj)
#   stargazer::stargazer(reg2newey,type='text',report = "vct*")
#   
#   
#   reg2newey<-list()
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='eur'] %>% neweymod(.,abs(cip)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='gbp'] %>% neweymod(.,abs(cip)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='jpy'] %>% neweymod(.,abs(cip)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='aud'] %>% neweymod(.,abs(cip)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='chf'] %>% neweymod(.,abs(cip)~levfac2adj)
#   reg2newey[[length(reg2newey)+1]]<-dtreglev[ccy=='cad'] %>% neweymod(.,abs(cip)~levfac2adj)
#   stargazer::stargazer(reg2newey,type='text',report = "vct*")
#   dtreglev[!is.na(levfac2adj) & !is.na(credit),cor(levfac2adj,abs(credit)),ccy]
#   dtreglev[!is.na(levfac2adj) & !is.na(cip),cor(levfac2adj,abs(cip)),ccy]
#   dtreglev[!is.na(levfac2adj) & !is.na(credit),cor(levfac2adj,abs(credit))]
#   dtreglev[!is.na(levfac2adj) & !is.na(cip),cor(levfac2adj,abs(cip))]
#   # level panel; but need to do with panel newey
#   reglev<-list()
#   reglev[[length(reglev)+1]]<-dtreglev %>% felm(abs(credit)~levfac2adj|ccy|0|ccy,data=.)
#   reglev[[length(reglev)+1]]<-dtreglev %>% felm(abs(cip)~levfac2adj|ccy|0|ccy,data=.)
#   stargazer(reglev,type='text',report = "vct*")

#### this is what works::
  dtreglev <- dtreglev[,abscip:=abs(cip)][,abscredit:=abs(credit)]
  # dtreglev %>% write.dta('temp.dta')
  stata_src <- '
  format quarterly %tq
  xi:reg abscip levfac2adj i.ccy,robust
  xi:reg abscredit levfac2adj i.ccy,robust
  tsset ccy quarterly 
  newey abscip levfac2adj,lag(4) force
  newey abscredit levfac2adj,lag(4) force
  '
  stata(stata_src,data.in=dtreglev)
  
  
  
  
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
    tsset ccy monthly 
    newey abscip vix,lag(6) force
    newey abscredit vix,lag(6) force
    '
  sout<-stata(stata_src,data.in=dtregvix)
  sout
  
  
  
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
	

