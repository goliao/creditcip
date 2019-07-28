
figurespath='../../figures/'
source('../util.r')
creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename=str_c('fig_panel_usd_flipped.pdf'),yrstr.='5',wide=T) 


# Without Narrative
Labels=c('CIP deviation (5 yr)','Residualized credit spread differential (EU-US)'); Ltype=c('solid','dashed'); Lcolor=c('red','darkblue')
fig <- creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('basis points')+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=6))+theme(legend.position='bottom')+geom_hline(yintercept = 0,colour='lightgrey')
fig+annotate('text',x=ymd('2006-06-01'),y=-80,label='Lower EUR\ncredit spread',color='darkblue')+annotate('text',x=ymd('2006-06-01'),y=50,label='Higher cost of\nswapping to USD',color='red')

#+annotate('text',x=ymd('2013-06-01'),y=-80,label=str_c('cor=',as.character(round(creditcip.result[[3]][ccy=='eur',corr],2))))#+ theme(text = element_text(size=12))+ theme(axis.title=element_text(face="bold",size=12),axis.text=element_text(size=12,face="bold")) 
ggsave(file=str_c(figurespath,'eurcreditcip_few.pdf'),width=6,height=3.5)


 creditcip.result[[1]][,.(date,ccy,credit,cip)] %>% fwrite('fig1fig7.csv')


# with narrative
# creditcip.result[[2]][ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+
#   geom_rect(aes(xmin = as.Date('2007-08-01'), ymin = -Inf, xmax = as.Date('2008-08-01'), ymax = Inf),fill = "grey89",alpha=.2)+
#   geom_rect(aes(xmin = as.Date('2008-09-01'), ymin = -Inf, xmax = as.Date('2009-08-01'), ymax = Inf),fill = "grey89",alpha=.2)+
#   geom_rect(aes(xmin = as.Date('2011-05-01'), ymin = -Inf, xmax = as.Date('2012-06-01'), ymax = Inf),fill = "grey89",alpha=.2)+
#   geom_rect(aes(xmin = as.Date('2014-09-01'), ymin = -Inf, xmax = as.Date('2016-10-01'), ymax = Inf),fill = "grey89",alpha=.2)+
#   geom_line(aes(linetype=variable,colour=variable))  +xlab('date')+ylab('basis points')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=6))+theme(legend.position='bottom')+
#   annotate('text',size=3.1,x=ymd('2011-11-01'),y=9,label='Bank dollar\nshortage')+
#   annotate('text',size=3.1,x=ymd('2011-11-01'),y=-90,label='CIP\nspills over\n to credit sprd')+
#   annotate('text',size=3.1,x=ymd('2015-09-01'),y=10,label='ECB QE')+
#   annotate('text',size=3.1,x=ymd('2015-09-01'),y=-90,label='credit sprd\nspills over\n to CIP')+
#   annotate('text',size=3.1,x=ymd('2008-02-01'),y=9,label='U.S. credit\ncrunch')+
#   annotate('text',size=3.1,x=ymd('2008-02-01'),y=-90,label='credit sprd\nspills over\n to CIP')+
#   annotate('text',size=3.1,x=ymd('2009-02-15'),y=9,label='Lehman,\nfinancial\ncrisis')+
#   annotate('text',size=3.1,x=ymd('2009-02-15'),y=-90,label='liquidity\ncontraction\n in both\nmarkets')
# ggsave(file=str_c('fig1credit_cip_spillover.pdf'),width=9,height=6)
###################
############ Generate Figure 2
####################
registerDoParallel(1)
dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.)

dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
} %>% rbindlist()
dtiss.collapse.q %>% setkey(date,ccy)
## merging issuance
# construct quarterly using creditcip from begining of the month, merge with quarterly issuance
#dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[1],.(date,ccy)]
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
 dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
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

dtreg.q[,netcimax:=1.96*netmispse+netmisp]
dtreg.q[,netcimin:=-1.96*netmispse+netmisp]

#dtreg.q[,netmisp:=credit-cip]
secaxfactor=2.5
require(latex2exp)

dtreg.q[ccy=='eur' & date<ymd('2017-01-01'),.(date,i_netflow,netmisp,netcimax,netcimin)] %>% ggplot(aes(x=date,y=netmisp)) +geom_bar(aes(x=date,y=i_netflow*secaxfactor,fill='Issuance flow (EMU to US)'),stat="identity")+geom_line(aes(x=date,y=netmisp,color='Corporate basis'),size=1.1)+scale_y_continuous(sec.axis = sec_axis(~./secaxfactor,name='Issuance flow as % of total issuance'))+theme_few()+ylab('Corporate basis (in basis points)')+geom_errorbar(aes(ymin=netcimin,ymax=netcimax),colour='red',alpha=.2)+theme(legend.position='bottom')+labs(x='')+scale_colour_manual(" ", values=c("Corporate basis" = "red", "Issuance flow (EMU to US)" = "darkblue"))+
  scale_fill_manual("",values="darkblue")+
  theme(legend.key=element_blank(),
        legend.title=element_blank(),
        legend.box="horizontal")+
  annotate('text',size=3.1,x=ymd('2006-01-01'),y=-60,label='cheaper to borrow in EUR',color='red')+
  annotate('text',size=3.1,x=ymd('2006-01-01'),y=40,label='cheaper to borrow in USD',color='red')

dtreg.q[ccy=='eur' & date<ymd('2017-01-01'),.(date,i_netflow,netmisp,netcimax,netcimin)] %>% fwrite('fig2data.csv')

ggsave(file=str_c(figurespath,'eurnetdeviss_werrbar.pdf'),width=6,height=4)

####################3
###### Generate comparision of residualized with benchmarks 
####################

aa<-haven::read_dta('../prices_extended.dta') %>% as.data.table
dtpr <- ((aa %>% as.data.table)[!is.na(date)] %>% melt.data.table(id.vars='date'))[!is.na(value)][,value:=as.numeric(value)]

dtind<-dtpr[variable %like% 'Cdif_euus_00$|^Crd_diff_barc_zvs$$']
dtcalc<-ys1m$regcoef[,.(date,variable='rescred',value=eur)]
dtcomp <- rbind(dtind,dtcalc)

Labels=c('Barclays','BAML','Residualized'); Lcolor=c('blue','darkred','black'); Ltype=c('dashed','dotted','solid');
dtcomp[date>=ymd('2004-01-01') & date<=ymd('2016-01-01')] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable))+theme_few()+xlab(element_blank())+ylab('basis points')+scale_x_date(breaks=scales::pretty_breaks(n=7))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)
ggsave('../../figures/crdindcomp.pdf',width=6,height=4)



###################
#credit cip panel graph
###################



plot.panel.creditcip<-function(prw=dtm$prw,rys=ys1m$regresult, filename='',x11.=F,yrstr.='5',wide=F){
  dt.merged<-create.dev.long(prw,rys,yrstr.) 
  dt.merged[,cip:=-cip]
  dtcreditcip.plot<-dt.merged %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) 

#  dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
  dt.corr.ccy<-dt.merged[,.(corr=cor(cip,credit)),ccy]
  dt.merged[,.(corr=cor(cip,credit))]
  
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  require('gridExtra')
  
  Labels=c('CIP deviation (5 yr)','Residualized credit spread differential'); Ltype=c('solid','solid'); Lcolor=c('red','darkblue')
  fig6<-list()
  if (x11.) X11(width=7,height=9)
  fig6[[1]]<-dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('basis points')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='bottom')+annotate('text',x=ymd('2013-01-01'),y=-75,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='eur',corr],2))))#+geom_hline(yintercept=0,colour='lightblue')
  legendcommon<-get_legend(fig6[[1]])
  fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
  fig6[[2]]<-dtcreditcip.plot[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')+annotate('text',x=ymd('2013-01-01'),y=-60,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='gbp',corr],2))))+theme(axis.title.y=element_blank())#+geom_hline(yintercept=0,colour='lightblue')
  fig6[[3]]<-dtcreditcip.plot[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')+annotate('text',x=ymd('2013-01-01'),y=-110,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='jpy',corr],2))))+theme(axis.title.y=element_blank())#+geom_hline(yintercept=0,colour='lightblue')
  fig6[[4]]<-dtcreditcip.plot[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('basis points')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')+annotate('text',x=ymd('2013-01-01'),y=-60,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='aud',corr],2))))#+geom_hline(yintercept=0,colour='lightblue')
  fig6[[5]]<-dtcreditcip.plot[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')+annotate('text',x=ymd('2013-01-01'),y=-100,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='chf',corr],2))))+theme(axis.title.y=element_blank())#+geom_hline(yintercept=0,colour='lightblue')
  fig6[[6]]<-dtcreditcip.plot[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.25) +xlab('')+ylab('')+ scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')+annotate('text',x=ymd('2013-01-01'),y=-60,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='cad',corr],2))))+theme(axis.title.y=element_blank())#+geom_hline(yintercept=0,colour='lightblue')
  if (wide){
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=3,nrow=3,layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,7,7)),heights=c(2.25,2.25,.25))
  } else{
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
  }
  fig6all 
  if (filename!=''){
    if (wide) ggsave(file=filename,fig6all,width=8,height=5) 
    else ggsave(file=filename,fig6all,width=7,height=9)
  }
  list('dt.credit.cip'=dt.merged,'dt.credit.cip.l'=dtcreditcip.plot,'dt.corr'=dt.corr.ccy)  
}
creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult,filename=str_c(figurespath,'PanelCreditCip.pdf'),yrstr.='5',wide=T) 

###################
### net dev panel graph
###################


plot.netdev<-function(yseff.result,fileout=''){
  yseff.result[,cimax:=est+1.96*se][,cimin:=est-1.96*se]
  require('gridExtra')
  fig7<-list()
  lcolor='darkblue';barcolor='lightgrey'
  fig7[[1]]<-yseff.result[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('basis points')+ scale_color_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+scale_linetype_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+ ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7[[2]]<-yseff.result[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('')+ ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7[[3]]<-yseff.result[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('')+ ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7[[4]]<-yseff.result[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('basis points')+ ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7[[5]]<-yseff.result[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('')+ ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7[[6]]<-yseff.result[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour=lcolor) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour=barcolor,alpha=.25,size=1.1) +xlab('')+ylab('')+ ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=5))+theme(legend.position='none')#+geom_hline(yintercept=0,colour='lightblue')
  fig7all<-grid.arrange(fig7[[1]],fig7[[2]],fig7[[3]],fig7[[4]],fig7[[5]],fig7[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25)) 
  if(fileout!='') ggsave(file=fileout,fig7all,width=10,height=6)
}



ys1meff$regresult %>% plot.netdev(fileout=str_c(figurespath,'Netdev.pdf'))



data2export <- merge(ys1meff$regresult[,.(date,ccy,corporate.cip.net.basis.est=est)],creditcip.result$dt.credit.cip[,.(date,ccy,credit.sprd.diff=credit,cip5y=cip)],by=c('date','ccy'),all=T)
data2export %>% fwrite('corpcipbasis.csv')
data2export[ccy=='jpy',.(date,corporate.cip.net.basis.est,credit.minus.cip)] %>% ggplotw()

# credit cip individual ---------------------------------------------------


#' credit deviations

# credit spreads
ccy2plot=c('eur','gbp','jpy','aud','chf','cad') %>% sort()
Labels=toupper(ccy2plot);Lcolors=c('red','blue','orange','black','green','purple'); Ltype=rep(c('solid'),6)
dtcreditcip[ccy %in% ccy2plot] %>% ggplot(data=.,aes(x=date,y=credit))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('basis points')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=5)) + scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+scale_colour_manual(name='',labels=Labels,values=Lcolors,breaks=ccy2plot)+scale_linetype_manual(name='',labels=Labels,values = Ltype,breaks=ccy2plot)+annotate('text',x=ymd(20060101),y=-60,label='Lower non-USD \n credit spread')+annotate('text',x=ymd(20060101),y=33,label='Lower USD \n credit spread')

ggsave(file=str_c(figurespath,'active_credit.pdf'),width=6,height=3.6)

#' CIP

ccy2plot=c('eur','gbp','jpy','aud','chf','cad') %>% sort()
fxbasislookup <- data.table(ccy=c('eur','gbp','jpy','aud','chf','cad'),ticker=c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'))
tmp <- dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)] %>% tidyr::gather(.,'ticker','value',-date) %>% as.data.table()
dt2plotcip <- merge(tmp,fxbasislookup,by='ticker',all.x=T)
dt2plotcip[date==ymd(20160630)]
Labels=toupper(ccy2plot);Lcolors=c('red','blue','orange','black','green','purple'); Ltype=rep(c('solid'),6)

dt2plotcip %>% ggplot(data=.,aes(x=date,y=-value))+geom_line(aes(linetype=ccy,colour=ccy))+xlab('')+ylab('basis point')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=5)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_manual(name='',labels=Labels,values=Lcolors,breaks=ccy2plot)+scale_linetype_manual(name='',labels=Labels,values=Ltype,breaks=ccy2plot)+annotate('text',x=ymd(20060101),y=60,label='Expensive to\n swap to USD')+annotate('text',x=ymd(20060101),y=-33,label='Expensive to\nswap from USD')
ggsave(file=str_c(figurespath,'active_cip.pdf'),width=6,height=3.6)


#' scatter
library(latex2exp)
aa<-creditcip.result$dt.credit.cip[,.(ccy,cip=cip,credit)]#[credit>-130]
aa.reg<-lm(cip~credit,data=aa)$coefficient;aa.reg
aa[,ccy:=toupper(ccy)]
ggplot(aa,aes(credit,cip,colour=ccy))+geom_point(aes(colour=ccy)) +scale_color_discrete(guide = guide_legend(title = ""))+ geom_abline(intercept=aa.reg[[1]],slope=aa.reg[[2]])+xlab('Residualized Credit Spread Differential in basis points')+ylab('CIP deviation (5yr) in basis points')+geom_hline(yintercept=0,colour='grey')+geom_vline(xintercept=0,colour='grey')+scale_x_continuous(breaks=scales::pretty_breaks(n=13))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+theme_few()+annotate('text',x=30,y=60,label=str_c('cor=',as.character(round(aa[,cor(credit,cip)],3))))+annotate('text',x=0,y=80,label='Costly to \nswap to USD')+annotate('text',x=-85,y=0,label='Lower local \n credit spread')
ggsave(file=str_c(figurespath,'active_creditcipscatter.pdf'),width=6,height=3.6)

