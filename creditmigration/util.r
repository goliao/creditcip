# install.packages(c('stringr','tidyr','dplyr','lubridate','ggplot2','sandwich','magrittr','beepr','ggthemes','data.table'))
# install.packages(c('lfe','RStata','stargazer','ggthemes','sandwich','lmtest','readstata13','lubridate','lmtest','vars'))
require(lfe)
  require(RStata)
  if (Sys.info()['sysname'][[1]]=='Windows'){
    print('windows stata')
    options("RStata.StataVersion" = 13)
    options("RStata.StataPath"='"\"C:\\Program Files (x86)\\Stata13\\StataMP-64\""')  
  } else{
    options("RStata.StataVersion" = 14)
    options("RStata.StataPath"='/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')  
  }
  
require(foreign)

# require(xts)
# require(tidyr)
require(dplyr)
require('readstata13')
# require('haven')
# require('ggfortify')
# require('doBy')
require(lubridate)
require(ggplot2)
require(stargazer)
# require(reshape2)
# require(sqldf)
require(magrittr)
# require(xda)
require(beepr)
require(ggthemes)
require(lmtest);
require(data.table)
require(stringr)
require(doParallel)
#options(width=as.integer(Sys.getenv("COLUMNS")))
require(sandwich);require(lmtest)

packages.do.export<-c('data.table','dplyr','stringr','lfe','lubridate','beepr','lmtest','sandwich')

'%ni%' <- Negate('%in%')
'%nlk%' <- Negate('%like%')
'%lk%' <- '%like%'
#source('dbutil.r')
dcast<-data.table::dcast
tocusip6<-function(dtin, field='upcusip'){
  dtin[,cusip6:=str_sub(eval(exparse(field)),1,6)]  
  dtin[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
}

wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(Sys.getenv("COLUMNS")))
}

df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")){
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}
toc <- function(){
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  if (!isTRUE(getOption('knitr.in.progress'))) print(toc - tic)
  # beep()
  invisible(toc)
}

  plot.event.study<-function(dtin, event.dates,type.=1){
    if (type.==1)
      dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
    else if(type.==2) # plot cipest as well
      dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','cipest','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.1) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','cipest','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','cipest','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
    else if(type.==3) # use ribbon instead
      dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates)) +geom_ribbon(aes(ymin=cimin,ymax=cimax),alpha=.3,colour=NA) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
  }


 plot.irf<-function(resirf,v1='credit',v2='cip',v3='i_netflow',filename=''){
    require(latex2exp)
    labelv1<-'$c$'
    labelv2<-'$b$'
    labelv3<-'$\\mu$'
    dttemp<-list()
    dttemp[[1]]<-(resirf$irf %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="irf"]
    dttemp[[2]]<-(resirf$Upper %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Upper"]
    dttemp[[3]]<-(resirf$Lower %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Lower"]
    dtirf<-(dttemp %>% rbindlist %>% melt(id.vars=c('rn','type')) %>% dcast(rn+variable~type))[,rn:=as.numeric(rn)][order(rn)]  
    par(ask=F)  
    require('gridExtra')
    figirf<-list()
    figirf[[1]]<-dtirf[variable==str_c(v1,'.',v1)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv1)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf[[2]]<-dtirf[variable==str_c(v1,'.',v2)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv2)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf[[3]]<-dtirf[variable==str_c(v1,'.',v3)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv3)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf[[4]]<-dtirf[variable==str_c(v2,'.',v1)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv1)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf[[5]]<-dtirf[variable==str_c(v2,'.',v2)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv2)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf[[6]]<-dtirf[variable==str_c(v2,'.',v3)] %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv3)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    figirf.all<-grid.arrange(figirf[[1]],figirf[[2]],figirf[[3]],figirf[[4]],figirf[[5]],figirf[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
    #if(filename!='') ggsave(file=filename,figirf.all,width=8,height=5)
    figirf.all
  } 
plot.irf.panel<-function(fname=str_c(params$figfile,'panelvar.dta'),v1='credit',v2='cip',v3='i_netflow',filename=''){
  require(latex2exp)
  labelv1<-'$c$'
  labelv2<-'$b$'
  labelv3<-'$\\mu$'
  pvar<-read.dta13(fname) %>% as.data.table()
  pvarl<-(pvar %>% melt.data.table(id.vars='step'))[variable %nlk% 'FEVD']
  pvarl[variable %like% '^p',prefix:='p']
  pvarl[variable %like% '^n',prefix:='n']
  pvarl[variable %like% 'll',postfix:='ll']
  pvarl[variable %like% 'ul',postfix:='ul']
  pvarl[,series:=as.numeric(str_extract_all(variable,'\\d+'))]
  pvarl[is.na(prefix),prefix:='est']
  pvarl[is.na(postfix),postfix:='est']
  pvar2<-(pvarl[prefix %in% c('est','n')] %>% dcast(step+series~postfix))[order(series,step)]
  par(ask=F)  
  require('gridExtra')
  figirf<-list()
  figirf <- list()
  # figirf[[length(figirf)+1]]<-pvar2[series==1] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv3)))
  # figirf[[length(figirf)+1]]<-pvar2[series==2] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv1)))
  # figirf[[length(figirf)+1]]<-pvar2[series==3] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv2)))
  figirf[[length(figirf)+1]]<-pvar2[series==5] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv1)))
  figirf[[length(figirf)+1]]<-pvar2[series==6] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv2)))
  figirf[[length(figirf)+1]]<-pvar2[series==4] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv3)))
  figirf[[length(figirf)+1]]<-pvar2[series==8] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv1)))
  figirf[[length(figirf)+1]]<-pvar2[series==9] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv2)))
  figirf[[length(figirf)+1]]<-pvar2[series==7] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv2,' $\\rightarrow$ ',labelv3)))
  figirf.all<-grid.arrange(figirf[[1]],figirf[[2]],figirf[[3]],figirf[[4]],figirf[[5]],figirf[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
  if(filename!='') ggsave(file=filename,figirf.all,width=8,height=5)
  figirf.all
} 

  plot.irf.single<-function(resirf,v1='netmsip',v2='i_netflow'){
    require(latex2exp)
    labelv1<-'$(c-b)$'
    labelv2<-'$\\mu$'
    dttemp<-list()
    dttemp[[1]]<-(resirf$irf %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="irf"]
    dttemp[[2]]<-(resirf$Upper %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Upper"]
    dttemp[[3]]<-(resirf$Lower %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Lower"]
    dtirf<-(dttemp %>% rbindlist %>% melt(id.vars=c('rn','type')) %>% dcast(rn+variable~type))[,rn:=as.numeric(rn)][order(rn)]  
    par(ask=F)      
    outplot<-dtirf %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv2)))+xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
    outplot
  } 
  
 

plot.event.study.moiss2<-function(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-01-01','2016-08-01'),event.dates.in=ecbqe,type.in=1,filepathhead='',datetics=10){
    figa <- dtin.in[ccy==ccy.][date %between% date.range] %>% plot.event.study(.,event.dates=event.dates.in,type.=type.in)+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
    figb <- dtin.in[ccy==ccy.][date %between% date.range][,.(date,netmispricing=credit-cip)] %>% ggplot(aes(x=date,y=netmispricing,colour='green'))+geom_line()+theme_few()+theme(legend.position='bottom')+xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_color_discrete('',labels='Net Deviation (Credit-CIP)')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
    figc <- dtiss.in[date %between% date.range,.(date,eval(exparse(str_c('I_net_USD',toupper(ccy.)))))] %>% melt(id.vars='date') %>% ggplot(aes(x=date,y=value,fill=variable))+geom_bar(stat='identity',position='identity')+xlab('')+ylab('$billions')+geom_hline(yintercept=0,colour='lightblue')+geom_vline(xintercept = as.numeric(event.dates.in))+scale_fill_discrete('',labels='Monthly Net Issuance Flow (EU to US)')+theme_few()+theme(legend.position='bottom')+scale_x_date(breaks=scales::pretty_breaks(n=datetics))
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
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
plot.netdev<-function(yseff.result,fileout=''){
  yseff.result[,cimax:=est+1.96*se][,cimin:=est-1.96*se]
  require('gridExtra')
  fig7<-list()
  fig7[[1]]<-yseff.result[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+scale_linetype_discrete('',labels = c('Net deviation (credit diff.- CIP)'))+ ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7[[2]]<-yseff.result[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7[[3]]<-yseff.result[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7[[4]]<-yseff.result[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7[[5]]<-yseff.result[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7[[6]]<-yseff.result[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=est))+geom_line(colour='blue') +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig7all<-grid.arrange(fig7[[1]],fig7[[2]],fig7[[3]],fig7[[4]],fig7[[5]],fig7[[6]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25)) 
  if(fileout!='') ggsave(file=fileout,fig7all,width=10.5,height=6.5)
}


filter.sdc<-function(sdc,type.=1){
  # filter the sdc database based on criteria 10/05/2016
  flds.keep<-c("amt","btypc","c","ccy","country_of_incorp","cu","cusip9","d","deal_no","descr","foreign","glaw","i","isin","issue_type_desc","mat","mat2","mdealtype","mkt","modnat","modupnat","monthly","nat","natccy","nrating","pub","rule144a","secur","settlement2","sicp","supranational","tf_macro_desc","tf_mid_desc","tic","uop","upcusip","upforeign","upnames","upnatccy","upsicp","upsupranational","ytofm")
  sdc.0<-sdc[,c(flds.keep),with=F] %>% as.data.frame %>% as.data.table %>%  copy()
  if(type. %like% '6ccy'){
    print('6 ccy only')
    sdc.0<-sdc.0[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
  } 
  if (type. %like% 'v1'){
    print('v1')
    # the version that matches stata filter exactly to generate sdc96_clean
    dtiss.stata.match<-sdc.0[amt>=50][ytofm>=2][ytofm<99][nrating<=16][nrating!=0][nrating!=1][pub %in% c('Public','Sub.')][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][tf_mid_desc!="Government Sponsored Enterprises"][secur %ni% stringr::str_to_lower(c("Cum Red Pfd Shs", "Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk"))]
    sdc.1<-dtiss.stata.match
    # aa<-read.dta13('sdc96_clean3.dta') %>% as.data.table();
    #compare.dt(aa,dtiss.stata.match[d>=ymd('1996-01-01') & d<ymd('2016-07-01')],'deal_no') 
  } else if(type. %like% 'v2'){
    print('v2')
    # better version? NO, worse
    sdc.1<-sdc.0[amt>=50][ytofm>=2][ytofm<99][nrating<=16][nrating!=0][nrating!=1][pub %in% c('Public','Sub.')][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][tf_mid_desc!="Government Sponsored Enterprises"][secur %nlk% 'fl|mtg|pdf|sh|zero|fr|index|mortg|eqt|pass|islamic|step|pfanbriefe|cont|var|perp|loan|extendible|pik']
    sdc.1[,acq:=ifelse(str_to_lower(uop) %like% 'acq',1,0)]
    sdc.1[str_to_lower(i) %like% 'acqs|acquisition',acq:=1]
    sdc.1<-sdc.1[acq==0]
  } else if(type. %like% 'v3'){
    print('v3')
    # more generous, include floating, gov sectors, etc
    sdc.1<-sdc.0[amt>=50][ytofm>=1][ytofm<99][nrating<=16][nrating!=0][nrating!=0][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][str_to_lower(tf_mid_desc) %nlk% "govern"][secur %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik']
  }
  sdc.1
}
issfilter<-function(dtin,type=1,desc=F){
  dtout<-copy(dtin)
  if (desc==T){
    print(str_c('dtin N: ',dtin[,.N]))
    criteria=c('amt >= 50','ytofm >= 1','ytofm <= 99999',
        'mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP")',
        'secur %ni% c("Cum Red Pfd Shs","Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk")',
        "!grepl('Government Sponsored Enterprises',tf_mid_desc)",
        "!grepl('Government Sponsored Enterprises',tf_mid_desc)",
        "!grepl('Flt', secur)",
        "!grepl('Zero Cpn', secur)",
        "!grepl('Float', secur)",
        "!grepl('Fl', descr)",
        "!grepl('Zero Cpn', descr)",
        "!grepl('Mortgage-backed', issue_type_desc)",
        "!grepl('Asset-backed', issue_type_desc)",
        "!grepl('Federal Credit Agency', issue_type_desc)",
        "!grepl('Loan', descr)",
        "!grepl('Mtg',typesec)",
        "!grepl('FRN',typesec)",
        "!grepl('Mortgage',secur)",
        "!grepl('Mtg',secur)",
        "issue_type_desc!='Agency, Supranational, Sovereign'",
        "amt>=100",
        "grepl('Fxd/Straight')"
      )
    dt.criteria<-data.table('criteria'=criteria)
    counthelper<-function(dtin,dtcriteria,bylist) { 
      sat=dtin[eval(exparse(bylist[[1]])),.N]
      unsat=dtin[eval(exparse(str_c('!',bylist[[1]]))),.N]
      empty=dtin[,.N]-sat-unsat
      # bylist=list('amt >= 50')
      # marginal.drop=dtin[eval(exparse(paste(as.character(dtcriteria$criteria),collapse=' | '))),.N] - dtin[eval(exparse(paste(as.character(eval(exparse(str_c('dtcriteria[criteria %nlk% "',as.character(bylist[[1]]),'"]')))),collapse=' | '))),.N]
      # browser()
      data.table('sat'=sat,'unsat'=unsat,'empty'=empty)
    }
    dt.filter.summary<-dt.criteria[,counthelper(dtin,dt.criteria,.BY),criteria]
    print(dt.filter.summary)
  }
  dtout %<>% filter(
     amt >= 50 | is.na(amt),
     ytofm >= 1,
     ytofm <= 99999,
     mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
     secur %ni% c("Cum Red Pfd Shs","Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk"),
     !grepl('Government Sponsored Enterprises',tf_mid_desc),!grepl('Flt', secur),!grepl('Zero Cpn', secur),!grepl('Float', secur),!grepl('Fl', descr),
     !grepl('Zero Cpn', descr),!grepl('Mortgage-backed', issue_type_desc),!grepl('Asset-backed', issue_type_desc),!grepl('Federal Credit Agency', issue_type_desc),!grepl('Loan', descr)
   ) 
  if (type==2){
    dtout %<>% filter(amt>=100)
  } else if (type==3){
    dtout  %<>%  filter(!grepl('Mtg',typesec),
          !grepl('FRN',typesec), issue_type_desc!='Agency, Supranational, Sovereign',
          !grepl('Mortgage',secur),!grepl('Mtg',secur)
        )
  }  else if (type==4){ # testing:
    dtout  %<>%  filter(
          !grepl('Mtg',typesec),
          !grepl('FRN',typesec),
          !grepl('Mortgage',secur),!grepl('Mtg',secur), !grepl('Index',secur)
      )
  } else if (type==5){ # testing:
    dtout  %<>%  filter(
          !grepl('Mtg',typesec),
          !grepl('FRN',typesec),
          !grepl('Mortgage',secur),!grepl('Mtg',secur), !grepl('Index',secur)
      )  %>% as.data.table()
    dtout<-dtout[!(pub=='Govt.' & tf_mid_desc=='National Government')][!(pub=='Govt.' & tf_mid_desc=='Regional Government')] 
    # dtout[pub=='Govt.',upcusip:=cu]
  } else if (type==6){
    dtout  %<>%  filter(
      !grepl('Mtg',typesec),
      !grepl('FRN',typesec),
      !grepl('Mortgage',secur),!grepl('Mtg',secur), !grepl('Index',secur)
    )  %>% as.data.table()
    dtout<-dtout[!(pub=='Govt.' & tf_mid_desc=='National Government')][!(pub=='Govt.' & tf_mid_desc=='Regional Government')]
    dtout<-dtout[pub %in% c('Public','Sub.')][nrating>1][ytofm<99][nrating<16]
  }
  dtout<-dtout %>% as.data.table()
  # print(str_c('total out:'))  dtout[,.N]
  dtout
}
  estimate.crddiff.firm<-function(dtregdata.,winsor=.025,parallel.core=1,regversion=1){
    # estimates credit differential at the firm level
    # winsor=.025
    # parallel.core=1
    tic()
    dtregdata<-copy(dtregdata.)
    dtregdata[ccy=='usd',ccy:='1usd']
    dtregdata[,pctl:=percent_rank(swapsprd),by=.(date,ccy)]
    dtregdata2<-dtregdata[pctl>=winsor & pctl<=(1-winsor)]
    dtregdata3<-dtregdata2 %>% filterglobaluponly()
      require(doParallel)
      setkey(dtregdata3,'date')
      indx<-split(seq(nrow(dtregdata3)),dtregdata3$date)
          registerDoParallel(parallel.core)
          out_list <- foreach(i = indx, .packages = packages.do.export ) %dopar% {
          # i=indx[1][[1]]
            tryCatch({
                Psubset<-dtregdata3[i,]
                if (regversion==1){
                  reg1<-Psubset%>% felm(swapsprdadj~ccy+upcusip+ccy*upcusip,data=.)
                  reg2<-Psubset%>% felm(swapsprd~ccy+upcusip+ccy*upcusip,data=.)
                } else if (regversion==2){
                  reg1<-Psubset%>% felm(swapsprdadj~ccy+upcusip+ccy*upcusip|ytm_bucket,data=.)
                  reg2<-Psubset%>% felm(swapsprd~ccy+upcusip+ccy*upcusip|ytm_bucket,data=.)
                } else if (regversion==3){
                  reg1<-Psubset%>% felm(swapsprdadj~ccy+upcusip+ccy*upcusip|ytm_bucket+liq_bucket,data=.)
                  reg2<-Psubset%>% felm(swapsprd~ccy+upcusip+ccy*upcusip|ytm_bucket+liq_bucket,data=.)
                } else if (regversion==4){
                  reg1<-Psubset%>% felm(swapsprdadj~ccy+upcusip+ccy*upcusip|ytm_bucket+liq_bucket+rating_bucket,data=.)
                  reg2<-Psubset%>% felm(swapsprd~ccy+upcusip+ccy*upcusip|ytm_bucket+liq_bucket+rating_bucket,data=.)
                }
                reg1coef<-summary(reg1)$coefficients %>% as.data.table(keep.rownames=T)
                upcrd<-list()
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccyeur',Estimate]),reg1coef[rn %like% '^ccyeur:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccyeur',Estimate])])[,ccy:='eur']
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccyaud',Estimate]),reg1coef[rn %like% '^ccyaud:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccyaud',Estimate])])[,ccy:='aud']
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccygbp',Estimate]),reg1coef[rn %like% '^ccygbp:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccygbp',Estimate])])[,ccy:='gbp']
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccyjpy',Estimate]),reg1coef[rn %like% '^ccyjpy:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccyjpy',Estimate])])[,ccy:='jpy']
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccycad',Estimate]),reg1coef[rn %like% '^ccycad:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccycad',Estimate])])[,ccy:='cad']
                upcrd[[length(upcrd)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg1coef[rn=='ccychf',Estimate]),reg1coef[rn %like% '^ccychf:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccychf',Estimate])])[,ccy:='chf']
                upcrd.single.date<-rbindlist(upcrd)
                upcrd.single.date[,date:=Psubset[1,.(date)]]
                # upcrd.single.date

                reg2coef<-summary(reg2)$coefficients %>% as.data.table(keep.rownames=T)
                upcrd2<-list()
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccyeur',Estimate]),reg2coef[rn %like% '^ccyeur:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccyeur',Estimate])])[,ccy:='eur']
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccyaud',Estimate]),reg2coef[rn %like% '^ccyaud:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccyaud',Estimate])])[,ccy:='aud']
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccygbp',Estimate]),reg2coef[rn %like% '^ccygbp:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccygbp',Estimate])])[,ccy:='gbp']
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccyjpy',Estimate]),reg2coef[rn %like% '^ccyjpy:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccyjpy',Estimate])])[,ccy:='jpy']
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccycad',Estimate]),reg2coef[rn %like% '^ccycad:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccycad',Estimate])])[,ccy:='cad']
                upcrd2[[length(upcrd2)+1]]<-rbind(data.table('upcusip'=Psubset[order(upcusip)][1,upcusip],'est'=reg2coef[rn=='ccychf',Estimate]),reg2coef[rn %like% '^ccychf:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg2coef[rn=='ccychf',Estimate])])[,ccy:='chf']
                upcrd.single.date2<-rbindlist(upcrd2)
                upcrd.single.date2[,date:=Psubset[1,.(date)]]
                print(Psubset[1,.(date)])
                # upcrd2.single.date
                upcrd.single.date[,ytype:='netdiff']
                upcrd.single.date2[,ytype:='crddiff']
                rbind(upcrd.single.date,upcrd.single.date2)
              }, error=function(e){print(data.table('error'=e))} 
            )
          }
          regresult <- rbindlist(out_list,use.names=TRUE,fill=TRUE) #, fill=TRUE, etc.
          regresult %>% setkey(date,upcusip)
          toc()
      regresult 
  }

plot.panel.creditcip<-function(prw=dtm$prw,rys=ys1m$regresult, filename='',x11.=F,yrstr.='5',wide=F){
  dt.merged<-create.dev.long(prw,rys,yrstr.) 
  dtcreditcip.plot<-dt.merged %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit'))   
  dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
  dt.corr.ccy<-dt.merged[,.(corr=cor(cip,credit)),ccy]
  dt.merged[,.(corr=cor(cip,credit))]

  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  require('gridExtra')
  fig6<-list()
  if (x11.) X11(width=7,height=9)
  fig6[[1]]<-dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('basis points')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='eur',corr],2))))
  legendcommon<-get_legend(fig6[[1]])
  fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
  fig6[[2]]<-dtcreditcip.plot[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='gbp',corr],2))))+theme(axis.title.y=element_blank())
  fig6[[3]]<-dtcreditcip.plot[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='jpy',corr],2))))+theme(axis.title.y=element_blank())
  fig6[[4]]<-dtcreditcip.plot[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('basis points')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='aud',corr],2))))
  fig6[[5]]<-dtcreditcip.plot[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='chf',corr],2))))+theme(axis.title.y=element_blank())
  fig6[[6]]<-dtcreditcip.plot[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='cad',corr],2))))+theme(axis.title.y=element_blank())
  if (wide){
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=3,nrow=3,layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,7,7)),heights=c(2.25,2.25,.25))
  } else{
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
  }
  fig6all 
  if (filename!=''){
    if (wide) ggsave(file=filename,fig6all,width=10.5,height=6.5) 
    else ggsave(file=filename,fig6all,width=7,height=9)
  }
  list('dt.credit.cip'=dt.merged,'dt.credit.cip.l'=dtcreditcip.plot,'dt.corr'=dt.corr.ccy)  
}
plot.panel.creditcip.any.ccy<-function(ys,yseff, filename='',wide=F){
  # ys<-ys.eur;yseff<-ys.eur.eff;
  credit.cip.exact.ccy<-(ys$regresult[yseff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff]
  invisible(credit.cip.exact.ccy[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se])
  
  dtcreditcip.plot<-credit.cip.exact.ccy %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit'))   
  
  #dtcreditcip.plot[ccy=='usd'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
  
  dt.corr.ccy<-credit.cip.exact.ccy[,.(corr=cor(cip,credit)),ccy]
  print(str_c('corr:',credit.cip.exact.ccy[,.(corr=cor(cip,credit))]))
  
  require('gridExtra')
  fig<-list()
  for (iccy in credit.cip.exact.ccy[,.N,ccy]$ccy){
    fig[[length(fig)+1]]<-dtcreditcip.plot[ccy==iccy] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+ggtitle(str_to_upper(iccy))+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy==iccy,corr],2))))+theme(axis.title.y=element_blank())  
  }
  
  legendcommon<-get_legend(fig[[1]]+theme(legend.position='bottom'))
  if (length(fig)==5) fig[[6]]<-fig[[5]]
  if (wide){
    figall<-grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]],fig[[5]],fig[[6]],legendcommon,ncol=3,nrow=3,layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,7,7)),heights=c(2.25,2.25,.25))
  } else{
    figall<-grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]],fig[[5]],fig[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
  }
  #figall 
  if (filename!=''){
    if (wide) ggsave(file=filename,figall,width=10.5,height=6.5) 
    else ggsave(file=filename,figall,width=7,height=9)
  }
  list('dt.credit.cip'=credit.cip.exact.ccy,'dt.credit.cip.l'=dtcreditcip.plot,'dt.corr'=dt.corr.ccy)  
}


plot.panel.creditrating<-function(dtin, filename='',wide=F){

  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  require('gridExtra')
  fig6<-list()
  fig6[[1]]<-dtin[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('basis points')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('EUR')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')
  legendcommon<-get_legend(fig6[[1]])
  fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
  fig6[[2]]<-dtin[ccy=='gbp'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('GBP')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.title.y=element_blank())
  fig6[[3]]<-dtin[ccy=='jpy'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('JPY')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.title.y=element_blank())
  fig6[[4]]<-dtin[ccy=='aud'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('basis points')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('AUD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')
  fig6[[5]]<-dtin[ccy=='chf'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('CHF')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.title.y=element_blank())
  fig6[[6]]<-dtin[ccy=='cad'] %>% ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable)) +xlab('')+ylab('')+geom_hline(yintercept=0,colour='darkgreen')+ scale_color_discrete('',labels = c('High Grade','Low Grade'))+scale_linetype_discrete('',labels = c('High Grade','Low Grade'))+ggtitle('CAD')+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='none')+theme(axis.title.y=element_blank())
  if (wide){
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=3,nrow=3,layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,7,7)),heights=c(2.25,2.25,.25))
  } else{
    fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
  }
  
  if (filename!=''){
    if (wide) ggsave(fig6all,file=filename,fig6all,width=10.5,height=6.5) 
    else ggsave(fig6all,file=filename,fig6all,width=7,height=9)
  }
  
}

  
add.earlist.iss.in.ccy <- function(dtin,bondrefall.=FALSE){
  # this function finds the earliest issuance date for each upcusip in each currency and adds it to dtin, it relies on the bigger bondrefall to get the complete set of bonds issued by each upcusip
    if(!bondrefall.){
      refdt<-dtin
    } else{
      refdt<-rbind(bondrefall.,dtin,fill=T)
    }
    allccys <- c('aud','cad','chf','eur','gbp','jpy','usd')
    dtearlistforeign<-refdt[order(d)][,first(d),.(upcusip,ccy)] %>% dcast(upcusip~ccy,value.var='V1') #%>% melt(id.vars='upcusip',variable.name='ccy',value.name='earliest.iss.in.ccy')
    dtearlistforeign %>% setnames(allccys,str_c('earlist.',allccys))
    dtearlistforeign %>% setkey(upcusip)

    # upcusip.commonccy<-dtin[,.(upcusip,upnatccy)] %>% setkey(upcusip)
    # upcusip.commonccy<-dtin[,.N,.(ccy,upcusip)][order(upcusip,-N)][,.(mainccy=.SD[1,ccy]),upcusip]
    # dtin %>% ds('ccy')

    # dtearlistforeign<-upcusip.commonccy[dtearlistforeign]
    
    # min.row<-function(x){
    #   upnatccy<-x[1,upnatccy]
    #   print(x[1,str_c('earlist.',allccys[allccys %nlk% upnatccy]),with=F] %>% as.numeric %>% na.omit %>% min %>% as.Date())
    #   browser()
    # }

    # dtearlistforeign[,earliest.foreign:=min.row(.SD),upcusip]

    dtin %>% setkey(upcusip)
    dtin<-dtearlistforeign[dtin]
    dtin  
  }



create.dev.long<-function(prwin=prw,dtregresult,yrstr='5'){
  # exclude net deviation
  cip<-copy(prwin[,.(date,eval(exparse(str_c('eubs',yrstr))),eval(exparse(str_c('bpbs',yrstr))),eval(exparse(str_c('jybs',yrstr))),eval(exparse(str_c('adbs',yrstr))),eval(exparse(str_c('sfbs',yrstr))),eval(exparse(str_c('cdbs',yrstr))))])
  cip %>% setnames(str_c(c('eubs','bpbs','jybs','adbs','sfbs','cdbs'),yrstr),c('eur','gbp','jpy','aud','chf','cad'))
  cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
  cipl %>% setkey(date,ccy)

  dtregresult %>% setkey(date,ccy)
  dt.mispl<-cipl[dtregresult]
  dt.mispl %>% setnames('est','credit')
  dt.mispl[,cimax:=credit+1.96*se][,cimin:=credit-1.96*se]
  dt.mispl
}
create.dev.long2<-function(prwin=prw,creditmispin,netmispin,yrstr='5'){
  # include net dev
    cip<-copy(prwin[,.(date,eval(exparse(str_c('eubs',yrstr))),eval(exparse(str_c('bpbs',yrstr))),eval(exparse(str_c('jybs',yrstr))),eval(exparse(str_c('adbs',yrstr))),eval(exparse(str_c('sfbs',yrstr))),eval(exparse(str_c('cdbs',yrstr))))])
  creditmisp=copy(creditmispin)
  netmisp=copy(netmispin)
  cip %>% setnames(str_c(c('eubs','bpbs','jybs','adbs','sfbs','cdbs'),yrstr),c('eur','gbp','jpy','aud','chf','cad'))
  cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
  cipl %>% setkey(date,ccy)
  creditmisp %>% setkey(date,ccy)
  netmisp %>% setkey(date,ccy)
  creditmisp %>% setnames(c('est','se'),c('credit','creditse'))
  netmisp %>% setnames(c('est','se'),c('netmisp','netmispse'))
  dt.mispl<-netmisp[creditmisp]
  dt.mispl %>% setkey(date,ccy)
  dt.mispl<-cipl[dt.mispl]
  dt.mispl %>% setkey(date,ccy)
  dt.mispl
}
preprocess<-function(bondref,dtl,prl,monthlyonly=TRUE,issfiltertype=2,ccyfilter=c('usd','eur','jpy','gbp','aud','cad','chf')){
   tic()
  dtl<-copy(dtl)
  prl<-copy(prl)
  prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
  br<-bondref[!is.na(pk)] %>% issfilter(.,type=issfiltertype)
  br[,ccy:=tolower(ccy)]
  br <- br[ccy %in% ccyfilter]
  br <- br %>% semi_join(dtl,by='pk') %>% as.data.table()
  setkey(br,pk)
  if (nrow(showdups(br,'pk'))!=0){
    print('error: duplicate pks; dedupe based on amt')
    br<-br[order(pk,-amt)]
    setkey(br,pk)
    br<-unique(br)
  }
  # MERGE RATING AND ADD MATURITY BUCKETS -----------------------------------
  setkey(dtl,pk);setkey(br,pk)
  if ('monthend' %in% ds(dtl)){
    if (monthlyonly) {
      dtl<-dtl[monthend==1]
      prl<-prl[monthend==1]
    } else{
      print('daily data')
      pk_daily0<-unique(dtl[monthend==0,.(pk)])
      # alternatively, count pk as daily obs if there are more than three times as many daily obs as monthly obs
      pkcount<-dtl[,.N,.(pk,monthend)] %>% dcast.data.table(pk~monthend)
      pk_daily<-pkcount[`0`>3*`1`,.(pk)]
      setkey(pk_daily,pk)
      dtl<-dtl[pk_daily]
    }
  }

  br[DEBT_SENR_CD=='SU',senior:='SU']
  br[DEBT_SENR_CD=='SS',senior:='SS']
  br[DEBT_SENR_CD %in% c('SB','SR') ,senior:='SB']
  br[is.na(senior),senior:='UN']

  br[is.na(amt) & !is.na(FACE_US_AMNT),amt:=FACE_US_AMNT]
  br[,amt_bucket:=as.numeric(NA)]
  br[amt>=100,amt_bucket:=ntile(amt,4)]
  br[amt<100,amt_bucket:=0]
  br[is.na(amt),amt_bucket:=-1]
  
  dtl2<-dtl[br[,.(ccy,mat2,nrating,upcusip,cu,pk,ytofm,issue_type_desc,pub,tf_mid_desc,sic1,amt,amt_bucket,DEBT_CLASS_CD,MKT_TYP_CD,senior)],nomatch=0]
  dtl2[,ytm:=as.numeric((mat2-date)/365)]
  dtl3<-dtl2[ytm >.05]
  dtl3[is.na(nrating),nrating:=0]
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='YLD_YTM_MID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
  prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
  prw<-prl %>% distinct() %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value')
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.05,1.0)]
  dtl3[liq<.25,liq_bucket:=0] # more illiq
  dtl3[liq>=.25 & liq<.5,liq_bucket:=1] # more illiq
  dtl3[liq>=.5 & liq<.75,liq_bucket:=2] # liq
  dtl3[liq>=.75 & liq<=1,liq_bucket:=3] # liq


  prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
  prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
  prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
  prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
  prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
  prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz5=cdsw5+cdbs5,cdsz7=cdsw7+cdbs7,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz30=cdsw30+cdbs30)]
  prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz5=sfsw5+sfbs5,sfsz7=sfsw7+sfbs7,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz30=sfsw30+sfbs30)]
  # prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz3=cdsw3+cdbs3,cdsz4=cdsw4+cdbs4,cdsz5=cdsw5+cdbs5,cdsz6=cdsw6+cdbs6,cdsz7=cdsw7+cdbs7,cdsz8=cdsw8+cdbs8,cdsz9=cdsw9+cdbs9,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz25=cdsw25+cdbs25,cdsz30=cdsw30+cdbs30)]
  # prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz3=sfsw3+sfbs3,sfsz4=sfsw4+sfbs4,sfsz5=sfsw5+sfbs5,sfsz6=sfsw6+sfbs6,sfsz7=sfsw7+sfbs7,sfsz8=sfsw8+sfbs8,sfsz9=sfsw9+sfbs9,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz25=sfsw25+sfbs25,sfsz30=sfsw30+sfbs30)]
  # prw[,approx_eubs1:=(eur/(eur+eur12m/10000)*(1+ussw1/10000)-(1+eusw1/10000))*10000]
  # prw[,approx_eubs5:=((eur/(eur+eur5y/10000))^(1/5)*(1+ussw5/10000)-(1+eusw5/10000))*10000]
  # transform prw back to prl
  prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
  prl<-prl[!is.na(value)]
  #browser()
  dtl4<-dtl.addswapsprd(dtl3,prl)
   toc()
  list('prw'=prw,'prl'=prl,'dtl4'=dtl4,'br'=br)
}
resyldsprdv4<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=T,adjccybs=0,winsor.=.025,parallel.core.=1,mainccyin='usd'){
  # a wrapper for FE regression
  tic()
  dtl<-copy(dtlin)
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

  if (globaluponly){ # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  if (adjccybs==1)
    lsout<-getccyFE2(dtl,fieldstr='swapsprdadj',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  else
    lsout<-getccyFE2(dtl,fieldstr='swapsprd',version=regversion,winsor=winsor.,parallel.core=parallel.core.,mainccy=mainccyin)
  toc()
  if (returndt)
    lsout
  else
    lsout$regcoef
}
winsorize2<-function(dfin,lhs,bylist,winsor=.01){
  df2<-copy(dfin)
  df2[,pctl:=percent_rank(eval(exparse(lhs))),by=bylist]
  df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
  df2
}

#testing 
# dtreg<-filterglobaluponly(dtl)
# mainccy='usd'
# winsor=.025
# dtreg[,pctl:=percent_rank(swapsprd),by=.(date,ccy)]
# dtreg<-dtreg[pctl>=winsor & pctl<=(1-winsor)]
# dtreg[date==ymd('2004-01-30')] %>% felm(swapsprd~ccy+liq+amt | upcusip+ytm_bucket+rating_bucket+senior| 0 | upcusip, data=.) %>% summary

regfun<-function(dt,ccylist='',regversion=4,bylist='',lhs.,mainccy.){
  tc.ret<-tryCatch({
    if (regversion==1){
      reg<-felm(eval(exparse(lhs.))~ccy |upcusip |0 | upcusip,data=dt)
    } else if (regversion==3){
      reg<-felm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket | 0 | upcusip, data=dt)
    } else if (regversion==-4){
      reg<-felm(eval(exparse(lhs.))~0 | upcusip+ytm_bucket+rating_bucket | 0 | upcusip, data=dt)
    } else if (regversion==4){
      reg<-felm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket+rating_bucket | 0 | upcusip, data=dt)
    } else if (regversion==4.5){
      reg<-felm(eval(exparse(lhs.))~ccy+ytm | upcusip+rating_bucket | 0 | upcusip, data=dt)
    } else if (regversion==5){
      reg<-felm(eval(exparse(lhs.))~ccy +liq+amt| upcusip+ytm_bucket+rating_bucket+senior | 0 | upcusip, data=dt)
    } else if (regversion==5.5){
      reg<-felm(eval(exparse(lhs.))~ccy +liq+amt+ytm| upcusip+rating_bucket+senior | 0 | upcusip, data=dt)
    } else if (regversion==6){
      reg<-felm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket+rating_bucket+senior+liq_bucket+amt_bucket | 0 | upcusip, data=dt)
    } else if (regversion==7){
      reg<-felm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket+rating_bucket+liq_bucket|0|upcusip,data=dt)
    } else if (regversion==8){
      reg<-lm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket+liq_bucket|0|upcusip,data=dt)
    } else if (regversion==9){
      reg<-lm(eval(exparse(lhs.))~ccy | upcusip+ytm_bucket+ccy*rating_bucket| 0 | upcusip,data=dt)
    }
  }, error=function(err){
    print(err);print(bylist)
  })
  if (exists('reg')){
    if (regversion==9){dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy' | rn %like% '^rating'][,.(ccy=rn,est=Estimate,se=`Cluster s.e.`)]} else{
      dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Cluster s.e.`)]
      dtcoef2 <- cbind(dtcoef2,data.table('N'=reg$N,'rsq'=summary(reg)$r2adj))
      dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
      dtcoef2 %>% setkey(ccy)
      dtcoef2 <- dtcoef2[dtccy[ccy!=str_c(1,mainccy.)]]
    } 
    dtcoef2
  } else {
    browser('error in regfun')
    return(data.table('ccy'='eur','est'=as.numeric(NA),se=as.numeric(NA)))
  }
}
getccyFE2<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.025,parallel.core=1,mainccy='usd'){
  #  Generalized function for calculating FE 
  require(lfe)
  # print(str_c('FE on field: ',fieldstr))
    if ('field' %in% ds(dfin)) { # if dfin is in the long format with a field called 'field'
      df2<-dfin[field==fieldstr]
      lhs<-'value'
    } else { # if dfin is in the semi-wide format with a column called fieldstr
      df2<-copy(dfin)
      lhs<-fieldstr
    }
    setkey(df2,date,upcusip,ccy)
  #winsorize each date
    if (winsor!=0){
      df2[,pctl:=percent_rank(eval(exparse(lhs))),by=.(date,ccy)]
      df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
    }
  # set alphabetical order such that dummies are on foreign ccys
    df2[ccy==mainccy,ccy:=str_c(1,mainccy)]
    
  # introduce liquidity measure based on bond age
      df2[,liq:=ytm/ytofm]
      df2<-df2[liq %between% c(0,1.1)]
      df2[liq<.5,liq_bucket:=0] # more illiq
      df2[liq>=.5,liq_bucket:=1] # liq
      ccylist<-(df2 %>% distinct(ccy) %>% select(ccy))[[1]]
    if (parallel.core>1){     
      require(doParallel) 
      setkey(df2 ,'date'); indx<-split(seq(nrow(df2)),df2$date); registerDoParallel(parallel.core)
      out_list <- foreach(i = indx, .packages = packages.do.export,.export=c('regfun')) %dopar% {
        Psubset<-df2[i,]
        dtsubsetout<-regfun(Psubset,ccylist,version,Psubset[1,.(date)],lhs,mainccy.=mainccy)
        dtsubsetout[,date:=Psubset[1,.(date)]]
        dtsubsetout
      }
      regresult <- rbindlist(out_list,use.names=TRUE) #, fill=TRUE, etc.
    } else{ #non parallel version
      regresult<-df2[,regfun(.SD,ccylist,version,.BY,lhs,mainccy.=mainccy),by='date'] #,.SDcols=c(lhs,'ccy','upcusip','ytm_bucket','rating_bucket','liq_bucket','amt','amt_bucket','senior','liq')
    }
    regresult %>% setkey(date,ccy)
    regcoef <- regresult %>% dcast.data.table(date~ccy,value.var='est') %>% setkey(date)
    regse <- regresult %>% dcast.data.table(date~ccy,value.var='se'); regse %>% setkey(date)
    lsout<-list('regcoef'=regcoef,'regse'=regse,'regresult'=regresult,'dtreg'=df2)
    lsout
}
intrwrap<-function(dfin,sp,bylist,interprule=1){
 #wrapper function for interpolation
  splocal<-sp[date==bylist$date & ccy==bylist$ccy]
  if (nrow(splocal)<3) {
    if (bylist$date %between% c(2,6)) 
      print(str_c('No swap data; NAs on ',bylist$date,bylist$ccy))
    #browser()
    rep(0,nrow(dfin))
  } else{
    tryCatch(dfout<-approx(x=splocal$tenor,y=splocal$value,xout=dfin$ytm,rule=interprule),
             error=function(err){
               print(err)
               print(bylist)
               browser()
             })
    dfout$y
  }
}

backfillNAs.prl<-function(prl,tickerpattern='^eubsv\\d',dates2fill.='eu', roll.=10){
  # 7/8/16 Self-rolling join to fill missing 3s6s basis swap data
  # this function back-fills NANs for the datatable prl (price in long form); it fills tickers matching tickerpattern; new dates are either given by dates2fill.
  # roll. determines how far to lookback before giving up and filling NANs
  prl<-copy(prl)
  prw<-prl %>% distinct() %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value')
  if (dates2fill.=='eu'){
    dates2fill<-prw[!is.na(eusa5) & !is.na(eubs5),.(date)]
  } else {
    dates2fill<-dates2fill.
  }
  prlrolldata<-prl[ticker %like% tickerpattern,.(date,ticker,value)]
  setkey(prlrolldata,ticker,date)
  fields2fill<-dsl(prl,tickerpattern)
  blankslate<-data.table()
  for (f2f in fields2fill){
    blankslate<-rbind(blankslate,dates2fill[,.(date,ticker=f2f)])
  }
  setkey(blankslate,ticker,date)
  dtfill<-prlrolldata[blankslate,roll=roll.]
  #dtfill %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value') %>%  View
  dtfill<-dtfill[!is.na(value)]
  prlnew<-update.prl(prl,dtfill)
  setkey(prlnew,date,ticker,pk)
  prlnew
}
get.dtl.status.mo<-function(dtl,gracewindow=60,bondref.=bondref){
  # get a sense of what data needs to be updated at the monthly and daily frequency
  # gracewindow is the number of days that are allowed to elapse to be considered as filled back starting today
  dtl<-copy(dtl)
  aa<-dtl[,.(MonthlyMin=min(date),MonthlyMax=max(date)),.(pk)] 
  #setnames(aa,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  bb<-merge(bondref.[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],aa,by='pk',all.y=TRUE)
  bb[!is.na(mat2),mat:=mat2][is.na(mat) & !is.na(matbbg),mat:=matbbg]
  bb[mat<=today(),matured:=1][is.na(matured),matured:=0]
  bb[MonthlyMax>mat-50 | MonthlyMax>today()-gracewindow,monthlyfilled:=1][is.na(monthlyfilled),monthlyfilled:=0]
  print(bb[monthlyfilled==0,.N,MonthlyMax][order(-N)])
  bb
}

get.dtl.status<-function(dtl,gracewindow=60,bondref.=bondref){
  # get a sense of what data needs to be updated at the monthly and daily frequency
  # gracewindow is the number of days that are allowed to elapse to be considered as filled back starting today
  dtl<-copy(dtl)
  aa<-dtl[,.(mindt=min(date),maxdt=max(date)),.(pk,monthend)] %>% dcast.data.table(pk~monthend,value.var=c('maxdt','mindt'))
  setnames(aa,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  bb<-merge(bondref.[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],aa,by='pk',all.y=TRUE)
  bb[!is.na(mat2),mat:=mat2][is.na(mat) & !is.na(matbbg),mat:=matbbg]
  bb[mat<=today(),matured:=1][is.na(matured),matured:=0]
  bb[DailyMax>mat-50 | DailyMax>today()-gracewindow,dailyfilled:=1][is.na(dailyfilled),dailyfilled:=0]
  bb[MonthlyMax>mat-50 | MonthlyMax>today()-gracewindow,monthlyfilled:=1][is.na(monthlyfilled),monthlyfilled:=0]
  print(bb[dailyfilled==0,.N,DailyMax][order(-N)])
  print(bb[monthlyfilled==0,.N,MonthlyMax][order(-N)])
  bb
}
get.prl.status<-function(prl){
  prl.status<-prl[,.(mindt=min(date),maxdt=max(date)),.(ticker,monthend)] %>% dcast.data.table(ticker~monthend,value.var=c('maxdt','mindt'))
  setnames(prl.status,c('maxdt_0','mindt_0','maxdt_1','mindt_1'),c('DailyMax','DailyMin','MonthlyMax','MonthlyMin'))
  print(prl.status[,.N,DailyMax][order(-N)])
  print(prl.status[,.N,MonthlyMax][order(-N)])
  prl.status
}

dtl.addswapsprd<-function(dtl,prl){
  ######## calculate interpolated swap spread for each and every single bond
  ####################Move this entire part to preprossing part!!!
  dtl<-copy(dtl)
  prl<-copy(prl)
  
  swappricesl<-prl[ticker %like% '^\\w\\wsw\\d+',.(date,ticker,value)]   
  swappricesladj<-prl[ticker %like% '^\\w\\wsz\\d+' | ticker %like% '^ussw\\d+',.(date,ticker,value)] 
  
  swappricesl[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(ticker,regex('\\d+')))]
  swappricesladj[,ccy:=stringr::str_sub(ticker,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(ticker,regex('\\d+')))]
  
  swappricesl<-swappricesl[str_length(ccy)==3]
  swappricesladj<-swappricesladj[str_length(ccy)==3]

  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')  
  if (swappricesladj[is.na(tenor),.N]!=0) warning('swappricesladj has tenor not parsed')  
  
  setkey(swappricesl,date,ccy,tenor)  
  setkey(swappricesladj,date,ccy,tenor)  
  setkey(dtl,date,ccy)
  
  # find out what swap prices are missing
    dates2interp<-dtl[!is.na(ytm) & wday(date) %between% c(2,6),.N,by=.(date,ccy)]
    setkey(dates2interp,date,ccy)
    sp2interp<-swappricesl[,.N,.(date,ccy)][str_length(ccy)==3]
    missingswap<-sp2interp[dates2interp][is.na(N) | N<3]
    setkey(missingswap,date,ccy)
    print('missing swap prices on these dates for these ccys:')
    print(missingswap)

    dtl <- dtl %>% anti_join(missingswap,by=c('date','ccy')) %>% as.data.table()
    setkey(dtl,date,pk,value)
  
  dtl<-dtl[!is.na(ytm) & wday(date) %between% c(2,6)]
  
  
   dtl[,swapyld:=intrwrap(.SD,swappricesl,.BY,interprule=1),by=.(date,ccy)][swapyld==0,swapyld:=NA]
   dtl[,swapyldadj:=intrwrap(.SD,swappricesladj,.BY,interprule=1),by=.(date,ccy)][swapyldadj==0,swapyldadj:=NA]
  
   if(0){ # still testing, muitlpe core 
  # tic()
      dtin<-copy(dtl)  
      cores=4
      registerDoParallel(cores)
      setkey(dtin ,date,ccy)
      indx<-split(seq(nrow(dtin)),dtin[,.(date,ccy)])
      out_list <- foreach(i = indx, .packages = packages.do.export ) %dopar% {
        Psubset<-dtin[i,]
        dtsubsetout<-data.table('swapyld'=intrwrap(Psubset,swappricesl,Psubset[1,.(date,ccy)],interprule=1))
        dtsubsetout
      }
      dtout <- rbindlist(out_list,use.names=TRUE) #, fill=TRUE, etc.
      dtout[swapyld==0,swapyld:=NA]
      dtl.<-cbind(dtin,dtout)

      cores=4
      registerDoParallel(cores)
      setkey(dtin ,date,ccy)
      indx<-split(seq(nrow(dtin)),dtin[,.(date,ccy)])
      out_list <- foreach(i = indx, .packages = packages.do.export) %dopar% {
        Psubset<-dtin[i,]
        dtsubsetout<-data.table('swapyldadj'=intrwrap(Psubset,swappricesladj,Psubset[1,.(date,ccy)],interprule=1))
        dtsubsetout
      }
      dtout <- rbindlist(out_list,use.names=TRUE) #, fill=TRUE, etc.
      dtout[swapyldadj==0,swapyldadj:=NA]
      dtl.<-cbind(dtl.,dtout)
    # toc()
    }
  
  # print(identical(dtl,dtl.))
  # browser()
    
  dtl[,swapsprdadj:=value*100-swapyldadj]
  dtl[,swapsprd:=value*100-swapyld]
  dtl[,swapyld:=NULL][,swapyldadj:=NULL]
  # dtl.na<-dtl[is.na(swapsprd)] 
  # dtladj.na<-dtl[is.na(swapsprdadj)] 
  # dtladj.na[!is.na(swapsprd),.N,ccy] # mainly missing GBP 30 yr+, need xccb for gbp 30
  dtl<-dtl[!is.na(swapsprd)] #get rid of ones that can't be interpolated for one reason or another
  #dtl<-dtl[!is.na(swapsprdadj)] #get rid of ones that can't be interpolated for one reason or another
  dtl
}

icollapse.againstall<-function(dtin.,ccyA='eur',collapse.freq='month',filter=0){
  # newer version of collapsing against all ccy in calculating mu
  #ccyA="eur";dtin.<-dtiss.in
  
  if (ccyA=='eur'){ 
    natA<-'eurozone'
  } else if(ccyA=='gbp'){
    natA<-'united kingdom'
  } else if(ccyA=='jpy'){
    natA<-'japan'
  } else if(ccyA=='aud'){
    natA<-'australia'
  } else if(ccyA=='chf'){
    natA<-'switzerland'
  } else if(ccyA=='cad'){
    natA<-'canada'
  } else if(ccyA=='usd'){
    natA<-'united states'
  } else {stop(str_c('no ccy country match: ',ccyA))}
  
  # allccys <- dtin.[,.N,ccy]$ccy
  #   otherccys <-allccys[allccys %nlk% ccyA] 

  #   dtin.
  #   dtin.2 <- dtin. %>% add.earlist.iss.in.ccy()
    
  #   if (filter==1){ # issued in both ccy before
  #     dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][d>=earlist.usd & d>=eval(exparse(str_c('earlist.',ccyA)))]
  #   } else if (filter==2){ # issued in both ccy ever (before and after)
  #     dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
  #   }
    
    #print(str_c('collapsing using: ', ccyA, ' ',natA))
    dtin<-copy(dtin.[d>=ymd('2002-01-01')])
    dtin[,ccy:=str_to_lower(ccy)]
    dtin[,modnat:=str_to_lower(modnat)]
    dtin<-dtin[modnat %in% c(natA)]
    
    dtin[,date:=floor_date(d,collapse.freq)]
    
    # basic summation in this section: cannot yet do add/subract/divid/multiple since rows are different
    # domestic issuance
    dtin[ccy==ccyA,I_domestic:=sum(na.omit(amt))/1000,by=date]
    # foreign
    dtin[ccy!=ccyA,I_foreign:=sum(na.omit(amt))/1000,by=date]
    # total 
    dtin[,I_total:=sum(na.omit(amt))/1000,by=date]

    # first collapse into unique values by all columns, but each of the variables appear on a different row
    dt2<-dtin[,.(date,I_domestic,I_foreign,I_total)] %>% unique()    
    # get rid of NAs by combining rows of identical yrmo, first melt into long, get rid of NAs, then cast back to wide
    dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')))[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})
    
    # when there are no issuance for a certain month, use zero
    dtout[is.na(I_domestic),I_domestic:=0][is.na(I_foreign),I_foreign:=0][is.na(I_total),I_usd_total:=0]
    
    # Calculations based on earlier summations: mu
    dtout[,mu:=I_domestic/I_total*100]
    
    # check to make sure there's no dates missing in case there is a month without any issuance at all!
    dtout<-dtout %>% expandfulldates(.,freq=collapse.freq) %>% as.data.table()
    
    # # cacluate a smooth version: this is actually less smooth
    # if (collapse.freq=='month'){
    #   navg=11  
    # }else if (collapse.freq=='quarter'){
    #   navg=3
    # }else if (collapse.freq=='year'){
    #   navg=1
    # }
    # dtout[,I_total_L.avg:=rowMeans(dtout[,shift(I_total,n=0:navg,type='lag')])]
    # dtout[,mu.smooth:=I_domestic/I_total_L.avg*100] 
    # 
    # if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');browser()}
    dtout[,ccy:=ccyA]
    dtout %>% setkey(date,ccy)
    dtout
  }

    icollapse4<-function(dtin.,ccyA=dtin.[str_to_lower(ccy) %nlk% 'usd'][1,str_to_lower(ccy)],collapse.freq='month',filter=0){
      # newer version of collapsing 
      # todo: construct and use modupccy
      #ccyA="eur";dtin.<-dtin
      require(doParallel)
      if (ccyA=='eur') natA<-'eurozone'
      else if (ccyA=='gbp') natA<-'united kingdom'
      else if (ccyA=='jpy') natA<-'japan'
      else if (ccyA=='aud') natA<-'australia'
      else if (ccyA=='chf') natA<-'switzerland'
      else if (ccyA=='cad') natA<-'canada'
      else if (ccyA=='usd') return(data.table(NA))
      else stop(str_c('no ccy country match: ',ccyA))

      if (filter==1){ # issued in both ccy before
        dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][d>=earlist.usd & d>=eval(exparse(str_c('earlist.',ccyA)))]
      } else if (filter==2){ # issued in both ccy ever (before and after)
        dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
      }

      #print(str_c('collapsing using: ', ccyA, ' ',natA))
      dtin<-copy(dtin.[d>=ymd('2002-01-01')])
      dtin[,ccy:=str_to_lower(ccy)]
      dtin[,modnat:=str_to_lower(modnat)]
      dtin<-dtin[modnat %in% c(natA,'united states') & ccy %in% c('usd','1usd',ccyA)]
      natA=str_to_lower(natA)
      dtin[,date:=floor_date(d,collapse.freq)]
      
      # basic summation in this section: cannot yet do add/subract/divid/multiple since rows are different
      # Yankee isuance
      dtin[modnat==natA & ccy %like% 'usd|1usd',I_fUSD:=sum(na.omit(amt))/1000,by=date]
      # reverse yankee
      dtin[modnat=='united states' & ccy==ccyA,I_usF:=sum(na.omit(amt))/1000,by=date]
      # issuance from both countries in either currencies
      dtin[,I_both:=sum(na.omit(amt))/1000,by=date]
      # mu: issuance only in usd/total issuance
      dtin[ccy %like% 'usd|1usd',I_usd_tot:=sum(na.omit(amt))/1000,by=date]
      
      

      # first collapse into unique values by all columns, but each of the variables appear on a different row
      dt2<-dtin[,.(date,I_fUSD,I_usF,I_both,I_usd_tot)] %>% unique()    
      # get rid of NAs by combining rows of identical yrmo, first melt into long, get rid of NAs, then cast back to wide
      dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')))[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})
      
      # when there are no flow for a certain month, use zero
      dtout[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][is.na(I_usd_tot),I_usd_tot:=0]

      # Calculations based on earlier summations: net flow are net Yankee flow
      dtout[,I_netflow:=I_fUSD-I_usF][,i_netflow:=I_netflow/I_both*100][,mu:=I_usd_tot/I_both]
      
      # cacluate a smooth version
      dtout[,I_both_12m.L.avg:=rowMeans(dtout[,shift(I_both,n=0:11,type='lag')])]
      dtout[,i_netflow.smooth:=I_netflow/I_both_12m.L.avg*100] 

      # check to make sure there's no dates missing in case there is a month without any issuance at all!
      dtout<-dtout %>% expandfulldates(.,freq=collapse.freq) %>% as.data.table()
      # if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');browser()}
      dtout[,ccy:=ccyA]
      dtout %>% setkey(date,ccy)
      dtout
    }

    
    icollapse4.mature<-function(dtin.,ccyA=dtin.[str_to_lower(ccy) %nlk% 'usd'][1,str_to_lower(ccy)],collapse.freq='month',filter=2){
      # newer version of collapsing 
      # todo: construct and use modupccy
      #ccyA="eur";dtin.<-dtin
      #dtin.=dtin2;  ccyA='eur';  freq='month';  filter=0;
      if (ccyA=='eur') natA<-'eurozone'
      else if (ccyA=='gbp') natA<-'united kingdom'
      else if (ccyA=='jpy') natA<-'japan'
      else if (ccyA=='aud') natA<-'australia'
      else if (ccyA=='chf') natA<-'switzerland'
      else if (ccyA=='cad') natA<-'canada'
      else if (ccyA=='usd') return(data.table(NA))
      else stop(str_c('no ccy country match: ',ccyA))
      natA=str_to_lower(natA)
      
      if (filter==1){ # issued in both ccy before
        dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][mat2>=earlist.usd & mat2>=eval(exparse(str_c('earlist.',ccyA)))]
      } else if (filter==2){ # issued in both ccy ever (before and after)
        dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
      } else{
        dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')]
      }
      
      print(str_c('collapsing using: ', ccyA, ' ',natA))
      dtin<-copy(dtin.[d>=ymd('2002-01-01')])
      dtin[,ccy:=str_to_lower(ccy)]
      dtin[,modnat:=str_to_lower(modnat)]
      dtin<-dtin[modnat %in% c(natA,'united states') & ccy %in% c('usd','1usd',ccyA)]
      dtin[,date:=floor_date(mat2,collapse.freq)]
      dtin <- dtin[!is.na(date)]
      
      # basic summation in this section: cannot yet do add/subract/divid/multiple since rows are different
      # Yankee isuance
      dtin[modnat==natA & ccy %like% 'usd|1usd',I_fUSD:=sum(na.omit(amt))/1000,by=date]
      
      # reverse yankee
      dtin[modnat=='united states' & ccy==ccyA,I_usF:=sum(na.omit(amt))/1000,by=date]
      # issuance from both countries in either currencies
      dtin[,I_both:=sum(na.omit(amt))/1000,by=date]
      # mu: issuance only in usd/total issuance
      dtin[ccy %like% 'usd|1usd',I_usd_tot:=sum(na.omit(amt))/1000,by=date]
       
      
      # first collapse into unique values by all columns, but each of the variables appear on a different row
      dt2<-dtin[,.(date,I_fUSD,I_usF,I_both,I_usd_tot)] %>% unique()    
      # get rid of NAs by combining rows of identical yrmo, first melt into long, get rid of NAs, then cast back to wide
      dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')))[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})
      
      # when there are no flow for a certain month, use zero
      dtout[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][is.na(I_usd_tot),I_usd_tot:=0]
      
      # Calculations based on earlier summations: net flow are net Yankee flow
      dtout[,I_netflow:=I_fUSD-I_usF][,i_netflow:=I_netflow/I_both*100][,mu:=I_usd_tot/I_both]
      
      # check to make sure there's no dates missing in case there is a month without any issuance at all!
      dtout<-dtout %>% expandfulldates(.,freq=collapse.freq) %>% as.data.table()
      #if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');}
      dtout[,ccy:=ccyA]
      dtout %>% setkey(date,ccy)
      dtout 
    } 
  
expandfulldates<-function(dfin,freq='month'){
    #dfin<-dtout;freq='month'
  date.max<-max(dfin$date)
  date.min<-min(dfin$date)
  all.dates <- seq(date.min, date.max, by=freq)
  all.dates.frame <- data.frame(list(date=all.dates))
  dfout<-dfin %>% full_join(.,all.dates.frame,by='date') %>% 
    replace(is.na(.),0) %>% 
    arrange(date)
  dfout
  #dfin
}
icollapsedaily <- function(dfin,ccyA="EUR",natA="Eurozone"){
  # collapse into daily aggregate flows
  df_fUSD<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat==natA, ccy=='USD') %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_fUSD=amt,nrating_fUSD=nrating,ytofm_fUSD=ytofm,date=d)
  
  df_usF<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat=='United States', ccy==ccyA) %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_usF=amt,nrating_usF=nrating,ytofm_usF=ytofm,date=d)
  
  dfout<-df_fUSD %>% full_join(.,df_usF,by='date') %>% 
    replace_na(list(I_fUSD=0,I_usF=0)) %>%     
    mutate(I_net_fus=I_fUSD-I_usF) %>% 
    select(date,I_net_fus,I_usF,I_fUSD)
  if (ccyA=="EUR") dfout<-filter(dfout,date>'2001-12-31')
  dfout
}


icollapse_all <- function(dfin){
  df_euus<- dfin %>% icollapse(.,ccyA = "EUR",natA="Eurozone") %>% 
    select(date,I_net_fus,IN_fus) %>% 
    rename(I_net_euus=I_net_fus,IN_euus=IN_fus) %>% 
    expandfulldates(.)
  df_gbus<- dfin %>% icollapse(.,ccyA = "GBP",natA="United Kingdom") %>% 
    select(date,I_net_fus,IN_fus) %>% 
    rename(I_net_gbus=I_net_fus,IN_gbus=IN_fus) %>% 
    expandfulldates(.)
  df_jpus<- dfin %>% icollapse(.,ccyA = "JPY",natA="Japan") %>% 
    select(date,I_net_fus,IN_fus) %>% 
    rename(I_net_jpus=I_net_fus,IN_jpus=IN_fus) %>% 
    expandfulldates(.)
  df_auus<- dfin %>% icollapse(.,ccyA = "AUD",natA="Australia") %>% 
    select(date,I_net_fus,IN_fus) %>% 
    rename(I_net_auus=I_net_fus,IN_auus=IN_fus) %>% 
    expandfulldates(.)

  df_fus<- df_euus %>% 
    full_join(.,df_gbus,by='date') %>% 
    full_join(.,df_jpus,by='date') %>% 
    full_join(.,df_auus,by='date') %>% 
    arrange(date)
  df_fus
}

plotgl <- function(dfin,fields=c('I_net_euus'),afteryr=2002){
  dfin %>% 
  filter(year>=afteryr) %>%
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()

}
ggplotw<-function(dfin, fields=ds(dfin),x11.=FALSE){
  if (x11.) X11(width=15,height=9)
  dfin %>% 
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()

}
ggplotl<-function(dfin){
  dfin %>% ggplot(data=.,aes(x=date,y=value,colour=ticker))+geom_line()+geom_point()
}
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
ds<-function(dfin,matchpattern='.'){
  require(stringr)
  cn<-colnames(dfin) 
  try(cn[str_locate(as.character(cn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}
dsl<-function(dtlin,matchpattern='.',field='ticker'){

  #fn<-(dtlin[,ticker] %>% unique())[[1]]
   dtlin<-copy(dtlin)
   setkeyv(dtlin,field)
   fn<-unique(dtlin)[,field,with=F][[1]]
  
  try(fn[str_locate(as.character(fn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}


wgplot <- function(dfwide,idvar='date'){
  dfwide %>% 
  reshape2::melt(.,id.vars=idvar) %>%
  #filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line()+geom_point()
}





tabulate <- function(dfin,byvar='variable'){
  # dfin %>% group_by_(byvar) %>% dplyr::summarise_(count=length(byvar)) %>% arrange(desc(count))
  table(dfin[byvar]) %>% as.data.frame() %>%  tbl_df() %>% arrange(desc(Freq))
}

unpackbbgprices<-function(prices){
  # use when prices are batched in a list of say 20 groups
  a0 <- unlist(prices, recursive = FALSE)
  tickernames <- names(a0)
  df_prices <- data.frame() %>% tbl_df()
  for (i in 1:length(tickernames)) {
    temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
    if (nrow(temp_new) == 0)
      print (str_c('empty:#', i, ' name:', tickernames[i]))
    df_prices %<>% dplyr::bind_rows(., temp_new)
  }
  df_prices
}

requestfigibyisin<-function(df_isins){
 # given a dataframe of isins, get a dataframe of isin and figi mappings 
  require('magrittr')
  require('httr')
  require('jsonlite')
  tic()
  #require('tidyjson')
  # figireq<-'[{"idType":"ID_ISIN","idValue":"XS1033736890"},
  # {"idType":"ID_BB_UNIQUE","idValue":"JK354407"},
  # {"idType":"ID_BB","idValue":"JK354407"},
  # {"idType":"COMPOSITE_ID_BB_GLOBAL","idValue":"JK354407"},
  # {"idType":"TICKER","idValue":"JK354407 Corp"},
  # {"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]' ## FIGI code
  # # figireq<-'[{"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'
  print(str_c('min est:',nrow(df_isins)/10000))
    ptm <- proc.time()
    counter <- 0 # count request up to 100, for figi limit of 100 request per minute
    df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
    diag_response<-list()
    for (j in 1:ceiling(nrow(df_isins)/100)){
      counter <- counter+1
      if (counter==100){
       save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
         print(str_c("row (j):",j*100))
         # if ((proc.time() - ptm)[[3]]<=60){ # let it sleep to a full minute if it hasn't been a full minute
           print(str_c('last cycle:',(proc.time() - ptm)[[3]]))
           print(str_c('sleeping max of full 60 sec or next min:',Sys.time()))
           Sys.sleep(max(61-(Sys.time() %>% second()),61-((proc.time() - ptm)[[3]])))
           print(str_c('awake:',Sys.time()))
           #counter %>% print
           ptm <- proc.time()
           counter <- 0
      #    } else { # continue and reset counters and time
      #   ## let it sleep till the next minute regardless
      #     print(str_c('sleeping till next min:',proc.time()[[3]]))
      #     Sys.sleep(60-Sys.time() %>% second())
      #     ptm <- proc.time()
      #     print(str_c('awake:',proc.time()[[3]]))
      #     counter <- 0
      # } 
      }
      tempreq <- df_isins %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
      figireq<- tempreq %>%  mutate(idType='ID_ISIN',idValue=isin) %>% select(-isin)  %>% jsonlite::toJSON() 
      r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
      # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
      tryCatch({
        responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
      }, error = function(err) {
        print(r %>% content(as = "text") %>% str_sub(.,1,50))
        
        counter <- 0
        print(str_c('sleeping on error:',Sys.time()))
        Sys.sleep(60)
        ptm <- proc.time()
        print(str_c('awake:',Sys.time()))
        r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
        tryCatch({
          responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
        }, error=function(err){
          error('repeated request error, stopping')
          stop()
        })
      })
      
     if (colnames(responsejson)==c('error')){
        next
      }
      diag_response[j]<-responsejson
      # extract 100x100 results at a time
      temp_isin2figi<-as.data.frame(list()) %>% tbl_df()
      if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
      for  (i in 1:nrow(responsejson)){
        if (ncol(responsejson)==1) { # only data column
          
          temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
        } else{ # data column and error column #####something is not right
          if (is.na(responsejson$error[i][[1]]))
            temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
          else
            temp_isin2figi %<>% bind_rows(.,data_frame(isin=tempreq$isin[i][[1]]))
        }
      }
      if (nrow(temp_isin2figi)<nrow(tempreq)) browser()
      df_isin2figi_all %<>% bind_rows(.,temp_isin2figi)
    }
    toc()
    #this is the isin to figi mapping that contains 
    list(df_isin2figi_all,diag_response)
}

requestfigibyID2<-function(dtid_in,idtypein='isin',diagnostics=F,uselocalfirst=T){
 # given a dataframe of ID, get a dataframe of isin and figi mappings 




  require('httr');  require('jsonlite');  
  tic()
  ##########
  getresult100<-function(id2req){
    # this function would return a data.table of 100 rows if it's good, return 1 if bad
     json2req <- id2req %>% jsonlite::toJSON() 
      tryCatch({
      result100<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = json2req, encode = "json")
        result100_json <-   result100 %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
        

        if (!is.data.frame(result100_json)) browser()
        dt100temp<-result100_json %>% as.data.table()
        dt100temp$idValue<-id2req$idValue
        
        dtout<-data.table()
        for (m in 1:nrow(dt100temp)){
          singlerow<-dt100temp[,.(data)][[1]][[m]] %>% as.data.table()
          if (nrow(singlerow)>0){
            singlerow$idValue=dt100temp[,.(idValue)][[1]][[m]]
            dtout<-rbind(dtout,singlerow,fill=T)
          } else if (dt100temp[,.(error)][[1]][[m]]=='No identifier found.'){
            # dtout<-rbind(dtout,data.table('idValue'=dt100temp[,.(idValue)][[1]][[m]],'figi'=NA),fill=T)
          } else {
            browser()
          }
        }
                
        #dtout <- unlist(result100_json,recursive=F) %>% rbindlist(.,idcol='Nid')
        # dtout[,Nid:=as.numeric(str_sub(Nid,5))]
        # id2req[,Nid:=.I]
        # dtout<-merge(dtout,id2req,by='Nid')
        # browser()
        dtout
      }, error = function(err) {
        errstr<-result100 %>% content(as = "text") %>% str_sub(.,1,50)
        #browser()
        #result100 %>% content(as = "text") %>% str_replace_all(.,'{\"error\":\"No identifier found.\"}','')
        if (length(grep('error|Too Many',errstr))==0) browser()
        # if (length(grep('Too Many',errstr))==0) print(errstr)
        if (length(grep('Too Many',errstr))==1) {
          Sys.sleep(60); counter <<- 0
          print(str_c("Sleep: row (j):",j*100))
        }
        return(errstr)
      })
    }

    print(str_c('min est:',nrow(dtid_in)/10000))

    dtid_in<-unique(copy(dtid_in[!is.na(eval(exparse(idtypein))),.(eval(exparse(idtypein)))]))
    dtid<-copy(dtid_in) # create a copy so that the original can be compared to get missing at the end
    
    # first check the existance of the data in local database
    if ((idtypein=='isin' | idtypein=='figi') & uselocalfirst){
      load(file='db/dt_isin_figi.RData')
      setkeyv(dt.isin.figi,idtypein)
      setkeyv(dtid,idtypein)
      if ( idtypein=='isin'){ # if requesting by isin, can check against known missing list
        setkeyv(isin.no.figi,idtypein)
        dtid <- dtid[!isin.no.figi] #rid of missing figi requests
      }
      dtfigiout<-dt.isin.figi[dtid,nomatch=0] # matched requests output
      dtid <- dtid[!dtfigiout] #rid of matched requests
      if (nrow(dtid)==0) {
        print ('no new request made, all requests are from local DB')
        if (diagnostics)
          return(list('dtout'=dtfigiout))
        else
          return(dtfigiout)
      }
    } else if ((idtypein=='cusip9' | idtypein=='cusip8') & uselocalfirst){
      load(file='db/cusip_figi.RData')
      setkeyv(cusip.figi,idtypein)
      setkeyv(dtid,idtypein)
      dtfigiout<-cusip.figi[dtid,nomatch=0] # matched requests output
      dtid <- dtid[!dtfigiout] #rid of matched requests
      if (nrow(dtid)==0) {
        print ('no new request made, all requests are from local DB')
        if (diagnostics)
          return(list('dtout'=dtfigiout))
        else
          return(dtfigiout)
      }
    } else { #starting from scratch
      print('starting from scratch')
      dtfigiout<-data.table()
    }

    print(str_c('requesting total of :',nrow(dtid)))
    splitN<-ceiling(nrow(dtid)/100)
    dtid[,grp:=ceiling(.I/100)]


    if (idtypein=='isin') dtid$idType='ID_ISIN'
    if (idtypein=='figi') dtid$idType='ID_BB_GLOBAL'
    if (idtypein=='cusip8' | idtypein=='cusip9') dtid$idType='ID_CUSIP'
    

    counter <- 0 # count request up to 100, for figi limit of 100 request per minute
    
    
    #####Loop
    for (j in 1:ceiling(nrow(dtid)/100)){
      counter <- counter+1
      # if (counter==100){
         # print(str_c("Sleep: row (j):",j*100))
         # Sys.sleep(60); counter <- 0
      # }
      
      idreqtemp <- dtid[grp==j,.(idType,idValue=eval(exparse(idtypein)))]
      result100 <- getresult100(idreqtemp)

      if (!is.data.table(result100)){
        # Sys.sleep(60); counter=0
        result100 <- getresult100(idreqtemp)
      }
      if (!is.data.table(result100)){
        print('sleep for a second time due to error')
        # Sys.sleep(60); counter=0
        result100 <- getresult100(idreqtemp)
      }
      if (!is.data.table(result100)){
        print(str_c('give up due to error on grp: ',j))
        print(result100)
      } else { # this is the good scenario
        result100[,grp:=j]
        if (idtypein=='isin') setnames(result100,'idValue','isin')
        if (idtypein=='cusip8' | idtypein=='cusip9') setnames(result100,'idValue',idtypein)
        if (idtypein=='figi'){
           setnames(result100, 'figi','figi_old')
           setnames(result100,'idValue','figi')
          }

        dtfigiout<-rbind(dtfigiout,result100,fill=T)
      }
    }
    toc()
    setkeyv(dtfigiout,idtypein)
    setkeyv(dtid_in,idtypein)
    missing <- dtid_in[!dtfigiout]
    if (nrow(missing)>0){ 
      print('missing:')
      print(missing)
    }

    dups<-dtfigiout[,.N,eval(exparse(idtypein))][N>1]
    if (nrow(dups)>0){
      print('dups:')
      print(dups)
      print('marketsector of dups:')
      print(dups[dtfigiout,nomatch=0][,.N,marketSector])
    }

    if (diagnostics)
      list('dtout'=dtfigiout[,c(idtypein,'figi', 'marketSector',  'ticker', 'name', 'securityType', 'exchCode'),with=F], 'dtfull'=dtfigiout, 'missing'=missing, 'dups'=dups)
    else 
      dtfigiout[,c(idtypein,'figi', 'marketSector',  'ticker', 'name', 'securityType', 'exchCode'),with=F]
}


requestfigiinfo<-function(df_id,idstr='ID_ISIN'){
 # given a dataframe of ids, get info from openfigi isin
  tic()
  require('magrittr'); require('httr'); require('jsonlite')
  print(str_c('min est:',nrow(df_id)/10000))
  ptm <- proc.time()
  counter <- 0 # count request up to 100, for figi limit of 100 request per minute
  df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
  diag_response<-list()
  for (j in 1:ceiling(nrow(df_id)/100)){
    counter <- counter+1
    if (counter==100){
     save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
       print(str_c("row (j):",j*100))
         print(str_c('last cycle:',(proc.time() - ptm)[[3]]))
         print(str_c('sleeping max of full 60 sec or next min:',Sys.time()))
         Sys.sleep(max(61-(Sys.time() %>% second()),61-((proc.time() - ptm)[[3]])))
         print(str_c('awake:',Sys.time()))
         #counter %>% print
         ptm <- proc.time()
         counter <- 0
    }
    tempreq <- df_id %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
    figireq<- tempreq %>%  mutate(idType=idstr,idValue=id) %>% select(-id)  %>% jsonlite::toJSON() 
    r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
    # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
    tryCatch({
      responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
    }, error = function(err) {
      print(r %>% content(as = "text") %>% str_sub(.,1,50))  
      counter <- 0
      print(str_c('sleeping on error:',Sys.time()))
      Sys.sleep(60)
      ptm <- proc.time()
      print(str_c('awake:',Sys.time()))
      r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
      tryCatch({
        responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
      }, error=function(err){
        error('repeated request error, stopping')
        stop()
      })
    })
    
   if (colnames(responsejson)==c('error')){
      next
    }
    diag_response[j]<-responsejson
    # extract 100x100 results at a time
    temp_isin2figi<-as.data.frame(list()) %>% tbl_df()
    if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
    for  (i in 1:nrow(responsejson)){
      if (ncol(responsejson)==1) { # only data column
        
        temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(id=tempreq$id[i][[1]]))
      } else{ # data column and error column #####something is not right
        if (is.na(responsejson$error[i][[1]]))
          temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(id=tempreq$id[i][[1]]))
        else
          temp_isin2figi %<>% bind_rows(.,data_frame(id=tempreq$id[i][[1]]))
      }
    }
    if (nrow(temp_isin2figi)<nrow(tempreq)) browser()
    df_isin2figi_all %<>% bind_rows(.,temp_isin2figi)
  }
  toc()
  #this is the isin to figi mapping that contains 
  list(df_isin2figi_all,diag_response)
}

countdups<-function(dfin,field='isin'){
  (dfin %>% nrow)-(dfin %>% distinct_(field) %>% nrow)
}


loadBBGdownload2df<-function(filename='../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData'){
  # takes in a filename associated with bbg download; spits out price dataframe with field as colnames and a variable type
  load(filename)
  a0 <- unlist(prices, recursive = FALSE)
  dtw<-data.table::rbindlist(a0,use.names=TRUE,idcol=TRUE)
  dtl<-melt(dtw,id.vars=c('date','.id'),variable.name='field')[!is.na(value)]
  setnames(dtl,'.id','pk')
  setkey(dtl,date,pk)
  dtl
}

loadBBGdownload2df_older<-function(filein='../data/bloomberg/bbgpricesall151207_daily.RData'){
  load(filein)
  i=1
  dfout<-price[[i]]
  colnames(dfout)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
  
  for (i in 2:nrow(tickerraw)){
    dfnew<-price[[i]]
    colnames(dfnew)<-c('datestr',str_to_lower(str_split(tickerraw[i,],' ')[[1]][1]))
    dfout=merge(dfout, dfnew, all=TRUE)
  }
  dtout<- dfout %>% as.data.table() %>% melt(iv.vars='datestr',variable.name='ticker')
  dtout[,date:=ymd(datestr)][,datestr:=NULL]
  dtout<-dtout[!is.na(value)]
  setkey(dtout,date,ticker)
  dtout
}
melt.gl<-function(dfin,idvars=c('date','pk','batch')){
  #Deprecated: not useful: a wrapper for melt that sets measure.vars to be anything other than idvars
  setkey_(dfin,idvars)
  dtl_out<-melt(dfin,id.vars=idvars,measure.vars=((dfin %>% ds)[(dfin %>% ds) %ni% idvars]),
              variable.name='field')[!is.na(value)]
}

assessDataCoverage<-function(bondinfo,bondprices,field='YLD_YTM_MID',lastdate='lastobs',startdate='firstobs'){
  # check monthly data coverage; bondinfo is essentially sdc infomation on maturity etc, bondprice is from bbg download
  # 
  #   bondprices<-df_p %>% filter(date>='2005-01-01',date<='2016-04-01') 
  #   bondinfo<-df_sdc_all
  #   field="YLD_YTM_MID"
  # field="BLP_Z_SPRD_MID"
  # lastdate='lastobs'
  # startdate='firstobs'
  # #   
  #count unique bond tickers
  if (startdate=='firstobs') {
    startdate = min(bondprices$date)
    str_c('startdate: ', startdate) %>% print
  }
  if (lastdate == 'lastobs') {
    lastdate = max(bondprices$date)
    str_c('lastdate: ', lastdate) %>% print
  }
  
  
  if ('pk' %ni% (bondprices %>% ds)) bondprices %<>% rename(pk=ticker) 
  str_c('unique securities: ',bondprices %>% tabulate('pk') %>% nrow) %>% print
  
  # add expected number of months
  bondinfo %<>% distinct(pk) %>%  mutate(expmonthlyobs=ceiling((pmin(as.numeric(lastdate),as.numeric(mat2))-pmax(as.numeric(d),as.numeric(startdate)))/30.5)) 
  # count number of observations by isin # compare to number of expected obs by isin
  bondtickers <- bondprices %>% group_by(pk) %>% summarise(obs_allflds=length(pk))
  fldfreq<-bondprices[!is.na(bondprices[field]),] %>% select_('date','pk',field) %>% group_by(pk) %>% summarize(obs_specfld=length(date))
  bondtickers %<>% left_join(fldfreq,by='pk')
  bondtickers %<>% mutate(obs_specfld=ifelse(is.na(obs_specfld),0,obs_specfld))
  df_obs<-bondtickers %>% inner_join(bondinfo,by='pk') %>% mutate(obsdiff=expmonthlyobs-obs_specfld, obscoverage=ifelse(expmonthlyobs>=12,obs_specfld/expmonthlyobs,1)) %>% select(pk,isin,obsdiff,obscoverage,obs_allflds,obs_specfld,expmonthlyobs,mat2,d,settlement2)
  print('coverage:')
  str_c('# obs matching sdc:',df_obs %>% nrow()) %>% print
  df_obs$obscoverage %>% summary %>% print
  ggplot(df_obs,aes(x=obscoverage))+stat_ecdf()
  print('all fld obs diff from expected number of obs')
  df_obs %>% mutate(allflddiff=expmonthlyobs-obs_allflds) %$%summary(allflddiff) %>% print
  df_obs
  # df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
  # df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View 
}


showdups<-function(dfin,vars=key(dfin)){
  print('showing duplicates in:')
  print(vars)
  oldkey<-key(dfin)
  setkeyv(dfin,vars)
  dupkey<-dfin[,.N,by=vars][N>1,vars,with=FALSE]
  dfout<-dfin[dupkey]
  if(length(oldkey)>0) {
    if (!identical(oldkey,vars)) setkeyv(dfin,oldkey)
  }
  else {
    warning('set key(s) to vars:',vars)
  }
  dfout
}



openfigi.response2DT<-function(res){
  # not used
  aa<-list()
  # i=1
  for (i in 1:length(res)){
    aa[[i]]<-data.table()
    # tryCatch(aa[[i]]<-rbindlist(res[[i]]))
     for (j in 1:length(res[[i]])){
      aa[[i]]<-rbind(aa[[i]],data.table(res[[i]][[j]]))
    }
    print(i)
  }
  bb<-rbindlist(aa,fill=TRUE)
  bb
}


Mode <- function(x,bylist,field) {
  # find the most frequent appearance of field for bylist
  #y<-copy(bondref); bylist='upcusip'; field='ccy'
  y<-copy(x)
  y %>% setkeyv(c(bylist,field))
  out<-y[,c(bylist,field),with=F][,Ncount:=.N,c(bylist,field)] %>% unique
  out<-out[order(upcusip,-Ncount),.SD[1],bylist][,c(bylist,field),with=F]
  setkeyv(out,bylist)
  out
}
mergebymonth<-function(dfin1,dfin2){
  dfin1<-as.data.table(dfin1)
  dfin2<-as.data.table(dfin2)
  dfin1[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfin2[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfjoin<-merge(dfin1,dfin2,by='yrmo')
  dfjoin[,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  dfjoin
}
winsorize<-function(dfin){
  dfin[,pctl:=percent_rank(value)][pctl>=.01 & pctl<=.99]
  # tempdf<-dti %>% mutate(pctl=percent_rank(value))
  # tempdf[pctl>=.01 & pctl<=.99]
}

filterglobaluponly<-function(dtlin){
  dtl<-copy(dtlin)
  tokeep<-dtl[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N!=1][,.(date,upcusip)]
  setkey(tokeep,date,upcusip)
  setkey(dtl,date,upcusip)
  dtl[tokeep]
}

bucketrating<-function(dtlin){
  # creates new column called rating bucket as factor with 4 levels 
  #dtlout<-dtlin
  #fread('rating.csv') %>% dt2clip
  dtlin[nrating %between% c(1,4),rating_bucket:=3]
  dtlin[nrating %between% c(5,10),rating_bucket:=2]
  dtlin[nrating>10,rating_bucket:=1]
  dtlin[nrating==0,rating_bucket:=0]
  dtlin[is.na(nrating),rating_bucket:=0]
  dtlin[,rating_bucket:=factor(rating_bucket)]
  dtlin
}
bucketytm<-function(dtlin){
  dtlin<-dtlin[!is.na(ytm)]
  dtlin[ytm %between% c(0,3),ytm_bucket:=1]
  dtlin[ytm %between% c(3,7),ytm_bucket:=2]
  dtlin[ytm %between% c(7,10),ytm_bucket:=3]
  dtlin[ytm >10,ytm_bucket:=4]
  dtlin[,ytm_bucket:=factor(ytm_bucket)]
  dtlin
}
bucketytofm<-function(dtlin){
  dtlin<-dtlin[!is.na(ytofm)]
  dtlin[ytofm %between% c(0,3),ytofm_bucket:=1]
  dtlin[ytofm %between% c(3,7),ytofm_bucket:=2]
  dtlin[ytofm %between% c(7,10),ytofm_bucket:=3]
  dtlin[ytofm >10,ytofm_bucket:=4]
  #dtlin[,ytofm_bucket:=factor(ytofm_bucket)]
  dtlin
}


downloadbbg<-function(tickers,filestr=str_c('bbg_',today(),'.RData'),startdt=ymd('1996-01-01'),periodstr='MONTHLY',fieldstr='PX_LAST',splitN=1){
  require(Rblpapi); require(data.table); require(lubridate); require(dplyr)
  if (is.data.table(tickers)) {
    tickers<-tickers[,.(pk)]
  } else{
    tickers<-data.table('pk'=tickers)
  }


  if (tickers[1]=='restart'){ # if ticker=='restart', then restart using temp_bbgdownload_restart.RData file
    if (filestr==str_c('bbg_',today(),'.RData')){ 
      print('restarting from temp_bbgdownload_restart.RData')
      load('temp_bbgdownload_restart.RData')
    } else{
      print(str_c('restarting from ',filestr))
      load(filestr)
    }
    istart<-i
    message('restarting from batch ',i, ' out of total ',splitN)
  } else{ # regular new download
    tickers$batchfactor<-sample(1:splitN,size=nrow(tickers),replace=T)
    tickerslist<-split(as.character(tickers$pk),tickers$batchfactor)
    opt <- c("periodicitySelection"=periodstr)
    istart<-1
    prices<-list()
  }

  con <- Rblpapi::blpConnect()     # automatic if option("blpAutoConnect") is TRUE
  for (i in istart:splitN) {
    print(i)
    if (length(tickerslist[[i]])==0) {error('no tickers error'); browser()}
    tryCatch({
      #if (startdt=='BDP'){
      #  prices[[i]]<-bdp(tickerslist[[i]], fieldstr)  
      #} else{
        prices[[i]]<-bdh(tickerslist[[i]], fieldstr, start.date=startdt, options=opt)
      #}

    }, error=function(err){
        print(err)
        message('Limit Hit on ', i, ' consider restarting download on another machine or another day')
        save(prices,tickers,tickerslist,i,fieldstr,startdt,opt,splitN,filestr,file='temp_bbgdownload_restart.RData')
        #browser()
        blpDisconnect(con)      
        stop(err)
    }
    )
    # will remove this following line later
    save(prices,tickers,tickerslist,i,fieldstr,startdt,opt,splitN,filestr,file=str_c('temp_',filestr))
  }
  blpDisconnect(con)
  save(prices,file=filestr)
  loadBBGdownload2df(filestr)
}

update.prl<-function(prlin,prladd,overridein=FALSE,monthenddates.=monthenddates,diagret=FALSE){
  dtout<-copy(prlin)
  prladd<-copy(prladd)
  if ('pk' %ni% colnames(prladd)) prladd[,pk:=str_c(ticker,' curncy')]
  if ('field' %in% colnames(prlin)) {
    if ('field' %ni% colnames(prladd)) prladd[,field:='PX_LAST']
    keyfield.=c('date','pk','field')
  } else{
    keyfield.=c('date','pk')
  }
  prladd[,pk:=tolower(pk)]
  prladd[!is.na(pk),ticker:=str_extract(tolower(pk),regex('.*(?= curncy)'))]
  prladd<-fixmonthend(monthenddates.,prladd)
  # nextbatchN<-max(na.omit(prl[,batch]))+1;  # prladd[,batch:=nextbatchN]
  dtout<-update.dt(dtout,prladd,keyfield = keyfield.,override=overridein,diagnostic_rt=diagret)
  dtout
}

update.dtl<-function(dtlin,dtladd,overridein=FALSE,diagret=FALSE,monthenddates.=monthenddates){
  dtout<-copy(dtlin)
  dtladd<-copy(dtladd)
  if ('batch' %in% ds(dtlin)){
    nextbatchN<-max(na.omit(dtout[,batch]))+1
    dtladd[,batch:=nextbatchN]
  }
  dtladd[,pk:=tolower(pk)]
  dtladd<-fixmonthend(monthenddates.,dtladd)
  dtout<-update.dt(dtout,dtladd,keyfield = c('date','pk','field'),override=overridein,diagnostic_rt=diagret)
  dtout
}

update.dtl.mo<-function(dtlin,dtladd,overridein=FALSE,diagret=FALSE){
  # a condensed version for only YLD_YTM_MID and w/o regard for monthend
  # can also be applied to daily data
  dtout<-copy(dtlin)
  dtladd<-copy(dtladd)
  if ('field' %in% ds(dtladd)){
    message('resticting field to be only YLD_YTM_MID')
    dtladd<-dtladd[field=='YLD_YTM_MID']
  }
  dtout[,pk:=tolower(pk)]
  dtladd[,pk:=tolower(pk)]
  dtout<-dtout[,.(date,pk,value)]
  dtladd<-dtladd[,.(date,pk,value)]
  
  setkey(dtout,date,pk); setkey(dtladd,date,pk)
  dtout<-update.dt(dtout,dtladd,keyfield = c('date','pk'),override=overridein,diagnostic_rt=diagret)
  dtout
}
update.br<-function(bondref,dtadd,keystr='figi'){
  bondrefout<-copy(bondref)
  dtadd<-copy(as.data.table(dtadd))
  dtadd[!is.na(figi),figiloaded:=1]
  checkcn<-checkcnexist(bondrefout,dtadd)
  if (length(checkcn)!=0) {
    message('no matching col in bondref, not inserted: '); print(checkcn)
    dtadd<-dtadd[,!checkcn,with=F]
  }
  
  dtadd[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")))]
  bondrefout<-update.dt(bondrefout,dtadd,keyfield = keystr )
  bondrefout[,matdiff:=as.numeric((matbbg-mat2)/365)]
  bondrefout
}
update.br2<-function(bondref,dtadd,keystr='figi',diagnostics=F){
  bondrefout<-copy(bondref)
  dtadd<-copy(as.data.table(dtadd))

  dtadd[!is.na(figi),figiloaded:=1]
  checkcn<-checkcnexist(bondrefout,dtadd)
  if (length(checkcn)!=0) {
    message('no matching col in bondref, not inserted: '); print(checkcn)
    dtadd<-dtadd[,!checkcn,with=F]
  }
  
  dtadd[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")))]
  bondrefout<-update.dt(bondrefout,dtadd,keyfield = keystr )
  bondrefout[,matdiff:=as.numeric((matbbg-mat2)/365)]
  bondrefout
}
fixmonthend<-function(dtby=monthenddates,dtfix=dtby){
  #fix monthend date field of dtfix with monthend dates of dtby; return dtfix
  dtby<-copy(dtby)
  dtfix<-copy(dtfix)
  keyoriginal<-key(dtfix)
  if (colnames(dtby)[1]==c('date') & ncol(dtby)==1) 
    monthenddate<-dtby 
  else 
    monthenddate<-dtby[monthend==1,.N,date][N>100,.(date)]
  setkey(monthenddate,date)
  setkey(dtfix,date)
  dtfix[monthenddate,monthend:=1]
  dtfix[!monthenddate,monthend:=0]
  setkeyv(dtfix,keyoriginal)
  dtfix
}

update.dt<-function(dtA,dtB,keyfield='auto',updatefield='auto',insertnewrow=TRUE,override=FALSE,diagnostic_rt=FALSE){
  # this function allows easy addition and update of columns; adding new rows require insertnewrow=TRUE
  # update will override existing values currently; ideally want update on NA, do nothing on records where update is the same as original, and split out differentiated records where original value!=update value
  
  dtA<-copy(dtA)
  dtB<-copy(dtB)
  keyoriginal<-key(dtA)
  conflist<-list()
  if (keyfield[1]=='auto') keyfield<-keyoriginal
  print(str_c('Keyfield: ',paste(keyfield,collapse=' ')))
  setkeyv(dtA,keyfield)
  setkeyv(dtB,keyfield)
  if (updatefield=='auto') {
    fieldA<-ds(dtA)[ds(dtA) %ni% keyfield]
    fieldB<-ds(dtB)[ds(dtB) %ni% keyfield]
    updatefield<-fieldB[fieldB %in% fieldA]
    newfield<-fieldB[fieldB %ni% fieldA]
    lhs<-c(updatefield,newfield)
    rhs<-c(str_c('i.',updatefield),newfield)
    rhs<-rhs[rhs %ni% 'i.']
  }else{
    lhs<-updatefield
    rhs<-str_c('i.',updatefield)
  }
  if (length(lhs)!=length(rhs)){ print('len(lhs)!=len(rhs)');browser()}
  for (j in 1:length(lhs)){
    message(str_c('inserting/updating: lhs: ',lhs[j],' rhs: ',rhs[j]))
    #show warning message first
    problemdt<-dtA[dtB,nomatch=0][eval(exparse(lhs[j]))!=eval(exparse(rhs[j]))]
    if (nrow(problemdt)!=0){
      message('matched but existing value conflicts for field: ',lhs[j])
      #if (override) warning('overriding:') else warning('skiping:')
      #print(problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE])
      
      # if (lhs[j]=='value') { # additionally create a value diff if field is 'value'
      gettypefrom<-problemdt[,lhs[j],with=FALSE][[1]]
      if (is.numeric(gettypefrom) | is.Date(gettypefrom) | is.character(gettypefrom)){
        if (is.numeric(gettypefrom)) 
          problemdt[,valdiff:=eval(exparse(lhs[j]))-eval(exparse(rhs[j]))]
        else if (is.Date(gettypefrom))
          problemdt[,valdiff:=(as.numeric(eval(exparse(lhs[j]))-eval(exparse(rhs[j]))))/365]
        else if (is.character(gettypefrom))
          problemdt[,valdiff:=adist(eval(exparse(lhs[j])),eval(exparse(rhs[j])),ignore.case=TRUE)/str_length(eval(exparse(lhs[j]))),by=1:nrow(problemdt)]
        else
          message('ERROR type of problemdt column')

        confdt<-problemdt[,c(keyfield,lhs[j],rhs[j],'valdiff'),with=FALSE]
        message('\n val diff summary: \n',print_and_capture(summary(confdt$valdiff)))
      } else {
        confdt<-problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE]
      }
      message('\n',print_and_capture(confdt))
      conflist[[length(conflist)+1]]<-confdt
    }

    if (override){
      # override when there are existing value
      tempdt<-dtA[dtB,nomatch=0]
      message(str_c('updated/inserted: ',tempdt[is.na(eval(exparse(lhs[j]))),.N]))
      message(str_c('override: ',tempdt[!is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=eval(exparse(rhs[j])),nomatch=0]
    } else{
      # update/insert if na /override 
      message(str_c('updated/inserted (no override) on ',dtA[dtB,nomatch=0][is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=ifelse(is.na(eval(exparse(lhs[j]))),eval(exparse(rhs[j])),eval(exparse(lhs[j]))),nomatch=0]
    }
    
    
  }
  if (insertnewrow){
    #insert row
    rowinsert<-dtB[!dtA]
    # dtA<-rbind(dtA,rowinsert,fill=TRUE)
    dtA<-bind_rows(dtA,rowinsert) %>% as.data.table()
    message(str_c('Rows inserted: ',rowinsert[,.N]))
  }
  setkeyv(dtA,keyoriginal)
  if (diagnostic_rt){
    # merging the list of conflicting problematic data.tables all together into one single database
    if (length(conflist)>0) dtconflict<-data.table(conflist[[1]]) else dtconflict<-data.table()
    tryCatch({setnames(dtconflict,'valdiff',str_c('diff_',colnames(conflist[[1]])[2]))},error=function(e){})
    if (length(conflist)>1){
      sink("/dev/null")
      print('Constructing conflict table:')
      for (k in 2:length(conflist)){
         dtconflictadd<-as.data.table(conflist[[k]])
         tryCatch({setnames(dtconflictadd,'valdiff',str_c('diff_',colnames(dtconflictadd)[2]))},error=function(e){})
         dtconflict<-update.dt(dtconflict,dtconflictadd,keyfield=keyfield,diagnostic_rt=F)
      }



      sink();sink();
        dtconflict[,Nconf:=(sum(!is.na(.SD))-1)/3,by=1:nrow(dtconflict)]
    }

    list('dtout'=dtA,'dtconflicts'=dtconflict)
  } else {
    dtA  
  }
  
}

exparse<-function(strin){parse(text=strin)}
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

checkcnexist<-function(dtold,dtadd){
# check which colnames in dtadd do not exists in dtold
  cnexist<-dtold %>% ds()
  cnadd<-dtadd %>% ds()
  cnadd[cnadd %ni% cnexist]
}
missSummary<-function(dtin){
  cn<-dtin %>% ds()
  misslist<-list()
  for (cni in cn) 
    misslist[[length(misslist)+1]]<-data.table(vname=cni,missN=dtin[is.na(eval(exparse(cni))),.N])
  missdt<-rbindlist(misslist)
  missdt[,total:=dtin[,.N]][,misspct:=missN/total]
  print(missdt)
  missdt
}
tsdiff<-function(dtin){
  # simply generate diff=i.var-var for all i.var
  dtin<-copy(dtin)
  cn<-dtin %>% ds()
  cn<-cn[cn %like% '^i\\.']
  for (cni in cn){
    cnis<-str_sub(cni,3)
    dtin %<>% mutate_(str_c('diff_',cnis,'=',cni,'-',cnis))
  }
  dtin
}
readkey <- function(){
  cat ("Press [enter] to continue")
  line <- readline()
}
readkeygraph <- function(prompt){
  getGraphicsEvent(prompt = prompt, 
                   onMouseDown = NULL, onMouseMove = NULL,
                   onMouseUp = NULL, onKeybd = onKeybd,
                   consolePrompt = "[click on graph then follow top prompt to continue]")
  Sys.sleep(0.01)
  return(keyPressed)
}

onKeybd <- function(key){
  keyPressed <<- key
}

ggplotw.comp<-function(dtin){
  #X11()
  X11(width=15,height = 9)
  print(dtin[,.(date,eur,i.eur)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]") 
  print(dtin[,.(date,gbp,i.gbp)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,jpy,i.jpy)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,aud,i.aud)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,chf,i.chf)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
  keyPressed = readkeygraph("[press any key to continue]")
  print(dtin[,.(date,cad,i.cad)] %>% ggplotw())
  dev.flush();flush.console();Sys.sleep(1)
}
ggplotw.comp2<-function(dtA,dtB,labout=c('Main Result','Additional Controls')){
  require(gridExtra)
  dtp<-dtA[dtB]   
  fig<-list()
  fig[[length(fig)+1]]<- dtp[,.(date,eur,i.eur)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('eur')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))+scale_color_discrete('',labels = labout)+scale_linetype_discrete('',labels = labout)
  fig[[length(fig)+1]]<- dtp[,.(date,gbp,i.gbp)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('gbp')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))
  fig[[length(fig)+1]]<- dtp[,.(date,jpy,i.jpy)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('jpy')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))
  fig[[length(fig)+1]]<- dtp[,.(date,aud,i.aud)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('aud')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))
  fig[[length(fig)+1]]<- dtp[,.(date,chf,i.chf)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('chf')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))
  fig[[length(fig)+1]]<- dtp[,.(date,cad,i.cad)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+geom_hline(yintercept=0,colour='lightblue')+ggtitle('cad')+theme_few()+theme(legend.position='none')+xlab('')+ylab('')+scale_x_date(breaks=scales::pretty_breaks(n=7))
  legendcommon<-get_legend(fig[[1]]+theme(legend.position='bottom'))
  fig.all<-grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]],fig[[5]],fig[[6]],legendcommon,ncol=3,nrow=3,layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,7,7)),heights=c(2.25,2.25,.25))
  fig.all
}

dt2clip = function(x,sep="\t",col.names=T,...) { 
  write.table(x
             ,file = pipe("pbcopy")
             ,sep=sep
             ,col.names = col.names
             ,row.names = F
             ,quote = F,...)
}
get.dt.class<-function(dtin){
  data.table('columnname'=ds(dtin),'classtype'=unlist(lapply(dtin,class)))
}

cleanup.sdc.by.col<-function(dtsdc){
  # this function cleans up dtsdc by column, replaceing '-'  with NA and get rid of factor columns
  dtsdc<-copy(dtsdc)
  dtclass<-get.dt.class(dtsdc)
  dtsdc<-subset(dtsdc,select=dtclass[classtype!='factor',columnname])
  dtclass<-get.dt.class(dtsdc)
  dtclass[,rowN:=.I]
  for (k in 1:nrow(dtclass)){
    if(dtclass[rowN==k,classtype=='character']){
      cname<-dtclass[rowN==k,columnname]
      dtsdc[,eval(exparse(cname)):=str_trim(eval(exparse(cname)))]
      dtsdc[eval(exparse(cname)) %in% c('','-','N/A','TBA','NA','na','n/a'),eval(exparse(cname)):=NA]
    }
  }
  dtsdc
}

CUSIPcheck <- function(x){
  #check cusip number
  # cusip9.to.check<-cusip9.to.get[str_length(cusip9)==9]
  # cusip9.to.check[,digit9:=CUSIPcheck(str_sub(.SD[,cusip9],0,8)),by=1:nrow(cusip9.to.check)]
  # cusip9.to.check[str_sub(.SD[,cusip9],9)!=digit9]
  if(nchar(x)!=8){stop("Provided CUSIP does not have length 8")}
  v <- unlist(strsplit(x,""))
  if(any(v %in% LETTERS)){
  v[which(v %in% LETTERS)] <- which(LETTERS %in% v[which(v %in% LETTERS)])+9
  v <- as.numeric(v)
  }else{v <- as.numeric(v)}
  out <- 0
  for(i in 1:8){
  if(i%%2==0){v[i]<-v[i]*2}
  out <- out + v[i]%/%10 + v[i]%%10
  }
  (10-(out%%10))%%10  
}

  checkcusipdt<-function(cusipin.){
  #wraps CUSIPcheck; name cusip column cusip9
  cusipin<-copy(cusipin.)
  cusipin[,cusip8:=str_sub(cusip9,1,8)]
  tocheck<-cusipin[str_length(cusip8)==8 & !is.na(cusip9)]
  tocheck[,error:=0]
  tocheck[,checked:=0]
  for(i in 1:nrow(tocheck)){
    digit9=CUSIPcheck(tocheck[i,cusip8])

    if ((str_sub(tocheck[i,cusip9],9)!=digit9)) {
      print(i)
      tocheck[i,error:=1]
    }
    tocheck[i,checked:=1]
  }
  tocheck
  }
show.dupe.col<-function(dtin){
  # iterate through the columns of dtin and show only duplicated asOneSidedFormula
  dup.col<-data.table('col'=1:ncol(dtin),'dupe'=0)
  for (colq in 1:ncol(dtin))  {
    # if(anyDuplicated(dtin[,colq,with=F])==0) # colq is not the same
    if((dtin[,colq,with=F] %>% duplicated() %>% not()  %>% sum())>1)
      dup.col[col==colq,dupe:=1]
  }
  dtin[,dup.col[dupe==1,col],with=F]
}

compare.dt<-function(A,B, bykey.=key(A),mask=T,unique.=T){
  A<-copy(A)
  B<-copy(B)
  A %>% setkeyv(bykey.); B %>% setkeyv(bykey.)
  if (unique.){
    A<-unique(A)
    B<-unique(B)
  }
  Acount<-A[,.N,bykey.][,.N]; print(str_c('A N.= ',Acount))
  Bcount<-B[,.N,bykey.][,.N]; print(str_c('B N.= ',Bcount))
  AB.intersect<-A[B,nomatch=0]; print(str_c('AB intersect N.= ',AB.intersect[,.N]))
  AnotB <- A[!B]; print(str_c('A not in B N.= ',AnotB[,.N]))
  BnotA <- B[!A]; print(str_c('B not in A N.= ',BnotA[,.N]))
  if (mask) 
    return(NULL)
  else
    list('AB'=AB.intersect,'AniB'=AnotB,'BniA'=BnotA,'A.N'=Acount,'B.N'=Bcount,'AB.N'=AB.intersect[,.N],'AniB.N'=AnotB[,.N],'BniA.N'=BnotA[,.N])
}

fread.xls<-function(path,sheet.=excel_sheets(path)){
  require('readxl')
  dt.out<-rbindlist(lapply(sheet., function(x,path){
    dt.sheet<-(read_excel(path,x) %>% as.data.table())
    dt.sheet[,sheet:=x]
    dt.sheet
  },path),fill=T) 
  dt.out
}

nonmarket.dates<-function(dtl,bondref){
  #retrieve nonmarket dates for daily time series based on holiday schedule and consecutive observation drop
  dtl<-copy(dtl)
  br<-copy(bondref[!is.na(pk),.(ccy,mat2,nrating,upcusip,pk,ytofm)])
  br[,ccy:=tolower(ccy)]
  setkey(dtl,pk); setkey(br,pk)
  dtl<-dtl[br]

  load('db/holidaycalendar.RData')
  holidays<-data.table('date'=unique(c(cdrus,cdreu)))
  setkey(holidays,date)
  setkey(dtl,date)
  dtl<-dtl[!holidays][date<'2016-07-26']
 # consecutive obs drop 
  dtl.count<-(dtl[ccy %in% c('usd','eur')][,.N,.(date,ccy)][N>5] %>% dcast.data.table(date~ccy,value.var='N'))[order(date)]   
  dtl.count[,usd.L:=lag(usd)]
  # dtl.count[,usd.L:=mean(lag(usd),lag(usd),lag(usd),lag(usd),lag(usd))]
  dtl.count[usd<.90*usd.L,sharpobsdrop:=1][is.na(sharpobsdrop),sharpobsdrop:=0]
  drop.dates<-rbind(dtl.count[sharpobsdrop==1,.(date)],holidays)
  drop.dates %>% setkey(date)
  drop.dates<-drop.dates %>% unique(.)
  #dtl.count<-dtl.count[sharpobsdrop==0]
  drop.dates
}


  foreachsplit<-function(dtin.,by,func.,cores=1){
    # untested/incomplete
    # currently only support singular by statement; such as by='date'; func. is a string, such as func.='function(x) mean(x)'
    dtin<-copy(dtin.)
    registerDoParallel(cores)
    setkey(dtin ,by)
    indx<-split(seq(nrow(dtin)),dtin[,.(eval(exparse(by)))])
    out_list <- foreach(i = indx, .packages = c('data.table') ) %dopar% {
      Psubset<-dtin[i,]
      dtsubsetout<-eval(exparse(func.))
      dtsubsetout[,eval(exparse(by)):=Psubset[1,.(eval(exparse(by)))]]
      dtsubsetout
    }
    dtout <- rbindlist(out_list,use.names=TRUE) #, fill=TRUE, etc.
  }
## Regressions
###

neweymod<-function(dtin,formula,outformat=0,value.name=''){
  #########NOT SUITABLE FOR PANEL NEWEY Regressions
  # MATCHING STATA EXACTLY
    model <- lm(formula, data=dtin)
    #print(formula)
    coefout<-coeftest(model, NeweyWest(model,prewhite=F,adjust=T))    # Newey-West '94 auto-lag
    nlag <-floor(bwNeweyWest(model)) # provides number of lags used by Newey-West '94
    #print(coefout);
    dtout<-(coefout %>% as.data.frame.matrix() %>% as.data.table(keep.rownames=T) %>% melt('rn'))[variable %like% 'Estimate|value'][rn=='(Intercept)',rn:='const'][order(rn)]
    
    dtout[variable %like% 'Estimate',valuestr:=sprintf("%.3g",value)]
    dtout[variable %like% 'value',valuestr:=sprintf("[%.2f]",value)]
    dtout<-rbind(dtout,data.table('rn'='N','valuestr'=as.character(nrow(dtin))),fill=T)
    dtout<-rbind(dtout,data.table('rn'='R2','valuestr'=sprintf("%.2f",summary(model)$r.squared)),fill=T)
    dtout<-rbind(dtout,data.table('rn'='Lags','valuestr'=as.character(nlag)),fill=T)
    dtout[,value:=valuestr][,valuestr:=NULL]
    
    # setting ordering of variables
    dtout[,order:=1]
    dtout[rn=='const',order:=2]
    dtout[rn=='N',order:=3]
    dtout[rn=='R2',order:=4]
    dtout[rn=='Lags',order:=5]
    
    if(value.name!='') dtout[,varname:=value.name] #setnames(dtout,'value',value.name) #colname label
    if (outformat==0) {
      dtout
    } else if (outformat==1) {
      coefout
    } else if (outformat==2){
      list('res'=coefout,'lag'=nlag,'model'=model)
    }
  }
reg.newey.all<-function(dtreg.,formula.){
  result<-list()
  for (iccy in c('eur','gbp','jpy','aud','chf','cad')){
    result[[length(result)+1]]<-dtreg.[ccy==iccy] %>% neweymod(formula.,value.name=iccy)
  }
  result<-(rbindlist(result) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  result[,.(rn,eur,gbp,jpy,aud,chf,cad)] #%T>% dt2clip
}
reg.newey.all2<-function(dtreg.,formula.){
  result<-list()
  for (iccy in dtreg.[,.N,ccy]$ccy){
    result[[length(result)+1]]<-dtreg.[ccy==iccy] %>% neweymod(formula.,value.name=iccy)
  }
  result<-(rbindlist(result) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
  result[] #%T>% dt2clip
}


regformat<-function(dtin,formula,value.name='',setype='HC0',regtype='felm'){
  # generic regression format
  if (regtype=='felm'){
    modeltemp<-dtin %>% felm(eval(exparse(formula)),data=.)
    dtout<-(summary(modeltemp)$coefficients %>% as.data.table(keep.rownames=T) %>% melt('rn'))[variable %like% 'Estimate|value'][rn=='(Intercept)',rn:='const'][order(rn)]
  } else{
    modeltemp<-dtin %>% lm(formula,.)
    dtout<-(coeftest(modeltemp, vcovHC(modeltemp, type=setype)) %>% as.data.frame.matrix() %>% as.data.table(keep.rownames=T) %>% melt('rn'))[variable %like% 'Estimate|value'][rn=='(Intercept)',rn:='const'][order(rn)]
  }
  dtout[variable %like% 'Estimate',valuestr:=sprintf("%.3g",value)]
  dtout[variable %like% 'value',valuestr:=sprintf("[%.2f]",value)]
  dtout<-rbind(dtout,data.table('rn'='rsq','valuestr'=sprintf("%.2g",summary(modeltemp)$r.squared)),fill=T)
  dtout<-rbind(dtout,data.table('rn'='n','valuestr'=as.character(nrow(dtin))),fill=T)
  dtout[,value:=valuestr][,valuestr:=NULL]
  dtout[,order:=1]
  dtout[rn=='const',order:=2]
  dtout[rn=='rsq',order:=3]
  dtout[rn=='n',order:=4]
  if(value.name!='') dtout[,varname:=value.name] #setnames(dtout,'value',value.name) #colname label
  dtout
}
regformatcoef<-function(dtin,reg1,res1,value.name='reg',lags=0){
  dtrestemp <- ((res1 %>% as.data.frame.matrix() %>% as.data.table(keep.rownames=T))[rn %nlk% '^as.factor'] %>% melt('rn'))[variable %like% 'Estimate|value'][rn=='(Intercept)',rn:='const'][order(rn)]
  dtrestemp[variable %like% 'Estimate',valuestr:=sprintf("%.3g",value)]
  dtrestemp[variable %like% 'value',valuestr:=sprintf("[%.2f]",value)]
  if('const' %ni% dtrestemp$rn){
    dtrestemp<-rbind(dtrestemp,data.table(rn='const','valuestr'=''),fill=T)
    dtrestemp<-rbind(dtrestemp,data.table(rn='const','valuestr'=''),fill=T)
  }
  dtrestemp<-rbind(dtrestemp,data.table(rn='N','valuestr'=sprintf("%i",nrow(dtin))),fill=T)
  dtrestemp<-rbind(dtrestemp,data.table(rn='R2','valuestr'=sprintf("%.2f",summary(reg1)$r.squared['rsq'])),fill=T)
  dtrestemp<-rbind(dtrestemp,data.table(rn='Lags','valuestr'=sprintf("%i",lags)),fill=T)
  dtrestemp<-dtrestemp[,.(rn,valuestr)]
  dtrestemp %>% setnames('valuestr',value.name)
  dtrestemp
}
regout<-function(regres.,statname='t value'){
  regsum<-summary(regres.)
  dtout<-as.data.table(regsum$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  dtout[,`t value`:=sprintf("[%.2f]",`t value`)]
  dtout[,Estimate:=sprintf('="%.3g"',Estimate)]
  dtout2<-(dtout %>% melt(id.vars='rn',measure.vars=c('Estimate',statname)))[order(-rn)] 
  dtout3<-rbind(dtout2,data.table('rn'='Rsq', 'variable'='', 'value'=sprintf("%.2f",sum(regsum$r.squared))))
  dtout4<-rbind(dtout3,data.table('rn'='N', 'variable'='', 'value'=sum(regsum$df[1:2])))
  dtout4 %>% dt2clip
  dtout4
}

regoutz<-function(regres.,statname='z value'){
  regsum<-summary(regres.)
  dtout<-as.data.table(regsum$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  dtout[,`z value`:=sprintf("[%.2f]",`z value`)]
  dtout[,Estimate:=sprintf('="%.3g"',Estimate)]
  dtout2<-(dtout %>% melt(id.vars='rn',measure.vars=c('Estimate',statname)))[order(-rn)] 
  dtout3<-rbind(dtout2,data.table('rn'='Rsq', 'variable'='', 'value'=sprintf("%.2f",sum(regsum$r.squared))))
  dtout4<-rbind(dtout3,data.table('rn'='N', 'variable'='', 'value'=sum(regsum$df[1:2])))
  dtout4 %>% dt2clip
  dtout4
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
table.summary<-function(dtin.sum){
  summarylist<-list()
  summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000)][,.(field='all',cat='',nbonds,notional)]
  summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)][order(-notional)]
  summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][order(-rating_bucket)][,.(field='rating',cat=rating_bucket,nbonds,notional)]
  summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][order(ytofm_bucket)][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
  summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),sicind][order(sicind)][,.(field='ind',cat=sicind,nbonds,notional)]
  sum2<-rbindlist(summarylist)
  sum2 
}

require(data.table)