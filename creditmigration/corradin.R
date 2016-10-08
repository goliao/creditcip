# corradin.R
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
# setwd('/mnt/disks/xccy/creditmigration')
rm(list=ls(all=TRUE));
source('util.R')
load('db/bondrefall.RData')
load('db/prl.RData');load('db/monthenddates.RData');
corradin.br<-fread('corradin/corradin_gov_pk.csv')[pk!=''][,d:=dmy(d)][,mat2:=dmy(mat2)];

bondrefall %>% setkey(isin)
corradin.br %>% setkey(isin)
aa<-bondrefall[corradin.br,nomatch=0];aa[,.N]
aa

govdtl.mo1<-loadBBGdownload2df('corradin/corradin.RData')
corradin.br[,nrating:=0]
corradin.br[,ytofm:=(as.numeric(mat2)-as.numeric(d))/365]
corradin.br[,upcusip:=modnat]
preprocess.gov<-function(bondref,dtl,prl,ccyfilter=c('usd','eur')){
  tic()
  dtl<-copy(dtl)
  prl<-copy(prl)
  prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
  br<-bondref[!is.na(pk)]
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
  
  dtl2<-dtl[br[,.(ccy,mat2,amt,coupon,modnat,isin,pk,ytofm,nrating,upcusip)],nomatch=0]
  dtl2[,ytm:=as.numeric((mat2-date)/365)]
  dtl3<-dtl2[ytm >.05]
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='YLD_YTM_MID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  prl<-prl[date>'2002-01-01' & wday(date) %between% c(2,6)]
  prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
  prw<-prl %>% distinct() %>% data.table::dcast.data.table(.,date~ticker,value.var = 'value')
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.05,1.0)]
  dtl3[liq<.5,liq_bucket:=0] # more illiq
  dtl3[liq>=.5,liq_bucket:=1] # liq
  # gen eusw=eusa-eubsv
  # gen eusz=eusw+eubs
  prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
  prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
  
  # transform prw back to prl
  prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
  prl<-prl[!is.na(value)]
  #browser()
  dtl4<-dtl.addswapsprd(dtl3,prl)
  toc()
  list('prw'=prw,'prl'=prl,'dtl4'=dtl4,'br'=br)
}


dtm<-preprocess.gov(corradin.br,govdtl.mo1,prl)


dtm$dtl4
dtl<-dtm$dtl4[upcusip!='Turkey']
ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=1,returndt=T,parallel.core. = 1,winsor. = 0,globaluponly = 0)

dtl[,datefac:=factor(date)]
dtl[ccy=='usd',ccy:="1usd"]
aa<-dtl %>% felm(swapsprd~ccy*datefac,.)
bb<-summary(aa)$coefficients %>% as.data.table(keep.rownames=T)
cc<-bb[rn %like% '^ccyeur:',.(date=ymd(str_sub(rn,15)),eur=Estimate+bb[rn=='ccyeur',Estimate])]
cc %>% ggplotw()

dtl[,.N,upcusip]
dd<-dtl[,.(crd=mean(swapsprd),netdev=mean(swapsprdadj)),.(date,ccy)]
ee<-(dd %>% dcast(date~ccy,value.var='crd'))[,.(date,c=eur-`1usd`)]
ee %>% ggplotw

#net after adj cip
dd<-dtl[,.(crd=mean(swapsprd),netdev=mean(swapsprdadj)),.(date,ccy)]
ee<-(dd %>% dcast(date~ccy,value.var='netdev'))[,.(date,c=eur-`1usd`)]
ee %>% ggplotw

#crd by nat
dd<-dtl[,.(crd=mean(swapsprd),netdev=mean(swapsprdadj)),.(date,ccy,upcusip)]
ee<-(dd %>% dcast(date+upcusip~ccy,value.var=c('crd','netdev')))[,.(date,upcusip,c=crd_eur-crd_1usd,netdev=netdev_eur-netdev_1usd)]
ee[,cip:=c-netdev]
ff<-ee %>% melt(id.vars=c('date','upcusip'))

fig1[[length(fig1)+1]]<-ee[upcusip=='Austria'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()
fig1[[length(fig1)+1]]<-ee[upcusip=='Belgium'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()
fig1[[length(fig1)+1]]<-ee[upcusip=='Finland'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()
fig1[[length(fig1)+1]]<-ee[upcusip=='Italy'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()
fig1[[length(fig1)+1]]<-ee[upcusip=='Spain'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()
fig1[[length(fig1)+1]]<-ee[upcusip=='Turkey'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

zz<-ff[variable=='c' | variable=='cip'][upcusip=='Austria'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Austria')+xlab('')+ylab('')+scale_color_discrete('',labels=c('-c (crdsprddiff)','-b (cip basis)'))+theme(legend.position='bottom')
zz
legendcommon<-get_legend(zz)
fig1<-list()
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Austria'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Austria')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Belgium'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Belgium')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Finland'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Finland')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Italy'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Italy')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Spain'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Spain')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
fig1[[length(fig1)+1]]<-ff[variable=='c' | variable=='cip'][upcusip=='Turkey'] %>% ggplot(aes(x=date,y=-value,colour=variable))+geom_line()+ggtitle('Turkey')+xlab('')+ylab('')+theme_classic()+scale_color_discrete('',c('-c','-b'))+scale_x_date(breaks=scales::pretty_breaks(n=7))
require('gridExtra')
fig1all<-grid.arrange(fig1[[1]],fig1[[2]],fig1[[3]],fig1[[4]],fig1[[5]],fig1[[6]],legendcommon,ncol=2,nrow=4,heights=c(3,3,3,.2))
ggsave(fig1all, width=8,height=9.2,filename='../paper/figures/govie.pdf')

ff[variable=='c' | variable=='cip'] %>% ggplot(aes(x=date,y=-value,colour=upcusip))+geom_line()

