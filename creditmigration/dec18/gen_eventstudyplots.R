setwd('..')
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
load('dailyregrun.RData')
ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))


### all ccy daily
ys3<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=4,returndt = T)
# save(ys3,file='dailyts_allccy_clean_831_isstype4.RData')

load('dtmd160912.RData')
load('dailyts_eu_clean_831_isstype4.RData')
load('dailyts_allccy_clean_831_isstype4.RData')
load('dailyregrun.RData')
source('util.r')
dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)

# merge issuance data
dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
dtiss %>% setkey(year,month)
monthend2 %>% setkey(year,month)
# dtiss<-dtiss[monthend2][,date:=i.date]
dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]



plot.event.study<-function(dtin, event.dates,type.=1){
  labtext=c('CIP deviation','Residualized credit spread differential')
  if (type.==1)
    dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = labtext)+scale_linetype_discrete('',labels = labtext)+theme_few()+theme(legend.position='bottom')
  else if(type.==2) # plot cipest as well
    dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','cipest','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates))+geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.1) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','cipest','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','cipest','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
  else if(type.==3) # use ribbon instead
    dtin %>% melt(.,id.vars=c('date','ccy','se','cimin','cimax'),measure.vars=c('cip','credit')) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_vline(xintercept = as.numeric(event.dates)) +geom_ribbon(aes(ymin=cimin,ymax=cimax),alpha=.3,colour=NA) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP','Credit Spread Diff.'))+scale_linetype_discrete('',labels = c('CIP','Credit Spread Diff.'))+theme_classic()+theme(legend.position='bottom')
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
   # ggsave(filename=str_c(filepathhead,'_B.pdf'),width=9,height=3,plot=figb)
    ggsave(filename=str_c(filepathhead,'_C.pdf'),width=9,height=3,plot=figc)
  }
  figab
}


# US credit crisis
plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
# plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='../paper/figures/eventstudy_creditcrunch',datetics=7)
# slightly longer history and equally interesting:
plot.event.study.moiss2(dtin.in=dt.merged,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-07-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)

# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='')#../paper/figures/eventstudy_eusovereigncrisis


# Long-term vs short-term CIP deviation
cipeursl<-prl[ticker %in% c('eubsc','eubs5') & date>ymd('2007-01-01'),.(date,ticker,value)]
Labels=c('5y', '3m'); Lcolor=c('black','red'); Ltype=c('solid','solid');
cipeursl %>% ggplot(aes(x=date,y=value,colour=ticker))+geom_line()+theme_few()+xlab('')+ylab('basis points')+scale_x_date(breaks=scales::pretty_breaks(n=5))+scale_y_continuous(breaks=scales::pretty_breaks(n=5))+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+geom_vline(xintercept = ymd('2011-05-01'))+geom_vline(xintercept = ymd('2012-06-01'))
ggsave('../../figures/cipeur3m5y.pdf',width=8,height=3)

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


