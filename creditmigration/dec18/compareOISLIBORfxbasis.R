## compares OIS basis with libor FX basis
## currently only for EUR, GBP, JPY
## missing jybsf5
setwd('C:/Users/Gordon/Dropbox/Research/ccy basis/creditmigration/dec18/')
rm(list=ls(all=TRUE));
load('../db/dtlmo.rdata');load('../db/bondref.RData');load('../db/prl.RData');load('../db/monthenddates.RData');
load('../db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('../util.r')


oisfx <- fread('../oisxcbasis_icpl.csv') %>% melt(id.vars='date',variable.name='ticker') %>% na.omit()
oisfx[,date:=mdy(date)]
#oisfx[,ticker:=str_extract(ticker,'^\\w+')]
#oisfx %>% setnames('px_last','value')
oisfx[ticker=='eousff5']

oisfx[,.N,ticker]

oisfx[,ccy:=str_extract(ticker,'\\w')]

oisfx[,tenor:=str_extract(ticker,'\\d+$')]

liborfx <- prl[ticker %like% '^jybs\\d|^eubs\\d|^bpbs\\d']

liborfx[,ccy:=str_extract(ticker,'\\w')]
liborfx[,tenor:=str_extract(ticker,'\\d+$')]



fxbs <- merge(oisfx[,.(date,ccy,tenor,value)],liborfx[,.(date,ccy,tenor,value)],by=c('date','ccy','tenor'),all.x=T,all.y=T,suffixes = c('ois','libor'))


fxbsl <- fxbs[date>=ymd('2007-01-01') & date<=ymd('2015-01-01')] %>% melt(id.vars=c('date','ccy','tenor'))


fxbsl[,type:=toupper(str_sub(variable,6))]

figeur <- fxbsl[ccy=='e' & tenor==1] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()+theme_few()+ylab('basis points')+theme(legend.position = 'bottom')+scale_color_discrete("")
figgbp <- fxbsl[ccy=='b' & tenor==5] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()+theme_few()+ylab('basis points')+theme(legend.position = 'bottom')+scale_color_discrete("")
figjpy <- fxbsl[ccy=='j' & tenor==5] %>% ggplot(aes(x=date,y=value,colour=type))+geom_line()+theme_few()+ylab('basis points')+theme(legend.position = 'bottom')+scale_color_discrete("")


ggsave(figeur,file='../../figures/OISxcBasis_EUR.pdf',height=3,width=5,units = 'in')
ggsave(figgbp,file='../../figures/OISxcBasis_GBP.pdf',height=3,width=5,units = 'in')
ggsave(figjpy,file='../../figures/OISxcBasis_JPY.pdf',height=3,width=5,units = 'in')

# 
fxbsl[,.N,tenor] 
fxbsl[ccy=='e' & tenor==1] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
# 
# fxbsl[ccy=='b' & tenor==1] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
# 
# fxbsl[ccy=='j' & tenor==1] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
liborfx[ticker=='eubsc']

dt <- prl[ticker %in% c('eubsc') & date>ymd(20080101),.(date,value=-value)]

dt[date<=ymd(20100201)] %>% ggplot(aes(x=date,y=value))+geom_line()+theme_few()+ylab('basis points')+geom_vline(xintercept = ymd(20100201))+geom_vline(xintercept = ymd(20080929))

prl[ticker %in% c('eubsc') & date>ymd(20080101) & date<ymd(20160101),.(date,value=-value)] %>% ggplot(aes(x=date,y=value))+geom_line()+theme_few()+ylab('basis points')+geom_vline(xintercept = ymd(20111130))#+geom_vline(xintercept = ymd(20080929))
# 
# 
# 
# 
