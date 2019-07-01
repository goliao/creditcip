## CDS

#rm(list=ls(all=TRUE));
source('util.r')

fp='C:\\Users\\Gordon\\Dropbox\\Research\\ccy basis\\data\\markitcds\\cds.csv'

df=fread(fp)
colnames(df)<-str_to_lower(colnames(df))


df[,date:=lubridate::as_date(date)]

sprd=df[ccy %in% c('USD','EUR','JPY','AUD','CAD','CHF','GBP')] %>% unique(by=c('date','ticker','tier','docclause','ccy'))
sprd=sprd[!is.na(spread5y)]; sprd[,value:=spread5y*10000]
# in September 14', all doc caluse switched over to XX14 (small bang); use the XX14 version whenever available
sprd<-sprd[order(ticker,redcode,date,tier,ccy,-docclause)]
sprd[date>=ymd('2014-09-22') & docclause=='MM14',docclause:='MM']
sprd[date>=ymd('2014-09-22') & docclause=='MR14',docclause:='MR']
sprd[date>=ymd('2014-09-22') & docclause=='XR14',docclause:='XR']
sprd[date>=ymd('2014-09-22') & docclause=='CR14',docclause:='CR']
sprd<-sprd %>%  unique(by=c('date','ticker','tier','docclause','ccy'))



sprdw=sprd %>% dcast(date+ticker+tier+docclause~ccy,value.var='value')

# generate spreads
sprdw[,EUUS:=EUR-USD][,JPUS:=JPY-USD][,AUUS:=AUD-USD][,CHUS:=CHF-USD][,CAUS:=CAD-USD][,GBUS:=GBP-USD]

sprdw[!is.na(EUR) & !is.na(USD) & year(date)>2010,.(eur=mean(EUR),usd=mean(USD)),.(date)] %>% ggplotw()
sprdw[!is.na(EUR) & !is.na(USD) & year(date)>2010 & ticker=='AKZO',.(eur=mean(EUR),usd=mean(USD)),.(date)] %>% ggplotw()
sprdw[!is.na(EUR) & !is.na(USD) & year(date)>2010,.N,ticker] %>% head(20)

sprdw<-sprdw[,.(date,ticker,tier,docclause,EUUS,JPUS,AUUS,CHUS,CAUS,GBUS)]

# convert to monthly
MONTHENDS=sprdw[,.(monthend=max(date)),.(floor_date(date,unit = 'month'))][,monthend]
sprdw=sprdw[date %in% MONTHENDS]
sprd<-sprd[date %in% MONTHENDS]


sprd %>% head()
# summary stat
sprd[,.N,rating5y]
# # regression analysis of does currency mater
# sprd[,datef:=as.factor(date)]
# sprd[ccy=='USD',ccy:='1USD']
# sprd %>% colnames()
# reg=sprd[] %>% felm(value~ccy|datef+ticker+docclause+tier|0|datef+ticker,data=.)
# summary(reg)
# 
# reg=sprd[docclause=='MR'] %>% felm(value~ccy|datef+ticker+tier|0|datef+ticker,data=.)
# summary(reg)
# 
# reg=sprd[docclause=='MR' & tier=='SNRFOR'] %>% felm(value~ccy*datef|0|0|datef+ticker,data=.)
# summary(reg)
#############

## Plot
#######################
# Eur vs USD
Labels=c('10%','Mean','Median','90%'); Ltype=c('dashed','solid','solid','twodash'); Lcolor=c('darkblue','black','red','darkgreen')
ploteur<-sprdw[date<ymd('2016-07-01'),.(eu10=quantile(na.omit(EUUS),c(.1)),eu=mean(na.omit(EUUS)),eu50=quantile(na.omit(EUUS),c(.5)),eu90=quantile(na.omit(EUUS),c(.9))),.(date)] %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+xlab('date')+ylab('basis points')+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+ggtitle('CDS(EUR)-CDS(USD)')
#ggsave('../paper/figures/cds_EU.pdf',width=5)
# JPY vs USD
plotjpy<-sprdw[date<ymd('2016-07-01'),.(jp10=quantile(na.omit(JPUS),c(.1)),jp=mean(na.omit(JPUS)),jp50=quantile(na.omit(JPUS),c(.5)),jp90=quantile(na.omit(JPUS),c(.9))),.(date)]  %>% melt(id.vars='date') %>% ggplot(aes(date,value))+geom_line(aes(linetype=variable,colour=variable))+xlab('date')+ylab('basis points')+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)+theme_few()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+ggtitle('CDS(JPY)-CDS(USD)')
#ggsave('../paper/figures/cds_JP.pdf')
require('gridExtra')
figboth<-grid.arrange(ploteur,plotjpy,ncol=2,nrow=1)#,layout_matrix=rbind(1,2),width=c(2.25,2.25))

ggsave('../../paper/figures/cds.pdf',plot=figboth,width=8,units='in')
# number of CDS contracts
sprdw[date<ymd('2016-07-01'),.(Ncds_EU=length(na.omit(EUUS)),Ncds_JP=length(na.omit(JPUS))),date] %>% summary()

# number of firms
sprdw[date<ymd('2016-07-01') & !is.na(EUUS),.(Nfirm=length(unique(ticker))),.(date)] %>% summary()
sprdw[date<ymd('2016-07-01') & !is.na(JPUS),.(Nfirm=length(unique(ticker))),.(date)] %>% summary()

sprdw[date<ymd('2016-07-01') & !is.na(EUUS),.(Nfirm=length(unique(ticker))),.(date)] %>% ggplotw()
sprdw[date<ymd('2016-07-01') & !is.na(JPUS),.(Nfirm=length(unique(ticker))),.(date)] %>% ggplotw()


# 
# ##################
# sprdw[date<ymd('2016-07-01'),.(eu=mean(na.omit(EUUS)),jp=mean(na.omit(JPUS))),.(date)] %>% ggplotw()
# 
# ## MEAN CDS CURRENCY SPREAD
# sprdw[date<ymd('2016-07-01') & tier=='SNRFOR' & docclause %in% c('MR'),.(eu=mean(na.omit(EUUS)),jp=mean(na.omit(JPUS)),ch=mean(na.omit(CHUS)),gb=mean(na.omit(GBUS)),ca=mean(na.omit(CAUS),au=mean(na.omit(AUUS)))),.(date)] %>% ggplotw()
# sprdw[date<ymd('2016-07-01') & tier=='SNRFOR' & docclause %in% c('MR'),.(eu=mean(na.omit(EUUS)),jp=mean(na.omit(JPUS))),.(date)] %>% ggplotw()
# sprdw[date<ymd('2016-07-01'),.(eu=mean(na.omit(EUUS)),jp=mean(na.omit(JPUS))),.(date)] %>% ggplotw()
# 
# sprdw[date<ymd('2016-07-01'),.(eu10=quantile(na.omit(EUUS),c(.1)),eu=mean(na.omit(EUUS)),eu50=quantile(na.omit(EUUS),c(.5)),eu90=quantile(na.omit(EUUS),c(.9))),.(date)] %>% ggplotw()
# 
# sprdw[date<ymd('2016-07-01'),.(jp10=quantile(na.omit(JPUS),c(.1)),jp=mean(na.omit(JPUS)),jp50=quantile(na.omit(JPUS),c(.5)),jp90=quantile(na.omit(JPUS),c(.9))),.(date)] %>% ggplotw()
# 
# 
# ## SE
# sprdw[date<ymd('2016-07-01'),.(eu=sd(na.omit(EUUS)),jp=sd(na.omit(JPUS))),.(date)] %>% ggplotw()
# 
# # percentage greater than 5 bps
# sprdw[date<ymd('2016-07-01'),.(eu=1-sum(abs(na.omit(EUUS))<=5)/length(na.omit(EUUS)), jp=1-sum(abs(na.omit(JPUS))<=5)/length(na.omit(JPUS))), date] %>% ggplotw()
# 
# #sprdw[date<ymd('2016-07-01'),.(eu=mean(na.omit(EUUS)),jp=mean(na.omit(JPUS)),ch=mean(na.omit(CHUS)),gb=mean(na.omit(GBUS)),ca=mean(na.omit(CAUS),au=mean(na.omit(AUUS)))),.(date)] %>% ggplotw()
# 
# 
# sprdw2<-sprdw[date<ymd('2016-07-01') & tier=='SNRFOR' & docclause %in% c('MR')]# %>% na.omit()
# sprdw2[,.(eu=sum(abs(na.omit(EUUS))<=5)/length(na.omit(EUUS)), jp=sum(abs(na.omit(JPUS))<=5)/length(na.omit(JPUS)), ch=sum(abs(na.omit(CHUS))<=5)/length(na.omit(CHUS)), gb=sum(abs(na.omit(GBUS))<=5)/length(na.omit(GBUS)), au=sum(abs(na.omit(AUUS))<=5)/length(na.omit(AUUS)), ca=sum(abs(na.omit(CAUS))<=5)/length(na.omit(CAUS))), date] %>% ggplotw()
# 
# sprdw2[,.(eu=sum(abs(na.omit(EUUS))<=5)/length(na.omit(EUUS)), jp=sum(abs(na.omit(JPUS))<=5)/length(na.omit(JPUS))), date] %>% ggplotw()
# sprdw[date<ymd('2016-07-01'),.(eu=sum(abs(na.omit(EUUS))<=5)/length(na.omit(EUUS)), jp=sum(abs(na.omit(JPUS))<=5)/length(na.omit(JPUS))), date] %>% ggplotw()
# 
# sprdw2[,.(eu=length(na.omit(EUUS)), jp=length(na.omit(JPUS)), ch=length(na.omit(CHUS)), gb=length(na.omit(GBUS)), au=length(na.omit(AUUS)), ca=length(na.omit(CAUS))), date] %>% ggplotw()
# 
# 
# 
# # plot SE
# sprdw[date<ymd('2016-07-01') & tier=='SNRFOR' & docclause %in% c('MR'),.(eu=sd(na.omit(EUUS)),jp=sd(na.omit(JPUS))),.(date)] %>% ggplotw()
# 
# # plot with SE
# 
# 
# sprdw[date<ymd('2016-07-01'),.(eu=mean(na.omit(EUUS)),euse=sd(na.omit(EUUS))),.(date)] 
# 
# 
# 
# ### count
# ctdocclause=sprdw[date<ymd('2016-07-01') & tier=='SNRFOR',.(eu=length(na.omit(EUUS))),.(date,docclause)] %>% dcast(date~docclause) 
# ctdocclause[year(date)>=2014]
