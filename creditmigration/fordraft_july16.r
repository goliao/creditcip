setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));load('gldbsmall.RData')
source('util.r')
dtl<-dtl[monthend==1]
dtm<-preprocess(bondref,dtl,prl,issfiltertype =2)
prw<-dtm$prw
ys1<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6)
#ys1b<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur')],dtm$prl,regversion=6)
#ys1[ys1b][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()
ys2<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6,adjccybs=TRUE)


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
fig3<-ys1[prw][date>'2004-01-01',.(date,ccyeur,eubs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (EU-US)','CIP deviations 5yr (implied - actual euro funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig3
#ggsave(file='../paper/figures/fig3_creditCIPeur.pdf',fig3,width=9,height=6)

# Figure 4 Credit mispring and CIP for JPY
fig4<-ys1[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (JP-US)','CIP deviations 5yr (implied - actual yen funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig4
#ggsave(file='../paper/figures/fig4_creditCIPjpy.pdf',fig4,width=9,height=6)

# Figure ... : rating
#fread('rating.csv')
ys_hg<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating %between% c(1,6)],dtm$prl,regversion=3)
ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3)
fig5<-ys_hg[ys_hy][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('HG','HY'))+theme_stata(base_size = 12)+theme(axis.title.y = element_text(margin =margin(0, 0, 0, 0)))
fig5

require('gridExtra')
# matching mispricing graphs
fig6<-list()
X11(width=7,height=9)
fig6[[1]]<-ys1[prw][date>'2004-01-01',.(date,ccyeur,eubs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Credit mispricing','CIP deviations'))+ggtitle('EUR')+theme_stata()
legendcommon<-get_legend(fig6[[1]])
fig6[[1]]<-fig6[[1]]+theme(legend.position='none')
fig6[[2]]<-ys1[prw][date>'2004-01-01',.(date,ccygbp,bpbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('GBP')
fig6[[3]]<-ys1[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('JPY')
fig6[[4]]<-ys1[prw][date>'2004-01-01',.(date,aud=ccyaud,basis_aud=adbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('AUD')
fig6[[5]]<-ys1[prw][date>'2004-01-01',.(date,ccychf,sfbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('CHF')
fig6[[6]]<-ys1[prw][date>'2004-01-01',.(date,ccycad,cdbs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+theme_stata()+theme(legend.position='none')+ggtitle('CAD')
fig6all<-grid.arrange(fig6[[1]],fig6[[2]],fig6[[3]],fig6[[4]],fig6[[5]],fig6[[6]],legendcommon,ncol=2,nrow=4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,7)),heights=c(2,2,2,.25))
#ggsave(file='../paper/figures/fig6_creditmispricings.pdf',fig6all,width=7,height=9)




# daily time series -------------------------------------------------------


rm(list=ls(all=TRUE));load('gldbsmall.RData')
source('util.r')

dtmd<-preprocess(bondref,dtl,prl,issfiltertype =2,monthlyonly = FALSE)
ys1<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T)
#dtm<-preprocess(bondref,dtl,prl,issfiltertype =2)
#ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur')],dtm$prl,regversion=6)

rm(dtl,prl,bondref)

obs<-ys1$dtreg[,.N,.(date,ccy)] %>% dcast(date~ccy)
obs[,yr:=year(date)][,mo:=month(date)][,euryrmo:=as.double(median(eur)),.(yr,mo)][,usdyrmo:=as.double(median(`1usd`)),.(yr,mo)]
obsdiscard<-obs[eur<0.8*euryrmo | `1usd`<.8*usdyrmo,.(date)]
#obs[!obsdiscard][!monthenddates][,.(date,`1usd`,eur)] %>% ggplotw()

cdrus<-RQuantLib::getHolidayList('UnitedStates',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdreu<-RQuantLib::getHolidayList('Germany',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdrjp<-RQuantLib::getHolidayList('Japan',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdrau<-RQuantLib::getHolidayList('Australia',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdrbp<-RQuantLib::getHolidayList('UnitedKingdom',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdrcd<-RQuantLib::getHolidayList('Canada',from=ymd('2002-01-01'),to=ymd('2017-08-01'))
cdrsf<-RQuantLib::getHolidayList('Switzerland',from=ymd('2002-01-01'),to=ymd('2017-08-01'))

save(cdrus,cdreu,cdrjp,cdrau,cdrbp,cdrcd,cdrsf,file='holidaycalendar.RData')

holidays<-data.table('date'=unique(c(cdrus,cdreu)))
setkey(holidays,date)


holidays[!obsdiscard] %>% View
#obs %>% write.csv('dailyeurobs.csv')
dt.merged<-dtmd$prw[,.(date,eubs5)][ys1$regcoef][!obsdiscard][!holidays]


# ECB QE announcements
ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

dt.merged %>% ggplotw(x11. = T)
dt.merged[date %between% c('2012-03-01','2016-07-01')] %>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))
dt.merged[date %between% c('2012-03-01','2012-09-01')] %>% ggplotw(x11. = T)+geom_vline(xintercept = as.numeric(ecbqe))


dt.merged[,diff(ccyeur)] %>% sd()
dt.merged[,diff(ccyeur)] %>% sd()
dtmd$prw[,.(date,eubs5)][ys1$regcoef][!obsdiscard][!holidays][date>'2008-01-01',diff(ccyeur)] %>% sd()
dt.merged[,diff(eubs5)] %>% sd(na.rm = T)
beep(sound=2)

#plot a rolling covariance graph!!! somehow showing lead lag would be ideal
