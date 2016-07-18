setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));load('gldbsmall.RData')
source('util.r')
dtm<-preprocess(bondref,dtl,prl,issfiltertype =2)
prw<-dtm$prw
ys1<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6)
ys2<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion=6,adjccybs=TRUE)

# Figure 1 CIP deviations
dfg<-prw[,.(date,eubs5,bpbs5,jybs5,adbs5)] 
setnames(dfg,old = c('eubs5','bpbs5','jybs5','adbs5'),new=c('EUR','GBP','JPY','AUD'))
fig1<-dfg %>% ggplotw()+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "EUR",'GBP','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig1
#ggsave(file='../paper/figures/fig1_cip.pdf',fig1,width=9,height=6)

# Figure 2 Credit mispring
fig2<-ys1[,.(date,ccyeur,ccygbp,ccyjpy,ccyaud)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "EUR",'GBP','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
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

obsdiscard %>% View

#obs %>% write.csv('dailyeurobs.csv')
dt.merged<-dtmd$prw[,.(date,eubs5)][ys1$regcoef][!obsdiscard]
dt.merged %>% ggplotw(x11. = T)
dt.merged[date %between% c('2014-12-01','2015-03-01')] %>% ggplotw(x11. = T)


dt.merged[,diff(ccyeur)] %>% sd()
dt.merged[!obsdiscard][,diff(ccyeur)] %>% sd()
dt.merged[,diff(eubs5)] %>% sd(na.rm = T)
beep(sound=2)

#plot a rolling covariance graph!!! somehow showing lead lag would be ideal
