setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r')
dtm<-preprocess(bondref,dtl,prl,issfiltertype =2)

dtm$ys1<-resyldsprdv3(dtm$dtl3,dtm$prl,regversion=6)

dtm$ys_rall<-resyldsprdv3(dtm$dtl3,dtm$prl,regversion=3)

# plot for high grade and high yield
ys_hg<-resyldsprdv3(dtm$dtl3[ccy %in% c('usd','eur') & nrating %between% c(1,6)],dtm$prl,regversion=3)
ys_hy<-resyldsprdv3(dtm$dtl3[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3)
ys_hg[ys_hy][,.(date,ccyeur,i.ccyeur)] %>% ggplotw(x11.=TRUE)

dtm$br[,mean(na.omit(nrating)),ccy]

ys_hg<-resyldsprdv3(dtm$dtl3[ccy %in% c('usd','eur') & nrating %between% c(1,2)],dtm$prl,regversion=3)
ys_mg<-resyldsprdv3(dtm$dtl3[ccy %in% c('usd','eur') & nrating %between% c(4,7)],dtm$prl,regversion=3)
ys_hy<-resyldsprdv3(dtm$dtl3[ccy %in% c('usd','eur') & nrating>7],dtm$prl,regversion=3)
ys_hg[ys_mg][ys_hy][,.(date,ccyeur,i.ccyeur,i.ccyeur.1)] %>% ggplotw(x11.=TRUE)



ys_hg[ys_hg2][,.(date,ccyeur,i.ccyeur,i.ccyeur.1)] %>% ggplotw()

ys_hy[ys_hy2][,.(date,ccyeur,i.ccyeur,i.ccyeur.1)] %>% ggplotw()

dtm$dtl3[nrating %between% c(1,3),.N,ccy]
dtm$dtl3[nrating %between% c(4,7),.N,ccy]
dtm$dtl3[nrating>7,.N,ccy]


fread('rating.csv')
# Figure 1 CIP deviations
fig1<-prw[,.(date,eubs5,bpbs5,jybs5,adbs5)] %>% ggplotw()+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "GBP",'EUR','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
#ggsave(file='../paper/figures/fig1_cip.pdf',fig1,width=9,height=6)

# Figure 2 Credit mispring
fig2<-ys1 %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "GBP",'EUR','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
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

# fig4b<-ys1nf[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Residualized credit spread diff (JP-US)','CIP deviations 5yr (implied - actual yen funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
# fig4b
##ggsave(file='../paper/figures/fig3_creditCIPjpy.pdf',fig4,width=9,height=6)
ys1[prw][date>'2004-01-01',.(date,ccygbp,bpbs5)] %>% ggplotw()
ys1[prw][date>'2004-01-01',.(date,ccyaud,adbs5)] %>% ggplotw()

## Figure 5: crd mispricing and issuance; save data for it first
ys2[,.(date,ccyeur)] %>% ggplotw()
ys2 %>% write.dta('temp_ys.dta')

