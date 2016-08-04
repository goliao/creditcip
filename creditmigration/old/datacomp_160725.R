rm(list=ls(all=TRUE));
load('gldbsmall.RDATA')
source('util.r')

dtm<-preprocess(bondref,dtl,prl,issfiltertype = 2)
dtm$ys1<-resyldsprdv4(dtm$dtl4,dtm$prl,regversion = 6,winsor.=.025)
dtm$ys1[,.(date,ccyeur,ccygbp,ccyjpy,ccyaud)] %>% ggplotw()

dtm2<-preprocess(bondref,dtl,prl,issfiltertype = 1)
dtm2$ys1<-resyldsprdv4(dtm2$dtl4,dtm2$prl,regversion = 6,winsor.=.025)
dtm2$ys1[,.(date,ccyeur,ccygbp,ccyjpy,ccyaud)] %>% ggplotw()

dtm$ys1[dtm2$ys1] %>% ggplotw.comp()

dtm2$dtl4[,.N,ccy]
dtm$dtl4[,.N,ccy]

dtm$dtl4[ccy=='aud'] %>% ggplot(aes(x=date,y=swapsprd,colours=pk))+geom_line()
dtm2$dtl4[ccy=='aud'] %>% ggplot(aes(x=date,y=swapsprd,colours=pk))+geom_line()


df2<-dtm$dtl4
df2[,pctl:=percent_rank(eval(exparse('swapsprd'))),by=.(date,ccy)]
winsor=.025;df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
df2 %>% ggplot(aes(x=date,y=swapsprd,colours=pk))+geom_line()


df2[ccy=='usd'] %>% ggplot(aes(x=date,y=swapsprd,colours=pk))+geom_line()
