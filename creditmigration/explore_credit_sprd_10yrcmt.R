
source('util.r')
require(Quandl)
cm10<-Quandl("FRED/GS10") %>% as.data.table
# Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
bbb10sprd<-Quandl("FRED/BAA10YM") %>% as.data.table()
colnames(cm10)<-c('date','tsry10yr')
colnames(bbb10sprd) <- c('date','bbb10yrsprd')
cm10 %>% setkey(date)
bbb10sprd %>% setkey(date)
dtylds<-cm10[bbb10sprd,nomatch=0]
dtylds %>% ggplotw()


dtylds %>% felm(bbb10yrsprd~tsry10yr,.) %>% summary

aa<-dtylds %>% neweymod(bbb10yrsprd~tsry10yr);aa
aa<-dtylds[date>=ymd('2004-01-01')] %>% neweymod(bbb10yrsprd~tsry10yr)
aa


aa<-dtylds[date<ymd('1970-01-01')] %>% neweymod(bbb10yrsprd~tsry10yr)
aa
