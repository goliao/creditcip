
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');
# checking whether the old daily data, dtl, or the new monthly data is correct

load('dailyprices.rdata')
dtadd<-df_p_daily %>% as.data.table() %>% melt(id.vars=c('date','ticker'),measure.vars=c('OAS_SPREAD_BID','YLD_YTM_MID'),variable.name='field') %>% rename(pk=ticker)
dtadd<-dtadd[!is.na(value)]
temp1<-update.dtl(dtl,dtadd,diagret=T)
dtl<-temp1[[1]]
monthlydownloadagain<-temp1[[2]][[1]][,.N,pk]


dtadd2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_daily_batch2.RData')
temp2<-update.dtl(dtl,dtadd2,diagret=T)
dtl<-temp2[[1]]
monthlydownloadagain<-rbind(monthlydownloadagain,temp2[[2]][[1]][,.N,pk])
## save(monthlydownloadagain,file='get_mo_160627.RData')

dtcheck<-loadBBGdownload2df('../data/bloomberg/160628Sam/dtl160628C.RData')
temp3<-update.dtl(dtl,dtcheck,diagret=T)


conf1<-temp1[[2]][[1]]
conf2<-temp2[[2]][[1]]
conf3<-temp3[[2]][[1]]

conf3<-distinct(conf3)
setnames(conf1,'value','dtlmonthly')
setnames(conf1,'i.value','dailypr1')
setnames(conf2,'value','dtlmonthly')
setnames(conf2,'i.value','dailypr2')
setnames(conf3,'value','dtlmonthly')
setnames(conf3,'i.value','monthlycheck')

confs<-merge(merge(conf1,conf2,all=TRUE),conf3,all=TRUE)


dtadd[pk=='dd107712 corp']
dtcheck[pk=='dd107712 corp' & date=='2013-05-31']
									  dtl    dailyprices    diff
 2013-05-31 dd107712 corp YLD_YTM_MID 4.681   4.803   0.122
 									  monthly daily_batch2
 2013-05-31 dd107712 corp YLD_YTM_MID 4.681   4.803   0.122

