
require(xlsx)


#################################
## New summary
#################################
dtl<-dtl %>% bucketytofm(.)

dtl[amt>0,.(amt=mean(na.omit(amt))),.(pk,ccy)][,.(sum(amt)/1000)]
dtl[amt>0 & !is.na(swapsprd),.(amt=mean(na.omit(amt))),.(pk,ccy)][,.(sum(amt)/1000)]


### since inceptoin
dtlsi<-dtl[order(date)][amt>0,.SD[1],pk]
dtlsi[,.(notional=sum(amt)/1000)]

sbyccy=dtlsi[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)]
ssum=sbyccy[,.(field='all',cat='',nbonds=sum(nbonds),notional=sum(notional))]
sbyrating=dtlsi[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][,.(field='rating',cat=rating_bucket,nbonds,notional)]
sbymaturity=dtlsi[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
sum1<-rbindlist(list(ssum,sbyccy,sbyrating,sbymaturity))
ord<-fread('../SUMMARYTABLEORDER.CSV')
sum1b<-merge(sum1,ord,by=c('field','cat'))[order(order)]
sum1b %T>% dt2clip()

### June 16 cross-section
dtlh16=dtl[date==ymd('2016-06-30') & amt>0]

sbyccy=dtlh16[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)]
ssum=sbyccy[,.(field='all',cat='',nbonds=sum(nbonds),notional=sum(notional))]
sbyrating=dtlh16[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][,.(field='rating',cat=rating_bucket,nbonds,notional)]
sbymaturity=dtlh16[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
sbymaturity

sum2<-rbindlist(list(ssum,sbyccy,sbyrating,sbymaturity))

ord<-fread('../SUMMARYTABLEORDER.CSV')
sum2b<-merge(sum2,ord,by=c('field','cat'))[order(order)]


sumall<-merge(sum1b,sum2b,by=c('order','field','cat'),suffixes = c('all','H16'))
sumall %T>% dt2clip() 
sumall %>% xlsx::write.xlsx(file='tables.xlsx',sheetName='datasummary',append=TRUE)

# number of ultimate parents
dtl[,.N,upcusip]
# ###################
# 
# dtl.bonds<-dtl[,.SD[1],pk]
# dtl.bonds.global<-(dtl %>% filterglobaluponly())[,.SD[1],pk]
# dtl.bonds[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds[sic1==6,sicind:=6];dtl.bonds[sic1==9,sicind:=9]
# 
# 
# dtl.bonds<-dtl.bonds %>% bucketytofm(.)
# 
# 
# summarylist<-list()
# summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000)][,.(field='all',cat='',nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)][order(-notional)]
# summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][order(-rating_bucket)][,.(field='rating',cat=rating_bucket,nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][order(ytofm_bucket)][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtl.bonds[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),sicind][order(sicind)][,.(field='ind',cat=sicind,nbonds,notional)]
# sum1<-rbindlist(summarylist)
# sum1 %T>% dt2clip()
# sum1 %>% write.xlsx(file='tables.xlsx',sheetName='summary1',append=TRUE)
# 
# 
# dtl.bonds.global[sic1!=9 | sic1!=6,sicind:=1];dtl.bonds.global[sic1==6,sicind:=6];dtl.bonds.global[sic1==9,sicind:=9]
# dtl.bonds.global<-dtl.bonds.global %>% bucketytofm(.)
# dtin.sum<-dtl.bonds.global
# summarylist<-list()
# summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000)][,.(field='all',cat='',nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ccy][,.(field='ccy',cat=ccy,nbonds,notional)][order(-notional)]
# summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),rating_bucket][order(-rating_bucket)][,.(field='rating',cat=rating_bucket,nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),ytofm_bucket][order(ytofm_bucket)][,.(field='maturity',cat=ytofm_bucket,nbonds,notional)]
# summarylist[[length(summarylist)+1]] <- dtin.sum[,.(nbonds=.N,notional=sum(na.omit(amt))/1000),sicind][order(sicind)][,.(field='ind',cat=sicind,nbonds,notional)]
# sum2<-rbindlist(summarylist)
# sum2 %T>% dt2clip()
# sum2 %>% write.xlsx(file='tables.xlsx',sheetName='summary2',append=TRUE)
