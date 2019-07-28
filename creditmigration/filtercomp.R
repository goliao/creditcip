#comp
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")  

rm(list=ls(all=TRUE));
load('preprocessedtemp.RData');load('db/sdc.RData');load('db/prl.RData');load('db/monthenddates.RData');load('db/bondref.RData');load('db/bondrefall.RData');source('util.r')
source('util.r'); 



# b1<-dtm$br %>% onefilter()
# b2<- dtm$br %>% issfilter(type=4)  
# bb<-dtm$dtl4[,.(pk,ccy,upcusip,ytofm,mdealtype,secur,issue_type_desc,tf_mid_desc,sic1,amt,amt_bucket,DEBT_CLASS_CD,MKT_TYP_CD,senior,rating_bucket,pub)] %>% distinct()
# b1<-dtm$br %>% onefilter()
# b2<- dtm$br
# compare.dt(b1,b2,'pk')
# ticker2download <- b2[is.na(DEBT_TYP_CD),.(pk)] %>% distinct()
# save(ticker2download,file='bds2download_161110.RData')
#load('bds2download_161110.RData')
# b2[is.na(amt),.N] %>% semi_join
# b2[is.na(amt) & !is.na(i),.N]
# flds<-c('PAYMENT_RANK','MARKET_ISSUE','CRNCY','AMT_ISSUED','CPN_TYP','COLLAT_TYP','SECURITY_TYP','MTY_TYP','ISSUE_DT')
# res<-bdpgl(tickers=ticker2download$pk,fieldstr= flds, filestr='bdp_moody_augment_161110.RData',splitN = 30)
# res<-bdpgl(tickers='restart')

load('dbin/bdp_moody_augment_161110.RData')
for (i in 1:length(prices))
  prices[[i]]<-prices[[i]] %>% as.data.table(keep.rownames=T)
dtp<-rbindlist(prices) %>% setnames('rn','pk')
bondref<-merge(bondref,dtp,by='pk',all.x = T)
bondref[PAYMENT_RANK %in% c('Secured','1st lien','2nd lien'),senior:='SS']
bondref[PAYMENT_RANK %in% c('SR Unsecured','Unsecured'),senior:='SU']
bondref[PAYMENT_RANK %like% 'Subord',senior:='SB']
exchrate<-prl[ticker %in% c('eur','aud','gbp','jpy','chf','cad'),.(ISSUE_DT=date,ccy=ticker,exchrate=value)][ccy %in% c('chf','cad','jpy'),exchrate:=1/exchrate][ccy=='usd',exchrate:=1]
bondref<-merge(bondref,exchrate,by=c('ISSUE_DT','ccy'),all.x=T,all.y=F)
bondref[is.na(amt) & !is.na(AMT_ISSUED) ,amt:=exchrate*AMT_ISSUED]




br[is.na(amt)]
br[is.na(amt) & !is.na(AMT_ISSUED), .(diff=as.numeric(d-settlement2))]

br[is.na(amt) & !is.na(AMT_ISSUED),.(d,ISSUE_DT)]

br[,.N,senior]

aa<-br %>% onefilter()

aa[,.N,MARKET_ISSUE]
aa[,.N,CRNCY]
aa[,.N,CPN_TYP]
aa[,.N,MTY_TYP]
aa[,.N,COLLAT_TYP]
aa[,.N,SECURITY_TYP]

aa[MTY_TYP=='CALLABLE']


load('db/dtlmo.rdata')
pkg<-dtl.mo[!is.na(pk),.(pk)] %>% distinct()
todownload<-merge(bondref,pkg,by='pk')
todownload2<-todownload %>% onefilter('v0')
todownload3<-todownload2 %>% anti_join(ticker2download,by='pk') %>% as.data.table


compare.dt(distinct(dtl.mo[,.(pk)]),bondref,'pk')

distinct(dtl.mo[,.(pk)]) %>% anti_join(bondref,by='pk') %>% as.data.table
distinct(dtl.mo[,.(pk)]) %>% anti_join(bondref,by='pk') %>% as.data.table

distinct(dtl.mo[,.(pk)]) %>% anti_join(bondrefall[,.(str_to_lower(ti))],by='pk') %>% as.data.table
