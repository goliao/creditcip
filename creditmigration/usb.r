setwd('E:/')
setwd('J:/')
rm(list=ls(all=TRUE))
load('gldb.RData')
source('util.r')

# 2016-06-27: redownload some data for verification
load('2downloadmonthly160625.RData')
dtbondadd<-downloadbbg(todownloadmonthly,filestr='bbg_temp_160627.RData',fieldstr = 'YLD_YTM_MID',startdt =ymd('2002-01-01'),splitN = 1)
dtbondadd<-loadBBGdownload2df('bbg_2016-06-27.RData')
dtl<-update.dtl(dtl,dtbondadd,1)
prl<-fixmonthend(monthenddates,prl)
setkey(prl,date,ticker,pk,field)
#resave(dtl,file='gldb.RData')

dtcheck<-loadBBGdownload2df('temp.RData')

# 2016-06-28
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');
prl.status<-get.prl.status(prl)
dtl.status<-get.dtl.status(dtl)
dtl.status[matured==0,.N,.(ccy,DailyMax)][order(ccy,-N)] %>% View

#to download daily these
togetdaily<-dtl.status[matured==0 & ccy %in% c('eur','usd') & (DailyMax<'2016-06-01' | is.na(DailyMax))]

#resolve maturity difference
bb<-merge(bondref[,.(pk,i,descr,d,settlement2,mat2,matbbg,ccy)],dtl[,.N,pk],by='pk',all.y=TRUE)
togetmat<-bb[,.(pk,i,descr,d,settlement2,mat2,matbbg,diff=as.numeric(matbbg-mat2))][abs(diff)>10]

#dtcheck<-downloadbbg(togetmat,fieldstr=c('MATURITY','ISSUE_DT'),startdt='BDP',filestr='temp.RData')
source('util.r')
dtcheck<-loadBBGdownload2df('temp.RData')
merge(togetmat,dtcheck,by='pk')

#download on sam's computer
rm(list=ls(all=TRUE));load('gldb.RData')
source('util.r');
prl.status<-get.prl.status(prl)
#prlnew<-downloadbbg(str_c(prl.status$ticker,' curncy'),filestr='prldaily160628.RData',fieldstr = 'PX_LAST',startdt =ymd('1996-01-01'),periodstr="DAILY",splitN = 3)
prlnew<-loadBBGdownload2df('prldaily160628.RData')
prl<-update.prl(prl,prlnew,override=TRUE)

#dtl.status<-get.dtl.status(dtl)
#togetdaily<-dtl.status[matured==0 & ccy %in% c('eur','usd') & (DailyMax<'2016-06-01' | is.na(DailyMax))]
#dtln1<-downloadbbg(togetdaily[is.na(DailyMax)],filestr='dtl160628.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=3)
dtln1<-loadBBGdownload2df('dtl160628.RData')
dtl<-update.dtl(dtl,dtln1)

#dtln2<-downloadbbg(togetdaily[DailyMax>'2016-04-25'],filestr='dtl160628B.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2016-04-01'),periodstr='DAILY',splitN=20)
dtln2<-loadBBGdownload2df('dtl160628B.RData')
dtl<-update.dtl(dtl,dtln2,override=TRUE)
#dtl<-aa[[1]]
# # most serious conflicts comes from last date; better to override
# conf2<-aa[[2]][[1]]
# conf2[,.N,pk][order(-N)]
# conf2[,.N,date][order(-N)]
# conf2[abs(valdiff)>.25]

#togetdaily<-dtl.status[ccy %in% c('gbp')]
#dtln3<-downloadbbg(togetdaily,filestr='dtl160628GBP.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=10)
dtln3<-loadBBGdownload2df('dtl160628GBP.RData')
dtl<-update.dtl(dtl,dtln3,override = TRUE)
# bb<-update.dtl(dtl,dtln3,diagret = TRUE)
# conf3<-bb[[2]][[1]]
# conf3[,.N,pk][order(-N)]
# conf3[,.N,date][order(-N)]
# conf3[abs(valdiff)>.25]
# dtl[dtln3[pk=='ec220597 corp']] %>% tsdiff(.) %>% write.csv('J:/temp_test.csv')


#load('get_mo_160627.RData')
#dtl4<-downloadbbg(monthlydownloadagain,filestr='dtl160628C.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=1)
dtlcheck4<-loadBBGdownload2df('dtl160628C.RData')
dtl<-update.dtl(dtl,dtlcheck4,override=TRUE)

#resave(dtl,prl,file='gldb.RData')

####incomplete##########
#togetdaily<-dtl.status[ccy %in% c('chf')]
#dtln5<-downloadbbg(togetdaily,filestr='dtl160628CHF.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=10)

#6/29
#made some erros in the code where I have manually restart chf daily download
dtlsmall<-downloadbbg('restart',filestr='chf...')
load('temp_bbg_smalliss_160624.RData')
rm(list=ls(all=TRUE))
dtchf1<-loadBBGdownload2df('temp_dtl160628CHF.RData')
dtchf2<-loadBBGdownload2df('temp_bbgdownload_restart.RData')
dtchf<-rbind(dtchf1,dtchf2)
setkey(dtchf,date,pk)
dtchf %>% showdups()
save(dtchf,file='tempdtchffirst2batches.rdata')

dtchf3<-downloadbbg('restart',filestr='chf...')
