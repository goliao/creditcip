setwd('E:/')
setwd('f:/')
setwd('J:/')
setwd('/Volumes/GORDONLIAO')
rm(list=ls(all=TRUE))
#load('gldb.RData')
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

#6/30
dtchf4<-downloadbbg('restart')
load('tempdtchffirst2batches.rdata')
dtchf4
dtchfdaily<-update.dt(dtchf,dtchf4)
save(dtchfdaily,file='dtchfdaily.RData')

#dtsmalliss<-downloadbbg('restart',filestr = 'temp_bbg_smalliss_160624.RData')
#save(dtsmalliss,file='smallIssuance.RData')

dtsmalliss<-loadBBGdownload2df('smallIssuance.RData')
load('temp_temp_bbg_smalliss_160624.RData')
aa<-data.table('pk'=(tickerslist %>% unlist()))

aa<-data.table()
for (i in 1:length(tickerslist)){
  aa<-rbind(aa,data.table('pk'=tickerslist[[i]],'nbatch'=i,'tot'=length(tickerslist[[i]])))
}

miss<-aa %>% anti_join(dtsmalliss[,.N,pk],by='pk')
miss %>% numSummary()
miss[,.N,nbatch][order(nbatch)] %>% View


# 2016-07-26 update daily bond data 
load('bonds2update160726.rdata')
dtladd.daily2<-downloadbbg(unique(dttoget2[,.N,pk][,.(pk)]),filestr='dtl160726dailyB.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2016-04-01'),periodstr='DAILY',splitN=1)
load('temp_bbgdownload_restart.RData')
i<-36
#save(prices,tickers,tickerslist,i,fieldstr,startdt,opt,splitN,file='temp_bbgdownload_restart.RData')
source('util.r')
dtladd.daily<-downloadbbg('restart')
#save.image('temp4_daily.rdata')
dtladd.daily<-update.dt(dtladd.daily,dtladd.daily2,keyfield = c('pk','date'))
#save(dtladd.daily,file='dtl160726_daily_addition.RData')
dttoget2<-dttoget %>% anti_join(dtladd.daily[,.N,pk],by='pk')



# 2016-07-31 update monthly bond data 
source('util.r')
#load('bonds2update160726.rdata')
#dtladd.monthly<-downloadbbg(unique(dttoget[,.N,pk][,.(pk)]),filestr='dtl160731mo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2016-03-01'),periodstr='MONTHLY',splitN=50)
dtladd.monthly<-downloadbbg('restart')
#save(dtladd.monthly,file='dtlmoadd20160731.RData')

#update prl daily
rm(list=ls(all=TRUE))
load('gldb.RData')
source('util.r')

prl2get<-get.prl.status(prl)
prladd.daily<-downloadbbg(prl2get[,.(pk=str_c(ticker,' curncy'))],filestr='prl160731daily.RData',fieldstr='PX_LAST',startdt=ymd('2016-05-25'),periodstr='DAILY',splitN=4)
#save(prladd.daily,file='prlupdate160731.RData')
prladd.mo<-downloadbbg(prl2get[,.(pk=str_c(ticker,' curncy'))],filestr='prl160731daily.RData',fieldstr='PX_LAST',startdt=ymd('2016-05-25'),periodstr='MONTHLY',splitN=4)
#save(prladd.daily,prladd.mo,file='prlupdate160731.RData')
prl2get[,.N,DailyMax]
prl2get[DailyMax<'2016-06-01']


# getting new issuance
rm(list=ls(all=TRUE))
source('util.r')
newiss2get<-fread('newiss2get160731.csv',sep=',')
dtladd.monthly<-downloadbbg(newiss2get,filestr='dtl160731newissmo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2016-04-01'),periodstr='MONTHLY',splitN=6)
dtladd.daily<-downloadbbg(newiss2get,filestr='dtl160731newissdaily.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2016-04-01'),periodstr='DAILY',splitN=6)
# save(dtladd.monthly,dtladd.daily,file='dtl160731newiss.RData')



# 2016-08-03
	# get even more bond data:
	load('db/pk_lookup.RData')
	pk2get1<-fread('pk2download160803_04-06.csv',sep=',') #2004-2006 missing data
	pk2get2<-fread('pk2download160803_1-2yrallccynew.csv',sep=',') 
	pk2get3<-fread('pk2download160803_otherccyA.csv',sep=',')

	pk2get1 %>% setkey(pk)
	pk2get2 %>% setkey(pk)
	pk2get3 %>% setkey(pk)

	pk2get1[,pk:=tolower(pk)]
	pk2get2[,pk:=tolower(pk)]
	pk2get3[,pk:=tolower(pk)]

	load('db/bondref160803.RData')
	bondref %>% setkey(pk)

	pk2get1.<-bondref[pk2get1,nomatch=0]
	pk2get2.<-bondref[pk2get2,nomatch=0]
	pk2get3.<-bondref[pk2get3,nomatch=0]
	# many are missing in SDC data base unfortunate
	pk2get1.[,.N]/pk2get1[,.N]
	pk2get2.[,.N]/pk2get2[,.N]
	pk2get3.[,.N]/pk2get3[,.N]

	dtladd1.monthly<-downloadbbg(pk2get1.$pk,filestr='dtl160803_yr04-06addmo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=1)
	dtladd1.daily<-downloadbbg(pk2get1.$pk,filestr='dtl160803_yr04-06adddaily.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=1)
	#save(dtladd1.monthly,dtladd1.daily, file='dtl160803_yr04-06add.RData')


	dtladd2.monthly<-downloadbbg(pk2get2.$pk,filestr='dtl160803_1-2yrallccynewaddmo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=10)
	dtladd2.daily<-downloadbbg(pk2get2.$pk,filestr='dtl160803_1-2yrallccynewadddaily.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=10)
	#save(dtladd2.monthly,dtladd2.daily, file='dtl160803_1-2yrallccynewadd.RData')


	dtladd3.monthly<-downloadbbg(pk2get3.$pk,filestr='dtl160803_otherccymo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=20)
	#dtladd3.monthly<-downloadbbg('restart')
	dtladd3.daily<-downloadbbg(pk2get3.$pk,filestr='dtl160803_otherccydaily.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=20)
	#save(dtladd3.monthly,dtladd3.daily, file='dtl160803_otherccy.RData')

	pkgot<-rbind(pk2get1.[,.(pk)],pk2get2.[,.(pk)],pk2get3.[,.(pk)]) %>% unique()
	pkgot %>% setkey(pk)
	
	#get what's missing from SDC's prespective
	load('db/bondref160803.RData')
	load('db/dtlmo.RData')
	bondref %>% setkey(pk)
	dtl.mo %>% setkey(pk)
	pk2getnew<-bondref[!dtl.mo][!is.na(pk)] %>% issfilter(.,3)
	
	pk2getnew<-pk2getnew[,.(pk)]
	pk2getnew %>% setkey(pk)
	
	pk2get4.<-pk2getnew[pkgot]
	#save(pk2get4.,file='pk2get_complete_sdc_160803.RData')
	
	dtladd4.monthly<-downloadbbg(pk2get4.$pk,filestr='dtl160803_completesdc.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=40)
	#dtladd4.monthly<-downloadbbg('restart')
	dtladd4.daily<-downloadbbg(pk2get4.$pk,filestr='dtl160803_completesdc.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=40)
	#save(dtladd4.monthly,dtladd4.daily, file='dtl160803_completesdc.RData')
	
	# recheck conflicting bond prices in dtlmo.RData
	recheck1.monthly<-downloadbbg(recheck$pk,filestr='dtl160803_recheck1mo.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=1)
	recheck1.daily<-downloadbbg(recheck$pk,filestr='dtl160803_recheck1daily.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=1)
	#save(recheck1.monthly,recheck1.daily, file='dtl160803_recheck1add.RData')
	
	
	
#8/4 to get next: 
	rm(list=ls(all=TRUE));
	load('db/dtlmo.RData');load('db/bondref160803.RData');
	source('util.r')

	bondref %>% setkey(pk)
	dtl.mo %>% setkey(pk)
	pk2get84<-(bondref %>% issfilter(.,3))[,.(pk)][!dtl.mo]
	# load(file='pk2get_complete_sdc_160803.RData')
	pk2get0804<-pk2get84 #%>% anti_join(pk2get4.) %>% as.data.table()
	
	dtladd5.monthly<-downloadbbg(pk2get0804$pk,filestr='dtl160804_completesdc.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='MONTHLY',splitN=40)
	#dtladd5.monthly<-downloadbbg('restart')
	dtladd5.daily<-downloadbbg(pk2get0804$pk,filestr='dtl160804_completesdc.RData',fieldstr='YLD_YTM_MID',startdt=ymd('2002-01-01'),periodstr='DAILY',splitN=40)
	#save(dtladd5.monthly,dtladd5.daily, file='dtl160804_completesdc.RData')

