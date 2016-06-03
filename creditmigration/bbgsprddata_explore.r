rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# combine all price data --------------------------------------------------
#load the old sprd data
dtl_spd<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')  %>% mutate(batch=0) 
dtl_yld2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413.rdata')  %>% mutate(batch=0) 
dtl_batch1<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData') %>% mutate(batch=1) 
dtl_batch2<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch2_asw.RData')  %>% mutate(batch=1) 
dtl_batch3<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY.RData') %>% mutate(batch=2)
dtl_batch4<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY_asw.RData') %>% mutate(batch=2)
dtl<-rbind(dtl_spd,dtl_yld2,dtl_batch1,dtl_batch2,dtl_batch3,dtl_batch4)
# get rid of duplicates, keep more recent downloads
setkey(dtl,date,parsekeyable,field)
dtl<-unique(dtl, fromLast = TRUE)
tickersloaded<-dtl[,parsekeyable,by=parsekeyable][,.(parsekeyable)]
#save(dtl,tickersloaded,file='bbg_bond_prices.rdata')


# New merge: price with sdc -----------------------------------------------
rm(list=ls(all=TRUE))
source('util.r')
load('bbg_bond_prices.rdata')
load('sdc.rdata')
sdc %<>% semi_join(tickersloaded,by='parsekeyable')
# minor renaming
sdc[ccy=='USD',ccy:='usd']
sdc[ccy=='EUR',ccy:='eur']

## need to augment nrating for ones that are missing 
# todo: augment nratings even more with bbg data; do this next
setkey(dtl,parsekeyable)
setkey(sdc,parsekeyable)
dtl2<-dtl[sdc[,.(ccy,mat2,nrating,upcusip,parsekeyable)],nomatch=0]
dtoas<-dtl2[field=='OAS_SPREAD_BID',!"field",with=FALSE]
###

#get rid of days with only single observation
dtoas<-dtoas[date %ni% dtoas[,.N,by=c('date','ccy')][N==1,date]]

# Version 0: demean method
setkey(dtl2,date,upcusip,ccy,field)
dt_oas_upmean<-dtoas[,.(oas_upmean=mean(value)),by=.(date,upcusip)]
setkey(dt_oas_upmean,date,upcusip)
setkey(dtoas,date,upcusip)
# demeaned oas spread for each bond; demeaning issuer*time specific means; residualize and aggregate up to ccy level
dtoas_ccy<-dtoas[dt_oas_upmean][,.(date,ccy,oas_updemeaned=value-oas_upmean)][oas_updemeaned!=0,.(oas_demean_ccy=mean(oas_updemeaned)),by=.(date,ccy)]
 #create a spread
dtoas_ccy[,oas_euus:=(.SD[ccy=='eur',oas_demean_ccy]-.SD[ccy=='usd',oas_demean_ccy]),by=.(date)]
dtoas_ccy %>% ggplot(aes(x=date,y=oas_euus))+geom_line()

# version 1::
regcoef<-dtoas[,lm(value~ccy+upcusip,data=.SD)$coefficients['ccyusd'],by='date']

# version 2:: get rid of price obs with only single ccy per date per upcusip; that is, obs is counted only when upcusip has both ccys
tokeep<-dtoas[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
setkey(tokeep,date,upcusip)
regcoef2<-dtoas[tokeep][,lm(value~ccy+upcusip,data=.SD)$coefficients['ccyusd'],by='date']

#save(regcoef,regcoef2,file='temp_ccyferegcoef.rdata')
load(file='temp_ccyferegcoef.rdata')

# compare v1 & v2
setnames(regcoef2,'V1','V2')
tmp<-merge(regcoef,regcoef2,by='date') %>% melt(id.vars='date')
setkey(tmp,date,variable)
tmp
tmp %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()

# compare v1 and demeaning method
temp_comp<-merge(regcoef,dtoas_ccy,by='date')
temp_comp[,euussprd:=-V1]
temp_comp %>% melt(id.vars='date', measure.vars=c('euussprd','oas_euus')) %>%  ggplot(aes(x=date,y=value,colour=variable))+geom_line()


# Let's try cross sectional reg for one single date
dailyregdata<-dtoas[date=='2004-01-30']
tdg<-dailyregdata[upcusip %in% (dailyregdata[,.N,by=c('upcusip','ccy')][,.N,by='upcusip'][N!=1,upcusip])]
lm(value~ccy+upcusip,dailyregdata)$coefficients['ccyusd']
lm(value~ccy+upcusip,tdg)$coefficients['ccyusd']

#small sample test
testsamp<-tdg[5:9,.(value,ccy,upcusip)]
setkey(testsamp,upcusip,ccy)
lm(value~ccy+upcusip,testsamp)$coefficients['ccyusd']
lm(value~ccy+upcusip,testsamp)
#lm(value~ccy+upcusip+ccy*upcusip,testsamp)
testres<-testsamp[,upmean:=mean(value),by=upcusip][,demeaned:=value-upmean][,mean(demeaned),by='ccy']
-(testres[ccy=='eur',V1]-testres[ccy=='usd',V1])

reg1<-lm(value~upcusip,testsamp)
testsamp$res<-reg1$residuals
lm(res~ccy,testsamp)

lm(demeaned~ccy,testsamp)
testsamp %>% write.csv(file=('temp.csv'))





## aggregate by ccy for each up, subtract up mean, then aggregate again to ccy level
# dtoas_ccy2<-dtoas[,.(oas_up=mean(value)),by=.(date,upcusip,ccy)][dt_oas_upmean][,.(date,ccy,oas_updemeanccy=oas_up-oas_upmean)][oas_updemeanccy!=0,.(oas_demean_ccy=mean(oas_updemeanccy)),by=.(date,ccy)]
# dtoas_ccy2[,oas_euus:=(.SD[ccy=='eur',oas_demean_ccy]-.SD[ccy=='usd',oas_demean_ccy]),by=.(date)]
# dtoas_ccy2 %>% ggplot(aes(x=date,y=oas_euus))+ge om_line()
# #why are these two different? ## think about it more

priceraw<-read.dta13('prices_extended.dta') %>% data.table()
priceraw %>% ds('date')
setkey(priceraw,date)

# adjusting for xccy
 %>% mutate(oas_res_eff=euus_sprd-eubs5) %>% 
  select(date,oas_res_eff,euus_sprd) %>% filter(date>'2006-01-01') %>% wgplot(.)

# plot ccy spread directly
dtoas[,.(oas_ccy=mean(value)),by=.(date,ccy)][,.(oas_euus=(.SD[ccy=='eur',oas_ccy]-.SD[ccy=='usd',oas_ccy])),by=.(date)] %>% 
  ggplot(aes(x=date,y=oas_euus))+geom_line()

dtoas_ccy

#keeping only bonds with up in the same period
up_date_multccy<-dtoas[,.N,by=.(date,upcusip,ccy)][,.N,by=.(date,upcusip)][N>1,.(date,upcusip)]
setkey(up_date_multccy,date,upcusip)
setkey(dtoas,date,upcusip)
  #keeping global bonds only
dtoas_glob<-merge(dtoas,up_date_multccy,all.y=TRUE)
dtoas_glob[,.N,by=.(date,ccy)] %>% ggplot(aes(x=date,y=N,colour=ccy))+geom_line()


dtoas[,,by=.(date,upcusip)]



dtl2[field=='OAS_SPREAD_BID',.(mean(value)),by=.(date,ccy)] %>% ggplot(.,aes(x=date,y=V1,colour=ccy))+geom_line()
# demeaned oas spread for each bond; demeaning issuer*time specific means
dt_oas_upmean %>% View



# Demeaned spread ---------------------------------------------------------

df_bond2<-df_bond %>% rename(oas=OAS_SPREAD_BID,upco=id_bb_ultimate_co) %>% select(-fac)
df_oas_issmean<-df_bond2 %>% group_by(date,upco) %>% summarise(oas_issmean=mean(oas))
# demeaned oas spread for each bond; demeaning issuer*time specific means
df_bond_de<-sqldf('select A.*, B.oas_issmean from df_bond2 as A left join df_oas_issmean as B on (A.date==B.date and A.upco==B.upco)') %>%
  tbl_df() %>% mutate(oas_res=oas-oas_issmean) %>% select(date,oas, oas_issmean, oas_res,upco,ccy) %>% arrange(date) 
# residual oas spread between eur and usd, issuer matched
df_ccyres<-df_bond_de %>% group_by(date,ccy) %>% summarise(mean_oas_res=mean(na.omit(oas_res))) %>% mutate(ccy=tolower(ccy)) %>% 
  dcast(.,date~ccy,value.var='mean_oas_res') %>% tbl_df() %>% mutate(euus_sprd=eur-usd)
df_ccyres %>% melt(.,id.vars='date') %>% filter(variable=='euus_sprd') %>% 
  ggplot(.,aes(x=date,y=value)) +geom_line()

priceraw<-read.dta13('prices_extended.dta')

# adjusting for xccy
df_price<-df_ccyres %>% full_join(.,priceraw,by='date') %>% mutate(oas_res_eff=euus_sprd-eubs5) %>% 
  select(date,oas_res_eff,euus_sprd) %>% filter(date>'2006-01-01') %>% wgplot(.)
# filter issuance 
df_reg_data<- df_sdc %>% as.data.frame() %>%  issfilter(.) %>% icollapse_all(.) %>% full_join(.,df_price,by='date')

df_reg_data %>% lm(I_net_euus~oas_res_eff,data=.)

regtemp<-function(dfreg){
  reg10_1<- dfreg %>% lm(I_net_euus~Cdif_euus_30,data=.)
  stargazer(reg10_1,type='text',report="vct*")
}


# explore yield -----------------------------------------------------------

load('../data/bloomberg/bbg_gbonds_160413.rdata')

a0 <- unlist(prices, recursive = FALSE)
tickernames <- names(a0)
df_yld <- data.frame() %>% tbl_df()
for (i in 1:length(tickernames)) {
  temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
  if (nrow(temp_new) == 0)
    print (str_c('empty:#', i, ' name:', tickernames[i]))
  df_yld <- df_yld %>% dplyr::bind_rows(., temp_new)
}
df_yld %<>% left_join(.,globalbonds,by='ticker')
agg_yld<-df_yld %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(YLD_YTM_MID))) 

# plot yields for eur, usd
agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
  gather(.,key='type',value='yield',-date) %>% 
  filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

# describe the number of data points missing
df_yldw<-df_yld %>% dcast(.,date~ticker,value.var="yield")
df_yldw %>% summarise_each(.,funs(length(is.na())))
missings<-colSums(is.na(df_yldw[,-1])) %>% as.data.frame() 
colnames(missings)<-'Nmissings'
missings$ticker=rownames(missings)
summary(missings)
missings %>% ggplot(.,aes(x=Nmissings))+geom_density()

# describe number of data points avaialbe for each currency at each time
df_yld %>% group_by(date, crncy) %>% summarise(Ndp=length(ticker)) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=crncy))+geom_line()

# yld sprd construct ------------------------------------------------------
priceraw<-read.dta13('prices_extended.dta')
# add bond maturity for each date in time

# isolate swap yield curve from priceraw

# merge df_yld with swap yield curve

# take diff with matched 

# Merge with sdc data main ------------------------------------------------

raw<-read.dta13('sdc96_clean2.dta')
df_sdc<-raw %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
# filter and augment with expected number of monthly observations
df_sdc2<-df_sdc[!duplicated(df_sdc$isin),] %>% filter(isin!='-')  %>%
  arrange(isin)  %>% 
  filter(mat2>'2005-01-01') %>% 
  mutate(expmonthlyobs=ceiling((pmin(as.numeric(ymd('2016-04-01')),as.numeric(mat2))-
                                  pmax(as.numeric(settlement2),as.numeric(ymd('2005-01-01'))))/30.5)) 
  
# count number of observations by isin
df_obs<-df_yld2 %>% group_by(isin) %>% summarise(.,ct=length(isin)) %>% arrange(desc(ct))
# compare to number of expected obs by isin
df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
# df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View

# merging
df_bond<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.d,B.mdealtype,B.amt,B.pub,B.issue_type_desc from df_yld2 as A, df_sdc2 as B where A.isin=B.isin')
df_bond2<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.mdealtype from df_yld2 as A left join df_sdc2 as B on A.isin=B.isin')

# merge with sec new ------------------------------------------------------
# matching to newly downloaded sdc isins
sdc_raw<-read.csv('../data/sdc/sdc_bbgmatch.csv',stringsAsFactors = FALSE)
rating_sp<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_sp=rating,nrating_sp=nrating)
rating_mdy<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_mdy=rating,nrating_mdy=nrating)
df_sdcnew<-sdc_raw %>% mutate(d=mdy(d),mat2=mdy(mat2),amt=as.numeric(amt),ytofm=as.numeric(ytofm)) %>% tbl_df() %>% 
  left_join(.,rating_sp, by='rating_sp') %>% left_join(.,rating_mdy,by='rating_mdy') %>% 
  select(issname,ticker_sdc,isin,cusip,d,nat,amt,descr,ccy,mat2,ytofm,everything()) %>% arrange(d)
save(df_sdcnew,file='sdcnew.rdata')
df_sdcn2<-df_sdcnew[!duplicated(df_sdcnew$isin),] %>% filter(isin!='-')  %>% arrange(isin)

df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2 from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')
df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2, B.issname, B.mdealtype from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')


# explore what's missing in which dataset ---------------------------------
# what's missing compare to earlier merge
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>%  group_by(i) %>% top_n(.,1,desc(date)) %>% dplyr::arrange(pub) %>% select(1,10:26) %>% View
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>%  xtabs(~pub+mdealtype+issue_type_desc,data=.)
  # so a lot of bonds are excluded due to dealtypes, gov, R144D. but still has some that's unclear why was excluded
   anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>% filter(issue_type_desc=='Investment Grade Corporate',mdealtype!='R144D') %>% View

# what's added compare to earlier merge
anti_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's the joint between new and old merge
semi_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's not captured in the sdc data but showed up in bloomberg bsrch
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% View
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% select(ticker:crncy) %>% write.csv(.,file='GetBBGBondData.csv')

#what's covered in sdc but not in bloomberg?
bbgmiss<-anti_join(df_sdcn2,df_yld2,by='isin') %>% filter(ccy!='gbp',nrating_sp<=10,nrating_mdy<=10,nrating_sp!=0,nrating_mdy!=0,amt>=100,ytofm>2,ytofm<50,
                                                 !grepl('Flt',typesec),!grepl('Zero Cpn',typesec),!grepl('Float',typesec),!grepl('Fl',descr),!grepl('Zero Cpn',descr)) 
bbgmiss 
bbgmiss  %>% xtabs(~typesec,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~mktplace,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~upsic,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss %>% View
bbgmiss %>% tabulate(.,'issname') %>% View
bbgmiss %>% write.csv(.,file='GetBBGTickerPrice.csv')

#######
df_bond %>% xtabs(~ccy+mdealtype,data=.)
df_bond %>% tabulate(.,'isin')
Hmisc::describe(df_sdcnew$mktplace)

# out of the ones that are matched, what's the data coverage?
# very good price coverage

df_bond %>% mutate(datestr=as.character(date),matstr=as.character(mat2)) %>% select(-date,-mat2,-fac) %>% 
  write.dta(.,'bondsprdpanel.dta')






# explore daily data ------------------------------------------------------

# df_p_daily<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_temp_daily.RData')
# save(df_p_daily,file='dailyprices.rdata')
load('dailyprices.rdata')
df_p_daily %<>% mutate(dofm=lubridate::mday(date)) 
aa<-df_p_daily %>% group_by((dofm)) %>% summarise(ct=length(na.omit(YLD_YTM_MID))) %>% ggplot(aa,aes(x=`(dofm)`,y=ct))+geom_line()
oas<-df_p_daily %>% group_by(dofm) %>% summarise(oas_dom=median(na.omit(OAS_SPREAD_BID)),
                                                 yld_dom=median(na.omit(YLD_YTM_MID)))
oas %>% ggplot(aes(x=dofm,y=yld_dom))+geom_line()
oas %>% ggplot(aes(x=dofm,y=oas_dom))+geom_line()


# assess data availability ------------------------------------------------
df_pasw_mo<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch2_asw.RData')
load(file='sdc_all.rdata')
df_sdc_all %<>% distinct(isin)
countdups(df_sdc_all %>% filter(!is.na(parsekeyable)),'parsekeyable')

ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_pasw_mo,field='ASSET_SWAP_SPD_MID')

df_p<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData')
df_p %>% View
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p)
df_p
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p,field='OAS_SPREAD_BID')
ac %>% filter(parsekeyable=='EF600203 Corp') %>% View

df_p<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY.RData')
df_p
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p)
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p,field='OAS_SPREAD_BID')

bondprices %>% filter_('YLD_YTM_MID'!="NA") %>% filter(ticker=='EF600203 Corp') %>% View
bondprices %>% filter(YLD_YTM_MID!="NA") %>% filter(ticker=='EF600203 Corp') %>% View

bondprices[bondprices['YLD_YTM_MID']!="NA",] %>% filter(ticker=='EF600203 Corp') %>% View
bondprices[bondprices[field]!="NA",] %>% filter(ticker=='EF600203 Corp')

df_p_daily %>% filter(ticker=='EF600203 Corp') %>% View

df_yld %>% ds
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='BLP_ASW_SPREAD_MID')
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='BLP_Z_SPRD_MID')

ac
ac %>% View

df_yld
df_yld<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')


ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='OAS_SPREAD_BID')

df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
ab<-df_yld2 %>% distinct(isin) %>% semi_join(df_sdc_all,by='isin')

### why are these two a little different????
nrow(ac)
nrow(ab)
ab %>% anti_join(ac,by='isin') %>% View


# see which fields to use based on sample data -------------------------------------------------
# concluded that oas and yld are the ones to use, other flds do not start till 2010
df_pt<-loadBBGdownload2df('../data/bloomberg/sample_flds_test.rdata')
load('sdc_all.rdata')
# df_sampleticker<-df_pt %>% group_by(ticker) %>% rename(parsekeyable=ticker) %>% summarise(ct=length(date)) %>% left_join(df_sdc_all %>% distinct(parsekeyable))
# df_sampleticker %>% filter(ccy=='EUR') %>% select(isin) %>% write.csv(file='sample_eur_isin.csv')
df_pt %<>% filter(date>='2005-01-01',date<='2016-04-01')

df_pt<-df_p %>% filter(date>='2005-01-01',date<='2016-04-01')
df_pt %>% ds
ac<-df_pt %>% filter(date>='2005-01-01',date<='2016-04-01') %>%
  assessDataCoverage(bondinfo=df_sdc_bbg,bondprices=.,field='OAS_SPREAD_BID')

ac %>% mutate(allflddiff=expmonthlyobs-obs_allflds) %>% filter(allflddiff %ni% c(-1,0,1)) %>%  
  left_join(df_sdc_bbg) %>% arrange(allflddiff) %>% select(figi,name,ticker,i,d,mat2,everything()) %>% 
  write.csv(file='temp.csv')

# df_obs %>% arrange(desc(obscoverage)) %>% View
# bondprices %>% filter(parsekeyable=='EH728416 Corp') %>% View
# df_sdc_all %>% filter(parsekeyable=='EH728416 Corp') %>% write.csv(file='temp.csv')
# df_sdc_bbg %>% filter(isin=='FR0010359810') %>% select(figi,name,ticker,i,d,mat2,everything()) %>% View


df_yld_long<-df_p %>%  melt(.,id.vars=c('date','parsekeyable'),
                            measure.vars=((df_pt %>% ds)[(df_pt %>% ds) %ni% c('date','parsekeyable','batch')]),
                            variable.name='field') %>% dplyr::tbl_df() %>% filter(!is.na(value)) %>% distinct(date,parsekeyable,field) 
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>%  ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

df_yld_long %>% showdups(c('date','parsekeyable','field'))
df_yld_long %>% distinct(parsekeyable) %>% nrow


# load bbg bond sprd, unpack ----------------------------------------------
df_yld<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')
load('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')

# df_yld_long<-df_yld %>% select(-BLP_CDS_BASIS_MID) %>%  gather(key = 'field',value='value',-date,-ticker) %>% dplyr::tbl_df()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

# add ccy
df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
# df_yld_long<-df_yld2 %>% select(-BLP_CDS_BASIS_MID) %>%  
#   melt(.,id.vars=c('date','ticker','crncy'),
#        measure.vars=c("OAS_SPREAD_BID","BLP_Z_SPRD_MID","BLP_ASW_SPREAD_MID","BLP_I_SPRD_MID","BLP_Z_SPRD_LAST","BLP_ASW_SPREAD_LAST","BLP_I_SPRD_LAST"),
#        variable.name='field') %>% 
#   dplyr::tbl_df()
# #plot available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()

#View available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(OAS_SPREAD_BID))) 
# plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(BLP_I_SPRD_MID))) 
# # plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# # plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()


