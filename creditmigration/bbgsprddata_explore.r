# New merge: price with sdc -----------------------------------------------
rm(list=ls(all=TRUE))
source('util.r')
load('gldb.RDATA')

sdc %<>% semi_join(dtl,by='parsekeyable')

# set induatry code
# sdc[,.(upsic2=str_sub(upsicp,1,2),upsic1=str_sub(upsicp,1,1))][,.N,by=upsic1]
sdc<-sdc %>% mutate(upsic1=as.numeric(str_sub(as.character(upsicp),1,1))) %>% as.data.table()
sdc[is.na(sic1),sic1:=upsic1]
sdc[,sicfac:=factor(sic1)]


# MERGE RATING AND ADD MATURITY BUCKETS -----------------------------------
setkey(dtl,parsekeyable)
setkey(sdc,parsekeyable)
dtl2<-dtl[sdc[abs(matdiff)<.1,.(ccy,mat2,rating,nrating,upcusip,parsekeyable,isin,ytofm,sicfac,sic1)],nomatch=0]

dtl2[,ytm:=as.numeric((mat2-date)/365)]

dtl3<-dtl2[ytm>0.1][!is.na(nrating)] %>% bucketrating() %>% bucketytm()

yldsprd2<-resyldsprd(dtl3,priceraw,regversion=2)
yldsprd3<-resyldsprd(dtl3,priceraw,regversion=3) %>% rename(euus_yldsprd_3=euus_yldsprd)
yldsprd4<-resyldsprd(dtl3,priceraw,regversion=4) %>% rename(euus_yldsprd_4=euus_yldsprd)
yldsprd5<-resyldsprd(dtl3,priceraw,regversion=5) %>% rename(euus_yldsprd_5=euus_yldsprd)

yldsprd4[yldsprd5] %>% ggplotw()
yldsprd2[yldsprd3][yldsprd4]  %>% ggplotw()


# finance vs nonfiannce
ys_nf<-dtl3[sic1!=6] %>% resyldsprd(.,priceraw,regversion=4) %>% rename(euus_yldsprd_nf=euus_yldsprd)
ys_f<-dtl3[sic1==6] %>% resyldsprd(.,priceraw,regversion=4) %>% rename(euus_yldsprd_f=euus_yldsprd)
ys_nf[ys_f] %>% ggplotw()

# Old residualizing  ----------------------------------------------------------
ccyfe<-getccyFE(dtl2,fieldstr='OAS_SPREAD_BID')
ccyfe_yield<-getccyFE(dtl2,fieldstr='YLD_YTM_MID')
ccyfe_as<-getccyFE(dtl2,fieldstr='ASSET_SWAP_SPD_MID')

feg<-ccyfe_as[ccyfe_yield][ccyfe]
setnames(feg,c('euus','i.euus','i.euus.1'),c('euus_oas','euus_yld','euus_as'))
ggplotw(feg)

#create yield spread for aggregate 
dtl2[,ytm:=as.numeric((mat2-date)/365)]
#winsorize by value a little
dtl3<-dtl2[field=='YLD_YTM_MID',pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99 & field=='YLD_YTM_MID']
# get rid of up where up doesn't have bonds in both ccys for each date
tokeep<-dtl3[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
setkey(tokeep,date,upcusip)
setkey(dtl3,date,upcusip)
dtl3<-dtl3[tokeep]

# winsorize by ytm a little
dtl3<-dtl3[,pctl:=percent_rank(ytm),by=.(date,ccy)][pctl>=.01 & pctl<.99]

# graph avg ytm by ccy by date
ytm_cc<-dtl3[,.(mean(na.omit(ytm))),by=.(date,ccy)]
setnames(ytm_cc,'V1','ytm_avg')
ytm_cc %>% qplot(x=date,y=ytm_avg,colour=ccy,data=.,geom='line')

# bring in the bbg prices
setkey(priceraw,date)
ussw_colnames<-priceraw %>% ds('ussw') %>% str_extract(regex('ussw.*')) %>% sort %>% unique
eusa_colnames<-priceraw %>% ds('eusa') %>% str_extract(regex('eusa.*')) %>% sort %>% unique
swapus<-priceraw[date>='1996-06-28',c('date',ussw_colnames),with=FALSE]
swapeu<-priceraw[date>='1999-01-29',c('date',eusa_colnames),with=FALSE]
swapprices<-swapus %>% left_join(swapeu,by='date')

#interpolate 
swapusl<-swapus %>% gather('tenor','ussw',-date) %>% mutate(tenor=as.numeric(str_extract(tenor,regex('\\d+')))) %>% as.data.table()
setkey(swapusl,date)
ytm_us<-swapusl[ytm_cc[ccy=='usd']]
usswyld<-ytm_us[!is.na(ussw),approx(x=tenor,y=ussw,xout=ytm_avg),by=date] %>% rename(ytm=x,usswyld=y) %>% unique %>% select(-ytm)
usswyld %>% qplot(date,usswyld,data=.,geom='line')

swapeul<-swapeu %>% gather('tenor','eusa',-date) %>% mutate(tenor=as.numeric(str_extract(tenor,regex('\\d+')))) %>% as.data.table()
setkey(swapeul,date)
ytm_eu<-swapeul[ytm_cc[ccy=='eur']]
euswyld<-ytm_eu[!is.na(eusa),approx(x=tenor,y=eusa,xout=ytm_avg),by=date] %>% rename(ytm=x,eusayld=y) %>% unique() %>% select(-ytm)
euswyld %>% qplot(date,eusayld,data=.,geom='line')


yldsprdcomp<-merge(usswyld,euswyld,by='date') %>% mutate(euus_swapsprd=eusayld-usswyld) %>% right_join(feg,by='date') %>% mutate(euus_yldsprd=euus_yld*100-euus_swapsprd) %>% 
  select(date,euus_yldsprd,euus_yld,euus_oas) %>% mutate(euus_yld=100*euus_yld) 
yldsprdcomp %>% ggplotw()
# euus_oas  :  oas directly from bloomberg
# euus_yld  : yld spread directly from bloomberg, not adjusting for underlying swap or treasury yld
# euus_yldsprd : euus_yld subtracting maturity avgd swap yield spread btw eu and us

# next step, try to generate yield sprd at the individual bond level instead of taking avg 
swappricesl<-swapprices %>% melt(id.vars='date',variable.name='field')
swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
setkey(swappricesl,date,ccy,tenor,field)
setkey(dtl3,date,ccy)
swappricesl[date=='2004-01-30' & ccy=='eur']


dtl3[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY),by=.(date,ccy)][swapyld==0,swapyld:=NA]
dtl3[,yldsprd:=value*100-swapyld]

setkey(dtl3,date,upcusip)

dfreg<-dtl3[yldsprd!='NA',.(date,ccy,upcusip,value=yldsprd,field='yldsprd')]
ccyfe_yieldsprd<-getccyFE(dfreg,fieldstr='yldsprd')
setnames(ccyfe_yieldsprd,'euus','euus_yldsprd')
ccyfe_yieldsprd %>% ggplotw()

feg2<-feg[ccyfe_yieldsprd]
feg2[,euus_yld:=NULL]
feg2 %>% ggplotw()
feg<-feg2

#save.image('temp.rdata')
load(file='temp.rdata')
source('util.r')



# compare to baml
Cdif_euus<-priceraw[,c('date',ds(priceraw,'Cdif_euus'),ds(priceraw,'eubs')),with=FALSE]

dfeu<-feg %>% left_join(Cdif_euus,by='date')
dfeu[,.(date,euus_yldsprd,Cdif_euus_30)] %>% ggplotw()

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_oas_eff,euus_yldsprd_eff,euus_oas,euus_yldsprd) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd_eff,euus_yldsprd) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd_eff,Cdif_euus_30_eff) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd,Cdif_euus_30) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% select(date,euus_yldsprd,eubs5) %>% wgplot()


raw<-read.dta13('sdc96_clean2.dta') %>% as.data.table()
df1<-raw[!is.na(raw$amt) & ccy %in% c('USD',"EUR",'AUD',"JPY",'GBP'),]
# current 


# new icollapse -----------------------------------------------------------
dfin<-df1  %>% 
  filter(amt>=50,ytofm>=2, ytofm<=99999,nrating<=16, (pub=="Public" | pub=="Sub."), 
         mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
         secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
         tf_mid_desc!='Government Sponsored Enterprises',
         nrating>1)
dfin<-dfin[ccy %in% c('USD','EUR') & amt>=100]
# retain sdc where isin/cusip matches credit spread

dfin %<>% semi_join(dtl3,by='upcusip')

#dfin %>% semi_join(dtl3,by='isin')


dfreg<-icollapse2(dfin) %>% mergebymonth(dfeu)
dfreg[,euus_yldsprd_eff:=euus_yldsprd-eubs5]
dfreg %>% write.dta('temp.dta')
dfreg[date>='2006-01-01',.(date,i_net_euus,Cdif_euus_30,euus_yldsprd)] %>% ggplotw()

dfreg<-dfreg[date>='2006-01-01']
#dfreg[,i_net_euus:=i_net_euus*100]
reg2<- dfreg %>% lm(lead(i_net_euus)~Cdif_euus_30,data=.)
reg3<- dfreg %>% lm(lead(i_net_euus)~Cdif_euus_30_eff,data=.)
reg4<- dfreg %>% lm(lead(i_net_euus)~euus_yldsprd,data=.)
reg5<- dfreg %>% lm(lead(i_net_euus)~euus_yldsprd_eff,data=.)
reg6<- dfreg %>% lm(lead(i_net_euus)~euus_oas,data=.)

stargazer(reg2,reg3,reg4,reg5,reg6,type='text',report="vct*")


dfreg[date>='2006-01-01',.(date,i_net_euus)] %>% ggplotw()


# previous stuff ----------------------------------------------------------

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


