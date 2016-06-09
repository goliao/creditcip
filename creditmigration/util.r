library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)
require('readstata13')
require('ggfortify')
require('doBy')
require(lubridate)
require(ggplot2)
require(sandwich)
require(stargazer)
require(reshape2)
require(sqldf)
require(magrittr)
require(data.table)
require(beepr)
'%ni%' <- Negate('%in%')
source('dbutil.r')
df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))

icollapse2<-function(dfin,ccyA="EUR",natA="Eurozone"){
  # newer version of collapsing 
  dfin<-dfin %>% as.data.table()
  dfin[,yrmo:=as.numeric(format(d,'%Y%m'))]
  df_fUSD<-dfin[modnat==natA & ccy=='USD',.(I_fUSD=sum(amt)/1000),by=yrmo]
  df_usF<-dfin[modnat=='United States' & ccy==ccyA,.(I_usF=sum(amt)/1000),by=yrmo]
  df_both<-dfin[modnat %in% c(natA,'United States') & ccy %in% c(ccyA,'USD'),.(I_both=sum(amt)/1000),by=yrmo]
  setkey(df_fUSD,yrmo)
  setkey(df_usF,yrmo)
  setkey(df_both,yrmo)
  df_all<-df_both %>% merge(df_fUSD,by='yrmo',all=TRUE) %>% merge(df_usF,by='yrmo',all=TRUE)
  df_all[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  df_all[,I_net_fus:=I_fUSD-I_usF][,i_net_fus:=I_net_fus/I_both]
  df_all %>% expandfulldates(.) %>%  rename(I_net_euus=I_net_fus,i_net_euus=i_net_fus) %>% as.data.table()
}

icollapse <- function(dfin,ccyA="EUR",natA="Eurozone"){
  # dfin=df2
  # ccyA="EUR"
  # natA="Eurozone"
  # 
  # collapse into monthly aggregate flows
  df_fUSD<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat==natA, ccy=='USD') %>% 
    mutate(yrmo=as.numeric(format(d,'%Y%m')),ct=1) %>% 
    group_by(yrmo) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, IN_fUSD=sum(ct), nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_fUSD=amt,nrating_fUSD=nrating,ytofm_fUSD=ytofm)
  
  df_usF<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat=='United States', ccy==ccyA) %>% 
    mutate(yrmo=as.numeric(format(d,'%Y%m')),ct=1) %>% 
    group_by(yrmo) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, IN_usF=sum(ct),nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_usF=amt,nrating_usF=nrating,ytofm_usF=ytofm)
  
  dfjoint<-df_fUSD %>% full_join(.,df_usF,by='yrmo') 
  dfjoint[,c('I_usF','I_fUSD','IN_fUSD','IN_usF')]<-apply(dfjoint[,c('I_usF','I_fUSD','IN_fUSD','IN_usF')],2, function(x){replace(x,is.na(x),0)})
  dfout<-dfjoint %>% 
    mutate(I_net_fus=I_fUSD-I_usF,IN_fus=IN_fUSD-IN_usF) %>% 
    mutate(date=as.Date(str_c(yrmo,"01"),format="%Y%m%d"))
    # tidyr::gather(.,'type','value',-year) %>% 
    # filter(type %in% c('nrating_fUSD','nrating_usF')) %>%
    # filter(type %in% c('ytofm_fUSD','ytofm_usF')) %>%
    # filter(type %in% c('I_fUSD','I_usEUR','I_net_fus')) %>%   
   if (ccyA=="EUR") dfout<-filter(dfout,yrmo>200112)
   dfout
}
icollapsedaily <- function(dfin,ccyA="EUR",natA="Eurozone"){
  # collapse into daily aggregate flows
  df_fUSD<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat==natA, ccy=='USD') %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_fUSD=amt,nrating_fUSD=nrating,ytofm_fUSD=ytofm,date=d)
  
  df_usF<-dfin %>% dplyr::tbl_df() %>% 
    filter(modnat=='United States', ccy==ccyA) %>% 
    group_by(d) %>% 
    mutate(sumamt=sum(amt),weight=amt/sum(amt)) %>% 
    summarise(amt=sum(amt)/1000, nrating=sum(nrating*weight),ytofm=sum(ytofm*weight)) %>% 
    rename(I_usF=amt,nrating_usF=nrating,ytofm_usF=ytofm,date=d)
  
  dfout<-df_fUSD %>% full_join(.,df_usF,by='date') %>% 
    replace_na(list(I_fUSD=0,I_usF=0)) %>%     
    mutate(I_net_fus=I_fUSD-I_usF) %>% 
    select(date,I_net_fus,I_usF,I_fUSD)
  if (ccyA=="EUR") dfout<-filter(dfout,date>'2001-12-31')
  dfout
}
expandfulldates<-function(dfin){
  date.max<-max(dfin$date)
  date.min<-min(dfin$date)
  all.dates <- seq(date.min, date.max, by="month")
  all.dates.frame <- data.frame(list(date=all.dates))
  dfout<-dfin %>% full_join(.,all.dates.frame,by='date') %>% 
    replace(is.na(.),0) %>% 
    arrange(date)
  dfout
  #dfin
}

icollapse_all <- function(dfin){
df_euus<- dfin %>% icollapse(.,ccyA = "EUR",natA="Eurozone") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_euus=I_net_fus,IN_euus=IN_fus) %>% 
  expandfulldates(.)
df_gbus<- dfin %>% icollapse(.,ccyA = "GBP",natA="United Kingdom") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_gbus=I_net_fus,IN_gbus=IN_fus) %>% 
  expandfulldates(.)
df_jpus<- dfin %>% icollapse(.,ccyA = "JPY",natA="Japan") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_jpus=I_net_fus,IN_jpus=IN_fus) %>% 
  expandfulldates(.)
df_auus<- dfin %>% icollapse(.,ccyA = "AUD",natA="Australia") %>% 
  select(date,I_net_fus,IN_fus) %>% 
  rename(I_net_auus=I_net_fus,IN_auus=IN_fus) %>% 
  expandfulldates(.)

df_fus<- df_euus %>% 
  full_join(.,df_gbus,by='date') %>% 
  full_join(.,df_jpus,by='date') %>% 
  full_join(.,df_auus,by='date') %>% 
  arrange(date)
df_fus
}

plotgl <- function(dfin,fields=c('I_net_euus'),afteryr=2002){
  dfin %>% 
  filter(year>=afteryr) %>%
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()

}
ggplotw<-function(dfin, fields=ds(dfin)){
  dfin %>% 
  tidyr::gather(.,'type','value',-date) %>% 
  filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()+geom_point()

}


ds<-function(dfin,matchpattern='.'){
  require(stringr)
  cn<-colnames(dfin) 
  try(cn[str_locate(as.character(cn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}

wgplot <- function(dfwide,idvar='date'){
  dfwide %>% 
  reshape2::melt(.,id.vars=idvar) %>%
  #filter(type %in% fields) %>% 
  ggplot(data=.,aes(x=date,y=value,colour=variable))+geom_line()+geom_point()
}


issfilter <- function(df_issraw){

  df_issraw  %>% 
  filter(amt>=50,ytofm>=2, ytofm<=99999,nrating<=16, (pub=="Public" | pub=="Sub."), 
         mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
         secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
         tf_mid_desc!='Government Sponsored Enterprises',
         nrating>1)
}


tabulate <- function(dfin,byvar='variable'){
  # dfin %>% group_by_(byvar) %>% dplyr::summarise_(count=length(byvar)) %>% arrange(desc(count))
  table(dfin[byvar]) %>% as.data.frame() %>%  tbl_df() %>% arrange(desc(Freq))
}

unpackbbgprices<-function(prices){
  # use when prices are batched in a list of say 20 groups
  a0 <- unlist(prices, recursive = FALSE)
tickernames <- names(a0)
df_prices <- data.frame() %>% tbl_df()
for (i in 1:length(tickernames)) {
  temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
  if (nrow(temp_new) == 0)
    print (str_c('empty:#', i, ' name:', tickernames[i]))
  df_prices %<>% dplyr::bind_rows(., temp_new)
}
df_prices
}



requestfigibyisin<-function(df_isins){
 # given a dataframe of isins, get a dataframe of isin and figi mappings 
  require('magrittr')
  require('httr')
require('jsonlite')
#require('tidyjson')
# figireq<-'[{"idType":"ID_ISIN","idValue":"XS1033736890"},
# {"idType":"ID_BB_UNIQUE","idValue":"JK354407"},
# {"idType":"ID_BB","idValue":"JK354407"},
# {"idType":"COMPOSITE_ID_BB_GLOBAL","idValue":"JK354407"},
# {"idType":"TICKER","idValue":"JK354407 Corp"},
# {"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'
# # figireq<-'[{"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'
print(str_c('min est:',nrow(df_isins)/10000))
  ptm <- proc.time()
  counter <- 0 # count request up to 100, for figi limit of 100 request per minute
  df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
  diag_response<-list()
  for (j in 1:ceiling(nrow(df_isins)/100)){
    counter <- counter+1
    if (counter==100){
     save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
       print(str_c("row (j):",j*100))
       # if ((proc.time() - ptm)[[3]]<=60){ # let it sleep to a full minute if it hasn't been a full minute
         print(str_c('last cycle:',(proc.time() - ptm)[[3]]))
         print(str_c('sleeping max of full 60 sec or next min:',Sys.time()))
         Sys.sleep(max(61-(Sys.time() %>% second()),61-((proc.time() - ptm)[[3]])))
         print(str_c('awake:',Sys.time()))
         #counter %>% print
         ptm <- proc.time()
         counter <- 0
    #    } else { # continue and reset counters and time
    #   ## let it sleep till the next minute regardless
    #     print(str_c('sleeping till next min:',proc.time()[[3]]))
    #     Sys.sleep(60-Sys.time() %>% second())
    #     ptm <- proc.time()
    #     print(str_c('awake:',proc.time()[[3]]))
    #     counter <- 0
    # } 
    }
    tempreq <- df_isins %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
    figireq<- tempreq %>%  mutate(idType='ID_ISIN',idValue=isin) %>% select(-isin)  %>% jsonlite::toJSON() 
    r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
    # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
    tryCatch({
      responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
    }, error = function(err) {
      print(r %>% content(as = "text") %>% str_sub(.,1,50))
      
      counter <- 0
      print(str_c('sleeping on error:',Sys.time()))
      Sys.sleep(60)
      ptm <- proc.time()
      print(str_c('awake:',Sys.time()))
      r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
      tryCatch({
        responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
      }, error=function(err){
        error('repeated request error, stopping')
        stop()
      })
    })
    
    diag_response[j]<-responsejson
    # extract 100x100 results at a time
    temp_isin2figi<-as.data.frame(list()) %>% tbl_df()
    if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
    for  (i in 1:nrow(responsejson)){
      if (ncol(responsejson)==1) { # only data column
        temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
      } else{ # data column and error column #####something is not right
        if (is.na(responsejson$error[i][[1]]))
          temp_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
        else
          temp_isin2figi %<>% bind_rows(.,data_frame(isin=tempreq$isin[i][[1]]))
      }
    }
    if (nrow(temp_isin2figi)<nrow(tempreq)) browser()
    df_isin2figi_all %<>% bind_rows(.,temp_isin2figi)
  }
  #this is the isin to figi mapping that contains 
  list(df_isin2figi_all,diag_response)
}

countdups<-function(dfin,field='isin'){
  (dfin %>% nrow)-(dfin %>% distinct_(field) %>% nrow)
}


loadBBGdownload2df<-function(filename='../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData'){
  # takes in a filename associated with bbg download; spits out price dataframe with field as colnames and a variable type
  load(filename)
  a0 <- unlist(prices, recursive = FALSE)
  # old slow version
  # tickernames <- names(a0)
  # df_prices <- data.frame() %>% tbl_df()
  # for (i in 1:length(tickernames)) {
  #   temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
  #   if (nrow(temp_new) == 0)
  #     print (str_c('empty:#', i, ' name:', tickernames[i]))
  #   df_prices <- df_prices %>% dplyr::bind_rows(., temp_new)
  # }
  # df_prices %>% rename(parsekeyable=ticker)
  dtw<-data.table::rbindlist(a0,use.names=TRUE,idcol=TRUE)
  dtl<-melt(dtw,id.vars=c('date','.id'),variable.name='field')[!is.na(value)]
  setnames(dtl,'.id','parsekeyable')
  setkey(dtl,date,parsekeyable)
  dtl
}

melt.gl<-function(dfin,idvars=c('date','parsekeyable','batch')){
  #Deprecated: not useful: a wrapper for melt that sets measure.vars to be anything other than idvars
  setkey_(dfin,idvars)
  dtl_out<-melt(dfin,id.vars=idvars,measure.vars=((dfin %>% ds)[(dfin %>% ds) %ni% idvars]),
              variable.name='field')[!is.na(value)]
}

assessDataCoverage<-function(bondinfo,bondprices,field='YLD_YTM_MID',lastdate='lastobs',startdate='firstobs'){
  # check monthly data coverage; bondinfo is essentially sdc infomation on maturity etc, bondprice is from bbg download
  # 
  #   bondprices<-df_p %>% filter(date>='2005-01-01',date<='2016-04-01') 
  #   bondinfo<-df_sdc_all
  #   field="YLD_YTM_MID"
  # field="BLP_Z_SPRD_MID"
  # lastdate='lastobs'
  # startdate='firstobs'
  # #   
  #count unique bond tickers
  if (startdate=='firstobs') {
    startdate = min(bondprices$date)
    str_c('startdate: ', startdate) %>% print
  }
  if (lastdate == 'lastobs') {
    lastdate = max(bondprices$date)
    str_c('lastdate: ', lastdate) %>% print
  }
  
  
  if ('parsekeyable' %ni% (bondprices %>% ds)) bondprices %<>% rename(parsekeyable=ticker) 
  str_c('unique securities: ',bondprices %>% tabulate('parsekeyable') %>% nrow) %>% print
  
  # add expected number of months
  bondinfo %<>% distinct(parsekeyable) %>%  mutate(expmonthlyobs=ceiling((pmin(as.numeric(lastdate),as.numeric(mat2))-pmax(as.numeric(d),as.numeric(startdate)))/30.5)) 
  # count number of observations by isin # compare to number of expected obs by isin
  bondtickers <- bondprices %>% group_by(parsekeyable) %>% summarise(obs_allflds=length(parsekeyable))
  fldfreq<-bondprices[!is.na(bondprices[field]),] %>% select_('date','parsekeyable',field) %>% group_by(parsekeyable) %>% summarize(obs_specfld=length(date))
  bondtickers %<>% left_join(fldfreq,by='parsekeyable')
  bondtickers %<>% mutate(obs_specfld=ifelse(is.na(obs_specfld),0,obs_specfld))
  df_obs<-bondtickers %>% inner_join(bondinfo,by='parsekeyable') %>% mutate(obsdiff=expmonthlyobs-obs_specfld, obscoverage=ifelse(expmonthlyobs>=12,obs_specfld/expmonthlyobs,1)) %>% select(parsekeyable,isin,obsdiff,obscoverage,obs_allflds,obs_specfld,expmonthlyobs,mat2,d,settlement2)
  print('coverage:')
  str_c('# obs matching sdc:',df_obs %>% nrow()) %>% print
  df_obs$obscoverage %>% summary %>% print
  ggplot(df_obs,aes(x=obscoverage))+stat_ecdf()
  print('all fld obs diff from expected number of obs')
  df_obs %>% mutate(allflddiff=expmonthlyobs-obs_allflds) %$%summary(allflddiff) %>% print
  df_obs
  # df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
  # df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View 
}
showdups<-function(dfin,key='figi'){
  # duplicatedkey<-isin2figi[[key]][duplicated(isin2figi[[key]])]
  # multiple
  dupindex<-duplicated(dfin[,key])
  duplicatedkey<-dfin[dupindex,key]
  dfin %>% semi_join(duplicatedkey,by=key)
}


showdups.dt<-function(dfin,vars=key(dfin)){
  print('showing duplicates in:')
  print(vars)
  oldkey<-key(dfin)
  setkeyv(dfin,vars)
  dupkey<-dfin[,.N,by=vars][N>1,vars,with=FALSE]
  dfout<-dfin[dupkey]
  if(length(oldkey)>0) {
    if (!identical(oldkey,vars)) setkeyv(dfin,oldkey)
  }
  else {
    warning('set key(s) to vars:',vars)
  }
  dfout
}



openfigi.response2DT<-function(res){
  # not used
  aa<-list()
  # i=1
  for (i in 1:length(res)){
    aa[[i]]<-data.table()
    # tryCatch(aa[[i]]<-rbindlist(res[[i]]))
     for (j in 1:length(res[[i]])){
      aa[[i]]<-rbind(aa[[i]],data.table(res[[i]][[j]]))
    }
    print(i)
  }
  bb<-rbindlist(aa,fill=TRUE)
  bb
}


getccyFE<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.05){
#  dfin<-dtl2
print(str_c('FE on field: ',fieldstr))
  df2<-dfin[field==fieldstr,.(date,ccy,value,upcusip)]
  setkey(df2,date,upcusip,ccy)


  # Version 0: demean method
  # df2[,upmean:=mean(value),by=.(date,upcusip)]
  # # demeaned oas spread for each bond; demeaning issuer*time specific means; residualize and aggregate up to ccy level
  # df2_ccy<-df2[,.(date,ccy,demeaned=value-upmean)][demeaned!=0,.(demean_ccy=mean(demeaned)),by=.(date,ccy)]
  # #create a spread
  # df2_ccy[,euus:=(.SD[ccy=='eur',demean_ccy]-.SD[ccy=='usd',demean_ccy]),by=.(date)]
  # df2_ccy %>% ggplot(aes(x=date,y=euus))+geom_line()
#winsorize each date
  if (winsor!=0){
    df2[,pctl:=percent_rank(value),by=date]
    df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
  }

    #get rid of days with only single observation
  df2<-df2[date %ni% df2[,.N,by=c('date','ccy')][N==1,date]]

  # version 1:: run regression directly on data set without taking out bonds that do not have matching pairs
  if (version==1){
    regcoef<-df2[,lm(value~ccy+upcusip,data=.SD)$coefficients['ccyusd'],by='date']
  } else if (version==2){
  # version 2:: FAST: get rid of price obs with only single ccy per date per upcusip; that is, obs is counted only when upcusip has both ccys
    tokeep<-df2[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    regcoef<-df2[tokeep][,lm(value~ccy+upcusip,data=.SD)$coefficients['ccyusd'],by='date']
  }
  require(beepr)
  beep()
  regcoef[,`:=`(euus=-V1,V1=NULL)]
  #save(regcoef,regcoef2,file='temp_ccyferegcoef.rdata')
  #load(file='temp_ccyferegcoef.rdata')
  regcoef

}
intrwrap<-function(dfin,sp,bylist){
  splocal<-sp[date==bylist$date & ccy==bylist$ccy]
  if (nrow(splocal)==0) {
    print(str_c('NAs on',bylist$date,bylist$ccy))
    #browser()
    rep(0,nrow(dfin))
  } else{
    tryCatch(dfout<-approx(x=splocal$tenor,y=splocal$value,xout=dfin$ytm),
             error=function(err){
               print(err)
               browser()
             })
    dfout$y
  }
}
mergebymonth<-function(dfin1,dfin2){
  dfin1<-as.data.table(dfin1)
  dfin2<-as.data.table(dfin2)
  dfin1[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfin2[,yrmo:=as.numeric(format(date,'%Y%m'))]
  dfjoin<-merge(dfin1,dfin2,by='yrmo')
  dfjoin[,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  dfjoin
}
winsorize<-function(dfin){
  dfin[,pctl:=percent_rank(value)][pctl>=.01 & pctl<=.99]
  # tempdf<-dti %>% mutate(pctl=percent_rank(value))
  # tempdf[pctl>=.01 & pctl<=.99]
}

# dreg1<-df2[date=='2004-01-30']
# dreg2<-df2[tokeep][date=='2004-01-30']
# reg1<-lm(value~ccy+upcusip,data=dreg1)
# reg2<-lm(value~ccy+upcusip,data=dreg2)

# summary(reg1)
# summary(reg2)

# # compare v1 & v2
# setnames(regcoef2,'V1','V2')
# tmp<-merge(regcoef,regcoef2,by='date') %>% melt(id.vars='date')
# setkey(tmp,date,variable)
# tmp
# tmp %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()

# # compare v1 and demeaning method
# temp_comp<-merge(regcoef,dtoas_ccy,by='date')
# temp_comp[,euussprd:=-V1]
# temp_comp %>% melt(id.vars='date', measure.vars=c('euussprd','oas_euus')) %>%  ggplot(aes(x=date,y=value,colour=variable))+geom_line()


# # Let's try cross sectional reg for one single date
# dailyregdata<-df2[date=='2004-01-30']
# tdg<-dailyregdata[upcusip %in% (dailyregdata[,.N,by=c('upcusip','ccy')][,.N,by='upcusip'][N!=1,upcusip])]
# lm(value~ccy+upcusip,dailyregdata)$coefficients['ccyusd']
# lm(value~ccy+upcusip,tdg)$coefficients['ccyusd']

# #small sample test
# testsamp<-tdg[5:9,.(value,ccy,upcusip)]
# setkey(testsamp,upcusip,ccy)
# lm(value~ccy+upcusip,testsamp)$coefficients['ccyusd']
# lm(value~ccy+upcusip,testsamp)
# #lm(value~ccy+upcusip+ccy*upcusip,testsamp)
# testres<-testsamp[,upmean:=mean(value),by=upcusip][,demeaned:=value-upmean][,mean(demeaned),by='ccy']
# -(testres[ccy=='eur',V1]-testres[ccy=='usd',V1])

# reg1<-lm(value~upcusip,testsamp)
# testsamp$res<-reg1$residuals
# lm(res~ccy,testsamp)

# lm(demeaned~ccy,testsamp)
# testsamp %>% write.csv(file=('temp.csv'))
