require(foreign)
require(stringr)
require(xts)
require(tidyr)
require(dplyr)
require('readstata13')
#require('ggfortify')
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
require(ggthemes)
'%ni%' <- Negate('%in%')
#source('dbutil.r')
df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))

icollapse3<-function(dtin,ccyA="eur",natA="Eurozone"){
  # newer version of collapsing 
  # todo: construct and use modupccy
  dtin<-copy(dtin)
  dtin[,yrmo:=as.numeric(format(d,'%Y%m'))]
  df_fUSD<-dtin[modnat==natA & ccy=='usd',.(I_fUSD=sum(amt)/1000),by=yrmo]
  df_usF<-dtin[modnat=='United States' & ccy==ccyA,.(I_usF=sum(amt)/1000),by=yrmo]
  df_both<-dtin[modnat %in% c(natA,'United States') & ccy %in% c(ccyA,'usd'),.(I_both=sum(amt)/1000),by=yrmo]
  setkey(df_fUSD,yrmo)
  setkey(df_usF,yrmo)
  setkey(df_both,yrmo)
  df_all<-df_both %>% merge(df_fUSD,by='yrmo',all=TRUE) %>% merge(df_usF,by='yrmo',all=TRUE)
  df_all[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][,date:=as.Date(str_c(yrmo,"01"),format="%Y%m%d")]
  df_all[,I_net_fus:=I_fUSD-I_usF][,i_net_fus:=I_net_fus/I_both]
  df_all %>% expandfulldates(.) %>%  rename(I_net_euus=I_net_fus,i_net_euus=i_net_fus) %>% as.data.table()
}

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
  ggplot(data=.,aes(x=date,y=value,colour=type))+geom_line()

}
ggplotl<-function(dfin){
  dfin %>% ggplot(data=.,aes(x=date,y=value,colour=ticker))+geom_line()+geom_point()
}

ds<-function(dfin,matchpattern='.'){
  require(stringr)
  cn<-colnames(dfin) 
  try(cn[str_locate(as.character(cn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
}
dsl<-function(dtlin,matchpattern='.',field='ticker'){

  #fn<-(dtlin[,ticker] %>% unique())[[1]]
   dtlin<-copy(dtlin)
   setkeyv(dtlin,field)
   fn<-unique(dtlin)[,field,with=F][[1]]
  
  try(fn[str_locate(as.character(fn),regex(matchpattern))[,1]>0] %>% na.omit() %>% as.character())
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
# showdups<-function(dfin,key='figi'){
#   # duplicatedkey<-isin2figi[[key]][duplicated(isin2figi[[key]])]
#   # multiple
#   dupindex<-duplicated(dfin[,key])
#   duplicatedkey<-dfin[dupindex,key]
#   dfin %>% semi_join(duplicatedkey,by=key)
# }


showdups<-function(dfin,vars=key(dfin)){
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


intrwrap<-function(dfin,sp,bylist,interprule=1){
#wrapper function for interpolation
  splocal<-sp[date==bylist$date & ccy==bylist$ccy]
  if (nrow(splocal)==0) {
    print(str_c('NAs on',bylist$date,bylist$ccy))
    #browser()
    rep(0,nrow(dfin))
  } else{
    tryCatch(dfout<-approx(x=splocal$tenor,y=splocal$value,xout=dfin$ytm,rule=interprule),
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



resyldsprd<-function(dtlin,pricein,regversion=2){
  # Residualize yld sprd ----------------------------------------------------
  #create yield spread for aggregate 

  dtlin[,ytm:=as.numeric((mat2-date)/365)]
  #winsorize by value a little
  dtl<-dtlin[field=='YLD_YTM_MID',pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99 & field=='YLD_YTM_MID']
  # get rid of up where up doesn't have bonds in both ccys for each date
  tokeep<-dtl[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
  setkey(tokeep,date,upcusip)
  setkey(dtl,date,upcusip)
  dtl<-dtl[tokeep]
  
  # next step, try to generate yield sprd at the individual bond level instead of taking avg 
  # bring in the bbg prices
  setkey(pricein,date)
  ussw_colnames<-pricein %>% ds('ussw') %>% str_extract(regex('ussw.*')) %>% sort %>% unique
  eusa_colnames<-pricein %>% ds('eusa') %>% str_extract(regex('eusa.*')) %>% sort %>% unique
  swapus<-pricein[date>='1996-06-28',c('date',ussw_colnames),with=FALSE]
  swapeu<-pricein[date>='1999-01-29',c('date',eusa_colnames),with=FALSE]
  swapprices<-swapus %>% left_join(swapeu,by='date')
  
  swappricesl<-swapprices %>% melt(id.vars='date',variable.name='field')
  swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
  setkey(swappricesl,date,ccy,tenor,field)
  setkey(dtl,date,ccy)
  
  dtl[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[,value:=value*100-swapyld][,field:='yldsprd']
  setkey(dtl,date,upcusip)
  #dfreg<-dtl[yldsprd!='NA',.(date,ccy,upcusip,value=yldsprd,field='yldsprd',ytm,rating_bucket)]  #old 
  dtl<-dtl[value!='NA']
  ccyfe_yieldsprd<-getccyFE(dtl,fieldstr='yldsprd',version=regversion)
  setnames(ccyfe_yieldsprd,'euus','euus_yldsprd')
  ccyfe_yieldsprd
}
getccyFE<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.01){
#  dfin<-dtl2
  print(str_c('FE on field: ',fieldstr))
  # df2<-dfin[field==fieldstr,.(date,ccy,value,upcusip,ytm,rating_bucket)]
  df2<-dfin[field==fieldstr]
  setkey(df2,date,upcusip,ccy)

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
  } else if (version==3){
  # version 3: like version 2 but also adds maturity considerations in regression
    tokeep<-df2[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    regcoef<-df2[tokeep][,lm(value~ccy+upcusip+ytm,data=.SD)$coefficients['ccyusd'],by='date']
  } else if (version==3.1){
  # version 3: like version 2 but also adds maturity considerations in regression
    tokeep<-df2[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    regcoef<-df2[tokeep][,lm(value~ccy+upcusip+ytm_bucket,data=.SD)$coefficients['ccyusd'],by='date']
  } else if (version==4){
  # version 4: version 3+ 3 rating buckets as dummies
    tokeep<-df2[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    regcoef<-df2[tokeep][,lm(value~ccy+upcusip+ytm_bucket+rating_bucket,data=.SD)$coefficients['ccyusd'],by='date']
  } else if (version==5){
  # version 4: version 3+ 3 rating buckets as dummies
    tokeep<-df2[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    regcoef<-df2[tokeep][,lm(value~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=.SD)$coefficients['ccyusd'],by='date']
  }
  require(beepr)
  beep()
  regcoef[,`:=`(euus=-V1,V1=NULL)]
  #save(regcoef,regcoef2,file='temp_ccyferegcoef.rdata')
  #load(file='temp_ccyferegcoef.rdata')
  regcoef
}
extractpricefromdtl<-function(dtl,fieldstr){
  
  # colnames<-pricein %>% ds('ussw') %>% str_extract(regex('ussw.*')) %>% sort %>% unique
  # eusa_colnames<-pricein %>% ds('eusa') %>% str_extract(regex('eusa.*')) %>% sort %>% unique
  # bpsw_colnames<-pricein %>% ds('bpsw') %>% str_extract(regex('bpsw.*')) %>% sort %>% unique
  # swapus<-pricein[date>='1996-06-28',c('date',ussw_colnames),with=FALSE]
  # swapeu<-pricein[date>='1999-01-29',c('date',eusa_colnames),with=FALSE]
  # swapgb<-pricein[date>='1996-06-28',c('date',bpsw_colnames),with=FALSE]
}
filterglobaluponly<-function(dtlin){
    dtl<-copy(dtlin)
    tokeep<-dtl[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N!=1][,.(date,upcusip)]
    setkey(tokeep,date,upcusip)
    setkey(dtl,date,upcusip)
    dtl[tokeep]
}
preprocess<-function(bondref,dtl,prl,monthlyonly=TRUE,issfiltertype=2){
  tic()
  dtl<-copy(dtl)
  prl<-copy(prl)
  prl<-backfillNAs.prl(prl) # fill eubsv 3s6s basis backwarks a bit
  br<-bondref[!is.na(pk)] %>% issfilter(.,type=issfiltertype)
  br <- br %>% semi_join(dtl,by='pk') %>% as.data.table()
  setkey(br,pk)
  if (nrow(showdups(br,'pk'))!=0){
    message('error: duplicate pks; dedupe based on amt')
    br<-br[order(pk,-amt)]
    setkey(br,pk)
    br<-unique(br)
  }

  # MERGE RATING AND ADD MATURITY BUCKETS -----------------------------------
  setkey(dtl,pk);setkey(br,pk)
  if (monthlyonly) {
    dtl<-dtl[monthend==1]
    prl<-prl[monthend==1]
  } else{
    print('daily data')
    
    pk_daily0<-unique(dtl[monthend==0,.(pk)])
    # alternatively, count pk as daily obs if there are more than three times as many daily obs as monthly obs
    pkcount<-dtl[,.N,.(pk,monthend)] %>% dcast(pk~monthend)
    pk_daily<-pkcount[`0`>3*`1`,.(pk)]
    setkey(pk_daily,pk)
    dtl<-dtl[pk_daily]
  }
  dtl2<-dtl[br[,.(ccy,mat2,nrating,upcusip,pk,ytofm,sicfac,sic1,amt)],nomatch=0]
  dtl2[,ytm:=as.numeric((mat2-date)/365)]
  dtl3<-dtl2[ytm >.05]
  dtl3[is.na(nrating),nrating:=0]
  dtl3<-dtl3[ccy %ni% c('sek','nok')] # don't know how these appeared, but let's get rid of them.
  if ('field' %in% ds(dtl)) dtl3<-dtl3[field=='YLD_YTM_MID']
  dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
  prl<-prl[date>'2002-01-01']
  prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
  prw<-prl %>% distinct() %>% data.table::dcast(.,date~ticker,value.var = 'value')
  dtl3<-dtl3[date>'2004-01-01']
  dtl3[,liq:=ytm/ytofm]
  dtl3<-dtl3[liq %between% c(0.05,1.0)]
  dtl3[liq<.5,liq_bucket:=0] # more illiq
  dtl3[liq>=.5,liq_bucket:=1] # liq
  # gen eusw=eusa-eubsv
  # gen eusz=eusw+eubs
  prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
  prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
  prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
  prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
  prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
  prw[,`:=`(cdsz1=cdsw1+cdbs1,cdsz2=cdsw2+cdbs2,cdsz3=cdsw3+cdbs3,cdsz4=cdsw4+cdbs4,cdsz5=cdsw5+cdbs5,cdsz6=cdsw6+cdbs6,cdsz7=cdsw7+cdbs7,cdsz8=cdsw8+cdbs8,cdsz9=cdsw9+cdbs9,cdsz10=cdsw10+cdbs10,cdsz12=cdsw12+cdbs12,cdsz15=cdsw15+cdbs15,cdsz20=cdsw20+cdbs20,cdsz25=cdsw25+cdbs25,cdsz30=cdsw30+cdbs30)]
  prw[,`:=`(sfsz1=sfsw1+sfbs1,sfsz2=sfsw2+sfbs2,sfsz3=sfsw3+sfbs3,sfsz4=sfsw4+sfbs4,sfsz5=sfsw5+sfbs5,sfsz6=sfsw6+sfbs6,sfsz7=sfsw7+sfbs7,sfsz8=sfsw8+sfbs8,sfsz9=sfsw9+sfbs9,sfsz10=sfsw10+sfbs10,sfsz12=sfsw12+sfbs12,sfsz15=sfsw15+sfbs15,sfsz20=sfsw20+sfbs20,sfsz25=sfsw25+sfbs25,sfsz30=sfsw30+sfbs30)]
  prw[,approx_eubs1:=(eur/(eur+eur12m/10000)*(1+ussw1/10000)-(1+eusw1/10000))*10000]
  prw[,approx_eubs5:=((eur/(eur+eur5y/10000))^(1/5)*(1+ussw5/10000)-(1+eusw5/10000))*10000]
  # transform prw back to prl
  prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')
  prl<-prl[!is.na(value)]
  #browser()
  dtl4<-dtl.addswapsprd(dtl3,prl)
################
  toc()
  list('prw'=prw,'prl'=prl,'dtl4'=dtl4,'br'=br)
}
#new: windsorize by date and ccy
resyldsprdv3new<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=0,approxrule=1,adjccybs=0){
  # v3 improvement: using swap data
  # Residualize yld sprd ----------------------------------------------------
  #create yield spread for aggregate 
  dtl<-copy(dtlin[field=='YLD_YTM_MID'])
  dtl[,ytm:=as.numeric((mat2-date)/365)]
  #winsorize by value a little
  #[,pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99]
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

  if (globaluponly){
  # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  
  # next step, try to generate yield sprd at the individual bond level instead of taking avg 
  # bring in the bbg prices
  
  if (adjccybs==1){
    message('adj. for ccy basis')
    swappricesl<-pricein[ticker %like% '^ussw' | ticker %like% '^eusz' | ticker %like% '^bpsz' | ticker %like% '^jysz' | ticker %like% '^adsz' | ticker %like% '^cdsz\\d+' | ticker %like% '^sfsz\\d+',.(date,ticker,value)] 
  } else{ # just getting swap spread
    swappricesl<-pricein[ticker %like% '^\\w\\wsw\\d+'][ticker %like% '^ussw' | ticker %like% '^eusw' | ticker %like% '^bpsw' | ticker %like% '^jysw' | ticker %like% '^adsw' | ticker %like% '^cdsw\\d+' | ticker %like% '^sfsw\\d+',.(date,ticker,value)]   
  }
  swappricesl<-swappricesl[ticker!='euswec' & ticker!='bpswsc'] # get rid of 3 month
  setnames(swappricesl,'ticker','field')
  swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
  #swappricesl[,.N,ticker][,.(field,tictenor=str_sub(ticker,5))] 
  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')
  setkey(swappricesl,date,ccy,tenor,field)
  setkey(dtl,date,ccy)
  
  dtl[!is.na(ytm),swapyld:=intrwrapnew(.SD,swappricesl,.BY,interprule=approxrule),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[,value:=value*100-swapyld][,field:='yldsprd']
  setkey(dtl,date,upcusip)

  dtl<-dtl[value!='NA'] #get rid of ones that can't be interpolated for one reason or another
  lsout<-getccyFE2new(dtl,fieldstr='yldsprd',version=regversion)
  if (returndt==1)
    lsout
  else
    lsout[[1]]
}
intrwrapnew<-function(dfin,sp,bylist,interprule=1){
#wrapper function for interpolation
  splocal<-sp[date==bylist$date & ccy==bylist$ccy]
  if (nrow(splocal)<3) {
    if (bylist$date %between% c(2,6)) 
      print(str_c('No swap data; NAs on ',bylist$date,bylist$ccy))
    #browser()
    rep(0,nrow(dfin))
  } else{
    tryCatch(dfout<-approx(x=splocal$tenor,y=splocal$value,xout=dfin$ytm,rule=interprule),
             error=function(err){
               print(err)
               print(bylist)
               browser()
             })
    dfout$y
  }
}
resyldsprdv3<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=0,approxrule=1,adjccybs=0){
  # v3 improvement: using swap data
  # Residualize yld sprd ----------------------------------------------------
  #create yield spread for aggregate 
  dtl<-copy(dtlin[field=='YLD_YTM_MID'])
  dtl[,ytm:=as.numeric((mat2-date)/365)]
  #winsorize by value a little
  #[,pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99]
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

  if (globaluponly){
  # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  
  # next step, try to generate yield sprd at the individual bond level instead of taking avg 
  # bring in the bbg prices
  
  if (adjccybs==1){
    message('adj. for ccy basis')
    swappricesl<-pricein[ticker %like% '^ussw' | ticker %like% '^eusz' | ticker %like% '^bpsz' | ticker %like% '^jysz' | ticker %like% '^adsz' | ticker %like% '^cdsz\\d+' | ticker %like% '^sfsz\\d+',.(date,ticker,value)] 
  } else{ # just getting swap spread
    swappricesl<-pricein[ticker %like% '^\\w\\wsw\\d+'][ticker %like% '^ussw' | ticker %like% '^eusw' | ticker %like% '^bpsw' | ticker %like% '^jysw' | ticker %like% '^adsw' | ticker %like% '^cdsw\\d+' | ticker %like% '^sfsw\\d+',.(date,ticker,value)]   
  }
  swappricesl<-swappricesl[ticker!='euswec' & ticker!='bpswsc'] # get rid of 3 month
  setnames(swappricesl,'ticker','field')
  swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][ccy=='cd',ccy:='cad'][ccy=='sf',ccy:='chf'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
  #swappricesl[,.N,ticker][,.(field,tictenor=str_sub(ticker,5))] 
  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')
  setkey(swappricesl,date,ccy,tenor,field)
  setkey(dtl,date,ccy)
  
  dtl[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY,interprule=approxrule),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[,value:=value*100-swapyld][,field:='yldsprd']
  setkey(dtl,date,upcusip)

  dtl<-dtl[value!='NA'] #get rid of ones that can't be interpolated for one reason or another
  lsout<-getccyFE2(dtl,fieldstr='yldsprd',version=regversion)
  if (returndt==1)
    lsout
  else
    lsout[[1]]
}
resyldsprdv2<-function(dtlin,pricein,regversion=2,globaluponly=1,returndt=0){
  # Residualize yld sprd ----------------------------------------------------
  #create yield spread for aggregate 
  dtl<-copy(dtlin[field=='YLD_YTM_MID'])
  dtl[,ytm:=as.numeric((mat2-date)/365)]
  #winsorize by value a little
  #[,pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99]
  # get rid of dates with only one ccy
  setkey(dtl,date)
  dtl<-dtl[dtl[,.N,by=c('date','ccy')][,.N,date][N!=1,.(date)]]

  if (globaluponly){
  # get rid of up where up doesn't have bonds in both ccys for each date
    dtl<-filterglobaluponly(dtl)
  }
  
  # next step, try to generate yield sprd at the individual bond level instead of taking avg 
  # bring in the bbg prices
  swappricesl<-pricein[ticker %like% '^ussw' | ticker %like% '^eusa' | ticker %like% '^bpsw' | ticker %like% '^jysw' | ticker %like% '^adsw',.(date,ticker,value)] 
  setnames(swappricesl,'ticker','field')
  swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][ccy=='bp',ccy:='gbp'][ccy=='jy',ccy:='jpy'][ccy=='ad',ccy:='aud'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
  #swappricesl[,.N,ticker][,.(field,tictenor=str_sub(ticker,5))] 
  swappricesl[field=='bpswsc',tenor:=.25]
  if (swappricesl[is.na(tenor),.N]!=0) warning('swappricesl has tenor not parsed')
  setkey(swappricesl,date,ccy,tenor,field)
  setkey(dtl,date,ccy)
  
  dtl[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY),by=.(date,ccy)][swapyld==0,swapyld:=NA]
  dtl[,value:=value*100-swapyld][,field:='yldsprd']
  setkey(dtl,date,upcusip)

  dtl<-dtl[value!='NA'] #get rid of ones that can't be interpolated for one reason or another
  lsout<-getccyFE2(dtl,fieldstr='yldsprd',version=regversion)
  if (returndt==1)
    lsout
  else
    lsout[[1]]
}
getccyFE2new<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.01){
  #  Generalized function for calculating FE 
  print(str_c('FE on field: ',fieldstr))
  if ('field' %in% ds(dfin)) { # if dfin is in the long format with a field called 'field'
    df2<-dfin[field==fieldstr]
    lhs<-'value'
  } else { # if dfin is in the semi-wide format with a column called fieldstr
    df2<-copy(dfin)
    lhs<-fieldstr
  }
  setkey(df2,date,upcusip,ccy)
  #winsorize each date
  if (winsor!=0){
    df2[,pctl:=percent_rank(eval(exparse(lhs))),by=.(date,ccy)]
    df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
  }
    #get rid of days with only single observation and ones with only one ccy
    #df2<-df2[date %ni% df2[,.N,by=c('date','ccy')][N==1,date]]
    #df2<-df2[date %ni% df2[,.N,.(date,ccy)][,.N,date][N==1,date]]

  # set alphabetical order such that dummies are on foreign ccys
    df2[ccy=='usd',ccy:='1usd']
          
  # introduce liquidity measure based on bond age
    df2[,liq:=ytm/ytofm]
    df2<-df2[liq %between% c(0,1.1)]
    df2[liq<.5,liq_bucket:=0] # more illiq
    df2[liq>=.5,liq_bucket:=1] # liq
  regfun<-function(dt,ccylist,regversion=1,bylist){
    tryCatch({
        if (regversion==1){
          # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
          reg<-lm(eval(exparse(lhs))~ccy+upcusip,data=dt)
        } else if (regversion==3){
          # regversion 3: like regversion 2 but also adds maturity considerations in regression
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket,data=dt)
        } else if (regversion==4){
          # regversion 4: regversion 3+ 3 rating buckets as dummies
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket,data=dt)
        } else if (regversion==5){
         # regversion 5: regversion 3+ 3 rating buckets as dummies
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
        } else if (regversion==6){
         # regversion 6, add illiqudity index
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
        } else if (regversion==7){
         # regversion 7, like 6 but w/o sicfac
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+liq_bucket,data=dt)
        } else if (regversion==8){
         # regversion 8, like 7 but only focus on liq
          reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+liq_bucket,data=dt)
        }
    }, error=function(err){
      print(err)
      print(bylist)
      reg<-data.table(coefficients=as.numeric(NA))
      browser()
    })
      dtcoef<-data.frame(as.list(reg$coefficients)) %>% select(starts_with('ccy')) %>%  as.data.table()
      missccy<-ccylist[str_c('ccy',ccylist) %ni% c('ccy1usd',ds(dtcoef,'ccy'))]
      if (length(missccy)>0){ # if missing ccy on a particular date
        for (iccy in missccy){
          dtcoef[,str_c('ccy',iccy):=as.numeric(NA)]
        }
      }
      dtcoef
  }

  ccylist<-(df2 %>% distinct(ccy) %>% select(ccy))[[1]]
  regcoef<-df2[,regfun(.SD,ccylist,version,.BY),by='date']
  setkey(regcoef,date)
  lsout<-list('regcoef'=regcoef,'dtreg'=df2)
  lsout
}
## OLD; original version used at student presentation; windsorize by date only
getccyFE2<-function(dfin,fieldstr='OAS_SPREAD_BID',version=2,winsor=.01){
  #  dfin<-dtl2
  print(str_c('FE on field: ',fieldstr))
  # df2<-dfin[field==fieldstr,.(date,ccy,value,upcusip,ytm,rating_bucket)]
  df2<-dfin[field==fieldstr]
  setkey(df2,date,upcusip,ccy)

  #winsorize each date
  if (winsor!=0){
    df2[,pctl:=percent_rank(value),by=date]
    df2<-df2[pctl>=winsor & pctl<=(1-winsor)]
  }
    #get rid of days with only single observation
  df2<-df2[date %ni% df2[,.N,by=c('date','ccy')][N==1,date]]

  # set alphabetical order such that dummies are on foreign ccys
  df2[ccy=='usd',ccy:='1usd']
        
  # introduce liquidity measure based on bond age
    df2[,liq:=ytm/ytofm]
    df2<-df2[liq %between% c(0,1.1)]
    df2[liq<.5,liq_bucket:=0] # more illiq
    df2[liq>=.5,liq_bucket:=1] # liq
  regfun<-function(dt,regversion=1){
      if (regversion==1){
        # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
        reg<-lm(value~ccy+upcusip,data=dt)
      } else if (regversion==3){
        # regversion 3: like regversion 2 but also adds maturity considerations in regression
        reg<-lm(value~ccy+upcusip+ytm_bucket,data=dt)
      } else if (regversion==4){
        # regversion 4: regversion 3+ 3 rating buckets as dummies
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket,data=dt)
      } else if (regversion==5){
       # regversion 5: regversion 3+ 3 rating buckets as dummies
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
      } else if (regversion==6){
       # regversion 6, add illiqudity index
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
      } else if (regversion==7){
       # regversion 7, like 6 but w/o sicfac
        reg<-lm(value~ccy+upcusip+ytm_bucket+rating_bucket+liq_bucket,data=dt)
      } else if (regversion==8){
       # regversion 8, like 7 but only focus on liq
        reg<-lm(value~ccy+upcusip+ytm_bucket+liq_bucket,data=dt)
      }

      if (nrow(dt[ccy=='chf'])>0){
        data.table(ccyeur=reg$coefficients['ccyeur'],
        ccygbp=reg$coefficients['ccygbp'],
        ccyjpy=reg$coefficients['ccyjpy'],
        ccyaud=reg$coefficients['ccyaud'],
        ccychf=reg$coefficients['ccychf'],
        ccycad=reg$coefficients['ccycad'])
      } else{
        data.table(ccyeur=reg$coefficients['ccyeur'],
        ccygbp=reg$coefficients['ccygbp'],
        ccyjpy=reg$coefficients['ccyjpy'],
        ccyaud=reg$coefficients['ccyaud'])
      }
  }

  regcoef<-df2[,regfun(.SD,version),by='date']
  setkey(regcoef,date)
  lsout<-list(regcoef,df2)
  beep()
  lsout
}

bucketrating<-function(dtlin){
  # creates new column called rating bucket as factor with 4 levels 
  dtlout<-dtlin
  dtlout[nrating %between% c(1,4),rating_bucket:=3]
  dtlout[nrating %between% c(5,10),rating_bucket:=2]
  dtlout[nrating>10,rating_bucket:=1]
  dtlout[nrating==0,rating_bucket:=0]
  dtlout[is.na(nrating),rating_bucket:=0]
  dtlout[,rating_bucket:=factor(rating_bucket)]
  dtlout
}
bucketytm<-function(dtlin){
  dtlout<-dtlin[!is.na(ytm)]
  dtlout[ytm %between% c(0,3),ytm_bucket:=1]
  dtlout[ytm %between% c(3,7),ytm_bucket:=2]
  dtlout[ytm %between% c(7,10),ytm_bucket:=3]
  dtlout[ytm >10,ytm_bucket:=4]
  dtlout[,ytm_bucket:=factor(ytm_bucket)]
  dtlout
}


issfilter<-function(dtin){
  dtout<-dtin
  dtout %<>% filter(
     amt >= 50,
     ytofm >= 1,
     ytofm <= 99999,
     mdealtype %ni% c("P", "ANPX", "M", "EP", "CEP", "TM", "PP"),
     secur %ni% c(
       "Cum Red Pfd Shs",
       "Non-Cum Pref Sh" ,
       "Preferred Shs" ,
       "Pfd Stk,Com Stk"
     ),
     !grepl('Government Sponsored Enterprises',tf_mid_desc),!grepl('Flt', secur),!grepl('Zero Cpn', secur),!grepl('Float', secur),!grepl('Fl', descr),
     !grepl('Zero Cpn', descr),!grepl('Mortgage-backed', issue_type_desc),!grepl('Asset-backed', issue_type_desc),!grepl('Federal Credit Agency', issue_type_desc),!grepl('Loan', descr)
   ) 
dtout %>% as.data.table()
}


downloadbbg<-function(tickers,filestr=str_c('bbg_',today(),'.RData'),startdt=ymd('1996-01-01'),periodstr='MONTHLY',fieldstr='PX_LAST',splitN=1){
  require(Rblpapi)
  if (is.data.table(tickers)) {
    tickers<-tickers[,.(parsekeyable)]
  } else{
    tickers<-data.table('parsekeyable'=tickers)
  }
  tickers$batchfactor<-sample(1:splitN,size=nrow(tickers),replace=T)
  tickerslist<-split(as.character(tickers$parsekeyable),tickers$batchfactor)
  opt <- c("periodicitySelection"=periodstr)
  con <- Rblpapi::blpConnect()     # automatic if option("blpAutoConnect") is TRUE
  prices<-list()
  for (i in 1:splitN) {
    print(i)
    prices[[i]]<-bdh(tickerslist[[i]], fieldstr, start.date=startdt, options=opt)  
    save(prices,tickers,tickerslist,i,file=str_c('temp_',filestr))
  }
  blpDisconnect(con)
  save(prices,file=filestr)
  #loadBBGdownload2df(filestr)
}

update.dt<-function(dtA,dtB,keyfield='auto',updatefield='auto',insertnewrow=TRUE,override=FALSE){
  # this function allows easy addition and update of columns; adding new rows require insertnewrow=TRUE
  # update will override existing values currently; ideally want update on NA, do nothing on records where update is the same as original, and split out differentiated records where original value!=update value
  
  dtA<-copy(dtA)
  dtB<-copy(dtB)
  keyoriginal<-key(dtA)
  if (keyfield=='auto') keyfield<-keyoriginal
  print(str_c('Keyfield: ',paste(keyfield,collapse=' ')))
  setkeyv(dtA,keyfield)
  setkeyv(dtB,keyfield)
  if (updatefield=='auto') {
    fieldA<-ds(dtA)[ds(dtA) %ni% keyfield]
    fieldB<-ds(dtB)[ds(dtB) %ni% keyfield]
    updatefield<-fieldB[fieldB %in% fieldA]
    newfield<-fieldB[fieldB %ni% fieldA]
    lhs<-c(updatefield,newfield)
    rhs<-c(str_c('i.',updatefield),newfield)
    rhs<-rhs[rhs %ni% 'i.']
  }else{
    lhs<-updatefield
    rhs<-str_c('i.',updatefield)
  }
  if (length(lhs)!=length(rhs)){ print('len(lhs)!=len(rhs)');browser()}
  for (j in 1:length(lhs)){
    message(str_c('inserting/updating: lhs: ',lhs[j],' rhs: ',rhs[j]))
    #show warning message first
    problemdt<-dtA[dtB,nomatch=0][eval(exparse(lhs[j]))!=eval(exparse(rhs[j]))]
    if (nrow(problemdt)!=0){
      message('matched but existing value conflicts for field: ',lhs[j])
      #if (override) warning('overriding:') else warning('skiping:')
      #print(problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE])
      message('\n',print_and_capture(problemdt[,c(keyfield,lhs[j],rhs[j]),with=FALSE]))
    }

    if (override){
      # override when there are existing value
      tempdt<-dtA[dtB,nomatch=0]
      message(str_c('updated/inserted: ',tempdt[is.na(eval(exparse(lhs[j]))),.N]))
      message(str_c('override: ',tempdt[!is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=eval(exparse(rhs[j])),nomatch=0]
    } else{
      # update/insert if na /override 
      message(str_c('updated/inserted (no override) on ',dtA[dtB,nomatch=0][is.na(eval(exparse(lhs[j]))),.N]))
      dtA[dtB,(lhs[j]):=ifelse(is.na(eval(exparse(lhs[j]))),eval(exparse(rhs[j])),eval(exparse(lhs[j]))),nomatch=0]
    }
    
    
  }
  if (insertnewrow){
    #insert row
    rowinsert<-dtB[!dtA]
    # dtA<-rbind(dtA,rowinsert,fill=TRUE)
    dtA<-bind_rows(dtA,rowinsert) %>% as.data.table()
    message(str_c('Rows inserted: ',rowinsert[,.N]))
  }
  setkeyv(dtA,keyoriginal)
  dtA
}
exparse<-function(strin){parse(text=strin)}
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

resave <- function(..., list = character(), file) {
   previous  <- load(file)
   var.names <- c(list, as.character(substitute(list(...)))[-1L])
   for (var in var.names) assign(var, get(var, envir = parent.frame()))
   save(list = unique(c(previous, var.names)), file = file)
}