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
'%ni%' <- Negate('%in%')
df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))

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
      } else{ # data column and error column
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
  c(df_isin2figi_all,diag_response)
}

countdups<-function(dfin,field='isin'){
  (dfin %>% nrow)-(dfin %>% distinct_(field) %>% nrow)
}

loadBBGdownload2df<-function(filename='bbg_gbonds_160426_mo_batch2_asw.RData'){
  # takes in a filename associated with bbg download; spits out price dataframe with field as colnames and a variable type
  load(filename)
  a0 <- unlist(prices, recursive = FALSE)
  tickernames <- names(a0)
  df_prices <- data.frame() %>% tbl_df()
  for (i in 1:length(tickernames)) {
    temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
    if (nrow(temp_new) == 0)
      print (str_c('empty:#', i, ' name:', tickernames[i]))
    df_prices <- df_prices %>% dplyr::bind_rows(., temp_new)
  }
  df_prices %>% rename(parsekeyable=ticker)
}

assessDataCoverage<-function(bondinfo,bondprices,field='YLD_YTM_MID',lastdate='lastobs',startdate='firstobs'){
  # check monthly data coverage; bondinfo is essentially sdc infomation on maturity etc, bondprice is from bbg download
  # 
  #   bondprices<-df_yld
  #   bondinfo<-df_sdc_all
  #   field="ASSET_SWAP_SPD_MID"
  #field="BLP_Z_SPRD_MID"
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