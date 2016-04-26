# Generate global bond tickers/isins
rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# BBG generated -----------------------------------------------------------
# takes in bbg tiker and ultimate parent info and spits out global bonds tickers
raw<-read.dta13('sdc96_clean2.dta')
bondinfo<-read.csv("../data/bloomberg/bbg_bsrch_all-usd-eur-ig-small.csv")
colnames(bondinfo)<-c('ticker','isin','id_bb_co','id_bb_ultimate_co','crncy')
up_global<-bondinfo %>% group_by(id_bb_ultimate_co,crncy) %>% dplyr::summarise(ct=length(ticker)) %>% 
  reshape2::dcast(.,id_bb_ultimate_co~crncy,value.var='ct',fill=0) %>% mutate(isglobal=(EUR & USD)) %>% filter(isglobal==TRUE)
#break into 20 chuncks
up_global$fac=sample(1:20,nrow(up_global),replace=TRUE)
globalbonds<-bondinfo %>% inner_join(.,up_global %>% select(id_bb_ultimate_co,fac),by='id_bb_ultimate_co') %>% arrange(fac,id_bb_ultimate_co) 
save(globalbonds,file='globalbondtickers.rdata')
save(globalbonds,file='J:/data/globalbondtickers.rdata')


# SDC generated isin ------------------------------------------------------
df_sdc_raw<-read.dta13('sdc96_clean2.dta') %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
df_sdc <- df_sdc_raw %>% sample_frac(1)
#find issuances that are global in EU and USD
up_global<-df_sdc %>% group_by(upcusip,ccy) %>% filter(ccy %in% c('USD','EUR','GBP','JPY','CHF','AUD','CAD')) %>% 
  dplyr::summarise(ct=length(upcusip)) %>% tbl_df %>% 
  tabulate('upcusip') %>% filter(Freq>1) %>% rename(upcusip=Var1) %>%  filter(upcusip!=0) %>% select(upcusip)
up_global$fac=sample(1:20,nrow(up_global),replace=TRUE)
# keeping only global issuer bonds
df_sdc %<>% inner_join(.,up_global %>% select(upcusip,fac),by='upcusip') %>% 
  arrange(fac,upcusip)

# generous filtering, for sending to bloomberg to get bbg tickers 
df_sdc_generous <- df_sdc %>% filter(
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
  tf_mid_desc != 'Government Sponsored Enterprises',
  !grepl('Flt', secur),
  !grepl('Zero Cpn', secur),
  !grepl('Float', secur),
  !grepl('Fl', descr),
  !grepl('Zero Cpn', descr),
  !grepl('Mortgage-backed',issue_type_desc),
  !grepl('Asset-backed',issue_type_desc),
  !grepl('Federal Credit Agency',issue_type_desc),
  !grepl('Loan',descr)
) %>% print
bbg_isin_req <- df_sdc_generous %>% filter(isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print
# send this file to bloomberg to request parskey
bbg_isin_req %>% write.csv(file='isin_sdc.csv')

# # only EUR USD
# bbg_isin_req_euus <- df_sdc_generous %>% 
#   filter(ccy %in% c('EUR','USD') ,isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print
# # eur usd small
# bbg_isin_req_euus <- df_sdc_generous %>%
#   filter(amt>=100, ytofm>=2,pub!='Govt.',ccy %in% c('EUR', 'USD') , isin != '-') %>% select(isin) %>% dplyr::distinct(isin) %>% print


# isin to figi ------------------------------------------------------------

require('magrittr')
require('httr')
require('jsonlite')
require('tidyjson')
# figireq<-'[{"idType":"ID_ISIN","idValue":"XS1033736890"},
# {"idType":"ID_BB_UNIQUE","idValue":"JK354407"},
# {"idType":"ID_BB","idValue":"JK354407"},
# {"idType":"COMPOSITE_ID_BB_GLOBAL","idValue":"JK354407"},
# {"idType":"TICKER","idValue":"JK354407 Corp"},
# {"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'
# # figireq<-'[{"idType":"ID_BB_GLOBAL","idValue":"BBG0005HH8B8"}]'


ptm <- proc.time()
counter <- 0 # count request up to 100, for figi limit of 100 request per minute
df_isin2figi_all<-as.data.frame(list()) %>% tbl_df()
for (j in 1:ceiling(nrow(bbg_isin_req)/100)){
  counter <- counter+1
  if (counter==100){
    if ((proc.time() - ptm)[[3]]<=65){ # let it sleep to a full minute if it hasn't been a full minute
      print ((proc.time() - ptm)[[3]])
      Sys.sleep(60-((proc.time() - ptm)[[3]]))
      counter %>% print
      ptm <- proc.time()
      counter <- 0
    } else { # continue and reset counters and time
      print(str_c("row (j):",j*100))
      counter %>% print
      ptm <- proc.time()
      counter <- 0
    }
    save(df_isin2figi_all,file='temp_dfisinfigi.rdata')
  }
  tempreq <- bbg_isin_req %>% slice(((j-1)*100+1):min(j*100,nrow(.)))
  figireq<- tempreq %>%  mutate(idType='ID_ISIN',idValue=isin) %>% select(-isin)  %>% jsonlite::toJSON() 
  r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
  # responsejson %<>% bind_rows(.,r %>% content(as="text") %>% fromJSON(simplifyDataFrame = TRUE))
  tryCatch({
    responsejson <-   r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
  }, error = function(err) {
    warning('too many requests')
    ptm <- proc.time()
    counter <- 0
    Sys.sleep(60-Sys.time() %>% second())
    r<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = figireq, encode = "json")
    responsejson <-  r %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)
  })
  
  # extract 100 results at a time
  df_isin2figi<-as.data.frame(list()) %>% tbl_df()
  if (nrow(responsejson)!=nrow(tempreq)) stop('response not mathcing request row numbers') 
  for  (i in 1:nrow(responsejson)){
    if (ncol(responsejson)==1) { # only data column
      df_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
    } else{ # data column and error column
      if (is.na(responsejson$error[i][[1]]))  df_isin2figi %<>% bind_rows(.,responsejson$data[i][[1]] %>% mutate(isin=tempreq$isin[i][[1]]))
    }
  }
  df_isin2figi_all %<>% bind_rows(.,df_isin2figi)
}

df_isin2figi_all

save(df_isin2figi,file='df_isin2figi.rdata')






responsejson[3,2][[1]] %>% str
responsejson %>% View


  
jsonlite::fromJSON(r[[1]])
r %>% str
content(r)[[1]][[1]] %>% fromJSON(.,simplifyDataFrame = TRUE)
content(r)[[1]][[1]][[1]] %>% as.data.frame() %>% View
content(r)[[49]]

content(r) %>% str
  json_types()
unlist(content(r)[[1]],recursive = FALSE) 

t1<-as.data.frame(content(r)[[1]][[1]][[1]])
for (i in 2:134){
  t1<-rbind(t1,content(r)[[1]][[1]][[i]] %>% as.data.frame())
}
t1 %>% View

content_type()
content_type_json()
add_headers(`Content-Type`='text/json')

content_type_json() %>% str
content_type_json()[1]



# test --------------------------------------------------------------------

people <- '
[
  {
  "name": "bob",
  "age": 32
  }, 
  {
  "name": "susan", 
  "age": 54
  }
  ]'
  
  # Structure the data
  people %>%    
    gather_keys
    gather_array %>%          # gather (stack) the array by index
    spread_values(            # spread (widen) values to widen the data.frame
      name = jstring("name"), # value of "name" becomes a character column
      age = jnumber("age")    # value of "age" becomes a numeric column
    )
