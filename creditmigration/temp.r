setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
load('db/dtlmo.RData');load('db/bondref160803.RData');
source('util.r')




load('dtl160803_yr04-06add.RData')
update.dtl.mo(dtladd1.daily,dtladd1.monthly)

load('dtl160803_1-2yrallccynewadd.RData')
update.dtl.mo(dtladd2.daily,dtladd2.monthly)

load('dtl160803_otherccy.RData')
update.dtl.mo(dtladd3.daily,dtladd3.monthly)

load('dtl160803_completesdc.RData')
update.dtl.mo(dtladd4.daily,dtladd4.monthly)

load('dtl160803_recheck1add.RData')
update.dtl.mo(recheck1.daily,recheck1.monthly)

load('dtl160804_completesdc.RData')
update.dtl.mo(dtladd5.daily,dtladd5.monthly)
aa<-compare.dt(dtladd5.daily,dtladd5.monthly,bykey.=c('date','pk'),mask=F)
aa$BniA

dtladd5.daily[pk=='cp501918 corp']
lubridate::wday('2002-01-31',label=T)


dtl.mo %>% setkey(pk)
bondref %>% setkey(pk)

aa<-bondref[!dtl.mo] %>% issfilter(.,3)

tickers %>% setkey(pk)

tickers[aa$pk,nomatch=0]
load('dtl160804_completesdc.RData')

dtladd5.monthly %>% setkey(pk)
dtladd5.monthly[aa$pk,nomatch=0]
  getresult100<-function(id2req){
      test<-tryCatch({
      	result100_json='c'
     	#stop('glerr')
      }, error = function(err) {
        print('erro step')
        return(1)
      })
      test
    }


    jj<-getresult100('test')
    jj







 testfun<-function(){
 	a<-21
 	if (1){
 		return(7)
 	}
 	8
 }
 testfun()





 json2req<-[{"idType":"ID_CUSIP_8_CHR","idValue":"001765AC0"},{"idType":"ID_CUSIP_8_CHR","idValue":"048825AU7"},{"idType":"ID_CUSIP_8_CHR","idValue":"048825AV5"}]

 result100<-POST(url='https://api.openfigi.com/v1/mapping',add_headers(`Content-Type`='text/json',`X-OPENFIGI-APIKEY`='b0128aa2-fe78-4707-a8ec-d9623d6f9928'), body = json2req, encode = "json")
        result100_json <-   result100 %>% content(as = "text") %>% fromJSON(simplifyDataFrame = TRUE)



curl -v -X POST 'https://api.openfigi.com/v1/mapping'  --header 'Content-Type: text/json'  --data '[{"idType":"ID_WERTPAPIER","idValue":"851399","exchCode":"US"}]'



curl -v -X POST 'https://api.openfigi.com/v1/mapping'  --header 'Content-Type: text/json'  --data '[{"idType":"ID_CUSIP_8_CHR","idValue":"001765AC0"}]'


curl -v -X POST 'https://api.openfigi.com/v1/mapping'  --header 'Content-Type: text/json'  --data '[{"idType":"ID_CUSIP","idValue":"001765AC"}]'