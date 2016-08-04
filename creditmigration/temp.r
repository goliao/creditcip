

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