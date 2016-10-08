# parse hedge file

# system('gcsfuse glbucket /mnt/glbucket')
setwd('/mnt/disks/xccy/creditmigration')
source('util.r')
source('utilsec.R')
require('doParallel')

# wrdsind<-fread('wrdsindex.csv',sep=',')
# spind<-wrdsind[conm=='S&P 500 Comp-Ltd']
# spind[,from.date:=ymd(from)]
# spind[,thru.date:=ymd(thru)]
# spind<-spind[order(from.date)]
# spind %>% setkey('co_cik')

# #get the file path for each individual filing
# con <- dbConnect(drv=SQLite(), dbname="edgar_idx.db")
# res<-dbGetQuery(con,"select * from idx where (type='10-K') and date>='2004-01-01' and date<='2016-12-31'")
# dbDisconnect(con)
# dtind<-as.data.table(res)
# # merge with sp500
# dtind[,cik:=as.numeric(cik)]
# setkey(dtind,cik)
# dtindexsp500<-dtind[!unique(spind)[!is.na(co_cik),.(co_cik)]]
# dtindexsp500 %>% setkey(path)
# dt2download<-unique(dtindexsp500)#[!is.na(path)]
# save(dt2download,file='non_sp500_10k.RData')
# dt2download

# dtdownloaded<-markdownloaded(dt2download,prefix='/mnt/disks/xccy/sec/nonsp500v3/',parcore=31)
# dtdownloaded[,.N,downloaded]
# save(dtdownloaded,file='non_sp500_10k_downloaded_ind.RData')
extract.text.all<-function(dtin.,startn=1,pathinprefix='/mnt/disks/xccy/sec/sp500v3/',pathoutprefix='/mnt/disks/xccy/sec/blocktext_sp500v3/',parcore=30){
  # previously known as gen.hedge.all.par
  # dtin.=dtdownloaded
  # startn=62
  # pathinprefix='/mnt/disks/xccy/sec/nonsp500v3/'
  # pathoutprefix='/mnt/disks/xccy/sec/blocktext_nonsp500v3/'
  # parcore=1
  # require('doParallel')
  # registerDoParallel(parcore)
  dtin<-copy(dtin.)
  dtin[,ioriginal:=rownames(dtin)]
  dtin %>% setkey(path)
  dtin<-unique(dtin)
  dtout<-dtin[downloaded==1]	
  # i=62
  # foreach (i=startn:nrow(dtout)) %dopar% {
  for (i in startn:nrow(dtout)){
    # tryCatch({
      print(i)
      filename=str_c(pathinprefix,str_replace_all(dtout[i,path],'/','_'))
      fnout=str_c(pathoutprefix,str_replace_all(dtout[i,path],'/','_'),'.RData')
      if(!file.exists(fnout) | file.info(fnout)$size==0){
        # gen.hedge(filename,fnout)
        textblock<-gen.text.sec(filename)
        if (is.na(textblock)) next
        save(textblock,file=str_c(fnout))
      }
    # },error=function(e){
    #   print(e)
    #   browser()})
  }
}


load('non_sp500_10k_downloaded_ind.RData')
print(dtdownloaded[,.N,downloaded])
# extract.text.all.par(dtdownloaded,pathinprefix='/mnt/disks/xccy/sec/nonsp500v3/', pathoutprefix='/mnt/disks/xccy/sec/blocktext_nonsp500v3/',parcore=24)
extract.text.all(dtdownloaded,startn=60000,pathinprefix='/mnt/disks/xccy/sec/nonsp500v3/', pathoutprefix='/mnt/disks/xccy/sec/blocktext_nonsp500v3/',parcore=1)



system('sudo shutdown -h now')




# # rm(dtind)

# # download.sec.par<-function(dtindex.in,pathprefix='sp500v3/',startn=1,endn=nrow(dtindex.in)){
# #   require('doParallel')
# #   registerDoParallel(8)
# #   foreach (i=startn:endn) %dopar% {
# #     fn=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
# #     if (!file.exists(fn) | file.info(fn)$size==0){
# #       # print(str_c(i,' out of ',nrow(dtindex.in)))
# #       urlstr=str_c('ftp://ftp.sec.gov/',dtindex.in[i,path])
# #       fileout=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
# #       # print(str_c('url: ',urlstr))
# #       # print(str_c(' fileout: ',fileout))
# #       cmdstr<-str_c('curl -u anonymous:liao@fas.harvard.edu -o ', fileout, ' ',urlstr)
# #       # print(cmdstr)
# #       system(cmdstr)
# #     }
# #   }
# # }

# # out<-construct.hedge.result.all(dtind,parcore=30,fileprefix='/mnt/glbucket/sec/sp500v3/')
#   # download.sec.par(dt2download,pathprefix=)

