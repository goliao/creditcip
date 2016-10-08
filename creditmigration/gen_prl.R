
# started using this file on Sept 27, 2016
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
source('util.r')
load('db/monthenddates.RData')
load('db/archive/prl160731.RData')
prlnew<-loadBBGdownload2df('dbin/prl161001daily.RData')
prl<-update.prl(prl,prlnew,overridein = T)
save(prl, file='db/prl.RData')




# prl tickers 2 download for future
#prl2get<-get.prl.status(prl)
#prl.tdd<-prl2get[ticker %like% '^(us|bp|cd|ad|jy|sf)sw\\d+' | ticker %like% '^eusa\\d+' | ticker %like% '^(ad|eu|jy|bp|cd|sf)bs\\d+' | ticker %like% '^eubsv\\d+']
