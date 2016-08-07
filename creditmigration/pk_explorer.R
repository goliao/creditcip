setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
source('util.r')
require('readxl')

load('db/pk_lookup.RData')
pk.figi[,pk:=tolower(pk)];pk.isin[,pk:=tolower(pk)]
pk.mapping.exist<-rbind(pk.figi[,.(pk)],pk.isin[,.(pk)]) %>% distinct(); pk.mapping.exist %>% setkey(pk); pk.mapping.exist[,pk:=tolower(pk)]
pk.mapping.exist %>% setkey(pk)

# load('dtl160804_completesdc.RData')
# pkadded<-dtladd5.monthly[,.(pk)] %>% distinct(); pkadded %>% setkey(pk)
# pkadded[pkraw1,nomatch=0]

path='bsrch_all.xlsx'
bsrch.ls<-lapply(excel_sheets(path), function(x,path){
	dt.sheet<-(read_excel(path,x) %>% as.data.table())[,.(pk,sheet=x)]
	dt.sheet %>% setkey(pk)
	dt.sheet
},path)
bsrch.pk<-rbindlist(bsrch.ls)
bsrch.pk %>% setkey(pk)
bsrch.pk.unique<-bsrch.pk %>% unique()
bsrch.pk.unique[,pk:=tolower(pk)]

merge<-compare.dt(pk.mapping.exist,bsrch.pk.unique,bykey.='pk',mask=F)

load('db/bondref160803.RData');bondref %>% setkey(pk); 
# these mappings of BK to figi exists, but not from bsrch; and half of them can't be matched to SDC
merge$AniB
compare.dt(bondref %>% issfilter(.,3),merge$AniB)


# these PKs are from filtered bsrch, but doesn't have figi mapping; think about requesting these for the future
merge$BniA[,.N,sheet]
merge$BniA %>% View



dt.pk.figi.email<-fread.xls('pk_figi_mapping_all.xlsx')[,.(pk,figi,sheet)] %>% distinct(); 
dt.pk.figi.email[,pk:=tolower(pk)]
dt.pk.figi.email %>% setkey(pk) %>% unique


compare.dt(merge$BniA, dt.pk.figi.email)


m2<-compare.dt(pk.mapping.exist, dt.pk.figi.email,mask=F)
m2$BniA[,.N,sheet]
dt.pk.figi.email[,.N,sheet]

# how many of these new mappings are in bond ref
compare.dt(bondref,m2$BniA) #none

# what about in all of SDC
load('bondrefall_sdc.RData')
bondref %>% setkey(figi)
compare.dt(bondref  %>% issfilter(.,3),m2$BniA,bykey.='figi') #none
# so only around 200 or os


#merge new mapping into old mapping
pk.figi<-update.dt(pk.figi,m2$BniA,'pk')
#resave(pk.figi,file='db/pk_lookup.RData')