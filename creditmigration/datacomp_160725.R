rm(list=ls(all=TRUE));
source('util_20160620.r')
#load old data that works and remove to 'name0'
load('dtclean160624.RData')
br0<-copy(br)
dtl30<-copy(dtl3)
prl0<-copy(prl)
prw0<-copy(prw)
rm(br,dtl3,prl,prw)
#load new data that doesn't work
load('dtclean.RData')

# 
# dtl30[,.N,field]
# dtl3[,.N,field]
# 
# dtl30[,median(value),ccy]
# dtl3[,median(value),ccy]
# 
# dtl30[,pk:=tolower(pk)]
# dtl3[,monthend:=NULL]
# setkey(dtl30,pk,date)
# setkey(dtl3,pk,date)
# 
# aa<-update.dt(dtl30,dtl3,keyfield = c('pk','date'))
# bb<-update.dt(prl0,prl,keyfield=c('ticker','date'))



ys0<-resyldsprdv3(dtl30,prl0,regversion=6)
ys1<-resyldsprdv3(dtl3,prl,regversion=6)


ys0
ys1 %>% ggplotw()
ys0[ys1][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()



source('util.r')
setkey(prl0,date,ticker)
setkey(prl,date,ticker)
aa<-update.dt(prl0,prl,diagnostic_rt = T)
aa %>% str()
bb<-aa[[2]][[1]]
bb[abs(valdiff)>.1,.N,ticker]


bb<-prl[ticker %like% '^ussw' | ticker %like% '^eusw' | ticker %like% '^bpsw' | ticker %like% '^jysw' | ticker %like% '^adsw',.(date,ticker,value)]   
bb[,.N,ticker] %>% View
