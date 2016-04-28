rm(list=ls(all=TRUE))
library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)
require('readstata13')
require('ggfortify')
require('doBy')


# df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
df2clip<-function(x)(write.table(x, "clipboard.csv", sep=","))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")

raw<-read.dta13('sdc96_clean2.dta')
df1<-raw[!is.na(raw$amt),]
df1<-filter(df1,ccy %in% c('USD',"EUR",'AUD',"JPY"))
# globalissuers<-unique(filter(df1,foreign==1)$i)
# dfgi<-data.frame(i=globalissuers,gi=1)
# df2<-merge(df1,dfgi,by='i',all=TRUE)
# df2[is.na(df2$gi),]$gi=0
# write.dta(df2,'sdc96_clean2.dta')

dfs<-summaryBy(amt~ccy+foreign,data=df1,FUN=sum)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~modupnat+foreign,data=df1,FUN=sum)
orderBy(~-foreign-amt.sum,dfs)

dfs5<-summaryBy(amt~foreign+year,data=df1,FUN=sum)
dfs6<-reshape(dfs5,timevar='foreign',idvar='year',direction='wide')
dfs6$total<-(dfs6$amt.sum.0+dfs6$amt.sum.1)
dfs6$foreignp<-dfs6$amt.sum.1/(dfs6$amt.sum.0+dfs6$amt.sum.1)
dfs6

# foreign share by secur type
dfs<-summaryBy(amt~secur+foreign,data=filter(df1),FUN=c(sum,length),order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
dfs3<-reshape(dfs2,timevar='foreign',idvar='secur',direction='wide')
str(dfs3)
dfs4<-orderBy(~-amt.sum.0-amt.sum.1,dfs3)
dfs4$amtforeignP<-dfs4$amt.sum.1/(dfs4$amt.sum.1+dfs4$amt.sum.0)
dfs4$nforeignP<-dfs4$amt.length.1/(dfs4$amt.length.1+dfs4$amt.length.0)
df2clip(dfs4)




dfs<-summaryBy(amt~foreign+secur,data=filter(df1),FUN=c(sum,function(x) quantile(x,c(.1,.5,.9),na.rm=TRUE)),order=TRUE)
dfs
dfs2<-orderBy(~foreign,dfs)
dfs2



dfs<-summaryBy(amt~foreign+ccy,data=filter(df1),FUN=c(sum,function(x) quantile(x,c(.1,.5,.9),na.rm=TRUE)),order=TRUE)
dfs
dfs2<-orderBy(~ccy,dfs)
dfs2



dfs<-summaryBy(nrating~foreign+ccy,data=filter(df1),FUN=c(mean,median, function(x) quantile(x,c(.1,.9))),order=TRUE)
dfs
dfs2<-orderBy(~foreign+ccy,dfs)
dfs2

dfs<-summaryBy(amt~nrating+ccy,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~ccy,dfs)
dfs2
df2clip(dfs2)

dfs<-summaryBy(amt~nrating+foreign,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~foreign+nrating-amt.sum,dfs)

dfs<-summaryBy(amt~nrating+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~nrating-amt.sum,dfs)

dfs<-summaryBy(amt~mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-amt.sum,dfs)

dfs<-summaryBy(amt~mdy+sp,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
df2clip(dfs2)



dfs<-summaryBy(ytofm~sp,data=filter(df1,!(is.na(df1$ytofm))),FUN=c(mean,median),order=TRUE)
dfs

dfs2<-orderBy(~-amt.sum,dfs)
dfs2


summary(filter(df1,!(is.na(df1$ytofm)))$ytofm)


dfs<-summaryBy(amt~sp,data=filter(df1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)
dfs2
df2clip(dfs2)


dfs<-summaryBy(amt~foreign+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)


dfs<-summaryBy(amt~foreign+mdealtype,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~foreign+btypc,data=filter(df1),FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,dfs)

dfs<-summaryBy(amt~foreign+gi,data=filter(df2),FUN=sum,order=TRUE)
dfs



dfs<-summaryBy(amt~i,data=filter(df1,pub=="Govt." & foreign==1),FUN=sum,order=TRUE)
dfs2<-orderBy(~-amt.sum,dfs)




View(dfs2)
dfs<-summaryBy(amt~pub+sic1,data=df1,FUN=sum,order=TRUE)
dfs

dfs<-summaryBy(amt~pub+secur+foreign,data=df1,FUN=sum,order=TRUE)
dfs2<-orderBy(~pub-amt.sum,data=dfs)

filter(dfs2,pub=='Public',foreign==1)


dfs<-summaryBy(amt~pub+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~pub,data=dfs)

dfs<-summaryBy(amt~ccy,data=df1,FUN=sum,order=TRUE)
orderBy(~-amt.sum,data=dfs)
sum(dplyr::filter(dfs,ccy %in% c('USD',"EUR",'AUD',"JPY"))$amt.sum)/sum(dfs$amt.sum)


dfs<-summaryBy(amt~ccy+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,data=dfs)


dfs<-summaryBy(amt~modnat,data=df1,FUN=sum,order=TRUE)
orderBy(~-amt.sum,data=dfs)


dfs<-summaryBy(amt~modnat+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign-amt.sum,data=dfs)


dfs<-summaryBy(amt~sp,data=df1,FUN=sum,order=TRUE)
as.numeric(filter(dfs,sp==""))/sum(dfs$amt.sum)

dfs<-summaryBy(amt~mdy,data=df1,FUN=sum,order=TRUE)
as.numeric(filter(dfs,mdy==""))/sum(dfs$amt.sum)

dfs<-summaryBy(amt~sp+foreign,data=df1,FUN=sum,order=TRUE)
orderBy(~-foreign,data=dfs)
as.numeric(filter(dfs,sp=="" & foreign==1)[,3])/sum(filter(dfs,foreign==1)$amt.sum)

orderBy(~-amt.sum,data=dfs)

# what's going on with aud
df2<-


