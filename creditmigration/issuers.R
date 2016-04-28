rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
require(sqldf)

raw<-read.dta13('sdc96_clean3.dta')

df1<-raw[!is.na(raw$amt),]

# create a list of cusips, names, tickers
tics<-df1 %>% group_by(tic) %>% summarise(Ntic=length(amt)) %>% filter(Ntic>=1) %>% arrange(desc(Ntic)) %>% filter(tic!="-")
ids<-sqldf("select distinct A.i, A.tic, A.cu from tics as B left join df1 as A on A.tic=B.tic") 
ids %>% View()

tics<-df1 %>% group_by(i) %>% summarise(Ntic=length(amt)) %>% filter(Ntic>=1) %>% arrange(desc(Ntic)) 
ids<-sqldf("select distinct A.i, A.cu from tics as B left join df1 as A on A.i=B.i") 
ids %>% View()
ids %>% distinct(i) %>% count()
ids %>% distinct(cu) %>% count()

# add UP ticker and name 
df2<-sqldf("select A.*, B.i as upi, B.tic as uptic from df1 as A left join ids as B ON A.upcusip=B.cu")
nrow(df1)
nrow(df2)
df3 <- df2 %>%  filter(foreign==1, ccy %in% c('USD',"EUR",'GBP'), modnat %in% c('United States','Eurozone', 'United Kingdom'),year>2007) 
nrow(df3)

df3 %>% group_by(upi) %>% summarise(Niss=length(amt)) %>% filter(Niss>=1) %>% 
  arrange(desc(Niss)) %>% 
  View()
  write.csv(.,file='commonitckers.csv')



sqldf("select ccy, d, i, amt, ytofm, upnames, upcusip from df2 where i like '%allianz%'")
colnames(df1)
View(df1)

df1 %>% filter(tic)

df1 %>% count()
df1 %>% filter(tic=="-") %>% summaryBy(amt~i,data=.,FUN=sum) %>% orderBy(~-amt.sum,.) %>% head(20)
df1 %>% filter(tic=="-") %>% summaryBy(amt~i,data=.,FUN=length) %>% orderBy(~-amt.length,.) %>% head(20)

  
amtsum_all<-df1 %>% select(amt) %>% sum()  
dftic<-df1 %>% summaryBy(amt~tic,data=.,FUN=sum) %>% orderBy(~-amt.sum,.)
dftic$pct=dftic$amt.sum/amtsum_all
View(dftic)


df1 %>% summaryBy(amt~tic,data=.,FUN=length) %>% orderBy(~-amt.length,.) %>% head(30)


df1 %>% summaryBy(amt~tic,data=.,FUN=length) %>% orderBy(~-amt.length,.) %>% head(30)
df1 %>% summaryBy(amt~isin,data=.,FUN=length) %>% orderBy(~-amt.length,.) %>% head(30)


nrow(df1)
df1 %>%  filter(foreign==1) %>% mutate(totnrow=nrow(.)) %>% summaryBy(amt~tic,data=.,FUN=length) %>% orderBy(~-amt.length,.) %>% head(30)


df2 <- df1 %>%  filter(foreign==1) 
totnrow<-nrow(df2)
df2 %>% group_by(tic) %>% summarise(Ntic=length(amt)) %>% filter(Ntic>=1) %>% arrange(desc(Ntic)) %>% write.csv(.,file='commonitckers.csv')
df2 %>% group_by(isin) %>% summarise(length(amt))

df1 %>% group_by(tic) %>% summarise(Ntic=length(amt)) %>% filter(Ntic>=1) %>% arrange(desc(Ntic)) %>% write.csv(.,file='commonitckersall.csv')
df2 %>% 
  
df1 %>% filter
sqldf("select A.i,A.tic, A.isin, A.cu, A.upcusip, A.upnames, B.i, B.tic from df1 as A left join df1 as B where A.i like '%volkswagen%' and A.upcusip=B.cu") %>% View(.)

sqldf("select A.i,A.tic, A.isin, A.cu, A.upcusip, A.upnames, B.i, B.tic from df1 as A left join df1 as B where A.i like '%volkswagen%' and A.upnames=B.i") %>% View(.)
sqldf("select i, tic, isin, upnames, d, amt from df1 where cu=928662")
