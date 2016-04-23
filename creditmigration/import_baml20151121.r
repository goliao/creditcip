rm(list=ls(all=TRUE))
library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)

df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
setwd("/Users/gliao/Dropbox/Research/ccy basis/data/BAML")


bamlcsv<-function(filecsv="/Users/gliao/Dropbox/Research/ccy basis/data/BAML/indices_1.csv"){
  raw<-read.csv(filecsv,header=FALSE, stringsAsFactors = FALSE)
  if (is.na(raw[2,ncol(raw)])[[1]]) {raw[,ncol(raw)]=NULL}
  
  rows_names<-index(raw)[raw$V1=='Date']
  rows_field<-rows_names-1
  names<-str_to_lower(str_trim(as.character(raw[rows_names[1],]),"both"))
  fieldraw<-str_trim(as.character(raw[rows_field,2]),"both")
  fieldnames<-str_to_lower(str_replace_all(fieldraw,pattern = "[ ./]",replacement = ''))
  rows_field<-c(rows_field,nrow(raw)+4)
  
  i=1
  df1<-raw[rows_field[i]+2:(rows_field[i+1]-4),]
  colnames(df1)<-c(names[1],paste0('',names[2:length(names)],'_',fieldnames[i]))
  df1$date<-as.Date(as.character(df1$date),'%m/%d/%Y')
  for (i in 2:(length(rows_field)-1)){
    dfnew<-raw[(rows_field[i]+2):(rows_field[i+1]-4),]
    colnames(dfnew)<-c(names[1],paste0(names[2:length(names)],'_',fieldnames[i]))
    dfnew$date<-as.Date(as.character(dfnew$date),'%m/%d/%Y')
    nrow(dfnew)
    df1<-merge(df1,dfnew,all=TRUE,by='date')
  }
  for (colname in names(df1)) {
    if (is.character(df1[[colname]])) {
      df1[[colname]] <- as.numeric(df1[[colname]])
    }
  }
  df1
}


# 
filenames<-c('corpAAA_160205.csv','eur_indices1.csv','gbp_indices3.csv','eur_indices2.csv','indices_1.csv','eur_indices3.csv','jpy_indices.csv','gbp_indices1.csv','usdindicescsv.csv','gbp_indices2.csv','indices_add_151123.csv','indices_additional_20151129.csv')

dfout<-bamlcsv(filenames[1])
for (i in 2:length(filenames)){
  dfnew<-bamlcsv(filenames[i])
  dfout<-merge(dfout,dfnew,all=TRUE,by='date', suffixes=c("",".z"))
}
dfout=dfout[dfout$date>'1996-01-01',]

write.dta(dfout,'baml_indices.dta')
write.dta(dfout,'/Users/gliao/Dropbox/Research/ccy basis/creditmigration/baml_indices.dta')


## Daily
filenames<-c('ind_daily_151207.csv')
dfout<-bamlcsv(filenames[1])
write.dta(dfout,'/Users/gliao/Dropbox/Research/ccy basis/creditmigration/baml_indices_daily.dta')
