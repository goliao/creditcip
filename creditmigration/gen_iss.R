
# load data ---------------------------------------------------------------
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
# load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
# load('db/bondrefall.RData') # using bondrefall for issuance flow earliest date calculation
source('util.r')

if(0){
# generate from scratch ---------------------------------------------------
  require(readxl)
  fnprefix<-'/Users/gliao/Dropbox/Research/ccy basis/data/sdc/'
  # use xlsx file when possible to avoid errors
  sdcfn<-c('sdc_min2_pre-96.xlsx','sdc_1996a_min2.xls','sdc_1996b_min2.xls','sdc_1997a_min2.xls','sdc_1997b_min2.xls','sdc_1998a_min2.xls','sdc_1998b_min2.xls','sdc_1999a_min2.xls','sdc_1999b_min2.xls','sdc_2000a_min2.xls','sdc_2000b_min2.xls','sdc_2001a_min2.xls','sdc_2001b_min2.xls','sdc_2002a_min2.xls','sdc_2002b_min2.xls','sdc_2003a_min2.xls','sdc_2003b_min2.xls','sdc_2004a_min2.xls','sdc_2004b_min2.xls','sdc_2005a_min2.xls','sdc_2005b_min2.xls','sdc_2006a_min2.xls','sdc_2006b_min2.xls','sdc_2007a_min2.xls','sdc_2007b_min2.xls','sdc_2008_min2.xls','sdc_2009_min2.xls','sdc_2010_min2.xls','sdc_2011_min2.xls','sdc_2012_min2.xls','sdc_2013_min2.xls','sdc_2014_min2.xlsx','sdc_2015_min2-till1006.xlsx','sdc_2015b_min2.xlsx','sdc_2016a_min2.xlsx','sdc_min2_2016_july-sept.xlsx')
  colnamein<-c('i','tic','nat','rank_domicile_nation','tf_macro_desc','tf_mid_desc','sicp','cu','cusip9','upnames','upnat','upcusip','upsicp','pub','d','mat','settlement','deal_no','master_deal_type','issue_type_desc','secur','descr','mkt','c','sp','mdy','cur','y','hyld','bps','rule144a','exch','num_packageid','main_tranche','uop','ytofm','rank1_overallot_totdolamtpro','proovsld','amt','procds','totdolamt','intldollaramount','euro_rank1_overallot_proceeds','amtosold','rank1_overallot_totdolamt','r_totdolamtpro','buss','usexchangerate','fmcod','btypc','glaw','country_of_incorp','isin','exchnatregc','supranational','upsupranational','packageid','mdealtype')
  coltypes<-c("text","text","text","text","text","text","text","text","text","text","text","text","text","text","date","text","text","numeric","text","text","text","text","text","text","text","text","text","text","text","numeric","text","text","numeric","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","text","text","text","text","text","text","text","text","numeric","text")
  
  registerDoParallel(1)
  sdc.raw.ls <- foreach (fn=sdcfn) %dopar% {
    print(fn)
    temp <- read_excel(str_c(fnprefix,fn),na='-',skip=3,col_names = colnamein,col_types = coltypes) %>% as.data.table()
    print(dim(temp))
    temp
  }  
  # remove duplicates from file sdc_2015_min2-till1006.xlsx and sdc_min2_2016_july-sept.xlsx
  sdc.0 <- sdc.raw.ls %>% rbindlist(.) %>% unique() %>% unique(fromLast=T,by='deal_no')
  # rid of duplicate column on masterdealtype/mdealtype
  sdc.0[,master_deal_type:=NULL]
  # convert date better
  sdc.0[,d:=ymd(as.character(d))]
  
  
  cleanup.sdc.by.col2<-function(dtsdc){
      # this function cleans up dtsdc by column, replaceing '-'  with NA 
      dtsdc<-copy(dtsdc)
      dtclass<-get.dt.class(dtsdc)
      dtclass[,rowN:=.I]
      for (k in 1:nrow(dtclass)){
        if(dtclass[rowN==k,classtype=='character']){
          cname<-dtclass[rowN==k,columnname]
          dtsdc[,eval(exparse(cname)):=str_trim(eval(exparse(cname)))]
          dtsdc[eval(exparse(cname)) %in% c('','-','N/A','TBA','NA'),eval(exparse(cname)):=NA]
          dtsdc[,eval(exparse(cname)):=str_replace_all(eval(exparse(cname)),'\n|\r',' ')]
        }
        if(cname %in% c('cu','cusip9','sicp','upcusip'))
          dtsdc[,eval(exparse(cname)):=str_replace_all(eval(exparse(cname)),'.000000','')]
        }
      dtsdc
    }
    
  sdc.1<-sdc.0 %>% cleanup.sdc.by.col2(.)
  # parse maturity date  
  sdc.1[,mat2:=mdy(mat)]
  sdc.1[,settlement2:=mdy(settlement)]
  
  # maturity dates here are sometimes unclear whether it's in the 2000s or 1900s, but whatever
  # sdc.1[mat2<d,.(d,mat2,mat,ytofm)]
  # sdc.1[mat2<d,ytofm] %>% summary
  # # sdc.1[,ytofm2:=as.numeric(mat2-d)/365]
  # sdc.1[abs(ytofm2-ytofm)>1,.(d,mat,mat2,ytofm,ytofm2)] 
  
  # this is similar to sdc96.dta
  save(sdc.1,file='sdcall_rgenerated.RData')
}
if(0){
# code used to generate this file: essentially combines sdc96_clea --------
rm(list=ls());load('sdcall_rgenerated.RData') ; fnprefix<-'/Users/gliao/Dropbox/Research/ccy basis/data/sdc/'; source('util.r')

sdc.3<-sdc.1
# update with sdcnew download that matches bbg filtering some how this messes up the data, so let's not use
  # load(file='sdcnew.rdata')
  # dtsdcadd<-df_sdcnew %>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,upnames=upco,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% as.data.table()  %>% cleanup.sdc.by.col(.)
  # dtsdcadd[,id_package_sdc:=NULL];dtsdcadd[,ccy:=str_to_lower(ccy)]
  # sdc3<-update.dt(sdc.1,dtsdcadd,keyfield='deal_no',diagnostic_rt=T,override=T)

# cleaning as done in stata from sdc96 to sdc96_clean 2
  sdc.3[(nat=="Cayman Islands" | nat=="Bermuda" | nat=="Jersey") & (exch=="New York" | exch=="Nasdaq"),nat:="United States"]
  sdc.3[(upnat=="Cayman Islands" | upnat=="Bermuda" | upnat=="Jersey") & (exch=="New York" | exch=="Nasdaq"),upnat:="United States"]
  sdc.3[,monthly:=floor_date(d,'month')]
  
  ezcountries<-c("Austria","Belgium","Cyprus","Finland","France","Germany","Greece","Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")
  sdc.3[,modnat:=nat][modnat %in% ezcountries,modnat:='Eurozone']
  sdc.3[,modupnat:=upnat][modupnat %in% ezcountries,modupnat:='Eurozone']
  
  ccycode<-(fread(str_c(fnprefix,'currency_code_country.csv')) %>% unique())[,ccy:=str_to_lower(ccy)]
  # first add ccy
  sdc.4<-update.dt(sdc.3,unique(ccycode[,.(ccy,cur)]),'cur',override = T,insertnewrow = F)
  # then add national ccy
  sdc.4<-update.dt(sdc.4,unique(ccycode[,.(nat=ctry,natccy=ccy)]),'nat',override = F,insertnewrow = F)
  sdc.4<-update.dt(sdc.4,unique(ccycode[,.(upnat=ctry,upnatccy=ccy)]),'upnat',override = F,insertnewrow = F)
  # need to add FFR,ECU,DM to eur
  sdc.4[,foreign:=ifelse(natccy==ccy,0,1)]
  sdc.4[,upforeign:=ifelse(upnatccy==ccy,0,1)]

  sdc.4[,rating:=ifelse(!is.na(sp) & sp %ni% c('NR','','A1+','CPA1'),sp,mdy)]
  ratinglu<-fread('rating.csv');
  sdc.4<-update.dt(sdc.4,ratinglu,keyfield = 'rating',override = T,insertnewrow=F)
  sdc.4[is.na(nrating),nrating:=0]
  sdc.4[,secur:=stringr::str_to_lower(secur)]  
  # sdc.4[,uop:=str_replace_all(uop,'General Corp. Purp.','GCP')]
  sdc<-sdc.4 
  save(sdc,file='db/sdc.RData')
}


# filtering into sdc clean3---------------------------------------------------------------
rm(list=ls());source('util.r');load('db/sdc.RData')

sdc.1<-sdc %>% filter.sdc(2)
# sdc.1[,.(sumamt=sum(amt)/1000,N=.N),secur] %T>% dt2clip()
# sdc.1[,.(sumamt=sum(amt)/1000,N=.N),tf_macro_desc] %T>% dt2clip()
# sdc.1[,.(sumamt=sum(amt)/1000,N=.N),tf_mid_desc] %T>% dt2clip()
# bb<-sdc.1[,.(sum(amt)/1000),.(monthly,ccy)] 
# bb %>% ggplot(aes(x=monthly,y=V1,colour=ccy))+geom_line()

registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %dopar% {
  sdc.1 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=0)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)


# dtiss.collapse.m %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()

dtiss.collapse.m[ccy=='eur'] %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()
dtiss.collapse.m[ccy=='gbp'] %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()
dtiss.collapse.m[ccy=='jpy'] %>% ggplot(aes(x=date,y=i_netflow,colour=ccy))+geom_line()
dtiss.collapse.m[ccy=='eur'][date %between% c(ymd('2004-09-01'),ymd('2007-10-01'))] %>% head(30)

# check individual issuances in a given month
sdc.1[d %between% c(ymd('2016-09-01'),ymd('2016-10-01'))][ccy=='usd' & modnat=='Eurozone'] %T>% dt2clip
sdc.1[d %between% c(ymd('2016-09-01'),ymd('2016-10-01'))][ccy=='usd' & modnat=='Eurozone'] %T>% dt2clip

sdc.1[monthly=='2006-06-01'][ccy=='usd' & modnat=='Eurozone'][,.(i,amt,descr,buss,uop)][,sum(amt)]
sdc.1[monthly=='2006-06-01'][ccy=='eur' & modnat=='United States'][,.(i,amt,descr,buss,uop)][,sum(amt)]

# cusip from wrds---------------------------------------------------------------

  # cusip from wrds
  # cusip.wrds<-fread('dbin/cusip_wrds.csv',stringsAsFactors=T)
  # cusip.wrds[,.N,CURRENCY_CODE][order(-N)]
  # cusip.wrds[,.N,CURRENCY_CODE][order(-N)]
  # cusip.wrds[,.N,DOMICILE_CODE][order(-N)]
  # cusip.wrds[is.na(DATED_DATE),.N]
  # cusip.wrds[is.na(ISSUE_UPDATE_DATE),.N]
  # cusip.wrds[is.na(MATURITY_DATE),.N]
  # cusip.wrds[is.na(ACTIVITY_DATE),.N]
  # cusip.wrds[is.na(CLOSING_DATE),.N]
  # cusip.wrds[is.na(ACTIVITY_DATE),.N]
  # cusip.wrds[is.na(ACTIVITY_DATE),.N]
