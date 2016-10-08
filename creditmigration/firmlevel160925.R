# based on workspace160907.R

# load data & process ---------------------------------------------------------------
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
# setwd('/mnt/disks/xccy/creditmigration')
rm(list=ls(all=TRUE));
load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
# load('db/bondrefall.RData')
load('db/sdc_raw.RData')
#load('monthlyrun.rdata')


# setup -------------------------------------------------------------------
firmlevel <- 'upcusip'

# initial calc ------------------------------------------------------------
#bondref <- (bondref %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
#dt.sdc.raw <- (dt.sdc.raw %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
#dtl<-(dtm$dtl4 %>% tocusip6(field=firmlevel))[,upcusip:=cusip6]


# calc firm level deviations ----------------------------------------------

# dtregdata<-copy(dtl)
# regresult_old<-estimate.crddiff.firm(dtregdata,parallel.core=6)	
# #save(regresult,file='firmlevel.rdata')	
# 
# regresult_v2<-estimate.crddiff.firm(dtregdata,parallel.core=6,regversion=2)
# # save(regresult_v2,file='firmlevelmo_alt.RData')
# regresult_v3<-estimate.crddiff.firm(dtregdata,parallel.core=6,regversion=3)	
# # save(regresult_v2,regresult_v3,file='firmlevelmo_alt.RData')
# regresult_v4<-estimate.crddiff.firm(dtregdata,parallel.core=6,regversion=4)	
# # save(regresult_v2,regresult_v3,regresult_v4,file='firmlevelmo_alt.RData')



# cik ---------------------------------------------------------------------
# load CIK cusip look up
load('db/cik_cusip.RData')
cik_cusip[,.(str_length(co_cusip))][,.N,V1]
cik_cusip[,cusip6:=str_sub(co_cusip,1,6)]
cik_cusip %>% setkey(cusip6)
cik.<-cik_cusip[,.(cusip6,cik=co_cik)]
cik.<-cik. %>% setkey(cusip6,cik) %>% unique()
cik.<-cik.[cik!='' & cusip6 !='']
cik. %>% setkey(cusip6)



# show issuance choice and credit cip -------------------------------------
load('firmlevel.rdata')
dtcred<-copy(regresult)
dtcred.mt<-dtcred[order(date),.(est=first(est)),.(upcusip,ym,ytype,ccy)] %>% dcast(ym+upcusip+ccy~ytype,value.var='est')

# dtiss<-copy(bondref)
dtiss<-copy(dt.sdc.raw)
dtiss[,cusip6:=str_sub(upcusip,1,6)]  #### could chg this to cusip9
dtiss[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
dtiss %>% setkey(cusip6)
dtiss<-cik.[dtiss]
dtiss[,ccy:=str_to_lower(ccy)]
dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]

dtiss %>% setkey(upcusip); dtcred %>% setkey(upcusip)
dtiss2<-dtiss %>% semi_join(dtcred,by='upcusip') %>% as.data.table()  %>% issfilter(4)  
dtiss2[!is.na(cik),.N]

# floor the issuance dates
dtiss2[,ym:=floor_date(d,'month')];dtcred[,ym:=floor_date(date,'month')]

dtiss3<-dtiss2[,.N,.(ym,upcusip,cik,ccy)] %>% dcast(ym+upcusip+cik~ccy,value.var='N')
dtiss3[is.na(aud),aud:=0][aud>0,aud:=1]; dtiss3[is.na(eur),eur:=0][eur>0,eur:=1]; dtiss3[is.na(usd),usd:=0][usd>0,usd:=1]; dtiss3[is.na(cad),cad:=0][cad>0,cad:=1]; dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1]; dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1]; dtiss3[is.na(chf),chf:=0][chf>0,chf:=1]; 
dtiss3[,issued:=max(eur,usd,gbp,jpy,aud,cad,chf)]
dtiss3 %>% setkey(ym,upcusip);dtcred %>% setkey(ym,upcusip)
dtiss3[,.N,issued]
dtiss3[,sum(usd)]
dtiss3[,sum(eur)]
## reshape into multi currency reg version
dtcred.mt %>% key
dtiss3 %>% key
dtregiss[,.N,ccy]

dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01']#[issued==1]#[!is.na(cik)]
dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
dtregiss[,cip:=crddiff-netdiff]

dtregiss[ccy=='eur',issuedinccy:=eur];		dtregiss[ccy=='aud',issuedinccy:=aud];		dtregiss[ccy=='cad',issuedinccy:=cad];		dtregiss[ccy=='jpy',issuedinccy:=jpy];		dtregiss[ccy=='gbp',issuedinccy:=gbp];		dtregiss[ccy=='chf',issuedinccy:=chf];		
dtregiss[,.(mean(na.omit(cip))),ccy]

dtregiss[,time.factor:=factor(ym)]
dtregiss[issuedinccy==1,issuedinccy:=100]
reg1<-list()
reg1[[1]]<-dtregiss %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1[[2]]<-dtregiss %>% felm(issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1[[3]]<-dtregiss %>% felm(issuedinccy~crddiff+cip|0|0|time.factor+upcusip,data=.)
reg1[[4]]<-dtregiss %>% felm(issuedinccy~netdiff|0|0|time.factor+upcusip,data=.)

reg1 %>% stargazer(type='text',report='vct*')

dtregiss[,.N,ccy]
dtregiss[ccy %in% c('usd','eur')] %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)


reg1<-list()
reg1[[1]]<- regformat(dtin=dtregiss,formula='issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip',value.name='both',regtype='felm')
reg1[[2]]<- regformat(dtin=dtregiss,formula='issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip',value.name='netdiff',regtype='felm')
regfirmlevel<-(rbindlist(reg1) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regfirmlevel %T>% dt2clip()

# reg3prob<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="probit"), data=.)
# summary(reg3prob)$coefficients %>% head

# reg3logit<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="logit"), data=.)
# summary(reg3prob)$coefficients %>% head


# redo --------------------------------------------------------------------


# show issuance choice and credit cip -------------------------------------
load('firmlevel.rdata')
dtcred<-copy(regresult)
dtcred.mt<-dtcred[order(date),.(est=first(est)),.(upcusip,ym,ytype,ccy)] %>% dcast(ym+upcusip+ccy~ytype,value.var='est')

# dtiss<-copy(bondref)
dtiss<-copy(dt.sdc.raw)
dtiss[,cusip6:=str_sub(upcusip,1,6)]  #### could chg this to cusip9
dtiss[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
dtiss %>% setkey(cusip6)
dtiss<-cik.[dtiss]
dtiss[,ccy:=str_to_lower(ccy)]
dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]

dtiss %>% setkey(upcusip); dtcred %>% setkey(upcusip)
dtiss2<-dtiss %>% semi_join(dtcred,by='upcusip') %>% as.data.table()  %>% issfilter(4) 
dtiss2[!is.na(cik),.N]

# floor the issuance dates
dtiss2[,ym:=floor_date(d,'month')];dtcred[,ym:=floor_date(date,'month')]

dtiss3<-dtiss2[,.N,.(ym,upcusip,cik,ccy)] %>% dcast(ym+upcusip+cik~ccy,value.var='N')
dtiss3[is.na(aud),aud:=0][aud>0,aud:=1]; dtiss3[is.na(eur),eur:=0][eur>0,eur:=1]; dtiss3[is.na(usd),usd:=0][usd>0,usd:=1]; dtiss3[is.na(cad),cad:=0][cad>0,cad:=1]; dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1]; dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1]; dtiss3[is.na(chf),chf:=0][chf>0,chf:=1]; 
dtiss3[,issued:=eur+usd+aud+cad+chf+gbp+jpy,.I];dtiss3
dtiss3[,issuedin.eur.usd:=eur+usd,.I];dtiss3
dtiss3 %>% setkey(ym,upcusip);dtcred %>% setkey(ym,upcusip)
dtiss3[,.N,issued]
dtiss3[,.N,issuedin.eur.usd]
dtiss3[,sum(usd)]
dtiss3[,sum(eur)]
## reshape into multi currency reg version
dtiss4<-dtiss3 %>% melt.data.table(id.vars=c('ym','upcusip','cik','issued','issuedin.eur.usd'),variable.name = 'ccy',value.nam='issuedinccy')
#quick check that we didn't mess up and lose observations
  dtiss3[,sum(aud+cad+chf+eur+gbp+jpy+usd)]==dtiss4[,sum(issuedinccy)]

dtcred.mt %>% key
dtiss4 %>% setkey(ym,upcusip,ccy)

dtcred.mt %>% unique


dtregiss<-dtcred.mt[dtiss4][ym>='2004-01-01']#[issued==1]#[!is.na(cik)]
dtregiss[,.N]
dtregiss[ccy=='usd',`:=`(crddiff=0,netdiff=0)]
dtregiss<-dtregiss[!is.na(crddiff) & !is.na(netdiff)];dtregiss[,.N]

winsor=.01
dtregiss[,pctl:=percent_rank(crddiff),by=c('ym','ccy')]
dtregiss<-dtregiss[(pctl>=winsor & pctl<=(1-winsor)) | ccy=='usd'];dtregiss[,.N]

dtregiss[,pctl:=percent_rank(netdiff),by=c('ym','ccy')]
dtregiss<-dtregiss[(pctl>=winsor & pctl<=(1-winsor)) | ccy=='usd'];dtregiss[,.N]

# dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
# dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	

dtregiss[,cip:=crddiff-netdiff]
dtregiss[,.N,ccy]
dtregiss[,.(mean(na.omit(cip))),ccy]

dtregiss[,time.factor:=factor(ym)]
dtregiss[issuedinccy==1,issuedinccy:=100]
reg1<-list()
reg1[[1]]<-dtregiss %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1[[2]]<-dtregiss %>% felm(issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1[[3]]<-dtregiss[ccy %in% c('eur','gbp','chf','jpy','aud','cad')] %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1[[4]]<-dtregiss[ccy %in% c('eur','gbp','chf','jpy','aud','cad')] %>% felm(issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
# *reg1[[5]]<-dtregiss[issuedin.eur.usd==1] %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
# reg1[[6]]<-dtregiss[issuedin.eur.usd==1] %>% felm(issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)
reg1 %>% stargazer(type='text',report='vct*')


dtregiss

dtregiss[,.N,ccy]
dtregiss[ccy %in% c('usd','eur')] %>% felm(issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip,data=.)


reg1<-list()
reg1[[1]]<- regformat(dtin=dtregiss,formula='issuedinccy~crddiff+cip|time.factor+upcusip+ccy|0|time.factor+upcusip',value.name='both',regtype='felm')
reg1[[2]]<- regformat(dtin=dtregiss,formula='issuedinccy~netdiff|time.factor+upcusip+ccy|0|time.factor+upcusip',value.name='netdiff',regtype='felm')
regfirmlevel<-(rbindlist(reg1) %>% dcast(order+rn+variable~varname))[order(order,rn,variable)][,!c('order','variable'),with=F]
regfirmlevel %T>% dt2clip()

# reg3prob<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="probit"), data=.)
# summary(reg3prob)$coefficients %>% head

# reg3logit<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="logit"), data=.)
# summary(reg3prob)$coefficients %>% head


# quarterly ---------------------------------------------------------------


  dtcred<-copy(regresult)
  
  # dtiss<-copy(bondref)
  dtiss<-copy(dt.sdc.raw)
  dtiss[,cusip6:=str_sub(upcusip,1,6)]  #### could chg this to cusip9
  dtiss[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
  dtiss %>% setkey(cusip6)
  dtiss<-cik.[dtiss]
  # dtiss[,.N]
  # dtiss[!is.na(cik),.N]
  dtiss[,ccy:=str_to_lower(ccy)]
  dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]
  # dtiss<-dtiss[ccy %in% c('eur','usd')]
  dtiss %>% setkey(upcusip)
  dtcred %>% setkey(upcusip)
  dtiss2<-dtiss %>% semi_join(dtcred,by='upcusip') %>% as.data.table()  %>% issfilter(4)  
  dtiss2[!is.na(cik),.N]
  
  dtiss2[,yq:=floor_date(d,'quarter')]
  dtcred[,yq:=floor_date(date,'quarter')]

## DOES NOT MAKE ANY SENSE WHAT SO EVER TO FILTER BY GLOBAL HERE SINCE EVEN IF FIRMS DON'T ISSUE BONDS IN THE SAME QUARTER IN TWO DIFFERENT CCYS, THEY STILL HAVE BONDS IN TWO DIFF CCY
##### WRONG dtiss2<-dtiss2 %>% filterglobaluponly()

  dtcred.qt<-dtcred[order(date),.(est=first(est)),.(upcusip,yq,ytype,ccy)] %>% dcast(yq+upcusip+ccy~ytype,value.var='est')
  
  dtiss3<-dtiss2[,.N,.(yq,upcusip,cik,ccy)] %>% dcast(yq+upcusip+cik~ccy,value.var='N')
  dtiss3[is.na(aud),aud:=0][aud>0,aud:=1];	dtiss3[is.na(eur),eur:=0][eur>0,eur:=1];	dtiss3[is.na(usd),usd:=0][usd>0,usd:=1];	dtiss3[is.na(cad),cad:=0][cad>0,cad:=1];	dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1];	dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1];	dtiss3[is.na(chf),chf:=0][chf>0,chf:=1];	dtiss3[,issued:=max(eur,usd)]
  
  dtiss3 %>% setkey(yq,upcusip)
  dtcred %>% setkey(yq,upcusip)


## reshape into multi currency reg version
  dtregiss<-dtcred.qt[dtiss3][yq>='2004-01-01'][issued==1]#[!is.na(cik)]
  # dtregiss[,.N]
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('yq','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('yq','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',issuedinccy:=eur];	dtregiss[ccy=='aud',issuedinccy:=aud];	dtregiss[ccy=='cad',issuedinccy:=cad];	dtregiss[ccy=='jpy',issuedinccy:=jpy];	dtregiss[ccy=='gbp',issuedinccy:=gbp];	dtregiss[ccy=='chf',issuedinccy:=chf];	
  dtregiss[,.N]
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  reg3<-dtregiss %>% lm(issuedinccy~crddiff+cip+factor(yq)+factor(upcusip)+factor(ccy),data=.)	
  regout(reg3)	
  reg3<-dtregiss %>% lm(issuedinccy~netdiff+factor(yq)+factor(upcusip)+factor(ccy),data=.)
  regout(reg3)

# regress netdiff/crd chg on issuance amt/issuance indicator at the firm level

  reg3prob<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(yq)+factor(upcusip)+factor(ccy), family=binomial(link="probit"), data=.)
  
  regoutz(reg3prob,statname='z value')
  reg3prob<-dtregiss %>% glm(issuedinccy~netdiff+factor(yq)+factor(upcusip)+factor(ccy), family=binomial(link="probit"), data=.)
  regoutz(reg3prob,statname='z value')
  
  
  reg3logit<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(yq)+factor(upcusip), family=binomial(link="logit"), data=.)
  summary(reg3prob)$coefficients %>% head
  
  # try only with cik
  dtregiss<-dtcred.qt[dtiss3][yq>='2004-01-01'][issued==1][!is.na(cik)][ccy=='eur']
  dtregiss[,.N]
  dtregiss[!is.na(cik),.N]
  reg2<-dtregiss %>% lm(eur~netdiff+factor(yq)+factor(upcusip),data=.)
  summary(reg2)$coefficients %>% head

  # reg2prob<-dtregiss[issued==1] %>% glm(eur~est+factor(yq)+factor(upcusip), family=binomial(link="probit"), data=.)
  # summary(reg2prob)$coefficients %>% head
  dtregiss

  reg2<-dtregiss[issued==1] %>% lm(eur~est+factor(yq),data=.)
  summary(reg2)$coefficients %>% head

  #################################### netdiff ~ ISSUE 
  # price impact#####################
  # reg4<-dtregiss %>% lm(chgnetdiff~issuedinccy/amt+factor(yq)+factor(upcusip),data=.)
  # summary(reg4)$coefficients %>% head
  ### what works
  dtregiss<-dtcred.qt[dtiss3][yq>='2004-01-01']#[issued==1]#[!is.na(cik)]
  # dtregiss<-merge(dtcred.qt,dtiss3,all=T)[yq>='2004-01-01']#[issued==1]#[!is.na(cik)]
  dtregiss[,.N,issued]
  dtregiss[is.na(issued),issued:=0]
  
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('yq','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('yq','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',issuedinccy:=eur];	dtregiss[ccy=='aud',issuedinccy:=aud];	dtregiss[ccy=='cad',issuedinccy:=cad];	dtregiss[ccy=='jpy',issuedinccy:=jpy];	dtregiss[ccy=='gbp',issuedinccy:=gbp];	dtregiss[ccy=='chf',issuedinccy:=chf];	dtregiss[,.N]
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  dtregiss2<-dtregiss[,.(yq,issued,issuedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
  dtregiss2[,.N,issuedinccy]
  dtregiss[is.na(issuedinccy),issuedinccy:=0]
  # dtregiss2 %>% dt2clip
  reg4<-dtregiss2 %>% lm(chgnetdiff~issuedinccy+factor(yq)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  
  
  ### what's not really working but equivalent
  # dtregiss<-dtcred.qt[dtiss3][yq>='2004-01-01']#[issued==1]#[!is.na(cik)]
  
  dtregiss<-merge(dtcred.qt,dtiss3,by=c('yq','upcusip'),all=T)[yq>='2004-01-01']#[issued==1]#[!is.na(cik)]
  dtregiss[,.N,issued]
  dtregiss[is.na(issued),issued:=0]
  
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('yq','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('yq','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',issuedinccy:=eur];	dtregiss[ccy=='aud',issuedinccy:=aud];	dtregiss[ccy=='cad',issuedinccy:=cad];	dtregiss[ccy=='jpy',issuedinccy:=jpy];	dtregiss[ccy=='gbp',issuedinccy:=gbp];	dtregiss[ccy=='chf',issuedinccy:=chf];	dtregiss[,.N]
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  dtregiss2<-dtregiss[,.(yq,issued,issuedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
  dtregiss[,.N,issuedinccy]
  dtregiss[is.na(issuedinccy),issuedinccy:=0]
  # dtregiss2 %>% dt2clip
  reg4<-dtregiss2 %>% lm(chgnetdiff~issuedinccy+factor(yq)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  dtregiss[issued==1,.N,issuedinccy]
  reg4<-dtregiss2[issued==1] %>% lm(chgnetdiff~issuedinccy+factor(yq)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  
  # need to used abs shrinkage defnition to measure reduction of deviation
  dtregiss[,.N,.(issued,issuedinccy)]
  reg4<-dtregiss2 %>% lm(chgabsnetdiff~issuedinccy+issued+factor(yq)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  reg4<-dtregiss2 %>% lm(chgabsnetdiff~issued+factor(yq)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  


# Monthly -----------------------------------------------------------------




# Monthly Price impact; reduction of net dev  -----------------------------


#################################### netdiff ~ ISSUE #### MONTHLY
# reg4<-dtregiss %>% lm(chgnetdiff~issuedinccy/amt+factor(ym)+factor(upcusip),data=.)
# summary(reg4)$coefficients %>% head
### what works
dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01'][issued==1]#[!is.na(cik)]
# dtregiss<-merge(dtcred.mt,dtiss3,all=T)[ym>='2004-01-01']#[issued==1]#[!is.na(cik)]
dtregiss[,.N,issued]
dtregiss[is.na(issued),issued:=0]

dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
dtregiss[,cip:=crddiff-netdiff]
dtregiss[ccy=='eur',issuedinccy:=eur];	dtregiss[ccy=='aud',issuedinccy:=aud];	dtregiss[ccy=='cad',issuedinccy:=cad];	dtregiss[ccy=='jpy',issuedinccy:=jpy];	dtregiss[ccy=='gbp',issuedinccy:=gbp];	dtregiss[ccy=='chf',issuedinccy:=chf];	dtregiss[,.N]
dtregiss[,.(mean(na.omit(cip))),ccy]

dtregiss2<-dtregiss[,.(ym,issued,issuedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
dtregiss2[,.N,issuedinccy]

# dtregiss2 %>% dt2clip
reg4<-dtregiss2 %>% lm(chgnetdiff~issuedinccy+factor(ym)+factor(upcusip),data=.)
as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 


### what's not really equivalent but equivalent
dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01'][issued==1]#[!is.na(cik)]

# dtregiss<-merge(dtcred.mt,dtiss3,by=c('ym','upcusip'),all=T)[ym>='2004-01-01']#[issued==1]#[!is.na(cik)]
dtregiss[,.N,issued]
dtregiss[is.na(issued),issued:=0]

dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
dtregiss[,cip:=crddiff-netdiff]
dtregiss[ccy=='eur',issuedinccy:=eur];	dtregiss[ccy=='aud',issuedinccy:=aud];	dtregiss[ccy=='cad',issuedinccy:=cad];	dtregiss[ccy=='jpy',issuedinccy:=jpy];	dtregiss[ccy=='gbp',issuedinccy:=gbp];	dtregiss[ccy=='chf',issuedinccy:=chf];	dtregiss[,.N]
dtregiss[,.(mean(na.omit(cip))),ccy]

dtregiss2<-dtregiss[,.(ym,issued,issuedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
dtregiss[,.N,issuedinccy]
dtregiss[is.na(issuedinccy),issuedinccy:=0]

reg4<-dtregiss2 %>% lm(chgnetdiff~issuedinccy+factor(ym)+factor(upcusip),data=.)
as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
regout(reg4)

# ## this works well
reg4<-dtregiss2 %>% lm(chgabsnetdiff~issued+factor(ym)+factor(upcusip),data=.)
as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
regout(reg4)

reg4<-dtregiss2 %>% lm(chgabsnetdiff~issued+factor(ym)+factor(upcusip)+factor(ccy),data=.)
as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 

reg4<-dtregiss2 %>% lm(chgabsnetdiff~issued+factor(upcusip)+factor(ym)+factor(ccy)+factor(ym)*factor(ccy),data=.)
as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 

# # dtregiss2 %>% dt2clip
# reg4<-dtregiss2 %>% lm(chgnetdiff~issuedinccy+factor(ym)+factor(upcusip),data=.)
# as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
# dtregiss[issued==1,.N,issuedinccy]
# reg4<-dtregiss2[issued==1] %>% lm(chgnetdiff~issuedinccy+factor(ym)+factor(upcusip),data=.)
# as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 

# # need to used abs shrinkage defnition to measure reduction of deviation
# dtregiss[,.N,.(issued,issuedinccy)]
# reg4<-dtregiss2 %>% lm(chgabsnetdiff~issuedinccy+factor(ym)+factor(upcusip),data=.)
# as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
# reg4<-dtregiss2 %>% lm(chgabsnetdiff~issued+factor(ym)+factor(upcusip),data=.)
# as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 

########### debt that are due;
  dtcred<-copy(regresult)
  # dtiss<-copy(bondref)
  dtiss<-copy(dt.sdc.raw)
  dtiss[,cusip6:=str_sub(upcusip,1,6)]  #### could chg this to cusip9
  dtiss[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
  dtiss %>% setkey(cusip6)
  dtiss<-cik.[dtiss]
  dtiss[,.N]
  dtiss[!is.na(cik),.N]
  dtiss[,ccy:=str_to_lower(ccy)]
  dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]
  # dtiss<-dtiss[ccy %in% c('eur','usd')]
  dtiss %>% setkey(upcusip)
  dtcred %>% setkey(upcusip)
  dtiss2<-dtiss %>% semi_join(dtcred,by='upcusip') %>% as.data.table()  %>% issfilter(4)  
  dtiss2[!is.na(cik),.N]
  ############ CHG HERE		
  dtiss2[,ym:=floor_date(mat2,'month')]
  dtcred[,ym:=floor_date(date,'month')]
  
  dtcred.mt<-dtcred[order(date),.(est=first(est)),.(upcusip,ym,ytype,ccy)] %>% dcast(ym+upcusip+ccy~ytype,value.var='est')
  
  dtiss3<-dtiss2[,.N,.(ym,upcusip,cik,ccy)] %>% dcast(ym+upcusip+cik~ccy,value.var='N')
  dtiss3[is.na(aud),aud:=0][aud>0,aud:=1]; dtiss3[is.na(eur),eur:=0][eur>0,eur:=1]; dtiss3[is.na(usd),usd:=0][usd>0,usd:=1]; dtiss3[is.na(cad),cad:=0][cad>0,cad:=1]; dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1]; dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1]; dtiss3[is.na(chf),chf:=0][chf>0,chf:=1]; 
  dtiss3[,debtmatured:=max(eur,usd)]
  
  dtiss3 %>% setkey(ym,upcusip)
  dtcred %>% setkey(ym,upcusip)


## reshape into multi currency reg version
  dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01']#[debtmatured==1]#[!is.na(cik)]
  dtregiss[,.N]
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',debtmaturedinccy:=eur];		dtregiss[ccy=='aud',debtmaturedinccy:=aud];		dtregiss[ccy=='cad',debtmaturedinccy:=cad];		dtregiss[ccy=='jpy',debtmaturedinccy:=jpy];		dtregiss[ccy=='gbp',debtmaturedinccy:=gbp];		dtregiss[ccy=='chf',debtmaturedinccy:=chf];
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  
  dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01']#[debtmatured==1]#[!is.na(cik)]
  dtregiss[,.N,debtmatured]
  dtregiss[is.na(debtmatured),debtmatured:=0]
  
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',debtmaturedinccy:=eur];	dtregiss[ccy=='aud',debtmaturedinccy:=aud];	dtregiss[ccy=='cad',debtmaturedinccy:=cad];	dtregiss[ccy=='jpy',debtmaturedinccy:=jpy];	dtregiss[ccy=='gbp',debtmaturedinccy:=gbp];	dtregiss[ccy=='chf',debtmaturedinccy:=chf];	dtregiss[,.N]
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  dtregiss2<-dtregiss[,.(ym,debtmatured,debtmaturedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
  dtregiss2[,.N,debtmaturedinccy]
  dtregiss[is.na(debtmaturedinccy),debtmaturedinccy:=0]
  # dtregiss2 %>% dt2clip
  reg4<-dtregiss2 %>% lm(chgabsnetdiff~debtmaturedinccy+factor(ym)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 


### what's not really equivalent but equivalent
  dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01']#[debtmatured==1]#[!is.na(cik)]
  
  dtregiss<-merge(dtcred.mt,dtiss3,by=c('ym','upcusip'),all=T)[ym>='2004-01-01']#[debtmatured==1]#[!is.na(cik)]
  dtregiss[,.N,debtmatured]
  dtregiss[is.na(debtmatured),debtmatured:=0]
  
  dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
  dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
  dtregiss[,cip:=crddiff-netdiff]
  dtregiss[ccy=='eur',debtmaturedinccy:=eur];	dtregiss[ccy=='aud',debtmaturedinccy:=aud];	dtregiss[ccy=='cad',debtmaturedinccy:=cad];	dtregiss[ccy=='jpy',debtmaturedinccy:=jpy];	dtregiss[ccy=='gbp',debtmaturedinccy:=gbp];	dtregiss[ccy=='chf',debtmaturedinccy:=chf];	dtregiss[,.N]
  dtregiss[,.(mean(na.omit(cip))),ccy]
  
  dtregiss2<-dtregiss[,.(ym,debtmatured,debtmaturedinccy,crddiff,netdiff,chgcrddiff=diff(crddiff),chgnetdiff=diff(netdiff),chgabsnetdiff=diff(abs(netdiff))),.(ccy,upcusip)] 
  dtregiss[,.N,debtmaturedinccy]
  dtregiss[is.na(debtmaturedinccy),debtmaturedinccy:=0]

# ## this works well
  reg4<-dtregiss2 %>% lm(chgabsnetdiff~debtmatured+factor(ym)+factor(upcusip),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  
  reg4<-dtregiss2 %>% lm(chgabsnetdiff~debtmatured+factor(ym)+factor(upcusip)+factor(ccy),data=.)
  as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  
  # reg4<-dtregiss2 %>% lm(chgabsnetdiff~debtmatured+factor(upcusip)+factor(ym)+factor(ccy)+factor(ym)*factor(ccy),data=.)
  # as.data.table(summary(reg4)$coefficients,keep.rownames=T)[rn %nlk% '^factor'] 
  
