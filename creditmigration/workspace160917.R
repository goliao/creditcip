# Proposition: Debt maturity affects net deviation
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
# load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
load('db/sdc_raw.RData')
load('db/bondref.RData')
load('firmlevel.rdata')

# credit spread differential for each individual firm
	dtcred<-copy(regresult)
	dtcred %>% tocusip6(field='upcusip')
	dtcred %>% setkey(cusip6)
	dtcred[,ym:=floor_date(date,'month')]

	# Taking only the first date of the month
	dtcred.l<-dtcred %>% dcast(ym+upcusip+ccy~ytype,value.var='est')
	
# bond issuance date and reference data from SDC
	dtiss<-copy(dt.sdc.raw) 
	# dtiss<-copy(bondref) 
	dtiss %>% tocusip6(field='cusip9')
	dtiss %>% setkey(cusip6)
	dtiss[,ccy:=str_to_lower(ccy)]
	dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]
	dtiss<-dtiss[amt>=100][pub!='Govt.']  %>% as.data.table()  %>% issfilter(4) #%>% semi_join(dtcred,by='cusip6')
	dtiss[,ym:=floor_date(d,'quarter')]

# see the correlation btw debt mature and issuance
	# generate issued amoutn by firm and month
	issued_mo<-dtiss[,.(issued=sum(amt)),.(ym,cusip6)]
	# generate matured amount by firm and month
	dtiss[,ym_matured:=floor_date(mat2,'quarter')]
	matured_mo<-dtiss[,.(matured=sum(amt)),.(ym_matured,cusip6)] %>% copy(.)
	# rename 
	matured_mo %>% setnames('ym_matured','ym')
	matured_mo %>% setkey(ym,cusip6)
	issued_mo %>% setkey(ym,cusip6)
	#limit to only '04 to 16
	matured_mo<-matured_mo[!is.na(ym)][ym %between% c(ymd('2004-01-01','2016-07-01'))]
	issued_mo<-issued_mo[!is.na(ym)][ym %between% c(ymd('2004-01-01','2016-07-01'))]
	# merge and fill NAs with 0
	issmat_mo<-merge(matured_mo, issued_mo,by=c('ym','cusip6'),all=T)
	issmat_mo[is.na(matured),matured:=0][is.na(issued),issued:=0]
	issmat_mo<-issmat_mo[!is.na(cusip6)]
	# reg issued amt on matured amt
	reg1<-issmat_mo %>% lm(issued~matured+factor(ym)+factor(cusip6),data=.)
	summary(reg1)$coefficients %>% head

	reg1<-issmat_mo[issued>0 & matured>0] %>% lm(issued~matured+factor(ym)+factor(cusip6),data=.)
	summary(reg1)$coefficients %>% head

	issmat_mo[matured>0 & issued>0]
	issmat_mo[,cor(matured,issued)]
	issmat_mo[matured>0 & issued>0,cor(matured,issued)]


	# use dummy to indicate whether there has been an issuance or maturity
	issmat_mo_dum<-copy(issmat_mo)
	issmat_mo_dum[matured>0,matured:=1][issued>0,issued:=1]
	#linear probability model
	reg2<-issmat_mo_dum %>% lm(issued~matured+factor(ym)+factor(cusip6),data=.)
	summary(reg2)$coefficients %>% head

	issmat_mo_dum[,cor(matured,issued)]

	# sum up amt issued per firm, month, and ccy and transform to long
	dtiss2<-dtiss[,.(iss=sum(amt)),.(ym,upcusip,ccy)] %>% dcast(ym+upcusip~ccy,value.var='iss')
	# count if bond is issued in ccy
	dtiss2[is.na(aud),aud:=0][aud>0,aud:=1]; dtiss2[is.na(eur),eur:=0][eur>0,eur:=1]; dtiss2[is.na(usd),usd:=0][usd>0,usd:=1]; dtiss2[is.na(cad),cad:=0][cad>0,cad:=1]; dtiss2[is.na(jpy),jpy:=0][jpy>0,jpy:=1]; dtiss2[is.na(gbp),gbp:=0][gbp>0,gbp:=1]; dtiss2[is.na(chf),chf:=0][chf>0,chf:=1]; 
	dtiss.l<-dtiss2  %>% melt(id.vars=c('ym','upcusip'),variable.name='ccy',value.name='issued')



# merge
	dtiss.l %>% setkey(ym,upcusip,ccy)
	dtcred.l %>% setkey(ym,upcusip,ccy)
	
	dtregiss<-dtiss.l[dtcred.l]
	dtregiss<-dtregiss[!is.na(netdiff)]
	dtregiss[is.na(issued),issued:=0]
	dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.025)
	dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.025)	
	dtregiss[,cip:=crddiff-netdiff]

	dtregiss[,ccy:=as.character(ccy)]
	dtregiss %>% write.dta('dta/isscred_firm1.dta')
# regressing quantity on price
	reg1<-dtregiss %>% lm(issued~crddiff+cip+factor(ym)+factor(upcusip)+factor(ccy),data=.)
	summary(reg1)$coefficients %>% head

# regressing price change on quantity
	reg2<-dtregiss %>% lm(abs(netdiff)~issued+factor(ym)+factor(upcusip)+factor(ccy),data=.)
	summary(reg2)$coefficients %>% head

