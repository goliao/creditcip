# workspace160907.R

setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
# setwd('/mnt/disks/xccy/creditmigration')
rm(list=ls(all=TRUE));
# load('monthlyrun.rdata')
load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
# load('db/bondrefall.RData')
 load('db/sdc_raw.RData')
load('monthlyrun.rdata')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)


# save(dtm,file='monthlyrun.rdata')
# dtm$dtl4 %>% write.dta('dta/dtl4.dta')
# regdata<-dtm$dtl4
# regdata[ccy=='usd',ccy:='1usd']
# reg1<-regdata[date=='2016-05-31'] %>% lm(swapsprd~ccy+upcusip,data=.)
# reg<-dtm$dtl4 %>% lm(swapsprd~factor(date)+ccy+factor(date)*ccy,data=.)
# #  reg specification with ccy*upcusip interaction
# 	dtregdata<-copy(dtm$dtl4)
# 	dtregdata[ccy=='usd',ccy:='1usd']
# 	winsor=.025
# 	dtregdata[,pctl:=percent_rank(swapsprd),by=.(date,ccy)]
# 	dtregdata2<-dtregdata[pctl>=winsor & pctl<=(1-winsor)]
# 	dtregdata3<-dtregdata2 %>% filterglobaluponly()

# 	# dtregdata3 %>% write.dta('dta/dtl4_upglobalonly.dta')
	
# 	dtregdata4<-dtregdata3[date=='2016-05-31'][ccy %in% c('1usd','eur')] %>% filterglobaluponly() 
# 	# dtregdata4[upcusip=='000375',upcusip:='zzz357']
# 	dtregdata4[upcusip=='W90937',upcusip:='000000']
# 	reg1<-dtregdata4%>% lm(swapsprd~ccy+upcusip+ccy*upcusip,data=.)
# 	reg1coef<-summary(reg1)$coefficients %>% as.data.table(keep.rownames=T)
# 	reg1coef[rn %like% 'Intercept|W90937|^ccy1usd|^ccyeur$']
# 	reg1coef[rn %like% 'Intercept|W90937|^ccyeur$'][,sum(Estimate)]
# 	reg1coef[rn %like% 'ccyeur:upcusipW90937|^ccyeur$'][,sum(Estimate)]

# 	reg1coef[rn %like% '^ccyeur:upcusip'][,`Pr(>|t|)`] %>% summary()

# 	upcrddiff<-reg1coef[rn %like% '^ccyeur:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccyeur',Estimate])]
# 	upcrddiff2<-rbind(data.table('upcusip'=dtregdata4[order(upcusip)][1,upcusip],est=reg1coef[rn=='ccyeur',Estimate]),upcrddiff)
# 	# check 
# 	if(nrow(dtregdata4[,.N,upcusip] %>% anti_join(upcrddiff2,by='upcusip'))) browser()

##
	dtregdata<-copy(dtm$dtl4)
	
	
	regresult_v2<-estimate.crddiff.firm(dtregdata,parallel.core=20,regversion=2)
	save(regresult_v2,file='firmlevelmo_alt.RData')
	regresult_v3<-estimate.crddiff.firm(dtregdata,parallel.core=30,regversion=3)	
	save(regresult_v2,regresult_v3,file='firmlevelmo_alt.RData')
	regresult_v4<-estimate.crddiff.firm(dtregdata,parallel.core=30,regversion=4)	
	save(regresult_v2,regresult_v3,regresult_v4,file='firmlevelmo_alt.RData')

# now let's do this for each date
	dtregdata<-copy(dtm$dtl4)
	regresult_old<-estimate.crddiff.firm(dtregdata,parallel.core=32)	
	#save(regresult,file='firmlevel.rdata')	
	save(regresult_old,file='firmlevel_augdata.rdata')	
	
	dtregdata<-copy(dtm$dtl4) %>% issfilter(3)
	regresult_old_filter3<-estimate.crddiff.firm(dtregdata,parallel.core=32)	
	save(regresult_old_filter3,file='firmlevel_augdata_filter3.rdata')	

	regresult[ccy=='eur',.(mean(est)),.(date,ytype)] %>% ggplot(aes(x=date,y=V1,colour=ytype))+geom_line()
## grid the calc for firm level credit spread at the daily level
	rm(list=ls(all=TRUE));load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')
	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	# dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	load('dailyregrun.RData')
	dtregdata<-copy(dtmd$dtl4)
	dtregdata[,year:=year(date)]
	regres<-list()
	for(i in 2004:2016){
		regres[[i-2003]]<-estimate.crddiff.firm(dtregdata[year==i],parallel.core=32)	
		save(regres,file='firmlevel_daily_temp.rdata')	
	}
	save(regres,file='firmlevel_daily.rdata')	
##
	rm(list=ls(all=TRUE));
load('db/dtlmo.RData');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
source('util.r')
 load('db/sdc_raw.RData')
dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =3)
dtregdata<-copy(dtm$dtl4) %>% issfilter(3)
regresult_new_filter3<-estimate.crddiff.firm(dtregdata,parallel.core=32)	
save(regresult_new_filter3,file='firmlevel_newdata_filter3.rdata')	


# load CIK cusip look up
	load('db/cik_cusip.RData')
	cik_cusip[,.(str_length(co_cusip))][,.N,V1]
	cik_cusip[,cusip6:=str_sub(co_cusip,1,6)]
	cik_cusip %>% setkey(cusip6)
	cik.<-cik_cusip[,.(cusip6,cik=co_cik)]
	cik.<-cik. %>% setkey(cusip6,cik) %>% unique()
	cik.<-cik.[cik!='' & cusip6 !='']
	cik. %>% setkey(cusip6)

# create variable that indicate 1 if issuance is placed by firm f in EUR in quarter t
	# dtcred<-copy(regresult)
	# # dtiss<-copy(bondref)
	# dtiss<-copy(dt.sdc.raw)
	# dtiss[,ccy:=str_to_lower(ccy)]
	# dtiss<-dtiss[ccy %in% c('eur','usd','gbp','aud','cad','jpy','chf')]
	# # dtiss<-dtiss[ccy %in% c('eur','usd')]
	# dtiss %>% setkey(upcusip)
	# dtcred %>% setkey(upcusip)
	# dtiss2<-dtiss %>% semi_join(dtcred,by='upcusip') %>% as.data.table() %>% issfilter(4) 
	# dtiss2[,ym:=floor_date(d,'month')]
	# dtcred[,ym:=floor_date(date,'month')]
	# dtiss3<-dtiss2[,.N,.(ym,upcusip,ccy)] %>% dcast(ym+upcusip~ccy,value.var='N')
	# dtiss3[is.na(aud),aud:=0][aud>0,aud:=1]
	# dtiss3[is.na(eur),eur:=0][eur>0,eur:=1]
	# dtiss3[is.na(usd),usd:=0][usd>0,usd:=1]
	# dtiss3[is.na(cad),cad:=0][cad>0,cad:=1]
	# dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1]
	# dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1]
	# dtiss3[is.na(chf),chf:=0][chf>0,chf:=1]
	# dtiss3[,issued:=max(eur,usd)]
	# dtiss3 %>% setkey(ym,upcusip)
	# dtcred %>% setkey(ym,upcusip)
	# dtregiss<-dtcred[dtiss3][ym>'2004-01-01']

	# reg2<-dtregiss[issued==1] %>% lm(eur~est+factor(ym)+factor(upcusip),data=.)
	# summary(reg2)$coefficients %>% head

	# reg2<-dtregiss[issued==1] %>% lm(eur~est+factor(ym),data=.)
	# summary(reg2)$coefficients %>% head

# quarterly regression of issuance choice on credit spread
	load('firmlevel.rdata')
	
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

	### this works well EUR
		# dtregiss<-dtcred.qt[dtiss3][yq>='2004-01-01'][issued==1][ccy=='eur']
		# dtregiss[,.N]
		# dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('yq','ccy'),winsor=.01)
		# dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('yq','ccy'),winsor=.01)	
		# dtregiss[,cip:=crddiff-netdiff]
		# # dtregiss<-winsorize2(dtregiss,lhs='cip',bylist=c('yq','ccy'),winsor=.01)
		# dtregiss[,.(mean(na.omit(cip))),ccy]
		# reg2<-dtregiss %>% lm(eur~crddiff+cip+factor(yq)+factor(upcusip),data=.)
		# summary(reg2)$coefficients %>% head
		# reg2prob<-dtregiss[issued==1] %>% glm(eur~est+factor(yq)+factor(upcusip), family=binomial(link="probit"), data=.)
		# summary(reg2prob)$coefficients %>% head

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
	########## price impact#####################
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


# try monthly price impact
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
		
		dtiss2[,ym:=floor_date(d,'month')]
		dtcred[,ym:=floor_date(date,'month')]
		
		dtcred.mt<-dtcred[order(date),.(est=first(est)),.(upcusip,ym,ytype,ccy)] %>% dcast(ym+upcusip+ccy~ytype,value.var='est')

		dtiss3<-dtiss2[,.N,.(ym,upcusip,cik,ccy)] %>% dcast(ym+upcusip+cik~ccy,value.var='N')
		dtiss3[is.na(aud),aud:=0][aud>0,aud:=1]; dtiss3[is.na(eur),eur:=0][eur>0,eur:=1]; dtiss3[is.na(usd),usd:=0][usd>0,usd:=1]; dtiss3[is.na(cad),cad:=0][cad>0,cad:=1]; dtiss3[is.na(jpy),jpy:=0][jpy>0,jpy:=1]; dtiss3[is.na(gbp),gbp:=0][gbp>0,gbp:=1]; dtiss3[is.na(chf),chf:=0][chf>0,chf:=1]; 
		dtiss3[,issued:=max(eur,usd)]

		dtiss3 %>% setkey(ym,upcusip)
		dtcred %>% setkey(ym,upcusip)

		
		## reshape into multi currency reg version
		dtregiss<-dtcred.mt[dtiss3][ym>='2004-01-01'][issued==1]#[!is.na(cik)]
		dtregiss[,.N]
		dtregiss<-winsorize2(dtregiss,lhs='crddiff',bylist=c('ym','ccy'),winsor=.01)
		dtregiss<-winsorize2(dtregiss,lhs='netdiff',bylist=c('ym','ccy'),winsor=.01)	
		dtregiss[,cip:=crddiff-netdiff]
		dtregiss[ccy=='eur',issuedinccy:=eur];		dtregiss[ccy=='aud',issuedinccy:=aud];		dtregiss[ccy=='cad',issuedinccy:=cad];		dtregiss[ccy=='jpy',issuedinccy:=jpy];		dtregiss[ccy=='gbp',issuedinccy:=gbp];		dtregiss[ccy=='chf',issuedinccy:=chf];		
		dtregiss[,.N]
		dtregiss[,.(mean(na.omit(cip))),ccy]
		reg3<-dtregiss %>% lm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip)+factor(ccy),data=.)
		summary(reg3)$coefficients %>% head

		reg3<-dtregiss %>% lm(issuedinccy~netdiff+factor(ym)+factor(upcusip)+factor(ccy),data=.)
		summary(reg3)$coefficients %>% head

		# reg3prob<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="probit"), data=.)
		# summary(reg3prob)$coefficients %>% head
		
		# reg3logit<-dtregiss %>% glm(issuedinccy~crddiff+cip+factor(ym)+factor(upcusip), family=binomial(link="logit"), data=.)
		# summary(reg3prob)$coefficients %>% head


	#################################### netdiff ~ ISSUE #### MONTHLY
		########## price impact#####################
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



### adding hedging interaction
	load('db/sec_hedge_result_10k_notxt.RData')
	dt.hedge.result.notxt
	load('db/cik_cusip.RData')
	cik_cusip[,.(str_length(co_cusip))][,.N,V1]
	cik_cusip[,cusip6:=str_sub(co_cusip,1,6)]
	cik_cusip %>% setkey(cusip6)
	dtiss3[,cusip6:=str_sub(upcusip,1,6)]
	dtiss3[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
	dtiss3 %>% setkey(cusip6)

	# dtiss3[,.N,cusip6][,.(cusip6)]

	# bondrefall[!is.na(cu)] %>% ds
	cik_cusip  %>% semi_join(dtiss3[,.N,cusip6][,.(cusip6)],by='cusip6') %>% as.data.table()
	# 
	dt.sdc.raw[,.(cu,cusip9,upcusip)]
	dt.sdc.raw[,cusip6:=str_sub(upcusip,1,6)]
	# dt.sdc.raw[,.(str_length(cusip6))][,.N,V1]
	# cik_cusip[,.(str_length(cusip6))][,.N,V1]
	dt.sdc.raw[str_length(cusip6)<6,cusip6:=sprintf("%06d", as.numeric(cusip6))]
	cik_cusip[,cusip6:=str_trim(str_to_lower(cusip6))]
	dt.sdc.raw[,cusip6:=str_trim(str_to_lower(cusip6))]

	cik_cusip  %>% semi_join(dt.sdc.raw[,.N,cusip6][,.(cusip6)],by='cusip6') %>% as.data.table()

	merge(dtiss3[,.N,cusip6][,.(cusip6)],cik_cusip,by.x='upcusip',by.y='cusip6',all.x=F,all.y=T,nomatch=0)
	[cik_cusip]
	dtiss3
	dtiss3[,.(str_length(upcusip))][,.N,V1]
######

	dtregdata4<-dtregdata3[date=i]
	reg1<-dtregdata4%>% lm(swapsprd~ccy+upcusip+ccy*upcusip,data=.)
	reg1coef<-summary(reg1)$coefficients %>% as.data.table(keep.rownames=T)
	upcrddiff<-reg1coef[rn %like% '^ccyeur:upcusip',.(upcusip=str_sub(rn,15),est=Estimate+reg1coef[rn=='ccyeur',Estimate])]
	upcrddiff2<-rbind(data.table('upcusip'=dtregdata4[order(upcusip)][1,upcusip],est=reg1coef[rn=='ccyeur',Estimate]),upcrddiff)
	# check 
	if(nrow(dtregdata4[,.N,upcusip] %>% anti_join(upcrddiff2,by='upcusip'))) browser()





	# dtregdata4[,.N,upcusip]

	reg1coef[rn %like% '^ccy']
	reg1coef[rn %like% '^ccyeur:']
	aa<-reg1coef[rn %like% '^upcusip',.(upcusip=str_sub(rn,8))]
	aa %>% setkey(upcusip)
	bb<-dtregdata4[,.N,upcusip]
	bb %>% setkey(upcusip)
	bb %>% anti_join(aa)

	bb
	dtregdata4[upcusip=='000375']

	reg2<-dtregdata3 %>% lm(swapsprd~factor(date)*ccy,data=.)
	dtreg2coef<-reg2$coefficients %>% as.data.table(keep.rownames=T)
	dtreg2coef %>% setnames(c('rn','.'),c('cn','est'))
	dtreg2coef[,date:=ymd(str_sub(cn,13,22))]
	dtreg2coef[cn %like% 'ccy???',ccy:=str_sub(cn,-3)]
	dtreg2ccyavg<-dtreg2coef[!is.na(ccy) & is.na(date)]
	dtreg2ccyavg
	dtreg2ccyavg[,csavg:=est+dtreg2coef[cn=='(Intercept)',est]]
	dtreg2ccyavg
	dtreg2ccyl<-dtreg2coef[!is.na(ccy) & !is.na(date)]
	dtreg2w<-dtreg2ccyl[,.(date,ccy,est)] %>% dcast(date~ccy)
	dtreg2w %>% ggplotw
	dtreg2w[,.(date,eur)] %>% ggplotw
	dtreg2w[,.(date,jpy)] %>% ggplotw
	# looks ok but not really good

# parallel
	lhs='swapsprd'
	# original

	dtregdata3<-dtregdata2 %>% filterglobaluponly()
		  regfun<-function(dt,ccylist='eur',regversion=1,bylist=''){
      tc.ret<-tryCatch({
          if (regversion==1){
            # regversion 1:: run regression directly on data set without taking out bonds that do not have matching pairs
            reg<-lm(eval(exparse(lhs))~ccy+upcusip,data=dt)
          } else if (regversion==3){
            # regversion 3: like regversion 2 but also adds maturity considerations in regression
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket,data=dt)
          } else if (regversion==4){
            # regversion 4: regversion 3+ 3 rating buckets as dummies
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket,data=dt)
          } else if (regversion==5){ # makes no sense!!!! deprecated
            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac,data=dt)
          } else if (regversion==6){
            # reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+sicfac+liq_bucket,data=dt)
          } else if (regversion==7){
           # regversion 7, like 6 but w/o sicfac
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+rating_bucket+liq_bucket,data=dt)
          } else if (regversion==8){
           # regversion 8, like 7 but only focus on liq
            reg<-lm(eval(exparse(lhs))~ccy+upcusip+ytm_bucket+liq_bucket,data=dt)
          }
      }, error=function(err){
        print(err)
        print(bylist)
      })
        if (exists('reg')){
          dtcoef2<-(coef(summary(reg)) %>% as.data.table(keep.rownames=T))[rn %like% '^ccy',.(ccy=str_sub(rn,4),est=Estimate,se=`Std. Error`)]
          dtccy<-data.table('ccy'=ccylist);dtccy %>% setkey(ccy)
          dtcoef2 %>% setkey(ccy)
          dtcoef2 <- dtcoef2[dtccy[ccy!='1usd']]
          dtcoef2
        } else {
          return(data.table('ccy'='eur','est'=as.numeric(NA),se=as.numeric(NA)))
        }
    }

	# tic()
	# regresult<-dtregdata3[ccy %in% c('eur','1usd'),regfun(.SD,'eur',3,.BY),by='date',.SDcols=c(lhs,'ccy','upcusip','ytm_bucket','rating_bucket','liq_bucket')]	
	# toc()

    # parallel version
    dtregdata3[,.N,ccy]
    
    DataP<-copy(dtregdata3[ccy %in% c('eur','1usd')])
    tic()
    setkey(DataP ,'date')
	indx<-split(seq(nrow(DataP)),DataP$date)
	registerDoParallel(16)
	  out_list <- foreach(i = indx, .packages = c('data.table') ) %dopar% {
	  	Psubset<-DataP[i,]
    	  dtsubsetout<-regfun(Psubset,'eur',3)
    	  dtsubsetout[,date:=Psubset[1,.(date)]]
    	  dtsubsetout
		}
	out <- rbindlist(out_list,use.names=TRUE) #, fill=TRUE, etc.
	toc()
	out[,.(date,est)] %>% ggplotw

	out %>% setkey(date)
	regresult %>% setkey(date)
	identical(out[,.(date,ccy,est,se)],regresult)

# parallel run
ys1m<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,returndt=T,parallel.core.=4)
ys1m2<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur')][as.character(rating_bucket) %in% c('1','2','3')],dtm$prl,regversion=9,returndt=T,parallel.core.=1)

ys1m2$regcoef[,.(date,ccyeur, ccyeur1=ccyeur+`ccyeur:rating_bucket1`,ccyeur2=ccyeur+`ccyeur:rating_bucket2`,ccyeur3=ccyeur+`ccyeur:rating_bucket3`)] %>% ggplotw()
ys1m2$regcoef[,.(date,ccyeur1=ccyeur+`ccyeur:rating_bucket1`,ccyeur2=ccyeur+`ccyeur:rating_bucket2`,ccyeur3=ccyeur+`ccyeur:rating_bucket3`)] %>% ggplotw()
ys1m2$regcoef[,.(date,rating_bucket1,rating_bucket2,rating_bucket3)] %>% ggplotw()

ys1m2$regcoef[,.(date,ccyeur,ccyeur2=ccyeur+`ccyeur:rating_bucket2`,ccyeur3=ccyeur+`ccyeur:rating_bucket3`)] %>% ggplotw()
ys1m2$regcoef[,.(date,ccyeur2=ccyeur+`ccyeur:rating_bucket2`,ccyeur3=ccyeur+`ccyeur:rating_bucket3`)] %>% ggplotw()
ys1m2$regcoef[,.(date,rating_bucket3)] %>% ggplotw()


ys1meff<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm$prl,regversion=4,adjccybs=1,returndt=T)

ys1m$regresult[,absdev:=abs(est)]
ys1meff$regresult[,absdevnet:=abs(est)]

absdev<-merge(ys1meff$regresult,ys1m$regresult,by=c('date','ccy'))
absdev[,netfrac:=absdevnet/absdev]

absdev[,mean(netfrac),date] %>% ggplotw()
absdev[,median(netfrac),date]
absdev[,summary(netfrac)]

absdev[,.(mean_absdev=mean(absdev),mean_absdevnet=mean(absdevnet),fracdev=mean(absdevnet)/mean(absdev)),date][,.(date,fracdev)] %>% ggplotw
absdev[,mean(absdevnet)/mean(absdev),ccy]

creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult) 

aa<-creditcip.result[[1]][,.(date,ccy,cip,credit)] %>% copy
bb<-aa[,.(date,ccy,absdevnet=abs(credit-cip),cipabs=abs(cip),creditabs=abs(credit))]
bb[date>ymd('2010-01-01'),.(mean(absdevnet),mean(creditabs),mean(cipabs),mean(absdevnet)/(.5*mean(creditabs)+.5*mean(cipabs)))]
# this is excellent
bb[creditabs>20 & cipabs>20,.(mean(absdevnet),mean(creditabs),mean(cipabs),mean(absdevnet)/(.5*mean(creditabs)+.5*mean(cipabs)))]
bb[creditabs>10 & cipabs>10,.(mean(absdevnet),mean(creditabs),mean(cipabs),mean(absdevnet)/(.5*mean(creditabs)+.5*mean(cipabs)))]

bb[,.(mean(absdevnet),mean(creditabs),mean(cipabs)),ccy]
bb[date>ymd('2012-07-01'),.(mean(absdevnet),mean(creditabs),mean(cipabs)),ccy]
bb[date>ymd('2012-07-01'),.(absdevnet/(0.5*creditabs+0.5*cipabs)),.(date)] 

bb[date>ymd('2012-07-01'),.(absdevnet/(0.5*creditabs+0.5*cipabs)),.(date,ccy)][ccy %in% c('eur')] %>% dcast(date~ccy) %>% ggplotw()
bb[date>ymd('2012-07-01'),.(absdevnet/(0.5*creditabs+0.5*cipabs)),.(date,ccy)][ccy %in% c('eur')] %>% summary


dtl.mo.new<-dtl.mo
bondref.new<-bondref
load('db/archive/dtlmo160804.RData');
load('db/archive/bondref160806.RData')
# load('db/pktickerlookup.RData')
dtlold.pk.ticker<-dtl.mo[,.N,pk][,.(pk)]
dtlold.pk.ticker %>% setkey(pk)

bondref %>% setkey(pk)

dtlold.pk.ticker<-bondref[dtlold.pk.ticker,nomatch=0][,.(pk,ticker)]
dtlold.pk.ticker %>% setkey(pk,ticker)
dtlold.pk.ticker[,oldpk:=pk]
dtlold.pk.ticker[!is.na(ticker),pk:=tolower(str_c(ticker, ' corp'))]

dtlold.pk.ticker %>% setkey(pk)
save(dtlold.pk.ticker,file='db/dtlmo160804_pk_ticker.RData')



pk.ticker.lookup %>% setkey(pk)
pk.ticker.lookup[dtlold.pk.ticker,nomatch=0]
dtl.mo.old<-dtl.mo
bondref.old<-bondref
dtm.old<-preprocess(bondref.old,dtl.mo.old,prl,issfiltertype =4)
ys1m.old<-resyldsprdv4(dtm.old$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm.old$prl,regversion=4,returndt=T)
ys1meff.old<-resyldsprdv4(dtm.old$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtm.old$prl,regversion=4,adjccybs=1,returndt=T)


dtreg2<-preprocess(bondref,dtl.mo.new,prl,issfiltertype =4)
ys2m<-resyldsprdv4(dtreg2$dtl4[ccy %in% c('usd','eur','gbp','aud','jpy','cad','chf')],dtreg2$prl,regversion=4,returndt=T)
creditcip.result<-plot.panel.creditcip(dtreg2$prw,ys2m$regresult) 
creditcip.result[[3]]


ys1m.old$regcoef[ys1m$regcoef] %>% ggplotw.comp
bondref.new %>% setkey(pk)
ys2m$dtreg %>% setkey(pk)
bb<-bondref.new[unique(ys1m$dtreg)]
bb[,.N,issue_type_desc]
bb[pub=='Govt.'][,.N,issue_type_desc]
bb[pub=='Govt.'][,.N,tf_mid_desc]
bb[pub=='Govt.'][,.N,tf_macro_desc]
bb[pub=='Govt.'][tf_mid_desc=='National Government'][,.N,.(upnames,i,name,cu,upcusip)]
bb[pub=='Govt.'][tf_mid_desc=='National Government'][,.N,descr]
bb[pub=='Govt.'][tf_mid_desc=='Regional Government'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='Supranational'][,.N,upnames]
bb[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,.(upnames,name,cu,upcusip)] %>% View
bb[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,name]
bb[pub=='Govt.'][,.N,sicp]
bb[upnames=='Japan'][,.N,name]
bb[issue_type_desc=='Agency, Supranational, Sovereign'][,.N,pub]
bb[issue_type_desc=='Agency, Supranational, Sovereign'][pub=='Priv.'][,.N,name]



bondref.old %>% setkey(pk)
ys1m.old$dtreg %>% setkey(pk)
cc<-bondref.old[unique(ys1m.old$dtreg)]

cc[,.N,issue_type_desc]
cc[pub=='Govt.'][,.N,issue_type_desc]
cc[pub=='Govt.'][,.N,tf_mid_desc]
cc[pub=='Govt.'][,.N,tf_macro_desc]
cc[pub=='Govt.'][tf_mid_desc=='National Government'][,.N,upnames]
cc[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,.(upnames,name,i,cusip9,upcusip,cu)]
cc[pub=='Govt.'][tf_mid_desc=='Regional Government'][,.N,upnames]
cc[pub=='Govt.'][tf_mid_desc=='Supranational'][,.N,.(upnames,)]
cc[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,upnames]
cc[pub=='Govt.'][tf_mid_desc=='National Agency'][,.N,name]
cc[pub=='Govt.'][,.N,sicp]
cc[upnames=='Japan'][,.N,name]
cc[issue_type_desc=='Agency, Supranational, Sovereign'][,.N,pub]
cc[issue_type_desc=='Agency, Supranational, Sovereign'][pub=='Priv.'][,.N,name]





ys1m$regcoef %>% write.dta('dta/creditmisp_160903.dta')
ys1meff$regcoef %>% write.dta('dta/netmisp_160903.dta')
dt.mispl<-create.misp.long2(dtm$prw,ys1m$regresult,ys1meff$regresult)
dt.mispl[,.(date,ccy=as.character(ccy),credit,netmisp,cip)] %>% write.dta('dta/misplong_160903.dta')

# ys1meff %>% write.dta('dta/creditmispri.dta') # ('effresys_160826_retro_0625.dta')
# Fig: 1 credit mispricings
	ys1m$regcoef[,.(date,eur,gbp,aud,jpy)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('Residualized credit spread relative to USD in bps')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('aud','gbp','eur','jpy'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_creditmisprice_160903.pdf',width=9,height=6)
	
# fig: 4 CIP 
	dtm$prw[monthenddates][date>=ymd('2004-01-01'),.(date,eubs5,bpbs5,jybs5,adbs5)] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))+scale_linetype_discrete(name='',labels=c("AUD", "GBP",'EUR','JPY'),breaks=c('adbs5','bpbs5','eubs5','jybs5'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/fig1_cip_160830.pdf',width=9,height=6)

	

# HG LG
	source('util.r')
	ys_hy<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating>6],dtm$prl,regversion=3,returndt=T)
	ys_hg<-resyldsprdv4(dtm$dtl4[ccy %in% c('usd','eur') & nrating<=6],dtm$prl,regversion=3,returndt=T)
	# fread('rating.csv')
	# setnames(ys_hy,'ccyeur','Low Grade')
	# setnames(ys_hg,'ccyeur','High Grade (Single A or better)')

	ys_hy$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='lowgrade']
	ys_hg$regresult[,cimin:=est-1.96*se][,cimax:=est+1.96*se][,rating:='highgrade']
	ys_byrating<-rbind(ys_hy$regresult,ys_hg$regresult)

	ys_byrating %>% ggplot(aes(x=date,y=est,colour=rating))+geom_line()+geom_errorbar(aes(ymin=cimin,ymax=cimax,colour=rating))+theme_few()

	setnames(ys_hy$regcoef,'eur','highyield')
	setnames(ys_hg$regcoef,'eur','highgrade')
	ys_hy$regcoef[ys_hg$regcoef] %>% tidyr::gather(.,'type','value',-date) %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=type,colour=type))+xlab('')+ylab('credit mispricing (bps)')+geom_hline(yintercept=0,colour='grey')+scale_x_date(breaks=scales::pretty_breaks(n=10)) + scale_y_continuous(breaks=scales::pretty_breaks(n=10))+theme_few()+scale_colour_discrete(name='',labels=c("High Grade",'Low Grade'))+scale_linetype_discrete(name='',labels=c("High Grade",'Low Grade'))#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
	# ggsave(file='../paper/figures/hghy_eur_160903.pdf',width=9,height=6)

# Graphing CIP and credit mispriing overlay
	creditcip.result<-plot.panel.creditcip(dtm$prw,ys1m$regresult) 

# plot figure 3 EUR credit cip
	dtcreditcip.plot[ccy=='eur'] %>% ggplot(data=.,aes(x=date,y=value))+geom_line(aes(linetype=variable,colour=variable)) +geom_errorbar(aes(ymin=cimin,ymax=cimax),colour='lightgrey',alpha=.5) +xlab('')+ylab('bps')+geom_hline(yintercept=0,colour='lightblue')+ scale_color_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+scale_linetype_discrete('',labels = c('CIP deviations 5 yr(implied-actual euro funding rate)','Credit Mispricing (EU-US)'))+theme_classic()+scale_x_date(breaks=scales::pretty_breaks(n=7))+theme(legend.position='bottom')+annotate('text',x=ymd('2015-06-01'),y=10,label=str_c('cor=',as.character(round(dt.corr.ccy[ccy=='eur',corr],2))))
	# ggsave(file='../paper/figures/EURcreditcip160903.pdf',width=9,height=6)


# export price data for regression in stata
	load('db/prl.RData'); load('db/monthenddates.RData')
	p2dta<-prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^\\w\\wsz\\d+' | ticker %like% '^ussw\\d+',.(date,ticker,value)]
	p2dta %>% setkey(date)
	p2dta<-p2dta[monthenddates] 
	p2dta[,value:=value*100]
	p2dta %>% dcast(.,date~ticker) %>% write.dta('workspace826/pricesv1.dta')


# Event study using daily data
	rm(list=ls(all=TRUE));load('db/dtldaily.RData');load('db/bondref.RData')
	load('db/prl.RData');load('db/monthenddates.RData');
	source('util.r')

	nmdates<-nonmarket.dates(dtl.daily,bondref)
	dtl.daily<-dtl.daily[!nmdates][date<'2016-07-26']
	dtmd<-preprocess(bondref,dtl.daily,prl,issfiltertype =4,monthlyonly = FALSE)
	
	# ys2<-resyldsprdv4(dtmd$dtl4[ccy %in% c('usd','eur')],dtmd$prl,regversion=6,returndt = T,adjccybs=T)
	# save.image('dailyts_eu_clean_831_isstype4.RData')
	
	ecbqe<-c(mdy('7/26/2012'),mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

	### all ccy daily
	# ys3<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=6,returndt = T)
	# save(ys3,file='dailyts_allccy_clean_831_isstype4.RData')
	
	# merge issuance data

	dtiss<-readstata13::read.dta13('regdata_02_160901_simple.dta') %>% as.data.table
	monthend2<-monthenddates[,dom:=mday(date)][,month:=month(date)][,year:=year(date)]
	dtiss %>% setkey(year,month)
	monthend2 %>% setkey(year,month)
	# dtiss<-dtiss[monthend2][,date:=i.date]
	dtiss<-dtiss[,date:=ymd(str_c(year,'-',month,'-',15))]


	load('db/dtlmo160804_pk_ticker.RData')
	regdata<-dtmd$dtl4[date>ymd('2007-01-01') & date<ymd('2009-01-01')][ccy %in% c('usd','eur')]
	regdata %>% setkey(pk)
	regdata<-regdata[dtlold.pk.ticker[,.(pk)]]


regdata %>% View
aa<-regdata[,.(pk=str_trim(pk))] %>% unique
bb<-dtlold.pk.ticker[,.(pk=str_trim(pk))] %>% unique
aa %>% setkey(pk)
bb %>% setkey(pk)
aa[bb,nomatch=0]

load('db/archive/dtldaily160806.RData')
load('db/archive/bondref160803.RData')
dtl.daily[,.N]
dtlold.pk.ticker<-dtl.daily[,.N,pk][,.(pk)]
dtlold.pk.ticker %>% setkey(pk)
bondref %>% setkey(pk)
dtlold.pk.ticker<-bondref[dtlold.pk.ticker,nomatch=0][,.(pk,ticker)]
dtlold.pk.ticker %>% setkey(pk,ticker)
dtlold.pk.ticker[,oldpk:=pk]
dtlold.pk.ticker[!is.na(ticker),pk:=str_to_lower(str_c(ticker, ' corp'))]
dtlold.pk.ticker %>% setkey(pk)
save(dtlold.pk.ticker,file='db/dtldaily160804_pk_ticker.RData')





	ys1<-resyldsprdv4(regdata,dtmd$prl,regversion=4,returndt = T,parallel.core.=1)
	dt.mispl<-create.misp.long(dtmd$prw,ys1$regresult)
	# US credit crisis
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-11-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2007-07-01','2008-11-30'),event.dates.in=c(ymd('2008-03-16'),ymd('2008-09-15')),type.in=1,filepathhead='',datetics=7)
		
	# EUR soverign crisis: works pretty will with netmispriing and issuance now!!!
	plot.event.study.moiss2(dtin.in=dt.mispl,dtiss.in=dtiss,ccy.='eur',date.range=c('2011-03-01','2012-10-01'),event.dates.in=c(ymd('2011-05-01'),ymd('2012-06-17')),type.in=1,filepathhead='')
	


