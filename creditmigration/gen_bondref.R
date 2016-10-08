## This file is dedicated to maintaining rdata database. all modifications are recordered here

rm(list=ls(all=TRUE))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
#setwd("C:/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')

# load sdc raw and add figi data via isin and via cusip
load('db/sdc_raw.RData')

bondref<-dt.sdc.raw[!is.na(isin) | !is.na(cusip9)] # no way to match if SDC doesn't have either isin or cusip



bondref[!is.na(isin)] %>% showdups('isin')
bondref[!is.na(cusip9)] %>% showdups('cusip9')
# merge figi via isin
load('db/dt_isin_figi.RData')
bondref<-update.dt(bondref,dt.isin.figi,keyfield='isin',insertnewrow=F)
bondref[!is.na(isin)] %>% showdups('isin')
bondref[!is.na(figi)] %>% showdups('figi')

# merge figi via cusip
load('db/cusip_figi.RData')
bondref<-update.dt(bondref,cusip.figi,keyfield='cusip9',insertnewrow=F,diagnostic_rt=F) # has some overlap disagreement with isin based figi, but close enough...

# add pk data via isin, and via figi
load('db/pk_lookup.RData')
bondref<-update.dt(bondref,pk.isin,keyfield='isin',insertnewrow=F)
bondref<-update.dt(bondref,pk.figi,keyfield='figi',insertnewrow=F)



maintain.bondref<-function(bondref){
# run everytime there is new additions

  bondref<-copy(bondref)
  bondref[,sic1:=as.numeric(str_sub(as.character(sicp),1,1))]
  bondref[,upsic1:=as.numeric(str_sub(as.character(upsicp),1,1))]
  bondref[is.na(sic1),sic1:=upsic1]
  bondref[,sicfac:=factor(sic1)]
  # fill rating & insert rating for new bondref data
  bondref[is.na(rating),rating:=sp]
  bondref[is.na(rating),rating:=mdy]
  ratinglu<-fread('rating.csv')
  bondref<-update.dt(bondref,ratinglu,keyfield = 'rating',override = T,insertnewrow=F)
  bondref[,nrating_sp:=NULL];bondref[,nrating_mdy:=NULL]
  setkey(bondref,isin,pk)
  bondref[,`:=`(matbbg=mdy(str_extract(ticker,"\\d\\d\\/\\d\\d\\/\\d\\d")),couponbbg=as.numeric(str_extract(ticker,"(?<=\\s)\\d+\\.*(\\d+)?(?=\\s)")),couponsdc=as.numeric(str_extract(descr,"^\\d+.\\d*(?=\\%)")))]
  bondref[,matbbg_diff:=as.numeric((matbbg-mat2)/365)]
  bondref[,couponbbg_diff:=(couponbbg-couponsdc)]
  bondref[,namebbg_diff:=adist(name,i,ignore.case=T)/(0.5*(str_length(name)+str_length(i))),by=1:nrow(bondref)]
  bondref
}
# bondref data maintainance -----------------------------------------------
bondref<-bondref %>% maintain.bondref(.)
	# deprecated function
	# dedupe.by.pk<-function(dtbondref){
	# 	# input a bondref dt, output a bondref dt unique in pk
	# 	dedupe.by.pk.helper<-function(dtin,bypk){
	# 		# print(bypk)
	# 		# print(dtin %>% show.dupe.col)
	# 		# print(dtin[,.(matbbg_diff,couponbbg_diff,namebbg_diff,i,name)])
	# 		if(nrow(dtin)==1) return(dtin)
	# 		dtranked<-dtin[!abs(matbbg_diff)>.1 & !abs(couponbbg_diff)>.1 & !namebbg_diff>.5]
			
	# 		if (nrow(dtranked)==0) { # if we filtered too much, then back out one step
	# 			dtranked<-dtin[order(abs(matbbg_diff), abs(couponbbg_diff),-deal_no)]
	# 		} else { # allow the filter
	# 			dtranked<-dtranked[order(abs(matbbg_diff), abs(couponbbg_diff),-deal_no)]
	# 		}
	# 		dtout<-dtranked[1]
	# 		# print('******************')
	# 		# print(dtranked %>% show.dupe.col)
	# 		# browser()
	# 		dtout	
	# 	}

	# 	dtbondref<-copy(dtbondref)
	# 	deduped<-dtbondref[,dedupe.by.pk.helper(.SD,.BY),pk]
	# 	# setkey(deduped,deal_no); setkey(dtbondref,deal_no)
	# 	# rid<-dtbondref[!deduped]
	# 	deduped
	# }



dedupe.by.pk2<-function(dtbondref){
	# input a bondref dt, output a bondref dt unique in pk
	# order by deal_no ascending
	dedupe.by.pk.helper<-function(dtin,bypk){
		if(nrow(dtin)==1) return(dtin)
		dtranked<-dtin[order(deal_no)]
		dtout<-dtranked[1]
		dtout	
	}
	dtbondref<-copy(dtbondref)
	deduped<-dtbondref[,dedupe.by.pk.helper(.SD,.BY),pk]
	deduped
}

# decided against ranking strictly by date
	# dedupe.by.pk3<-function(dtbondref){
	# 	# input a bondref dt, output a bondref dt unique in pk
	# 	# order by date ascending
	# 	dedupe.by.pk.helper<-function(dtin,bypk){
	# 		if(nrow(dtin)==1) return(dtin)
	# 		dtout<-dtin[order(d)][1]
	# 		dtout	
	# 	}

	# 	dtbondref<-copy(dtbondref)
	# 	deduped<-dtbondref[,dedupe.by.pk.helper(.SD,.BY),pk]
	# 	# setkey(deduped,deal_no); setkey(dtbondref,deal_no)
	# 	# rid<-dtbondref[!deduped]
	# 	deduped
	# }


# get.rid of bbg mismatches based on figi data:
	rid<-unique(bondref[abs(matbbg_diff)>.0 | abs(couponbbg_diff)>.0,.(deal_no)])
	setkey(bondref,deal_no); setkey(rid,deal_no)
	bondref<-bondref[!rid]

# Checks
## we would only care about deduping these:
  	bondref[,pk:=tolower(pk)]

	load('db/dtlmo.rdata')
	satisfy.filter<-bondref[!is.na(pk)] %>% issfilter(.)  %>%  showdups('pk')
	dtl.pk<-dtl.mo[,.N,pk][,.(pk)]
	dedupe.these<-satisfy.filter[dtl.pk,nomatch=0]
	dedupe.these[,.N] # 5k
	setkey(dtl.pk,pk); setkey(bondref,pk)
	dtl.pk[!bondref][,.N] # missing these in bond ref: 8k


# using new bbg information to deduplicate
	# dedupe.these[,.N,pk][order(-N)] %>% write.csv('getbbg_amt_issdate.csv')
	bdp.ref.add<-fread('dupe_bdp_ref.csv')
	setnames(bdp.ref.add,c('ISSUE_DT','AMT_ISSUED','ORIGINAL_AMOUNT_SOLD','FIRST_SETTLE_DT','ID_ISIN','ID_CUSIP'),c('d_bbg','amt_bbg','amt2_bbg','settlement2_bbg','isin_bbg','cusip9_bbg'))
	bdp.ref.add[,d_bbg:=mdy(d_bbg)][,settlement2_bbg:=mdy(settlement2_bbg)]
	bdp.ref.add[,amt_bbg:=amt_bbg/10^6][,amt2_bbg:=amt2_bbg/10^6]


	bb<-update.dt(dedupe.these,bdp.ref.add,keyfield='pk')
	bb[,`:=`(amt_bbg_diff=amt_bbg-amt,amt2_bbg_diff=amt2_bbg-amt,d_bbg_diff=(as.numeric(d_bbg)-as.numeric(d))/365,settlement2_bbg_diff=(as.numeric(settlement2_bbg)-as.numeric(settlement2))/365)] # positive means bbg is later

	cc<-bb[,.(deal_no,pk,settlement2_bbg_diff,d_bbg_diff,amt_bbg_diff,amt2_bbg_diff,amt,amt_bbg,amt2_bbg,main_tranche,num_packageid,cusip9,cusip9_bbg)]
	rid1<-cc[!is.na(cusip9)][tolower(str_sub(cusip9,0,8))!=tolower(str_sub(cusip9_bbg,0,8))][settlement2_bbg_diff!=0][,.(deal_no)]
	dedupe.by.pk.special<-function(dtbondref){
		# Tis is used just ot get rid of settlement2_bbg_diff!=0
		dedupe.by.pk.helper<-function(dtin,bypk){
			if(nrow(dtin)==1) return(dtin)
			dtranked<-dtin[abs(settlement2_bbg_diff)==0]
			
			if (nrow(dtranked)==0) { # if we filtered too much, then back out one step
				dtranked<-dtin[order(deal_no)]
			} else { # allow the filter
				dtranked<-dtranked[order(deal_no)]
			}
			dtout<-dtranked
			dtout	
		}
		dtbondref<-copy(dtbondref)
		deduped<-dtbondref[,dedupe.by.pk.helper(.SD,.BY),pk]
		deduped
	}
	dd<-cc %>% dedupe.by.pk.special(.)
	setkey(cc,deal_no);setkey(dd,deal_no)
	rid2<-cc[!dd]

	setkey(bondref,deal_no);setkey(rid1,deal_no);setkey(rid2,deal_no)
	bondref<-bondref[!rid1][!rid2]
	bondrefall<-bondref
	# save(bondrefall,file='db/bondrefall.RData')
	# Lastly; just apply dedupe by deal number order on the rest of duplicates
	bondref<-bondref[!is.na(pk)] %>% dedupe.by.pk2()

	readme<-'bondref generated from sdc data, unique in pk, deal_no; deduped by various conflict resolvation with bbg and finally deduping by ascending deal_no'

	# save(bondref,readme,file='db/bondref.RData')

	
	
	
# 9/7/2016 improved using bondrefall; replace all pk by ticker 
	rm(list=ls(all=TRUE));
	source('util.r')
	load('db/bondrefall.RData')
	bondref<-copy(bondrefall)
		pk.ticker.lookup<-bondrefall[!is.na(ticker) & !is.na(pk),.(pk,ticker=str_c(tolower(ticker), ' corp'))]
		pk.ticker.lookup  %>% setkey(ticker)
		pk.ticker.lookup<-unique(pk.ticker.lookup)
		readme='unique mapping of pk to ticker from figi dataset, generated from gen_bondref.R'
		save(pk.ticker.lookup,readme,file='db/pktickerlookup.RData')
	bondref[!is.na(ticker),pk:=str_c(tolower(ticker), ' corp')]
	bondref<-bondref[!is.na(pk)]
	# bondref %>% issfilter(4) %>% showdups('pk') #%>% dt2clip()
	bondref<-bondref %>% dedupe.by.pk2()
	readme<-'bondref generated from sdc data, unique in pk which is actually ticker from figi dataset, deal_no; deduped by various conflict resolvation with bbg and finally deduping by ascending deal_no'
	save(bondref,readme,file='db/bondref.RData')


#9/27/2016 # load newest bonds (not yet complete)
	rm(list=ls(all=TRUE));
	source('util.r')
	load('db/archive/bondrefall160806.RData')
	
	bondrefall
	
	
	
# because dedup.these has 0 rows, there fore we don't really care, and so we can quickly dedupe; but we don't even have to dedupe
	# A quick way of setting unique
	# setorder(bondref,-main_tranche,-deal_no)
	# setkey(bondref,pk)
	# bondref<-bondref %>% unique()


# check if we missed anything major; not really
	# isin2req<-bondref[!is.na(isin) | !is.na(cusip9)][is.na(figi)][!is.na(isin),.(isin)]
	# cusip2req<-bondref[!is.na(isin) | !is.na(cusip9)][is.na(figi)][!is.na(cusip9),.(cusip9)]
	# isin.figi.add<-isin2req %>% requestfigibyID2(.,'isin')
	# cusip.figi.add<-cusip2req %>% requestfigibyID2(.,'cusip9')
	# cusip.figi.add[marketSector!='Equity'] %>% showdups('cusip9')


# check that bondref data corresponds to pk.bdp data; currently only ccy matters
	# load('db/pk_bdp.RData')
	# pk.bdp<-pk.bdp[,.(pk,ccy=toupper(ccy))]
	# bb<-update.dt(bondref,pk.bdp,keyfield='pk',diagnostic_rt=T)
	# bb$dtconflicts[dtl.pk,nomatch=0]



###### assess what pks missing from the SDC data.base
	pkmissing<-issfilter(bondrefall[is.na(pk)],2)[,.(totamt=na.omit(sum(amt)),N=.N),.(isin,cusip9)][order(-totamt)]
	pkmissing2<-pkmissing %>% head(100)
	pkmissing2
	pkmissing2 %>% setkey(isin)
	bondrefall %>% setkey(isin)
	bondrefall[pkmissing2,nomatch=0]
bondrefall['IT0004969207']
bb<-issfilter(bondref[!is.na(pk)],2)[,.(totamt=na.omit(sum(amt)),N=.N),.(isin,cusip9)][order(-totamt)]
bb[,sum(totamt)]

bondref %>% ds()
cc<-issfilter(bondrefall,3)[marketSector=='Corp' | is.na(marketSector)]
cc[,.N,ccy][order(-N)] %>% head(20)