readme.txt


'db/pk_lookup.RData'
contains unique mapping of pk to isin, (and pk to figi); 
most importantly, pk to isin must be maintained since pk is the unqiue identifier of bond price data and isin is the unique identifier of bond ref data, pk to figi merely helps to figure out pk to isin when needed
to update, make sure pk, isin, and fifi are all unique
	#dtin<-refadd2
	#tryCatch({setnames(dtin,'parsekeyable','pk')},error=function(err){})
	#dtin<-dtin[,.(pk,isin)]
	#dtin %>% showdups('pk')
	#dtin %>% showdups('isin')
	#load('db/pk_lookup.RData')
	#pk.isin<-update.dt(pk.isin,dtin,keyfield='pk')
	#resave(pk.isin,file='db/pk_lookup.RData')
	########
	# #update pk.figi
	# dtin<-padd0
	# tryCatch({setnames(dtin,'parsekeyable','pk')},error=function(err){})
	# setkey(dtin,pk,figi)
	# dtin<-unique(dtin[,.(pk,figi)])
	# dtin %>% showdups('pk')
	# dtin %>% showdups('figi')
	# load('db/pk_lookup.RData')
	# pk.figi<-update.dt(pk.figi,dtin,keyfield='pk')
	# resave(pk.figi,file='db/pk_lookup.RData')
	# #and update pk.isin via dt.isin.figi
	# load('db/dt_isin_figi.RData')
	# setkey(pk.isin,pk); setkey(pk.figi,pk)
	# pk2isinlookup <- pk.figi[!pk.isin]
	# pk2isinlookup %>% showdups('pk')
	# pk2isinlookup %>% showdups('figi')
	# setkey(dt.isin.figi,'figi')
	# dt.isin.figi %>% showdups('figi')
	# pk.isin.add<-pk2isinlookup[dt.isin.figi,nomatch=0][,.(pk,isin)]
	# pk.isin.add ## if this is non-zero, then update ...
	###########
	# # check isin.no.figi can't be mapped via pk_lookup; currently there are some, but it's ok since I don't completely trust the isin to pk to figi mapping
	# load('db/dt_isin_figi.RData')
	# load('db/pk_lookup.RData')
	# setkey(isin.no.figi,'isin')
	# setkey(pk.isin,'isin')
	# pklink<-isin.no.figi[pk.isin,nomatch=0] 
	# setkey(pklink,pk)
	# setkey(pk.figi,pk)
	# isin.figi.add<-pklink[pk.figi,nomatch=0][,.(isin,figi)]
	# bb<-isin.figi.add %>% requestfigibyID2(.,idtypein='figi',diagnostics=T,uselocalfirst=F)
	# bb 


'db/dt_isin_figi.RData'
contains unique isin to figi mapping and isins that does not have corresponding figi;
this mapping of isin to figi must be maintained since isin is the unique identifier of bondref data and figi can be linked to pk via pk_lookup
to update, make sure isin and figi are both unique, apply requestfigibyID2
	# update code
	# load('db/dt_isin_figi.RData')
	# aa<-newisin %>% requestfigibyID2(.,diagnostics=T)
	# dt.isin.figi<-update.dt(dt.isin.figi,aa$dtout,keyfield='isin')
	# isin.no.figi<-rbind(isin.no.figi,aa$missing)
	# resave(dt.isin.figi,isin.no.figi,file='db/dt_isin_figi.RData')


'db/pk_bdp.RData'
keeps all bloomberg bdp field requests based on pks; to update, make sure pk is unique; this is for use someday later when I want to expand sdc data with bloomberg data
	#update code
	# dtin<-audbond
	# tryCatch({setnames(dtin,'parsekeyable','pk')},error=function(err){})
	# dtin %>% showdups('pk')
	# pk.bdp<-update.dt(pk.bdp,dtin,keyfield='pk')
	# resave(pk.bdp,file='db/pk_bdp.RData')



'cusip_figi.RData'
a lookup (and figi info table) for unique cusip9 to figi mapping;
these cusips corresponds to SDC data with missing isin but has cusip;
this is used as an alternative to isin for mapping SDC to bloomberg bond prices
	# generated with the following  
	# cusip9.to.get<-dtsdc3[is.na(isin) & !is.na(cusip9),.(cusip9)]
	# cusip9.to.get<-cusip9.to.get[str_length(cusip9) %between% c(8,9)]
	# setkey(cusip9.to.get,cusip9)
	# cusip9.to.get<-unique(cusip9.to.get[cusip9!='NEWISSUE'])

	# cusip.figi<-cusip9.to.get[,.(cusip9)] %>% requestfigibyID2(.,idtypein='cusip9',diagnostics=T)
	# cusip.figi<-cusip.figi$dtout
	# rid<-(cusip.figi %>% showdups(c('cusip9')))[marketSector!='Corp',.(figi)]
	# setkey(cusip.figi,figi)
	# setkey(rid,figi)
	# cusip.figi<-cusip.figi[!rid]



'sdc_raw.RData'
this is the raw sdc filing with minimal changes without merging with figi data, without filling in isin or cusip
it has many records without either isin or cusip, thus unidentifiable; 
this is unique in deal_no;
this is used to construct bondref
	## code used to generate this file: essentially combines sdc96_clean2.dta and sdcnew.rdata by deal_no
	# dt_sdc0<-readstata13::read.dta13('sdc96_clean2.dta') %>% as.data.table()
	# dtsdc1<-dt_sdc0[,.(i,tic,isin,cu,upcusip,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,deal_no,master_deal_type,issue_type_desc,mdealtype,secur,tf_mid_desc,sic1,sic2,upsicp,sicp,upnat,sp,mdy,settlement2,modnat,modupnat,rank_domicile_nation,upnames,tf_macro_desc,mkt,exch,pub,cusip9,num_packageid,main_tranche)][order(isin,-amt)] %>% cleanup.sdc.by.col(.)
	# load(file='sdcnew.rdata')
	# dtsdc2<-df_sdcnew %>% rename(i=issname,rank_domicile_nation=domnat,tic=ticker_sdc,cu=cusip,mkt=mktplace,mdy=rating_mdy,sp=rating_sp,upnames=upco,upsicp=upsic,sicp=sic_main,deal_no=sdcdealnumber) %>% as.data.table()  %>% cleanup.sdc.by.col(.)
	# dtsdc2[,id_package_sdc:=NULL];dtsdc2[,ccy:=toupper(ccy)]
	# dtsdc1 %>% showdups('deal_no') # not many
	# dtsdc1<-unique(dtsdc1)
	# dtsdc2 %>% showdups('deal_no')
	# dt.sdc.raw<-update.dt(dtsdc1,dtsdc2,keyfield='deal_no',diagnostic_rt=F,override=T)


'dtlmo.rdata'
has monthly bond price data from bloomberg, constructed with gen_dtlmo.R

'db/dtldaily.RData'
has daily bond price data from bloomberg, constructed with gen_dtldaily.R in the same way as dtl.mo

'db/bondref160803.RData'
bondref generated from sdc data, unique in pk, deal_no; 
deduped by various conflict resolvation with bbg and finally deduping by ascending deal_no
# generated using 'gen_bondref.R'


'bondrefall.RData'  
this has the undedubed version of bondref even when tehre are no pks



'prl.RData'

'montheenddates.Rdata'