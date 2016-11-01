
# load('db/sdc.RData')
#dtissraw<-bondrefall %>% issfilter(type=6) 
dtissraw<-sdc %>% filter.sdc(type='6ccyv3') %>% copy()
dtissraw<-dtissraw[ccy %in% c('usd','eur','cad','aud','chf','gbp','jpy')][,monthly:=floor_date(d,'month')]
dtissraw<-(dtissraw %>% tocusip6(field='cu'))[,upcusip:=cusip6] %>% add.earlist.iss.in.ccy()

# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
dtissraw[,.N,pub]
dtiss.in<-copy(dtissraw)[amt>=100][ytofm>=1][ytofm<99][nrating<=16][nrating!=0][nrating!=1][pub %in% c('Public','Govt.','Sub.')][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][tf_mid_desc!="Government Sponsored Enterprises"][secur %ni% stringr::str_to_lower(c("Cum Red Pfd Shs", "Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk"))][str_to_lower(tf_mid_desc) %nlk% "govern"][secur %nlk% 'mtg|pdf|sh|mortg|eqt|pass|islamic|step|pfanbriefe|cont|perp|loan|extendible|pik']


dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
} %>% rbindlist() %>% setkey(date,ccy)
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]; dtcreditcip.q %>% setkey(date,ccy);
dtreg.q<-dtcreditcip.q[dtiss.collapse.q,nomatch=0]

## make issuance lead by one period
dtreg.q[,F.I_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]
dtreg.q[,D.credit:=credit-shift(credit,n=1,type='lag'),ccy]
dtreg.q[,D.cip:=cip-shift(cip,n=1,type='lag'),ccy]

# ..................................................................
dtreg.q[ccy=='eur'][,.(date,i_netflow)] %>% ggplotw()
#dtreg.q[ccy=='eur'][,.(date,mu)] %>% ggplotw()
dtreg.q[ccy=='eur'][,.(date,i_netflow,netmisp)] %>% ggplotw()



#Or get rid of pub=sub. for better match in the latest data












#__________________________
# try out new things ------------------------------------------------------
if (FALSE) {
 #' alternate ccy pairs
 bb<-(dtl[ccy %in% c('eur','gbp')] %>% resyldsprdv4(.,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore))$regresult
 
 bb[ccy=='gbp'][,.(date,est)] %>% ggplotw()
 #' Additional FE
 
 aa<-bondref[str_to_lower(ccy) %in% c('usd','eur','gbp','jpy','aud','chf','cad')]
 aa[,secur:=str_to_lower(secur)]
 #' floating notes
 aa[,f.floating:=ifelse(str_to_lower(secur) %like% 'fl|fr' | str_to_lower(descr) %like% 'flt|fl ',TRUE,FALSE)]
 aa[,.N,f.floating]
 #' 144a
 sdc[,.N,.(rule144a)]
 
 # senior vs sub
 aa[,.N,secur %like% 'senior|sr']
 aa[secur %like% 'senior|sr'][,.N,.(secur)][order(-N)]
 aa[,.N,secur %like% 'sub']
 aa[secur %like% 'sub'][,.N,.(secur)][order(-N)]
 #
 # collateral, secured vs unsecured
 
 #' private placement
 aa[,.N,str_to_lower(securityType) %like% 'priv']
 #' amt
 
 
 
 # step up
 aa[,.N,secur %like% 'step'| str_to_lower(descr) %like% 'step']
 
 #
 
 #
 aa %>% ds()
 aa[,.N,name] %>% head(20)
 
 aa %>% ds
 aa[,.N,secur %like% 'pri']
 
 aa[,.N,str_to_lower(typesec)==str_to_lower(secur)]
 
 aa [,.N,.(tf_mid_desc)][order(-N)] %>% head(50) %T>% dt2clip()
 aa [,.(.N,sum(na.omit(amt))),.(tf_mid_desc)][order(-V2)] %>% head(50) %T>% dt2clip()
 
 
 aa[str_to_lower(tf_mid_desc) %like% 'govern'][,.N,.(tf_mid_desc)][order(-N)]
 aa[,.N,secur %like% 'step'| str_to_lower(descr) %like% 'step']
 aa[,.N,secur %like% 'step']
 
 
 aa %>% ds
 aa[,.N,securityType]
 aa %>% ds
 aa[,.N,.(rule144a)][order(-N)] %T>% dt2clip()
 sdc[,.N,.(rule144a)]
 sdc[str_to_lower(ccy) %in% c('usd','eur','gbp','jpy','aud','chf','cad')][!is.na(isin) | !is.na(cu)][!is.na(amt)][,.N]
}

###################### test out graph iss


#dtissraw<-bondrefall %>% issfilter(type=6) 


dtissraw<-sdc %>% filter.sdc(type='6ccyv1')
dtissraw<-dtissraw[ccy %in% c('usd','eur','gbp','jpy','aud','chf','cad')][,monthly:=floor_date(d,'month')]
dtissraw<-(dtissraw %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6] %>% add.earlist.iss.in.ccy()

# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
#  dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
dtiss.in<-copy(dtissraw)[amt>=50][ytofm>=2][ytofm<99][nrating<=16][nrating!=0][nrating!=1][pub %in% c('Public','Sub.')][mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P")][tf_mid_desc!="Government Sponsored Enterprises"][secur %ni% stringr::str_to_lower(c("Cum Red Pfd Shs", "Non-Cum Pref Sh" ,"Preferred Shs" ,"Pfd Stk,Com Stk"))]
#+ Quarterly issunace calc2,include=F
#Issuance quarterly collapsing each ccy pair
dtiss.collapse.q <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
 dtiss.in %>% icollapse4(.,iccy,collapse.freq = 'quarter',filter=1)
} %>% rbindlist() %>% setkey(date,ccy)

dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]; dtcreditcip.q %>% setkey(date,ccy);
dtreg.q<-dtcreditcip.q[dtiss.collapse.q,nomatch=0]

## make issuance lead by one period
dtreg.q[,F.I_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.i_netflow:=shift(i_netflow,n=1,type='lead'),ccy]
dtreg.q[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg.q[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg.q[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),ccy]
dtreg.q[,D.credit:=credit-shift(credit,n=1,type='lag'),ccy]
dtreg.q[,D.cip:=cip-shift(cip,n=1,type='lag'),ccy]

# ..................................................................
dtreg.q[ccy=='eur'][,.(date,i_netflow)] %>% ggplotw()
dtreg.q[ccy=='eur'][,.(date,netmisp)] %>% ggplotw()
dtreg.q[ccy=='eur'][date<'2016-01-01'][,.(date,netmisp,i_netflow)] %>% ggplotw()
dtreg.q[ccy=='eur'][date<'2016-01-01'] %>% felm(i_netflow~netmisp,.) %>% stargazer(type='text',report='vct*')
dtreg.q[ccy=='eur'][date<'2016-01-01'] %>% felm(F.i_netflow~netmisp,.) %>% stargazer(type='text',report='vct*')

