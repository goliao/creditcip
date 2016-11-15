
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





##########################################################

# test --------------------------------------------------------------------
#' ## redefining mu 
#+ eval=FALSE
## issuance
dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
lsiss<-list()
for(iccy in c('eur','usd','jpy','aud','cad','chf','gbp')){
 lsiss[[length(lsiss)+1]] <- dtiss.in %>% icollapse.againstall(iccy,collapse.freq = 'month')
};
dtiss<-rbindlist(lsiss) %>% setkey(date,ccy)
iss.weight<-dtiss[,sum(I_total),ccy][order(ccy)]

### credit cip
ysls <- list(); yseffls <- list(); cc.exact.ls <- list()
for (iccy in c('eur','usd','jpy','aud','cad','chf','gbp')){
 ysiccy<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,mainccyin = iccy)
 ysiccyeff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,mainccyin = iccy)
 cc.exact.ls[[length(cc.exact.ls)+1]] <- (ysiccy$regresult[ysiccyeff$regresult,on=c('date','ccy')] %>% setnames(c('est','i.est'),c('credit','crediteff')))[,cip:=credit-crediteff][,mainccy:=iccy]
}
dt.cc.exact<-rbindlist(cc.exact.ls) %>% setkey(mainccy,ccy,date)
# save(dt.cc.exact,file='temp_dtccexact.RData')
# geometric avg
# cc.avg<-dt.cc.exact[,.(credit.avg=mean(credit),cip.avg=mean(cip),netdev.avg=mean(crediteff)),.(date,mainccy)][,date:=floor_date(date,'month')] %>% setnames('mainccy','ccy') %>% setkey(date,ccy)
# weighted avg
cc.avg<-dt.cc.exact[order(date,mainccy,ccy)][,.(netdev.avg=weighted.mean(crediteff, iss.weight[ccy!=.BY$mainccy,V1])),.(date,mainccy)][,date:=floor_date(date,'month')] %>% setnames('mainccy','ccy') %>% setkey(date,ccy)
dtreg<-cc.avg[dtiss]

dtreg[,F.mu:=shift(mu,n=1,type='lead'),ccy]
dtreg[,mu_3mf:=(shift(mu,n=1,type='lead')+shift(mu,n=2,type='lead')+shift(mu,n=3,type='lead'))/3,ccy]
dtreg[,mu_6mf:=(shift(mu,n=1,type='lead')+shift(mu,n=2,type='lead')+shift(mu,n=3,type='lead')+shift(mu,n=4,type='lead')+shift(mu,n=5,type='lead')+shift(mu,n=6,type='lead'))/6,ccy]
dtreg[,D.mu:=mu-shift(mu,n=1,type='lag'),ccy]
dtreg[,FD.mu:=shift(D.mu,n=1,type='lead'),ccy]
dtreg[,D.netdev.avg:=netdev.avg-shift(netdev.avg,n=1,type='lag'),ccy]

res1 <- list()
res1[[length(res1)+1]] <- dtreg %>% felm(D.mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(D.mu~D.netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(F.mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(mu~netdev.avg|ccy,.) 
res1[[length(res1)+1]] <- dtreg %>% felm(FD.mu~netdev.avg|ccy,.) 
res1 %>% stargazer(type='text',report='vct*')

dtreg %>% reg.newey.all2(mu~netdev.avg)
dtreg %>% reg.newey.all2(F.mu~netdev.avg)
dtreg %>% reg.newey.all2(F.mu~netdev.avg)
dtreg %>% reg.newey.all2(mu_3mf~netdev.avg)
dtreg %>% reg.newey.all2(mu_6mf~netdev.avg)


dtreg[ccy=='usd',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='eur',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='jpy',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='aud',.(date,mu_3mf,netdev.avg)] %>% ggplotw
dtreg[ccy=='cad',.(date,mu_3mf,netdev.avg)] %>% ggplotw
#dtreg[ccy=='usd',.(date,mu_3mf,netdev.avg)] %>% ggplotw
# dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% ggplotw
# dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% lm(mu~netdev.avg,.) %>% summary
# zz<-dtreg[ccy=='usd',.(date,mu,netdev.avg)] %>% neweymod(mu~netdev.avg);zz;zz



#' ## Impact of large issuance on basis


icollapse4.daily<-function(dtin.,ccyA=dtin.[str_to_lower(ccy) %nlk% 'usd'][1,str_to_lower(ccy)],collapse.freq='month',filter=0){
 # newer version of collapsing 
 # todo: construct and use modupccy
 #ccyA="eur";dtin.<-dtin
 
 if (ccyA=='eur') natA<-'eurozone'
 
 
 if (filter==1){ # issued in both ccy before
   dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][d>=earlist.usd & d>=eval(exparse(str_c('earlist.',ccyA)))]
 } else if (filter==2){ # issued in both ccy ever (before and after)
   dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
 }
 
 #print(str_c('collapsing using: ', ccyA, ' ',natA))
 dtin<-copy(dtin.[d>=ymd('2002-01-01')])
 dtin[,ccy:=str_to_lower(ccy)]
 dtin[,modnat:=str_to_lower(modnat)]
 dtin<-dtin[modnat %in% c(natA,'united states') & ccy %in% c('usd','1usd',ccyA)]
 natA=str_to_lower(natA)
 dtin[,date:=d]
 
 # basic summation in this section: cannot yet do add/subract/divid/multiple since rows are different
 # Yankee isuance
 dtin[modnat==natA & ccy %like% 'usd|1usd',I_fUSD:=sum(na.omit(amt))/1000,by=date]
 # reverse yankee
 dtin[modnat=='united states' & ccy==ccyA,I_usF:=sum(na.omit(amt))/1000,by=date]
 # issuance from both countries in either currencies
 dtin[,I_both:=sum(na.omit(amt))/1000,by=date]
 # mu: issuance only in usd/total issuance
 dtin[ccy %like% 'usd|1usd',I_usd_tot:=sum(na.omit(amt))/1000,by=date]
 
 # first collapse into unique values by all columns, but each of the variables appear on a different row
 dt2<-dtin[,.(date,I_fUSD,I_usF,I_both,I_usd_tot)] %>% unique()    
 # get rid of NAs by combining rows of identical yrmo, first melt into long, get rid of NAs, then cast back to wide
 dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')))[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})
 
 # when there are no flow for a certain month, use zero
 dtout[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][is.na(I_usd_tot),I_usd_tot:=0]
 
 # Calculations based on earlier summations: net flow are net Yankee flow
 dtout[,I_netflow:=I_fUSD-I_usF][,i_netflow:=I_netflow/I_both*100][,mu:=I_usd_tot/I_both]
 
 # cacluate a smooth version
 dtout[,I_both_12m.L.avg:=rowMeans(dtout[,shift(I_both,n=0:11,type='lag')])]
 dtout[,i_netflow.smooth:=I_netflow/I_both_12m.L.avg*100] 
 
 # check to make sure there's no dates missing in case there is a month without any issuance at all!
 # dtout<-dtout %>% expandfulldates(.,freq='daily') %>% as.data.table()
 # if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');browser()}
 dtout[,ccy:=ccyA]
 dtout %>% setkey(date,ccy)
 dtout
}

dtiss.in<-dtissraw %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
dtiss.in<-dtissraw
dtiss.collapse.d <- dtiss.in %>% icollapse4.daily('eur',collapse.freq = 'daily',filter=params$icollapse.filter) %>% setkey(date,ccy)

#dtin.=dtiss.in;ccyA='eur';collapse.freq='daily';filter=0


#' #### add 3s6s basis as well later
eubs5<-prl[ticker=='eubs5',.(date,value)] %>% setnames('value','cip') %>% setkey(date)
dtiss.collapse.d %>% setkey(date)
dtreg<-dtiss.collapse.d[eubs5]
#+ include=F
dtreg[is.na(I_netflow),I_netflow:=0]
dtreg[,D.cip:=cip-shift(cip,n=1,type='lag')]
dtreg[,D1.cip:=shift(cip,n=1,type='lead')-shift(cip,n=1,type='lag')]
dtreg[,D2.cip:=shift(cip,n=2,type='lead')-shift(cip,n=2,type='lag')]
dtreg[,D3.cip:=shift(cip,n=3,type='lead')-shift(cip,n=3,type='lag')]
dtreg[,D4.cip:=shift(cip,n=4,type='lead')-shift(cip,n=4,type='lag')]
dtreg[,D5.cip:=shift(cip,n=5,type='lead')-shift(cip,n=5,type='lag')]
dtreg[,D6.cip:=shift(cip,n=6,type='lead')-shift(cip,n=6,type='lag')]

#+ include=T,echo=T
resls <- list()
resls[[length(resls)+1]] <- dtreg %>% felm(D.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D1.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D2.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D3.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D4.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D5.cip~I_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D6.cip~I_netflow,.)
stargazer(resls,type='text',report='vct*')

resls <- list()
resls[[length(resls)+1]] <- dtreg %>% felm(D.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D1.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D2.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D3.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D4.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D5.cip~i_netflow,.)
resls[[length(resls)+1]] <- dtreg %>% felm(D6.cip~i_netflow,.)
stargazer(resls,type='text',report='vct*')
#save.image(file='results161010.RData')
