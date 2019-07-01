


# abs net dev and D, all iss ----------------------------------------------

# collapse each

dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.)
dtin2 %>% setkey(upcusip,ccy)
# limiting to relevent bonds: sol1: just merge with with ys1m$dtreg
dtin2<-dtin2 %>% semi_join(ys1m$dtreg[,.N,upcusip],by='upcusip') %>% as.data.table()
# Monthly
registerDoParallel(1)
dtiss.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtiss.collapse.m %>% setkey(date,ccy)

## merging issuance

dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtcreditcip.m %>% setkey(date,ccy); dtiss.collapse.m %>% setkey(date,ccy)
dtreg.m<-dtcreditcip.m[dtiss.collapse.m,nomatch=0]

dtreg.m[,month:=month(date)]
dtreg.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
dtreg.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
dtreg.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
dtreg.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreg.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreg.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]

# intial result on all issuance and net deviation reduction 
dtreg.m %>% felm(D.abs.netmisp~I_both|ccy|0|ccy,.) %>% summary()
dtreg.m[ccy=='eur'] %>% lm(D.abs.cip~I_both,.) %>% summary()
# dtreg.m %>% ggplot(aes(I_both,D.abs.credit))+geom_point(aes(colour=ccy))+stat_smooth(method = 'lm')
# dtreg.m[ccy=='eur'] %>% ggplot(aes(I_both,D.abs.credit))+geom_point()+stat_smooth(method = 'lm')   


# debt maturity aggregate
dtin2 %>% icollapse4(.,'eur',collapse.freq = 'month',filter=1)

dtin2 %>% icollapse4.mature(.,'eur',collapse.freq = 'month',filter=1)


# Monthly

registerDoParallel(1)
dtmat.collapse.m <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4.mature(.,iccy,collapse.freq = 'month',filter=1)
} %>% rbindlist()
dtmat.collapse.m <- tmp %>% rbindlist()
dtmat.collapse.m %>% setkey(date,ccy)

## merging matured with credit cip
dtcreditcip.m<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'month')][,.SD[1],.(date,ccy)]
dtcreditcip.m %>% setkey(date,ccy); dtmat.collapse.m %>% setkey(date,ccy)
dtreg.mat.m<-dtcreditcip.m[dtmat.collapse.m,nomatch=0]

dtreg.mat.m[,month:=month(date)]
dtreg.mat.m[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.credit:=credit-shift(credit,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.netmisp:=netmisp-shift(netmisp,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.I_both:=I_both-shift(I_both,n=1,type='lag'),.(ccy)]
dtreg.mat.m[,D.abs.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreg.mat.m[,D.abs.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreg.mat.m[,D.abs.netmisp:=abs(netmisp)-abs(shift(netmisp,n=1,type='lag')),.(ccy)]


dtreg.mat.m %>% setnames('I_both','I_both.mat')
##dt.iss.mat.m<-update.dt(dtreg.m,dtreg.mat.m,keyfield = c('date','ccy'),override = T)
dt.iss.mat.m<-merge(dtreg.m,dtreg.mat.m,by= c('date','ccy'),suffixes = c('','.y'))
dt.iss.mat.m %>% setnames(c('I_both'),c('I_both.iss'))
dt.iss.mat.m[is.na(I_both.iss),I_both.iss:=0]


# properly do IV
dt.iss.mat.m %>% ds
regres<-list()
regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | 0,.) 
regres[[length(regres)+1]]<-dt.iss.mat.m %>% felm(D.abs.netmisp ~ 0 |ccy| (I_both.iss~I_both.mat) | ccy,.) 
stargazer(regres,type='text',report='*vct*')
regres[[2]] %>% summary(robust=T)
regres[[2]] %>% summary(robust=F)

# dt.iss.mat.m %>% write.dta('temp_absdiss.dta')
system('rm temp.csv')
stata('format date %tm
      gen year=year(date)
      gen monthly=ym(year,month)
      format monthly %tm
      tsset ccy monthly
      eststo clear
      *eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust 
      *eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust 
      eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp i.ccy,robust 
      eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy,robust 
      *eststo: xi: reg I_both_iss I_both_mat i.ccy,robust 
      eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust 
      esttab
      esttab using temp.csv, order(I_both*) bracket r2 nostar nogaps replace', data.in=dt.iss.mat.m[date<ymd('2026-07-30')]) #& ccy %in% c('eur','gbp','chf','cad')])#dtout<-fread('temp.csv',sep=',')
# dtout<-fread('temp.csv',sep=',')
system('open temp.csv')
# dtout<-read.csv('temp.csv',sep=',')


# leverage ----------------------------------------------------------------

dtlev<-read.dta13(file='bdlevq.dta') %>% as.data.table()
dtlev<-dtlev[,month:=1+(quarter-1)*3][,date:=ymd(str_c(year,'-',month,'-01'))]

# dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,.SD[3],.(date,ccy)]
dtcreditcip.q<-copy(dtcreditcip)[order(date)][,date:=lubridate::floor_date(date,'quarter')][,lapply(.SD,mean),.(date,ccy)]
dtlev %>% setkey(date)
dtreglev<-dtlev[dtcreditcip.q]
dtreglev[,D.credit:=abs(credit)-abs(shift(credit,n=1,type='lag')),.(ccy)]
dtreglev[,D.cip:=abs(cip)-abs(shift(cip,n=1,type='lag')),.(ccy)]
dtreglev[,D.bdleverage2:=bdleverage2-shift(bdleverage2,n=1,type='lag'),.(ccy)]
dtreglev[,D.levfac2:=levfac2-shift(levfac2,n=1,type='lag'),.(ccy)]
dtreglev[,D.levfac2adj:=levfac2adj-shift(levfac2adj,n=1,type='lag'),.(ccy)]
dtreglev[,D.lnbdleverage2:=log(bdleverage2)-log(shift(bdleverage2,n=1,type='lag')),.(ccy)]
dtreglev[,D.lnbdleverage:=log(bdleverage)-log(shift(bdleverage,n=1,type='lag')),.(ccy)]
# 

#### this is what works::
dtreglev <- dtreglev[,abscip:=abs(cip)][,abscredit:=abs(credit)]
# dtreglev %>% write.dta('temp.dta')
stata_src <- '
format quarterly %tq
eststo clear
*eststo: xi:reg abscip levfac2adj i.ccy,robust
*eststo: xi:reg abscredit levfac2adj i.ccy,robust
tsset ccy quarterly 
eststo: newey abscip levfac2adj,lag(4) force
eststo: newey abscredit levfac2adj,lag(4) force
esttab using temp.csv, bracket r2 nostar nogaps replace
'
stata(stata_src,data.in=dtreglev)
system('open temp.csv')


####################
##########################
############################################
##########################
############################################
##########################
############################################
##########################
########################
ccyA='eur'
collapse.freq = 'month';
filter=1
dtin.<-dtin2 %>% copy()
# newer version of collapsing 
# todo: construct and use modupccy
#ccyA="eur";dtin.<-dtin
#dtin.=dtin2;  ccyA='eur';  freq='month';  filter=0;
if (ccyA=='eur') natA<-'eurozone'
else if (ccyA=='gbp') natA<-'united kingdom'
else if (ccyA=='jpy') natA<-'japan'
else if (ccyA=='aud') natA<-'australia'
else if (ccyA=='chf') natA<-'switzerland'
else if (ccyA=='cad') natA<-'canada'
else if (ccyA=='usd') return(data.table(NA))
else stop(str_c('no ccy country match: ',ccyA))
natA=str_to_lower(natA)

if (filter==1){ # issued in both ccy before
  dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)][mat2>=earlist.usd & mat2>=eval(exparse(str_c('earlist.',ccyA)))]
} else if (filter==2){ # issued in both ccy ever (before and after)
  dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')][!is.na(eval(exparse(str_c('earlist.',ccyA)))) & !is.na(earlist.usd)]
} else{
  dtin.<-dtin.[ccy %in% c(ccyA,'usd','1usd')]
}

print(str_c('collapsing using: ', ccyA, ' ',natA))
dtin<-copy(dtin.[d>=ymd('2002-01-01')])
dtin[,ccy:=str_to_lower(ccy)]
dtin[,modnat:=str_to_lower(modnat)]
dtin<-dtin[modnat %in% c(natA,'united states') & ccy %in% c('usd','1usd',ccyA)]
dtin[,date:=floor_date(mat2,collapse.freq)]
dtin <- dtin[!is.na(date)]

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
dtout<-(dt2[order(date)] %>% melt(id.vars=c('date')) %>% unique())[!is.na(value)] %>% dcast.data.table(date~variable,fun=function(x) {if(length(unique(x))>1) {print('error with collapsing'); browser()}; first(x)})

aa<-(dt2[order(date)] %>% melt(id.vars=c('date')) %>% unique())[!is.na(value)] %>% dcast(date~variable)
aa[,.(date,I_both)] %>% ggplotw()
# when there are no flow for a certain month, use zero
dtout[is.na(I_fUSD),I_fUSD:=0][is.na(I_usF),I_usF:=0][is.na(I_usd_tot),I_usd_tot:=0]

# Calculations based on earlier summations: net flow are net Yankee flow
dtout[,I_netflow:=I_fUSD-I_usF][,i_netflow:=I_netflow/I_both*100][,mu:=I_usd_tot/I_both]

# check to make sure there's no dates missing in case there is a month without any issuance at all!
dtout<-dtout %>% expandfulldates(.,freq=collapse.freq) %>% as.data.table()
#if(nrow(dtout)!=dtout_check) {print('missing issuance in certain months, set to 0');}
dtout[,ccy:=ccyA]
dtout %>% setkey(date,ccy)
dtout 