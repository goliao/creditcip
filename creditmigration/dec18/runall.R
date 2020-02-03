setwd("C:/Users/Gordon/Dropbox/Research/ccy basis/creditmigration/dec18")
source('../util.r')
# opt
opt_calc=0


if (opt_calc==1) source('setupcalc.R') else load('img_calc.RData')
source('gen_summary_table.R')
# Table Issuance flow response  & Table Pre- and post crisis issuance flow sensitivity
# Quarterly issuance graph FIgure 1b
source('gen_flow_tables.R') 
source('gensubsamplev2.R') # these are the subsample plots

source('gen_plots_flipped.R')



################

bop<-fread('bop.csv') %>% melt(id.vars='year',variable.name='ccy',value.name='ca')


bops<-bop[year %between% c(2009,2016),.(ca=mean(ca)),ccy]
bops[order(ca)]
prl
yrstr='5'
cip<-copy(dtm$prw[,c('date',str_c('eubs',yrstr),str_c('bpbs',yrstr),str_c('jybs',yrstr),str_c('adbs',yrstr),str_c('sfbs',yrstr),str_c('cdbs',yrstr)),with=FALSE])
cip %>% setnames(str_c(c('eubs','bpbs','jybs','adbs','sfbs','cdbs'),yrstr),c('eur','gbp','jpy','aud','chf','cad'))
cipl<-melt(cip,id.vars=c('date'),variable.name='ccy',value.name='cip')
cipl %>% setkey(date,ccy)

cipl



###
## 1) organize data to have ccy, tenor, par swap value 
## 2) interpolate function to interpolate to any tenor; interpolate on the fly or construct large set of forward start data.table
## 3) calculate forwards, 1m carry, 1m realized return using forwards
## 4) do summary stats on raw realized return for different fwd swaps and conditional   
spx<-fread('C:\\Users\\Gordon\\Dropbox\\Research\\barroraredisaster\\data\\spxTR.csv')[,.(date=ymd(date),px=value)]
spx[,return:=log(px/shift(px,n=1,type='lag'))]
spx[,diff:=(px-shift(px,n=1,type='lag'))]

spx[date>=ymd('2016-01-01'),.(date,px)] %>% ggplotw
spx[!is.na(return) & date>=ymd('2016-04-01') & date<=ymd('2016-10-15'),.(mean(return)/sd(return)*252/sqrt(252))]
spx[!is.na(return) & date>=ymd('2016-04-01') & date<=ymd('2016-10-15'),.(mean(diff)/sd(diff)*252/sqrt(252))]

252*mean(spx$return %>% na.omit)/(sqrt(252)*sd(spx$return %>% na.omit))

spxmo<-spx[,.SD[date==max(date),.(px)],floor_date(date,'month')]
spxmo[,ret:=log(px/shift(px,n=1,type='lag'))]
12*mean(spxmo[!is.na(ret)]$ret)/(sqrt(12)*sd(spxmo[!is.na(ret)]$ret))


spxmo[,ret:=(px/shift(px,n=1,type='lag')-1)]
12*mean(spxmo[!is.na(ret)]$ret)

spxmo[,diff:=(px-shift(px,n=1,type='lag'))]
12*mean(spxmo[!is.na(diff)]$diff)/(sqrt(12)*sd(spxmo[!is.na(diff)]$diff))

require('PerformanceAnalytics')

PerformanceAnalytics::SharpeRatio()
##################

dtcreditcip[,cipret:=cip-shift(cip,n=1,type='lag'),ccy]
dtcreditcip[,F.cipret:=shift(cipret,n=1,type='lead'),ccy]
dtcreditcip[,F3.cipret:=shift(cip,n=3,type='lead')-cip,ccy]
dtcreditcip[,F6.cipret:=shift(cip,n=6,type='lead')-cip,ccy]

dtcreditcip[,creditret:=credit-shift(credit,n=1,type='lag'),ccy]
dtcreditcip[,F.creditret:=shift(creditret,n=1,type='lead'),ccy]
dtcreditcip[,F3.creditret:=shift(credit,n=3,type='lead')-credit,ccy]
dtcreditcip[,F6.creditret:=shift(credit,n=6,type='lead')-credit,ccy]


dtcreditcip[,cipcarry:=0]
dtcreditcip[cip>0,cipcarry:=-F.cipret]
dtcreditcip[cip<0,cipcarry:=F.cipret]

#dtcreditcip[date>ymd('2010-01-01'),.(mean(na.omit(cipcarry)),sd(na.omit(cipcarry)))]

threshold=10
dtcreditcip[,cipcarry:=0]
dtcreditcip[cip>credit+1.96*creditse & cip > threshold,cipcarry:=-F.cipret]
dtcreditcip[cip<credit-1.96*creditse & cip < -threshold,cipcarry:=F.cipret]

dtcreditcip[,.N,cipcarry!=0]
dtcreditcip[date>ymd('2010-01-01'),.(avg=mean(na.omit(cipcarry)),sd=sd(na.omit(cipcarry)))][,.(avg,sd,sharpe=avg/sd*(12/sqrt(12)))]
dtcreditcip[,.(avg=mean(na.omit(cipcarry)),sd=sd(na.omit(cipcarry)))][,.(avg,sd,sharpe=avg/sd*(12/sqrt(12)))]

# assume vol=10
targetvol=.1/sqrt(12)
mu=.1
sigma=3
# assume $100 notional
capital=100*sigma/10000/targetvol

ret=.1/10000/capital
sigmacash=capital*targetvol
ret/sigmacash


return(code)dtcreditcip[,cipret %>% na.omit() %>% mean()]

dtcreditcip %>% felm(F.cipret~netmisp|ccy|0|0,data=.) %>% summary()
dtcreditcip %>% felm(F3.cipret~netmisp|ccy|0|0,data=.) %>% summary()


dtcreditcip %>% felm(F.creditret~netmisp|ccy|0|0,data=.) %>% summary()
dtcreditcip %>% felm(F.creditret~netmisp+creditret|ccy|0|0,data=.) %>% summary()
dtcreditcip %>% felm(F3.creditret~netmisp|ccy|0|0,data=.) %>% summary()
