setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
# New merge: price with sdc -----------------------------------------------
rm(list=ls(all=TRUE));load('gldb.RDATA')
source('util_20160620.r')
br<-bondref[!is.na(pk)] %>% issfilter(.)
br<- br %>% semi_join(dtl,by='pk') %>% as.data.table()

# MERGE RATING AND ADD MATURITY BUCKETS -----------------------------------
setkey(dtl,pk);setkey(br,pk)
dtl<-dtl[monthend==1]
dtl2<-dtl[br[,.(ccy,mat2,rating,nrating,upcusip,pk,isin,ytofm,sicfac,sic1)],nomatch=0]
dtl2[,ytm:=as.numeric((mat2-date)/365)]
dtl3<-dtl2[ytm >.05]
dtl3[is.na(nrating),nrating:=0];dtl3[ccy=='sek',ccy:='eur']
dtl3<-dtl3[field=='YLD_YTM_MID']
dtl3<-dtl3 %>% bucketrating() %>% bucketytm()
prl<-prl[date>'2002-01-01']
prl[ticker %like% '^\\w\\wsw\\d+' | ticker %like% '^eusa\\d+',value:=value*100]
prw<-prl %>% distinct() %>% data.table::dcast(.,date~ticker,value.var = 'value')
dtl3<-dtl3[date>'2004-01-01']

# gen eusw=eusa-eubsv # gen eusz=eusw+eubs
prw[,`:=`(eusw1=eusa1-eubsv1,eusw10=eusa10-eubsv10,eusw12=eusa12-eubsv12,eusw15=eusa15-eubsv15,eusw2=eusa2-eubsv2,eusw20=eusa20-eubsv20,eusw30=eusa30-eubsv30,eusw5=eusa5-eubsv5,eusw7=eusa7-eubsv7)]
prw[,`:=`(eusz10=eusw10+eubs10,eusz12=eusw12+eubs12,eusz15=eusw15+eubs15,eusz2=eusw2+eubs2,eusz20=eusw20+eubs20,eusz30=eusw30+eubs30,eusz5=eusw5+eubs5,eusz7=eusw7+eubs7,eusz1=eusw1+eubs1)]
prw[,`:=`(jysz10=jysw10+jybs10,jysz12=jysw12+jybs12,jysz15=jysw15+jybs15,jysz2=jysw2+jybs2,jysz20=jysw20+jybs20,jysz30=jysw30+jybs30,jysz5=jysw5+jybs5,jysz7=jysw7+jybs7,jysz1=jysw1+jybs1)]
prw[,`:=`(bpsz10=bpsw10+bpbs10,bpsz12=bpsw12+bpbs12,bpsz15=bpsw15+bpbs15,bpsz2=bpsw2+bpbs2,bpsz20=bpsw20+bpbs20,bpsz30=bpsw30+bpbs30,bpsz5=bpsw5+bpbs5,bpsz7=bpsw7+bpbs7,bpsz1=bpsw1+bpbs1)]
prw[,`:=`(adsz1=adsw1+adbs1,adsz10=adsw10+adbs10,adsz2=adsw2+adbs2,adsz5=adsw5+adbs5,adsz7=adsw7+adbs7,adsz15=adsw15+adbs15,adsz20=adsw20+adbs20,adsz12=adsw12+adbs12,adsz30=adsw30+adbs30)]
# transform prw back to prl
prl<-data.table::melt(prw,id.vars='date',variable.name='ticker')[date!='2016-02-25']
prw<-prw[date!='2016-02-25']
# save(br,dtl3,prl,prw,file='dtclean.RData')
# save.image('dtclean.RData')

rm(list=ls(all=TRUE));
#load('dtclean160624.RData')
load('dtclean.RData')
source('util_20160620.r')


dtl3[,liq:=ytm/ytofm]
dtl3<-dtl3[liq %between% c(0,1.1)]
dtl3[liq<.5,liq_bucket:=0] # more illiq
dtl3[liq>=.5,liq_bucket:=1] # liq

# ys1liq<-resyldsprdv3(dtl3[liq_bucket==1],prl,regversion=5)
# ys1iliq<-resyldsprdv3(dtl3[liq_bucket==0],prl,regversion=5)
# 
# ys1liq[ys1iliq] %>% ggplotw()
# ys1liq[ys1iliq][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()
# ys1liq[ys1iliq][,.(date,ccyjpy,i.ccyjpy)] %>% ggplotw()
# ys1liq[ys1iliq][,.(date,liquid,i.liquid)] %>% ggplotw()
# 
# ys1<-resyldsprdv3(dtl3,prl,regversion=8)

#ys0<-resyldsprdv2(dtl3,prl,regversion=6) # swap spread only w/o 3s6s adj.
ys1<-resyldsprdv3(dtl3[ccy %in% c('usd','eur')],prl,regversion=6)
# ys1nf<-resyldsprdv3(dtl3[sic1!=6],prl,regversion=6)
# ys2<-resyldsprdv3(dtl3,prl,regversion=6,adjccybs = 1)
# 
# ys1 %>% ds()
# ys1[,.(date,liquid)] %>% ggplotw()
# dtl3 %>% ds
# Figure 1 CIP deviations
# fig1<-prw[,.(date,eubs5,bpbs5,jybs5,adbs5)] %>% ggplotw()+xlab('')+ylab('CIP deviations 5-year horizon in bps (implied r - actual r)')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "GBP",'EUR','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
#ggsave(file='../paper/figures/fig1_cip.pdf',fig1,width=9,height=6)

# Figure 2 Credit mispring
fig2<-ys1[,.(date,ccyeur,ccygbp,ccyaud,ccyjpy)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c("AUD", "GBP",'EUR','JPY'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig2
#ggsave(file='../paper/figures/fig2_creditmisprice.pdf',fig2,width=9,height=6)
ys1[,.(date,ccyeur)] %>% ggplotw()

# Figure 3 Credit mispring and CIP for EUR
fig3<-ys1[prw][date>'2004-01-01',.(date,ccyeur,eubs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (EU-US)','CIP deviations 5yr (implied - actual euro funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig3
#ggsave(file='../paper/figures/fig3_creditCIPeur.pdf',fig3,width=9,height=6)

# Figure 4 Credit mispring and CIP for JPY
fig4<-ys1[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Res. credit spread diff (JP-US)','CIP deviations 5yr (implied - actual yen funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
fig4
#ggsave(file='../paper/figures/fig4_creditCIPjpy.pdf',fig4,width=9,height=6)

# fig4b<-ys1nf[prw][date>'2004-01-01',.(date,ccyjpy,jybs5)] %>% ggplotw()+xlab('')+ylab('bps')+geom_hline(yintercept=0)+ scale_color_discrete('',labels = c('Residualized credit spread diff (JP-US)','CIP deviations 5yr (implied - actual yen funding rate)'))+theme_stata(base_size = 15)+theme(axis.title.y = element_text(margin =margin(0, 10, 0, 0)))
# fig4b
#ggsave(file='../paper/figures/fig3_creditCIPjpy.pdf',fig4,width=9,height=6)
ys1[prw][date>'2004-01-01',.(date,ccygbp,bpbs5)] %>% ggplotw()
ys1[prw][date>'2004-01-01',.(date,ccyaud,adbs5)] %>% ggplotw()

# Figure 5: crd mispricing and issuance; save data for it first
ys2[,.(date,ccyeur)] %>% ggplotw()
ys2 %>% write.dta('temp_ys.dta')


ys2[,.(date,ccyjpy)] %>% ggplotw()


ys1 %>% ggplotw()
ys1nf %>% ggplotw()
# eur
ys1[prw][,.(date,ccyeur,eubs5)] %>% ggplotw()
ys1nf[prw][,.(date,ccyeur,eubs5)] %>% ggplotw()
# # adj 3s6s manual; manual adj looks better but oh well
# ys0[prw][,.(date,ccyeur,eubs5adj=eubs5-eubsv5)] %>% ggplotw()


ys1 %>% ggplotw()
ys2 %>% ggplotw()

ys1[ys2][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()
ys1[ys2][,.(date,ccyeur,i.ccyeur)] %>% ggplotw()
ys1[ys2][,.(date,ccyjpy,i.ccyjpy)] %>% ggplotw()
ys1[ys2][,.(date,ccyjpy,i.ccyjpy)] %>% ggplotw()


# using previous version, adjusted 3s6s spread and ccy basis manually
ysi1<-ys0[prw][,.(date,ccyeur,ccyeur_eff=ccyeur-(eubs5-eubsv5))] 
ysi1 %>% ggplotw()
ysi2<-ysi1[!is.na(ccyeur_eff)]
ysi2 %>% write.dta('temp_ys.dta')

ys1[prw][,.(date,ccyeur,eubs5)] %>% ggplotw()

# desc=c('ys: previous prl interpolate do not extend (rule1) ','ys1: previous prl interpolate extends (rule2)')
# save(ys,ys1,desc,file='ystemp.RData')

ys %>% ggplotw()
ys[prw][,.(date,ccyeur,eubs5,eubsv5)] %>% ggplotw()


### cool plot1
ys[prw][,.(date,ccyeur,eubs5adj=eubs5-eubsv5)] %>% ggplotw()
#adding swap euus
ys[prw][,.(date,ccyeur,eubs5adj=eubs5-eubsv5,swapeuus=eusa5-ussw5-eubsv5)] %>% ggplotw()

# non-financial
ys_nf<-resyldsprdv2(dtl3[sic1!=6],prl,regversion=5)
ys_nf %>% ggplotw()
ys_nf[prw][,.(date,ccyeur,eubs5adj=eubs5-eubsv5)] %>% ggplotw()
ys_nf[prw][,.(date,ccyjpy,jybs5)] %>% ggplotw()
ys_nf[prw][,.(date,ccygbp,bpbs5)] %>% ggplotw()

ys_nf[prw][,.(date,ccyaud,adbs5,zero=0)] %>% ggplotw()
yldsprd2[[1]][prw][,.(date,ccyaud,adbs5,zero=0)] %>% ggplotw()

ys_f<-resyldsprdv2(dtl3[sic1==6],prl,regversion=4)
ys_f[prw][,.(date,ccyaud,adbs5)] %>% ggplotw()


# Relating to issuance data -----------------------------------------------
#### Linking to isssuance data 
source('util.r')
dtissue<-bondref %>% issfilter(.)
## retain sdc where isin/cusip matches credit spread
dtissue2<-dtissue %>% semi_join(dtl3[ccy %in% c('eur','usd')],by='upcusip')
dtiss<-icollapse3(dtissue2[ccy %in% c('eur','usd') & amt>100])
dtreg<-mergebymonth(dtiss,ysi1)
#dtreg[,.(date,ccyeur_eff,ccyeur)] %>% ggplotw()
dtreg[,.(date,ccyeur_eff,isspct_euus=i_net_euus*100)] %>% ggplotw()



yldsprd2[[1]][date<ymd('2016-03-01')] %>% ggplotw()
prl[ticker %in% c('eubs5','bpbs5','jybs5','adbs5')] %>% ggplotl()

prl[ticker %in% c('eubs5','bpbs5','jybs5','adbs5')]
priceraw[,.(date,eubs5,eubs10,bpbs5,bpbs10)][yldsprd2][,.(date,eubs5,ccyeur)] %>% ggplotw()
priceraw[,.(date,eubs5,bpbs5,bpbs10)][yldsprd2][,.(date,bpbs10,ccygbp)] %>% ggplotw()
yldsprd4[yldsprd5] %>% ggplotw()
yldsprd5[yldsprd3][yldsprd4]  %>% ggplotw()


# finance vs nonfiannce
ys_nf<-dtl3[sic1!=6] %>% resyldsprdv2(.,prl,regversion=1)
ys_f<-dtl3[sic1==6] %>% resyldsprdv2(.,prl,regversion=1)
ys_nf[ys_f][date<'2016-02-01',.(date,ccyjpy,i.ccyjpy)] %>% ggplotw()
ys_nf[ys_f][date<'2016-02-01',.(date,ccyeur,i.ccyeur)] %>% ggplotw()
ys_nf[ys_f][date<'2016-02-01',.(date,ccyaud,i.ccyaud)] %>% ggplotw()
ys_nf[ys_f][date<'2016-02-01',.(date,ccygbp,i.ccygbp)] %>% ggplotw()

# for gbp
priceraw[,.(date,bpbs5)][ys_nf] %>% ggplotw()
priceraw[,.(date,bpbs5)][ys_f] %>% ggplotw()

#illiquid vs liquid split
# define illiquid as bonds that passed its prime, ytm/(ytofm)<.5
ys_liq<-(dtl3[liq>.5] %>% resyldsprdv2(.,priceraw,regversion=5))
ys_iliq<-(dtl3[liq<=.5] %>% resyldsprdv2(.,priceraw,regversion=5))
setnames(ys_iliq,'ccyeur','ccyeur_iliq')
ys_liq[ys_iliq][,.(date,ccyeur,ccyeur_iliq)] %>% ggplotw()
# including it as a factor doesn't chg much however
ys<-resyldsprdv2(dtl3,priceraw,regversion=1)
ysliq<-resyldsprdv2(dtl3,priceraw,regversion=6)
ys[ysliq] %>% ggplotw()

priceraw %>% ds('eubs')


# exploratory by single date ----------------------------------------------

regdata<-dtl3[date=='2016-05-31' & ccy %in% c('usd','eur')][,liq:=ytm/ytofm]
aa<-resyldsprdv2(regdata,prl,regversion=4,returndt = 1)
aa[[1]]
aa[[2]][,median(value),ccy]
regdata<-dtl3[date=='2016-01-29' & ccy %in% c('usd','eur')][,liq:=ytm/ytofm]
aa<-resyldsprdv2(regdata,prl,regversion=4,returndt = 1)
aa[[1]]
aa[[2]][,median(value),ccy]

prw[,.(date,eusa5,ussw5)] %>% View
prw[,.(date,eusa)]
prw[,.(date,ussw5)]

regdt<-regres[[2]]
regdt[,.N,by=.(date,ccy)] %>% ggplot(aes(x=date,y=N,colour=ccy))+geom_line()
dtl2[,.N,by=.(date,ccy)] %>% ggplot(aes(x=date,y=N,colour=ccy))+geom_line()

newadditions<-regdt[ccy=='1usd' & date=='2014-07-31'] %>% anti_join(regdt[ccy=='1usd' & date=='2014-06-30'],by='isin')
(br %>% semi_join(newadditions,by='isin'))[,.(pk,i,descr)] # why are prices here missing?


br %>% semi_join(dtl3[ccy=='usd' & date=='2014-07-31'] %>% anti_join(dtl3[ccy=='usd' & date=='2014-06-30'],by='isin'),by='isin') # why are prices here missing?

# Old residualizing  ----------------------------------------------------------
ccyfe<-getccyFE(dtl2,fieldstr='OAS_SPREAD_BID')
ccyfe_yield<-getccyFE(dtl2,fieldstr='YLD_YTM_MID')
ccyfe_as<-getccyFE(dtl2,fieldstr='ASSET_SWAP_SPD_MID')

feg<-ccyfe_as[ccyfe_yield][ccyfe]
setnames(feg,c('euus','i.euus','i.euus.1'),c('euus_oas','euus_yld','euus_as'))
ggplotw(feg)

#create yield spread for aggregate 
dtl2[,ytm:=as.numeric((mat2-date)/365)]
#winsorize by value a little
dtl3<-dtl2[field=='YLD_YTM_MID',pctl:=percent_rank(value),by=.(date,ccy)][pctl>=.01 & pctl<=.99 & field=='YLD_YTM_MID']
# get rid of up where up doesn't have bonds in both ccys for each date
tokeep<-dtl3[,.N,by=c('date','upcusip','ccy')][,.N,by=c('date','upcusip')][N==2][,.(date,upcusip)]
setkey(tokeep,date,upcusip)
setkey(dtl3,date,upcusip)
dtl3<-dtl3[tokeep]

# winsorize by ytm a little
dtl3<-dtl3[,pctl:=percent_rank(ytm),by=.(date,ccy)][pctl>=.01 & pctl<.99]

# graph avg ytm by ccy by date
ytm_cc<-dtl3[,.(mean(na.omit(ytm))),by=.(date,ccy)]
setnames(ytm_cc,'V1','ytm_avg')
ytm_cc %>% qplot(x=date,y=ytm_avg,colour=ccy,data=.,geom='line')

# bring in the bbg prices
setkey(priceraw,date)
ussw_colnames<-priceraw %>% ds('ussw') %>% str_extract(regex('ussw.*')) %>% sort %>% unique
eusa_colnames<-priceraw %>% ds('eusa') %>% str_extract(regex('eusa.*')) %>% sort %>% unique
swapus<-priceraw[date>='1996-06-28',c('date',ussw_colnames),with=FALSE]
swapeu<-priceraw[date>='1999-01-29',c('date',eusa_colnames),with=FALSE]
swapprices<-swapus %>% left_join(swapeu,by='date')

#interpolate 
swapusl<-swapus %>% gather('tenor','ussw',-date) %>% mutate(tenor=as.numeric(str_extract(tenor,regex('\\d+')))) %>% as.data.table()
setkey(swapusl,date)
ytm_us<-swapusl[ytm_cc[ccy=='usd']]
usswyld<-ytm_us[!is.na(ussw),approx(x=tenor,y=ussw,xout=ytm_avg),by=date] %>% rename(ytm=x,usswyld=y) %>% unique %>% select(-ytm)
usswyld %>% qplot(date,usswyld,data=.,geom='line')

swapeul<-swapeu %>% gather('tenor','eusa',-date) %>% mutate(tenor=as.numeric(str_extract(tenor,regex('\\d+')))) %>% as.data.table()
setkey(swapeul,date)
ytm_eu<-swapeul[ytm_cc[ccy=='eur']]
euswyld<-ytm_eu[!is.na(eusa),approx(x=tenor,y=eusa,xout=ytm_avg),by=date] %>% rename(ytm=x,eusayld=y) %>% unique() %>% select(-ytm)
euswyld %>% qplot(date,eusayld,data=.,geom='line')


yldsprdcomp<-merge(usswyld,euswyld,by='date') %>% mutate(euus_swapsprd=eusayld-usswyld) %>% right_join(feg,by='date') %>% mutate(euus_yldsprd=euus_yld*100-euus_swapsprd) %>% 
  select(date,euus_yldsprd,euus_yld,euus_oas) %>% mutate(euus_yld=100*euus_yld) 
yldsprdcomp %>% ggplotw()
# euus_oas  :  oas directly from bloomberg
# euus_yld  : yld spread directly from bloomberg, not adjusting for underlying swap or treasury yld
# euus_yldsprd : euus_yld subtracting maturity avgd swap yield spread btw eu and us

# next step, try to generate yield sprd at the individual bond level instead of taking avg 
swappricesl<-swapprices %>% melt(id.vars='date',variable.name='field')
swappricesl[,ccy:=stringr::str_sub(field,1,2)][ccy=='eu',ccy:='eur'][ccy=='us',ccy:='usd'][,tenor:=as.numeric(str_extract(field,regex('\\d+')))]
setkey(swappricesl,date,ccy,tenor,field)
setkey(dtl3,date,ccy)
swappricesl[date=='2004-01-30' & ccy=='eur']


dtl3[!is.na(ytm),swapyld:=intrwrap(.SD,swappricesl,.BY),by=.(date,ccy)][swapyld==0,swapyld:=NA]
dtl3[,yldsprd:=value*100-swapyld]

setkey(dtl3,date,upcusip)

dfreg<-dtl3[yldsprd!='NA',.(date,ccy,upcusip,value=yldsprd,field='yldsprd')]
ccyfe_yieldsprd<-getccyFE(dfreg,fieldstr='yldsprd')
setnames(ccyfe_yieldsprd,'euus','euus_yldsprd')
ccyfe_yieldsprd %>% ggplotw()

feg2<-feg[ccyfe_yieldsprd]
feg2[,euus_yld:=NULL]
feg2 %>% ggplotw()
feg<-feg2

#save.image('temp.rdata')
load(file='temp.rdata')
source('util.r')



# compare to baml
Cdif_euus<-priceraw[,c('date',ds(priceraw,'Cdif_euus'),ds(priceraw,'eubs')),with=FALSE]

dfeu<-feg %>% left_join(Cdif_euus,by='date')
dfeu[,.(date,euus_yldsprd,Cdif_euus_30)] %>% ggplotw()

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_oas_eff,euus_yldsprd_eff,euus_oas,euus_yldsprd) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd_eff,euus_yldsprd) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd_eff,Cdif_euus_30_eff) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% mutate(euus_oas_eff=euus_oas-eubs5,euus_yldsprd_eff=euus_yldsprd-eubs5) %>% 
  select(date,euus_yldsprd,Cdif_euus_30) %>% filter(date>'2004-01-01') %>% wgplot(.)

dfeu %>% select(date,euus_yldsprd,eubs5) %>% wgplot()


raw<-read.dta13('sdc96_clean2.dta') %>% as.data.table()
df1<-raw[!is.na(raw$amt) & ccy %in% c('USD',"EUR",'AUD',"JPY",'GBP'),]
# current 


# new icollapse -----------------------------------------------------------
dfin<-df1  %>% 
  filter(amt>=50,ytofm>=2, ytofm<=99999,nrating<=16, (pub=="Public" | pub=="Sub."), 
         mdealtype %ni% c("P","ANPX","M","EP","CEP","TM","PP","R144P"), 
         secur %ni% c("Cum Red Pfd Shs", "Non-Cum Pref Sh" , "Preferred Shs" ,"Pfd Stk,Com Stk"),
         tf_mid_desc!='Government Sponsored Enterprises',
         nrating>1)
dfin<-dfin[ccy %in% c('USD','EUR') & amt>=100]
# retain sdc where isin/cusip matches credit spread

dfin %<>% semi_join(dtl3,by='upcusip')

#dfin %>% semi_join(dtl3,by='isin')


dfreg<-icollapse2(dfin) %>% mergebymonth(dfeu)
dfreg[,euus_yldsprd_eff:=euus_yldsprd-eubs5]
dfreg %>% write.dta('temp.dta')
dfreg[date>='2006-01-01',.(date,i_net_euus,Cdif_euus_30,euus_yldsprd)] %>% ggplotw()

dfreg<-dfreg[date>='2006-01-01']
#dfreg[,i_net_euus:=i_net_euus*100]
reg2<- dfreg %>% lm(lead(i_net_euus)~Cdif_euus_30,data=.)
reg3<- dfreg %>% lm(lead(i_net_euus)~Cdif_euus_30_eff,data=.)
reg4<- dfreg %>% lm(lead(i_net_euus)~euus_yldsprd,data=.)
reg5<- dfreg %>% lm(lead(i_net_euus)~euus_yldsprd_eff,data=.)
reg6<- dfreg %>% lm(lead(i_net_euus)~euus_oas,data=.)

stargazer(reg2,reg3,reg4,reg5,reg6,type='text',report="vct*")


dfreg[date>='2006-01-01',.(date,i_net_euus)] %>% ggplotw()


# previous stuff ----------------------------------------------------------

# plot ccy spread directly
dtoas[,.(oas_ccy=mean(value)),by=.(date,ccy)][,.(oas_euus=(.SD[ccy=='eur',oas_ccy]-.SD[ccy=='usd',oas_ccy])),by=.(date)] %>% 
  ggplot(aes(x=date,y=oas_euus))+geom_line()

dtoas_ccy

#keeping only bonds with up in the same period
up_date_multccy<-dtoas[,.N,by=.(date,upcusip,ccy)][,.N,by=.(date,upcusip)][N>1,.(date,upcusip)]
setkey(up_date_multccy,date,upcusip)
setkey(dtoas,date,upcusip)
  #keeping global bonds only
dtoas_glob<-merge(dtoas,up_date_multccy,all.y=TRUE)
dtoas_glob[,.N,by=.(date,ccy)] %>% ggplot(aes(x=date,y=N,colour=ccy))+geom_line()


dtoas[,,by=.(date,upcusip)]



dtl2[field=='OAS_SPREAD_BID',.(mean(value)),by=.(date,ccy)] %>% ggplot(.,aes(x=date,y=V1,colour=ccy))+geom_line()
# demeaned oas spread for each bond; demeaning issuer*time specific means
dt_oas_upmean %>% View




# Demeaned spread ---------------------------------------------------------

df_bond2<-df_bond %>% rename(oas=OAS_SPREAD_BID,upco=id_bb_ultimate_co) %>% select(-fac)
df_oas_issmean<-df_bond2 %>% group_by(date,upco) %>% summarise(oas_issmean=mean(oas))
# demeaned oas spread for each bond; demeaning issuer*time specific means
df_bond_de<-sqldf('select A.*, B.oas_issmean from df_bond2 as A left join df_oas_issmean as B on (A.date==B.date and A.upco==B.upco)') %>%
  tbl_df() %>% mutate(oas_res=oas-oas_issmean) %>% select(date,oas, oas_issmean, oas_res,upco,ccy) %>% arrange(date) 
# residual oas spread between eur and usd, issuer matched
df_ccyres<-df_bond_de %>% group_by(date,ccy) %>% summarise(mean_oas_res=mean(na.omit(oas_res))) %>% mutate(ccy=tolower(ccy)) %>% 
  dcast(.,date~ccy,value.var='mean_oas_res') %>% tbl_df() %>% mutate(euus_sprd=eur-usd)
df_ccyres %>% melt(.,id.vars='date') %>% filter(variable=='euus_sprd') %>% 
  ggplot(.,aes(x=date,y=value)) +geom_line()

priceraw<-read.dta13('prices_extended.dta')

# adjusting for xccy
df_price<-df_ccyres %>% full_join(.,priceraw,by='date') %>% mutate(oas_res_eff=euus_sprd-eubs5) %>% 
  select(date,oas_res_eff,euus_sprd) %>% filter(date>'2006-01-01') %>% wgplot(.)
# filter issuance 
df_reg_data<- df_sdc %>% as.data.frame() %>%  issfilter(.) %>% icollapse_all(.) %>% full_join(.,df_price,by='date')

df_reg_data %>% lm(I_net_euus~oas_res_eff,data=.)

regtemp<-function(dfreg){
  reg10_1<- dfreg %>% lm(I_net_euus~Cdif_euus_30,data=.)
  stargazer(reg10_1,type='text',report="vct*")
}


# explore yield -----------------------------------------------------------

load('../data/bloomberg/bbg_gbonds_160413.rdata')

a0 <- unlist(prices, recursive = FALSE)
tickernames <- names(a0)
df_yld <- data.frame() %>% tbl_df()
for (i in 1:length(tickernames)) {
  temp_new <- a0[[i]] %>% mutate(ticker = tickernames[i]) %>% tbl_df()
  if (nrow(temp_new) == 0)
    print (str_c('empty:#', i, ' name:', tickernames[i]))
  df_yld <- df_yld %>% dplyr::bind_rows(., temp_new)
}
df_yld %<>% left_join(.,globalbonds,by='ticker')
agg_yld<-df_yld %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(YLD_YTM_MID))) 

# plot yields for eur, usd
agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
  gather(.,key='type',value='yield',-date) %>% 
  filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

# describe the number of data points missing
df_yldw<-df_yld %>% dcast(.,date~ticker,value.var="yield")
df_yldw %>% summarise_each(.,funs(length(is.na())))
missings<-colSums(is.na(df_yldw[,-1])) %>% as.data.frame() 
colnames(missings)<-'Nmissings'
missings$ticker=rownames(missings)
summary(missings)
missings %>% ggplot(.,aes(x=Nmissings))+geom_density()

# describe number of data points avaialbe for each currency at each time
df_yld %>% group_by(date, crncy) %>% summarise(Ndp=length(ticker)) %>% 
  ggplot(.,aes(x=date,y=Ndp,colour=crncy))+geom_line()

# yld sprd construct ------------------------------------------------------
priceraw<-read.dta13('prices_extended.dta')
# add bond maturity for each date in time

# isolate swap yield curve from priceraw

# merge df_yld with swap yield curve

# take diff with matched 

# Merge with sdc data main ------------------------------------------------

raw<-read.dta13('sdc96_clean2.dta')
df_sdc<-raw %>% tbl_df() %>%  select(i,tic,isin,cu,d,nat,amt,descr,ccy,rating, nrating,mat2,ytofm,everything()) %>% arrange(d)
# filter and augment with expected number of monthly observations
df_sdc2<-df_sdc[!duplicated(df_sdc$isin),] %>% filter(isin!='-')  %>%
  arrange(isin)  %>% 
  filter(mat2>'2005-01-01') %>% 
  mutate(expmonthlyobs=ceiling((pmin(as.numeric(ymd('2016-04-01')),as.numeric(mat2))-
                                  pmax(as.numeric(settlement2),as.numeric(ymd('2005-01-01'))))/30.5)) 
  
# count number of observations by isin
df_obs<-df_yld2 %>% group_by(isin) %>% summarise(.,ct=length(isin)) %>% arrange(desc(ct))
# compare to number of expected obs by isin
df_obs2<-sqldf('select A.*, B.expmonthlyobs from df_obs as A left join df_sdc2 as B on A.isin=B.isin')
# df_obs2 %>% mutate(obsdiff=expmonthlyobs-ct) %>% group_by(obsdiff) %>% summarise(ctt=length(obsdiff)) %>% View

# merging
df_bond<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.d,B.mdealtype,B.amt,B.pub,B.issue_type_desc from df_yld2 as A, df_sdc2 as B where A.isin=B.isin')
df_bond2<-sqldf('select A.*, B.ccy,B.nrating, B.mat2,B.ytofm,B.i,B.descr,B.mdealtype from df_yld2 as A left join df_sdc2 as B on A.isin=B.isin')

# merge with sec new ------------------------------------------------------
# matching to newly downloaded sdc isins
sdc_raw<-read.csv('../data/sdc/sdc_bbgmatch.csv',stringsAsFactors = FALSE)
rating_sp<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_sp=rating,nrating_sp=nrating)
rating_mdy<-read.csv('rating.csv',stringsAsFactors = FALSE) %>% tbl_df() %>% rename(rating_mdy=rating,nrating_mdy=nrating)
df_sdcnew<-sdc_raw %>% mutate(d=mdy(d),mat2=mdy(mat2),amt=as.numeric(amt),ytofm=as.numeric(ytofm)) %>% tbl_df() %>% 
  left_join(.,rating_sp, by='rating_sp') %>% left_join(.,rating_mdy,by='rating_mdy') %>% 
  select(issname,ticker_sdc,isin,cusip,d,nat,amt,descr,ccy,mat2,ytofm,everything()) %>% arrange(d)
save(df_sdcnew,file='sdcnew.rdata')
df_sdcn2<-df_sdcnew[!duplicated(df_sdcnew$isin),] %>% filter(isin!='-')  %>% arrange(isin)

df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2 from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')
df_bond3<-sqldf('select A.*, B.ccy, B.descr, B.ytofm, B.d, B.mat2, B.issname, B.mdealtype from df_yld2 as A, df_sdcn2 as B where A.isin=B.isin')


# explore what's missing in which dataset ---------------------------------
# what's missing compare to earlier merge
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>%  group_by(i) %>% top_n(.,1,desc(date)) %>% dplyr::arrange(pub) %>% select(1,10:26) %>% View
anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>%  xtabs(~pub+mdealtype+issue_type_desc,data=.)
  # so a lot of bonds are excluded due to dealtypes, gov, R144D. but still has some that's unclear why was excluded
   anti_join(df_bond,df_bond3,by='isin') %>% tbl_df() %>% group_by(i) %>% top_n(.,1,desc(date)) %>% filter(issue_type_desc=='Investment Grade Corporate',mdealtype!='R144D') %>% View

# what's added compare to earlier merge
anti_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's the joint between new and old merge
semi_join(df_bond3,df_bond,by='isin') %>% tbl_df() %>%  group_by(isin) %>% top_n(.,1,desc(date)) %>% arrange(desc(ytofm)) %>% View

#what's not captured in the sdc data but showed up in bloomberg bsrch
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% View
anti_join(df_yld2,df_bond,by='isin') %>% anti_join(.,df_bond3,by='isin') %>% tbl_df() %>% group_by(isin) %>% top_n(.,1,desc(date)) %>% select(ticker:crncy) %>% write.csv(.,file='GetBBGBondData.csv')

#what's covered in sdc but not in bloomberg?
bbgmiss<-anti_join(df_sdcn2,df_yld2,by='isin') %>% filter(ccy!='gbp',nrating_sp<=10,nrating_mdy<=10,nrating_sp!=0,nrating_mdy!=0,amt>=100,ytofm>2,ytofm<50,
                                                 !grepl('Flt',typesec),!grepl('Zero Cpn',typesec),!grepl('Float',typesec),!grepl('Fl',descr),!grepl('Zero Cpn',descr)) 
bbgmiss 
bbgmiss  %>% xtabs(~typesec,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~mktplace,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss  %>% xtabs(~upsic,data=.) %>% as.data.frame() %>% arrange(desc(Freq)) %>% View
bbgmiss %>% View
bbgmiss %>% tabulate(.,'issname') %>% View
bbgmiss %>% write.csv(.,file='GetBBGTickerPrice.csv')

#######
df_bond %>% xtabs(~ccy+mdealtype,data=.)
df_bond %>% tabulate(.,'isin')
Hmisc::describe(df_sdcnew$mktplace)

# out of the ones that are matched, what's the data coverage?
# very good price coverage

df_bond %>% mutate(datestr=as.character(date),matstr=as.character(mat2)) %>% select(-date,-mat2,-fac) %>% 
  write.dta(.,'bondsprdpanel.dta')






# explore daily data ------------------------------------------------------

# df_p_daily<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_temp_daily.RData')
# save(df_p_daily,file='dailyprices.rdata')
load('dailyprices.rdata')
df_p_daily %<>% mutate(dofm=lubridate::mday(date)) 
aa<-df_p_daily %>% group_by((dofm)) %>% summarise(ct=length(na.omit(YLD_YTM_MID))) %>% ggplot(aa,aes(x=`(dofm)`,y=ct))+geom_line()
oas<-df_p_daily %>% group_by(dofm) %>% summarise(oas_dom=median(na.omit(OAS_SPREAD_BID)),
                                                 yld_dom=median(na.omit(YLD_YTM_MID)))
oas %>% ggplot(aes(x=dofm,y=yld_dom))+geom_line()
oas %>% ggplot(aes(x=dofm,y=oas_dom))+geom_line()


# assess data availability ------------------------------------------------
df_pasw_mo<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch2_asw.RData')
load(file='sdc_all.rdata')
df_sdc_all %<>% distinct(isin)
countdups(df_sdc_all %>% filter(!is.na(pk)),'pk')

ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_pasw_mo,field='ASSET_SWAP_SPD_MID')

df_p<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch1.RData')
df_p %>% View
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p)
df_p
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p,field='OAS_SPREAD_BID')
ac %>% filter(pk=='EF600203 Corp') %>% View

df_p<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160426_mo_batch3_HY.RData')
df_p
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p)
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_p,field='OAS_SPREAD_BID')

bondprices %>% filter_('YLD_YTM_MID'!="NA") %>% filter(ticker=='EF600203 Corp') %>% View
bondprices %>% filter(YLD_YTM_MID!="NA") %>% filter(ticker=='EF600203 Corp') %>% View

bondprices[bondprices['YLD_YTM_MID']!="NA",] %>% filter(ticker=='EF600203 Corp') %>% View
bondprices[bondprices[field]!="NA",] %>% filter(ticker=='EF600203 Corp')

df_p_daily %>% filter(ticker=='EF600203 Corp') %>% View

df_yld %>% ds
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='BLP_ASW_SPREAD_MID')
ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='BLP_Z_SPRD_MID')

ac
ac %>% View

df_yld
df_yld<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')


ac<-assessDataCoverage(bondinfo = df_sdc_all,bondprices = df_yld,field='OAS_SPREAD_BID')

df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
ab<-df_yld2 %>% distinct(isin) %>% semi_join(df_sdc_all,by='isin')

### why are these two a little different????
nrow(ac)
nrow(ab)
ab %>% anti_join(ac,by='isin') %>% View


# see which fields to use based on sample data -------------------------------------------------
# concluded that oas and yld are the ones to use, other flds do not start till 2010
df_pt<-loadBBGdownload2df('../data/bloomberg/sample_flds_test.rdata')
load('sdc_all.rdata')
# df_sampleticker<-df_pt %>% group_by(ticker) %>% rename(pk=ticker) %>% summarise(ct=length(date)) %>% left_join(df_sdc_all %>% distinct(pk))
# df_sampleticker %>% filter(ccy=='EUR') %>% select(isin) %>% write.csv(file='sample_eur_isin.csv')
df_pt %<>% filter(date>='2005-01-01',date<='2016-04-01')

df_pt<-df_p %>% filter(date>='2005-01-01',date<='2016-04-01')
df_pt %>% ds
ac<-df_pt %>% filter(date>='2005-01-01',date<='2016-04-01') %>%
  assessDataCoverage(bondinfo=df_sdc_bbg,bondprices=.,field='OAS_SPREAD_BID')

ac %>% mutate(allflddiff=expmonthlyobs-obs_allflds) %>% filter(allflddiff %ni% c(-1,0,1)) %>%  
  left_join(df_sdc_bbg) %>% arrange(allflddiff) %>% select(figi,name,ticker,i,d,mat2,everything()) %>% 
  write.csv(file='temp.csv')

# df_obs %>% arrange(desc(obscoverage)) %>% View
# bondprices %>% filter(pk=='EH728416 Corp') %>% View
# df_sdc_all %>% filter(pk=='EH728416 Corp') %>% write.csv(file='temp.csv')
# df_sdc_bbg %>% filter(isin=='FR0010359810') %>% select(figi,name,ticker,i,d,mat2,everything()) %>% View


df_yld_long<-df_p %>%  melt(.,id.vars=c('date','pk'),
                            measure.vars=((df_pt %>% ds)[(df_pt %>% ds) %ni% c('date','pk','batch')]),
                            variable.name='field') %>% dplyr::tbl_df() %>% filter(!is.na(value)) %>% distinct(date,pk,field) 
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>%  ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

df_yld_long %>% showdups(c('date','pk','field'))
df_yld_long %>% distinct(pk) %>% nrow


# load bbg bond sprd, unpack ----------------------------------------------
df_yld<-loadBBGdownload2df('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')
load('../data/bloomberg/bbg_gbonds_160413_sprd.rdata')

# df_yld_long<-df_yld %>% select(-BLP_CDS_BASIS_MID) %>%  gather(key = 'field',value='value',-date,-ticker) %>% dplyr::tbl_df()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View

# add ccy
df_yld2<-left_join(df_yld,globalbonds,by='ticker') %>% tbl_df()
# df_yld_long<-df_yld2 %>% select(-BLP_CDS_BASIS_MID) %>%  
#   melt(.,id.vars=c('date','ticker','crncy'),
#        measure.vars=c("OAS_SPREAD_BID","BLP_Z_SPRD_MID","BLP_ASW_SPREAD_MID","BLP_I_SPRD_MID","BLP_Z_SPRD_LAST","BLP_ASW_SPREAD_LAST","BLP_I_SPRD_LAST"),
#        variable.name='field') %>% 
#   dplyr::tbl_df()
# #plot available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% 
#   ggplot(.,aes(x=date,y=Ndp,colour=field))+geom_line()

#View available data pionts for US and EU
# df_yld_long %>% dplyr::filter(crncy=='USD') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# df_yld_long %>% dplyr::filter(crncy=='EUR') %>% group_by(date, field) %>% summarise(Ndp=length(na.omit(value))) %>% dcast(date~field) %>% View
# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(OAS_SPREAD_BID))) 
# plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()

# agg_yld<-df_yld2 %>% group_by(date,crncy) %>%   summarise(yield=median(na.omit(BLP_I_SPRD_MID))) 
# # plot yields for eur, usd
# agg_yld %>% ggplot(.,aes(x=date,y=yield,colour=crncy)) +geom_line()
# # plot EUR-USD agg yield spread
# agg_yld %>% dcast(.,date~crncy,value.var="yield") %>% mutate(dif_crd=EUR-USD) %>%
#   gather(.,key='type',value='yield',-date) %>% 
#   filter(type=='dif_crd') %>% ggplot(.,aes(x=date,y=yield,colour=type))+geom_line()


