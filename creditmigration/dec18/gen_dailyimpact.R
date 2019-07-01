setwd("C:/Users/Gordon/Dropbox/Research/ccy basis/creditmigration")
## Proper event study


rm(list=ls(all=TRUE));load('db/dtldaily.RData');#load('db/bondref.RData')
#load('db/prl.RData');load('db/monthenddates.RData');
#devtools::install_github("nipfpmf/eventstudies", ref="master")
require(eventstudies)
load('dec18/img_calc.RData')
source('util.r')

dtin2<-dtissraw %>% add.earlist.iss.in.ccy(.)
dtin2 %>% setkey(upcusip,ccy)


dtiss.collapse.d <- foreach(iccy=c('eur','gbp','jpy','aud','chf','cad')) %do% {
  dtin2 %>% icollapse4(.,iccy,collapse.freq = 'day',filter=0)
} %>% rbindlist(use.names = T) %>% setkey(date,ccy)


prldaily<-haven::read_stata('bbg_prices_daily_clean.dta') %>% as.data.table()

prldaily[,date:=ymd(datestr)]
prldaily<-prldaily[date>=ymd('2004-01-01')]


prld<-prldaily[,.(date,eubs5,bpbs5,jybs5,adbs5,sfbs5,cdbs5)] %>% melt(id.vars='date',variable.name='ticker',value.name='cip') %>% na.omit()

ticccy<-data.table(ticker=c('eubs5','bpbs5','jybs5','adbs5','sfbs5','cdbs5'),ccy=c('eur','gbp','jpy','aud','chf','cad'))

prld<-merge(prld,ticccy,by='ticker')


dtreg0<-merge(dtiss.collapse.d,prld,by=c('date','ccy'))
spotccy=0
if (spotccy==1){
  spot<-prldaily[,.(date,eur=1/eur,gbp=1/gbp,aud=1/aud,nzd=1/nzd,cad,jpy,chf)] %>% melt(id.vars='date',variable.name='ccy',value.name='spot') %>% na.omit()
  dtreg<-merge(dtiss.collapse.d,spot,by=c('date','ccy'))
  
  dtreg[,spotret:=log(spot/shift(spot,n=1,type='lag')),ccy]
  tmp<-dtreg %>% dcast(date~ccy,value.var='spotret')
} else{
###############################
  dtreg<-dtreg0[date>ymd('2009-01-01')] %>% copy()
  dtreg[,cipret:=cip-shift(cip,n=1,type='lag'),ccy]
  tmp<-dtreg %>% dcast(date~ccy,value.var='cipret')
}
zooret<- tmp[,names(tmp)[names(tmp)!='date'],with=F] %>% zoo(.,order.by=tmp$date) 


################################

# Large flow into US
# Large flow out of US

threshold=.5;window=15;desc='fUSDusF'

dtreg[,fUSDevent:=0];dtreg[I_fUSD>=threshold & I_fUSD<99,fUSDevent:=1]
dtreg[,usFevent:=0];dtreg[I_usF>=threshold & I_usF<99,usFevent:=1]
dtreg[,noevent:=0];dtreg[fUSDevent==0 & usFevent==0,noevent:=1]

# Quantilethreshold=.97
# if (Quantilethreshold) {
#   'quantile threshold'
#   thres<-dtreg[I_fUSD!=0,.(th=quantile(.SD[,I_fUSD],probs=c(Quantilethreshold),na.rm=T),th2=quantile(.SD[,I_usF],probs=c(Quantilethreshold),na.rm=T)),ccy];
#   sprintf('Using quantile threshold at %s for large Yankee issuance in $bio:',Quantilethreshold)
#   print(thres)
#   dtreg<-merge(dtreg,thres,by='ccy')
#   dtreg[,fUSDevent:=0];dtreg[I_fUSD>th & I_fUSD<5,fUSDevent:=1]
#   dtreg[,usFevent:=0];dtreg[I_usF>th2 & I_usF<5,usFevent:=1]  
# }

dtreg[,.N,.(fUSDevent,usFevent,noevent,ccy)][order(ccy)]
dtreg[,.N,.(fUSDevent,usFevent,noevent)]
dtreg[,.N]


eventsflow_fUSD<-dtreg[fUSDevent==1 & usFevent!=1,.(name=as.character(ccy),when=date)] %>% as.data.frame()
eventsflow_usF<-dtreg[usFevent==1 & fUSDevent!=1,.(name=as.character(ccy),when=date)] %>% as.data.frame()
eventsflow_no<-dtreg[noevent==1,.(name=as.character(ccy),when=date)] %>% as.data.frame()

esfUSD<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_fUSD,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
esusF<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_usF,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
#es0<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_no,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )

plot(es0)
plot(esfUSD)
plot(esusF)
(zooret %>% na.omit() %>% mean())*window

esflowus<-list(desc=desc,theshold=threshold,window=window,esfUSD=esfUSD,esusF=esusF)

fUSD<-data.table(esflowus$esfUSD$result)
usF<-data.table(esflowus$esusF$result)

fUSD[,event:='fUSD']
usF[,event:='usF']

fUSD[,time:=index(fUSD)-median(index(fUSD)+.5)]
usF[,time:=index(usF)-median(index(usF)+.5)]

eslong<-rbind(fUSD,usF) %>% melt(id.vars=c('time','event'))
eslong[variable=='2.5%',variable:='lowereb']
eslong[variable=='97.5%',variable:='upppereb']
eslong[,var:=str_c(event,variable)]

Lcolor=c('red','red','red','blue','blue','blue');Ltype=c('dashed','solid','dashed','dashed','solid','dashed')
Labels=c('Yankee issue 97.5%','Yankee issue mean', 'Yankee issue 2.5%','Reverse Yankee issue 97.5%','Reverse Yankee issue mean','Reverse Yankee issue 2.5%')
eslong[,.(time,var,value)] %>% ggplot(aes(x=time,y=value))+geom_line(aes(linetype=var,colour=var))+theme_few()+scale_linetype_manual('',labels =Labels,values=Ltype)+theme(legend.position = 'none')+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+xlab('Days from issuance date')+ylab('Change in FX basis (basis points)')+geom_point(aes(shape=var,size=var,colour=var))+ scale_shape_manual(values=c(0,1,0,0,1,0)) +scale_size_manual(values=c(0,3,0,0,3,0)) +scale_color_manual('',labels =Labels,values=Lcolor) + annotate('text',size=5,x=8.5,y=1.25,label='Yankee bond issuance',colour='red')+ annotate('text',size=5,x=8,y=-1.3,label='Reverse Yankee bond issuance',colour='blue')+annotate('text',size=4,x=-11,y=1.7,label='Increase in expense to \n swap to foreign currency')+annotate('text',size=4,x=-12,y=-1.7,label='Increase in expense\n to swap to USD')
beepr::beep()



#ggsave('../figures/dailyissuanceimpact.pdf',height=5,width=7)
tmp<-list()
for (i in c('aud','eur','gbp','jpy','chf','cad')){
  tmp[[length(tmp)+1]]<-data.table(ccy=i,yankee=dtreg[ccy==i & fUSDevent==1,.N]/dtreg[ccy==i,.N],reverseyankee=dtreg[ccy==i & usFevent==1,.N]/dtreg[ccy==i,.N])
}
rbindlist(tmp) %>% summary()




##############
###credit impact### obviously this does not work given the large standard error estimated on the credit spreads
############
load('dailyregrun.RData')
source('util.r')
ys1<-resyldsprdv4(dtmd$dtl4,dtmd$prl,regversion=4,returndt = T)

dt.mispl<-create.dev.long(dtmd$prw,ys1$regresult)
dt.mispl[,.N,ccy]

dtreg0<-merge(dtiss.collapse.d,dt.mispl,by=c('date','ccy'),all.y = T)

dtreg<-dtreg0[date>ymd('2004-01-01')] %>% copy()

dtreg[,cipret:=cip-shift(cip,n=1,type='lag'),ccy]
dtreg[,creditret:=credit-shift(credit,n=1,type='lag'),ccy]
dtreg[order(ccy)]
tmp<-dtreg %>% dcast(date~ccy,value.var='creditret')
zooret<- tmp[,names(tmp)[names(tmp)!='date'],with=F] %>% zoo(.,order.by=tmp$date) 


######

# Large flow into US
# Large flow out of US

threshold=.25;window=14;desc='fUSDusF'

dtreg[,fUSDevent:=0];dtreg[I_fUSD>=threshold,fUSDevent:=1]
dtreg[,usFevent:=0];dtreg[I_usF>=threshold,usFevent:=1]
dtreg[,noevent:=0];dtreg[fUSDevent==0 & usFevent==0,noevent:=1]

dtreg[,.N,.(fUSDevent,usFevent,noevent,ccy)][order(ccy)]
dtreg[,.N,.(fUSDevent,usFevent,noevent)]

eventsflow_fUSD<-dtreg[fUSDevent==1 & usFevent==0,.(name=as.character(ccy),when=date)] %>% as.data.frame()
eventsflow_usF<-dtreg[usFevent==1 & fUSDevent==0,.(name=as.character(ccy),when=date)] %>% as.data.frame()
eventsflow_no<-dtreg[noevent==1,.(name=as.character(ccy),when=date)] %>% as.data.frame()

esfUSD<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_fUSD,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
esusF<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_usF,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
es0<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsflow_no,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )

plot(es0)
plot(esfUSD)
plot(esusF)

esflowus<-list(desc=desc,theshold=threshold,window=window,esfUSD=esfUSD,esusF=esusF,es0=es0)
# plot
fUSD<-data.table(esflowus$esfUSD$result)
usF<-data.table(esflowus$esusF$result)

fUSD[,event:='fUSD']
usF[,event:='usF']

fUSD[,time:=index(fUSD)-median(index(fUSD)+.5)]
usF[,time:=index(usF)-median(index(usF)+.5)]

eslong<-rbind(fUSD,usF) %>% melt(id.vars=c('time','event'))
eslong[variable=='2.5%',variable:='lowereb']
eslong[variable=='97.5%',variable:='upppereb']
eslong[,var:=str_c(event,variable)]

Lcolor=c('red','red','red','blue','blue','blue');Ltype=c('dashed','solid','dashed','dashed','solid','dashed')
Labels=c('Yankee issue 97.5%','Yankee issue mean', 'Yankee issue 2.5%','Reverse Yankee issue 97.5%','Reverse Yankee issue mean','Reverse Yankee issue 2.5%')
eslong[,.(time,var,value)] %>% ggplot(aes(x=time,y=value))+geom_line(aes(linetype=var,colour=var))+theme_few()+scale_linetype_manual('',labels =Labels,values=Ltype)+theme(legend.position = 'none')+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+xlab('Days from announced issuance date')+ylab('Change in FX basis (basis points)')+geom_point(aes(shape=var,size=var,colour=var))+ scale_shape_manual(values=c(0,1,0,0,1,0)) +scale_size_manual(values=c(0,3,0,0,3,0)) +scale_color_manual('',labels =Labels,values=Lcolor) + annotate('text',size=5,x=8.5,y=1.25,label='Yankee bond issuance',colour='red')+ annotate('text',size=5,x=8,y=-1.3,label='Reverse Yankee bond issuance',colour='blue')+annotate('text',size=4,x=-11,y=1.7,label='Increase in expense to \n swap to foreign currency')+annotate('text',size=4,x=-12,y=-1.7,label='Increase in expense\n to swap to USD')

beepr::beep()

##Credit## 
# #################################################
# # large positive vs negative 
# ###
# threshold=.25;window=14
# 
# dtreg[,posevent:=0];dtreg[I_netflow>=threshold,posevent:=1]
# dtreg[,negevent:=0];dtreg[I_netflow<=-threshold,negevent:=1]
# dtreg[,noevent:=0];dtreg[posevent==0 & negevent==0,noevent:=1]
# 
# dtreg[,.N,.(posevent,negevent,noevent,ccy)][order(ccy)]
# 
# eventsnetflow_pos<-dtreg[posevent==1,.(name=ccy,when=date)] %>% as.data.frame()
# eventsnetflow_neg<-dtreg[negevent==1,.(name=ccy,when=date)] %>% as.data.frame()
# eventsnetflow_no<-dtreg[noevent==1,.(name=ccy,when=date)] %>% as.data.frame()
# 
# esp<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsnetflow_pos,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
# esn<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsnetflow_neg,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
# es0<-eventstudies::eventstudy(firm.returns =zooret ,event.list =eventsnetflow_no,event.window=window,type='None',to.remap = TRUE, remap='cumsum',inference=TRUE,inference.strategy = "bootstrap" )
# 
# 
# plot(esp)
# plot(esn)
# plot(es0)
# 
# #es3<-list(theshold=threshold,window=window,esp=esp,esn=esn,es0=es0)
# #es1<-list(theshold=.1,window=30,esp=esp,esn=esn,es0=es0)
# 
# 
# plot(es1$esp)
# plot(es2$esp)
# plot(es3$esp)
# 
# plot(es1$esn)
# plot(es2$esn)
# plot(es3$esn)
# 
# es2$es0$result
# dtreg[,cipret] %>% summary()
# -0.009825*30
# ############################
# 
# 
# 
# dtreg0[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
# dtreg0[,LD.cip:=shift(D.cip,n=1,type='lag'),.(ccy)]
# dtreg0[,L2D.cip:=shift(D.cip,n=2,type='lag'),.(ccy)]
# dtreg0[,L3D.cip:=shift(D.cip,n=3,type='lag'),.(ccy)]
# dtreg0[,L4D.cip:=shift(D.cip,n=4,type='lag'),.(ccy)]
# dtreg0[,L5D.cip:=shift(D.cip,n=5,type='lag'),.(ccy)]
# dtreg0[,L6D.cip:=shift(D.cip,n=6,type='lag'),.(ccy)]
# dtreg0[,L7D.cip:=shift(D.cip,n=7,type='lag'),.(ccy)]
# 
# dtreg0[,FD.cip:=shift(D.cip,n=2,type='lead'),.(ccy)]
# dtreg0[,F2D.cip:=shift(D.cip,n=2,type='lead'),.(ccy)]
# dtreg0[,F3D.cip:=shift(D.cip,n=3,type='lead'),.(ccy)]
# dtreg0[,F4D.cip:=shift(D.cip,n=4,type='lead'),.(ccy)]
# dtreg0[,F5D.cip:=shift(D.cip,n=5,type='lead'),.(ccy)]
# dtreg0[,F6D.cip:=shift(D.cip,n=6,type='lead'),.(ccy)]
# dtreg0[,F7D.cip:=shift(D.cip,n=7,type='lead'),.(ccy)]
# 
# extractregcoef<-function(reg,coefname){
#   as.data.table(coef(summary(reg)),keep.rownames = TRUE)[rn==coefname][,.(coef=Estimate, se=`Std. Error`)]
# }
# 
# 
# dtreg<-dtreg0[abs(I_netflow)>.1 & ccy %in% c('eur','jpy','gbp','chf')]
# tmp=list()
# tmp[['L7D']]=dtreg %>% felm(L7D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L6D']]=dtreg %>% felm(L6D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L5D']]=dtreg %>% felm(L5D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L4D']]=dtreg %>% felm(L4D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L3D']]=dtreg %>% felm(L3D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L2D']]=dtreg %>% felm(L2D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['LD']]=dtreg %>% felm(LD.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['D']]=dtreg %>% felm(D.cip~I_netflow|0|0|0,data = .) %>% extractregcoef('I_netflow')
# tmp[['FD']]=dtreg %>% felm(FD.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F2D']]=dtreg %>% felm(F2D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F3D']]=dtreg %>% felm(F3D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F4D']]=dtreg %>% felm(F4D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F5D']]=dtreg %>% felm(F5D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F6D']]=dtreg %>% felm(F6D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F7D']]=dtreg %>% felm(F7D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# 
# coefs1<-rbindlist(tmp,idcol = TRUE)
# 
# coefs1[,tvalue:=coef/se]
# 
# coefs1
# 
# dtreg$I_netflow %>% sd
# 
# 
# ###########
# ## cumulative
# ############
# dtreg0[,D.cip:=cip-shift(cip,n=1,type='lag'),.(ccy)]
# dtreg0[,LD.cip:=cip-shift(cip,n=2,type='lag'),.(ccy)]
# dtreg0[,L2D.cip:=cip-shift(cip,n=2,type='lag'),.(ccy)]
# dtreg0[,L3D.cip:=cip-shift(cip,n=3,type='lag'),.(ccy)]
# dtreg0[,L4D.cip:=cip-shift(cip,n=4,type='lag'),.(ccy)]
# dtreg0[,L5D.cip:=cip-shift(cip,n=5,type='lag'),.(ccy)]
# dtreg0[,L6D.cip:=cip-shift(cip,n=6,type='lag'),.(ccy)]
# dtreg0[,L7D.cip:=cip-shift(cip,n=7,type='lag'),.(ccy)]
# dtreg0[,FD.cip:=cip-shift(cip,n=1,type='lead'),.(ccy)]
# dtreg0[,F1D.cip:=cip-shift(cip,n=2,type='lead'),.(ccy)]
# dtreg0[,F2D.cip:=cip-shift(cip,n=3,type='lead'),.(ccy)]
# dtreg0[,F3D.cip:=cip-shift(cip,n=4,type='lead'),.(ccy)]
# dtreg0[,F4D.cip:=cip-shift(cip,n=5,type='lead'),.(ccy)]
# dtreg0[,F5D.cip:=cip-shift(cip,n=6,type='lead'),.(ccy)]
# dtreg0[,F6D.cip:=cip-shift(cip,n=7,type='lead'),.(ccy)]
# 
# 
# 
# dtreg<-dtreg0[abs(I_netflow)>.1 & ccy %in% c('eur','jpy','gbp','chf')]
# tmp=list()
# tmp[['L7D']]=dtreg %>% felm(L7D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L6D']]=dtreg %>% felm(L6D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L5D']]=dtreg %>% felm(L5D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L4D']]=dtreg %>% felm(L4D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L3D']]=dtreg %>% felm(L3D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['L2D']]=dtreg %>% felm(L2D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['LD']]=dtreg %>% felm(LD.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['D']]=dtreg %>% felm(D.cip~I_netflow|0|0|0,data = .) %>% extractregcoef('I_netflow')
# tmp[['FD']]=dtreg %>% felm(FD.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F2D']]=dtreg %>% felm(F2D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F3D']]=dtreg %>% felm(F3D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F4D']]=dtreg %>% felm(F4D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F5D']]=dtreg %>% felm(F5D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F6D']]=dtreg %>% felm(F6D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# tmp[['F7D']]=dtreg %>% felm(F7D.cip~I_netflow|0|0|0,data = .)  %>% extractregcoef('I_netflow')
# 
# coefs1<-rbindlist(tmp,idcol = TRUE)
# 
# coefs1[,tvalue:=coef/se]
# coefs1
