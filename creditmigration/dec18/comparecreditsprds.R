# setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")  
# source('util.r'); library(yaml)
# if (Sys.info()['sysname'][[1]]=='Windows') mcore=1 else mcore=4
# load('db/dtlmo.rdata');load('db/bondref.RData');load('db/prl.RData');load('db/monthenddates.RData');
# source('util.r')
# # preprocessing -----------------------------------------------------------
# dtm<-preprocess(bondref,dtl.mo,prl,issfiltertype =4)
# 
# bondref <- (bondref %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6]
# dtl<-(dtm$dtl4 %>% tocusip6(field=params$firmlevel))[,upcusip:=cusip6]
# dtl<-dtl[tf_mid_desc %nlk% 'Gov']  
# 
# ys1m<-resyldsprdv4(dtl,dtm$prl,regversion=4,returndt=T,parallel.core. = mcore)
# ys1meff<-resyldsprdv4(dtl,dtm$prl,regversion=4,adjccybs=1,returndt=T,parallel.core.=mcore)
# dtcreditcip<-create.dev.long2(prwin = dtm$prw,creditmispin = ys1m$regresult,netmispin = ys1meff$regresult)


aa<-haven::read_dta('../prices_extended.dta') %>% as.data.table
dtpr <- ((aa %>% as.data.table)[!is.na(date)] %>% melt.data.table(id.vars='date'))[!is.na(value)][,value:=as.numeric(value)]
# 
# dtpr[variable %like% 'Cdif_euus_00$|^Crd_diff_barc_zvs$$'] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
# 
# dtpr[variable %like% 'Cdif_euus_citi$|Cdif_euus_00$|^Crd_diff_barc_zvs$|Crd_diff_jpm_gov$'] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line()
# 
# # baml IG
# dtpr[variable %like% 'Cdif_euus_00$'] %>% dcast(date~variable) %>% ggplotw()
# dtpr[variable %like% 'barc',.N,variable] # single A z-spread
# dtpr[variable %like% 'jpm',.N,variable] # currently single A 7-10yr 

dtind<-dtpr[variable %like% 'Cdif_euus_00$|^Crd_diff_barc_zvs$$']

dtcalc<-ys1m$regcoef[,.(date,variable='rescred',value=eur)]

dtcomp <- rbind(dtind,dtcalc)

Labels=c('Barclays','BAML','Residualized'); Lcolor=c('blue','darkred','black'); Ltype=c('dashed','dotted','solid');
dtcomp[date>=ymd('2004-01-01') & date<=ymd('2016-01-01')] %>% ggplot(aes(x=date,y=value,colour=variable))+geom_line(aes(linetype=variable,colour=variable))+theme_few()+xlab('')+ylab('basis points')+scale_x_date(breaks=scales::pretty_breaks(n=7))+scale_y_continuous(breaks=scales::pretty_breaks(n=7))+scale_color_manual('',labels =Labels,values=Lcolor)+scale_linetype_manual('',labels =Labels,values=Ltype)
ggsave('../../figures/crdindcomp.pdf',width=6,height=4)
