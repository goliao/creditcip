

dt <- fread('dtccvolume2019test.csv',skip = 1) %>% setnames(c('REFERENCE ENTITY','NOTIONAL (USD EQ)','TRADES/DAY'),c('ent','notl','ntrades'))

require(stringr)

dt[,.N,.(notl)]

dt[,notl2:=as.numeric(stringr::str_replace_all(notl,',',''))]
dt[,.N,REGION]
dt

eurfirm <- dt[REGION %in% c('EUROPE')][order(-Dealers)] %>% head(140)
eurfirm[,.(mean(notl2),mean(Dealers),mean(ntrades),mean(DEALERSAVG))]
dt[ent %in% EU][,.(mean(notl2),mean(Dealers),mean(ntrades),mean(DEALERSAVG))]



jpfirm <- dt[REGION %in% c('JAPAN')][order(-Dealers)] %>% head(82)
jpfirm[,.(mean(notl2),mean(Dealers),mean(ntrades),mean(DEALERSAVG))]
dt[ent=='JAPAN'][,.(mean(notl2),mean(Dealers),mean(ntrades),mean(DEALERSAVG))]


,'SOVEREIGN'
dt[REGION %in% c('SOVEREIGN')][order(-Dealers)]



dt[REGION %in% c('EUROPE','JAPAN','SOVEREIGN'),.(mean(notl2)),.(sov=(REGION=='SOVEREIGN'))]
dt[REGION %in% c('EUROPE','JAPAN','SOVEREIGN'),.(mean(DEALERSAVG)),.(sov=(REGION=='SOVEREIGN'))]


EU <- c('KINGDOM OF DENMARK', 'FRENCH REPUBLIC', 'FEDERAL REPUBLIC OF GERMANY', 'KINGDOM OF BELGIUM', 'HELLENIC REPUBLIC', 'PORTUGUESE REPUBLIC', 'KINGDOM OF SPAIN', 'REPUBLIC OF ITALY','IRELAND','REPUBLIC OF SLOVENIA','KINGDOM OF THE NETHERLANDS','REPUBLIC OF AUSTRIA')

length(EU)
