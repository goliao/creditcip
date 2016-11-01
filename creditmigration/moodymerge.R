# this is a testing pad, it is eventually incorported in gen_bondref.R
require('RSQLite')
source('util.r')
con <- dbConnect(drv=SQLite(), dbname="../data/Moody/moody.sqlite")
issu<-dbReadTable(con,'MAST_ISSU') %>% as.data.table()
ids<-dbReadTable(con,'DEBT_IDS') %>% as.data.table()
lookup<-dbReadTable(con,'LOOKUP') %>% as.data.table()
#dtout<-dbGetQuery(con,sql) %>% as.data.table()
dbDisconnect(con)

issu %>% setkey(MAST_ISSU_NUM)
ids %>% setkey(MAST_ISSU_NUM)

# ids[ID_NUM_CD=='ISI'] %>% showdups(c('ID_NUM'))
# ids[ID_NUM_CD=='CUS'] %>% showdups(c('ID_NUM'))

mdyiss<-ids[ID_NUM_CD=='ISI',.(MAST_ISSU_NUM,isin=ID_NUM)][issu]
mdyiss<-ids[ID_NUM_CD=='CUS',.(MAST_ISSU_NUM,cusip=ID_NUM)][mdyiss]
mdyiss<-mdyiss[!is.na(isin) | !is.na(cusip)]
mdyiss[,cu8:=str_sub(cusip,1,8)]
bondref[,cu8:=str_sub(cusip9,1,8)]
bondref %>% setkey(isin)
mdyiss %>% setkey(isin)
br2<-update.dt(bondref, unique(mdyiss[!is.na(isin)]), 'isin',insertnewrow = F)
br3<-update.dt(br2,unique(mdyiss[!is.na(cu8)]),'cu8',override = T,insertnewrow = F)
bondref<-br3

aa<-mdyiss %>% anti_join(bondref,'isin') %>% as.data.table()
merge(aa[FACE_US_AMNT>=100][DEBT_CLASS_CD=='REG'][,.N,DEBT_TYP_CD][order(-N)],lookup[FIELD_NAM %like% 'debt_typ'],by.x = 'DEBT_TYP_CD',by.y='FIELD_CD')[order(-N)] %>% head(50)
mdyiss

merge(mdyiss[FACE_US_AMNT>=100][DEBT_CLASS_CD=='REG'][,.N,DEBT_TYP_CD][order(-N)],lookup[FIELD_NAM %like% 'debt_typ'],by.x = 'DEBT_TYP_CD',by.y='FIELD_CD')[order(-N)] %>% head(50)


# br3
# 
# 
# (mdyiss[!is.na(cu8)][is.na(isin)] %>% semi_join(mdyiss,'cu8') %>% as.data.table)[,.N]
# 
# 
# 
# br2[,.N,DEBT_SENR_CD]
# br3[,.N,DEBT_SENR_CD]
# 
# bondref[,.N]
# br2[,.N]
# br3[,.N]
# 
# 
# 
# bondref[is.na(isin) & is.na(cu),.N]
# 
# issu[,.N,isin]
# bondref[,.N,isin]
# 
# bondref %>% setkey(pk)
# bondrefall %>% setkey(pk)
# dtl.mo  %>% setkey(pk)
# 
# dtl.mo[!bondref][,.N,pk][,.N]
# 
# dtl.mo[,.N,pk] %>% dt2clip()
# 
# issu
# 
# 
# 
# 
# issu[is.na(isin)][,.N]
# issu[is.na(cusip)][,.N]
# 
# 
# issu[is.na(cusip) & is.na(isin)][,.N]
# 
# idsl <- ids %>% dcast(MAST_ISSU_NUM~ID_NUM_CD)
# idsl[is.na(CUS) & is.na(ISI),.N]
# idsl[is.na(ISI),.N]
# 
# 
# 
# 
# 
# issu[,.N,MAST_ISSU_NUM]
# ids[,.N,MAST_ISSU_NUM]
# 
# issu[!ids,.N,DEBT_CLASS_CD][order(-N)]
# 
# ids[,.N,ID_NUM_CD]
# 
# 
# compare.dt(issu,ids,bykey. = 'MAST_ISSU_NUM')
# 
# dtout[,.N,DEBT_SENR_CD][order(-N)]
# dtout[MKT_TYP_CD=='EUR',.N,DEBT_SENR_CD][order(-N)]
# 
# dtout[,.N,DEBT_CLASS_CD][order(-N)]
# lookup[FIELD_NAM %like% 'currency'] %>% dt2clip()
 lookup[FIELD_NAM %like% 'debt_typ'] %>% dt2clip()
