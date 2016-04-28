rm(list=ls(all=TRUE))
library(foreign)
library(stringr)
library(xts)
library(tidyr)
library(dplyr)
require('readstata13')
require('ggfortify')

df2clip<-function(x)(write.table(x, "clipboard", sep="\t"))
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")

df1<-read.dta13('temp_regdata.dta')

# dflong<-tidyr::gather(df1,key='date')
# ggplot(dflong, aes(date, value)) + geom_line() +
#   scale_x_date(format = "%b-%Y") + xlab("") + ylab("issuance")


xt1<-as.xts(df1,order.by = df1$date)

pdf(file='graphs.pdf')

plot.xts(xt1$I_USDEUR_tot)
plot.xts(xt1$I_USDeu)
plot.xts(xt1$I_EURus)
plot.xts(xt1$I_net_USDEUR)
plot.xts(xt1$i_net_USDEUR)
plot.xts(xt1$i_USDeu)
plot.xts(xt1$i_EURus)

plot.xts(xt1$I_net_USDJPY)
plot.xts(xt1$i_net_USDJPY)
plot.xts(xt1$I_USDJPY_tot)
plot.xts(xt1$I_USDjp)
plot.xts(xt1$I_USD_tot)
plot.xts(xt1$i_USDjp)
plot.xts(xt1$I_JPYus)
plot.xts(xt1$I_JPY_tot)
plot.xts(xt1$i_JPYus)

plot.xts(xt1$I_net_USDGBP)
plot.xts(xt1$i_net_USDGBP)
plot.xts(xt1$i_USDgb)
plot.xts(xt1$i_GBPus)

plot.xts(xt1$I_net_USDAUD)
plot.xts(xt1$i_net_USDAUD)
plot.xts(xt1$i_USDau)
plot.xts(xt1$i_AUDus)
dev.off()

# require(dynlm)
# library(sandwich)
# 
# fm<-dynlm(eubs10~L(I_net_USDEUR,1),data=df1)
# summary(fm)
# bwNeweyWest(fm)
# 
# 


# lines(x=xt1$i_EURus,col='red',)





