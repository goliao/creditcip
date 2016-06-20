# THIS FILE MAKES THE PLOT FOR ISSUANCE AND CREDIT SPREAD, BUT LATER ON I OPTED TO USE EXCEL INSTEAD
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
rm(list=ls(all=TRUE));
#load('gldb.RDATA')
source('util.r')


dt<-read.dta13('plotdata_qe_crd_iss.dta') %>% as.data.table()
dt[,date:=ymd(str_c(year,'-',month,'-','01'))]
dtp<-dt[date>'2004-01-01' & date<'2016-01-01',.(date,year,month,i_net_USDEUR,ccyeur_eff_6m,i_net_USDEUR_6m,ccyeur_eff)] 
dtp[,qrt:=ceiling(month/3)]
dtp[,minq:=month-(qrt-1)*3]
dtp[,i_qrt_mean:=mean(i_net_USDEUR),by=.(year,qrt)]
dtp[,i_crd_mean:=mean(ccyeur_eff),by=.(year,qrt)]

dtp %>% View
dtp %>% ggplotw()

fig1<-dtp %>% ggplot(aes(x=date,y=i_net_USDEUR_6m))+geom_bar(stat='identity',position='identity')
fig1+ geom_line(data=dtp,aes(x=date,y=ccyeur_eff_6m))



fig1<-dtp %>% ggplot(aes(x=date,y=i_net_USDEURqrt))+geom_bar(stat='identity',position='identity')
fig1+ geom_line(data=dtp,aes(x=date,y=ccyeur_eff_6m))


dtp[,.(date,i_net_USDEUR,i_net_USDEURqrt,ccyeur_eff_6m)] %>% View


### new
fig1<-dtp[minq==1] %>% ggplot(aes(x=date,y=i_qrt_mean))+geom_bar(stat='identity',position='identity');fig1
fig2<-dtp[minq==1] %>% ggplot(aes(x=date,y=ccyeur_eff_6m))+geom_line();fig2



fig1
fig2

library(gtable)
library(grid)

grid.newpage()

# two plots
p1 <- fig1+theme_bw()
p2 <- fig2 + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)


dtp[minq==1] %>% write.csv('crd_iss.csv')
dtp %>% write.csv('crd_iss_mo.csv')
