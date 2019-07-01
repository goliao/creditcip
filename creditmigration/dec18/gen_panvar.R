params=data.frame(figfile='../../paper/figures/161031/')



plot.irf.panel<-function(fname=str_c(params$figfile,'panelvar.dta'),v1='credit',v2='cip',v3='i_netflow',filename=''){
  require(latex2exp)
  labelv1<-'$\\kappa$'
  labelv2<-'$b$'
  labelv3<-'$\\mu$'
  pvar<-read.dta13(fname) %>% as.data.table()
  pvarl<-(pvar %>% melt.data.table(id.vars='step'))[variable %nlk% 'FEVD']
  pvarl[variable %like% '^p',prefix:='p']
  pvarl[variable %like% '^n',prefix:='n']
  pvarl[variable %like% 'll',postfix:='ll']
  pvarl[variable %like% 'ul',postfix:='ul']
  pvarl[,series:=as.numeric(str_extract_all(variable,'\\d+'))]
  pvarl[is.na(prefix),prefix:='est']
  pvarl[is.na(postfix),postfix:='est']
  pvar2<-(pvarl[prefix %in% c('est','n')] %>% dcast(step+series~postfix))[order(series,step)]
  par(ask=F)  
  require('gridExtra')
  figirf<-list()
  figirf <- list()
  # figirf[[length(figirf)+1]]<-pvar2[series==1] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv3)))
  # figirf[[length(figirf)+1]]<-pvar2[series==2] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv1)))
  # figirf[[length(figirf)+1]]<-pvar2[series==3] %>% ggplot(aes(step,est))+geom_line()+geom_ribbon(aes(ymin=ll,ymax=ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3,' $\\rightarrow$ ',labelv2)))
  figirf[[length(figirf)+1]]<-pvar2[series==5] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv1)))+
    scale_x_continuous(breaks=c(0,3,6,9))
  figirf[[length(figirf)+1]]<-pvar2[series==6] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv2)))+    scale_x_continuous(breaks=c(0,3,6,9))

  figirf[[length(figirf)+1]]<-pvar2[series==4] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3)))+    scale_x_continuous(breaks=c(0,3,6,9))

  figirf[[length(figirf)+1]]<-pvar2[series==8] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv1)))+    scale_x_continuous(breaks=c(0,3,6,9))

  figirf[[length(figirf)+1]]<-pvar2[series==9] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv2)))+    scale_x_continuous(breaks=c(0,3,6,9))

  figirf[[length(figirf)+1]]<-pvar2[series==7] %>% ggplot(aes(step,-est))+geom_line()+geom_ribbon(aes(ymin=-ll,ymax=-ul),alpha=.3,colour=NA)+xlab('')+ylab('')+theme_few()+geom_hline(yintercept=0,colour='lightblue')+ggtitle(TeX(str_c(labelv3)))+    scale_x_continuous(breaks=c(0,3,6,9))

  figirf.all<-grid.arrange(figirf[[1]],figirf[[3]],figirf[[2]],figirf[[5]],figirf[[6]],figirf[[4]],ncol=3,nrow=2,layout_matrix=rbind(c(1,2,3),c(4,5,6)),heights=c(2.25,2.25))
  figirf.top<-grid.arrange(figirf[[1]],figirf[[3]],figirf[[2]],ncol=3,nrow=1,layout_matrix=rbind(c(1,2,3)),heights=c(2.25))
  figirf.bottom<-grid.arrange(figirf[[5]],figirf[[6]],figirf[[4]],ncol=3,nrow=1,layout_matrix=rbind(c(1,2,3)),heights=c(2.25))
  
  if(filename!='') {
    ggsave(file=filename,figirf.all,width=8,height=5)
    ggsave(file='../../figures/panelvartop.pdf',figirf.top,width=6,height=2.0)
    ggsave(file='../../figures/panelvarbottom.pdf',figirf.bottom,width=6,height=2.0)
  }
  figirf.all
} 


# Panel VAR with all 6 ccys
plot.irf.panel(fname=str_c('../../paper/figures/161019/panelvar.dta'),filename = str_c('../../','panelvar.pdf'))



##################################
################# 



plot.irf.single<-function(resirf,v1='netmsip',v2='i_netflow'){
  require(latex2exp)
  labelv1<-'$(\\kappa-b)$'
  labelv2<-'$\\mu$'
  dttemp<-list()
  dttemp[[1]]<-(resirf$irf %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="irf"]
  dttemp[[2]]<-(resirf$Upper %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Upper"]
  dttemp[[3]]<-(resirf$Lower %>% as.data.frame(keep.rownames=T) %>% as.data.table(keep.rownames=T))[,type:="Lower"]
  dtirf<-(dttemp %>% rbindlist %>% melt(id.vars=c('rn','type')) %>% dcast(rn+variable~type))[,rn:=as.numeric(rn)][order(rn)]  
  par(ask=F)      
  outplot<-dtirf %>% ggplot(data=.,aes(x=rn,y=irf,group=1))+geom_line()+geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.3,colour=NA)+ggtitle(TeX(str_c(labelv1,' $\\rightarrow$ ',labelv2)))+xlab('months')+ylab('')+geom_hline(yintercept=0,colour='lightblue')+theme_few()+theme(legend.position='none')
  outplot
} 



#+ eval=params$run.var
dtvar<-dtreg.m[date<'2016-09-01'][ccy=='eur'][,.(i_netflow,netmisp)]
res1<-vars::VAR(dtvar,1)
resirf<-vars::irf(res1,ci=.95,impulse='netmisp',response='i_netflow') 
resirf %>% plot.irf.single() %>%  ggsave(file=str_c('../../figures/oirf_inetdev.pdf'),.,width=5,height=3)
