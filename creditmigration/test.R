#' ---
#' title: "Results"
#' date: "Oct 07,2016"
#' output: html_document
#' params:   
#'  run.var: FALSE, 
#'  run.stata: FALSE,
#'  show.credit.cip: TRUE  # graph credit cip, run regressoin
#'  individual.resid: FALSE # individually generated residualized spread or generated together
#'  sdc.filter: '6ccyv3' # set iss filter options: bondrefall, 6ccyv1 6ccyv2 # this is first step filter
#'  sdc.filter.var: 
#'    restrict.to.dtl: TRUE
#'    collapse.filter: 1 # issfiltertype:  # collapse filter = 0,1,2  for upcusips that might/mightnot issued in ccy before or after, isssued in 2nd ccy previously, or issued in 2nd ccy anytime before or after current issuance
#'  sdc.filter.iss.reg: 
#'    restrict.to.dtl: TRUE
#'    collapse.filter: 0
#'  sdc.filter.iv.reg: 
#'    restrict.to.dtl: TRUE
#'    collapse.filter: 1
#'  gov: 'wogov'
#'  firmlevel: 'upcusip'
#'  mcore:  1
#'  quickstart: FALSE # to use previously saved results in the begining
#'  figfile: '../figure/160913/'
#' ---
#' 
#' Options -----------------------------------------------------------------


#+ startup,include=FALSE
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
library(knitr)
opts_chunk$set(echo=TRUE,include=TRUE)
#'
zz<-function(){print(params)}
zz()





#+ echo=FALSE,eval=FALSE
#include_graphics('../paper/figures/VAR_irfeur_A.png')
#knitr::stitch_rmd('test.R')
#rmarkdown::render('test.R',clean=F,params=list(testp=3))

require(yaml)
ystr<-'params:  
          run.var: FALSE
            a:1
            b:2'
aa<-yaml.load(ystr)
aa
