#' ---
#' title: "Results"
#' date: "Oct 07,2016"
#' output: html_document

#' ---
#' Options -----------------------------------------------------------------

gl <- list('run.stata'=TRUE,
           'run.var'=FALSE,
           'show.credit.cip'=FALSE, # graph credit cip, run regressoin
           'individual.resid'=FALSE,# individually generated residualized spread or generated together
           'sdc.filter'='6ccyv3',# set iss filter options: bondrefall, 6ccyv1 6ccyv2
           'firmlevel'='upcusip',
           'quickstart'=TRUE # to use previously saved results in the begining
           )
options(gl=gl)
(getOption('gl') %>% as.data.table())[,rn:=1] %>% melt('rn')

#' ##test##
#+ startup,include=FALSE
setwd("/Users/gliao/Dropbox/Research/ccy basis/creditmigration")
source('util.r')
library(knitr)
opts_chunk$set(echo=FALSE,include=TRUE)

#+ echo=FALSE,eval=FALSE
include_graphics('../paper/figures/VAR_irfeur_A.png')
#knitr::stitch_rmd('test.R')
#rmarkdown::render('test.R',clean=F)