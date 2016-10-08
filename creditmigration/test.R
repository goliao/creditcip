#' ---
#' title: "test"
#' date: "Oct 07,2016"
#' 
#' ---

#+ setupknitr, include=FALSE,echo=F
library(knitr)
opts_chunk$set(echo=FALSE,include=FALSE)

#'VAR
print('this is not included')

#+ show, include=T
print('included')

#+ show 2, include=T
print('included2')
1+1
#+
2+1
#' ***test text***
#' **test text**
#+ include=T
print('test3')
#test5
print('t4')

#' ##stata
#+ include=T
source('util.r')
RStata::stata('di "test"
              use vartemp
              su
              reg credit cip
              twoway scatter cip credit
              graph export "temp.png"')
#' ###Stata output
#' ![](temp.png)
#'
#' 
#+ test name of, include=TRUE 
src='
  clear matrix
set more off
set matsize 10000
use tempvar
format date %tm
gen year=year(date)
gen month=month(date)
gen monthly=ym(year,month)
format monthly %tm
decode ccy, gen(strccy)
xtset ccy monthly
drop if date>=date("20160901","YMD")
local graphopt "title("") subtitle("") note("") legend(off) xtitle(month) graphregion(color(white)) bgcolor(white)"

* A: irf
var i_netflow credit cip if strccy=="eur", lags(1) 
irf create eur_irf, set(irf1,replace) step(10) 
irf describe eur_irf
irf cgraph  (eur_irf credit credit irf,`graphopt\')  (eur_irf credit cip irf,`graphopt\')  (eur_irf credit i_netflow irf,`graphopt\') (eur_irf cip credit irf,`graphopt\') (eur_irf cip cip irf,`graphopt\')  (eur_irf cip i_netflow irf,`graphopt\') , cols(3) graphregion(color(white)) bgcolor(white)
graph export "../paper/figures/VAR_irfeur_A.png",replace

'

stata(src)
#' ![](../paper/figures/VAR_irfeur_A.png)
  
#rmarkdown::render('test.R',output_format='html_document')
rmarkdown::render('results_161007.R',output_format='all',output_dir = 'C:/Users/gliao/Documents',intermediates_dir='C:/Users/gliao/Documents')
#rmarkdown::render('results_161007.R',output_format='html_document')
rmarkdown::render('results_161007.R',output_format='pdf_document',output_dir = 'C:/Users/gliao/Documents',intermediates_dir='C:/Users/gliao/Documents')
