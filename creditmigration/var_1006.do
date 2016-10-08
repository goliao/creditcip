clear matrix
set more off
set matsize 10000

* use vardatain_1006.dta,clear
* use vartemp.dta,clear
use vardatain_1007.dta,clear

format date %tm
  gen year=year(date)
  gen month=month(date)
  gen monthly=ym(year,month)
  format monthly %tm

decode ccy, gen(strccy)

* tsset ccy monthly

* * keep if strccy=="eur"
* * simple irf euro
* 	var credit cip, lags(1)
* 	irf create irfeu, set(irf1,replace) step(10)
* 	irf graph irf, irf(irfeu) impulse(credit) response(cip) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
* 	* graph export "../paper/figures/VAR_sirfeur`filedate'.eps",replace
	
* 	* ortho irf euro
* 	var  cip credit, lags(1)
* 	irf create irfeu, order(credit cip) set(irf2,replace) step(10)
* 	irf graph oirf, irf(irfeu) impulse(credit) response(cip) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
* 	* graph export "../paper/figures/VAR_oirfeur`filedate'.eps",replace

xtset ccy monthly

drop if date>=date("20160901","YMD")
* eur only version with just cip and credit
	* varsoc  credit cip if strccy=="eur"
	* var credit cip if strccy=="eur", lags(1/3)
	* irf create irfeu, set(irf1,replace) step(10) 
	* irf describe irfeu
	* * irf graph irf, irf(irfeu) impulse(credit) response(cip) 
	* * irf graph irf, irf(irfeu) impulse(cip) response(credit) 
	* irf cgraph (irfeu credit cip irf) (irfeu cip credit irf) (irfeu credit credit irf) (irfeu cip cip irf)


local graphopt "title("") subtitle("") note("") legend(off) xtitle(month) graphregion(color(white)) bgcolor(white)"

* eur only: with issuance
	*  ordering is default to the order specified by var function
	var i_netflow credit cip if strccy=="eur", lags(1) 
	irf create eur_oirf, set(irf1,replace) step(10) 
	irf describe eur_oirf
	* irf only
	* irf cgraph  (eur_oirf credit credit irf)  (eur_oirf credit cip irf)  (eur_oirf credit i_netflow irf) (eur_oirf cip credit irf) (eur_oirf cip cip irf)  (eur_oirf cip i_netflow irf) , cols(3)
	* oirf only
	irf cgraph  (eur_oirf credit credit oirf,`graphopt')  (eur_oirf credit cip oirf,`graphopt')  (eur_oirf credit i_netflow oirf,`graphopt') (eur_oirf cip credit oirf,`graphopt') (eur_oirf cip cip oirf,`graphopt')  (eur_oirf cip i_netflow oirf,`graphopt') , cols(3) graphregion(color(white)) bgcolor(white)
	* graph export "../paper/figures/VAR_oirfeur_161007.eps",replace



	* reorder
	var i_netflow cip credit if strccy=="eur", lags(1) 
	irf create eur_oirf, set(irf1,replace) step(10) 
	irf describe eur_oirf
	irf cgraph  (eur_oirf credit credit oirf,`graphopt')  (eur_oirf credit cip oirf,`graphopt')  (eur_oirf credit i_netflow oirf,`graphopt') (eur_oirf cip credit oirf,`graphopt') (eur_oirf cip cip oirf,`graphopt')  (eur_oirf cip i_netflow oirf,`graphopt') , cols(3) graphregion(color(white)) bgcolor(white)
	* graph export "../paper/figures/VAR_oirfeur_161007b.eps",replace

	* using mu instead of i_netflow	
	var mu credit cip if strccy=="eur", lags(1) 
	irf create eur_oirf, set(irf1,replace) step(10) 
	irf describe eur_oirf
	irf cgraph  (eur_oirf credit credit oirf,`graphopt')  (eur_oirf credit cip oirf,`graphopt')  (eur_oirf credit mu oirf,`graphopt') (eur_oirf cip credit oirf,`graphopt') (eur_oirf cip cip oirf,`graphopt')  (eur_oirf cip mu oirf,`graphopt') , cols(3) graphregion(color(white)) bgcolor(white)
	* irf cgraph  (irfeu mu credit oirf)  (irfeu mu cip oirf)  (irfeu mu mu oirf)  , cols(3)
	* graph export "../paper/figures/VAR_oirfeur_161007mu.eps",replace		

	* lags=4 according to AIC
	varsoc  i_netflow credit cip  if strccy=="eur"
	var i_netflow credit cip if strccy=="eur", lags(1/4) 
	irf create eur_oirf, set(irf1,replace) step(10) 
	irf describe eur_oirf
	* irf only
	* irf cgraph  (irfeu credit credit irf)  (irfeu credit cip irf)  (irfeu credit i_netflow irf) (irfeu cip credit irf) (irfeu cip cip irf)  (irfeu cip i_netflow irf) , cols(3)
	* oirf only
	irf cgraph  (eur_oirf credit credit oirf,`graphopt')  (eur_oirf credit cip oirf,`graphopt')  (eur_oirf credit i_netflow oirf,`graphopt') (eur_oirf cip credit oirf,`graphopt') (eur_oirf cip cip oirf,`graphopt')  (eur_oirf cip i_netflow oirf,`graphopt') , cols(3) graphregion(color(white)) bgcolor(white)
	* graph export "../paper/figures/VAR_oirfeur_4lags_161007.eps",replace
	***  both irf and oirf
	* irf cgraph  (irfeu credit credit irf)  (irfeu credit cip irf)  (irfeu credit i_netflow irf) (irfeu cip credit irf) (irfeu cip cip irf)  (irfeu cip i_netflow irf)  (irfeu credit credit oirf)  (irfeu credit cip oirf)  (irfeu credit i_netflow oirf) (irfeu cip credit oirf) (irfeu cip cip oirf)  (irfeu cip i_netflow oirf) , cols(3)
	vargranger
	irf table fevd
	irf graph fevd

	* c-b and iss
	gen netdev=netmisp
	var i_netflow netdev if strccy=="eur", lags(1) 
	irf create oirf_eur, set(irf1,replace) step(10) 
	irf describe oirf_eur
	irf graph oirf, irf(oirf_eur) impulse(netdev) response(i_netflow) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
	graph export "../paper/figures/VAR_oirfeur_netdeviss_161007.eps",replace

* panvel version with mc bootstrap
	xtvar i_netflow credit cip if inlist(strccy,"eur","gbp","jpy","chf"), lags(1) step(10) mc
	* reordering looks better even!
	xtvar i_netflow cip credit if inlist(strccy,"eur","gbp","jpy","chf"), lags(1) step(10) mc

	* all ccy
	xtvar i_netflow credit cip , lags(1) mc step(12)

	
	* doing the version with mu
		gen lnmu=log(mu)
		xtvar lnmu credit cip if inlist(strccy,"eur","gbp","jpy","chf"), lags(1) step(10) mc


*************************************
 * some checks
	*  basic version that makes stata choose lags automatically
	varbasic i_netflow credit cip if strccy=="eur"
	varstable
	vargranger
		* similar but larger S.E. with xtvar
		xtvar i_netflow credit cip if strccy=="eur",lags(1)



* backing out what is being estimated with xtvar
xtvar credit cip , lags(1) nodraw
* this is equivalent to:
	xtreg credit L.credit L.cip i.ccy
	xtreg cip L.cip L.credit i.ccy



* similarily in the single variable case
var credit cip if strccy=="eur",lags(1)
	reg credit L.credit L.cip if strccy=="eur"
	reg cip L.cip L.credit if strccy=="eur"

*************************************


* xtvar credit cip, lags(1) step(12)	
* xtvar credit cip, lags(2) step(12)
* *  this generate a much better result
* xtvar credit cip, lags(1) step(12) pooled  
* * this also yields a much better result
* xtvar credit cip, lags(1) mc
* * one unit shock instead of one standard deviation shock
* xtvar credit cip, lags(1) mc stirf


* xtvar credit cip i_netflow if inlist(strccy,"eur","gbp","jpy","chf"), lags(1) mc step(12)

* xtvar credit cip i_netflow, lags(1) mc step(12)
* xtvar i_netflow credit cip , lags(1) mc step(12)
* xtvar i_netflow cip credit, lags(1) mc step(12)





* xtvar credit cip i_netflow if year>2007, lags(1) mc step(12)
* xtvar credit cip i_netflow if year>2008 & inlist(strccy,"eur","gbp","jpy","chf"), lags(1) mc step(12)
* xtvar credit cip i_netflow if year>2009, lags(1) mc step(8)

* xtvar credit cip i_netflow, lags(1) mc step(12) pooled  

* xtvar credit cip I_netflow, lags(1) mc 

* xtvar credit cip mu, lags(1) mc step(12)
* gen lnmu=log(mu)
* xtvar credit cip lnmu, lags(1) mc step(12)
* * pretty good graphs for ln mu
* xtvar credit cip lnmu if year>2008, lags(1) mc step(12) 
* xtvar credit cip lnmu if year>2008 & inlist(strccy,"eur","gbp","jpy","chf"), lags(1) mc step(12) 
* xtvar credit cip i_netflow if year>2008, lags(1) mc step(12) 

* xtvar crediteff i_netflow if year>2008, lags(1) mc step(12) 
* xtvar crediteff lnmu if year>2008, lags(1) mc step(12) 

* xtvar crediteff i_netflow if year>2008 & inlist(strccy,"eur","gbp","jpy","chf"), lags(1) mc step(12) 

* varsoc credit cip if strccy=="eur"



* ds
* xtvar  cip credit, lags(1) step(12)	
* xtvar credit cip, lags(1) nodraw


* xtset ccy monthly
* pvar credit cip if strccy=="eur", lags(1)

* reg credit L.credit L.cip if strccy=="eur"







* pvar cip credit, lags(1) fd
* pvar cip credit, td  instlags(1) gmmstyle 

* pvarirf, imp(cip) res(credit) porder(cip credit) oirf mc(100) title("impact of cip on credit") 


* help pvar




* pvar credit cip, lags(1) 
* pvar cip credit, instlags(1/2)
* pvarirf, imp(cip) res(credit) oirf mc(200) title("impact of cip on credit") 
* pvarirf, imp(credit) res(cip) oirf mc(200) title("impact of cip on credit") 


* varsoc credit cip if strccy=="eur", maxlag(6) 

* var credit cip if strccy=="eur", lags(1)
* irf create irfeu, set(irf1,replace) step(10)
* irf graph irf, irf(irfeu) impulse(credit) response(cip) 
* irf graph irf, irf(irfeu) impulse(cip) response(credit) 


* varsoc D.credit D.cip, maxlag(12)
* var D.credit D.cip if strccy=="eur", lags(1)
* irf create irfeu, set(irf1,replace) step(10)
* irf graph irf, irf(irfeu) impulse(D.credit) response(D.cip) 
* irf graph irf, irf(irfeu) impulse(D.cip) response(D.credit) 

* * Unit Root test
* reg credit L.credit 
* reg cip L.cip 
* dfuller credit if  & strccy=="eur", lag(1)
* dfuller cip if   & strccy=="eur", lag(1)
* dfuller i_netflow if strccy=="eur", lag(1)

* dfuller D.credit if strccy=="eur", lag(1)
* dfuller D.cip if strccy=="eur", lag(1)
* dfuller i_netflow if strccy=="eur", lag(1)


* pvar credit cip, lags(1)

* keep if strccy=="eur"
* drop e
* tsset monthly
* reg cip credit 
* predict e, resid
* dfuller e, lags(1)



*  tsset ccy monthly

*    eststo clear
*   *eststo: xi: reg D_abs_netdev I_both_iss i.ccy,robust 
*   *eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust

*   eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp if ccystr=="eur",robust 
*   eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp if ccystr=="eur",robust 

*   eststo: newey D_abs_netmisp I_both_iss if ccystr=="eur",lag(6)
*   eststo: newey D_abs_netmisp I_both_mat if ccystr=="eur",lag(6)

*   eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp i.ccy,robust cluster(monthly)
*   eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy,robust cluster(monthly)
*   eststo: xi: reg I_both_iss I_both_mat i.ccy  if ccystr=="eur",robust 
*   eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust 
*   eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat) if ccystr=="eur",fe robust 
*   eststo:newey D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy, lag(12) force
*   esttab using temp.csv, order(I_both*) bracket r2 nostar nogaps replace
  















* gen year=year(date)
* gen monthly=ym(year,month)
* format monthly %tm
* tsset ccy monthly

* * w/o cluster
* eststo clear
* eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust 
* eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust 
* eststo: xi: reg D_abs_netmisp I_both_iss L.D_abs_netmisp i.ccy,robust 
* eststo: xi: reg D_abs_netmisp I_both_mat L.D_abs_netmisp i.ccy,robust 
* eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust 
* esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps



* eststo clear
* drop isshat issres
* eststo: xi: reg I_both_iss I_both_mat i.ccy
* predict isshat , xb
* predict issres , resid
* eststo: xi: reg D_abs_netmisp isshat L.D_abs_netmisp i.ccy,robust 
* eststo: xi: reg D_abs_netmisp issres L.D_abs_netmisp i.ccy,robust 
* eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=I_both_mat),fe robust 
* esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps

* help predict







* drop mat_detrended
* xi:reg I_both_mat monthly i.ccy
* predict mat_detrended , resid

* br monthly ccy I_both_mat mat_detrended
* tsline I_both_mat mat_detrended if ccy==2

* eststo:xtivreg2 D_abs_netmisp  L.D_abs_netmisp (I_both_iss=mat_detrended),fe robust 
* eststo: xi: reg D_abs_netmisp mat_detrended L.D_abs_netmisp i.ccy,robust 





* use temp.dta,clear

* format date %tm
* gen year=year(date)
* gen monthly=ym(year,month)
* format monthly %tm
* tsset ccy monthly
* eststo clear
* eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust cluster(ccy)
* eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust cluster(ccy)
* eststo: xi: reg I_both_iss I_both_mat i.ccy,robust cluster(ccy)
* eststo:xtivreg2 D_abs_netmisp  (I_both_iss=I_both_mat),fe robust cluster(ccy)
* esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps


* eststo clear
* eststo: xi: reg D_abs_netmisp I_both_iss i.ccy,robust 
* eststo: xi: reg D_abs_netmisp I_both_mat i.ccy,robust 
* eststo: xi: reg I_both_iss I_both_mat i.ccy,robust 
* eststo:xtivreg2 D_abs_netmisp  I_both_mat,fe robust 
* eststo:xtivreg2 D_abs_netmisp  (I_both_iss=I_both_mat),fe robust 
* esttab, order(I_both*) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps




* eststo: reg D_abs_netmisp I_both,robust
* eststo: xi: reg D_abs_netmisp I_both i.ccy,robust
* eststo: xi: reg D_abs_netmisp I_both i.ccy,robust cluster(ccy)
* eststo: cluster2 D_abs_netmisp I_both, fcluster(ccy)  tcluster(monthly)
* eststo: xi:cluster2 D_abs_netmisp I_both i.ccy, fcluster(ccy)  tcluster(monthly)
* eststo: xi:cluster2 D_abs_netmisp I_both i.ccy i.monthly, fcluster(ccy)  tcluster(monthly)
* tsset ccy monthly
* eststo: newey D_abs_netmisp I_both,lag(6) force
* *bootstrap "regress  D_abs_netmisp I_both" _b, reps(1000) cluster(ccy)
* esttab
* * esttab using temp.csv,  bracket r2 nostar nogaps nonote addnotes(" ")  append

* xtivreg2  D_abs_netmisp I_both,fe robust


* format quarterly %tq
*   xi:reg cip levfac2adj i.ccy,robust
*   xi:reg credit levfac2adj i.ccy,robust
*   tsset ccy quarterly 
*   newey cip levfac2adj,lag(6) force
*   newey credit levfac2adj,lag(6) force