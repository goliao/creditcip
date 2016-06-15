clear matrix
adopath + .
clear
set more off
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"

/*
* cut the sdc data to monthly level in aggregate for EU and US and perform exploratory regression 
* try to explore % of total issuance in euro vs us
* specifically, create a variable: netissuanceabrd as % of total issuance for all EU/US firms; this is likely to be small
* failed:: now create a variable: netissuanceabrd as a % of only European firms; this is bigger but harder to justify
* ideally, create a variable: netissuanceabrd as a % of all firms that has the ability to issue in both mkts; this eliminates firms that would never consider issuing abrd
* not sure how much seasonality there are, but we can try to see removing seasonality would help
* next things to try: 3 mo rolling avg, seasonality adj, use different OAS sprd
* ideal: use individaul curves


* use original uncleaned version


use sdc96,clear  
replace nat="United States" if (nat=="Cayman Islands" | nat=="Bermuda" | nat=="Jersey") & (exch=="New York" | exch=="Nasdaq")
drop Enat
encode nat, gen(Enat)

gen month=month(d)
gen year=year(d)
gen monthly = ym(year,month)
format monthly %tm

* define nat source: nat is closer to 
local natsource nat
gen modnat=nat
replace modnat="Eurozone" if inlist(`natsource',"Austria","Belgium","Cyprus","Finland","France","Germany","Greece")
replace modnat="Eurozone" if inlist(`natsource',"Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta")
replace modnat="Eurozone" if inlist(`natsource',"Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")
drop uop
drop buss

save sdc96_clean1.dta,replace

* *** merge in human readable currency code and main currency for each country
* insheet using currency_code_country.csv, names clear
* drop ctry
* save currency_code_country.dta,replace
* insheet using currency_code_country.csv, names clear
* rename ctry nat
* rename ccy mainccy
* save currency_code_country2.dta,replace

use sdc96_clean1,clear
merge m:m cur using currency_code_country.dta
keep if _merge==3
drop _merge

merge m:1 nat using currency_code_country2.dta
keep if _merge==3
drop _merge
drop cur

gen foreign=1 if ccy!=mainccy
replace foreign=0 if foreign==.

save sdc96_clean2.dta,replace

*/


use sdc96_clean2,clear
merge m:1 monthly ccy using fxlong2.dta
drop _merge

drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh"
keep if pub=="Public" | pub=="Sub."
* drop if rule144a=="Y"
* drop if amt==.
* drop if amt<10
* gen lnamt=ln(amt)

encode ccy, gen(Eccy)
local natsource modnat
local ccyA "USD"
local nata "United States"
local ccyB "EUR"
local natb "Eurozone"
local amtsource amt

* keep `natsource' d cur E`natsource' Ecur amt procds proovsld ytofm month monthly year
keep if ccy=="`ccyA'" | ccy=="`ccyB'"
keep if `natsource'=="`nata'" | `natsource'=="`natb'"

* tabstat amt, s(mean, count, sd,  p5, p10, median, p90, p99) by(foreign)

collapse (sum) `amtsource', by(monthly ccy Eccy modnat) fast

gen I_Ab=`amtsource'/1000 if modnat=="`natb'" & ccy=="`ccyA'"
gen I_Ba=`amtsource'/1000 if modnat=="`nata'" & ccy=="`ccyB'"

gen I_ABtot=`amtsource'/1000
gen I_Btot=`amtsource'/1000 if ccy=="`ccyB'"
collapse (sum) I_Ab I_Ba I_ABtot I_Btot, by(monthly) fast

gen I_net_AB=I_Ab-I_Ba
* gen issueabroad_pct_sdc=I_net_AB/(I_Ab+I_Ba)

gen i_net_AB=I_net_AB/I_ABtot*100
gen i_gross_AB=(I_Ab+I_Ba)/I_ABtot*100
gen i_B_frac=I_Btot/I_ABtot

* gen I_Ab_yoy=I_Ab/L12.I_Ab-1
* gen I_Ba_yoy=I_Ba/L12.I_Ba-1

save temp_issuance.dta,replace


use credit_BBB_57yr.dta, clear
* merge 1:1 monthly using xccy4.dta
* worsens in the last few periods, let's not use it for now
merge 1:1 monthly using eubs5.dta 
drop _merge
merge 1:1 monthly using temp_issuance.dta
drop _merge

merge 1:1 monthly using ussw5.dta
drop if _merge==2
drop _merge

merge 1:1 monthly using eusa5.dta

* merge 1:1 monthly using fxwide2.dta
gen ratediff=eusa5-swaprate
rename OASdiff Crd_diff
tsset monthly
save temp_regdata.dta,replace



use temp_regdata.dta
gen post07=1 if year>=2008
replace post07=0 if post07==.
* local controls "swaprate ratediff post07"
local controls ""
local ccyA "USD"
local ccyB "EUR"
local filename "univar"


* start with 3 regressions done for presentation:
* neweymod F6.I_net_AB Crd_diff, lag(6)
* 1.
neweymod F.I_net_AB Crd_diff `controls', lag(6)
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, replace bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline F.I_net_AB) (tsline Crd_diff, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_1.eps,replace
* 2.
neweymod FD.eubs5 Crd_diff `controls' if year>=2008, lag(6)
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline FD.eubs5) (tsline Crd_diff, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_2.eps,replace
* 3.
reg D.eubs5 I_net_AB `controls'
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline I_net_AB) (tsline D.eubs5, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_3.eps,replace

* works much better using pct of total issuance
* 4.
neweymod F.i_net_AB Crd_diff `controls', lag(6)
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline F.i_net_AB) (tsline Crd_diff, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_4.eps,replace

* 5.
gen Crd_diff_effective=Crd_diff-eubs5
neweymod F.I_net_AB Crd_diff_effective `controls', lag(6)
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline F.I_net_AB) (tsline Crd_diff_effective, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_5.eps,replace
* 6.
neweymod F.i_net_AB Crd_diff_effective `controls', lag(6)
outreg2 using reg_`ccyB'`ccyA'_`filename'.doc, append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff Crd_diff_effective I_net_AB `controls')
* graph twoway (tsline F.i_net_AB) (tsline Crd_diff_effective, yaxis(2)) 
* graph export graph_`ccyB'`ccyA'_6.eps,replace



/*
* neweymod F6.i_net_AB OASdiff, lag(6)
* can also attemp to control linearly, but doesn't work as well
* neweymod F.I_net_AB OASdiff I_ABtot, lag(6)


gen i_net_AB_ma=(F.i_net_AB+F2.i_net_AB+F3.i_net_AB+F4.i_net_AB+F5.i_net_AB+F6.i_net_AB)/6
neweymod i_net_AB_ma OASdiff, lag(6)


* one problem is that i_net_AB is correlated with amt of european issuance
corr i_net_AB eurofirmshare

* multivariate version including fx cost
neweymod F.i_net_AB OASdiff eubs5, lag(6)

* post crisis loses significance
* that's because the crisis the the single most important event driving credit sprd differential, 
* prior to crisis there weren't much issuance anyway
neweymod F.i_net_AB OASdiff if year>2008, lag(6)
neweymod F6.i_net_AB OASdiff  if year>2008, lag(6)
neweymod issueabrdpctma OASdiff  if year>2008, lag(6)


neweymod F3.i_net_AB OASdiff, lag(6)

twoway (tsline i_net_AB) (tsline OASdiff, yaxis(2))
twoway (tsline issueabrdpctma) (tsline OASdiff, yaxis(2))

twoway (tsline issueabroad_pct_sdc) (tsline OASdiff, yaxis(2))
neweymod F.i_net_AB OASdiff year, lag(6)
* but doesn't work prior to crisis
neweymod F.i_net_AB OASdiff if year<2009, lag(6)

twoway (tsline i_net_AB) (tsline eurofirmshare, yaxis(2))
corr i_net_AB eurofirmshare




* doesn't work after the crisis 
neweymod F.I_net_AB OASdiff if year>2008, lag(6)
* but works before the crisis
neweymod F.I_net_AB OASdiff if year<2008 & year>2003, lag(6)
* 2003 screws up some
neweymod F.I_net_AB OASdiff if year<2008, lag(6)
* crisis screws up even more
neweymod F.I_net_AB OASdiff if year<=2008, lag(6)

* without constnat: dones't really make sense; we need the regression to have a meaning
* without constant after crisis works
neweymod F.I_net_AB OASdiff if year>2008, lag(6) nocons
* but fails really badly before crisis
neweymod F.I_net_AB OASdiff if year<2008, lag(6) nocons


twoway (tsline F.I_net_AB) (tsline OASdiff,yaxis(2)) 
twoway (tsline F.I_net_AB) (tsline OASdiff,yaxis(2)) if year<2008


** as a percentage of issuance works!!!
neweymod issueabroad_pct_sdc OASdiff, lag(6)
corr issueabroad_pct_sdc OASdiff
** but doesnt work well before the crisis :(
neweymod issueabroad_pct_sdc OASdiff if year<2009, lag(6)
neweymod issueabroad_pct_sdc OASdiff if year>=2009, lag(6)

* here's a more clear picture
twoway (tsline issueabroad_pct_sdc) (tsline OASdiff, yaxis(2)) 



* we see that regression 1 doesn't work at all!
* but we see that conditional on precrisis it works
* maybe this is saying that post 2008, other factors such as this spread become increasingly important
neweymod F.I_net_AB OASdiff if year<2008,lag(6)

* also this worked really well
reg F.I_net_AB D.OASdiff D.eubs5 if year>2008
*/