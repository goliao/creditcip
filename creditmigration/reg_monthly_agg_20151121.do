clear matrix
adopath + .
clear
set more off
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"

/*
* try to explore % of total issuance in euro vs us
* specifically, create a variable: netissuanceabrd as % of total issuance for all EU/US firms; this is likely to be small
* failed:: now create a variable: netissuanceabrd as a % of only European firms; this is bigger but harder to justify
* ideally, create a variable: netissuanceabrd as a % of all firms that has the ability to issue in both mkts; this eliminates firms that would never consider issuing abrd
* not sure how much seasonality there are, but we can try to see removing seasonality would help
* next things to try: 3 mo rolling avg, seasonality adj, use different OAS sprd

*** data prep
** do data_cleaning_20151121.do



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
drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh"
keep if pub=="Public" | pub=="Sub."
* drop if rule144a=="Y"
* drop if amt==.
* drop if amt<10
* gen lnamt=ln(amt)

save sdc96_clean3.dta,replace

capture program drop icollapse
program define icollapse 
    args  A B a b nata natb amtsource natsource
    use sdc96_clean3.dta,clear
    * keep `natsource' d cur E`natsource' Ecur amt procds proovsld ytofm month monthly year
    keep if ccy=="`A'" | ccy=="`B'"
    keep if `natsource'=="`nata'" | `natsource'=="`natb'"
    * tabstat amt, s(sum mean, count, sd,  p5, p10, median, p90, p99) by(ccy)
    collapse (sum) `amtsource', by(monthly year month ccy modnat) fast
    * convert amt to billions
    gen I_`A'`b'=`amtsource'/1000 if modnat=="`natb'" & ccy=="`A'"
    gen I_`B'`a'=`amtsource'/1000 if modnat=="`nata'" & ccy=="`B'"
    gen I_`A'`B'_tot=`amtsource'/1000
    gen I_`B'_tot=`amtsource'/1000 if ccy=="`B'"
    collapse (sum) I_`A'`b' I_`B'`a' I_`A'`B'_tot I_`B'_tot, by(monthly year month) fast
    gen I_net_`A'`B'=I_`A'`b'-I_`B'`a'
    gen i_net_`A'`B'=I_net_`A'`B'/I_`A'`B'_tot*100
    gen i_gross_`A'`B'=(I_`A'`b'+I_`B'`a')/I_`A'`B'_tot*100
    gen i_`B'_frac=I_`B'_tot/I_`A'`B'_tot
    save temp_issuance_`A'`B'.dta,replace
end 

icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat

* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
* save temp_issuance_all.dta,replace
* use temp_issuance_all,clear
merge 1:1 monthly using prices,nogen
tsset monthly
save temp_regdata.dta,replace



use temp_regdata.dta,clear
gen Crd_diff_ercy_43=baml_er43_govtoas -baml_cy43_govtoas 
gen Crd_diff_ercy_34=baml_er34_govtoas -baml_cy34_govtoas 
gen Crd_diff_ercy_33=baml_er33_govtoas -baml_cy33_govtoas 
gen Crd_diff_ercy_44=baml_er44_govtoas -baml_cy44_govtoas 

gen Crd_diff_erc_34=baml_er34_govtoas -baml_c4a3_govtoas
gen post07=1 if year>=2008
replace post07=0 if post07==.
* gen ratediff=eusa5-ussw5
* local controls "swaprate ratediff post07"
local controls "post07"

capture program drop ireg
program define ireg
    args A B crdsource basis filename controls
    * local filename "univar_test1"
    local outmode 1
    local outfiletype "xls"
    * start with 3 regressions done for presentation:
    * neweymod F6.I_net_AB Crd_diff`crdsource', lag(6)
    * 1.
    neweymod F.I_net_`A'`B' Crd_diff_`crdsource' `controls', lag(6)
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', replace bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline F.I_net_`A'`B') (tsline Crd_diff_`crdsource', yaxis(2)) 
    * graph export graph_`B'`A'_1.eps,replace
    * 2.
    neweymod FD.`basis' Crd_diff_`crdsource' `controls' if year>=2008, lag(6)
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline FD.`basis') (tsline Crd_diff_`crdsource', yaxis(2)) 
    * graph export graph_`B'`A'_2.eps,replace
    * 3.
    reg D.`basis' I_net_`A'`B' `controls'
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline I_net_`A'`B') (tsline D.`basis', yaxis(2)) 
    * graph export graph_`B'`A'_3.eps,replace

    * works much better using pct of total issuance
    * 4.
    neweymod F.i_net_`A'`B' Crd_diff_`crdsource' `controls', lag(6)
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline F.i_net_`A'`B') (tsline Crd_diff_`crdsource', yaxis(2)) 
    * graph export graph_`B'`A'_4.eps,replace

    * 5.
    capture drop Crd_diff_`crdsource'_effective
    gen Crd_diff_`crdsource'_effective=Crd_diff_`crdsource'-`basis'
    neweymod F.I_net_`A'`B' Crd_diff_`crdsource'_effective `controls', lag(6)
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline F.I_net_`A'`B') (tsline Crd_diff_`crdsource'_effective, yaxis(2)) 
    * graph export graph_`B'`A'_5.eps,replace
    * 6.
    neweymod F.i_net_`A'`B' Crd_diff_`crdsource'_effective `controls', lag(6)
    if `outmode' outreg2 using reg_`B'`A'_`filename'.`outfiletype', append bdec(3) tdec(2) bracket tstat sortvar(Crd_diff_`crdsource' Crd_diff_`crdsource'_effective I_net_`A'`B' `controls')
    * graph twoway (tsline F.i_net_`A'`B') (tsline Crd_diff_`crdsource'_effective, yaxis(2)) 
    * graph export graph_`B'`A'_6.eps,replace
end

ireg "USD" "EUR" "ercy_43" "eubs5" "test2" `controls'
ireg "USD" "EUR" "ercy_34" "eubs10" "test3" `controls'
ireg "USD" "EUR" "erc_34" "eubs10" "test4" `controls'



/*
* neweymod F6.i_net_AB OASdiff, lag(6)
* can also attemp to control linearly, but doesn't work as well
* neweymod F.I_net_AB OASdiff I_ABtot, lag(6)


gen i_net_AB_ma=(F.i_net_AB+F2.i_net_AB+F3.i_net_AB+F4.i_net_AB+F5.i_net_AB+F6.i_net_AB)/6
neweymod i_net_AB_ma OASdiff, lag(6)


* one problem is that i_net_AB is correlated with amt of european issuance
corr i_net_AB eurofirmshare

* multivariate version including fx cost
neweymod F.i_net_AB OASdiff `basis', lag(6)

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