clear matrix
adopath + .
clear
set more off
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"

*/*
* try to explore % of total issuance in euro vs us
* specifically, create a variable: netissuanceabrd as % of total issuance for all EU/US firms; this is likely to be small
* failed:: now create a variable: netissuanceabrd as a % of only European firms; this is bigger but harder to justify
* ideally, create a variable: netissuanceabrd as a % of all firms that has the ability to issue in both mkts; this eliminates firms that would never consider issuing abrd
* not sure how much seasonality there are, but we can try to see removing seasonality would help
* next things to try: 3 mo rolling avg, seasonality adj, use different OAS sprd

*** data prep
** do data_cleaning_20151121.do

/* Add foreign/upforeign, humanize ccy code use original uncleaned version

use sdc96,clear  
drop uop
drop buss

replace nat="United States" if (nat=="Cayman Islands" | nat=="Bermuda" | nat=="Jersey") & (exch=="New York" | exch=="Nasdaq")
replace upnat="United States" if (upnat=="Cayman Islands" | upnat=="Bermuda" | upnat=="Jersey") & (exch=="New York" | exch=="Nasdaq")
drop Enat Eupnat
encode nat, gen(Enat)
encode upnat, gen(Eupnat)

gen month=month(d)
gen year=year(d)
gen monthly = ym(year,month)
format monthly %tm

* define nat source: nat is closer to 
gen modnat=nat
replace modnat="Eurozone" if inlist(modnat,"Austria","Belgium","Cyprus","Finland","France","Germany","Greece")
replace modnat="Eurozone" if inlist(modnat,"Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta")
replace modnat="Eurozone" if inlist(modnat,"Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")

gen modupnat=upnat
replace modupnat="Eurozone" if inlist(modupnat,"Austria","Belgium","Cyprus","Finland","France","Germany","Greece")
replace modupnat="Eurozone" if inlist(modupnat,"Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta")
replace modupnat="Eurozone" if inlist(modupnat,"Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")

save sdc96_clean1.dta,replace

* *** merge in human readable currency code and main currency for each country
* insheet using currency_code_country.csv, names clear
* drop ctry
* save currency_code_country.dta,replace
* insheet using currency_code_country.csv, names clear
* rename ctry nat
* rename ccy mainccy
* drop cur
* save currency_code_country2.dta,replace
* rename nat upnat
* rename mainccy upmainccy
* save currency_code_country3.dta,replace

use sdc96_clean1,clear
merge m:m cur using currency_code_country.dta
keep if _merge==3
drop _merge

merge m:1 nat using currency_code_country2.dta
* keep if _merge==3
drop _merge
drop cur
* note that we dropped countries for which upnat is not in the data 
merge m:1 upnat using currency_code_country3.dta
keep if _merge==3
drop _merge


gen foreign=1 if ccy!=mainccy
replace foreign=0 if foreign==.

gen upforeign=1 if ccy!=upmainccy
replace upforeign=0 if upforeign==.

gen qrt=qofd(d)
format %tq qrt
gen sic1=substr(sicp,1,1)
destring sic1,replace
save sdc96_clean2.dta,replace

*/

use sdc96_clean2,clear

drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Asset Bkd Certs"
keep if pub=="Public" | pub=="Sub."
* gen famt=foreign*amt
* gen famtperc=famt/1.67e+07
* tabstat famt, s(sum) by(ccy) 
* tabstat famtperc, s(sum) by(modnat) 

* drop if rule144a=="Y"
drop if amt==.
drop if amt<50
drop if ytofm<1
drop if sp==""

* drop if sp=="AAA"
* drop if foreign==0
* tab sp 
* codebook sp
* tab mdy
* codebook mdy 
save sdc96_clean3.dta,replace

capture program drop icollapse
icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
icollapse "USD" "CAD" "us" "ca" "United States" "Canada" amt modnat
icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat

* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen

merge 1:1 monthly using prices_extended.dta,nogen
tsset monthly
gen date=dofm(monthly)
format date %td
save temp_regdata.dta,replace

use temp_regdata.dta,clear
gen post07=1 if year>=2008
replace post07=0 if post07==.


* collapse (sum) *, by(year qrt) fast

* gen ratediff=eusa10-ussw10
* gen swaprate=ussw10
* local controls "swaprate ratediff post07"
local controls ""




capture program drop ireg
ireg "USD" "EUR" "ercy_44" "eubs10" "ercy_44" D.eubsc `controls'
ireg "USD" "EUR" "ercy_44_3m" "eubs10" "ercy_44_3m" D.eubsc `controls'
ireg "USD" "EUR" "ercy_44_6m" "eubs10" "ercy_44_6m" D.eubsc `controls'

ireg "USD" "EUR" "ercy_43" "eubs5" "ercy_43" D.eubsc `controls'
ireg "USD" "EUR" "ercy_44" "eubs10" "ercy_44" D.eubsc `controls'
ireg "USD" "EUR" "erc_44" "eubs10" "erc_44" D.eubsc `controls'
ireg "USD" "EUR" "ercy_34" "eubs10" "ercy_34" D.eubsc `controls'
ireg "USD" "EUR" "ercy_36" "eubs10" "ercy_36" D.eubsc `controls'
ireg "USD" "EUR" "ercy_33" "eubs10" "ercy_33" D.eubsc `controls'
ireg "USD" "EUR" "erc_34" "eubs10" "erc_34" D.eubsc `controls'
ireg "USD" "EUR" "elc_34" "eubs10" "elc_34" D.eubsc `controls'

ireg "USD" "GBP" "urcy_43" "bpbs5" "test5" D.bpbsc `controls'
ireg "USD" "GBP" "urcy_34" "bpbs10" "test6" D.bpbsc `controls'
ireg "USD" "JPY" "jccy_30" "jybs10" "test2" D.jybsc `controls'



* capture program drop ireg
tsline er34_govtoas cy34_govtoas 
tsline Crd_diff_ercy_34

tsline Crd_diff_ercy_44 Crd_diff_ercy_44_3m Crd_diff_ercy_44_6m
gen i_net_USDEUR_3mf=(i_net_USDEUR+F.i_net_USDEUR+F2.i_net_USDEUR)/3
gen i_net_USDEUR_6mf=(i_net_USDEUR+F.i_net_USDEUR+F2.i_net_USDEUR+F3.i_net_USDEUR+F4.i_net_USDEUR+F5.i_net_USDEUR)/6
gen I_EURus_6mf=(I_EURus+F.I_EURus+F2.I_EURus+F3.I_EURus+F4.I_EURus+F5.I_EURus)/6
tsline i_net_USDEUR i_net_USDEUR_3mf i_net_USDEUR_6mf
twoway (tsline L3.i_net_USDEUR_6mf) (tsline F3.Crd_diff_ercy_44_6m,yaxis(2)) if year>=1999
neweymod F.i_net_USDEUR_6mf Crd_diff_ercy_44_6m if year>2000, lag(6)
neweymod F.i_net_USDEUR_6mf Crd_diff_ercy_34_6m if year>2000, lag(6)
neweymod F.i_net_USDEUR_6mf Crd_diff_ercy_34_6m if year>2000, lag(6)
neweymod F.i_net_USDEUR_3mf Crd_diff_ercy_34_3m if year>=2000, lag(6)

tsline Crd_diff_ercy_44_6m Crd_diff_ercy_34_6m
tsline I_EUR_tot I_EURus I_USDeu
tsline I_net_USDEUR
neweymod F6.I_EURus_6mf Crd_diff_ercy_34_6m if year>=2000,lag(6)











tsline Crd_diff_ercy_43 Crd_diff_ercy_34

tsline I_net_*

ivregress 2sls FD.eubs5 Crd_diff_ercy_43 `controls' if year>2007, vce(hac nw 6) 
ivregress 2sls FD.eubs10 Crd_diff_ercy_34 `controls' if year>2007, vce(hac nw 6) 
reg FD.eubs5 Crd_diff_ercy_43 `controls' if year>2007

ivregress 2sls D.bpbs10 I_net_USDGBP `controls' if year>2007, vce(hac nw 6) 
reg .bpbs10 I_net_USDGBP `controls' if year>2007
twoway (tsline D.bpbs10) (tsline I_net_USDGBP,yaxis(2))

tsline I_net_USDGBP
twoway (tsline I_net_USDEUR) (tsline I_net_USDGBP,yaxis(2))
gen I3=I_net_USDGBP+L.I_net_USDGBP+L2.I_net_USDGBP
gen B3=
reg D1.bpbs10 I3 `controls' if year>2007


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