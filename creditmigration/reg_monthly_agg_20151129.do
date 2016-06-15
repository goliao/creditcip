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

gen rating=sp
replace rating=mdy if rating=="" | rating=="NR"
merge m:1 rating using rating.dta
drop _merge
save sdc96_clean2.dta,replace

*/

* use sdc96_clean2,clear
* keep if foreign==1
* keep i
* duplicates drop 
* gen globalissuer=1
* save globalissuer.dta,replace


use sdc96_clean2,clear

drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" 
* drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Asset Bkd Certs" 
* drop if secur=="Asset Backd Nts" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" | secur=="Asset Bkd Bonds"
drop if amt==.
drop if amt<50
drop if ytofm<1
keep if pub=="Public" | pub=="Sub."
drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")

drop if nrating==0
drop if nrating==1

* drop if nrating>=14
* drop if rule144a=="Y"
* drop if pub=="Govt."
* merge m:1 i using globalissuer.dta
* keep if _merge==3
* drop _merge
* drop if globalissuer==.
* drop if sic1==6

save sdc96_clean3.dta,replace

capture program drop icollapse
quietly:icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
quietly:icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
quietly:icollapse "USD" "CAD" "us" "ca" "United States" "Canada" amt modnat
* quietly:icollapse "EUR" "GBP" "eu" "gb" "Eurozone" "United Kingdom" amt modnat
* quietly:icollapse "EUR" "AUD" "eu" "au" "Eurozone" "Australia" amt modnat
* quietly:icollapse "EUR" "JPY" "eu" "jp" "Eurozone" "Japan" amt modnat
* quietly:icollapse "JPY" "AUD" "jp" "au" "Japan" "Australia" amt modnat
* quietly:replace i_EURau=0 if year==2007 &  i_EURau==.


* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
quietly:merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_EURGBP.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_EURAUD.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_EURJPY.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_JPYAUD.dta,nogen

merge 1:1 monthly using prices_extended.dta,nogen
tsset monthly
gen date=dofm(monthly)
format date %td
drop if year==.
save temp_regdata.dta,replace

use temp_regdata.dta,clear
gen post07=1 if year>=2008
replace post07=0 if post07==.

* collapse (sum) *, by(year qrt) fast
* gen ratediff=eusa10-ussw10
* gen swaprate=ussw10
* local controls "swaprate ratediff post07"
local controls ""

capture program drop ireg2
rm reg_36_17.csv
quietly: ireg2 "USD" "EUR" "us" "eu" "36" "eubs10" "36_17" D.eubs1 `controls'
quietly: ireg2 "USD" "GBP" "us" "gb" "34" "bpbs10" "36_17" D.bpbs1 `controls'
quietly: ireg2 "USD" "JPY" "us" "jp" "30" "jybs10" "36_17" D.jybs1 `controls'
quietly: ireg2 "USD" "AUD" "us" "au" "30" "adbs10" "36_17" D.adbs1 `controls'
quietly: ireg2 "USD" "CAD" "us" "ca" "36" "cdbs10" "36_17" D.adbs1 `controls'

* quietly: ireg2 "EUR" "AUD" "eu" "au" "40" "aebs10" "40_15" D.aebs1 `controls'
* quietly: ireg2 "EUR" "GBP" "eu" "gb" "40" "gebs10" "40_15" D.gebs1 `controls'
* quietly: ireg2 "EUR" "JPY" "eu" "jp" "40" "jebs10" "40_15" D.jebs1 `controls'
* quietly: ireg2 "JPY" "AUD" "jp" "au" "40" "ajbs10" "40_15" D.ajbs1 `controls'

local controls "post07"
capture program drop iregbasis
rm reg_basis30_5.csv
quietly: iregbasis "USD" "EUR" "us" "eu" "30" "eubs" "basis30_5" `controls'
quietly: iregbasis "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_5" `controls'
quietly: iregbasis "USD" "JPY" "us" "jp" "30" "jybs" "basis30_5" `controls'
quietly: iregbasis "USD" "AUD" "us" "au" "30" "adbs" "basis30_5" `controls'
quietly: iregbasis "USD" "CAD" "us" "ca" "30" "cdbs" "basis30_5" `controls'
* quietly: iregbasis "EUR" "AUD" "eu" "au" "30" "aebs" "basis30_5" `controls'
* quietly: iregbasis "EUR" "GBP" "eu" "gb" "30" "gebs" "basis30_5" `controls'
* quietly: iregbasis "EUR" "JPY" "eu" "jp" "30" "jebs" "basis30_5" `controls'
* quietly: iregbasis "JPY" "AUD" "jp" "au" "30" "ajbs" "basis30_5" `controls'


* ***** quarterly basis chg!!!
* collapse (last) *bs* Cdif* (mean) i_* (sum) I_*, by(qrt) fast
* tsset qrt
* ds *,v(32)
* capture program drop iregbasis
* rm reg_basis30_5.csv
* quietly: iregbasis "USD" "EUR" "us" "eu" "30" "eubs" "basis30_5" `controls'
* quietly: iregbasis "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_5" `controls'
* quietly: iregbasis "USD" "JPY" "us" "jp" "30" "jybs" "basis30_5" `controls'
* quietly: iregbasis "USD" "AUD" "us" "au" "30" "adbs" "basis30_5" `controls'
* * quietly: iregbasis "EUR" "AUD" "eu" "au" "30" "aebs" "basis30_5" `controls'
* * quietly: iregbasis "EUR" "GBP" "eu" "gb" "30" "gebs" "basis30_5" `controls'
* * quietly: iregbasis "EUR" "JPY" "eu" "jp" "30" "jebs" "basis30_5" `controls'
* * quietly: iregbasis "JPY" "AUD" "jp" "au" "30" "ajbs" "basis30_5" `controls'

* help tsline
* tsline I_USDEUR_tot if tin(01jan2002,31dec2014)
* tsline I_USDJPY_tot

* twoway (tsline Cdif_auus_?0) (tsline F.i_net_USDAUD_6m, yaxis(2))