
cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
cd "\Users\gliao\Dropbox\Research\ccy basis\creditmigration"


clear matrix
adopath + .
clear
set more off

use sdc96_clean2,clear
count
drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" 
* drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Asset Bkd Certs" 
* drop if secur=="Asset Backd Nts" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" | secur=="Asset Bkd Bonds"
count
drop if amt==.
drop if amt<50
drop if ytofm<2
drop if nrating>16
count
keep if pub=="Public" | pub=="Sub."
*keep if pub="Private"
drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")

count
drop if nrating==0
drop if nrating==1
 drop if ytofm==.
* miscoded & perpetual
drop if ytofm>=99
count

* br i desc ytofm d settlement2 mat2 amt if year==2016 & month==1 & ccy=="EUR" & modnat=="United States"
* br i desc ytofm d settlement2 mat2 amt sic2 Euop if year==2016 & month==1 & ccy=="USD" & modnat=="Eurozone"

* decode Euop, gen(uop)
* drop if uop=="Acquisition Fin."
* drop if uop=="Leveraged Buyout Acquisition Fin."
* br i desc ytofm d settlement2 mat2 if ytofm==.
save sdc96_clean3.dta,replace


use bbg_prices_daily_clean.dta,clear
gen dow=dow(date)
keep if dow==5
gen weekinyear=week(date)
gen weekly=yw(year,weekinyear)
format weekly %tw
sort weekly
duplicates drop weekly, force
save bbg_prices_weekly.dta, replace

capture program drop weeklyicollapse
quietly:weeklyicollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
* quietly:weeklyicollapse "USD" "CHF" "us" "ch" "United States" "Switzerland" amt modnat
quietly:weeklyicollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:weeklyicollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:weeklyicollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
* quietly:icollapse "GBP" "EUR" "gb" "eu" "United Kingdom" "Eurozone" amt modnat
* quietly:icollapse "JPY" "EUR" "jp" "eu" "Japan" "Eurozone" amt modnat
* quietly:icollapse "EUR" "AUD" "eu" "au" "Eurozone" "Australia" amt modnat

* merge all monthly issuance data together
use temp_issuancew_USDEUR.dta,clear
quietly:merge 1:1 weekly using temp_issuancew_USDGBP.dta,nogen
quietly:merge 1:1 weekly using temp_issuancew_USDJPY.dta,nogen
quietly:merge 1:1 weekly using temp_issuancew_USDAUD.dta,nogen


merge 1:1 weekly using bbg_prices_weekly.dta
drop _merge
drop if year<1996
tsset weekly
gen chgeubs10=eubs10-L.eubs10
drop if date>=d(01mar2016)
drop if date>=d(01oct2015)
save regdata_02w.dta,replace

use regdata_02w.dta,clear
drop if year<2008
collapse (mean) I_net_USDEUR chgeubs10,by(weekinyear)
tsset weekinyear
tsline I_net_USDEUR chgeubs10 


