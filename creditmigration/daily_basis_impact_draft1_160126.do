
clear matrix
adopath + .
clear
set more off

* use sdc96_clean2,clear

* drop if tf_mid_desc=="Government Sponsored Enterprises"
* drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" 
* drop if amt==.
* drop if amt<50
* drop if ytofm<1
* keep if pub=="Public" | pub=="Sub." 
* drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")
* drop if inlist(mdealtype,"FP","ASPP")
* * drop if inlist(mdealtype,"HSDJP","SHD")
* drop if inlist(issue_type_desc,"Asset-backed","Mortgage-backed","Agency, Supranational, Sovereign")
* keep if inlist(ccy,"USD","GBP","EUR","AUD","JPY","CAD")
* drop if nrating==0
* drop if nrating==1
* * keep if nrating<6
* save sdc96_clean3.dta,replace


* capture program drop weeklyicollapse
* weeklyicollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modupnat
* tab weekinyear,su(I_USD_tot)
* collapse (mean) I_*, by(weekinyear)
* twoway bar I_USD_tot weekinyear

* * insheet using secur_type.csv, clear
* * drop if delete==1
* * drop if amtsum1=="NA"
* * keep secur 
* * save secur_type.dta, replace

* * merge 1:m secur using sdc96_clean3.dta
* * keep if _merge==3
* * drop _merge
* * save sdc96_clean3.dta, replace

* * br i secur descr mdealtype issue_type_desc * if secur=="Comm Mtg PT Crt" 
* * br i secur descr mdealtype issue_type_desc * if nrating==0
* * tab secur foreign if mdealtype=="SHD"

use sdc96_clean3.dta,clear
capture program drop dailyicollapse

quietly:dailyicollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
quietly:dailyicollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:dailyicollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:dailyicollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat

* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
quietly:merge 1:1 d using temp_issuance_USDGBP.dta,nogen
quietly:merge 1:1 d using temp_issuance_USDJPY.dta,nogen
quietly:merge 1:1 d using temp_issuance_USDAUD.dta,nogen



rename d date
* use bbg_prices_daily_clean.dta,clear
merge 1:1 date using bbg_prices_daily_clean.dta
keep if _merge==3
drop _merge
save temp_reg_daily.dta, replace

use temp_reg_daily.dta,clear
* tsset date

gen time = _n
tsset time

* su D.eubs10 if I_net_USDEUR>0
* su D.eubs10 if I_net_USDEUR<0
* su D.eubs10 if D.I_net_USDEUR>0
* su D.eubs10 if D.I_net_USDEUR<0

* * drop if I_net_USDEUR==0
* codebook I_net_USDEUR
* * drop if abs(I_net_USDEUR)<.1
* twoway scatter D.eubs10 I_net_USDEUR

replace eur=1/eur
replace gbp=1/gbp
replace aud=1/aud
replace nzd=1/nzd


rm reg_dailybasisreg.csv
estimates clear
capture program drop dailyregbasis
dailyregbasis eubs10 USDEUR eur "dailybasisreg"
dailyregbasis bpbs10 USDGBP gbp "dailybasisreg"
dailyregbasis jybs10 USDJPY jpy "dailybasisreg"
dailyregbasis adbs10 USDAUD aud "dailybasisreg"


coefplot (eur, asequation(-5)) (eurL4D, asequation(-4)) (eurL3D, asequation(-3)) (eurL2D, asequation(-2)) (eurLD, asequation(-1)) (eurD, asequation(0)) (eurFD, asequation(1)) (eurF2D, asequation(2)) (eurF3D, asequation(3)) (eurF4D, asequation(4)) (eurF5D, asequation(5)) || (gbp, asequation(-5)) (gbpL4D, asequation(-4)) (gbpL3D, asequation(-3)) (gbpL2D, asequation(-2)) (gbpLD, asequation(-1)) (gbpD, asequation(0)) (gbpFD, asequation(1)) (gbpF2D, asequation(2)) (gbpF3D, asequation(3)) (gbpF4D, asequation(4)) (gbpF5D, asequation(5)) || (jpy, asequation(-5)) (jpyL4D, asequation(-4)) (jpyL3D, asequation(-3)) (jpyL2D, asequation(-2)) (jpyLD, asequation(-1)) (jpyD, asequation(0)) (jpyFD, asequation(1)) (jpyF2D, asequation(2)) (jpyF3D, asequation(3)) (jpyF4D, asequation(4)) (jpyF5D, asequation(5))|| (aud, asequation(-5)) (audL4D, asequation(-4)) (audL3D, asequation(-3)) (audL2D, asequation(-2)) (audLD, asequation(-1)) (audD, asequation(0)) (audFD, asequation(1)) (audF2D, asequation(2)) (audF3D, asequation(3)) (audF4D, asequation(4)) (audF5D, asequation(5)), vertical drop(_cons) yline(0) mcolor(green) ciopt(lcolor(green)) swapnames byopts(legend(off) cols(2) yrescale compact) ytitle(bps) xtitle(t)

var D.eubs10 I_net_USDEUR
varbasic D.eubs10 I_net_USDEUR

*************************************************************
rm reg_dailybasisreg.csv
local basis eubs10
local CCY USDEUR
eststo: reg L7D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L7D
eststo: reg L6D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L6D
eststo: reg L5D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L5D
eststo: reg L4D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L4D
eststo: reg L3D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L3D
eststo: reg L2D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store L2D
eststo: reg LD.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store LD
eststo: reg D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store D
eststo: reg FD.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store FD
eststo: reg F2D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F2D
eststo: reg F3D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F3D
eststo: reg F4D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F4D
eststo: reg F5D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F5D
eststo: reg F6D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F6D
eststo: reg F7D.`basis' I_net_`CCY' if abs(I_net_`CCY')>0.1
estimates store F7D

coefplot (L5D, asequation(-5)) (L4D, asequation(-4)) (L3D, asequation(-3)) (L2D, asequation(-2)) (LD, asequation(-1)) (D, asequation(0)) (FD, asequation(1)) (F2D, asequation(2)) (F3D, asequation(3)) (F4D, asequation(4)) (F5D, asequation(5)) , vertical drop(_cons) yline(0)  mcolor(green) ciopt(lcolor(green))  swapnames 





coefplot (L5D, label(t=-5)) (L4D, label(t=-4)) ,  drop(_cons) xline(0)