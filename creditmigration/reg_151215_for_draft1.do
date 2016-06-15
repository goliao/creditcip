
use sdc96_clean2,clear

drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" 
* drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Asset Bkd Certs" 
* drop if secur=="Asset Backd Nts" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" | secur=="Asset Bkd Bonds"
drop if amt==.
drop if amt<50
drop if ytofm<2
drop if nrating>16
keep if pub=="Public" | pub=="Sub."
drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")

drop if nrating==0
drop if nrating==1
save sdc96_clean3.dta,replace

capture program drop icollapse
quietly:icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
quietly:icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat


* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
quietly:merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen

merge 1:1 monthly using prices_extended.dta,nogen
tsset monthly
gen date=dofm(monthly)
format date %td
drop if year==.

gen post07=1 if year>=2008
replace post07=0 if post07==.
save regdata_02.dta,replace

use regdata_02.dta,clear

local controls ""

estpost summarize I_net_USD??? 
rm test.csv
esttab using "test.csv",replace

capture program drop ireg5_univariate
local filename "30_05"
rm reg_30_05.csv
quietly: ireg5_univariate "USD" "EUR" "us" "eu" "30" `filename' `controls'
quietly: ireg5_univariate "USD" "GBP" "us" "gb" "30" `filename' `controls'
quietly: ireg5_univariate "USD" "JPY" "us" "jp" "30" `filename' `controls'
quietly: ireg5_univariate "USD" "AUD" "us" "au" "30" `filename' `controls'
* quietly: ireg3 "USD" "CAD" "us" "ca" "30" "cdbs10" "30_01" D.adbs1 `controls'


capture program drop ireg5_multivariate
local filename "30_05_m"
rm reg_`filename'.csv
quietly: ireg5_multivariate "USD" "EUR" "us" "eu" "30" `filename' "ussw10 ratediff_euus cy30_govtoas eurchg1"
quietly: ireg5_multivariate "USD" "GBP" "us" "gb" "30" `filename' "ussw10 ratediff_bpus cy30_govtoas gbpchg1"
quietly: ireg5_multivariate "USD" "JPY" "us" "jp" "30" `filename' "ussw10 ratediff_jyus cy30_govtoas jpychg1"
quietly: ireg5_multivariate "USD" "AUD" "us" "au" "30" `filename' "ussw10 ratediff_auus cy30_govtoas audchg1"


local controls "post07"
capture program drop iregbasis2
rm reg_basis30_02.csv
quietly: iregbasis2 "USD" "EUR" "us" "eu" "30" "eubs" "basis30_02" `controls'
quietly: iregbasis2 "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_02" `controls'
quietly: iregbasis2 "USD" "JPY" "us" "jp" "30" "jybs" "basis30_02" `controls'
quietly: iregbasis2 "USD" "AUD" "us" "au" "30" "adbs" "basis30_02" `controls'


local controls "post07"
capture program drop iregbasis2
rm reg_basis30_01_m2.csv
quietly: iregbasis2 "USD" "EUR" "us" "eu" "30" "eubs" "basis30_01_m2" "post07 ussw10 ratediff_euus eurchg1"
quietly: iregbasis2 "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_01_m2" "post07 ussw10 ratediff_bpus gbpchg1"
quietly: iregbasis2 "USD" "JPY" "us" "jp" "30" "jybs" "basis30_01_m2" "post07 ussw10 ratediff_jyus jpychg1"
quietly: iregbasis2 "USD" "AUD" "us" "au" "30" "adbs" "basis30_01_m2" "post07 ussw10 ratediff_jyus audchg1"



**** Forecasting of 
drop eubs10chg3
gen eubs10chg3=F3.eubs10-eubs10
gen eubs10chg6=F6.eubs10-eubs10
* reg eubs10chg3 Cdif_euus_30_eff post07
* reg eubs10chg6 Cdif_euus_30_eff post07
* reg eubs10chg3 Cdif_euus_30_eff_3m post07
* neweymod eubs10chg6 Cdif_euus_30 post07, lag(6)
neweymod eubs10chg6 Cdif_euus_30_eff post07, lag(6)
reg eubs10chg6 Cdif_euus_30_eff
gen eurchg6=log(F6.eur/eur)
reg eurchg6 Cdif_euus_30 post07

* ivreg2 eubs10chg6 Cdif_euus_30_eff post07, robust bw(auto) 
* problem
* neweymod eubs10chg6 Cdif_euus_30 eubs10 post07,lag(6)

drop bpbs10chg3
gen bpbs10chg3=F3.bpbs10-bpbs10
gen bpbs10chg6=F6.bpbs10-bpbs10
neweymod bpbs10chg6 Cdif_gbus_30_eff post07, lag(6)

drop jybs10chg?
gen jybs10chg3=F3.jybs10-jybs10
gen jybs10chg6=F3.jybs10-jybs10
neweymod jybs10chg6 Cdif_jpus_30_eff post07, lag(6)

drop adbs10chg3
gen adbs10chg3=F3.adbs10-adbs10
gen adbs10chg6=F6.adbs10-adbs10
neweymod adbs10chg6 Cdif_auus_30_eff post07, lag(6)





* quietly: ireg3 "USD" "CAD" "us" "ca" "30" "cdbs10" "30_01_m" D.adbs1 `controls'
* graph twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_30_effective,yaxis(2)) if year>=2002, legend(label(1 "issuancePct6m_EU->US") lab(2 "Credit Spread Diff EUR-USD")) ttitle("") ytitle("Percent") ytitle("basis points",axis(2)) 
* graph twoway tsline er30_govtoas cy30_govtoas if year>=2002, legend(label(1 "Eur Corp A OAS Spread") lab(2 "USD Corp A OAS Spread")) ttitle("") ytitle("basis points")

graph export "figures/eurissuancecredit.eps",replace
* graph twoway (tsline F.I_net_USDEUR_6mf) (tsline Cdif_euus_30_effective,yaxis(2)) if year>=2002, legend(label(1 "Issuance6m_EU->US") lab(2 "Credit Spread Diff EUR-USD")) ttitle(none) ytitle("Issuance in billion dollars") ytitle("basis points",axis(2)) 
* generate graph of credit differential and issuance flow

su I_net_USDEUR
br I_net_USDEUR
br monthly I_USDeu I_EURus I_USD_tot I_EUR_tot

su I_USDeu I_EURus I_USD_tot I_EUR_tot

* * generate graph of issuance and basis
graph twoway (tsline I_net_USDEUR) (tsline eubs10,yaxis(2)) if year>=2006, legend(label(1 "Issuance_EU->US") lab(2 "Euribor/Libor XC basis")) ttitle("") ytitle("Issuance in billion dollars") ytitle("xc basis: euribor+X vs libor",axis(2))
graph export "figures/eurissuancebasis.eps",replace

* graph twoway (tsline I_net_USDJPY) (tsline jybs5,yaxis(2)) if year>=1996, legend(label(1 "Issuance_JP->US") lab(2 "Yen Libor/Libor XC basis")) ttitle(none) ytitle("Issuance in billion dollars") ytitle("xc basis: euribor+X vs libor",axis(2)) 

* drop eurchgf3
* gen eurchgf3=log(F3.eur/eur)
* reg eurchg1 I_net_USDEUR
