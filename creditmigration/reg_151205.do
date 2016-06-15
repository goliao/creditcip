
use sdc96_clean2,clear

drop if tf_mid_desc=="Government Sponsored Enterprises"
drop if secur=="Cum Red Pfd Shs" | secur=="Non-Cum Pref Sh" | secur=="Preferred Shs" | secur=="Pfd Stk,Com Stk" 
drop if amt==.
drop if amt<50
drop if ytofm<1
keep if pub=="Public" | pub=="Sub." 
drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")
drop if inlist(mdealtype,"FP","ASPP")
* drop if inlist(mdealtype,"HSDJP","SHD")
drop if inlist(issue_type_desc,"Asset-backed","Mortgage-backed","Agency, Supranational, Sovereign")
keep if inlist(ccy,"USD","EUR","AUD","JPY","CAD")
* drop if nrating==0
* drop if nrating==1
* keep if nrating<6
save sdc96_clean3.dta,replace

* insheet using secur_type.csv, clear
* drop if delete==1
* drop if amtsum1=="NA"
* keep secur 
* save secur_type.dta, replace

* merge 1:m secur using sdc96_clean3.dta
* keep if _merge==3
* drop _merge
* save sdc96_clean3.dta, replace

* br i secur descr mdealtype issue_type_desc * if secur=="Comm Mtg PT Crt" 
* br i secur descr mdealtype issue_type_desc * if nrating==0
* tab secur foreign if mdealtype=="SHD"
capture program drop icollapse
quietly:icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
quietly:icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
* quietly:icollapse "USD" "CAD" "us" "ca" "United States" "Canada" amt modnat

* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
quietly:merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen

merge 1:1 monthly using prices_extended.dta,nogen
tsset monthly
gen date=dofm(monthly)
format date %td
drop if year==.

gen post07=1 if year>=2008
replace post07=0 if post07==.

gen eurchg=log(eur/L6.eur)
gen gbpchg=log(gbp/L6.gbp)
gen jpychg=log(jpy/L6.jpy)
gen audchg=log(aud/L6.aud)

gen eurchg1=log(eur/L1.eur)
gen gbpchg1=log(gbp/L1.gbp)
gen jpychg1=log(jpy/L1.jpy)
gen audchg1=log(aud/L1.aud)

save regdata_04.dta,replace


use regdata_04.dta,clear

capture program drop ireg3
local controls ""
rm reg_30_04.csv
quietly: ireg3 "USD" "EUR" "us" "eu" "30" "eubs10" "30_04" D.eubs1 `controls'
quietly: ireg3 "USD" "GBP" "us" "gb" "30" "bpbs10" "30_04" D.bpbs1 `controls'
quietly: ireg3 "USD" "JPY" "us" "jp" "30" "jybs10" "30_04" D.jybs1 `controls'
quietly: ireg3 "USD" "AUD" "us" "au" "30" "adbs10" "30_04" D.adbs1 `controls'
* quietly: ireg3 "USD" "CAD" "us" "ca" "30" "cdbs10" "30_04" D.cdbs1 `controls'


capture program drop ireg4
rm reg_30_04_m4.csv
quietly: ireg4 "USD" "EUR" "us" "eu" "30" "eubs10" "30_04_m4" D.eubs1 "ratediff_euus ussw10 eurchg"
quietly: ireg4 "USD" "GBP" "us" "gb" "30" "bpbs10" "30_04_m4" D.bpbs1 "ratediff_bpus ussw10 gbpchg"
quietly: ireg4 "USD" "JPY" "us" "jp" "30" "jybs10" "30_04_m4" D.jybs1 "ratediff_jyus ussw10 jpychg"
quietly: ireg4 "USD" "AUD" "us" "au" "30" "adbs10" "30_04_m4" D.adbs1 "ratediff_auus ussw10 audchg"


local controls "post07"
capture program drop iregbasis2
rm reg_basis30_04.csv
quietly: iregbasis2 "USD" "EUR" "us" "eu" "30" "eubs" "basis30_04" `controls'
quietly: iregbasis2 "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_04" `controls'
quietly: iregbasis2 "USD" "JPY" "us" "jp" "30" "jybs" "basis30_04" `controls'
quietly: iregbasis2 "USD" "AUD" "us" "au" "30" "adbs" "basis30_04" `controls'


local controls "post07"
capture program drop iregbasis2
rm reg_basis30_04_m2.csv
quietly: iregbasis2 "USD" "EUR" "us" "eu" "30" "eubs" "basis30_04_m2" "post07 ussw10 ratediff_euus eurchg1"
quietly: iregbasis2 "USD" "GBP" "us" "gb" "30" "bpbs" "basis30_04_m2" "post07 ussw10 ratediff_bpus gbpchg1"
quietly: iregbasis2 "USD" "JPY" "us" "jp" "30" "jybs" "basis30_04_m2" "post07 ussw10 ratediff_jyus jpychg1"
quietly: iregbasis2 "USD" "AUD" "us" "au" "30" "adbs" "basis30_04_m2" "post07 ussw10 ratediff_jyus audchg1"


tsline  cy30_govtoas er30_govtoas jc30_govtoas ac30_govtoas
tsline  cy34_govtoas er34_govtoas jc30_govtoas ac30_govtoas

graph twoway tsline Cdif_euus_30 Cdif_gbus_30 Cdif_jpus_30 Cdif_auus_30 if year>1995, legend(lab(1 "EU-US OAS diff") lab(2 "GB-US OAS diff") lab(3 "JP-US OAS diff") lab(4 "AU-US OAS diff")) ttitle("") ytitle("basis points")
graph export "figures/creditsprds.eps",replace
graph twoway tsline  I_net_USDEUR  I_net_USDGBP I_net_USDJPY I_net_USDAUD if year>1995 , legend(lab(1 "Issuance_EU->US") lab(2 "Issuance_GB->US") lab(3 "Issuance_JP->US") lab(4 "Issuance_AU->US")) ttitle("") ytitle("USD billion")
graph export "figures/IssuanceFlow.eps",replace

tsline Cdif_euus_34 Cdif_gbus_34 Cdif_euus_30 Cdif_gbus_30
tsline i_net_USDEUR_6mf i_net_USDJPY_6mf i_net_USDAUD_6mf i_net_USDGBP_6mf


* drop if year==.
* twoway (tsline F.i_net_USDEUR_6mf)  (tsline Cdif_euus_30,yaxis(2)) if year>2002

* twoway (tsline F.i_net_USDJPY_6mf)  (tsline Cdif_jpus_30,yaxis(2)) if year>1995
* twoway (tsline F.I_net_USDJPY_6mf)  (tsline Cdif_jpus_30,yaxis(2)) if year>1995

* twoway (tsline F.I_net_USDAUD_6mf)  (tsline Cdif_auus_30,yaxis(2)) if year>1995
* twoway (tsline F.i_net_USDAUD_6mf)  (tsline Cdif_auus_30,yaxis(2)) if year>1995














graph twoway tsline eubsc eubs10 if year>2005
* quietly: ireg3 "USD" "CAD" "us" "ca" "30" "cdbs10" "30_01_m" D.adbs1 `controls'
* graph twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_30_effective,yaxis(2)) if year>=2002, legend(label(1 "issuancePct6m_EU->US") lab(2 "Credit Spread Diff EUR-USD")) ttitle("") ytitle("Percent") ytitle("basis points",axis(2)) 
* graph twoway tsline er30_govtoas cy30_govtoas if year>=2002, legend(label(1 "Eur Corp A OAS Spread") lab(2 "USD Corp A OAS Spread")) ttitle("") ytitle("basis points")
* graph export "figures/eurissuancecredit.eps",replace
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




* su I_net_USD??? I_???us I_USD??
ds I_net_USD*
* drop I_net_USD*mf
rename I_net_USDEUR I_net_USD1
rename I_net_USDGBP I_net_USD2
rename I_net_USDJPY I_net_USD3
rename I_net_USDAUD I_net_USD4
rename I_net_USDCAD I_net_USD5

rename I_net_USDEUR_6mf I_net_USD6mf1
rename I_net_USDGBP_6mf I_net_USD6mf2
rename I_net_USDJPY_6mf I_net_USD6mf3
rename I_net_USDAUD_6mf I_net_USD6mf4
rename I_net_USDCAD_6mf I_net_USD6mf5

ds Cdif_??us_30
rename Cdif_euus_30 Cdif30us1
rename Cdif_gbus_30 Cdif30us2
rename Cdif_jpus_30 Cdif30us3
rename Cdif_auus_30 Cdif30us4
rename Cdif_caus_30 Cdif30us5

ds ??bs10
rename eubs10 bs101
rename bpbs10 bs102
rename jybs10 bs103
rename adbs10 bs104
rename cdbs10 bs105
reshape long I_net_USD I_net_USD6mf Cdif30us bs10, i(monthly) j(ctry)

keep monthly I_net_USD I_net_USD6mf Cdif30us bs10 ctry year month 

drop if year<2002
drop if year==2015
neweymod F.I_net_USD Cdif30us, lag(6)
ds 
codebook 
br
local controls ""

help tsset
xtset ctry monthly

br 
newey F.I_net_USD Cdif30us, lag(6)

xi:reg bs10 I_net_USD i.ctry i.ctry*I_net_USD if year>2007

tab year
