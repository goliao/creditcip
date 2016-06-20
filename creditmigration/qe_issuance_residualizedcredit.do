




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


capture program drop icollapse
quietly:icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
* quietly:icollapse "USD" "CHF" "us" "ch" "United States" "Switzerland" amt modnat
quietly:icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
quietly:icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
quietly:icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
* quietly:icollapse "GBP" "EUR" "gb" "eu" "United Kingdom" "Eurozone" amt modnat
* quietly:icollapse "JPY" "EUR" "jp" "eu" "Japan" "Eurozone" amt modnat
* quietly:icollapse "EUR" "AUD" "eu" "au" "Eurozone" "Australia" amt modnat

* merge all monthly issuance data together
use temp_issuance_USDEUR.dta,clear
quietly:merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
quietly:merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_JPYEUR.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_GBPEUR.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_USDCHF.dta,nogen
* quietly:merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen

* good old one is better
* merge 1:1 monthly using prices_extended_160207.dta,nogen
merge 1:1 monthly using prices_extended.dta,nogen
drop date
tsset monthly
gen date=dofm(monthly)
format date %td
drop if year==.
drop _merge
merge 1:1 monthly using bdlev.dta
sort monthly
tsset monthly

drop quarter
gen quarter=ceil(month/3)
gen mofq=1 if month==1 | month==4 | month==7 | month==10
replace mofq=2 if month==2 | month==5 | month==8 | month==11
replace mofq=3 if month==3 | month==6 | month==9 | month==12

drop if date>=d(01mar2016)
drop if date>=d(01oct2015)
save regdata_02.dta,replace


****

use temp_ys.dta,clear
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
tsset monthly
rename ccyeur ccyeur_eff
rename ccyjpy ccyjpy_eff
rename ccygbp ccygbp_eff
rename ccyaud ccyaud_eff
* keep monthly ccyeur_eff
save residualizedys2.dta,replace

use regdata_02.dta,clear
drop _merge
merge 1:1 monthly using residualizedys2.dta
egen i_net_USDEUR_mean_1=mean(i_net_USDEUR) if year>2005 & year<2009 
egen i_net_USDEUR_mean_2=mean(i_net_USDEUR) if year>=2009 & year<2014 
egen i_net_USDEUR_mean_3=mean(i_net_USDEUR) if year>=2014 & year<2016
drop if year>2016

tsset monthly
gen ccyeur_eff_6m=(ccyeur_eff+L.ccyeur_eff+L2.ccyeur_eff+L3.ccyeur_eff+L4.ccyeur_eff+L5.ccyeur_eff)/6
gen ccyjpy_eff_6m=(ccyjpy_eff+L.ccyjpy_eff+L2.ccyjpy_eff+L3.ccyjpy_eff+L4.ccyjpy_eff+L5.ccyjpy_eff)/6
gen ccygbp_eff_6m=(ccygbp_eff+L.ccygbp_eff+L2.ccygbp_eff+L3.ccygbp_eff+L4.ccygbp_eff+L5.ccygbp_eff)/6
gen ccyaud_eff_6m=(ccyaud_eff+L.ccyaud_eff+L2.ccyaud_eff+L3.ccyaud_eff+L4.ccyaud_eff+L5.ccyaud_eff)/6


* new plot
tsline i_net_USDEUR if year>2005 & year<2009, recast(sc) mc(green) || tsline i_net_USDEUR if year>=2009 & year<2014, recast(sc) mc(red) msymbol(+)  || tsline i_net_USDEUR if year>=2014 & year<2016, recast(sc) mc(blue) msymbol(d)  || tsline i_net_USDEUR_mean_1 if year>2005,lc(green) || tsline i_net_USDEUR_mean_2 if year>2005,lc(red) || tsline i_net_USDEUR_mean_3 if year>2005,lc(blue) || tsline ccyeur_eff_6m if year>2005, lc(black) yaxis(2) yscale(range(-30,30) axis(2)) yscale(range(-20,15) axis(1))  plotregion(margin(small)) graphregion(margin(l+5 r+5)) ytitle("monthly issuance flow (EU to US) as % of total") ytitle("residualized effective credit spread 6m avg (bps)",axis(2)) legend(label(1 "issflowPct pre '09") label(2 "issflowPct during FED QE") label(3 "issflowPct during ECB QE") label(7 "Resid. CrdSprdEff6m") order(1 2 3 7)) ttitle("")

* graph export "../paper/figures/crd_is_qe_euus.eps",replace

neweymod F.i_net_USDEUR ccyeur_eff, lag(6)
neweymod F.i_net_USDJPY ccyjpy_eff, lag(6)
neweymod F.i_net_USDGBP ccygbp_eff, lag(6)
neweymod F.i_net_USDAUD ccyaud_eff, lag(6)
save plotdata_qe_crd_iss.dta

tsline i_net_USDJPY if year>2005 & year<2009, recast(sc) mc(green) || tsline i_net_USDJPY if year>=2009 & year<2014, recast(sc) mc(red) msymbol(+)  || tsline i_net_USDJPY if year>=2014 & year<2016, recast(sc) mc(blue) msymbol(d)  || tsline ccyjpy_eff_6m if year>2005, lc(black) yaxis(2) yscale(range(-30,30) axis(2)) yscale(range(-20,15) axis(1))  plotregion(margin(small)) graphregion(margin(l+5 r+5)) ytitle("monthly issuance flow (EU to US) as % of total") ytitle("residualized effective credit spread 6m avg (bps)",axis(2)) legend(label(1 "issflowPct pre '09") label(2 "issflowPct during FED QE") label(3 "issflowPct during ECB QE") label(7 "Resid. CrdSprdEff6m") order(1 2 3 7)) ttitle("")


tsline i_net_USDGBP if year>2005 & year<2009, recast(sc) mc(green) || tsline i_net_USDGBP if year>=2009 & year<2014, recast(sc) mc(red) msymbol(+)  || tsline i_net_USDGBP if year>=2014 & year<2016, recast(sc) mc(blue) msymbol(d)  || tsline ccygbp_eff_6m if year>2005, lc(black) yaxis(2) yscale(range(-30,30) axis(2)) yscale(range(-20,15) axis(1))  plotregion(margin(small)) graphregion(margin(l+5 r+5)) ytitle("monthly issuance flow (EU to US) as % of total") ytitle("residualized effective credit spread 6m avg (bps)",axis(2)) legend(label(1 "issflowPct pre '09") label(2 "issflowPct during FED QE") label(3 "issflowPct during ECB QE") label(7 "Resid. CrdSprdEff6m") order(1 2 3 7)) ttitle("")

tsline i_net_USDAUD if year>2005 & year<2009, recast(sc) mc(green) || tsline i_net_USDAUD if year>=2009 & year<2014, recast(sc) mc(red) msymbol(+)  || tsline i_net_USDAUD if year>=2014 & year<2016, recast(sc) mc(blue) msymbol(d)  || tsline ccyaud_eff_6m if year>2005, lc(black) yaxis(2) yscale(range(-30,30) axis(2)) yscale(range(-20,15) axis(1))  plotregion(margin(small)) graphregion(margin(l+5 r+5)) ytitle("monthly issuance flow (EU to US) as % of total") ytitle("residualized effective credit spread 6m avg (bps)",axis(2)) legend(label(1 "issflowPct pre '09") label(2 "issflowPct during FED QE") label(3 "issflowPct during ECB QE") label(7 "Resid. CrdSprdEff6m") order(1 2 3 7)) ttitle("")
* save temp_ys0.dta









