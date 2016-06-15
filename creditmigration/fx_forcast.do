
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

save sdc96_clean3.dta,replace

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

ds hkd


gen lcaud=log(F.aud/aud)
gen lccad=log(F.cad/cad)
gen lceur=log(F.eur/eur)
gen lcgbp=log(F.gbp/gbp)
gen lcjpy=log(F.jpy/jpy)

gen lcaud3=log(F3.aud/aud)
gen lccad3=log(F3.cad/cad)
gen lceur3=log(F3.eur/eur)
gen lcgbp3=log(F3.gbp/gbp)
gen lcjpy3=log(F3.jpy/jpy)
* 

reg lcaud I_net_USDAUD 
reg lccad I_net_USDCAD
reg lceur I_net_USDEUR
reg lcgbp I_net_USDGBP
reg lcjpy I_net_USDJPY


reg lcaud I_AUDus
reg lccad I_CADus
reg lceur I_EURus
reg lcgbp I_GBPus
reg lcjpy I_JPYus


reg lcaud3 F2.I_net_USDAUD 
reg lccad F.I_net_USDCAD
reg lceur F.I_net_USDEUR
reg lcgbp F.I_net_USDGBP
reg lcjpy F.I_net_USDJPY


gen Cdif_auus_30_effective=Cdif_auus_30-adbs5
drop cdif3m
gen cdif3m=(Cdif_auus_30+L.Cdif_auus_30+L2.Cdif_auus_30)

reg lcaud3 Cdif_auus_30
reg lcaud3 cdif3m


reg lcaud L.Cdif_auus_30_effective
reg lccad F.I_net_USDCAD

