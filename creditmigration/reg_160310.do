
cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
cd "\Users\gliao\Dropbox\Research\ccy basis\creditmigration"


clear matrix
adopath + .
clear
set more off

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
*keep if pub="Private"
drop if inlist(mdealtype,"P","ANPX","M","EP","CEP","TM","PP","R144P")

drop if nrating==0
drop if nrating==1
* br i amt nrating ytofm * if year==2002 & month==3 & modnat=="Eurozone" & ccy=="USD"
* count if ytofm==. & foreign==1
* count if nrating==. & foreign==1
 drop if ytofm==.
* miscoded & perpetual
drop if ytofm>=99

* br i desc ytofm d settlement2 mat2 if ytofm==.
save sdc96_clean3.dta,replace
* su ytofm if ccy=="EUR" & foreign==1
* su ytofm if modnat=="Eurozone" & foreign==1

* su ytofm if ccy=="EUR" & foreign==1 & ytofm<31
* su ytofm if modnat=="Eurozone" & foreign==1 & ytofm<31

* su ytofm if ccy=="GBP" & foreign==1
* su ytofm if modnat=="United Kingdom" & foreign==1

* su ytofm if ccy=="GBP" & foreign==1 & ytofm<31
* su ytofm if modnat=="United Kingdom" & foreign==1 & ytofm<31

* su ytofm if ccy=="JPY" & foreign==1
* su ytofm if modnat=="Japan" & foreign==1

* su ytofm if ccy=="JPY" & foreign==1 & ytofm<31
* su ytofm if modnat=="Japan" & foreign==1 & ytofm<31


* su ytofm if ccy=="AUD" & foreign==1
* su ytofm if modnat=="Australia" & foreign==1

* su ytofm if ccy=="AUD" & foreign==1 & ytofm<31
* su ytofm if modnat=="Australia" & foreign==1 & ytofm<31


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

drop if date>=d(01mar2016)
* drop if date>=d(01oct2015)
save regdata_02.dta,replace


use regdata_02.dta,clear

* Table 1: Summary Stats
    rm table1_summary.csv
    eststo clear
    estpost summarize I_net_USD??? i_net_USD??? cy30_govtoas er30_govtoas ur30_govtoas jc30_govtoas ac30_govtoas eubs10 bpbs10 jybs10 adbs10 ussw10 eusa10 bpsw10 jysw10 adsw10 Cdif_??us_30 Cdif_??us_30_eff
    * estpost summarize I_net_USD??? cy00_govtoas cy20_govtoas cy30_govtoas cy40_govtoas er00_govtoas er20_govtoas er30_govtoas er40_govtoas ur00_govtoas ur20_govtoas ur30_govtoas ur40_govtoas jc00_govtoas jc20_govtoas jc30_govtoas jc40_govtoas ac20_govtoas ac30_govtoas ac40_govtoas auc0_govtoas eubs10 bpbs10 jybs10 adbs10 ussw10 eusa10 bpsw10 jysw10 adsw10 eur gbp jpy aud
    esttab using "table1_summary.csv", title(Summary Statistics) cell((count(label(Count)) mean(label(Mean) fmt(a3)) sd(label(S.D.) fmt(a3)) min(label(Min) fmt(a3)) max(fmt(a3) label(Max)))) nonumber append
     
* Table 2: Univariate regressions for EUR
    local controls ""
    local filename "Table2_univariate_EUR"
    capture rm`filename'.csv
    local A USD
    local B EUR
    local crdsource "euus_30"
    eststo clear
    eststo: neweymod F.I_net_`A'`B' Cdif_`crdsource', lag(6)
    eststo: neweymod F.I_net_`A'`B' Cdif_`crdsource'_eff, lag(6)
    eststo: neweymod F.I_net_`A'`B'_6mf Cdif_`crdsource'_eff, lag(6)
    eststo: neweymod F.i_net_`A'`B' Cdif_`crdsource', lag(6)
    eststo: neweymod F.i_net_`A'`B' Cdif_`crdsource'_eff, lag(6)
    eststo: neweymod F.i_net_`A'`B'_6mf Cdif_`crdsource'_eff, lag(6)
    esttab using `filename'.csv, title("Univariate Forecasting") bracket r2 nostar nogaps nonote addnotes(" ") order(Cdif_`crdsource' Cdif_`crdsource'_eff) replace

* Table 3: Multivariate regressions for EUR
    local filename "Table3_multivariate_EUR"
    capture rm `filename'.csv
    local controls "ratediff_euus ussw10 cy30_govtoas eurchg"
    local A USD
    local B EUR
    local crdsource "euus_30"
    eststo clear
    eststo: neweymod F.I_net_`A'`B'_6mf Cdif_`crdsource'_eff I_net_`A'`B', lag(6)
    eststo: neweymod F.I_net_`A'`B'_6mf Cdif_`crdsource'_eff `controls', lag(6)
    * eststo: neweymod F.I_net_`A'`B' Cdif_`crdsource' I_net_`A'`B' `controls', lag(6)
    * eststo: neweymod F.I_net_`A'`B' Cdif_`crdsource'_eff I_net_`A'`B' `controls', lag(6)
    eststo: neweymod F.I_net_`A'`B'_6mf Cdif_`crdsource'_eff I_net_`A'`B' `controls', lag(6)
    * eststo: neweymod F.i_net_`A'`B' Cdif_`crdsource' i_net_`A'`B' `controls', lag(6)
    * eststo: neweymod F.i_net_`A'`B' Cdif_`crdsource'_eff i_net_`A'`B' `controls', lag(6)
    eststo: neweymod F.i_net_`A'`B'_6mf Cdif_`crdsource'_eff i_net_`A'`B' , lag(6)
    eststo: neweymod F.i_net_`A'`B'_6mf Cdif_`crdsource'_eff `controls', lag(6)
    eststo: neweymod F.i_net_`A'`B'_6mf Cdif_`crdsource'_eff i_net_`A'`B' `controls', lag(6)
    esttab using `filename'.csv, title("Multivariate Forecasting'") bracket r2 nostar nogaps nonote addnotes(" ") order(Cdif_`crdsource'_eff I_net_`A'`B' i_net_`A'`B' `controls') append

* Table 4: Multivariate regressions for all currencies
    local filename "Table4_multivariate_allccy"
    capture rm `filename'.csv
    eststo clear
    eststo: neweymod F.I_net_USDEUR_6mf Cdif_euus_30_eff I_net_USDEUR ratediff_euus eurchg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.I_net_USDGBP_6mf Cdif_gbus_30_eff I_net_USDGBP ratediff_gbus gbpchg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.I_net_USDJPY_6mf Cdif_jpus_30_eff I_net_USDJPY ratediff_jpus jpychg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.I_net_USDAUD_6mf Cdif_auus_30_eff I_net_USDAUD ratediff_auus audchg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.i_net_USDEUR_6mf Cdif_euus_30_eff i_net_USDEUR ratediff_euus eurchg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.i_net_USDGBP_6mf Cdif_gbus_30_eff i_net_USDGBP ratediff_gbus gbpchg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.i_net_USDJPY_6mf Cdif_jpus_30_eff i_net_USDJPY ratediff_jpus jpychg ussw10 cy30_govtoas, lag(6)
    eststo: neweymod F.i_net_USDAUD_6mf Cdif_auus_30_eff i_net_USDAUD ratediff_auus audchg ussw10 cy30_govtoas, lag(6)
    esttab using `filename'.csv, title("Multivariate Forecasting'") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(CrdSprdDiffEff Iss iss ratediff ussw10) append

* Table 5: Basis impact: EUR univariate and multivariate
    local filename "Table5_basis_eur"
    capture rm `filename'.csv
    local A USD
    local B EUR
    local a us
    local b eu
    local basis eubs
    drop post07
    gen post07=1 if year>=2008
    replace post07=0 if post07==.
    local controls eurchg1 ussw10 ratediff_euus post07
    eststo clear
    eststo: reg D.`basis'10 I_net_`A'`B'
    eststo: reg D.`basis'10 I_`A'`b'
    eststo: reg D.`basis'10 I_`B'`a'
    eststo: reg D.`basis'10 I_net_`A'`B' `controls'
    eststo: reg D.`basis'10 I_`A'`b' `controls'
    eststo: reg D.`basis'10 I_`B'`a' `controls'
    esttab using `filename'.csv, title("Basis") bracket r2 nostar nogaps nonote addnotes(" ") order(I_net_`A'`B' I_`A'`b' I_`B'`a' `controls') append
* previous version with overcontrols
*  * Table 6: All currencies: basis
*     local filename "Table6_basis_all"
*     capture rm `filename'.csv
*     eststo clear
*     eststo: reg D.eubs10 I_net_USDEUR eurchg1 ussw10 ratediff_euus post07 if abs(I_net_USDEUR)>0
*     eststo: reg D.eubs10 I_USDeu eurchg1 ussw10 ratediff_euus post07 if abs(I_USDeu)>0
*     eststo: reg D.eubs10 I_EURus eurchg1 ussw10 ratediff_euus post07 if abs(I_EURus)>0

*     eststo: reg D.bpbs10 I_net_USDGBP gbpchg1 ussw10 ratediff_gbus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs10 I_USDgb gbpchg1 ussw10 ratediff_gbus post07 if abs(I_USDgb)>0
*     eststo: reg D.bpbs10 I_GBPus gbpchg1 ussw10 ratediff_gbus post07 if abs(I_GBPus)>0

*     eststo: reg D.jybs10 I_net_USDJPY jpychg1 ussw10 ratediff_jpus post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs10 I_USDjp jpychg1 ussw10 ratediff_jpus post07 if abs(I_USDjp)>0
*     eststo: reg D.jybs10 I_JPYus jpychg1 ussw10 ratediff_jpus post07 if abs(I_JPYus)>0

*     eststo: reg D.adbs10 I_net_USDAUD audchg1 ussw10 ratediff_auus post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs10 I_USDau audchg1 ussw10 ratediff_auus post07 if abs(I_USDau)>0
*     eststo: reg D.adbs10 I_AUDus audchg1 ussw10 ratediff_auus post07 if abs(I_AUDus)>0

*     esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet I_USDeu IssUS I_USDgb IssUS I_USDjp IssUS I_USDau IssUS I_EURus IssF I_GBPus IssF I_JPYus IssF I_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append
drop post07
    gen post07=1 if year>=2008
    replace post07=0 if post07==.
 * Table 6: All currencies: basis
    local filename "Table6_basis_all_A"
    capture rm `filename'.csv
    eststo clear
    eststo: reg D.eubs10 I_net_USDEUR post07 if abs(I_net_USDEUR)>0
    eststo: reg D.eubs10 I_USDeu post07 if abs(I_net_USDEUR)>0
    eststo: reg D.eubs10 I_EURus post07 if abs(I_net_USDEUR)>0

    eststo: reg D.bpbs10 I_net_USDGBP post07 if abs(I_net_USDGBP)>0
    eststo: reg D.bpbs10 I_USDgb post07 if abs(I_net_USDGBP)>0
    eststo: reg D.bpbs10 I_GBPus post07 if abs(I_net_USDGBP)>0

    eststo: reg D.jybs10 I_net_USDJPY post07 if abs(I_net_USDJPY)>0
    eststo: reg D.jybs10 I_USDjp post07 if abs(I_net_USDJPY)>0
    eststo: reg D.jybs10 I_JPYus post07 if abs(I_net_USDJPY)>0

    eststo: reg D.adbs10 I_net_USDAUD post07 if abs(I_net_USDAUD)>0
    eststo: reg D.adbs10 I_USDau post07 if abs(I_net_USDAUD)>0
    eststo: reg D.adbs10 I_AUDus post07 if abs(I_net_USDAUD)>0

    esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet I_USDeu IssUS I_USDgb IssUS I_USDjp IssUS I_USDau IssUS I_EURus IssF I_GBPus IssF I_JPYus IssF I_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 nostar nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append



    local filename "Table6_basis_all_B"
    capture rm `filename'.csv
    eststo clear
    eststo: reg D.eubs10 i_net_USDEUR post07 if abs(I_net_USDEUR)>0
    eststo: reg D.eubs10 i_USDeu post07 if abs(I_net_USDEUR)>0
    eststo: reg D.eubs10 i_EURus post07 if abs(I_net_USDEUR)>0

    eststo: reg D.bpbs10 i_net_USDGBP post07 if abs(I_net_USDGBP)>0
    eststo: reg D.bpbs10 i_USDgb post07 if abs(I_net_USDGBP)>0
    eststo: reg D.bpbs10 i_GBPus post07 if abs(I_net_USDGBP)>0


    eststo: reg D.jybs10 i_net_USDJPY post07 if abs(I_net_USDJPY)>0
    eststo: reg D.jybs10 i_USDjp post07 if abs(I_net_USDJPY)>0
    eststo: reg D.jybs10 i_JPYus post07 if abs(I_net_USDJPY)>0

    eststo: reg D.adbs10 i_net_USDAUD post07 if abs(I_net_USDAUD)>0
    eststo: reg D.adbs10 i_USDau post07 if abs(I_net_USDAUD)>0
    eststo: reg D.adbs10 i_AUDus post07 if abs(I_net_USDAUD)>0

    esttab using `filename'.csv, title("Basis") rename(i_net_USDEUR IssNet i_net_USDGBP IssNet i_net_USDJPY IssNet i_net_USDAUD IssNet i_USDeu IssUS i_USDgb IssUS i_USDjp IssUS i_USDau IssUS i_EURus IssF i_GBPus IssF i_JPYus IssF i_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 nostar nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append

* * Table 6: All currencies: basis for different maturities
*     local filename "Table6_basis_all"
*     capture rm `filename'.csv
*     eststo clear
*     eststo: reg D.eubs5 I_net_USDEUR post07 if abs(I_net_USDEUR)>0
*     eststo: reg D.eubs7 I_net_USDEUR post07 if abs(I_net_USDEUR)>0
*     eststo: reg D.eubs10 I_net_USDEUR post07 if abs(I_net_USDEUR)>0
*     eststo: reg D.eubs15 I_net_USDEUR post07 if abs(I_net_USDEUR)>0

*     eststo: reg D.bpbs5 I_net_USDGBP post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs7 I_net_USDGBP post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs10 I_net_USDGBP post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs15 I_net_USDGBP post07 if abs(I_net_USDGBP)>0

* * inflow: longer duration to satify pension fund demand: good evidence.
*     eststo: reg D.bpbs5 I_GBPus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs7 I_GBPus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs10 I_GBPus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs15 I_GBPus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs20 I_GBPus post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs25 I_GBPus post07 if abs(I_net_USDGBP)>0
* * outflow: shorter duration
*     eststo: reg D.bpbs5 I_USDgb post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs7 I_USDgb post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs10 I_USDgb post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs15 I_USDgb post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs20 I_USDgb post07 if abs(I_net_USDGBP)>0
*     eststo: reg D.bpbs25 I_USDgb post07 if abs(I_net_USDGBP)>0

*     eststo: reg D.jybs7 I_net_USDJPY post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs10 I_net_USDJPY post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs15 I_net_USDJPY post07 if abs(I_net_USDJPY)>0

* ** inflow: short duration: us corp looking for tight credit sprd: not great/inconclusive
*     eststo: reg D.jybs5 I_JPYus post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs7 I_JPYus post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs10 I_JPYus post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs15 I_JPYus post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs20 I_JPYus post07 if abs(I_net_USDJPY)>0

* ** outflow: longer duration: japanese pension hedging duration: inconclusive, somewhat peaks around 7 yr
*     eststo: reg D.jybs5 I_USDjp post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs7 I_USDjp post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs10 I_USDjp post07 if abs(I_net_USDJPY)>0
*     eststo: reg D.jybs15 I_USDjp post07 if abs(I_net_USDJPY)>0

*     eststo: reg D.adbs5 I_net_USDAUD post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs7 I_net_USDAUD post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs10 I_net_USDAUD post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs15 I_net_USDAUD post07 if abs(I_net_USDAUD)>0

* * inflow: suppose to be shorter duration: again in conclusive
*     eststo: reg D.adbs5 I_AUDus post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs7 I_AUDus post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs10 I_AUDus post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs15 I_AUDus post07 if abs(I_net_USDAUD)>0

* * outflow: longer maturity
*     eststo: reg D.adbs5 I_USDau post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs7 I_USDau post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs10 I_USDau post07 if abs(I_net_USDAUD)>0
*     eststo: reg D.adbs15 I_USDau post07 if abs(I_net_USDAUD)>0        

*     esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet I_USDeu IssUS I_USDgb IssUS I_USDjp IssUS I_USDau IssUS I_EURus IssF I_GBPus IssF I_JPYus IssF I_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append


* *Table 6: All currencies: basis
*     local filename "Table6_basis_all"
*     capture rm `filename'.csv
*     eststo clear
*     eststo: reg D.eubs10 I_net_USDEUR post07 
*     eststo: reg D.eubs10 I_USDeu post07 
*     eststo: reg D.eubs10 I_EURus post07 

*     eststo: reg D.bpbs10 I_net_USDGBP post07 
*     eststo: reg D.bpbs10 I_USDgb post07 
*     eststo: reg D.bpbs10 I_GBPus post07 

*     eststo: reg D.jybs10 I_net_USDJPY post07 
*     eststo: reg D.jybs10 I_USDjp post07 
*     eststo: reg D.jybs10 I_JPYus post07 

*     eststo: reg D.adbs10 I_net_USDAUD post07 
*     eststo: reg D.adbs10 I_USDau post07 
*     eststo: reg D.adbs10 I_AUDus post07 

*     esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet I_USDeu IssUS I_USDgb IssUS I_USDjp IssUS I_USDau IssUS I_EURus IssF I_GBPus IssF I_JPYus IssF I_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append


* Table 9: credit spread impact on basis directly
drop ??bs10chg*
gen eubs10chg=F.eubs10-eubs10
gen bpbs10chg=F.bpbs10-bpbs10
gen jybs10chg=F.jybs10-jybs10
gen adbs10chg=F.adbs10-adbs10

gen eubs10chg3=F3.eubs10-eubs10
gen bpbs10chg3=F3.bpbs10-bpbs10
gen jybs10chg3=F3.jybs10-jybs10
gen adbs10chg3=F3.adbs10-adbs10

gen eubs10chg6=F6.eubs10-eubs10
gen bpbs10chg6=F6.bpbs10-bpbs10
gen jybs10chg6=F6.jybs10-jybs10
gen adbs10chg6=F6.adbs10-adbs10


local filename "Table9_basis_credit"
    capture rm `filename'.csv
    eststo clear


eststo: neweymod eubs10chg6 Cdif_euus_30_eff post07, lag(6)

eststo: neweymod eubs10chg6 Cdif_euus_30_eff ratediff_euus eurchg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod bpbs10chg6 Cdif_gbus_30_eff post07, lag(6)
eststo: neweymod bpbs10chg6 Cdif_gbus_30_eff ratediff_gbus gbpchg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod jybs10chg6 Cdif_jpus_30_eff post07, lag(6)
eststo: neweymod jybs10chg6 Cdif_jpus_30_eff ratediff_jpus jpychg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod adbs10chg6 Cdif_auus_30_eff post07, lag(6)
eststo: neweymod adbs10chg6 Cdif_auus_30_eff ratediff_auus audchg ussw10 cy30_govtoas post07, lag(6)

esttab using `filename'.csv, title("Multivariate Forecasting'") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(CrdSprdDiffEff exchrate ussw10 ratediff cy30_govtoas post07) append
************************************************************************************************************************************************************************
* Figures
************************************************************************************************************************************************************************
use niip.dta,clear
scatter cip1y ca, mlabel(ccy) || lfit cip1y ca, ytitle(1-year CIP deviations in basis points) xtitle("current account (%GDP)")
graph export "../paper/figures/cip1yvsca.eps",replace
scatter cip10y ca, mlabel(ccy) || lfit cip1y ca, ytitle(10-year CIP deviations in basis points) xtitle("current account (%GDP)")
graph export "../paper/figures/cip10yvsca.eps",replace
* scatter cip1y niip, mlabel(ccy) || lfit cip1y niip, ytitle(10-year CIP deviations in basis points) xtitle("net international investment position (%GDP)")

* eur issuance scatter
egen i_net_USDEUR_mean_1=mean(i_net_USDEUR) if year>2005 & year<2009 
egen i_net_USDEUR_mean_2=mean(i_net_USDEUR) if year>=2009 & year<2014 
egen i_net_USDEUR_mean_3=mean(i_net_USDEUR) if year>=2014 & year<2016

tsline i_net_USDEUR if year>2005 & year<2009, recast(sc) mc(green) || tsline i_net_USDEUR if year>=2009 & year<2014, recast(sc) mc(red) msymbol(+)  || tsline i_net_USDEUR if year>=2014 & year<2016, recast(sc) mc(blue) msymbol(d)  || tsline i_net_USDEUR_mean_1 if year>2005,lc(green) || tsline i_net_USDEUR_mean_2 if year>2005,lc(red) || tsline i_net_USDEUR_mean_3 if year>2005,lc(blue) || tsline Cdif_euus_30_eff if year>2005, yaxis(2) yscale(range(-120,145) axis(2)) yscale(range(-20,15) axis(1))  plotregion(margin(small)) graphregion(margin(l+5 r+5)) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) legend(label(1 "Pre-2009") label(2 "2009-2013: FED QE") label(3 "Post-2014: ECB QE") label(7 "Corp A CrdSprdEff EU-US") order(1 2 3 7)) 
graph export "../paper/figures/qeissuance.eps",replace

* Fig 10 generate graph of credit differential and issuance flow
graph twoway (tsline F.i_net_USDEUR_6mf, lpattern(l)) (tsline Cdif_euus_30_eff,yaxis(2)) if year>=2002 & year<2016, legend(label(1 "IssEUtoUS/TotIss 6m avg") lab(2 "CrdSprdDiffEff EU-US")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")
graph export "../paper/figures/eurissuancecredit.eps",replace
* graph twoway (tsline i_net_USDEUR,recast(sc)) (tsline Cdif_euus_30_eff,yaxis(2)) if year>=2002 & year<2016, legend(label(1 "IssEUtoUS6m/TotIss") lab(2 "Corp A Credit Sprd Diff EUR-USD")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2))


* * generate graph of issuance and basis
graph twoway (tsline I_net_USDEUR if year>=2006 & monthly<=d(1oct2015), lpattern(l))  (tsline D.eubs10 if year>=2006 & monthly<=d(1oct2015),yaxis(2)) if year>=2006 & year<2016, legend(label(1 "Issuance_EU->US") lab(2 "Change in EUR-USD cross-currency basis")) ttitle("") ytitle("Issuance in billion dollars") ytitle("monthly change in currency basis",axis(2))
graph export "../paper/figures/eurissuancebasis.eps",replace
* ** JPY
* graph twoway (tsline I_net_USDJPY if year>=2006 & monthly<=d(1oct2015),yscale(range(-10,10)))  (tsline D.jybs7 if year>=2006 & monthly<=d(1oct2015),yaxis(2) ,yscale(range(-10,10))) if year>=2006 & year<2016, legend(label(1 "Issuance_JP->US") lab(2 "Change in JPY-USD cross-currency basis")) ttitle("") ytitle("Issuance in billion dollars") ytitle("monthly change in currency basis",axis(2))


* graph twoway (tsline I_net_USDGBP if year>=2006 & monthly<=d(1oct2015))  (tsline D.bpbs10 if year>=2006 & monthly<=d(1oct2015),yaxis(2) ) if year>=2006 & year<2016, legend(label(1 "Issuance_JP->US") lab(2 "Change in GBP-USD cross-currency basis")) ttitle("") ytitle("Issuance in billion dollars") ytitle("monthly change in currency basis",axis(2))

************************************************************************************************************************************************************************
* Others
************************************************************************************************************************************************************************

graph twoway tsline cy30_govtoas er30_govtoas ur30_govtoas jc30_govtoas ac30_govtoas if year>1995, legend(lab(1 "USD Corp A OAS") lab(2 "EUR Corp A OAS") lab(3 "GBP Corp A OAS") lab(4 "JPY Corp A OAS") lab(5 "AUD Corp A OAS")) ttitle("") ytitle("basis points")
graph export "figures/creditsprdraw.eps",replace
graph twoway tsline Cdif_euus_30 Cdif_gbus_30 Cdif_jpus_30 Cdif_auus_30 if year>1995, legend(lab(1 "EU-US OAS diff") lab(2 "GB-US OAS diff") lab(3 "JP-US OAS diff") lab(4 "AU-US OAS diff")) ttitle("") ytitle("basis points")
graph export "figures/creditsprds.eps",replace
graph twoway tsline  I_net_USDEUR  I_net_USDGBP I_net_USDJPY I_net_USDAUD if year>1995 , legend(lab(1 "Issuance_EU->US") lab(2 "Issuance_GB->US") lab(3 "Issuance_JP->US") lab(4 "Issuance_AU->US")) ttitle("") ytitle("USD billion")
graph export "figures/IssuanceFlow.eps",replace
graph twoway tsline eubsc eubs10 if year>2005 & eubsc>-150, legend(label(1 "Eur basis 3mo") lab(2 "Eur basis 10yr")) ttitle("") ytitle("basis points")  yline(0) ysc(range(-150 30))
graph export "figures/basiseur.eps",replace
graph twoway tsline eubs10 bpbs10 jybs10 adbs10 if year>1996, legend(label(1 "EUR") lab(2 "GBP") lab(3 "JPY") lab(4 "AUD")) ttitle("") ytitle("basis points")  yline(0) 
graph export "figures/basisall.eps",replace

* test:
graph twoway tsline Cdif_euus_30_eff Cdif_gbus_30_eff Cdif_jpus_30_eff Cdif_auus_30_eff if year>1995, legend(lab(1 "EU-US OAS diff eff") lab(2 "GB-US OAS diff eff") lab(3 "JP-US OAS diff eff") lab(4 "AU-US OAS diff eff")) ttitle("") ytitle("basis points")
graph twoway tsline Cdif_euus_30 Cdif_euus_30_eff if year>1995, yline(0)
graph twoway tsline Cdif_euus_20 Cdif_euus_20_eff if year>1995, yline(0)
graph twoway tsline Cdif_gbus_30 Cdif_gbus_30_eff if year>1995, yline(0)
graph twoway tsline Cdif_gbus_20 Cdif_gbus_20_eff if year>1995, yline(0)
graph twoway tsline Cdif_jpus_30 Cdif_jpus_30_eff if year>1995, yline(0)
graph twoway tsline Cdif_jpus_20 Cdif_jpus_20_eff if year>1995, yline(0)
graph twoway tsline Cdif_auus_30 Cdif_auus_30_eff if year>1995, yline(0)
graph twoway tsline Cdif_auus_20 Cdif_auus_20_eff if year>1995, yline(0)


graph twoway tsline Cdif_auus_30
********************************
***  VAR  ****************************
****************************************
var I_net_USDEUR Cdif_euus_30_eff, lags(1)
irf create irfeu, set(irf1,replace) step(10)
var I_net_USDGBP Cdif_gbus_30_eff, lags(1)
irf create irfgb, set(irf1) step(10)
var I_net_USDJPY Cdif_jpus_30_eff, lags(1)
irf create irfjp, set(irf1) step(10)
var I_net_USDAUD Cdif_auus_30_eff, lags(1)
irf create irfau, set(irf1) step(10)

* irf graph irf, irf(irfau) impulse(Cdif_euus_30_eff) response(I_net_USDEUR) ustep(10) byopt(subtitle("") note("")) subtitle("") ytitle("Monthly issuance flow") xtitle(Months)

irf cgraph (irfeu Cdif_euus_30_eff I_net_USDEUR irf,subtitle(EUR)  ) (irfgb Cdif_gbus_30_eff I_net_USDGBP irf,subtitle(GBP)) (irfjp Cdif_jpus_30_eff I_net_USDJPY irf,subtitle(JPY)) (irfau Cdif_auus_30_eff I_net_USDAUD irf,subtitle(AUD)), ytitle("Monthly issuance flow (Billions USD)") xtitle(Months) 
graph export "../paper/figures/VAR_Iss.eps",replace


var  Cdif_euus_30_eff I_net_USDEUR, lags(1)
irf create irfeu, order(Cdif_euus_30_eff I_net_USDEUR) set(irf2,replace) step(10)
irf graph oirf, irf(irfeu) impulse(Cdif_euus_30_eff) response(I_net_USDEUR) 
******** random tests
var FD.eur eubs10, lags(1)
irf create irftest, set(irftest,replace) step(10)
irf graph irf, irf(irftest) impulse(eubs10) response(FD.eur) 

varbasic I_net_USDEUR Cdif_euus_30_eff, irf lags(1) 
* example
var lrx3_10_sven scale_leh_mbs_moddur sventspread10 sveny01, lag(1)
irf create irf1, set(irf1, replace)
irf graph irf, impulse(scale_leh_mbs_moddur) response(lrx3_10_sven)


************************************************************************************************************************************************************************
br monthly I_net_USDEUR Cdif_euus_30
tsline F.I_net_USDCHF
reg D.sfbs7 I_net_USDCHF if year>2008
reg D.eubs7 I_net_USDEUR if year>2010
twoway (tsline D.sfbs7) (tsline I_net_USDCHF,yaxis(2))

reg D.bpbs7 I_net_GBPEUR if year>2009
reg F.I_net_USDEUR Cdif_euus_30_eff
reg F.I_net_USDEUR eubs5

reg F.I_net_USDJPY Cdif_jpus_30_eff
reg F.I_net_USDJPY Cdif_jpus_30
reg F.I_net_USDJPY jybs10

reg F.eurchg eubs10 if year<2007
reg F.jpychg jybs10 if year<2007

list eur eubs15 er44_govtoas cy44_govtoas er44_assetswapspread cy44_assetswapspread er44_liboroas cy44_liboroas er44_semiyldtoworst  cy44_semiyldtoworst if year==2014 & month==11
list eur eubs15 er40_govtoas cy40_govtoas er40_assetswapspread cy40_assetswapspread er40_liboroas cy40_liboroas ussp5 if year==2014 & month==11
neweymod F.i_net_USDEUR_6mf Cdif_euus_34_eff, lag(6)
neweymod F.i_net_USDEUR_6mf Cdif_euus_citi_eff, lag(6)

neweymod F.I_net_USDEUR_6mf Cdif_euus_34_eff, lag(6)
neweymod F.I_net_USDEUR_6mf Cdif_euus_citi_eff, lag(6)
neweymod F.I_net_USDEUR_6mf Cdif_euus_citi_liboroas_eff, lag(6)



* tsline i_net_USDEUR if year>2005 & year<2009, recast(sc) mc(green) legend(label(1 "Pre-2009")) || tsline i_net_USDEUR if year>=2009 & year<2014, recast(sc) mc(red) legend(label(2 "2009-2013: FED QE")) || tsline i_net_USDEUR if year>=2014 & year<2016, recast(sc) mc(blue) legend(label(3 "Post-2014: ECB QE")) ytitle("iss_EUR->USD")


twoway (scatter F.i_net_USDEUR Cdif_euus_30_eff if year>2001) || lfit F.i_net_USDEUR Cdif_euus_30_eff if year>2001


twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_30_eff,yaxis(2)) if year>2001


twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_30_eff,yaxis(2)) if year>2001
twoway (scatter F.i_net_USDEUR_6mf Cdif_euus_30_eff if year>2001) || lfit F.i_net_USDEUR_6mf Cdif_euus_30_eff if year>2001
twoway (scatter F.i_net_USDEUR_6mf Cdif_euus_30_eff if year>2007) (scatter F.i_net_USDEUR_6mf Cdif_euus_30_eff if year<=2007), legend(label(1 post08) label(2 pre08)) || lfit F.i_net_USDEUR_6mf Cdif_euus_30_eff if year>2001



graph twoway (tsline I_net_USDEUR) (tsline eubs10, yaxis(6))
graph twoway (tsline i_net_USDEUR_6mf) (tsline Cdif_euus_30_eff, yaxis(2))
reg F.i_net_USDEUR_6mf Cdif_euus_40_eff

graph twoway (tsline i_net_USDJPY_6mf) (tsline Cdif_jpus_30_eff, yaxis(2))
graph twoway (tsline i_net_USDGBP_6mf) (tsline Cdif_gbus_30_eff, yaxis(2))
graph twoway (tsline i_net_USDAUD_6mf) (tsline Cdif_auus_30_eff, yaxis(2))

graph twoway (tsline eubs10) (tsline i_net_USDEUR_6mf,yaxis(2))
graph twoway (tsline jybs10) (tsline i_net_USDJPY_6mf,yaxis(2))
graph twoway (tsline D.jybs10) (tsline i_net_USDJPY_6mf,yaxis(2))

* QE plot
tsline i_net_USDEUR if year>2006
tsline i_net_USDJPY if year>2007
tsline i_net_JPYEUR if year>2007
tsline i_net_GBPEUR if year>2007


reg D.jybs10 I_net_USDJPY

reg I_net_USDEUR Cdif_euus_30_eff if year>=2002

br year I_net_USDEUR I_EURus I_USDeu

gen I_JPYus3m=I_JPYus+L.I_JPYus + L2.I_JPYus
gen I_USDjp3m=I_USDjp+L.I_USDjp + L2.I_USDjp

tsline monthly I_JPYus I_USDjp
tsline I_EURus I_USDeu
tsline Cdif_euus_10 Cdif_euus_20 Cdif_euus_30 Cdif_euus_40

tsline Cdif_euus_10_eff Cdif_euus_20_eff Cdif_euus_30_eff Cdif_euus_40_eff
reg F.i_net_`A'`B'_6mf Cdif_euus_40_eff
neweymod F.i_net_`A'`B'_6mf Cdif_euus_40_eff, lag(6)
neweymod F.i_net_`A'`B'_6mf Cdif_euus_citi_eff, lag(6)
neweymod F.i_net_`A'`B'_6mf Cdif_euus_citi_liboroas_eff, lag(6)


twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_40_eff,yaxis(2))
twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_40_eff,yaxis(2))


local filename "Table_test_univariate"
capture rm`filename'.csv
local A USD
local B EUR
eststo clear
eststo: neweymod F.I_net_`A'`B'_6mf Cdif_euus_10_eff, lag(6)
eststo: neweymod F.I_net_`A'`B'_6mf Cdif_euus_20_eff, lag(6)
eststo: neweymod F.I_net_`A'`B'_6mf Cdif_euus_30_eff, lag(6)
eststo: neweymod F.i_net_`A'`B'_6mf Cdif_euus_40_eff, lag(6)

eststo: neweymod F.I_net_`A'GBP_6mf Cdif_gbus_10_eff, lag(6)
eststo: neweymod F.I_net_`A'GBP_6mf Cdif_gbus_20_eff, lag(6)
eststo: neweymod F.I_net_`A'GBP_6mf Cdif_gbus_30_eff, lag(6)
eststo: neweymod F.I_net_`A'GBP_6mf Cdif_gbus_40_eff, lag(6)

eststo: neweymod F.I_net_`A'JPY_6mf Cdif_jpus_20_eff, lag(6)
eststo: neweymod F.I_net_`A'JPY_6mf Cdif_jpus_30_eff, lag(6)
eststo: neweymod F.I_net_`A'JPY_6mf Cdif_jpus_40_eff, lag(6)

eststo: neweymod F.I_net_`A'AUD_6mf Cdif_auus_10_eff, lag(6)
eststo: neweymod F.I_net_`A'AUD_6mf Cdif_auus_20_eff, lag(6)
eststo: neweymod F.I_net_`A'AUD_6mf Cdif_auus_30_eff, lag(6)
eststo: neweymod F.I_net_`A'AUD_6mf Cdif_auus_40_eff, lag(6)

esttab using `filename'.csv, title("Univariate Forecasting") bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(Cdif_`crdsource' Cdif_`crdsource'_eff) replace

********



su Cdif_euus_30_eff Cdif_euus_30 eubs10
eststo: neweymod eubs10chg6 Cdif_euus_30 eubs10 post07, lag(6)
eststo: neweymod eubs10chg6 Cdif_euus_30 eubs10 post07, lag(6)
eststo: neweymod bpbs10chg6 Cdif_gbus_30 bpbs10 post07 if year>2007, lag(6)
eststo: neweymod jybs10chg6 Cdif_jpus_30 jybs10 post07 if year>2007, lag(6)
eststo: neweymod adbs10chg6 Cdif_auus_30 adbs10 post07 if year>2007, lag(6)




gen Iss6_USDEUR=(F5.I_net_USDEUR+F4.I_net_USDEUR+ F3.I_net_USDEUR+F2.I_net_USDEUR+ F.I_net_USDEUR+I_net_USDEUR)/6
reg eubs10chg6 Iss6_USDEUR post07

gen Iss6_USDGBP=(F5.I_net_USDGBP+F4.I_net_USDGBP+ F3.I_net_USDGBP+F2.I_net_USDGBP+ F.I_net_USDGBP+I_net_USDGBP)/6
reg eubs10chg6 Iss6_USDGBP post07

gen Iss6_USDJPY=(F5.I_net_USDJPY+F4.I_net_USDJPY+ F3.I_net_USDJPY+F2.I_net_USDJPY+ F.I_net_USDJPY+I_net_USDJPY)/6
reg eubs10chg6 Iss6_USDJPY post07

gen Iss6_USDAUD=(F5.I_net_USDAUD+F4.I_net_USDAUD+ F3.I_net_USDAUD+F2.I_net_USDAUD+ F.I_net_USDAUD+I_net_USDAUD)/6
reg eubs10chg6 Iss6_USDAUD post07


* esttab using `filename'.csv, title("Basis") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") append



************************************************************************************************************************************************************************

gen eurchgf3=log(F3.eur/eur)
gen gbpchgf3=log(F3.gbp/gbp)
gen jpychgf3=log(F3.jpy/jpy)
gen audchgf3=log(F3.aud/aud)
neweymod eurchgf3 Cdif_euus_30_eff post07, lag(3)
neweymod gbpchgf3 Cdif_gbus_30_eff post07, lag(3)
neweymod jpychgf3 Cdif_jpus_30_eff post07, lag(3)
neweymod audchgf3 Cdif_auus_30_eff post07, lag(3)


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
reg eurchg6 Cdif_euus_30 post07 if year>2008

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




graph twoway tsline er30_govtoas cy30_govtoas if year>=2002, legend(label(1 "Eur Corp A OAS Spread") lab(2 "USD Corp A OAS Spread")) ttitle("") ytitle("basis points")


* quietly: ireg3 "USD" "CAD" "us" "ca" "30" "cdbs10" "30_01_m" D.adbs1 `controls'
* graph twoway (tsline F.i_net_USDEUR_6mf) (tsline Cdif_euus_30_effective,yaxis(2)) if year>=2002, legend(label(1 "issuancePct6m_EU->US") lab(2 "Credit Spread Diff EUR-USD")) ttitle("") ytitle("Percent") ytitle("basis points",axis(2)) 

* graph export "figures/eurissuancecredit.eps",replace

su I_net_USDEUR
br I_net_USDEUR
br monthly I_USDeu I_EURus I_USD_tot I_EUR_tot

su I_USDeu I_EURus I_USD_tot I_EUR_tot


* graph twoway (tsline I_net_USDJPY) (tsline jybs5,yaxis(2)) if year>=1996, legend(label(1 "Issuance_JP->US") lab(2 "Yen Libor/Libor XC basis")) ttitle(none) ytitle("Issuance in billion dollars") ytitle("xc basis: euribor+X vs libor",axis(2)) 

* drop eurchgf3
* gen eurchgf3=log(F3.eur/eur)
* reg eurchg1 I_net_USDEUR
