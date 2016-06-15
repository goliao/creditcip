
* 160308 edits: chged file path to old archival files
cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"

use sdc96_clean2_151214,clear
* use sdc96_clean2,clear

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

merge 1:1 monthly using prices_extended_160207.dta,nogen
*merge 1:1 monthly using prices_extended.dta,nogen
tsset monthly
gen date=dofm(monthly)
format date %td
drop if year==.


save regdata_02.dta,replace

use regdata_02.dta,clear
drop if date>=d(01mar2016)
* previously oct15 issuance was zero, this is a small fix.
drop if date>=d(01oct2015) 


br monthly I_net_USDEUR Cdif_euus_30
* Table 1: Summary Stats
    rm table1_summary.csv
    eststo clear
    estpost summarize I_net_USD??? cy30_govtoas er30_govtoas ur30_govtoas jc30_govtoas ac30_govtoas eubs10 bpbs10 jybs10 adbs10 ussw10 eusa10 bpsw10 jysw10 adsw10 Cdif_??us_30 Cdif_??us_30_eff
    * estpost summarize I_net_USD??? cy00_govtoas cy20_govtoas cy30_govtoas cy40_govtoas er00_govtoas er20_govtoas er30_govtoas er40_govtoas ur00_govtoas ur20_govtoas ur30_govtoas ur40_govtoas jc00_govtoas jc20_govtoas jc30_govtoas jc40_govtoas ac20_govtoas ac30_govtoas ac40_govtoas auc0_govtoas eubs10 bpbs10 jybs10 adbs10 ussw10 eusa10 bpsw10 jysw10 adsw10 eur gbp jpy aud
    esttab using "table1_summary.csv", title(Summary Statistics) cell((count(label(Count)) mean(label(Mean)) sd(label(S.D.)) min(label(Min)) max(label(Max)))) nonumber append
     
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
    esttab using `filename'.csv, title("Univariate Forecasting") bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(Cdif_`crdsource' Cdif_`crdsource'_eff) replace

* Table 3: Multivariate regressions for EUR
    local filename "Table3_multivariate_EUR"
    capture rm `filename'.csv
    local controls "ussw10 ratediff_euus cy30_govtoas eurchg"
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
    esttab using `filename'.csv, title("Multivariate Forecasting'") bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(Cdif_`crdsource'_eff I_net_`A'`B' i_net_`A'`B' `controls') append

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
    esttab using `filename'.csv, title("Multivariate Forecasting'") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(CrdSprdDiffEff Iss iss ratediff ussw10) append

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
    esttab using `filename'.csv, title("Basis") bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(I_net_`A'`B' I_`A'`b' I_`B'`a' `controls') append

* Table 6: All currencies: basis
    local filename "Table6_basis_all"
    capture rm `filename'.csv
    eststo clear
    eststo: reg D.eubs10 I_net_USDEUR eurchg1 ussw10 ratediff_euus post07
    eststo: reg D.eubs10 I_USDeu eurchg1 ussw10 ratediff_euus post07
    eststo: reg D.eubs10 I_EURus eurchg1 ussw10 ratediff_euus post07

    eststo: reg D.bpbs10 I_net_USDGBP gbpchg1 ussw10 ratediff_gbus post07
    eststo: reg D.bpbs10 I_USDgb gbpchg1 ussw10 ratediff_gbus post07
    eststo: reg D.bpbs10 I_GBPus gbpchg1 ussw10 ratediff_gbus post07

    eststo: reg D.jybs10 I_net_USDJPY jpychg1 ussw10 ratediff_jpus post07
    eststo: reg D.jybs10 I_USDjp jpychg1 ussw10 ratediff_jpus post07
    eststo: reg D.jybs10 I_JPYus jpychg1 ussw10 ratediff_jpus post07

    eststo: reg D.adbs10 I_net_USDAUD audchg1 ussw10 ratediff_auus post07
    eststo: reg D.adbs10 I_USDau audchg1 ussw10 ratediff_auus post07
    eststo: reg D.adbs10 I_AUDus audchg1 ussw10 ratediff_auus post07

    esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet I_USDeu IssUS I_USDgb IssUS I_USDjp IssUS I_USDau IssUS I_EURus IssF I_GBPus IssF I_JPYus IssF I_AUDus IssF ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg1 exchrate gbpchg1 exchrate jpychg1 exchrate audchg1 exchrate) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(IssNet IssUS IssF) append



* Table 7: credit spread impact on basis directly
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


local filename "Table7_basis_credit"
    capture rm `filename'.csv
    eststo clear


eststo: neweymod eubs10chg3 Cdif_euus_30_eff post07, lag(6)

eststo: neweymod eubs10chg3 Cdif_euus_30_eff ratediff_euus eurchg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod bpbs10chg3 Cdif_gbus_30_eff post07, lag(6)
eststo: neweymod bpbs10chg3 Cdif_gbus_30_eff ratediff_gbus gbpchg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod jybs10chg3 Cdif_jpus_30_eff post07, lag(6)
eststo: neweymod jybs10chg3 Cdif_jpus_30_eff ratediff_jpus jpychg ussw10 cy30_govtoas post07, lag(6)

eststo: neweymod adbs10chg3 Cdif_auus_30_eff post07, lag(6)
eststo: neweymod adbs10chg3 Cdif_auus_30_eff ratediff_auus audchg ussw10 cy30_govtoas post07, lag(6)

esttab using `filename'.csv, title("Multivariate Forecasting'") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(CrdSprdDiffEff exchrate ussw10 ratediff cy30_govtoas post07) append




************************************************************************************************************************************************************************
* Figures
************************************************************************************************************************************************************************

graph twoway tsline cy30_govtoas er30_govtoas ur30_govtoas jc30_govtoas ac30_govtoas if year>1995, legend(lab(1 "USD Corp A OAS") lab(2 "EUR Corp A OAS") lab(3 "GBP Corp A OAS") lab(4 "JPY Corp A OAS") lab(5 "AUD Corp A OAS")) ttitle("") ytitle("basis points")
graph export "figures/creditsprdraw.eps",replace
graph twoway tsline Cdif_euus_30 Cdif_gbus_30 Cdif_jpus_30 Cdif_auus_30 if year>1995, legend(lab(1 "EU-US OAS diff") lab(2 "GB-US OAS diff") lab(3 "JP-US OAS diff") lab(4 "AU-US OAS diff")) ttitle("") ytitle("basis points")
graph export "figures/creditsprds.eps",replace
graph twoway tsline  I_net_USDEUR  I_net_USDGBP I_net_USDJPY I_net_USDAUD if year>1995 , legend(lab(1 "Issuance_EU->US") lab(2 "Issuance_GB->US") lab(3 "Issuance_JP->US") lab(4 "Issuance_AU->US")) ttitle("") ytitle("USD billion")
graph export "figures/IssuanceFlow.eps",replace
graph twoway tsline eubsc eubs10 if year>2005 & eubsc>-150, legend(label(1 "Eur basis 3mo") lab(2 "Eur basis 10yr")) ttitle("") ytitle("basis points")  yline(0) ysc(range(-150 30))
graph export "figures/basiseur.eps",replace
graph twoway tsline eubs10 bpbs10 jybs10 adbs10 if year>1996, legend(label(1 "EUR basis") lab(2 "GBP basis") lab(3 "JPY basis") lab(4 "AUD basis")) ttitle("") ytitle("basis points")  yline(0) 
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


************************************************************************************************************************************************************************




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
