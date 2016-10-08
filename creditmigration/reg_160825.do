* cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
cd "\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
clear matrix
adopath + .
clear
set more off

* clean bond records
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

	* new 160822 
	* drop if issue_type_desc=="Agency, Supranational, Sovereign"
	* drop if issue_type_desc=="Mortgage-backed"
	* drop if issue_type_desc=="Asset-backed"

	* br i desc ytofm d settlement2 mat2 amt if year==2016 & month==1 & ccy=="EUR" & modnat=="United States"
	* br i desc ytofm d settlement2 mat2 amt sic2 Euop if year==2016 & month==1 & ccy=="USD" & modnat=="Eurozone"

	* decode Euop, gen(uop)
	* drop if uop=="Acquisition Fin."
	* drop if uop=="Leveraged Buyout Acquisition Fin."
	* br i desc ytofm d settlement2 mat2 if ytofm==.
	save sdc96_clean3.dta,replace

* collapse and merge
	capture program drop icollapse
	quietly:icollapse "USD" "EUR" "us" "eu" "United States" "Eurozone" amt modnat
	quietly:icollapse "USD" "CHF" "us" "ch" "United States" "Switzerland" amt modnat
	quietly:icollapse "USD" "GBP" "us" "gb" "United States" "United Kingdom" amt modnat
	quietly:icollapse "USD" "JPY" "us" "jp" "United States" "Japan" amt modnat
	quietly:icollapse "USD" "AUD" "us" "au" "United States" "Australia" amt modnat
	quietly:icollapse "USD" "CAD" "us" "ca" "United States" "Canada" amt modnat

	* merge all monthly issuance data together
	use temp_issuance_USDEUR.dta,clear
	quietly:merge 1:1 monthly using temp_issuance_USDGBP.dta,nogen
	quietly:merge 1:1 monthly using temp_issuance_USDJPY.dta,nogen
	quietly:merge 1:1 monthly using temp_issuance_USDAUD.dta,nogen
	quietly:merge 1:1 monthly using temp_issuance_USDCHF.dta,nogen
	quietly:merge 1:1 monthly using temp_issuance_USDCAD.dta,nogen

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

	* drop if date>=d(01mar2016)
	* drop if date>=d(01oct2015)
	save regdata_02.dta,replace

* import residualized credit spread from r
	* use temp_ys.dta,clear
	* gen month=month(date)
	* gen year=year(date)
	* gen monthly = ym(year,month)
	* format monthly %tm
	* tsset monthly
	* rename ccyeur ccyeur_eff
	* rename ccyjpy ccyjpy_eff
	* rename ccygbp ccygbp_eff
	* rename ccyaud ccyaud_eff
	* * keep monthly ccyeur_eff
	* save residualizedys2.dta,replace
* new 160825 import effective residualized credit spread from r
	use "dta/effresys_160825b.dta",clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	tsset monthly
		* test against previous 
		* merge 1:1 monthly using residualizedys2.dta
		* br
		* br monthly eur ccyeur_eff
		* tsline eur ccyeur_eff
		* tsline jpy ccyjpy_eff
	rename eur ccyeur_eff
	rename jpy ccyjpy_eff
	rename gbp ccygbp_eff
	rename aud ccyaud_eff
	rename chf ccychf_eff
	rename cad ccycad_eff
	* drop if year==2016
	save effresidualizedys160825b.dta,replace

* new 160826 import effective residualized credit spread from r
	use "effresys_160826_retro_0625.dta",clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	tsset monthly
	rename ccyeur ccyeur_eff
	rename ccyjpy ccyjpy_eff
	rename ccygbp ccygbp_eff
	rename ccyaud ccyaud_eff
	rename ccychf ccychf_eff
	rename ccycad ccycad_eff
	* drop if year==2016
	save effresys_160826_retro_0625_clean.dta,replace

*reg prep
	use regdata_02.dta,clear
	drop _merge
	merge 1:1 monthly using residualizedys2.dta
	tsset monthly
	gen ccyeur_eff_6m=(ccyeur_eff+L.ccyeur_eff+L2.ccyeur_eff+L3.ccyeur_eff+L4.ccyeur_eff+L5.ccyeur_eff)/6
	gen ccyjpy_eff_6m=(ccyjpy_eff+L.ccyjpy_eff+L2.ccyjpy_eff+L3.ccyjpy_eff+L4.ccyjpy_eff+L5.ccyjpy_eff)/6
	gen ccygbp_eff_6m=(ccygbp_eff+L.ccygbp_eff+L2.ccygbp_eff+L3.ccygbp_eff+L4.ccygbp_eff+L5.ccygbp_eff)/6
	gen ccyaud_eff_6m=(ccyaud_eff+L.ccyaud_eff+L2.ccyaud_eff+L3.ccyaud_eff+L4.ccyaud_eff+L5.ccyaud_eff)/6

* impulse response with new residualized sprd
	********************************
	***  VAR  ****************************
	****************************************
		* previous version
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
			* graph export "../paper/figures/VAR_Iss.eps",replace

			* orthogonalized
			var  Cdif_euus_30_eff i_net_USDEUR, lags(1)
			irf create irfeu, order(Cdif_euus_30_eff i_net_USDEUR) set(irf2,replace) step(10)
			irf graph oirf, irf(irfeu) impulse(Cdif_euus_30_eff) response(i_net_USDEUR) 

			varbasic I_net_USDEUR Cdif_euus_30_eff, irf lags(1) 
			* example
			* var lrx3_10_sven scale_leh_mbs_moddur sventspread10 sveny01, lag(1)
			* irf create irf1, set(irf1, replace)
			* irf graph irf, impulse(scale_leh_mbs_moddur) response(lrx3_10_sven)
		* new version with residualized credit spread
			var i_net_USDEUR ccyeur_eff, lags(1)
			irf create irfeu, set(irf1,replace) step(10)
			* irf graph irf, irf(irfeu) impulse(ccyeur_eff) response(i_net_USDEUR) 
			var i_net_USDGBP ccygbp_eff, lags(1)
			irf create irfgb, set(irf1) step(10)
			var i_net_USDJPY ccyjpy_eff, lags(1)
			irf create irfjp, set(irf1) step(10)
			var i_net_USDAUD ccyaud_eff, lags(1)
			irf create irfau, set(irf1) step(10)
			var i_net_USDCHF ccychf_eff, lags(1)
			irf create irfch, set(irf1) step(10)
			var i_net_USDCAD ccycad_eff, lags(1)
			irf create irfca, set(irf1) step(10)

			irf cgraph (irfeu ccyeur_eff i_net_USDEUR irf,subtitle(EUR)  ) (irfgb ccygbp_eff i_net_USDGBP irf,subtitle(GBP)) (irfjp ccyjpy_eff i_net_USDJPY irf,subtitle(JPY)) (irfau ccyaud_eff i_net_USDAUD irf,subtitle(AUD)), ytitle("Monthly issuance flow (Billions USD)") xtitle(Months) 
			* irf cgraph (irfeu ccyeur_eff i_net_USDEUR irf,subtitle(EUR)  ) (irfgb ccygbp_eff i_net_USDGBP irf,subtitle(GBP)) (irfjp ccyjpy_eff i_net_USDJPY irf,subtitle(JPY)) (irfau ccyaud_eff i_net_USDAUD irf,subtitle(AUD)) (irfch ccychf_eff i_net_USDCHF irf,subtitle(CHF)) (irfca ccycad_eff i_net_USDCAD irf,subtitle(CAD)), ytitle("Monthly issuance flow (Billions USD)") xtitle(Months) 
			* orthogonalized
			var  i_net_USDEUR ccyeur_eff, lags(1)
			irf create irfeu, order(ccyeur_eff i_net_USDEUR) set(irf2,replace) step(10)
			irf graph oirf, irf(irfeu) impulse(ccyeur_eff) response(i_net_USDEUR) 

			* ortho for all 4 ccy
			var i_net_USDEUR ccyeur_eff, lags(1)
			irf create irfeu, order(ccyeur_eff i_net_USDEUR) set(irf2,replace) step(10)
			var i_net_USDGBP ccygbp_eff, lags(1)
			irf create irfgb, order(ccygbp_eff i_net_USDGBP) set(irf2) step(10)
			var i_net_USDJPY ccyjpy_eff, lags(1)
			irf create irfjp, order(ccyjpy_eff i_net_USDJPY) set(irf2) step(10)
			var i_net_USDAUD ccyaud_eff, lags(1)
			irf create irfau, order(ccyaud_eff i_net_USDAUD) set(irf2) step(10)
			irf cgraph (irfeu ccyeur_eff i_net_USDEUR oirf,subtitle(EUR)  ) (irfgb ccygbp_eff i_net_USDGBP oirf,subtitle(GBP)) (irfjp ccyjpy_eff i_net_USDJPY oirf,subtitle(JPY)) (irfau ccyaud_eff i_net_USDAUD oirf,subtitle(AUD)), ytitle("Monthly issuance flow (Billions USD)") xtitle(Months) 
	* what to graph for paper
	* ortho irf euro
	var  i_net_USDEUR ccyeur_eff, lags(1)
	irf create irfeu, order(ccyeur_eff i_net_USDEUR) set(irf2,replace) step(10)
	irf graph oirf, irf(irfeu) impulse(ccyeur_eff) response(i_net_USDEUR) 
	graph export "../paper/figures/VAR_oirfeur160825.eps",replace
	* simple irf euro
	var i_net_USDEUR ccyeur_eff, lags(1)
	irf create irfeu, set(irf1,replace) step(10)
	irf graph irf, irf(irfeu) impulse(ccyeur_eff) response(i_net_USDEUR) 
	graph export "../paper/figures/VAR_sirfeur160825.eps",replace

* panl reg of 2 misp. alignment
	use mispricings_long_160730.dta,clear
		* use dta/mispricings_long_160825.dta, clear
		* decode ccy, gen(sccy)
		* drop ccy
		* gen ccy=sccy
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	drop month year
	encode ccy, gen(Eccy)
	xtset Eccy monthly
	* xi:reg cip credit i.ccy
	* xtreg cip credit,fe
	* xi:reg cip credit i.ccy
    local filename "stataout/twomispricings"
    capture rm `filename'.csv
    eststo clear
	eststo:xtreg credit cip,fe
	eststo:reg credit cip
	eststo:reg credit cip if ccy=="eur"
	eststo:reg credit cip if ccy=="gbp"
	eststo:reg credit cip if ccy=="jpy"
	eststo:reg credit cip if ccy=="aud"
	eststo:reg credit cip if ccy=="chf"
	eststo:reg credit cip if ccy=="cad"
	esttab using `filename'.csv, title("Panel regression of two mispricings") bracket r2 nostar nogaps nonote addnotes(" ") order() append

* find out how I created residualizedys2.dta, recreate for all 6 ccys
* reg issuance flow on net mispricing
	use regdata_02.dta,clear
	capture drop _merge
	* merge 1:1 monthly using residualizedys2.dta
	* merge 1:1 monthly using effresidualizedys160825b.dta
	merge 1:1 monthly using effresys_160826_retro_0625_clean.dta
	drop if month==.
	drop monthly
	gen monthly=ym(year,month)
	format monthly %tm
	drop if year<2003
	keep monthly i_net_USD??? ccy???_eff
	rename ccyeur_eff cseffEUR
	rename ccygbp_eff cseffGBP
	rename ccyjpy_eff cseffJPY
	rename ccyaud_eff cseffAUD
	rename ccychf_eff cseffCHF
	rename ccycad_eff cseffCAD
	* drop i_net_USDCAD i_net_USDCHF

	reshape long i_net_USD cseff, i(monthly) j(ccy) string
	rename i_net_USD issflow
	encode ccy, gen(Eccy)
	xtset Eccy monthly
	drop if monthly<ym(2003,01)

	gen issflow3mf=(issflow+F.issflow+F2.issflow)/6
	gen issflow6mf=(issflow+F.issflow+F2.issflow+F3.issflow+F4.issflow+F5.issflow)/6


	local filename "stataout/issuancenetmispricing"
    capture rm `filename'.csv
    eststo clear

	* eststo:reg issflow cseff
	eststo:reg issflow cseff if issflow!=0
	eststo:xtreg issflow cseff if issflow!=0,fe
	eststo:reg issflow cseff if issflow!=0 & ccy=="EUR"
	eststo:reg issflow cseff if issflow!=0 & ccy=="GBP"
	eststo:reg issflow cseff if issflow!=0 & ccy=="JPY"
	eststo:reg issflow cseff if issflow!=0 & ccy=="AUD"
	eststo:reg issflow cseff if issflow!=0 & ccy=="CHF"
	eststo:reg issflow cseff if issflow!=0 & ccy=="CAD"
	xtline issflow cseff
    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append
    
    eststo clear	
	* eststo:reg issflow3m cseff
	eststo:reg issflow3m cseff if issflow3m!=0
	eststo:xtreg issflow3m cseff if issflow3m!=0,fe
	eststo:reg issflow3m cseff if issflow3m!=0 & ccy=="EUR"
	eststo:reg issflow3m cseff if issflow3m!=0 & ccy=="GBP"
	eststo:reg issflow3m cseff if issflow3m!=0 & ccy=="JPY"
	eststo:reg issflow3m cseff if issflow3m!=0 & ccy=="AUD"
    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append

    eststo clear
	* eststo:reg issflow6m cseff
	eststo:reg issflow6m cseff if issflow6m!=0
	eststo:xtreg issflow6m cseff if issflow6m!=0,fe
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="EUR"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="GBP"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="JPY"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="AUD"
    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append
    
    *NEWEY with panel 
    eststo clear
    tsset  Eccy monthly 
    * drop if monthly<ym(2004,01) | monthly>=ym(2016,01)
	eststo:newey F.issflow6m cseff,lag(6) force
	eststo:reg issflow6m cseff if issflow6m!=0
	eststo:xtreg issflow6m cseff if issflow6m!=0,fe
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="EUR"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="GBP"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="JPY"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="AUD"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="CHF"
	eststo:reg issflow6m cseff if issflow6m!=0 & ccy=="CAD"

	eststo:newey F.issflow6m cseff if ccy=="EUR",lag(6) force
	eststo:newey F.issflow6m cseff if ccy=="GBP",lag(6) force
	eststo:newey F.issflow6m cseff if ccy=="JPY",lag(6) force
	eststo:newey F.issflow6m cseff if ccy=="AUD",lag(6) force
	eststo:newey F.issflow6m cseff if ccy=="CHF",lag(6) force
	eststo:newey F.issflow6m cseff if ccy=="CAD",lag(6) force

    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append

	* **** 
	*  Pool these together and focus on EUR
	*  graph why jpy doesnt wokr


	*take 6m moving avg, etc
	*play around with it until it works vetters

* use mu as lhs: doesn't work since mu is very high for most ccys
	* use regdata_02.dta,clear
	* capture drop _merge
	* merge 1:1 monthly using residualizedys2.dta
	* * merge 1:1 monthly using effresidualizedys160825b.dta
	* drop if month==.
	* drop monthly
	* gen monthly=ym(year,month)
	* format monthly %tm
	* drop if year<2003
	* keep monthly mu_* ccy???_eff
	* rename ccyeur_eff cseffEUR
	* rename ccygbp_eff cseffGBP
	* rename ccyjpy_eff cseffJPY
	* rename ccyaud_eff cseffAUD
	* * rename ccychf_eff cseffCHF
	* * rename ccycad_eff cseffCAD
	* drop mu_CHF mu_CAD

	* reshape long mu_ cseff, i(monthly) j(ccy) string
	* rename mu_ mu
	* encode ccy, gen(Eccy)
	* xtset Eccy monthly
	* drop if monthly<ym(2003,01)
	* gen lnmu=log(mu)
	* *** really doesn't work for this reason
	* xtline lnmu 
	* xtline mu

	* local filename "stataout/mucb"
 *    capture rm `filename'.csv
 *    eststo clear
	* eststo:reg mu cseff
	* eststo:xtreg mu cseff,fe
	* eststo:reg mu cseff if ccy=="EUR"
	* eststo:reg mu cseff if ccy=="GBP"
	* eststo:reg mu cseff if ccy=="JPY"
	* eststo:reg mu cseff if ccy=="AUD"
	* esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append

 *    eststo clear
	* eststo:reg lnmu cseff
	* eststo:xtreg lnmu cseff,fe
	* eststo:reg lnmu cseff if ccy=="EUR"
	* eststo:reg lnmu cseff if ccy=="GBP"
	* eststo:reg lnmu cseff if ccy=="JPY"
	* eststo:reg lnmu cseff if ccy=="AUD"
	* esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append


























