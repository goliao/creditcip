* cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"
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

* new 160903 import residualized credit spread from r
	use "dta/creditmisp_160903.dta",clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	tsset monthly
	rename eur crdeur
	rename jpy crdjpy
	rename gbp crdgbp
	rename aud crdaud
	rename chf crdchf
	rename cad crdcad
	save dta/creditmisp_clean.dta,replace

* new 160903 import effective residualized credit spread from r
	use "dta/netmisp_160903.dta",clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	tsset monthly
	rename eur crdeur_eff
	rename jpy crdjpy_eff
	rename gbp crdgbp_eff
	rename aud crdaud_eff
	rename chf crdchf_eff
	rename cad crdcad_eff
	save dta/netmisp_clean.dta,replace

* import misps cip long
	use dta/misplong_160903.dta,clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	encode ccy, gen(Eccy)
	xtset Eccy monthly 
	save dta/misplong_clean.dta,replace

* new prices import from r
	use workspace826/pricesv1.dta,clear
	gen month=month(date)
	gen year=year(date)
	gen monthly = ym(year,month)
	format monthly %tm
	br date monthly
	tsset monthly
	drop date
	save workspace826/pricesv1_clean.dta,replace


*reg prep
	use regdata_02.dta,clear
	drop _merge
	merge 1:1 monthly using dta/netmisp_clean.dta
	tsset monthly
	gen crdeur_eff_6m=(crdeur_eff+L.crdeur_eff+L2.crdeur_eff+L3.crdeur_eff+L4.crdeur_eff+L5.crdeur_eff)/6
	gen crdjpy_eff_6m=(crdjpy_eff+L.crdjpy_eff+L2.crdjpy_eff+L3.crdjpy_eff+L4.crdjpy_eff+L5.crdjpy_eff)/6
	gen crdgbp_eff_6m=(crdgbp_eff+L.crdgbp_eff+L2.crdgbp_eff+L3.crdgbp_eff+L4.crdgbp_eff+L5.crdgbp_eff)/6
	gen crdaud_eff_6m=(crdaud_eff+L.crdaud_eff+L2.crdaud_eff+L3.crdaud_eff+L4.crdaud_eff+L5.crdaud_eff)/6

* VAR: impulse response with new residualized sprd
	* simple irf euro
	var i_net_USDEUR crdeur_eff, lags(1)
	irf create irfeu, set(irf1,replace) step(10)
	irf graph irf, irf(irfeu) impulse(crdeur_eff) response(i_net_USDEUR) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
	* graph export "../paper/figures/VAR_sirfeur160903.eps",replace
	
	* ortho irf euro
	var  i_net_USDEUR crdeur_eff, lags(1)
	irf create irfeu, order(crdeur_eff i_net_USDEUR) set(irf2,replace) step(10)
	irf graph oirf, irf(irfeu) impulse(crdeur_eff) response(i_net_USDEUR) byopts(title("") subtitle("") note("") legend(off) graphregion(color(white)) plotregion(color(white))) title("") subtitle("") xtitle(month) graphregion(color(white)) bgcolor(white)
	* graph export "../paper/figures/VAR_oirfeur160903.eps",replace
	
* Table 1: Summary Stats
	
	use regdata_02.dta,clear
	capture drop _merge
	merge 1:1 monthly using workspace826/pricesv1_clean.dta, update replace
	capture drop _merge
	merge 1:1 monthly using dta/netmisp_clean.dta
	drop if month==.
	drop monthly
	gen monthly=ym(year,month)
	format monthly %tm
	drop if year<2003
	gen rhoEUR=eusa5-eubsv5-ussw5
	gen rhoGBP=bpsw5-ussw5
	gen rhoJPY=jysw5-ussw5
	gen rhoAUD=adsw5-ussw5
	gen rhoCHF=sfsw5-ussw5
	gen rhoCAD=cdsw5-ussw5

	keep monthly i_net_USD??? crd???_eff year month qrt quarterly quarter rho*
	rename crdeur_eff cseffEUR
	rename crdgbp_eff cseffGBP
	rename crdjpy_eff cseffJPY
	rename crdaud_eff cseffAUD
	rename crdchf_eff cseffCHF
	rename crdcad_eff cseffCAD
	* drop i_net_USDCAD i_net_USDCHF

	reshape long i_net_USD cseff rho, i(monthly) j(ccy) string
	rename i_net_USD issflow
	replace ccy=strlower(ccy)
	encode ccy, gen(Eccy)
	xtset Eccy monthly
	drop if monthly<ym(2003,01)

	gen issflow3mf=(issflow+F.issflow+F2.issflow)/6
	gen issflow6mf=(issflow+F.issflow+F2.issflow+F3.issflow+F4.issflow+F5.issflow)/6


	merge 1:1 monthly ccy using dta/misplong_clean.dta
	bys ccy: su issflow cseff cip credit
	save workspace826/issmispricingslongall.dta,replace
	drop _merge
	drop Eccy

	reshape wide issflow cseff issflow3mf issflow6mf credit cip rho netmisp, i(monthly) j(ccy) string

    capture rm table1_summary.csv
    eststo clear
  	estpost summarize issflow??? credit* cip* cseff*
    esttab using "table1_summary.csv", title(Summary Statistics) cell((count(label(Count)) mean(label(Mean) fmt(a3)) sd(label(S.D.) fmt(a3)) min(label(Min) fmt(a3)) max(fmt(a3) label(Max)))) nonumber append
     
* Table 2:panel reg of 2 misp. alignment
	* use mispricings_long_160730.dta,clear
	use dta/misplong_160903.dta,clear
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

* Table 3: * reg issuance flow on net mispricing
* find out how I created residualizedys2.dta, recreate for all 6 ccys
	* follows workspace/826v1.do
	use regdata_02.dta,clear
		capture drop _merge
		merge 1:1 monthly using workspace826/pricesv1_clean.dta, update replace
		capture drop _merge
		merge 1:1 monthly using dta/netmisp_clean.dta
		drop if month==.
		drop monthly
		gen monthly=ym(year,month)
		format monthly %tm
		drop if year<2003
		gen rhoEUR=eusa5-eubsv5-ussw5
		gen rhoGBP=bpsw5-ussw5
		gen rhoJPY=jysw5-ussw5
		gen rhoAUD=adsw5-ussw5
		gen rhoCHF=sfsw5-ussw5
		gen rhoCAD=cdsw5-ussw5

		keep monthly i_net_USD??? crd???_eff year month qrt quarterly quarter rho*
		rename crdeur_eff cseffEUR
		rename crdgbp_eff cseffGBP
		rename crdjpy_eff cseffJPY
		rename crdaud_eff cseffAUD
		rename crdchf_eff cseffCHF
		rename crdcad_eff cseffCAD

		reshape long i_net_USD cseff rho, i(monthly) j(ccy) string
		rename i_net_USD issflow
		encode ccy, gen(Eccy)
		xtset Eccy monthly
		drop if monthly<ym(2003,01)

		gen issflow3mf=(issflow+F.issflow+F2.issflow)/6
		gen issflow6mf=(issflow+F.issflow+F2.issflow+F3.issflow+F4.issflow+F5.issflow)/6


		local filename "stataout/issuancenetmispricing"
	    capture rm `filename'.csv
	    *NEWEY with panel 
	    eststo clear
	    tsset  Eccy monthly 
	    * this one is pretty good
		eststo:newey F.issflow6m cseff,lag(6) force   
		eststo:newey F.issflow6m cseff if ccy=="EUR",lag(6) 
		eststo:newey F.issflow6m cseff if ccy=="GBP",lag(6) 
		eststo:newey F.issflow6m cseff if ccy=="JPY",lag(6) 
		eststo:newey F.issflow6m cseff if ccy=="AUD",lag(6) 
		eststo:newey F.issflow6m cseff if ccy=="CHF",lag(6) 
		eststo:newey F.issflow6m cseff if ccy=="CAD",lag(6) 
	    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append


	* collapsing into quarterly
		drop quarterly
		gen quarterly=yq(year,quarter)
		format quarterly %tq
		collapse (mean) issflow cseff, by(quarterly year quarter Eccy ccy) 

		* quarterly newey 
	    tsset  Eccy quarterly
		eststo clear
	    * this one is pretty good
		eststo:newey F.issflow cseff,lag(1) force   
		eststo:newey F.issflow cseff if ccy=="EUR",lag(1) 
		eststo:newey F.issflow cseff if ccy=="GBP",lag(1) 
		eststo:newey F.issflow cseff if ccy=="JPY",lag(1) 
		eststo:newey F.issflow cseff if ccy=="AUD",lag(1) 
		eststo:newey F.issflow cseff if ccy=="CHF",lag(1) 
		eststo:newey F.issflow cseff if ccy=="CAD",lag(1) 
	    esttab using `filename'.csv, title("Issflow and net mispricing'") bracket r2 nostar nogaps nonote addnotes(" ") order() append

