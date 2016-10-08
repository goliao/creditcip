use dta/dtl4_upglobalonly.dta,clear
br
format date %td


br * if date==date("31may2016","DMY",2000)

xi: reg swapsprd i.ccy i.upcusip  if date==date("31may2016","DMY",2000)







use regdata_02.dta,clear
keep I_* I_* date month* year* quarter*
save regdata_02_160901_simple.dta,replace





* # test whether net mispricing is smaller in august when issuance is smaller, doesn't really work
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

	gen netmisp=credit-cip
	gen absnetmisp=abs(netmisp)

	gen month=month(date)
	gen year=year(date)
	gen daug=1 if month==8
	replace daug=0 if daug==.

	reg absnetmisp daug if year>2005
	xtreg absnetmisp daug if year>2005,fe
	bys month: su netmisp
	reg absnetmisp i.month if year>2007

*  See if when issuance flow is smaller in abs dollar terms, is deviation larger?
*  works well!!!!!!!!
	* follows workspace/826v1.do
	use regdata_02.dta,clear
		capture drop _merge
		merge 1:1 monthly using workspace826/pricesv1_clean.dta, update replace
		capture drop _merge
		merge 1:1 monthly using effresys_160826_retro_0625_clean.dta
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

		keep monthly I_net_USD??? ccy???_eff year month qrt quarterly quarter rho*
		rename ccyeur_eff cseffEUR
		rename ccygbp_eff cseffGBP
		rename ccyjpy_eff cseffJPY
		rename ccyaud_eff cseffAUD
		rename ccychf_eff cseffCHF
		rename ccycad_eff cseffCAD

		reshape long I_net_USD cseff rho, i(monthly) j(ccy) string
		rename I_net_USD issflow
		encode ccy, gen(Eccy)
		xtset Eccy monthly
		drop if monthly<ym(2003,01)

		gen issflow3mf=(issflow+F.issflow+F2.issflow)/6
		gen issflow6mf=(issflow+F.issflow+F2.issflow+F3.issflow+F4.issflow+F5.issflow)/6

	    xtset  Eccy monthly 
	    gen absiss=abs(issflow)
	    gen absnetmisp=abs(cseff)
	    reg absnetmisp absiss if year>=2007
	    xtreg absnetmisp absiss if year>=2007,fe 

	    drop quarterly
		gen quarterly=yq(year,quarter)
		format quarterly %tq
		collapse (mean) absnetmisp absiss, by(quarterly year quarter Eccy ccy) 
		xtset  Eccy quarterly
	    reg absnetmisp absiss if year>=2007
	    xtreg absnetmisp absiss if year>=2007,fe 


	    su quarter
	    reg absnetmisp i.quarter

    * see how much more issuance flow in dollar amt are there for eurusd relative to others
	use regdata_02.dta,clear
	gen IabsEUR=abs(I_net_USDEUR)
	gen IabsGBP=abs(I_net_USDGBP)
	gen IabsJPY=abs(I_net_USDJPY)
	gen IabsAUD=abs(I_net_USDAUD)
	gen IabsCHF=abs(I_net_USDCHF)
	gen IabsCAD=abs(I_net_USDCAD)

	su I_net_USD???
	su Iabs??? if year>2007
	* For instance, since 2008, the average dollar amount of monthly issuance flow
	* still need to calculate gross issuance flow, not net
