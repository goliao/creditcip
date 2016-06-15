
* roll together and clean the sdc code, this takes a while
* do "/Users/gliao/Dropbox/Research/ccy basis/code/dataprep_sdc.do"

* clean baml bond indices
use baml_indices.dta,clear
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
tsset monthly
drop month date 
ds *_z,v(32)
drop *_z
save baml_indices_clean.dta,replace

* all bbg prices
use bbg_prices.dta,clear
gen date=date(datestr,"YMD",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
drop datestr date month
sort monthly
tsset monthly
save bbg_prices_clean.dta, replace


* barc
cd "/Users/gliao/Dropbox/Research/ccy basis/data/barc"
insheet using euro_corp_sprd.csv,clear name
ds,var(32)
rename euroaggregatecorporateazvspread barc_eur_a_zvsprd
destring barc_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"MDY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
by month year,sort: egen lastdateofmonth=max(date)
format lastdateofmonth %td
drop if date~=lastdateofmonth
drop lastdateofmonth
format monthly %tm
sort monthly
tsset monthly
keep monthly barc*
save barc_eur.dta,replace

insheet using usd_corp_sprd2.csv,clear name
rename acorporatezvspread barc_usd_a_zvsprd
destring barc_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"MDY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
by month year,sort: egen lastdateofmonth=max(date)
format lastdateofmonth %td
drop if date~=lastdateofmonth
drop lastdateofmonth
format monthly %tm
sort monthly
tsset monthly
keep monthly barc*
save barc_usd.dta,replace

use barc_usd.dta,clear
merge 1:1 monthly using barc_eur.dta
drop _merge
save barc_a.dta, replace
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration/"
save barc_a.dta, replace

* JPM
cd "/Users/gliao/Dropbox/Research/ccy basis/data/JPM"
insheet using JPM_EUR_A_7-10yr3.csv,clear name
rename corporatesa710modduration jpm_eur_a_710_moddur
rename corporatesa710yield jpm_eur_a_710_yield
rename corporatesa710assetswapspread jpm_eur_a_710_assetswap
rename corporatesa710govtspread jpm_eur_a_710_govsprd
rename corporatesa710numberofbonds jpm_eur_a_710_nbonds
drop corporatesa710indexlevel
destring jpm_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"DMY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
drop datestr date month
sort monthly
tsset monthly
* twoway (tsline jpm_eur_a_710_govsprd) (tsline jpm_eur_a_710_assetswap, yaxis(2)) 
save jpm_eur_a_710.dta,replace

insheet using JPM_USD_JULI_A_7-10yr.csv,clear name
rename juliallma710portfoliospreadtreas jpm_usd_a_710_portsprdtreas
rename juliallma710portfoliospreadlibor jpm_usd_a_710_portsprdlibor
rename juliallma710avgzspreadtreasuryal jpm_usd_a_710_zsprdtreas
rename juliallma710avgzspreadliborall jpm_usd_a_710_zsprdlibor
destring jpm_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"DMY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
keep monthly jpm*
sort monthly
tsset monthly
save jpm_usd_a_710.dta,replace

insheet using JPM_GBP_A_7-10yr.csv,clear name
rename corporatesa710modduration jpm_gbp_a_710_moddur
rename corporatesa710yield jpm_gbp_a_710_yield
rename corporatesa710assetswapspread jpm_gbp_a_710_assetswap
rename corporatesa710govtspread jpm_gbp_a_710_govsprd
rename corporatesa710numberofbonds jpm_gbp_a_710_nbonds
drop corporatesa710indexlevel
destring jpm_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"DMY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
drop datestr date month
sort monthly
tsset monthly
save jpm_gbp_a_710.dta,replace

insheet using JPM_JPY_A_7-10yr.csv,clear name
rename corporatesa710modduration jpm_jpy_a_710_moddur
rename corporatesa710yield jpm_jpy_a_710_yield
rename corporatesa710assetswapspread jpm_jpy_a_710_assetswap
rename corporatesa710govtspread jpm_jpy_a_710_govsprd
rename corporatesa710numberofbonds jpm_jpy_a_710_nbonds
keep jpm_jpy_a_710_moddur jpm_jpy_a_710_yield jpm_jpy_a_710_assetswap jpm_jpy_a_710_govsprd jpm_jpy_a_710_nbonds date
destring jpm_*,replace ignore("N/A")
rename date datestr
gen date=date(datestr,"DMY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
drop datestr date month
sort monthly
tsset monthly
save jpm_jpy_a_710.dta,replace

* merge all jpm files together
use jpm_eur_a_710.dta,clear
merge 1:1 monthly using jpm_usd_a_710.dta
drop _merge
merge 1:1 monthly using jpm_gbp_a_710.dta
drop _merge
merge 1:1 monthly using jpm_jpy_a_710.dta
drop _merge
save jpm_g4_a_710.dta,replace
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration/"
save jpm_g4_a_710.dta,replace




insheet using rating.csv,clear
save rating.dta,replace


*  merge together all data sources!
use baml_indices_clean,clear
merge 1:1 monthly using bbg_prices_clean.dta
drop _merge
* merge JPM credit spread
merge 1:1 monthly using jpm_g4_a_710.dta
drop _merge
ds jpm*,var(32)
gen Crd_diff_jpm_gov=jpm_eur_a_710_govsprd -jpm_usd_a_710_portsprdtreas
gen Crd_diff_jpm_as=jpm_eur_a_710_assetswap -jpm_usd_a_710_portsprdlibor
* merge with barc
merge 1:1 monthly using barc_a.dta
drop _merge
ds barc*,var(32)
gen Crd_diff_barc_zvs=(barc_eur_a_zvsprd-barc_usd_a_zvsprd)*100
* replace Crd_diff_barc_zvs=. if year<2001
tsset monthly
save prices.dta, replace


* add credit sprds
price_extend.do




******** daily bbg price

* daily all bbg prices 
use bbg_prices_daily.dta,clear
gen date=date(datestr,"YMD",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
sort date
tsset date
ds, v(32)
drop *_z
save bbg_prices_daily_clean.dta, replace



* graph twoway tsline eubsc eubs10 if year>2005 & eubsc>-150, legend(label(1 "Eur basis 3mo") lab(2 "Eur basis 10yr")) ttitle("") ytitle("basis points")  yline(0) ysc(range(-150 30))
* graph export "figures/basiseur.eps",replace

* graph twoway tsline eubs10 bpbs10 jybs10 adbs10 if year>1996, legend(label(1 "EUR basis") lab(2 "GBP basis") lab(3 "JPY basis") lab(4 "AUD basis")) ttitle("") ytitle("basis points")  yline(0) 
* graph export "figures/basisall.eps",replace


* daily baml
use baml_indices_daily.dta,clear
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
tsset date
save baml_indices_daily_clean.dta,replace


******
cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"

*/*
* try to explore % of total issuance in euro vs us
* specifically, create a variable: netissuanceabrd as % of total issuance for all EU/US firms; this is likely to be small
* failed:: now create a variable: netissuanceabrd as a % of only European firms; this is bigger but harder to justify
* ideally, create a variable: netissuanceabrd as a % of all firms that has the ability to issue in both mkts; this eliminates firms that would never consider issuing abrd
* not sure how much seasonality there are, but we can try to see removing seasonality would help
* next things to try: 3 mo rolling avg, seasonality adj, use different OAS sprd

*** data prep
** do data_cleaning_20151121.do

/* Add foreign/upforeign, humanize ccy code use original uncleaned version

use sdc96,clear  
drop uop
drop buss

replace nat="United States" if (nat=="Cayman Islands" | nat=="Bermuda" | nat=="Jersey") & (exch=="New York" | exch=="Nasdaq")
replace upnat="United States" if (upnat=="Cayman Islands" | upnat=="Bermuda" | upnat=="Jersey") & (exch=="New York" | exch=="Nasdaq")
drop Enat Eupnat
encode nat, gen(Enat)
encode upnat, gen(Eupnat)

gen month=month(d)
gen year=year(d)
gen monthly = ym(year,month)
format monthly %tm

* define nat source: nat is closer to 
gen modnat=nat
replace modnat="Eurozone" if inlist(modnat,"Austria","Belgium","Cyprus","Finland","France","Germany","Greece")
replace modnat="Eurozone" if inlist(modnat,"Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta")
replace modnat="Eurozone" if inlist(modnat,"Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")

gen modupnat=upnat
replace modupnat="Eurozone" if inlist(modupnat,"Austria","Belgium","Cyprus","Finland","France","Germany","Greece")
replace modupnat="Eurozone" if inlist(modupnat,"Ireland","Ireland-Rep","Italy","Latvia","Lithuania","Luxembourg","Malta")
replace modupnat="Eurozone" if inlist(modupnat,"Netherlands","Portugal","Slovak Rep","Spain","Denmark","Slovenia")

save sdc96_clean1.dta,replace

* *** merge in human readable currency code and main currency for each country
* insheet using currency_code_country.csv, names clear
* drop ctry
* save currency_code_country.dta,replace
* insheet using currency_code_country.csv, names clear
* rename ctry nat
* rename ccy mainccy
* drop cur
* save currency_code_country2.dta,replace
* rename nat upnat
* rename mainccy upmainccy
* save currency_code_country3.dta,replace

use sdc96_clean1,clear
merge m:m cur using currency_code_country.dta
keep if _merge==3
drop _merge

merge m:1 nat using currency_code_country2.dta
* keep if _merge==3
drop _merge
drop cur
* note that we dropped countries for which upnat is not in the data 
merge m:1 upnat using currency_code_country3.dta
keep if _merge==3
drop _merge


gen foreign=1 if ccy!=mainccy
replace foreign=0 if foreign==.

gen upforeign=1 if ccy!=upmainccy
replace upforeign=0 if upforeign==.

gen qrt=qofd(d)
format %tq qrt
gen sic1=substr(sicp,1,1)
destring sic1,replace
gen sic2=substr(sicp,1,2)
destring sic2,replace

gen rating=sp
replace rating=mdy if rating=="" | rating=="NR"
merge m:1 rating using rating.dta
drop _merge
save sdc96_clean2.dta,replace

*/

* use sdc96_clean2,clear
* keep if foreign==1
* keep i
* duplicates drop 
* gen globalissuer=1
* save globalissuer.dta,replace