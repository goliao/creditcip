* explore ecb excess liqudiity etc


twoway (tsline ecblxliq) (tsline eubs5,yaxis(2) legend(label(1 "ECB excess liqudiity") lab(2 "CIP deviation")) ttitle(none) ytitle("excess liquidity") ytitle("basis points",axis(2)) ttitle("")) if year>2008 
 ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")


twoway (tsline eur) (tsline eubs5,yaxis(2) legend(label(1 "EUR") lab(2 "CIP deviation")) ytitle("EUR") ytitle("basis points",axis(2)) ttitle("")) if year>2007
reg eubsc ecblxliq
reg eubs5 ecblxliq
reg eubs10 ecblxliq

reg D.eubs10 D.ecblxliq

reg D.eubsc D.ecblxliq
reg D.eubs10 D.ecblxliq 



reg D.eubs10 I_net_USDEUR LD.eubs10
reg D.jybs7 I_net_USDJPY LD.jybs7 if (abs(I_net_USDJPY)>.1)

neweymod I_net_USDEUR L.ecblxliq, lag(6)


use centralbankbalancesheet.dta, replace
gen date=date(datestr,"MDY",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly=ym(year,month)
format monthly %tm
order monthly,first
collapse (lastnm) datestr-year, by(monthly) fast
keep monthly boj fed ecb
save centralbankbalancesheet_clean.dta,replace


use regdata_02.dta,clear
drop _merge
merge 1:1 monthly using centralbankbalancesheet_clean.dta

gen BSdiff_fedecb=fed-ecb
gen BSdiff_fedboj=fed-boj


corr BSdiff_fedecb eubs10
corr BSdiff_fedboj jybs10


twoway (tsline BSdiff_fedecb) (tsline eubs10, yaxis(2)) if year>2005
twoway (tsline BSdiff_fedboj) (tsline jybs10, yaxis(2)) if year>2005


reg eubsc BSdiff_fedecb
twoway (tsline BSdiff_fedecb) (tsline eubsc, yaxis(2)) if year>2005

reg eubs10 BSdiff_fedecb
