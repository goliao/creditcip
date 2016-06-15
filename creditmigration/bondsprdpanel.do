
*temp exploratory work using bbg spreads

cd "C:\Users\gliao\Dropbox\Research\ccy basis\creditmigration"
cd "\Users\gliao\Dropbox\Research\ccy basis\creditmigration"


clear matrix
adopath + .
clear
set more off

use bondsprdpanel.dta,clear

gen date=date(datestr,"YMD",2050)
format date %td
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
drop datestr date month
sort monthly

rename id_bb_ultimate_co upco

encode isin, gen(Eisin)


xi i.upco*i.monthly


reg BLP_Z_SPRD_MID _IupcXmon*



xtset Eisin monthly
xtset upco monthly
tabulate upco, gen(Dup)


xi: reg BLP_Z_SPRD_MID i.upco i.monthly i.ccy*i.monthly