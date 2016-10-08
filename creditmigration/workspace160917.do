cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"
clear matrix
adopath + .
clear
set more on
set matsize 11000

use dta/isscred_firm1.dta,clear

* gen year=year(ym)
* gen month=month(ym)
* gen monthly=ym(year,month)
* format monthly %tm
format ym %tm

encode ccy, gen(Eccy)
encode upcusip, gen(Ecusip)

* keep if ccy=="eur"

gen upcusipccy=upcusip+ccy
encode upcusipccy,gen(Eupcusipccy)

xi:reg  issued crddiff cip i.ccy i.upcusip i.ym, robust cluster(ccy)


xtset Eupcusipccy ym
xtreg issued crddiff cip, fe
xi:xtreg issued crddiff cip, fe


*** Testing panel regression with newy
use temp.dta,clear
gen year=year(date)
gen month=month(date)
gen monthly=ym(year,month)
format monthly %tm
encode ccy, gen(Eccy)

*  fixed effect
xi:reg credit cip i.ccy

* panel reg
xtset Eccy monthly
xtreg credit cip,fe

* # panel with newey
tsset Eccy monthly
newey credit cip, lag(25) force

newey2 credit cip if ccy=="eur", lag(19) t(monthly)
newey2 credit cip if ccy=="gbp", lag(6) t(monthly)
newey2 credit cip if ccy=="jpy", lag(3) t(monthly)

keep if ccy=="aud"
tsset monthly
newey2 credit cip, lag(1) t(monthly)
newey credit cip, lag(6) 
neweymod credit cip, lag(1) 
neweymod credit cip, lag(1) 
newey2 credit cip if ccy=="chf", lag(3) t(monthly)
newey2 credit cip if ccy=="cad", lag(3) t(monthly)
newey2 credit cip, lag(3) t(monthly)