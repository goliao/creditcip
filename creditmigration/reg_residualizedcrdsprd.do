cd "/Users/gliao/Dropbox/Research/ccy basis/creditmigration"
use temp.dta,clear
gen month=month(date)
gen year=year(date)
gen monthly = ym(year,month)
format monthly %tm
tsset monthly

gen i_net_euus_6mf=(i_net_euus+F.i_net_euus+F2.i_net_euus+F3.i_net_euus+F4.i_net_euus+F5.i_net_euus)/6
gen i_net_euus_3mf=(i_net_euus+F.i_net_euus+F2.i_net_euus)/3

reg F.i_net_euus Cdif_euus_30_eff
reg F.i_net_euus Cdif_euus_30
reg F.i_net_euus_6mf Cdif_euus_30_eff

reg F.I_net_euus euus_yldsprd

capture drop euus_yldsprd_eff
gen euus_yldsprd_eff=euus_yldsprd-eubs5
neweymod F.i_net_euus euus_yldsprd_eff if year<2016 & year>2009,lag(6)
neweymod F.i_net_euus_6mf euus_yldsprd_eff if year<2016 & year>2008,lag(6)
neweymod F.i_net_euus_6mf euus_yldsprd_eff,lag(6)
neweymod F.i_net_euus euus_yldsprd_eff,lag(6)

reg D.eubs5 I_net_euus if I_net_euus!=0 & year>2008 & year<2016

twoway (tsline F.i_net_euus_6mf) (tsline euus_yldsprd_eff,yaxis(2))

twoway (tsline I_net_euus) (tsline D.eubs5) if year<2016 & year>2008

reg F.I_net_euus euus_oas
reg F.I_net_euus euus_as