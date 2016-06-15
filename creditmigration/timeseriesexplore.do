* aimed at maximizing visual of spreads
use baml_indices_160308.dta,clear
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


neweymod F.i_net_USDEUR_6mf Cdif_euus_30_libor_eff if year>2006, lag(6)
neweymod F.i_net_USDEUR_6mf Cdif_euus_30_libor if year>2006, lag(6)


graph twoway (tsline F.i_net_USDEUR_6mf, lpattern(l)) (tsline Cdif_euus_30,yaxis(2)) if year>=2002 & year<2016, legend(label(1 "IssEUtoUS/TotIss 6m avg") lab(2 "CrdSprdDiffEff EU-US")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")


graph twoway (tsline F.i_net_USDEUR_6mf, lpattern(l)) (tsline Cdif_euus_30_libor,yaxis(2)) if year>=2002 & year<2016, legend(label(1 "IssEUtoUS/TotIss 6m avg") lab(2 "CrdSprdDiffEff EU-US")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")


graph twoway (tsline F.i_net_USDEUR_6mf, lpattern(l)) (tsline Cdif_euus_30_libor_eff,yaxis(2)) if year>=2002 & year<2016, legend(label(1 "IssEUtoUS/TotIss 6m avg") lab(2 "CrdSprdDiffEff EU-US")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")




graph twoway (tsline i_net_USDEUR, lpattern(l)) (tsline Cdif_euus_40_nflibor_eff,yaxis(2)) if year>=2007 & year<2016, legend(label(1 "IssEUtoUS/TotIss 6m avg") lab(2 "CrdSprdDiffEff EU-US")) ttitle(none) ytitle("Pct Net Iss flow EU to US/Tot Iss") ytitle("basis points",axis(2)) ttitle("")



tsline Cdif_euus_40_nflibor Cdif_euus_30_libor if year>2006
tsline Cdif_euus_40_nflibor Cdif_euus_40_libor  Cdif_euus_40 if year>2006
tsline 

eusb10 
drop uslibois
gen uslibois=(us0003m-ussoc)*100
gen eulibois=(eur003m-euswec)*100
gen useulibois=uslibois-eulibois
tsline uslibois eulibois
gen eubs10ois=eubs10+useulibois
label var eubs10 "eubs10"
tsline eubs10 eubs10ois if year>2003
tsline useulibois
reg D.eubs10 I_net_USDEUR if (abs(I_net_USDEUR)>.1)
reg D.eubs10ois I_net_USDEUR if (abs(I_net_USDEUR)>.1)