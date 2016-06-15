* Table 8: credit spread impact on basis directly
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


gen eurchgf3=log(F3.eur/eur)
gen gbpchgf3=log(F3.gbp/gbp)
gen jpychgf3=log(F3.jpy/jpy)
gen audchgf3=log(F3.aud/aud)

gen eurchgf6=log(F6.eur/eur)
gen gbpchgf6=log(F6.gbp/gbp)
gen jpychgf6=log(F6.jpy/jpy)
gen audchgf6=log(F6.aud/aud)

local filename "Table10_FX_forecast"
    capture rm `filename'.csv
    eststo clear


eststo: neweymod eurchgf3 Cdif_euus_30_eff post07, lag(6)
eststo: neweymod eurchgf3 Cdif_euus_30_eff ratediff_euus ussw10 post07, lag(6)

eststo: neweymod gbpchgf3 Cdif_gbus_30_eff post07, lag(6)
eststo: neweymod gbpchgf3 Cdif_gbus_30_eff ratediff_gbus ussw10 post07, lag(6)

eststo: neweymod jpychgf3 Cdif_jpus_30_eff post07, lag(6)
eststo: neweymod jpychgf3 Cdif_jpus_30_eff ratediff_jpus ussw10 post07, lag(6)

eststo: neweymod audchgf3 Cdif_auus_30_eff post07, lag(6)
eststo: neweymod audchgf3 Cdif_auus_30_eff ratediff_auus ussw10 post07, lag(6)

eststo: neweymod eurchgf6 Cdif_euus_30_eff post07, lag(6)
eststo: neweymod eurchgf6 Cdif_euus_30_eff ratediff_euus ussw10 post07, lag(6)

eststo: neweymod gbpchgf6 Cdif_gbus_30_eff post07, lag(6)
eststo: neweymod gbpchgf6 Cdif_gbus_30_eff ratediff_gbus ussw10 post07, lag(6)

eststo: neweymod jpychgf6 Cdif_jpus_30_eff post07, lag(6)
eststo: neweymod jpychgf6 Cdif_jpus_30_eff ratediff_jpus ussw10 post07, lag(6)

eststo: neweymod audchgf6 Cdif_auus_30_eff post07, lag(6)
eststo: neweymod audchgf6 Cdif_auus_30_eff ratediff_auus ussw10 post07, lag(6)

esttab using `filename'.csv, title("Multivariate Forecasting'") rename(Cdif_euus_30_eff CrdSprdDiffEff Cdif_gbus_30_eff CrdSprdDiffEff Cdif_jpus_30_eff CrdSprdDiffEff Cdif_auus_30_eff CrdSprdDiffEff ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(CrdSprdDiffEff exchrate ussw10 ratediff cy30_govtoas post07) append


eststo clear
eststo: neweymod eurchgf3 eubs10 post07, lag(6)
eststo: neweymod eurchgf3 eubs10 ratediff_euus ussw10 post07, lag(6)

eststo: neweymod gbpchgf3 bpbs10 post07, lag(6)
eststo: neweymod gbpchgf3 bpbs10 ratediff_gbus ussw10 post07, lag(6)

eststo: neweymod jpychgf3 jybs10 post07, lag(6)
eststo: neweymod jpychgf3 jybs10 ratediff_jpus ussw10 post07, lag(6)

eststo: neweymod audchgf3 adbs10 post07, lag(6)
eststo: neweymod audchgf3 adbs10 ratediff_auus ussw10 post07, lag(6)

eststo: neweymod eurchgf6 eubs10 post07, lag(6)
eststo: neweymod eurchgf6 eubs10 ratediff_euus ussw10 post07, lag(6)

eststo: neweymod gbpchgf6 bpbs10 post07, lag(6)
eststo: neweymod gbpchgf6 bpbs10 ratediff_gbus ussw10 post07, lag(6)

eststo: neweymod jpychgf6 jybs10 post07, lag(6)
eststo: neweymod jpychgf6 jybs10 ratediff_jpus ussw10 post07, lag(6)

eststo: neweymod audchgf6 adbs10 post07, lag(6)
eststo: neweymod audchgf6 adbs10 ratediff_auus ussw10 post07, lag(6)

esttab using `filename'.csv, title("Multivariate Forecasting'") rename(eubs10 XCBS10 bpbs10 XCBS10 jybs10 XCBS10 adbs10 XCBS10 ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(XCBS10 exchrate ussw10 ratediff cy30_govtoas post07) append


eststo clear
eststo: neweymod eurchgf3 eubs10, lag(6)
eststo: neweymod eurchgf3 eubs10 ratediff_euus ussw10, lag(6)

eststo: neweymod gbpchgf3 bpbs10, lag(6)
eststo: neweymod gbpchgf3 bpbs10 ratediff_gbus ussw10, lag(6)

eststo: neweymod jpychgf3 jybs10, lag(6)
eststo: neweymod jpychgf3 jybs10 ratediff_jpus ussw10, lag(6)

eststo: neweymod audchgf3 adbs10, lag(6)
eststo: neweymod audchgf3 adbs10 ratediff_auus ussw10, lag(6)

eststo: neweymod eurchgf6 eubs10, lag(6)
eststo: neweymod eurchgf6 eubs10 ratediff_euus ussw10, lag(6)

eststo: neweymod gbpchgf6 bpbs10, lag(6)
eststo: neweymod gbpchgf6 bpbs10 ratediff_gbus ussw10, lag(6)

eststo: neweymod jpychgf6 jybs10, lag(6)
eststo: neweymod jpychgf6 jybs10 ratediff_jpus ussw10, lag(6)

eststo: neweymod audchgf6 adbs10, lag(6)
eststo: neweymod audchgf6 adbs10 ratediff_auus ussw10, lag(6)

esttab using `filename'.csv, title("Multivariate Forecasting'") rename(eubs10 XCBS10 bpbs10 XCBS10 jybs10 XCBS10 adbs10 XCBS10 ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(XCBS10 exchrate ussw10 ratediff cy30_govtoas post07) append


eststo clear
eststo: neweymod eurchgf3 eubs10 if year<2008, lag(6)

eststo: neweymod gbpchgf3 bpbs10 if year<2008, lag(6)

eststo: neweymod jpychgf3 jybs10 if year<2008, lag(6)

eststo: neweymod audchgf3 adbs10 if year<2008, lag(6)

eststo: neweymod eurchgf6 eubs10 if year<2008, lag(6)

eststo: neweymod gbpchgf6 bpbs10 if year<2008, lag(6)

eststo: neweymod jpychgf6 jybs10 if year<2008, lag(6)

eststo: neweymod audchgf6 adbs10 if year<2008, lag(6)

esttab using `filename'.csv, title("Simple pre08") rename(eubs10 XCBS10 bpbs10 XCBS10 jybs10 XCBS10 adbs10 XCBS10 ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(XCBS10 exchrate ussw10 ratediff cy30_govtoas post07) append


eststo clear
eststo: neweymod eurchgf3 eubs10 L3.eurchgf3 if year>2009, lag(6)

eststo: neweymod gbpchgf3 bpbs10 L3.gbpchgf3 if year>2009, lag(6)

eststo: neweymod jpychgf3 jybs10 L3.jpychgf3 if year>2009, lag(6)

eststo: neweymod audchgf3 adbs10 L3.audchgf3 if year>2009, lag(6)

eststo: neweymod eurchgf6 eubs10 L6.eurchgf6 if year>2009, lag(6)

eststo: neweymod gbpchgf6 bpbs10 L6.gbpchgf6 if year>2009, lag(6)

eststo: neweymod jpychgf6 jybs10 L6.jpychgf6 if year>2009, lag(6)

eststo: neweymod audchgf6 adbs10 L6.audchgf6 if year>2009, lag(6)

esttab using `filename'.csv, title("Simple post09 with lag") rename(eubs10 XCBS10 bpbs10 XCBS10 jybs10 XCBS10 adbs10 XCBS10 ratediff_euus ratediff ratediff_gbus ratediff ratediff_jpus ratediff ratediff_auus ratediff eurchg exchrate gbpchg exchrate jpychg exchrate audchg exchrate I_net_USDEUR Iss I_net_USDGBP Iss I_net_USDJPY Iss I_net_USDAUD Iss i_net_USDEUR iss i_net_USDGBP iss i_net_USDJPY iss i_net_USDAUD iss ) bracket r2 nostar nogaps nonote addnotes(" ") order(XCBS10 exchrate ussw10 ratediff cy30_govtoas post07) append
