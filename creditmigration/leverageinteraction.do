use regdata_02.dta,clear
drop _merge
merge 1:1 monthly using bdlev.dta
sort monthly
tsset monthly


local filename "leverage2"
capture rm `filename'.csv

* gen gllev=bdleverage2
* replace gllev=100 if year<2008

local levmeasure bdleverage2
eststo clear
capture drop Iss_??_LW
gen Iss_EU_LW=I_net_USDEUR/`levmeasure'
eststo: reg D.eubs10 I_net_USDEUR  if (abs(I_net_USDEUR)>.1)
eststo: reg D.eubs10 Iss_EU_LW if (abs(I_net_USDEUR)>.1)
* twoway (tsline D.eubs10) (tsline Iss_EU_LW,yaxis(2))

gen Iss_GB_LW=I_net_USDGBP/`levmeasure'
eststo: reg D.bpbs10 I_net_USDGBP if (abs(I_net_USDGBP)>.1)
eststo: reg D.bpbs10 Iss_GB_LW if (abs(I_net_USDGBP)>.1)

gen Iss_JP_LW=I_net_USDJPY/`levmeasure'
eststo: reg D.jybs7 I_net_USDJPY if (abs(I_net_USDJPY)>.1)
eststo: reg D.jybs7 Iss_JP_LW if (abs(I_net_USDJPY)>.1)
eststo: reg D.jybs10 Iss_JP_LW if (abs(I_net_USDJPY)>.1)

gen Iss_AD_LW=I_net_USDAUD/`levmeasure'
eststo: reg D.adbs10 I_net_USDAUD if (abs(I_net_USDAUD)>.1)
eststo: reg D.adbs10 Iss_AD_LW if (abs(I_net_USDAUD)>.1)

esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet Iss_EU_LW IssLevWeight Iss_GB_LW IssLevWeight Iss_JP_LW IssLevWeight Iss_AD_LW IssLevWeight) bracket r2 star(* 0.10 ** 0.05 *** 0.01) nogaps nonote addnotes(" ") order(IssNet IssLevWeight) append


******************************************
local filename "leverage"
capture rm `filename'.csv

local levmeasure bdleverage2
local levmeasure lnlev2
* local levmeasure lnlev2D
* local levmeasure lnlev
* local levmeasure lnlevD
* local levmeasure lnlevnorm2
* local levmeasure levfac2
* local levmeasure levfac2adj
eststo clear
drop issuancelev*
gen issuancelevEU=I_net_USDEUR*`levmeasure'
eststo: reg D.eubs10 I_net_USDEUR if (abs(I_net_USDEUR)>.1)
eststo: reg D.eubs10 I_net_USDEUR `levmeasure' issuancelevEU if (abs(I_net_USDEUR)>.1)

gen issuancelevGB=I_net_USDGBP*`levmeasure'
eststo: reg D.bpbs10 I_net_USDGBP if (abs(I_net_USDGBP)>.1)
eststo: reg D.bpbs10 I_net_USDGBP `levmeasure' issuancelevGB if (abs(I_net_USDGBP)>.1)

gen issuancelevJP=I_net_USDJPY*`levmeasure'
eststo: reg D.jybs7 I_net_USDJPY if (abs(I_net_USDJPY)>.1)
eststo: reg D.jybs7 I_net_USDJPY `levmeasure' issuancelevJP if (abs(I_net_USDJPY)>.1)

eststo: reg D.jybs10 I_net_USDJPY if (abs(I_net_USDJPY)>.1)
eststo: reg D.jybs10 I_net_USDJPY `levmeasure' issuancelevJP if (abs(I_net_USDJPY)>.1)

gen issuancelevAD=I_net_USDAUD*`levmeasure'
eststo: reg D.adbs10 I_net_USDAUD if (abs(I_net_USDAUD)>.1)
eststo: reg D.adbs10 I_net_USDAUD `levmeasure' issuancelevAD if (abs(I_net_USDAUD)>.1)

esttab using `filename'.csv, title("Basis") rename(I_net_USDEUR IssNet I_net_USDGBP IssNet I_net_USDJPY IssNet I_net_USDAUD IssNet issuancelevEU IssLev issuancelevGB IssLev issuancelevJP IssLev issuancelevAD IssLev) bracket r2 nostar nogaps nonote addnotes(" ") order(IssNet `levmeasure' IssLev) append


***************************************************



gen issuancelevEU=I_net_USDEUR*lnlev2
reg D.eubs10 I_net_USDEUR lnlev2 issuancelevEU


drop issuancelevEU
gen issuancelevEU=I_net_USDEUR*bdleverage2
reg D.eubs10 I_net_USDEUR bdleverage2 issuancelevEU 



drop issuancelevEU
gen issuancelevEU=I_net_USDEUR*lnlev
reg D.eubs10 I_net_USDEUR lnlev issuancelevEU

drop issuancelevEU
gen issuancelevEU=I_net_USDEUR*lnlev2D
reg D.eubs10 I_net_USDEUR lnlev2D issuancelevEU

drop issuancelevEU
gen issuancelevEU=I_net_USDEUR*lnlevD
reg D.eubs10 I_net_USDEUR lnlevD issuancelevEU


gen issuancelevEU=I_net_USDEUR*levfac2adj
reg D.eubs10 I_net_USDEUR
reg D.eubs10 I_net_USDEUR lnlev2 issuancelevEU

reg D.eubs10 I_net_USDEUR levfac2adj issuancelevEU


drop issuancelevGB 
gen issuancelevGB=I_net_USDGBP*lnlev2
reg D.bpbs10 I_net_USDGBP
reg D.bpbs10 I_net_USDGBP lnlev2 issuancelevGB

gen issuancelevJP=I_net_USDJPY*lnlev2
reg D.jybs7 I_net_USDJPY
reg D.jybs7 I_net_USDJPY lnlev2 issuancelevJP

gen issuancelevJP=I_net_USDJPY*lnlev2
reg D.jybs10 I_net_USDJPY
reg D.jybs10 I_net_USDJPY lnlev2 issuancelevJP

gen issuancelevAD=I_net_USDAUD*lnlev2
reg D.adbs10 I_net_USDAUD 
reg D.adbs10 I_net_USDAUD lnlev2 issuancelevAD D.adbs1

reg D.adbs1 I_net_USDAUD lnlev2 issuancelevAD


twoway (tsline D.eubs10) (tsline lnlev2,yaxis(2))

