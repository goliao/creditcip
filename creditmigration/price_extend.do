
use prices.dta, clear
su er30_govtoas if year>2001 & date<d(01oct2015)

replace eubs10=. if year<2002
replace eubs7=. if year<2002
replace eubs5=. if year<2002
replace eubs1=. if year<2002
replace eusa10=. if year<2002
replace eusa7=. if year<2002
replace eusa5=. if year<2002
replace eusa2=. if year<2002
replace eur=. if year<2002


*EUR
gen Cdif_euus_00=er00_govtoas -cy00_govtoas 
gen Cdif_euus_10=er10_govtoas -cy10_govtoas 
gen Cdif_euus_20=er20_govtoas -cy20_govtoas 
gen Cdif_euus_30=er30_govtoas -cy30_govtoas 
* replace cy30_liboroas=. if year<2004
* gen Cdif_euus_30_libor=er30_liboroas -cy30_liboroas 
* gen Cdif_euus_40_libor=er40_liboroas -cy40_liboroas 
* gen Cdif_euus_40_nflibor=en40_liboroas -c4nf_liboroas 
* br monthly en40_liboroas c4nf_liboroas Cdif_euus_40_nflibor
* replace Cdif_euus_40_nflibor=. if year<2007

gen Cdif_euus_33=er33_govtoas -cy33_govtoas 
gen Cdif_euus_34=er34_govtoas -cy34_govtoas 
gen Cdif_euus_36=er36_govtoas -cy36_govtoas 
gen Cdif_euus_40=er40_govtoas -cy40_govtoas 
gen Cdif_euus_43=er43_govtoas -cy43_govtoas 
gen Cdif_euus_44=er44_govtoas -cy44_govtoas 

    local basis eubs10
    gen Cdif_euus_00_eff=Cdif_euus_00-`basis'
    gen Cdif_euus_10_eff=Cdif_euus_10-`basis'
    gen Cdif_euus_20_eff=Cdif_euus_20-`basis'
    gen Cdif_euus_30_eff=Cdif_euus_30-`basis'
    * gen Cdif_euus_30_libor_eff=Cdif_euus_30_libor-`basis'
    * gen Cdif_euus_40_nflibor_eff=Cdif_euus_40_nflibor-`basis'
    gen Cdif_euus_33_eff=Cdif_euus_33-`basis'
    gen Cdif_euus_34_eff=Cdif_euus_34-`basis'
    gen Cdif_euus_36_eff=Cdif_euus_36-`basis'
    gen Cdif_euus_40_eff=Cdif_euus_40-`basis'
    gen Cdif_euus_43_eff=Cdif_euus_43-`basis'
    gen Cdif_euus_44_eff=Cdif_euus_44-`basis'
    * gen Cdif_euus_citi_eff=Cdif_euus_citi-`basis'
    * gen Cdif_euus_citi_liboroas_eff=Cdif_euus_citi_liboroas-`basis'

* GBP
gen Cdif_gbus_00=ur00_govtoas -cy00_govtoas 
gen Cdif_gbus_10=ur10_govtoas -cy10_govtoas
gen Cdif_gbus_20=ur20_govtoas -cy20_govtoas 
gen Cdif_gbus_30=ur30_govtoas -cy30_govtoas 
gen Cdif_gbus_34=ur34_govtoas -cy34_govtoas 
gen Cdif_gbus_40=ur40_govtoas -cy40_govtoas 
gen Cdif_gbus_43=ur43_govtoas -cy43_govtoas 

    local basis bpbs10
    gen Cdif_gbus_00_eff=Cdif_gbus_00-`basis'
    gen Cdif_gbus_10_eff=Cdif_gbus_10-`basis'
    gen Cdif_gbus_20_eff=Cdif_gbus_20-`basis'
    gen Cdif_gbus_30_eff=Cdif_gbus_30-`basis'
    gen Cdif_gbus_34_eff=Cdif_gbus_34-`basis'
    gen Cdif_gbus_40_eff=Cdif_gbus_40-`basis'
    gen Cdif_gbus_43_eff=Cdif_gbus_43-`basis'

*JPY
gen Cdif_jpus_00=jc00_govtoas-cy00_govtoas
* gen Cdif_jpus_10=jc10_govtoas-cy10_govtoas
gen Cdif_jpus_20=jc20_govtoas-cy20_govtoas
gen Cdif_jpus_30=jc30_govtoas-cy30_govtoas
gen Cdif_jpus_40=jc40_govtoas-cy40_govtoas

    local basis jybs10
    gen Cdif_jpus_00_eff=Cdif_jpus_00-`basis'
    gen Cdif_jpus_20_eff=Cdif_jpus_20-`basis'
    gen Cdif_jpus_30_eff=Cdif_jpus_30-`basis'
    gen Cdif_jpus_40_eff=Cdif_jpus_40-`basis'

*AUD
gen Cdif_auus_00=auc0_govtoas-cy00_govtoas
gen Cdif_auus_10=ac10_govtoas-cy10_govtoas
gen Cdif_auus_20=ac20_govtoas-cy20_govtoas
gen Cdif_auus_30=ac30_govtoas-cy30_govtoas
gen Cdif_auus_40=ac40_govtoas-cy40_govtoas

    local basis adbs10
    gen Cdif_auus_00_eff=Cdif_auus_00-`basis'
    gen Cdif_auus_10_eff=Cdif_auus_10-`basis'
    gen Cdif_auus_20_eff=Cdif_auus_20-`basis'
    gen Cdif_auus_30_eff=Cdif_auus_30-`basis'
    gen Cdif_auus_40_eff=Cdif_auus_40-`basis'

*CAD
gen Cdif_caus_00=f0c0_govtoas-cy00_govtoas
gen Cdif_caus_20=f0c2_govtoas-cy20_govtoas
gen Cdif_caus_30=f0c3_govtoas-cy30_govtoas
gen Cdif_caus_36=f6c3_govtoas-cy36_govtoas
gen Cdif_caus_40=f0c4_govtoas-cy40_govtoas

    local basis cdbs10
    gen Cdif_caus_00_eff=Cdif_caus_00-`basis'
    gen Cdif_caus_20_eff=Cdif_caus_20-`basis'
    gen Cdif_caus_30_eff=Cdif_caus_30-`basis'
    gen Cdif_caus_36_eff=Cdif_caus_36-`basis'
    gen Cdif_caus_40_eff=Cdif_caus_40-`basis'


** other crosses
gen Cdif_gbeu_00=ur00_govtoas -er00_govtoas 
gen Cdif_gbeu_30=ur30_govtoas -er30_govtoas 
gen Cdif_gbeu_34=ur34_govtoas -er34_govtoas 
gen Cdif_gbeu_40=ur40_govtoas -er40_govtoas 
gen Cdif_gbeu_43=ur43_govtoas -er43_govtoas 

gen Cdif_aueu_00=auc0_govtoas -er00_govtoas 
gen Cdif_aueu_30=ac30_govtoas -er30_govtoas 
gen Cdif_aueu_40=ac40_govtoas -er40_govtoas 

gen Cdif_jpeu_00=jc00_govtoas -er00_govtoas 
gen Cdif_jpeu_30=jc30_govtoas -er30_govtoas 
gen Cdif_jpeu_40=jc40_govtoas -er40_govtoas 

gen Cdif_aujp_00=auc0_govtoas-jc00_govtoas 
gen Cdif_aujp_30=ac30_govtoas-jc30_govtoas 
gen Cdif_aujp_40=ac40_govtoas-jc40_govtoas 


* clear european credits if it's before 2000
replace eubs7=. if year<2002
foreach var of varlist Cdif_euus_*{
    replace `var'=. if year<2002
}

foreach var of varlist cdbs*{
    replace `var'=. if year<2000
}


* generate more basis
* aud + basis vs euribor flat (approximate)
gen aebs1=adbs1-eubs1
gen aebs5=adbs5-eubs5
gen aebs7=adbs7-eubs7
gen aebs10=adbs10-eubs10

* gbp + basis vs euribor flat (approximate)
gen gebs1=bpbs1-eubs1
gen gebs5=bpbs5-eubs5
gen gebs7=bpbs7-eubs7
gen gebs10=bpbs10-eubs10

* jpy + basis vs euribor flat (approximate)
gen jebs1=jybs1-eubs1
gen jebs5=jybs5-eubs5
gen jebs7=jybs7-eubs7
gen jebs10=jybs10-eubs10

* aud + basis vs yen libor flat (approximate)
gen ajbs1=adbs1-jybs1
gen ajbs5=adbs5-jybs5
gen ajbs7=adbs7-jybs7
gen ajbs10=adbs10-jybs10

 foreach var of varlist Cdif_* {
    gen `var'_3m=(`var'+L.`var'+L2.`var')/3
    gen `var'_6m=(`var'+L.`var'+L2.`var'+L3.`var'+L4.`var'+L5.`var')/6
 }


 foreach var of varlist eusa* bpsw* jysw* adsw* ussw*{
    replace `var'=`var'*100
 }

gen ratediff_euus=(eusa10-ussw10)
gen ratediff_gbus=(bpsw10-ussw10)
gen ratediff_jpus=(jysw10-ussw10)
gen ratediff_auus=(adsw10-ussw10)

gen eurchg=log(eur/L6.eur)
gen gbpchg=log(gbp/L6.gbp)
gen jpychg=log(jpy/L6.jpy)
gen audchg=log(aud/L6.aud)

gen eurchg1=log(eur/L1.eur)
gen gbpchg1=log(gbp/L1.gbp)
gen jpychg1=log(jpy/L1.jpy)
gen audchg1=log(aud/L1.aud)

tsset monthly
capture gen date=dofm(monthly)
format date %td
su er30_govtoas if year>2001 & date<d(01oct2015)

save prices_extended.dta, replace


* tsline ajbs5 adbs5

* tsline Cdif_jpus_?0_6m
* tsline Cdif_euus_30*
* tsline Cdif_*us_30
* tsline er43_govtoas cy43_govtoas if year>1999
* tsline Cdif_barc_zvs Cdif_ercy_34 if year>1999
* tsline c4a3_govtoas cy34_govtoas 
* tsline c4a4_govtoas cy44_govtoas
* tsline elc4_govtoas er34_govtoas
