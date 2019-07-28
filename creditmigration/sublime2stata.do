
rm reg_dailybasisreg.csv
estimates clear
capture program drop dailyregbasis
dailyregbasis eubs5 USDEUR eur "dailybasisreg"
dailyregbasis bpbs5 USDGBP gbp "dailybasisreg"
dailyregbasis jybs5 USDJPY jpy "dailybasisreg"
dailyregbasis sfbs5 USDCHF chf "dailybasisreg"
dailyregbasis adbs5 USDAUD aud "dailybasisreg"

