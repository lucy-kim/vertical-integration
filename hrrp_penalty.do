*link hospital-level HRRP readmissions penalty level data across FY 2013-2018 (latest available)
*source: https://www.cms.gov/medicare/medicare-fee-for-service-payment/acuteinpatientpps/readmissions-reduction-program.html

loc gph /ifs/home/kimk13/VI/gph
cd /ifs/home/kimk13/VI/data

*define file names for each FY's readmission penalty data
loc f2013 "Readmissions PUF-FY 2013 IPPS Correction-March 2013.txt"
loc f2014 "FY 2014 Readmissions Supplemental Data Corection - Sept 2013.txt"
loc f2015 "Final FY15-CN Oct 2014.txt"
loc f2016 "FY 2016 IPPS Final Rule Readmissions PUF_readmadj revised 08-04-15.txt"
loc f2017 "FY 2017 IPPS Proposed Rule Readmissions PUF (NPRM FY 2017).txt"
loc f2018 "FY 2018 IPPS Final Rule Readmissions PUF (FY18 data).txt"

forval y=2013/2014 {
    import delimited using "`f`y''", clear
    list in 1/7
    *drop the first few lines containing info
    gen x = real(v2)
    drop if x==. & v1!="PROV"
    drop x

    gen fy = `y'
    rename v1 provid
    rename v2 payadjr
    rename v3 n_pn
    rename v4 err_pn
    rename v5 n_hf
    rename v6 err_hf
    rename v7 n_ami
    rename v8 err_ami
    capture drop v9 v10 v11
    drop if provid=="PROV"
    tempfile clean`y'
    save `clean`y''
}

* starting fy 2015, more conditions added: H/K, COPD
forval y=2015/2016 {
    import delimited using "`f`y''", clear
    gen x = real(v2)
    list in 1/7
    capture replace v1 = "PROV" if v1=="Provider"
    drop if x==. & v1!="PROV"
    drop x
    *drop if _n < 5
    gen fy = `y'
    rename v1 provid
    rename v2 payadjr
    rename v3 n_pn
    rename v4 err_pn
    rename v5 n_hf
    rename v6 err_hf
    rename v7 n_ami
    rename v8 err_ami
    rename v9 n_hk
    rename v10 err_hk
    rename v11 n_copd
    rename v12 err_copd
    capture drop v13 v14 v15 v16
    drop if provid=="PROV"
    tempfile clean`y'
    save `clean`y''
}

* starting fy 2015, more conditions added: CABG
forval y=2017/2018 {
    import delimited using "`f`y''", clear
    gen x = real(v2)
    list in 1/7
    capture replace v1 = "PROV" if v1=="Provider"
    drop if x==. & v1!="PROV"
    drop x
    *drop if _n < 5
    gen fy = `y'
    rename v1 provid
    rename v2 payadjr
    rename v3 n_pn
    rename v4 err_pn
    rename v5 n_hf
    rename v6 err_hf
    rename v7 n_ami
    rename v8 err_ami
    rename v9 n_hk
    rename v10 err_hk
    rename v11 n_copd
    rename v12 err_copd
    rename v13 n_cabg
    rename v14 err_cabg
    drop if provid=="PROV"
    tempfile clean`y'
    save `clean`y''
}

* link across FY
use `clean2013', clear
forval y=2014/2018 {
    append using `clean`y''
}

destring payadjr n* err*, replace

*convert payment adjustment factor to penalty rate ranging 0-3%
gen pnltr = 1 - payadjr
bys fy: sum pnltr

*excess readmission rate
sum err_ami err_hf err_pn err_hk err_copd err_cabg

foreach c in "ami" "hf" "pn" "hk" "copd" "cabg" {
    lab var n_`c' "# `c' cases"
    lab var err_`c' "excess readmit ratio: `c'"
}
lab var payadjr "Readmissions payment Adjustment Factor"
lab var pnltr "Readmissions penalty rate"
lab var provid "Hospital Medicare provider ID"
lab var fy "fiscal year"

compress
save hrrp_penalty, replace
*---------------------------------
*how many hospitals had penalty rate = 3% (max) in 2015?
tab pnltr if fy==2015
*39 hospitals (1% of 2015 hospitals) had 3% penalty rate - minority!

*---------------------------------
*define treatment gp = hospitals penalized in FY 2015 due to ERR for HF > 1 ;
*control group 1 = hospitals penalized in FY 2015 due to ERR for HF <= 1 ;
*control group 1 = hospitals not penalized in FY 2015;

use hrrp_penalty, clear

*ERR should be 0 if ERR < 1
foreach v of varlist err* {
    gen `v'2 = `v'
    replace `v'2 = 0 if `v' < 1
}

*count hospitals that were imposed the maximum penalty rate - we don't know their real penalty rate b/c it's censored
count if pnltr >= 0.01-0.00001 & fy==2013
*276 hosps
count if pnltr>=0.02-0.00001 & fy==2014
*18 hospitals

*how many hospitals had excess readmit ratio > 0 for all 3 conditions?
count if err_ami2 >0 & err_hf2 >0 & err_pn2 >0 & fy==2013
*only 498 / 3500 hospitals (14%) had all 3 conditions contibuting to penalty rate

*how many hospitals had H/K ERR > 0 but ERR for all 3 conditions==0?

*list pnltr* err*2 if fy==2015

*~70 hospitals in 2013, 2014 have penalty rate =0 even though ERR for one of the 3 conditions > 1 - my guess is they have very small share of DRG payments for the condition
assert err_ami2==0 & err_hf2==0 & err_pn2==0 if pnltr==0 & (fy==2013 | fy==2014)
preserve
keep if pnltr==0 & (fy==2013 | fy==2014)
gen good = err_ami2==0 & err_hf2==0 & err_pn2==0
tab fy good
list provid payadjr n* pnltr err*2 fy in 1/10
restore

*---------------------------------
*	Hospitals’ readmission penalty status
use hrrp_penalty, clear

*how many hospitals are there?
tab fy

*# hospitals penalized
gen penalized = pnltr > 0
bys fy: egen sp = sum(penalized)
tab fy, summarize(sp)

*# hospitals penalized due to H/K
capture drop penalized_hk sp2
gen penalized_hk = pnltr > 0 & err_hk > 1 if err_hk!=.
bys fy: egen sp2 = sum(penalized_hk)
tab fy, summarize(sp2)
tab fy, summarize(penalized_hk)

*# hospitals penalized due to H/K only
capture drop penalized_hk sp2
gen penalized_hk = pnltr > 0 & err_hk > 1 & err_ami < 1 & err_hf < 1 & err_pn < 1 & err_copd < 1 if err_hk!=.
bys fy: egen sp2 = sum(penalized_hk)
tab fy, summarize(sp2)
tab fy, summarize(penalized_hk)

*# hospitals penalized due to other conditions but not H/K
capture drop penalized sp2
gen penalized = err_hk < 1 & pnltr > 0 if err_hk!=.
bys fy: egen sp2 = sum(penalized)
tab fy, summarize(sp2)
tab fy, summarize(penalized)

*# hospitals penalized due to H/K and previously penalized (compare w/ 3)
capture drop penalized penalized_hk sp2
gen penalized = pnltr > 0 if fy == 2013
bys provid: egen penalized2013 = max(penalized)
gen penalized_hk = pnltr > 0 & err_hk > 1 & penalized2013==1 if err_hk!=.
bys fy: egen sp2 = sum(penalized_hk)
tab fy, summarize(sp2)
tab fy, summarize(penalized_hk)

*# hospitals not penalized
capture drop penalized penalized_hk sp2
gen notpenalized = pnltr == 0
bys fy: egen sp2 = sum(notpenalized)
tab fy, summarize(sp2)
tab fy, summarize(notpenalized)


*how many hospitals have been censored in each FY due to the cap?
count if pnltr >= 0.01-0.00001 & fy==2013
*276 hosps
count if pnltr>=0.02-0.00001 & fy==2014
*18 hospitals
count if pnltr>=0.03-0.00001 & fy==2015
count if pnltr>=0.03-0.00001 & fy==2016
count if pnltr>=0.03-0.00001 & fy==2017
count if pnltr>=0.03-0.00001 & fy==2018
*---------------------------------
*plot distribution of ERR for each condition-FY
use hrrp_penalty, clear

forval y = 2013/2016 {
    foreach c in "ami" "hf" "pn" "hk" "copd" {
        count if fy==`y'
        loc n = `r(N)'
        count if fy==`y' & err_`c' > 1
        loc penalizedn = `r(N)'

        tw histogram err_`c' if fy==`y', title("`c' Excess Readmission Ratio") subtitle("`y'") note("Note: `n' hospitals in year `y'; `penalizedn' hospitals are penalized") frac xsc(range(0 1.5)) width(0.1)
        graph export `gph'/err_`c'_`y'.eps, replace
    }
}
*---------------------------------
*scatterplot matrix of ERRs for different conditions for each year
use hrrp_penalty, clear

*change var labels in the graph
lab var err_ami "AMI"
lab var err_hf "HF"
lab var err_pn "PN"
lab var err_hk "HK"
lab var err_copd "COPD"

forval y = 2013/2014 {
    graph matrix err_ami err_hf err_pn if fy==`y', half ms(p) title("FY `y' Hospital's Excess Readmission Ratios for Penalty Conditions", size(medium))
    graph export `gph'/matsc_err_`y'.eps, replace
}

forval y = 2015/2016 {
    graph matrix err_ami err_hf err_pn err_hk err_copd if fy==`y', half ms(p) title("FY `y' Hospital's Excess Readmission Ratios for Penalty Conditions", size(medium))
    graph export `gph'/matsc_err_`y'.eps, replace
}
*---------------------------------
* scatterplot of current year's ERR to next year's ERR for each hospital
use hrrp_penalty, clear

keep provid fy err* pnltr
duplicates drop
reshape wide err* pnltr, i(provid) j(fy)

loc f0 = "red"
loc f1 = "black"
forval y = 2013/2016 {
    loc y2 = `y'+1
    foreach c in "ami" {
        *create an indicator for penalized for the condition
        capture drop penalized
        gen penalized = err_`c'`y' > 1

        lab var err_`c'`y2' "`c' Excess readmission ratio in `y2'"
        lab var err_`c'`y' "`c' Excess readmission ratio in `y'"

        tw (sc err_`c'`y2' err_`c'`y' if penalized==1, ti("Change in `c' excess readmission ratios from `y' to `y2'") mcolor(`f0') msymbol(Oh) leg(order(1 "Penalized for `c' in `y'" 2 "Not penalized for `c' in `y'"))) (sc err_`c'`y2' err_`c'`y' if penalized==0, mcolor(`f1') msymbol(Oh) xline(1) yline(1)) || line err_`c'`y' err_`c'`y'
        graph export `gph'/err_`c'_`y'.eps, replace
    }
}

*---------------------------------
* if a hospital has been penalized for 3 initial conditions in FY 2013, what's the H/K ERR for 2015?
* plot a scatterplot of penalty rate in 2013 vs H/K ERR in 2015

/**first get the FY 2013 weights on the 3 conditions used to compute penalty rate using the sample of hospitals whose penalty rate didn't get censored by the cap

use hrrp_penalty, clear

*ERR should be 0 if ERR < 1 & create ERR2 = (ERR - 1)
foreach v of varlist err* {
gen `v'2 = `v'-1
replace `v'2 = 0 if `v'2 < 0
}

*regression using the sample of hospitals whose penalty rate didn't get censored by the cap
preserve
drop if pnltr >= 0.01-0.00001 & fy==2013
*276 hosps

reg pnltr err_ami2 err_hf2 err_pn2 if fy==2013, nocons

*save coefficients on month dummies & other risk adjusters
matrix betas = e(b)
local names: colnames betas
local subset "err_*"
unab subset : `subset'
scalar LLL=wordcount("`subset'")
loc colnum `= colsof(betas)'
restore

*on all hospitals in 2013, get the uncensored penalty rate using the estimated weight for each condition from 2013 data
gen pnltr_uncens = 0

foreach v in "ami" "hf" "pn" {
loc n colnumb(betas, "err_`v'2")
replace pnltr_uncens = pnltr_uncens + betas[1,`n'] * err_`v'2
}

sum pnltr* if fy==2013 & pnltr >= 0.01-0.00001*/

* plot a scatterplot of penalty rate in 2013 vs H/K ERR in 2015
use hrrp_penalty, clear
keep provid fy err* pnltr
duplicates drop
reshape wide err* pnltr, i(provid) j(fy)
tempfile tmp
save `tmp'

loc f0 = "red"
loc f1 = "black"

loc y 2013
loc y2 2015

use `tmp', clear
*standardize variables
keep provid err_hk`y2' pnltr`y'
foreach v of varlist err_hk`y2' pnltr`y' {
    egen z_`v' = std(`v')
}

*create an indicator for penalized in FY 2013
capture drop penalized
gen penalized = pnltr`y' > 0

tw (sc err_hk`y2' pnltr`y' if penalized==1, ti("H/K excess readmission ratio in `y2' vs `y' penalty rate") mcolor(`f0') msymbol(Oh) leg(order(1 "Penalized in `y'" 2 "Not penalized in `y'"))  ) (sc err_hk`y2' pnltr`y' if penalized==0, mcolor(`f1') msymbol(Oh)  ) || line pnltr`y' pnltr`y'
*tw (sc err_hk`y2' pnltr`y' if penalized==1, ti("H/K excess readmission ratio in `y2' vs `y' penalty rate") mcolor(`f0') msymbol(Oh) leg(order(1 "Penalized in `y'" 2 "Not penalized in `y'")) xlabel(-3(1)3) ) (sc err_hk`y2' pnltr`y' if penalized==0, mcolor(`f1') msymbol(Oh) xlabel(-3(1)3) ) || line pnltr`y' pnltr`y'
graph export `gph'/err_hk`y2'_vs_pnltr`y'.eps, replace


forval y = 2013/2016 {
    loc y2 = `y'+1
    foreach c in "ami" {

        lab var err_`c'`y2' "`c' Excess readmission ratio in `y2'"
        lab var err_`c'`y' "`c' Excess readmission ratio in `y'"

        tw (sc err_`c'`y2' err_`c'`y' if penalized==1, ti("Change in `c' excess readmission ratios from `y' to `y2'") mcolor(`f0') msymbol(Oh) leg(order(1 "Penalized for `c'" 2 "Not penalized for `c'"))) (sc err_`c'`y2' err_`c'`y' if penalized==0, mcolor(`f1') msymbol(Oh) xline(1) yline(1)) || line err_`c'`y' err_`c'`y'
        graph export `gph'/err_`c'_`y'.eps, replace
    }
}

*---------------------------------
*for each hospital whose penalty rate didn't get censored due to the cap, regress the penalty rate on (ERR-1)*1(Err > 1) for each condition

use hrrp_penalty, clear

*ERR should be 0 if ERR < 1 & create ERR2 = (ERR - 1)
foreach v of varlist err* {
    gen `v'2 = `v'-1
    replace `v'2 = 0 if `v'2 < 0
}
drop if pnltr >= 0.01-0.00001 & fy==2013
*276 hosps
drop if pnltr>=0.02-0.00001 & fy==2014
*18 hospitals
drop if pnltr>=0.03-0.00001 & fy==2015
drop if pnltr>=0.03-0.00001 & fy==2016
drop if pnltr>=0.03-0.00001 & fy==2017
drop if pnltr>=0.03-0.00001 & fy==2018

list provid fy pnltr err*2 if provid=="010006"

reg pnltr err_ami2 err_hf2 err_pn2 err_hk2 err_copd2 err_cabg2 if provid=="010006", nocons

reg pnltr err_ami2 err_hf2 err_pn2 err_hk2 err_copd2 if provid=="010006" & fy < 2018, nocons



*in eacy year before 2015, regress the penalty indicator on the excess readmit ratios for 3 initial conditions

*reg pnltr err_ami2 err_hf2 err_pn2 if fy==2013 & pnltr < 0.01-0.00001, nocons
reg pnltr err_ami2 err_hf2 err_pn2 if fy==2014 & pnltr < 0.02-0.00001, nocons
*predict pnltr_pred, xb

reg pnltr err_ami2 err_hf2 err_pn2 err_copd2 err_hk2 if fy==2015 & pnltr < 0.03-0.00001, nocons
predict pnltr_pred, xb
sum pnltr_pred pnltr if fy==2015

* coeff on err_hk2 = .0037248
gen hkinfl = err_hk2*.0037248
gen sh_hkinfl = hkinfl/pnltr_pred
sum sh_hkinfl if fy==2015, de
*median = 0, 75th pct = .49, 90th pct = .91, 95th pct = 1

*---------------------------------
*how many hospitals repeatedly had penalty rate > 0 across years
use hrrp_penalty, clear
gen pnlind = pnltr > 0

*ERR should be 0 if ERR < 1
foreach v of varlist err* {
    gen `v'2 = `v'
    replace `v'2 = 0 if `v' < 1
}

keep provid pnlind fy pnltr err*2

reshape wide pnlind pnltr err*2, i(provid) j(fy)
tab pnlind2013
tab pnlind2014 if pnlind2013==1
tab pnlind2014 if pnlind2013==0
tab pnlind2015 if pnlind2013==1 & pnlind2014==1
tab pnlind2015 if pnlind2013==1 & pnlind2014==0
tab pnlind2015 if pnlind2013==0 & pnlind2014==1
tab pnlind2015 if pnlind2013==0 & pnlind2014==0

count if err_hk22015 > 0 & err_ami22015==0 & err_hf22015==0 & err_pn22015==0 & pnlind2013==0 & pnlind2014==0 & pnlind2015==1
*163 Hospitals
count if err_hk22015 > 0 & err_ami22015==0 & err_hf22015==0 & err_pn22015==0 & pnlind2015==1
*283 hospitals
*(283 - 163)/283 = 40% of 283 hospitals had penalty in 2013 or 2014 for the 3 conditions

count if err_hk22015== 0 & (err_ami22015 > 0 | err_hf22015 > 0 | err_pn22015 > 0) & pnlind2015==1
*1260 hospitals

count if err_hk22015== 0 & err_ami22015==0 & err_hf22015==0 & err_pn22015==0 & pnlind2015==0
*776 hospitals

drop if pnlind2015==.
gen pnlind_HK2015 = pnlind2015==1 & err_hk22015 > 0
gen pnlind_onlyHK2015 = pnlind2015==1 & err_hk22015 > 0 & err_ami22015==0 & err_hf22015==0 & err_pn22015==0 & err_copd22015==0
gen pnlind_bothHK2015 = pnlind2015==1 & err_hk22015 > 0 & (err_ami22015 > 0 | err_hf22015 > 0 | err_pn22015 > 0 | err_copd22015 > 0)
gen pnlind_notHK2015 = pnlind2015==1 & err_hk22015== 0
gen pnlind_no2015 = pnlind2015==0
assert pnlind_HK2015 + pnlind_notHK2015 + pnlind_no2015==1
assert pnlind_onlyHK2015 + pnlind_bothHK2015 + pnlind_notHK2015 + pnlind_no2015==1

tab pnlind_HK2015
*1238 have = 1
tab pnlind_onlyHK2015
*216 have = 1
tab pnlind_bothHK2015
*1022 have = 1
tab pnlind_notHK2015
*1400 have = 1
tab pnlind_no2015
*838 have =1

*list err*22015 if pnlind2013==0 & pnlind2014==0 & pnlind2015==1





*---------------------------------
*various ways to measure penalty pressure

*1) 0/1 penalty status for each hospital-fy
*penalty status is

*2) penalty rate

*3) get inpatient revenue to which the penalty rate was multiplied

*4)
