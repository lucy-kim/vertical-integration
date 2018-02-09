*regression model: run a diff-in-diff analysis of the impact of HRRP penalty on referral concentration

ssc install parmest

set matsize 11000
loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls

cd /ifs/home/kimk13/VI/data/Medicare

*sample exclusion

*restrict to FY 2011-2016
use hosp_fy_VI, clear
keep if fy > 2010

*restrict to hospitals that have

*restrict to hospitals that have greater than 25 cases for all conditions in FY 2013
foreach c in "ami" "hf" "pn" {
  gen x = n_`c' if fy==2013
  bys provid: egen xx = max(x)
  drop if xx < 25
  drop x xx
}
foreach c in "hk"{
  gen x = n_`c' if fy==2015
  bys provid: egen xx = max(x)
  drop if xx < 25
  drop x xx
}

tab fy if cond=="AMI" & pac=="SNF"

/* capture drop smallhk
gen smallhk = 1 if n_hk < 50 & fy==2015
loc v smallhk
bys provid: egen mm = max(smallhk)
drop if mm==1
drop mm smallhk */

*reconstruct size category b/c missing values in bed is filled with previously available values
*create hospital size category
drop size
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 500
replace size = 3 if beds > 500
assert size!=.
replace size = . if beds==.

xi i.fy i.size

tempfile an
save `an'

*-------------------
*for each hospital,


*-------------------
*reg model: VI_ht = a_h + b_t + c1*Post*[Penalized for H/K in 2015] + c2*Post*[Not penalized for H/K in 2015] + d*X_ht + e_ht

use `an', clear
*divide hospitals into 3 groups based on the 2015 penalty status: 1) being penalized for H/K in 2015, 2) being penalized for non-H/K conditions in 2015, 3) not penalized
capture drop pnlt_hk15 pnlt_nohk15
gen pnlt_hk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1
gen pnlt_nohk15 = 1 if pnltr > 0 & fy==2015 & err_hk <= 1
assert pnltr==0 if fy==2015 & pnlt_hk15==. & pnlt_nohk15==.

foreach v of varlist pnlt_hk15 pnlt_nohk15 {
    gsort provid -`v'
    bys provid: replace `v' = `v'[_n-1] if `v' >= .
    replace `v' = 0 if `v'==.
}
assert pnlt_nohk15 + pnlt_hk15==1 | pnlt_nohk15 + pnlt_hk15==0
tab pnlt_hk15 pnlt_nohk15 if fy==2015 & cond=="AMI" & pac=="HHA"

*create interaction terms: penalty status indicator X FY
foreach v of varlist pnlt_hk15 pnlt_nohk15 {
    forval y=2012/2015 {
        gen `v'_X_`y' = `v' * _Ify_`y'
    }
}

loc c "HK"
foreach p in "HHA" "SNF" {
    hist refhhi if pac=="`p'" & cond=="`c'", title("`p' referral concentration") subtitle("condition `c'") frac
    graph export `gph'/`p'_HK_refhhi.eps, replace
}

loc c "HK"
foreach p in "HHA" "SNF" {
    di "`p' condition: `c'"
    loc x_ht own_fp own_np own_gv _Isize* ses_score teaching
    *hrrhhi_`p'_`c'
    areg refhhi pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="`p'" & cond=="`c'", absorb(provid) cluster(provid)
    *pnlt_hk15 pnlt_nohk15 drop out due to collinearity; they are time-constant for each hospital
}

areg refhhi pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="SNF" & cond=="HK", absorb(provid) cluster(provid)
areg refhhi pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="SNF" & cond=="AMI", absorb(provid) cluster(provid)
areg refhhi pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="SNF" & cond=="HF", absorb(provid) cluster(provid)
areg refhhi pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="SNF" & cond=="PN", absorb(provid) cluster(provid)
*-------------------

*reg model: VI_ht = a_h + b_t + c0*Post*[Penalized only for H/K in 2015] + c1*Post*[Penalized for H/K in 2015] + c2*Post*[Penalized for non-H/K in 2015] + d*X_ht + e_ht

use `an', clear

*divide hospitals into 3 groups: 1) being penalized ONLY for H/K in 2015, 2) being penalized for H/K & others in 2015; 3) being not penalized for H/K but penalized for others in 2015; and omitted group = not penalized in 2015
capture drop pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15
gen pnlt_onlyhk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1 & err_ami <= 1 & err_hf <= 1 & err_pn <= 1
gen pnlt_hk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1 & (err_ami > 1 | err_hf > 1 | err_pn > 1)
gen pnlt_nohk15 = 1 if pnltr > 0 & fy==2015 & err_hk <= 1
assert pnltr==0 if fy==2015 & pnlt_hk15==. & pnlt_nohk15==. & pnlt_onlyhk15==.

foreach v of varlist pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15 {
    gsort provid -`v'
    bys provid: replace `v' = `v'[_n-1] if `v' >= .
    replace `v' = 0 if `v'==.
}
assert pnlt_nohk15 + pnlt_hk15 + pnlt_onlyhk15 ==1 | pnlt_nohk15 + pnlt_hk15 + pnlt_onlyhk15==0

*create interaction terms: penalty status indicator X FY
foreach v of varlist pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15 {
    forval y=2012/2015 {
        capture drop `v'_X_`y'
        gen `v'_X_`y' = `v' * _Ify_`y'
    }
}
tempfile tmp
save `tmp'

*-------------------
*in 2015, # hospitals penalized only for H/K, for both H/K & others, for only non-H/K, and not penalized
preserve
keep if fy==2015 & cond=="HK" & pac=="HHA"
count
*2145 hospitals
count if pnlt_onlyhk15==1
*224
count if pnlt_hk15==1
*814
count if pnlt_nohk15==1
*737
count if pnltr==0
*370
restore
*-------------------
*regression using interaction of penalty status & FY dummies to plot DiD coefficients

use `tmp', clear

loc x_ht own_fp own_np own_gv _Isize* ses_score teaching err*lead2


foreach p in "HHA" "SNF" {
    use `tmp', clear
    areg refhhi pnlt_onlyhk15_X* pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify* if pac=="`p'" & cond=="HK", absorb(provid) cluster(provid)
    *pnlt_hk15 pnlt_nohk15 drop out due to collinearity; they are time-constant for each hospital

    *save coefficients to plot
    tempfile coef_`p'
    parmest, saving(`coef_`p'')

    use `coef_`p'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "pnlt")
    split parm, p("_X_")
    keep estimate parm1 parm2 min95 max95
    rename estimate coef
    rename parm1 gp
    rename parm2 fy
    sort gp fy
    compress
    outsheet using `reg'/coef_VI_`p'.csv, replace comma names
}

*by size category


preserve
keep provid fy pnlt_onlyhk15 pnlt_hk15 pnlt_nohk15
duplicates drop
count if fy==2013 & pnlt_onlyhk15==1
*269
count if fy==2013 & pnlt_hk15==1
*944
count if fy==2013 & pnlt_nohk15==1
*986
restore

*--------------------------------
*create post indicator = 1 for FY >= 2013
use `tmp', clear
gen post = fy >= 2013

*create interaction terms: penalty status indicator X FY
foreach v of varlist pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15 {
    gen `v'_X_post = `v' * post
}

*tag balanced sample
preserve
keep if cond=="HK"
keep pac provid fy
duplicates drop
bys pac provid: egen min = min(fy)
bys pac provid: egen max = max(fy)
bys pac provid: gen s = _N
gen bal = s==5 & min==2011 & max==2015
keep pac provid bal
duplicates drop
tempfile bal
save `bal'
restore

merge m:1 pac provid using `bal', nogen

lab var refhhi "PAC Referral concentration"
lab var pnlt_onlyhk15_X_post "Penalized only for H/K X Post"
lab var pnlt_hk15_X_post "Penalized for both H/K and other X Post"
lab var pnlt_nohk15_X_post "Penalized only for other X Post"

tempfile tmp2
save `tmp2'

loc x_ht own_fp own_np own_gv _Isize* ses_score teaching err_ami_lead2 err_hf_lead2 err_pn_lead2
areg refhhi pnlt_onlyhk15_X_post pnlt_hk15_X_post pnlt_nohk15_X_post _Ify* `x_ht' urban if pac=="HHA" & cond=="HK", absorb(provid) cluster(provid)

hist refhhi if pac=="HHA" & cond=="HK"
graph export `gph'/test.eps, replace
sum n_hk if pac=="HHA" & cond=="HK" & refhhi==1

foreach p in "HHA" "SNF" {
    use `tmp2', clear

    loc x_ht0
    loc x_ht1 own_fp own_np own_gv _Isize* ses_score teaching
    loc x_ht2 own_fp own_np own_gv _Isize* ses_score teaching hrrhhi_`p'_HK
    loc x_ht3 own_fp own_np own_gv _Isize* ses_score teaching hrrhhi_`p'_HK err_ami_lead2 err_hf_lead2 err_pn_lead2

    loc file pnlty_vi1_`p'
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    loc out "outreg2 using `reg'/`file'.xls, append label"

    forval ss=0/3 {
        areg refhhi pnlt_onlyhk15_X_post pnlt_hk15_X_post pnlt_nohk15_X_post `x_ht`ss'' _Ify* if pac=="`p'" & cond=="HK", absorb(provid) cluster(provid)

        *mean dep var
        sum refhhi if e(sample)
        loc mdv: display %9.2f `r(mean)'

        loc sp0 addtext(Mean dep. var., `mdv')
        loc sp1 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y)
        loc sp2 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y)
        loc sp3 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y, ERR for 3 initial conditions, Y)

        `out' ctitle("All hospitals") `sp`ss''
    }
    forval ss=0/3 {
        areg refhhi pnlt_onlyhk15_X_post pnlt_hk15_X_post pnlt_nohk15_X_post `x_ht`ss'' _Ify* if pac=="`p'" & cond=="HK" & bal==1, absorb(provid) cluster(provid)

        *mean dep var
        sum refhhi if e(sample)
        loc mdv: display %9.2f `r(mean)'

        loc sp0 addtext(Mean dep. var., `mdv')
        loc sp1 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y)
        loc sp2 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y)
        loc sp3 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y, ERR for 3 initial conditions, Y)

        `out' ctitle("Balanced panel") `sp`ss''
    }
}

*by bed size category
loc size1 "Small # beds"
loc size2 "Medium # beds"
loc size3 "Large # beds"

use `tmp2', clear
sum beds if pac=="HHA" & cond=="HK", de

foreach p in "HHA" "SNF" {
    use `tmp2', clear

    xtile size3 = beds if pac=="`p'" & cond=="HK" & fy==2011, nq(3)
    tabstat beds, by(size3) stat(min max)
    bys provid: egen size3x = max(size3)
    drop size3
    rename size3x size3

    loc x_ht0
    loc x_ht1 own_fp own_np own_gv ses_score teaching
    loc x_ht2 own_fp own_np own_gv ses_score teaching hrrhhi_`p'_HK
    loc x_ht3 own_fp own_np own_gv ses_score teaching hrrhhi_`p'_HK err_ami_lead2 err_hf_lead2 err_pn_lead2

    loc file pnlty_vi1_`p'_bysize
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    loc out "outreg2 using `reg'/`file'.xls, append label"

    loc ss 2

    forval c=1/3 {
        areg refhhi pnlt_onlyhk15_X_post pnlt_hk15_X_post pnlt_nohk15_X_post `x_ht`ss'' _Ify* if pac=="`p'" & cond=="HK" & size3==`c', absorb(provid) cluster(provid)

        *mean dep var
        sum refhhi if e(sample)
        loc mdv: display %9.2f `r(mean)'

        loc sp0 addtext(Mean dep. var., `mdv')
        loc sp1 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y)
        loc sp2 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y)
        *loc sp3 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y, ERR for 3 initial conditions, Y)

        `out' ctitle("`size`c''") `sp`ss''
    }
}

*by H/K referral size category
loc size1 "Small # HK referrals"
loc size2 "Medium # HK referrals"
loc size3 "Large # HK referrals"

use `tmp2', clear
sum beds if pac=="HHA" & cond=="HK", de
xtile size3 = dischnum_pac if pac=="HHA" & cond=="HK" & fy==2011, nq(3)
tabstat dischnum_pac, by(size3) stat(min max)

foreach p in "HHA" "SNF" {
    use `tmp2', clear

    xtile size3 = dischnum_pac if pac=="`p'" & cond=="HK" & fy==2011, nq(3)
    tabstat dischnum_pac, by(size3) stat(min max)
    bys provid: egen size3x = max(size3)
    drop size3
    rename size3x size3

    loc x_ht0
    loc x_ht1 own_fp own_np own_gv _Isize* ses_score teaching
    loc x_ht2 own_fp own_np own_gv _Isize* ses_score teaching hrrhhi_`p'_HK
    loc x_ht3 own_fp own_np own_gv _Isize* ses_score teaching hrrhhi_`p'_HK err_ami_lead2 err_hf_lead2 err_pn_lead2

    loc file pnlty_vi1_`p'_bysize
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    loc out "outreg2 using `reg'/`file'.xls, append label"

    loc ss 2

    forval c=1/3 {
        areg refhhi pnlt_onlyhk15_X_post pnlt_hk15_X_post pnlt_nohk15_X_post `x_ht`ss'' _Ify* if pac=="`p'" & cond=="HK" & size3==`c', absorb(provid) cluster(provid)

        *mean dep var
        sum refhhi if e(sample)
        loc mdv: display %9.2f `r(mean)'

        loc sp0 addtext(Mean dep. var., `mdv')
        loc sp1 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y)
        loc sp2 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y)
        *loc sp3 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y, ERR for 3 initial conditions, Y)

        `out' ctitle("`size`c''") `sp`ss''
    }
}

*---------------------------------
*group hospitals: 1) penalized for HK and previously not penalized; 2) penalized for HK and previously penalized; 3) penalized for non-HK only
use `tmp2', clear

*for each hospital, sum the penalty rate over 2013-2014 to calculate the cumulative impact of previous penalty
preserve
use /ifs/home/kimk13/VI/data/hrrp_penalty, clear
keep if fy < 2015
keep provid pnltr fy
collapse (sum) pnltr_prior = pnltr, by(provid)
destring provid, replace
tempfile pnltr_prior
save `pnltr_prior'
restore

merge m:1 provid using `pnltr_prior', keep(1 3) nogen

capture drop pnlt_hk15
gen pnlt_hk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1

foreach v of varlist pnlt_hk15 {
    gsort provid -`v'
    bys provid: replace `v' = `v'[_n-1] if `v' >= .
    replace `v' = 0 if `v'==.
}
assert pnlt_hk15 + pnlt_nohk15==1 if pnltr > 0 & fy==2015

*create interaction terms: penalty status indicator X FY
foreach v of varlist pnlt_hk15 pnlt_nohk15 {
    capture drop `v'_X_post
    gen `v'_X_post = `v' * post
    gen `v'_X_pnltr_prior_X_post = `v' * pnltr_prior * post
}

gen pnltr_prior_X_post = pnltr_prior * post

lab var refhhi "PAC Referral concentration"
lab var pnlt_hk15_X_post "Penalized for H/K X Post"
lab var pnlt_nohk15_X_post "Penalized only for other X Post"
lab var pnlt_hk15_X_pnltr_prior_X_post "Penalized for H/K X Initial penalty rate X Post"
lab var pnlt_nohk15_X_pnltr_prior_X_post "Penalized only for other X Initial penalty rate X Post"
lab var pnltr_prior_X_post "Initial penalty rate X Post"

tempfile tmp3
save `tmp3'


*create interaction terms: continuous penalty rate
foreach p in "HHA" "SNF" {
    use `tmp3', clear

    loc x_ht0
    loc x_ht1 own_fp own_np own_gv _Isize* ses_score teaching
    loc x_ht2 own_fp own_np own_gv _Isize* ses_score teaching hrrhhi_`p'_HK

    loc sp0 addtext(Mean dep. var., `mdv')
    loc sp1 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y)
    loc sp2 addtext(Mean dep. var., `mdv', Hospital characteristics controls, Y, `p' market HHI, Y)

    loc file pnlty_vi2_`p'
    capture erase `reg'/`file'.xls
    capture erase `reg'/`file'.txt
    loc out "outreg2 using `reg'/`file'.xls, append label"

    forval ss=0/2 {
        areg refhhi pnlt_hk15_X_post pnlt_nohk15_X_post pnlt_hk15_X_pnltr_prior_X_post pnlt_nohk15_X_pnltr_prior_X_post pnltr_prior_X_post `x_ht`ss'' _Ify* if pac=="`p'" & cond=="HK", absorb(provid) cluster(provid)

        *mean dep var
        sum refhhi if e(sample)
        loc mdv: display %9.2f `r(mean)'

        `out' `sp`ss''
    }
}
*--------------------------------
*interact the continuous penalty pressure X post
use `tmp2', clear
gen err_hk15 = err_hk if fy==2015
sum err_hk15 if pnlt_nohk15==1

sc refhhi err_hk15 if pac=="HHA" & cond=="HK" & fy==2015
graph export `gph'/test.eps, replace

*compute
hist err_hk15 if pac=="HHA" & cond=="HK" & fy==2015
graph export `gph'/test.eps, replace

gen err_hk15 > 1

*divide hospitals into 3 groups: 1) being penalized ONLY for H/K in 2015, 2) being penalized for H/K & others in 2015; 3) being not penalized for H/K but penalized for others in 2015; and omitted group = not penalized in 2015
capture drop pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15
gen pnlt_onlyhk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1 & err_ami <= 1 & err_hf <= 1 & err_pn <= 1
gen pnlt_hk15 = 1 if pnltr > 0 & fy==2015 & err_hk > 1 & (err_ami > 1 | err_hf > 1 | err_pn > 1)
gen pnlt_nohk15 = 1 if pnltr > 0 & fy==2015 & err_hk <= 1
assert pnltr==0 if fy==2015 & pnlt_hk15==. & pnlt_nohk15==. & pnlt_onlyhk15==.

foreach v of varlist pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15 {
    gsort provid -`v'
    bys provid: replace `v' = `v'[_n-1] if `v' >= .
    replace `v' = 0 if `v'==.
}
assert pnlt_nohk15 + pnlt_hk15 + pnlt_onlyhk15 ==1 | pnlt_nohk15 + pnlt_hk15 + pnlt_onlyhk15==0


*create interaction terms: penalty status indicator X FY
foreach v of varlist pnlt_hk15 pnlt_nohk15 pnlt_onlyhk15 {
    gen `v'_X_post = `v' * post
}


areg refhhi pnlt_onlyhk15_X* pnlt_hk15_X* pnlt_nohk15_X* `x_ht' _Ify_* if pac=="SNF" & cond=="HK", absorb(provid) cluster(provid)

coefplot, drop(_cons) eform yline(0) vertical bycoefs



*control for FY 2013 excess readmission ratio for 3 initial conditions
foreach v of varlist err_ami err_hf err_pn {
    gen `v'13 = `v' if fy==2013
    gsort provid -`v'13
    bys provid: replace `v'13 = `v'13[_n-1] if `v'13 >= .
}
