*in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011
set maxiter 50

cd `dta'/Medicare

*get 2012-later err's
use `dta'/hrrp_penalty, clear
reshape wide payadjr pnltr n_* err_* , i(provid) j(fy)
drop err_cabg2016
tempfile penalty
save `penalty'

*get 2011 err's
use `dta'/Medicare/hosp-compare/err2011, clear
*relabel year to 2012 (b/c it's based on 2008-2010)
replace fy = 2012
replace cond = lower(cond)
rename n n_
rename err err_
reshape wide n_* err_* , i(provid fy) j(cond) string
reshape wide n_* err_* , i(provid) j(fy)
tempfile err2011
save `err2011'

*get 2015 err's for CABG only
use `dta'/Medicare/hosp-compare/err2015, clear
replace fy = 2016
keep if cond=="CABG"
replace cond = lower(cond)
rename err err_
reshape wide err_* , i(provid fy) j(cond) string
reshape wide err_* , i(provid) j(fy)
drop denom numer
merge 1:1 provid using `err2011', keep(3) nogen

merge 1:1 provid using `penalty', keep(3) nogen
codebook err_ami*
tempfile penalty
save `penalty'


*let's remerge with penalty rate data after reshape wide the penalty rate
use VI_hospsmpl_agg3c, clear

drop payadjr-pnltr
merge m:1 provid using `penalty', keep(1 3) nogen

*restrict to hospital-conditions whose mean # discharges over 2009-2010 is at least 25
capture drop x
bys provid: egen x = mean(dischnum) if fy < 2011
bys provid: egen mdischnum = max(x)
tab fy if mdischnum > 30

keep if mdischnum >= 30
drop x mdischnum

*drop small hospitals whose min # bed < 30
bys provid: egen x = min(beds)
tab fy if x >= 30
drop if x < 30
drop x
tab fy
*1722 hospitals

tempfile an
save `an'
*-------------------------------------
*get raw readmission rates, ERRs during {t-3,t-2,t-1} for each t = 2011, 2012, ...

/* use `an', clear

capture drop x
foreach v of varlist dischnum read30 {
  capture drop x_`v'
  gen x_`v' = .
  forval t = 2011/2016 {
    loc fy = `t'-3
    loc ly = `t'-1
    bys cond provid: egen x = sum(`v') if fy >= `fy' & fy <= `ly'
    bys cond provid: egen mx = max(x)
    replace x_`v' = mx if fy==`t'
    drop x mx
  }
}
gen rread = x_read30/x_dischnum
sum rread
tab fy, summarize(rread) */

*create one variable for ERR (like reshape long)
use `an', clear
rename err*, upper

foreach c in "AMI" "HF" "PN" "HK" "COPD" "CABG" {
  capture drop err_tl3_tl1_`c'
  gen err_tl3_tl1_`c' = .
  forval t = 2011/2016 {
    loc t2 = `t' + 1
    capture replace err_tl3_tl1_`c' = ERR_`c'`t2' if fy==`t'
  }
}
tab fy, summarize(err_tl3_tl1_AMI)
tab fy, summarize(err_tl3_tl1_HF)
tab fy, summarize(err_tl3_tl1_PN)
tab fy, summarize(err_tl3_tl1_HK)
tab fy, summarize(err_tl3_tl1_COPD)
tab fy, summarize(err_tl3_tl1_CABG)

*create penalty status, penalty rate, penalty dollar amount in t+2 for each t
capture drop penalized_t2
capture drop pnltrate_t2
capture drop pnltdollar_t2
gen penalized_t2 = .
gen pnltrate_t2 = .
gen pnltdollar_t2 = .

*get Medicare payment using t-1, t-2, t-3 values (e.g. for t=2012, get a sum of 2009-2011 Medicare payment)
tab fy, summarize(mcr_pmt)
capture drop x
foreach v of varlist mcr_pmt {
  capture drop x_`v'
  gen sum_`v'_tl3_tl1 = .
  forval t = `int'/2016 {
    loc fy = `t'-3
    loc ly = `t'-1
    bys provid: egen x = sum(`v') if fy >= `fy' & fy <= `ly'
    bys provid: egen mx = max(x)
    replace sum_`v'_tl3_tl1 = mx if fy==`t'
    drop x mx
  }
}
tab fy, summarize(sum_mcr_pmt_tl3_tl1)

*get Medicare payment using t, t-1, t-2 values (e.g. for t=2012, get a sum of 2010-2012 Medicare payment for the actual penalty dollars in 2014)
tab fy, summarize(mcr_pmt)
capture drop x
foreach v of varlist mcr_pmt {
  capture drop x_`v'
  gen sum_`v'_tl2_tl0 = .
  forval t = `int'/2016 {
    loc fy = `t'-2
    loc ly = `t'
    bys provid: egen x = sum(`v') if fy >= `fy' & fy <= `ly'
    bys provid: egen mx = max(x)
    replace sum_`v'_tl2_tl0 = mx if fy==`t'
    drop x mx
  }
}
tab fy, summarize(sum_mcr_pmt_tl2_tl0)

forval t = 2011/2016 {
  loc t2 = `t'+2
  di "pnltr`t2'"
  replace penalized_t2 = pnltr`t2' > 0 if fy==`t'
  replace pnltrate_t2 = pnltr`t2' if fy==`t'
  sum ERR_*`t2' if penalized_t2 ==1
  replace pnltdollar_t2 = pnltrate_t2 * sum_mcr_pmt_tl2_tl0 if fy==`t'
}

sum pnltrate_t2
table fy , content(mean penalized_t2 max pnltrate_t2 max pnltdollar_t2)
table fy , content(mean penalized_t2 max pnltrate_t2 max pnltdollar_t2)
table fy , content(mean penalized_t2 mean pnltrate_t2 mean pnltdollar_t2)

lab var penalized_t2 "Penalized in t+2"
lab var pnltrate_t2 "Actual penalty rate in t+2"
lab var pnltdollar_t2 "Actual penalty amount ($) in t+2"

foreach c in "AMI" "HF" "PN" "HK" "COPD" "CABG"  {
  *lab var rread "`c' Raw readmission rate, {t-3,...,t-1}"
  lab var err_tl3_tl1_`c' "`c' Excess readmission ratio, {t-3,t-2,t-1}"
  replace err_tl3_tl1_`c' = 0 if err_tl3_tl1_`c'==.
}
lab var sum_mcr_pmt_tl2_tl0 "Medicare DRG payment, {t-2,t-1,t}"
lab var sum_mcr_pmt_tl3_tl1 "Medicare DRG payment, {t-3,t-2,t-1}"

tempfile tmp2
save `tmp2'

*-------------
*predict the likelihood of penalty, penalty rate, penalty dollar amount for t+2 using the own performance during {t-3,t-2,t-1}

*1) likelihood of penalty
use `tmp2', clear

*gen pred_pnltstatus_t2_raw = .
gen pred_pnltstatus_t2_err = .
gen pred_pnltstatus_t2_err_c3 = .

*output the logit regression result
loc file logit_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

loc sp2011 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN
loc sp2012 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN
loc sp2013 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN err_tl3_tl1_HK err_tl3_tl1_COPD
loc sp2014 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN err_tl3_tl1_HK err_tl3_tl1_COPD
loc sp2015 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN err_tl3_tl1_HK err_tl3_tl1_COPD err_tl3_tl1_CABG
loc sp2016 err_tl3_tl1_AMI err_tl3_tl1_HF err_tl3_tl1_PN err_tl3_tl1_HK err_tl3_tl1_COPD err_tl3_tl1_CABG

loc int 2011
forval t = `int'/2016 {
  logit penalized_t2 `sp`t'' if fy==`t', vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  predict err
  sum err

  gen x = _b[_cons] + _b[err_tl3_tl1_AMI]*err_tl3_tl1_AMI + _b[err_tl3_tl1_HF]*err_tl3_tl1_HF + _b[err_tl3_tl1_PN]*err_tl3_tl1_PN
  sum x
  gen x2 = exp(x)/(1+exp(x))

  replace pred_pnltstatus_t2_err_c3 = x2 if fy==`t'
  replace pred_pnltstatus_t2_err = err if fy==`t'
  drop err x x2
}

keep provid fy pred_pnltstatus_t2_err pred_pnltstatus_t2_err_c3
*pred_pnltstatus_t2_raw
duplicates drop
tempfile pred_pnltstatus_t2_err
save `pred_pnltstatus_t2_err'

*-------------
*2) predicted penalty rate for t+2 using the own performance during {t-3,t-2,t-1}
use `tmp2', clear

gen pred_pnltrate_t2_err = .
gen pred_pnltrate_t2_err_c3 = .

*output the GLM regression result
loc file glm_pr_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*ERR not available for 2011
forval t = `int'/2016 {
  *glm pnltrate_t2 `sp`t'' if fy==`t', family(binomial) link(logit) vce(robust) nolog
  twopm pnltrate_t2 `sp`t'' if fy==`t', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  predict err
  replace pred_pnltrate_t2_err = err if fy==`t'
  sum err
  drop err

  preserve
  foreach v of varlist err_tl3_tl1_HK err_tl3_tl1_COPD err_tl3_tl1_CABG {
    replace `v'= 0
  }
  predict err
  sum err
  keep provid fy err
  tempfile yhat
  save `yhat'
  restore

  merge 1:1 provid fy using `yhat', nogen

  replace pred_pnltrate_t2_err_c3 = err if fy==`t'
  drop err
}

keep provid fy pred_pnltrate_t2_err pred_pnltrate_t2_err_c3
duplicates drop
tempfile pred_pnltrate_t2_err
save `pred_pnltrate_t2_err'
*-------------
*2) predicted penalty dollar amount
use `tmp2', clear

gen pred_pnltdollar_t2_err = .
gen pred_pnltdollar_t2_err_c3 = .

*output the GLM regression result
loc file glm_pd_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*ERR not available for 2011
forval t = `int'/2016 {
  twopm pnltdollar_t2 `sp`t'' if fy==`t', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t')

  predict err
  replace pred_pnltdollar_t2_err = err if fy==`t'
  sum err
  drop err

  preserve
  foreach v of varlist err_tl3_tl1_HK err_tl3_tl1_COPD err_tl3_tl1_CABG {
    replace `v'= 0
  }
  predict err
  sum err
  keep provid fy err
  tempfile yhat
  save `yhat'
  restore

  merge 1:1 provid fy using `yhat', nogen

  replace pred_pnltdollar_t2_err_c3 = err if fy==`t'
  drop err
}

keep provid fy pred_pnltdollar_t2_err pred_pnltdollar_t2_err_c3
duplicates drop
tempfile pred_pnltdollar_t2_err
save `pred_pnltdollar_t2_err'

*-------------
*merge all predicted data
use `tmp2', clear
foreach df in "pred_pnltdollar_t2_err" "pred_pnltrate_t2_err" "pred_pnltstatus_t2_err" {
  merge 1:1 provid fy using ``df'', keep(1 3) nogen
}

*for pre-years, recode predicted penalty rate to 0
foreach v of varlist pred_pnltdollar_t2_err pred_pnltdollar_t2_err_c3 pred_pnltrate_t2_err pred_pnltrate_t2_err_c3 pred_pnltstatus_t2_err pred_pnltstatus_t2_err_c3 {
  replace `v' = 0 if fy < `int' & `v'==.
}

rename pred_pnltstatus_t2_err ppst
rename pred_pnltrate_t2_err ppr
rename pred_pnltdollar_t2_err ppd
rename pred_pnltdollar_t2_err_c3 ppd_c3
rename pred_pnltstatus_t2_err_c3 ppst_c3
rename pred_pnltrate_t2_err_c3 ppr_c3

foreach v of varlist ppd ppd_c3 {
  gen l`v' = ln(`v' + 1)
}

lab var ppst "Predicted likelihood of penalty"
lab var ppr "Predicted penalty rate"
lab var ppd "Predicted penalty amount ($)"
lab var lppd "Log Predicted penalty amount ($)"
tab fy, summarize(lppd)

lab var ppst_c3 "Predicted likelihood of penalty based on AMI, HF, PN"
lab var ppr_c3 "Predicted penalty rate based on AMI, HF, PN"
lab var ppd_c3 "Predicted penalty amount ($) based on AMI, HF, PN"
lab var lppd_c3 "Log Predicted penalty amount ($) based on AMI, HF, PN"

local lppst "Predicted likelihood of penalty"
local lppr "Predicted penalty rate"
local llppd "Log Predicted penalty amount ($)"
loc ppst_c3 "Predicted likelihood of penalty based on AMI, HF, PN"
loc ppr_c3 "Predicted penalty rate based on AMI, HF, PN"
loc ppd_c3 "Predicted penalty amount ($) based on AMI, HF, PN"
loc lppd_c3 "Log Predicted penalty amount ($) based on AMI, HF, PN"

*cross-sectional variation: create static penalty pressure using the 2011 readmissions penalty pressure based on 2009-2011 performance
foreach v of varlist ppst ppr lppd ppst_c3 ppr_c3 lppd_c3 {
  capture drop x
  capture drop `v'`int'
  gen x = `v' if fy==`int'
  bys provid: egen `v'`int' = max(x)
  lab var `v'`int' "`l`v'' in `int'"
}
tab fy, summarize(lppd`int')
tab fy, summarize(lppd_c3`int')
drop x

/* *use 2012
loc int 2012
foreach v of varlist ppst ppr lppd  {
  capture drop x
  capture drop `v'`int'
  gen x = `v' if fy==`int'
  bys provid: egen `v'`int' = max(x)
  lab var `v'`int' "`l`v'' in `int'"
}
tab fy, summarize(lppd`int')
drop x */

compress
save predict_pnltprs_agg3c, replace
