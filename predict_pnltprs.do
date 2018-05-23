*in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011
set maxiter 50

cd `dta'/Medicare

*get 2013-later err's
use `dta'/hrrp_penalty, clear
keep err_* fy provid
*the ERRs are during t-4,..,t-2 for penalty in t ; since we predict penalty in t+2 using the previous 3 years as of t, it's equivalent of t-3,..., t-1 using to predict penalty rate in t+2 ; so should use ERRs reported for t+1
replace fy = fy + 1
keep if fy < 2017
reshape wide err_* , i(provid) j(fy)
tempfile err14_16
save `err14_16'

*get 2013-later err's
use `dta'/hrrp_penalty, clear
keep pnltr fy provid
reshape wide pnltr, i(provid) j(fy)
tempfile pnltr
save `pnltr'

*get 2011 err's
use `dta'/Medicare/hosp-compare/err2011, clear
*relabel year to 2012 (b/c it's based on 2008-2010)
replace fy = 2013
replace cond = lower(cond)
drop numer denom n
rename err err_
reshape wide err_* , i(provid fy) j(cond) string
reshape wide err_* , i(provid) j(fy)
merge 1:1 provid using `err14_16', keep(3) nogen
codebook err_ami*
tempfile penalty
save `penalty'

*let's remerge with penalty rate data after reshape wide the penalty rate
use VI_hospsmpl_agg3c, clear

drop payadjr-pnltr
merge m:1 provid using `penalty', keep(1 3) nogen
drop if err_pn2013==. & err_ami2013==. & err_hf2013==.

tab fy if err_pn2013!=. & err_ami2013!=. & err_hf2013!=.
*lose about 600 hospitals

*create one variable for ERR (like reshape long)
rename err*, upper
foreach c in "AMI" "HF" "PN" "HK" "COPD" "CABG" {
  capture drop err_tl3_tl1_`c'
  gen err_tl3_tl1_`c' = .
  forval t = 2013/2016 {
    capture replace err_tl3_tl1_`c' = ERR_`c'`t' if fy==`t'
  }
}
tab fy, summarize(err_tl3_tl1_AMI)
tab fy, summarize(err_tl3_tl1_HF)
tab fy, summarize(err_tl3_tl1_PN)
tab fy, summarize(err_tl3_tl1_HK)
tab fy, summarize(err_tl3_tl1_COPD)
drop ERR*

merge 1:1 provid fy using `pnltr', keep(1 3) nogen

tempfile an
save `an'
*-------------------------------------
*create penalty status, penalty rate in 2013 as of 2011

use `an', clear

keep if fy==2013
assert pnltr!=.
gen penalized_t2 = pnltr > 0
gen pnltrate_t2 = pnltr

lab var penalized_t2 "Penalized in t+2"
lab var pnltrate_t2 "Actual penalty rate in t+2"

foreach c in "AMI" "HF" "PN" "HK" "COPD" "CABG"  {
  *lab var rread "`c' Raw readmission rate, {t-3,...,t-1}"
  lab var err_tl3_tl1_`c' "`c' Excess readmission ratio, {t-3,t-2,t-1}"
  replace err_tl3_tl1_`c' = 1 if err_tl3_tl1_`c'==.
}

replace fy = 2011 if fy==2013

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
forval t = `int'/`int' {
  logit penalized_t2 `sp`t'' if fy==`t', vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  predict err
  sum err
  capture drop x
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
forval t = `int'/`int' {
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
*merge all predicted data
use `pred_pnltrate_t2_err', clear
foreach df in "pred_pnltstatus_t2_err" {
  merge 1:1 provid fy using ``df'', keep(1 3) nogen
}
keep provid fy pred_pnltrate_t2_err pred_pnltstatus_t2_err

replace pred_pnltrate = 0.01 if pred_pnltrate > 0.01
replace pred_pnltrate = round(pred_pnltrate,.0001)

drop fy
rename pred_pnltstatus_t2_err ppst11
rename pred_pnltrate_t2_err ppr11

merge 1:1 provid using `pnltr', keep(3) nogen

tempfile ppr
save `ppr'

*for pre-years, recode predicted penalty rate to 0
foreach v of varlist pred_pnltrate_t2_err pred_pnltstatus_t2_err {
  replace `v' = 0 if fy < `int' & `v'==.
}



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
