*in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011

cd `dta'/Medicare

*get 2012-later err's
use `dta'/hrrp_penalty, clear
reshape wide payadjr pnltr n_* err_* , i(provid) j(fy)
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
merge 1:1 provid using `penalty', keep(3) nogen
codebook err_ami*
tempfile penalty
save `penalty'


*let's remerge with penalty rate data after reshape wide the penalty rate
use VI_hospsmpl_agg3c, clear

drop payadjr-pnltr
merge m:1 provid using `penalty', keep(1 3) nogen

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

foreach c in "AMI" "HF" "PN" {
  capture drop err_tl3_tl1_`c'
  gen err_tl3_tl1_`c' = .
  forval t = 2011/2016 {
    loc t2 = `t' + 1
    replace err_tl3_tl1_`c' = ERR_`c'`t2' if fy==`t'
  }
}
tab fy, summarize(err_tl3_tl1_AMI)
tab fy, summarize(err_tl3_tl1_HF)
tab fy, summarize(err_tl3_tl1_PN)
*-------------------------------------
*predict the likelihood of penalty, penalty rate, penalty dollar amount for t+2 using the own performance during {t-3,t-2,t-1}

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

foreach c in "AMI" "HF" "PN" {
  *lab var rread "`c' Raw readmission rate, {t-3,...,t-1}"
  lab var err_tl3_tl1_`c' "`c' Excess readmission ratio, {t-3,t-2,t-1}"
}
lab var sum_mcr_pmt_tl2_tl0 "Medicare DRG payment, {t-2,t-1,t}"
lab var sum_mcr_pmt_tl3_tl1 "Medicare DRG payment, {t-3,t-2,t-1}"

tempfile tmp2
save `tmp2'

*-------------
*1) likelihood of penalty
use `tmp2', clear

/* mmp , mean(pr) smoother(lowess) smooptions(bwidth(.6666667)) predictors linear goptions(xsize(9) ysize(10))
graph export `gph'/test.eps, replace */

/* loc c "AMI"
npregress kernel penalized_t2_`c' rread if cond=="`c'" & fy==2012, reps(200) seed(12) predict(mean deriv)
npgraph, msymbol(circle_hollow) msize(large) lineopts(connect(direct) lw(thick))
graph export `out'/test.eps, replace */

*gen pred_pnltstatus_t2_raw = .
gen pred_pnltstatus_t2_err = .

*output the logit regression result
loc file logit_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*ERR not available for 2011
forval t = `int'/2016 {
  logit penalized_t2 err_tl3_tl1* if fy==`t', vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  predict err
  replace pred_pnltstatus_t2_err = err if fy==`t'
  drop err
}

tempfile tmp3
save `tmp3'

keep provid fy pred_pnltstatus_t2_err
*pred_pnltstatus_t2_raw
duplicates drop
tempfile pred_pnltstatus_t2_err
save `pred_pnltstatus_t2_err'

*graph the actual penalty status and predicted likelihood of penalty against ERRs for each condition & year
*create 100 percentiles of the condition-specific ERR


* COME BACK to this for visualization ---------------------------------------------

loc t `int'
loc c "AMI"
*foreach c in "AMI" "HF" "PN" {
  use `tmp3', clear
  drop if  err_tl3_tl1_`c'==0
  *xtile pctile_`c' = err_tl3_tl1_`c' if fy==`t', nq(20)
  egen pctile_`c' = cut(err_tl3_tl1_`c'), at(0(0.1)1.5)
  foreach n of numlist 25 50 75 {
    bys pctile_`c': egen p`n' = pctile(pred_pnltstatus_t2_err) if fy==`t', p(`n')
  }
  bys pctile_`c': egen mean = mean(pred_pnltstatus_t2_err) if fy==`t'

  tw (scatter penalized_t2 err_tl3_tl1_`c' if fy==`t', msymbol(circle_hollow) xsc(r(0 1.5)) xlab(0(0.5)1.5)) (qfitci pred_pnltstatus_t2_err err_tl3_tl1_`c' if fy==`t') , saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty status in t+2 for `c'" 2 "Median predicted likelihood of penalty in t+2") col(1))
graph export `gph'/test.eps, replace



forval t = `int'/2016 {
foreach c in "AMI" "HF" "PN" {
  use `tmp3', clear
  drop if  err_tl3_tl1_`c'==0
    tw (scatter penalized_t2 err_tl3_tl1_`c' if fy==`t', msymbol(circle_hollow) xsc(r(0 1.5)) xlab(0(0.5)1.5)) || (line pred_pnltstatus_t2_err err_tl3_tl1_`c' if fy==`t', sort), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty status in t+2 for `c'" 2 "Predicted likelihood of penalty in t+2") col(1))
  }
  grc1leg AMI_`t'.gph HF_`t'.gph PN_`t'.gph
  *grc1leg `c'_`int'.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/pred_pnltstatus_t2_err`t'.eps, replace
}

foreach c in "AMI" "HF" "PN" {
  forval t = `int'/2016 {
    tw (scatter penalized_t2 err_tl3_tl1_`c' if fy==`t', msymbol(circle_hollow) xsc(r(0 1.5)) xlab(0(0.5)1.5)) (line pred_pnltstatus_t2_err err_tl3_tl1_`c' if fy==`t', sort), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty status in t+2 for `c'" 2 "Predicted likelihood of penalty in t+2") col(1))
  }
  grc1leg `c'_`int'.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltstatus_t2_err.eps, replace
}



*-------------
*2) predicted penalty rate for t+2 using the own performance during {t-3,t-2,t-1}
use `tmp3', clear

gen pred_pnltrate_t2_err = .

*output the GLM regression result
loc file glm_pr_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*ERR not available for 2011
forval t = `int'/2016 {
  glm pnltrate_t2 err_tl3_tl1_* if fy==`t', family(binomial) link(logit) vce(robust) nolog
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  predict err
  replace pred_pnltrate_t2_err = err if fy==`t'
  drop err
}

/* *graph the actual penalty rate and predicted penalty rate against ERRs for each condition & year
foreach c in "AMI" "HF" "PN" {
  lab var err_tl3_tl1 "Risk-standardized readmission rate"
  sum err_tl3_tl1 if cond=="`c'"
  loc xmax : di %9.1f `r(max)'
  loc xmax : di %9.1f `r(max)'
  sum pred_pnltrate_t2_err if cond=="`c'"
  loc ymax : di %9.1f `r(max)'
  di "x-axis max: `xmax', x-axis max: `xmax', y-axis max: `ymax'"

  forval t = `int'/2016 {
    tw (scatter pnltrate_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', msymbol(circle_hollow) ysc(r(0 `ymax')) ylab(0(0.2)`ymax') xsc(r(`xmax' `xmax')) xlab(`xmax'(0.2)`xmax') ) (line pred_pnltrate_t2_err err_tl3_tl1 if cond=="`c'" & fy==`t', sort ysc(r(0 `ymax')) ylab(0(0.2)`ymax') xsc(r(`xmax' `xmax')) xlab(`xmax'(0.2)`xmax')), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty rate in t+2 for `c'" 2 "Predicted penalty rate in t+2 for `c'") col(1))
  }
  grc1leg `c'_`int'.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltrate_t2_err.eps, replace
} */

keep provid fy pred_pnltrate_t2_err
duplicates drop
tempfile pred_pnltrate_t2_err
save `pred_pnltrate_t2_err'
*-------------
*2) predicted penalty dollar amount
use `tmp2', clear

gen pred_pnltdollar_t2_err = .

*output the GLM regression result
loc file glm_pd_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*ERR not available for 2011
forval t = `int'/2016 {
  twopm pnltdollar_t2 err_tl3_tl1_* if fy==`t', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t')

  predict err
  replace pred_pnltdollar_t2_err = err if fy==`t'
  drop err
}

/* *graph the actual penalty rate and predicted penalty rate against ERRs for each condition & year
replace pnltdollar_t2 = pnltdollar_t2/100000
replace pred_pnltdollar_t2_err = pred_pnltdollar_t2_err/100000

loc incAMI 10
loc incHF 50
loc incPN 5

foreach c in "AMI" "HF" "PN" {
  lab var err_tl3_tl1 "Risk-standardized readmission rate"
  sum err_tl3_tl1 if cond=="`c'"
  loc xmax : di %9.1f `r(max)'
  loc xmax : di %9.1f `r(max)'

  sum pred_pnltdollar_t2_err if cond=="`c'"
  loc ymax : di %9.1f `r(max)'
  di "x-axis max: `xmax', x-axis max: `xmax', y-axis max: `ymax'"

  loc axis ysc(r(0 `ymax')) ylab(0(`inc`c'')`ymax') xsc(r(`xmax' `xmax')) xlab(`xmax'(0.2)`xmax')

  forval t = `int'/2016 {
    tw (scatter pnltdollar_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', msymbol(circle_hollow) `axis') (line pred_pnltdollar_t2_err err_tl3_tl1 if cond=="`c'" & fy==`t', sort `axis'), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty amount ($100,000s) in t+2 for `c'" 2 "Predicted penalty amount ($100,000s) in t+2 for `c'") col(1))
  }
  grc1leg `c'_2012.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltdollar_t2_err.eps, replace
} */

keep provid fy pred_pnltdollar_t2_err
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
foreach v of varlist pred_pnltdollar_t2_err pred_pnltrate_t2_err pred_pnltstatus_t2_err {
  replace `v' = 0 if fy < `int' & `v'==.
}

rename pred_pnltstatus_t2_err ppst
rename pred_pnltrate_t2_err ppr
rename pred_pnltdollar_t2_err ppd

foreach v of varlist ppd {
  gen l`v' = ln(`v' + 1)
}

lab var ppst "Predicted likelihood of penalty"
lab var ppr "Predicted penalty rate"
lab var ppd "Predicted penalty amount ($)"
lab var lppd "Log Predicted penalty amount ($)"
tab fy, summarize(lppd)

local lppst "Predicted likelihood of penalty"
local lppr "Predicted penalty rate"
local llppd "Log Predicted penalty amount ($)"

* 1) cross-sectional variation: use the 2011 readmissions penalty pressure based on 2009-2011 performance
foreach v of varlist ppst ppr lppd  {
  capture drop x
  capture drop `v'`int'
  gen x = `v' if fy==`int'
  bys provid: egen `v'`int' = max(x)
  lab var `v'`int' "`l`v'' in `int'"
}
tab fy, summarize(lppd`int')
drop x

*use 2012
loc int 2012
foreach v of varlist ppst ppr lppd  {
  capture drop x
  capture drop `v'`int'
  gen x = `v' if fy==`int'
  bys provid: egen `v'`int' = max(x)
  lab var `v'`int' "`l`v'' in `int'"
}
tab fy, summarize(lppd`int')
drop x

compress
save predict_pnltprs_agg3c, replace
