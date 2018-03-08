*in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use VI_hospsmpl, clear

*create size of Medicare payment for each hospital-fy t where t = 2012, 2013, ... using the sum of Medicare payment for the period {t-3, t-2, t-1}
capture drop x
foreach v of varlist mcr_pmt tot_pat_rev {
  capture drop x_`v'
  gen x_`v' = .
  forval t = 2009 /2016 {
    loc fy = `t'-3
    loc ly = `t'-1
    bys cond provid: egen x = sum(`v') if fy >= `fy' & fy <= `ly'
    bys cond provid: egen mx = max(x)
    replace x_`v' = mx if fy==`t'
    drop x mx
  }
  assert x_`v'!=. if fy >= 2011
}
rename x_mcr_pmt deptsize
rename x_tot_pat_rev totpatrev


*let's remerge with penalty rate data after reshape wide the penalty rate
preserve
use `dta'/hrrp_penalty, clear
reshape wide payadjr pnltr n_* err_* , i(provid) j(fy)
tempfile penalty
save `penalty'
restore

drop payadjr-pnltr
merge m:1 provid using `penalty', keep(1 3) nogen

tempfile an
save `an'
*-------------------------------------
*get raw readmission rates, ERRs during {t-3,t-2,t-1} for each t = 2011, 2012, ...

use `an', clear
drop if cond=="HK"

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
tab fy, summarize(rread)

*create one variable for ERR
rename err*, upper
capture drop x
gen x = .
foreach c in "AMI" "HF" "PN" {
  forval t = 2012/2016 {
    loc t2 = `t' + 1
    replace x = ERR_`c'`t2' if fy==`t' & cond=="`c'"
  }
}
rename x err_tl3_tl1
tab fy, summarize(err_tl3_tl1)

*-------------------------------------
*predict the likelihood of penalty, penalty rate, penalty dollar amount for t+2 using the own performance during {t-3,t-2,t-1}

*create penalty status, penalty rate, penalty dollar amount in t+2 for each t
capture drop penalized_t2
capture drop pnltrate_t2
capture drop pnltdollar_t2
gen penalized_t2 = .
gen pnltrate_t2 = .
gen pnltdollar_t2 = .

foreach c in "AMI" "HF" "PN" {
  forval t = 2011/2016 {
      loc t2 = `t'+2
      di "pnltr`t2'"
      replace penalized_t2 = pnltr`t2' > 0 & ERR_`c'`t2' > 1 if fy==`t' & cond=="`c'"
      replace pnltrate_t2 = (ERR_`c'`t2'-1) * penalized_t2 if fy==`t' & cond=="`c'"
      sum ERR_`c'`t2' if penalized_t2 ==1 & cond=="`c'"
      replace pnltdollar_t2 = pnltrate_t2 * deptsize if fy==`t' & cond=="`c'"
  }
}
table fy if cond=="AMI", content(mean penalized_t2 min pnltrate_t2 min pnltdollar_t2)
table fy if cond=="AMI", content(mean penalized_t2 mean pnltrate_t2 mean pnltdollar_t2)


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

gen pred_pnltstatus_t2_raw = .
gen pred_pnltstatus_t2_err = .

foreach c in "AMI" "HF" "PN" {
  lab var penalized_t2 "Penalized in t+2 for `c'"
  lab var rread "`c' Raw readmission rate, {t-3,...,t-1}"
  lab var err_tl3_tl1 "`c' Excess readmission ratio, {t-3,...,t-1}"

  *output the logit regression result
  loc file logit_`c'_raw
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex append label"

  forval t = 2011/2016 {
    logit penalized_t2 rread if cond=="`c'" & fy==`t'
    loc chi: display %9.2f `e(chi2)'
    loc pv: display %9.2f `e(p)'

    `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')
    predict raw
    replace pred_pnltstatus_t2_raw = raw if cond=="`c'" & fy==`t'
    drop raw
  }

  *output the logit regression result
  loc file logit_`c'_err
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex append label"

  *ERR not available for 2011
  forval t = 2012/2016 {
    logit penalized_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', vce(robust)
    loc chi: display %9.2f `e(chi2)'
    loc pv: display %9.2f `e(p)'

    `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

    predict err
    replace pred_pnltstatus_t2_err = err if cond=="`c'" & fy==`t'
    drop err
  }
}

*graph the actual penalty status and predicted likelihood of penalty against ERRs for each condition & year
foreach c in "AMI" "HF" "PN" {
  lab var err_tl3_tl1 "Risk-standardized readmission rate"

  forval t = 2012/2016 {
    tw (scatter penalized_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', msymbol(circle_hollow)) (line pred_pnltstatus_t2_err err_tl3_tl1 if cond=="`c'" & fy==`t', sort), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty status in t+2 for `c'" 2 "Predicted likelihood of penalty in t+2 for `c'") col(1))
  }
  grc1leg `c'_2012.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltstatus_t2_err.eps, replace
}

keep provid cond fy pred_pnltstatus_t2_err pred_pnltstatus_t2_raw
duplicates drop
tempfile pred_pnltstatus_t2_err
save `pred_pnltstatus_t2_err'

*-------------
*2) predicted penalty rate for t+2 using the own performance during {t-3,t-2,t-1}
use `tmp2', clear

gen pred_pnltrate_t2_err = .

foreach c in "AMI" "HF" "PN" {
  lab var pnltrate_t2 "Penalty rate in $t+2$ for `c'"
  lab var rread "`c' Raw readmission rate, $\{t-3,...,t-1}$"
  lab var err_tl3_tl1 "`c' Excess readmission ratio, $\{t-3,...,t-1}$"

  *output the GLM regression result
  loc file glm_pr_`c'_err
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex append label"

  *ERR not available for 2011
  forval t = 2012/2016 {
    glm pnltrate_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', family(binomial) link(logit) vce(robust) nolog
    loc chi: display %9.2f `e(chi2)'
    loc pv: display %9.2f `e(p)'

    `out' ctitle(`t')

    predict err
    replace pred_pnltrate_t2_err = err if cond=="`c'" & fy==`t'
    drop err
  }
}

*graph the actual penalty rate and predicted penalty rate against ERRs for each condition & year
foreach c in "AMI" "HF" "PN" {
  lab var err_tl3_tl1 "Risk-standardized readmission rate"
  sum err_tl3_tl1 if cond=="`c'"
  loc xmax : di %9.1f `r(max)'
  loc xmin : di %9.1f `r(min)'
  sum pred_pnltrate_t2_err if cond=="`c'"
  loc ymax : di %9.1f `r(max)'
  di "x-axis min: `xmin', x-axis max: `xmax', y-axis max: `ymax'"

  forval t = 2012/2016 {
    tw (scatter pnltrate_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', msymbol(circle_hollow) ysc(r(0 `ymax')) ylab(0(0.2)`ymax') xsc(r(`xmin' `xmax')) xlab(`xmin'(0.2)`xmax') ) (line pred_pnltrate_t2_err err_tl3_tl1 if cond=="`c'" & fy==`t', sort ysc(r(0 `ymax')) ylab(0(0.2)`ymax') xsc(r(`xmin' `xmax')) xlab(`xmin'(0.2)`xmax')), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty rate in t+2 for `c'" 2 "Predicted penalty rate in t+2 for `c'") col(1))
  }
  grc1leg `c'_2012.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltrate_t2_err.eps, replace
}

keep provid cond fy pred_pnltrate_t2_err
duplicates drop
tempfile pred_pnltrate_t2_err
save `pred_pnltrate_t2_err'
*-------------
*2) predicted penalty dollar amount
use `tmp2', clear

gen pred_pnltdollar_t2_err = .

foreach c in "AMI" "HF" "PN" {
  lab var pnltdollar_t2 "Penalty amount (\$) in $t+2$ for `c'"
  lab var rread "`c' Raw readmission rate, $\{t-3,...,t-1}$"
  lab var err_tl3_tl1 "`c' Excess readmission ratio, $\{t-3,...,t-1}$"

  *output the GLM regression result
  loc file glm_pd_`c'_err
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex append label"

  *ERR not available for 2011
  forval t = 2012/2016 {
    twopm pnltdollar_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)

    `out' ctitle(`t')

    predict err
    replace pred_pnltdollar_t2_err = err if cond=="`c'" & fy==`t'
    drop err
  }
}

*graph the actual penalty rate and predicted penalty rate against ERRs for each condition & year
replace pnltdollar_t2 = pnltdollar_t2/100000
replace pred_pnltdollar_t2_err = pred_pnltdollar_t2_err/100000

loc incAMI 10
loc incHF 50
loc incPN 5

foreach c in "AMI" "HF" "PN" {
  lab var err_tl3_tl1 "Risk-standardized readmission rate"
  sum err_tl3_tl1 if cond=="`c'"
  loc xmax : di %9.1f `r(max)'
  loc xmin : di %9.1f `r(min)'

  sum pred_pnltdollar_t2_err if cond=="`c'"
  loc ymax : di %9.1f `r(max)'
  di "x-axis min: `xmin', x-axis max: `xmax', y-axis max: `ymax'"

  loc axis ysc(r(0 `ymax')) ylab(0(`inc`c'')`ymax') xsc(r(`xmin' `xmax')) xlab(`xmin'(0.2)`xmax')

  forval t = 2012/2016 {
    tw (scatter pnltdollar_t2 err_tl3_tl1 if cond=="`c'" & fy==`t', msymbol(circle_hollow) `axis') (line pred_pnltdollar_t2_err err_tl3_tl1 if cond=="`c'" & fy==`t', sort `axis'), saving(`c'_`t', replace) subti(`t' `c') leg(order(1 "Actual penalty amount ($100,000s) in t+2 for `c'" 2 "Predicted penalty amount ($100,000s) in t+2 for `c'") col(1))
  }
  grc1leg `c'_2012.gph `c'_2013.gph `c'_2014.gph `c'_2015.gph `c'_2016.gph,
  graph export `gph'/`c'_pred_pnltdollar_t2_err.eps, replace
}

keep provid cond fy pred_pnltdollar_t2_err
duplicates drop
tempfile pred_pnltdollar_t2_err
save `pred_pnltdollar_t2_err'

*-------------
*merge all predicted data
use `tmp2', clear
foreach df in "pred_pnltdollar_t2_err" "pred_pnltrate_t2_err" "pred_pnltstatus_t2_err" {
  merge 1:1 provid cond fy using ``df'', keep(1 3) nogen
}

compress
save predict_pnltprs, replace
