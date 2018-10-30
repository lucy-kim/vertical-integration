*create dynamic penalty pressure: in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011
set maxiter 50

cd `dta'/Medicare

*get 2013-later err's
use `dta'/hrrp_penalty, clear
keep pnltr fy provid err*
tempfile pnltr
save `pnltr'

*for each year t=2012, 2013, ..., link it with penalty rate in t+2
use `pnltr', clear
replace fy = fy - 2
keep pnltr fy provid
tempfile rate
save `rate'


*for each year t=2012, 2013, ..., link it with ERR in t+1
use `pnltr', clear
tab fy
replace fy = fy - 1
keep err* provid fy
merge 1:1 provid fy using `rate', keep(3) nogen
*drop 2011 & 2017

gen penalized_t2 = pnltr > 0
tempfile an
save `an'

*-----------------------
*predict the likelihood of penalty, penalty rate, penalty dollar amount for t+2 using the own performance during {t-3,t-2,t-1}

use `an', clear

*output the logit regression result
loc file logit_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

loc sp2012 err_ami err_hf err_pn
loc sp2013 err_ami err_hf err_pn
loc sp2014 `sp2013' err_hk err_copd
loc sp2015 err_ami err_hf err_pn err_hk err_copd
loc sp2016 `sp2015' err_cabg

gen ppst = .
forval y= 2012/2016 {
  logit penalized_t2 `sp`y'' if fy==`y', vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'
  `out' ctitle(`y') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')
  predict yhat
  replace ppst = yhat if fy==`y'
}


*-------------
*2) predicted penalty rate for t+2 using the own performance during {t-3,t-2,t-1}

*output the GLM regression result
loc file glm_pr_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

gen ppr = .
forval y= 2012/2016 {
  twopm pnltr `sp`y'' if fy==`y', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)
  loc chi: display %9.2f `e(chi2)'
  loc pv: display %9.2f `e(p)'

  `out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

  capture drop yhat
  predict yhat
  replace ppr = yhat if fy==`y'
}

*2014 penalty rate max = 2%
replace ppr = 0.02 if ppr > 0.02 & fy==2012
*2015 onwards penalty rate max = 3%
replace ppr = 0.03 if ppr > 0.03 & fy>=2013
replace ppr = round(ppr,.0001)

lab var ppst "Predicted likelihood of penalty as of the start of each year"
lab var ppr "Predicted penalty rate as of the start of each year"

foreach v of varlist ppr {
  replace `v' = `v'*100
}

keep provid fy ppst ppr

compress
save predict_pnltprs_dynamic, replace


reshape wide pnltr err*, i(provid) j(fy)


keep pnltr201? provid err*
drop if err_pn2013==. & err_ami2013==. & err_hf2013==.
drop if pnltr2013==.

count if err_pn2013!=. & err_ami2013!=. & err_hf2013!=.
*lose about 600 hospitals


gen penalized_t2 = pnltr2013 > 0
gen pnltrate_t2 = pnltr2013
lab var penalized_t2 "Indicator for Penalized in 2013"
lab var pnltrate_t2 "Actual penalty rate in 2013"
