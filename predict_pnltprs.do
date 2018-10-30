*in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011
set maxiter 50

cd `dta'/Medicare

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
codebook err_ami*
tempfile err11
save `err11'

*let's remerge with penalty rate data after reshape wide the penalty rate
use `pnltr', clear
keep pnltr2013 provid
merge 1:1 provid using `err11', keep(3) nogen
drop if err_pn2013==. & err_ami2013==. & err_hf2013==.

count if err_pn2013!=. & err_ami2013!=. & err_hf2013!=.
*lose about 600 hospitals

drop if pnltr==.
gen penalized_t2 = pnltr2013 > 0
gen pnltrate_t2 = pnltr2013
lab var penalized_t2 "Indicator for Penalized in 2013"
lab var pnltrate_t2 "Actual penalty rate in 2013"

loc uami "AMI"
loc uhf "HF"
loc upn "PN"

foreach c in "ami" "hf" "pn" {
  *lab var rread "`c' Raw readmission rate, {t-3,...,t-1}"
  lab var err_`c' "`u`c'' Excess readmission ratio, 2008-10"
  replace err_`c' = 1 if err_`c'==.
}

lab var pnltr2013 "Actual penalty rate 2013"
tempfile an
save `an'

*-------------
*predict the likelihood of penalty, penalty rate, penalty dollar amount for t+2 using the own performance during {t-3,t-2,t-1}

*1) likelihood of penalty
use `an', clear

*output the logit regression result
loc file logit_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

loc sp err_ami2013 err_hf2013 err_pn2013

logit penalized_t2 `sp', vce(robust)
loc chi: display %9.2f `e(chi2)'
loc pv: display %9.2f `e(p)'

`out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

predict ppst
sum ppst

*-------------
*2) predicted penalty rate for t+2 using the own performance during {t-3,t-2,t-1}

*output the GLM regression result
loc file glm_pr_err
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

twopm pnltrate_t2 `sp', firstpart(logit) secondpart(glm, family(gamma) link(log)) vce(robust)
loc chi: display %9.2f `e(chi2)'
loc pv: display %9.2f `e(p)'

`out' ctitle(`t') addtext(LR chi-square test statistic, `chi', LR chi-square test p-value, `pv')

predict ppr
sum ppr

replace ppr = 0.01 if ppr > 0.01
replace ppr = round(ppr,.0001)

lab var ppst "Predicted likelihood of penalty as of 2011"
lab var ppr "Predicted penalty rate as of 2011"

merge 1:1 provid using `pnltr', keep(1 3) nogen

drop pnltrate_t2
foreach v of varlist ppr pnltr201?   {
  replace `v' = `v'*100
}

compress
save predict_pnltprs, replace

*-----------------------
*export to R for comparison of the predicted vs actual penalty rate
use predict_pnltprs, clear
keep provid pnltr* penalized_t2 ppst ppr
compress
outsheet using `reg'/predict_pnltprs.csv, comma names replace
