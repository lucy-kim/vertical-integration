*reshape coefficient estimate output into a formatted table
loc gph "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"

cd `dta'/Medicare

loc nn 1
loc n2 =`nn'*1
loc k 11

loc fname pbite
*pbite_noSNF pbite_actual pnltr2013 sppshort

use ivpenalty_VI_agg3c_nosw, clear

loc ii = 0
*shrefAMI shrefHF shrefPN qual_samh qual_star qual_def
*foreach v of varlist shref comorbidsum vi_snf refhhi shref_bytopSNF lnreqSNF_80pct qual_read read30_pac read30_other {
foreach v of varlist shref_bytopSNF vi_snf qual_read refhhi lnreqSNF_80pct shref comorbidsum read30_pac read30_other {
  loc ii = `ii' + 1
  loc yv`ii' `v'
}
loc k2 =`ii' + 2
loc vc =`ii'

lab var read30_other "30-day readmission rate among all other"
lab var read30_pac "Readmission rate among SNF referred"
lab var shref "Probability of referring to SNF"
lab var vi_snf "Probability of acquiring SNF"
lab var refhhi "SNF referral HHI"
lab var qual_read "Referral share to low-readmission SNFs"
lab var closeSNF "Closure of SNF"

forval x=1/`vc' {
  di "`yv`x'''"
  loc l`x' "`: var label `yv`x'''"
  di "`l`x''"
}

*reshape OLS & IV coefficients & 95% CI to report as tables

loc file ols_`fname'_ci
import delimited `reg'/`file'.txt, clear

drop if _n==1 | _n==3
drop if v1=="Constant" | v1[_n-1]=="Constant" | v1[_n-2]=="Constant"

*coeff
preserve
keep if _n==2
*| _n==4
keep v3-v`k2'
destring *, replace ig("*")
xpose, clear varname
tempfile coef
save `coef'
restore

*CI
preserve
keep if _n==3
*| _n==5
drop v1-v2

foreach v of varlist * {
  split `v', p(" - ")
  destring `v'1 `v'2, ig("()") replace
}
keep v31-v`k2'2
xpose, clear varname

foreach v of varlist v1-v`nn' {
  gen new`v' = string(`v')
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.000" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."

  capture drop next`v'
  gen next`v' = `v'[_n+1]

  gen new`v' = "[" + `v' + ", " + next`v' + "]"
}

forval x = 3/`k2' {
    drop if _varname=="v`x'2"
}
keep new* _varname

forval x=3/`k2' {
  replace _varname = "v`x'" if _varname=="v`x'1"
}
merge 1:1 _varname using `coef', nogen
format v1-v`nn' %9.2f

loc ll
forval x = 1/`nn' {
  loc ll `ll' v`x' newv`x'
}
order `ll'

gen group = substr(_var,2,.)
destring group, replace
sort group

*drop _varname
tempfile coef_ci
save `coef_ci'
restore

*get other stats like # obs, F-stats
import delimited `reg'/`file'.txt, clear
drop if _n==1 | _n==3
keep if v1=="R-squared" | v1=="Observations"

drop v1 v2
destring *, replace ig(",")
xpose, clear varname
renvars v1 v2  \ obs R2
merge 1:1 _varname using `coef_ci', nogen
drop _varname
order `ll' obs R2
format R2 %9.2f

sort group
capture drop outcome
gen outcome = ""
forval x=1/`ii' {
  replace outcome = "`l`x''" if _n==`x'
}

outsheet using `reg'/tbl_`file'.csv, replace names comma

*--------------------------

loc file ols_`fname'_pv
import delimited `reg'/`file'.txt, clear

drop if _n==1 | _n==3
drop if v1=="Constant" | v1[_n-1]=="Constant" | v1[_n-2]=="Constant"

*coeff
preserve
keep if _n==2
*| _n==4
keep v3-v`k2'
destring *, replace ig("*")
xpose, clear varname
tempfile coef
save `coef'
restore

*p-value
preserve
keep if _n==3
*| _n==5
drop v1-v2

foreach v of varlist * {
  destring `v', ig("()") replace
  format `v' %9.2f
}
xpose, clear varname

foreach v of varlist v1-v`nn' {
  gen new`v' = string(`v')
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.000" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."
  rename `v' new`v'
}

merge 1:1 _varname using `coef', nogen
format v1-v`nn' %9.2f

loc ll
forval x = 1/`nn' {
  loc ll `ll' v`x' newv`x'
}
order `ll'

gen group = substr(_var,2,.)
destring group, replace
sort group

tempfile coef_ci
save `coef_ci'
restore

use `coef_ci', clear
sort group
capture drop outcome
gen outcome = ""
forval x=1/`ii' {
  replace outcome = "`l`x''" if _n==`x'
}

outsheet using `reg'/tbl_`file'.csv, replace names comma

*-----------------------*-----------------------
*-----------------------*-----------------------
/* *use results from the spec with quadratic penalty rate term

loc fname quadratic
loc nn = 2

loc file "ols_`fname'_ci"
import delimited `reg'/`file'.txt, clear

drop if _n==1 | _n==3
drop if v1=="Constant" | v1[_n-1]=="Constant" | v1[_n-2]=="Constant"

*coeff
preserve
keep if _n==2| _n==4
keep v3-v`k2'
destring *, replace ig("*")
xpose, clear varname
tempfile coef
save `coef'
restore

*CI
preserve
keep if _n==3| _n==5
drop v1-v2

foreach v of varlist * {
  split `v', p(" - ")
  destring `v'1 `v'2, ig("()") replace
}
keep v31-v`k2'2
xpose, clear varname

foreach v of varlist v1-v`nn' {
  gen new`v' = string(`v')
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.000" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."

  capture drop next`v'
  gen next`v' = `v'[_n+1]

  gen new`v' = "[" + `v' + ", " + next`v' + "]"
}

forval x = 3/`k2' {
  drop if _varname=="v`x'2"
}
keep new* _varname

forval x=3/`k2' {
  replace _varname = "v`x'" if _varname=="v`x'1"
}
merge 1:1 _varname using `coef', nogen
format v1-v`nn' %9.2f

loc ll
forval x = 1/`nn' {
  loc ll `ll' v`x' newv`x'
}
order `ll'

gen group = substr(_var,2,.)
destring group, replace
sort group

*drop _varname
tempfile coef_ci
save `coef_ci'
restore

*get other stats like # obs, F-stats
import delimited `reg'/`file'.txt, clear
drop if _n==1 | _n==3
keep if v1=="R-squared" | v1=="Observations"

drop v1 v2
destring *, replace ig(",")
xpose, clear varname
renvars v1 v2  \ obs R2
merge 1:1 _varname using `coef_ci', nogen
drop _varname
order `ll' obs R2
format R2 %9.2f

sort group
capture drop outcome
gen outcome = ""
forval x=1/8 {
  replace outcome = "`l`x''" if _n==`x'
}
di "reached"
outsheet using `reg'/tbl_`file'.csv, replace names comma


*--------------------------

loc file ols_`fname'_pv
import delimited `reg'/`file'.txt, clear

drop if _n==1 | _n==3
drop if v1=="Constant" | v1[_n-1]=="Constant" | v1[_n-2]=="Constant"

*coeff
preserve
keep if _n==2| _n==4
keep v3-v`k2'
destring *, replace ig("*")
xpose, clear varname
tempfile coef
save `coef'
restore

*p-value
preserve
keep if _n==3 | _n==5
*| _n==5
drop v1-v2

foreach v of varlist * {
  destring `v', ig("()") replace
  format `v' %9.2f
}
xpose, clear varname

foreach v of varlist v1-v`nn' {
  gen new`v' = string(`v')
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.000" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."
  rename `v' new`v'
}

merge 1:1 _varname using `coef', nogen
format v1-v`nn' %9.2f

loc ll
forval x = 1/`nn' {
  loc ll `ll' v`x' newv`x'
}
order `ll'

gen group = substr(_var,2,.)
destring group, replace
sort group

tempfile coef_ci
save `coef_ci'
restore

use `coef_ci', clear
sort group
capture drop outcome
gen outcome = ""
forval x=1/8 {
  replace outcome = "`l`x''" if _n==`x'
}

outsheet using `reg'/tbl_`file'.csv, replace names comma */






/* *---------------------------
*IV 1st stage when dep var = spp X post 2011
foreach file in "iv1s_sppXpost2011_reform_post_2iv" {
  insheet using `reg'/`file'.csv, clear

  drop if _n==1 | _n==3
  keep if _n > 1
  keep v4 v5

  *p-value
  preserve
  gen pv = 0
  forval x = 1/`n2' {
    di 3*(`x'-1)+2
    replace pv = 1 if _n==3*(`x'-1)+2
  }
  keep if pv==1
  drop pv

  foreach v of varlist * {
    destring `v', ig("()") replace
    format `v' %9.2f
  }
  xpose, clear varname
  forval x=1/`n2' {
    rename v`x' pv`x'
  }
  tempfile pv
  save `pv'
  restore

  *coeff
  preserve
  gen coef = 0
  forval x = 1/`n2' {
    replace coef = 1 if _n==1+(`x'-1)*3
  }
  keep if coef ==1
  keep v4 v5
  destring v4 v5, replace ig("*")
  xpose, clear varname
  tempfile coef
  save `coef'
  restore

  *CI
  preserve
  gen ci = 0
  forval x = 1/`n2' {
    replace ci = 1 if _n==3+(`x'-1)*3
  }
  keep if ci ==1
  keep if _n < 3

  forval x=4/5 {
    split v`x', p(",")
    destring v`x'1 v`x'2,ig("[]") replace
  }

  keep v41 v42 v51 v52
  xpose, clear varname

  foreach v of varlist v1-v`n2' {
    gen new`v' = string(`v')
    format new`v' %5s
    drop `v'
    rename new`v' `v'
    replace `v' = "0.000" if `v'=="0"
    replace `v' = "0" + `v'  if substr(`v',1,1)=="."
    replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."

    capture drop next`v'
    gen next`v' = `v'[_n+1]

    gen new`v' = "[" + `v' + ", " + next`v' + "]"
  }
  keep if _varname=="v41" | _varname=="v51"
  keep new* _varname

  forval x=4/5 {
    replace _varname = "v`x'" if _varname=="v`x'1"
  }
  merge 1:1 _varname using `coef', nogen
  merge 1:1 _varname using `pv', nogen
  format v1-v`n2' pv1-pv`n2'  %9.2f

  loc ll
  forval x = 1/`n2' {
    loc ll `ll' v`x' newv`x' pv`x'
  }
  order `ll'
  *drop _varname
  tempfile coef_ci
  save `coef_ci'
  restore

  *get other stats like # obs, F-stats
  insheet using `reg'/`file'.csv, clear
  drop if _n==1 | _n==3
  keep if v1=="Observations" | v1=="F-statistic"
  keep v4-v5
  destring *, replace
  xpose, clear varname
  renvars v1 v2 \ obs Fstat
  merge 1:1 _varname using `coef_ci', nogen
  drop _varname
  order `ll' Fstat obs

  des
  outsheet using `reg'/tbl_`file'.csv, replace names comma
}
*------------------------
*IV 2nd stage
loc file iv2s_reform_post_2iv
insheet using `reg'/`file'.csv, clear
drop if _n==1 | _n==3
keep if _n > 1
drop v1

*coefficient
preserve
gen coef = _n==1
keep if coef ==1
keep v*
destring *, replace ig("*")
xpose, clear varname
tempfile coef
save `coef'
restore

*p-value
preserve
gen pv = 0
forval x = 1/`nn' {
  di 3*(`x'-1)+2
  replace pv = 1 if _n==3*(`x'-1)+2
}
keep if pv==1
drop pv

foreach v of varlist * {
  destring `v', ig("()") replace
  format `v' %9.2f
}
xpose, clear varname
forval x=1/`nn' {
  rename v`x' pv`x'
}
tempfile pv
save `pv'
restore

*CI
preserve
keep if _n==3

foreach v of varlist * {
  split `v', p(",")
  destring `v'1 `v'2, ig("[]*") replace
}
keep v21-v`k'2

xpose, clear varname

foreach v of varlist v1-v`nn' {
  gen new`v' = string(`v')
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.000" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."

  capture drop next`v'
  gen next`v' = `v'[_n+1]

  gen new`v' = "[" + `v' + ", " + next`v' + "]"
}

forval x = 2/9 {
    drop if _varname=="v`x'2"
}
keep new* _varname

forval x=2/9 {
  replace _varname = "v`x'" if _varname=="v`x'1"
}
merge 1:1 _varname using `coef', nogen
merge 1:1 _varname using `pv', nogen
format v1-v`nn' pv1-pv`nn' %9.2f

loc ll
forval x = 1/`nn' {
  loc ll `ll' v`x' newv`x' pv`x'
}
order `ll'
*drop _varname
tempfile coef_ci
save `coef_ci'
restore

*get other stats like # obs, F-stats
insheet using `reg'/`file'.csv, clear
drop if _n==1
keep if v1=="Observations" | v1=="J-statistic p-value" | v1=="R-squared"
drop v1
destring *, replace
xpose, clear varname
renvars v1 v2 v3 \ R2 Jstat obs
merge 1:1 _varname using `coef_ci', nogen
drop _varname
order `ll' obs R2 Jstat


capture drop outcome
gen outcome = ""
forval x=1/8 {
  replace outcome = "`l`x''" if _n==`x'
}
format R2 Jstat %9.2f

outsheet using `reg'/tbl_`file'.csv, replace names comma */
