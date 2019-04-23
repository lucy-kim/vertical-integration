*reshape coefficient estimate output into a formatted table
loc gph "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"

cd `dta'/Medicare

loc nn 1
loc n2 =`nn'*1
loc k 11

loc fname pbite
*spp pbite_noSNF pbite_actual actual sppshort

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
format v1 %9.2f
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
  *make decimal digits 2
  gen new`v' = string(`v', "%9.2f")
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.00" if `v'=="0"
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
  format `v' %9.3f
}
xpose, clear varname

foreach v of varlist v1-v`nn' {
  *make decimal digits 2
  gen new`v' = string(`v', "%9.2f")
  format new`v' %5s
  drop `v'
  rename new`v' `v'
  replace `v' = "0.00" if `v'=="0"
  replace `v' = "0" + `v'  if substr(`v',1,1)=="."
  replace `v' = subinstr(`v',"-.","-0.",.)  if substr(`v',1,2)=="-."
  rename `v' new`v'
}

merge 1:1 _varname using `coef', nogen
format v1-v`nn' %9.3f

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
