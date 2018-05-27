*create index admission data for each condition-hospital-FY-comorbidity group

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

foreach d in "AMI" "HF" "PN" {
  if "`d'"=="AMI" {
    loc f `d'_comorbidity_v9_April_18th.csv
  }
  else {
    loc f `d'_comorbidity_v7_April_17th.csv
  }
  insheet using `f', comma names clear
  de

  gen ym = ym(year, month)
  format ym %tm
  tab ym
  rename month dischmth
  rename year dischyear
  duplicates drop

  gen condition = "`d'"

  tempfile tmp_`d'
  save `tmp_`d''
}

clear
foreach d in "AMI" "HF" "PN" {
  append using `tmp_`d''
}

compress
save index_admit_comorbid_chm, replace


foreach d in "AMI" "HF" "PN" {
  use `tmp_`d'', clear

  *create quarters using months
  gen qtr = .
  forval x = 1/4 {
    replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
  }

  *create FY (ending in June) using months
  gen fy = .
  forval x = 2007/2017 {
    loc y = `x'-1
    replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
  }
  assert fy!=.
  tab fy

  collapse (sum) metacancer-hipfracture_ct, by(provid fy)
  gen condition = "`d'"

  tempfile tmp_`d'2
  save `tmp_`d'2'
}
/*
loc d "AMI"
use `tmp_`d'', clear
sum fy
loc lm = `r(max)'
loc fm = `r(min)'

*create a base FY list & expand
keep provid metacancer-ulcers
foreach v of varlist  metacancer-ulcers {
replace `v' = 0
}
duplicates drop

loc g = `lm' - `fm' + 1
expand `g'
bys provid: gen fy = `fm' + _n-1

merge 1:1 provid fy using `tmp_`d''

*fill in zero's if unmatched
sort cond provid drgcd fy
replace count = 0 if _m==1

drop _m */

clear
foreach d in "AMI" "HF" "PN" {
  append using `tmp_`d'2'
}

compress
save index_admit_comorbid_chy, replace

*----------------------------------
loc d AMI
loc f `d'_comorbidity_v9_April_18th.csv
insheet using `f', comma names clear
rename month dischmth
rename year dischyear

duplicates tag provid dischyear dischmth , gen(dup)
tempfile tmp
save `tmp'

use index_admit_chm, clear
*keep if cond=="AMI"
drop if cond=="HK"
keep provid dischyear dischmth dischnum cond

*sort provid dischyear dischmth cond

merge 1:1 provid dischyear dischmth cond using index_admit_comorbid_chm, keep(3) nogen

foreach v of varlist *_ct {
  qui gen `v'_r = `v' / dischnum
  qui count if `v'_r > 1 & `v'_r!=.
  di "`v': `r(N)' obs have the rate > 1"
  di ""
}
gen ulcersr = ulcers / dischnum


list provid ym cond ulcer* dischnum  if ulcersr !=. & ulcersr > 1
list provid ym cond ulcer* dischnum  if provid==40051 & dischyear==2015 & dischmth==3

loc f AMI_comorbidity_v5_April_16th.csv
insheet using `f', comma names clear
rename month dischmth
rename year dischyear
tempfile tmp2
save `tmp2'

use index_admit_chm, clear
keep if cond=="AMI"
keep provid dischyear dischmth dischnum

merge 1:1 provid dischyear dischmth using `tmp2', keep(3) nogen
gen ulcersr = ulcers / dischnum
list if provid==40051 & ulcersr > 1

/* 510047 2011/09
450104
list provid-dischnum ulcers* if provid==450104 & ulcers > 1 */

use index_admit_chm, clear
collapse (sum) dischnum, by(provid fy condition)

merge 1:1 provid fy condition using index_admit_comorbid_chy, keep(3) nogen

foreach v of varlist metacancer_ct-ulcers_ct {
  *gen rate_`v' = `v'/dischnum
  *tab cond if rate_`v' > 1
  *all AMI
  di "`v'"
  tab provid if rate_`v' > 1
}
list if provid==40051 & rate_ulcer > 1

gen ulcersr = ulcers / dischnum
sum ulcersr
