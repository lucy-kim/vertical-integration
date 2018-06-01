*create index admission data for each condition-hospital-FY-comorbidity group

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/2.comorbidities_index

foreach d in "MI" "HF" "PN" "HK" {
  loc f `d'_comorbidity_May_23rd.csv
  insheet using `f', comma names clear
  de

  gen ym = ym(year, month)
  format ym %tm
  tab ym
  rename month dischmth
  rename year dischyear
  duplicates drop

  gen condition = "`d'"
  replace cond = "AMI" if cond=="MI"

  tempfile tmp_`d'
  save `tmp_`d''
}

clear
foreach d in "MI" "HF" "PN" "HK" {
  append using `tmp_`d''
}

compress
save `dta'/Medicare/index_admit_comorbid_chm, replace


foreach d in "MI" "HF" "PN" "HK" {
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

  collapse (sum) metacancer-hipfracture_ct, by(cond provid fy)

  tempfile tmp_`d'2
  save `tmp_`d'2'
}

clear
foreach d in "MI" "HF" "PN" "HK" {
  append using `tmp_`d'2'
}

compress
save  `dta'/Medicare/index_admit_comorbid_chy, replace

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
