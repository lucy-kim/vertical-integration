*create # index admissions and SNF referrals that are dual-eligible for each condition-hospital-FY

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/5-31/dual_counts_May_31

foreach d in "mi" "hf" "pn" "hk" {
  loc f `d'_dual_counts_May_31st.csv
  insheet using `f', comma names clear
  de

  gen ym = ym(dischyear, dischmth)
  format ym %tm
  tab ym
  duplicates drop

  gen condition = "`d'"
  replace cond = "AMI" if cond=="mi"
  replace cond = "HF" if cond=="hf"
  replace cond = "PN" if cond=="pn"
  replace cond = "HK" if cond=="hk"

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

  tempfile tmp_`d'
  save `tmp_`d''
}

clear
foreach d in "mi" "hf" "pn" "hk" {
  append using `tmp_`d''
}

compress
save  `dta'/Medicare/index_admit_dual_chm_nosw, replace

*----------

use `dta'/Medicare/index_admit_dual_chm_nosw, clear
collapse (sum) snf_dual index_dual snf_count, by(cond provid fy)

compress
save  `dta'/Medicare/index_admit_dual_chy_nosw, replace


*---------------------

cd `dta'/Medicare
use SNFreferral_tchpm, clear
collapse (sum) dischnum_pac , by(cond provid dischyear dischmth)
merge 1:1 cond provid dischyear dischmth using index_admit_dual_chm
keep if _m==3
drop _m

gen snf_dual_rate2 = snf_dual / dischnum_pac

list cond provid dischyear dischmth dischnum_pac snf_count snf_dual_rate2 if snf_dual_rate2 > 1 in 1/100
