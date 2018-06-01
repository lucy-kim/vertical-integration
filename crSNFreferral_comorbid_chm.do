*create SNF referral data for each condition-hospital-SNF-FY-comorbidity group
*currently, at the hospital-month-condition level only among SNF referred admissions


loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/4.Comorbidities_SNF

foreach d in "MI" "HF" "PN" "hk" {
  loc f `d'_comorbidity_May_30th_SNF.csv
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
  replace cond = "HK" if cond=="hk"

  tempfile tmp_`d'
  save `tmp_`d''
}

clear
foreach d in "MI" "HF" "PN" "hk" {
  append using `tmp_`d''
}

compress
save `dta'/Medicare/SNFreferral_comorbid_chm, replace

*---------------------
*aggregate up to the year level
use `dta'/Medicare/SNFreferral_comorbid_chm, clear

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

compress
save `dta'/Medicare/SNFreferral_comorbid_chy, replace
