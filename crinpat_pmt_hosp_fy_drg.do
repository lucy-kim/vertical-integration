*create FY 14-15 hospital-FY-DRG level Medicare FFS inpat payment
*Source: Medicare FFS Provider Utilization & Payment Data Inpatient Public Use File in https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient.html

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/inpat-utilization-and-payment-PUF

/* *MS-DRGs for each condition (From https://www.cms.gov/icd10manual/fullcode_cms/P0108.html)
*AMI: 280-285
*HF: 291-293
*PN: 193-195
*HK: 466-470
loc ami drg >= 280 & drg <=285
loc hf drg >= 291 & drg <=293
loc pn drg >= 193 & drg <=195
loc hk drg >= 466 & drg <=470
*I'm a little unsure about 469 & 470 which is for total joint replacement beyond hip & knee */

*instead of restricting to pre-selected DRGs for each condition, get a list of DRGs reported in the Medicare claims data and match with payment data
use `dta'/Medicare/index_admit_DRG_chy.dta, clear
rename drgcd drg
tab drg, sort
tab drg cond
keep drg
duplicates drop
tempfile drg
save `drg'

lab define ll 1 "AMI" 2 "HF" 3 "PN" 4 "HK"

forval y= 2014/2015 {
  loc file Medicare_Provider_Charge_Inpatient_DRGALL_FY`y'.csv
  insheet using `file', comma names clear

  split drgdefinition, p(" - ")
  destring drgdefinition1, gen(drg)

  merge m:1 drg using `drg', keep(3) nogen

  gen fy = `y'

  *for each hospital-condition-DRG, get total Medicare payment by multiplying the average medicare payment by total discharges
  gen tmcr_pmt = totaldischarges * averagemedicare

  rename providerid provid
  rename hospitalreferral hrr_str
  rename totaldischarges ndisch
  rename averagemedicare amcr_pmt

  keep provid hrr ndisch amcr_pmt drg fy tmcr_pmt

  compress
  save inpat_pmt_hosp_fy_drg_`y', replace
}

forval y= 2011/2013 {
  loc file Medicare_Provider_Charge_Inpatient_DRG100_FY`y'.csv
  insheet using `file', comma names clear

  split drgdefinition, p(" - ")
  destring drgdefinition1, gen(drg)

  merge m:1 drg using `drg', keep(3) nogen

  gen fy = `y'

  *for each hospital-condition-DRG, get total Medicare payment by multiplying the average medicare payment by total discharges
  gen tmcr_pmt = totaldischarges * averagemedicare

  rename providerid provid
  rename hospitalreferral hrr_str
  rename totaldischarges ndisch
  rename averagemedicare amcr_pmt

  keep provid hrr ndisch amcr_pmt drg fy tmcr_pmt

  compress
  save inpat_pmt_hosp_fy_drg_`y', replace
}


*append 2014-2015 data b/c all DRGs are captured only for those fys
clear
forval y=2011/2015 {
  append using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hosp_fy_drg_`y'
}
sort provid drg fy

*since there is no payment data for 2008-2010 & 2016, use the most recent value
preserve
keep provid drg
duplicates drop
expand 9
bys provid drg: gen fy = 2008+_n-1
tempfile base
save `base'
restore

merge 1:1 provid drg fy using `base'

*fill in missing values downstream & upstream
sort provid drg fy
foreach v of varlist ndisch amcr_pmt tmcr_pmt {
  bys provid drg: replace `v' = `v'[_n-1] if `v' >=.
}
gsort provid drg -fy
foreach v of varlist ndisch amcr_pmt tmcr_pmt {
  bys provid drg: replace `v' = `v'[_n-1] if `v' >=.
}
sort provid drg fy
assert amcr_pmt!=.

*get a unique HRR label for each hospital
preserve
keep provid hrr
drop if hrr==""
replace hrr_str = "CA - Palm Springs/Rancho Mirage" if hrr=="CA - Palm Springs/Rancho M"
duplicates drop
bys provid: gen n = _N
assert n==1
drop n
tempfile hosp_hrr_xwalk
save `hosp_hrr_xwalk'

drop hrr_str
merge m:1 provid using `hosp_hrr_xwalk', nogen
drop _merge

*keep provid fy drg hrr_str amcr_pmt

tempfile pay
save `pay'

*merge the average Medicare payment for each hospital-FY-DRG with data on # admissions for that DRG in each hospital-FY
use `dta'/Medicare/index_admit_DRG_chy.dta, clear
rename drgcd drg
merge m:1 provid fy drg using `pay', keep(1 3)

*for each condition, what is the total Medicare payment in that hospital-FY?
*get drg-level average Medicare payment X # admissions with the DRG for each hospital-fy
gen prod = amcr_pmt*count

*if there is no average payment available for the hospital-DRG-FY, use the national median for the DRG-FY
preserve
keep drg provid fy amcr
duplicates drop
collapse (median) natlmed= amcr, by(drg fy)
tempfile natlmed
save `natlmed'
restore

merge m:1 drg fy using `natlmed', nogen
replace prod = natlmed*count if _m==1

sort cond provid drg fy

*aggregate the product across DRGs for each condition
collapse (sum) prod, by(provid cond fy)
rename prod mcr_pmt

*get the HRR
merge m:1 provid using `hosp_hrr_xwalk', nogen keep(1 3)

compress
save inpat_pmt_chy, replace

/* *for each hospital, what are the DRGs showing up for each condition?
forval y=2011/2015 {
  use `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hosp_fy_drg_`y', clear
  tab drg cond
} */



/* *sum payment across DRGs for each condition-hospital-fy
collapse (sum) ndisch tmcr_pmt, by(cond provid hrr_str fy)

forval x=1/4 {
  tab cond if cond==`x'
  table fy if cond==`x', contents(mean ndisch mean tmcr_pmt )
}

*to measure the size of Medicare payments for H/K for each hospital (i.e. bite of the H/K excess readmission ratio for the hospital), need to use the pre-FY 2014 (first year of reaction to H/K penalty) H/K payments. Since FY 2013 data don't show all DRGs for the H/K but only 467, 469, 470, get values from 2014 for 466, 468

clear
forval y=2011/2015 {
  append using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hosp_fy_drg_`y'
}
sort cond drg provid fy

*focus on H/K
keep if cond==4

*to minimize the idiosyncracy, use the mean of payments across years for each DRG during 2011-2013
preserve
keep if fy < 2014
collapse (mean) tmcr_pmt, by(provid hrr_str drg)
tempfile m11_13
save `m11_13'
restore

*get values from 2014 for 466, 468
preserve
keep if fy==2014
keep if drg==466 | drg==468
keep provid hrr_str drg tmcr_pmt
tempfile m14
save `m14'
restore

use `m11_13', clear
append using `m14'
*aggregate across DRGs for H/K
collapse (sum) tmcr_pmt, by(provid hrr_str)

compress
save inpat_pmt_hk_hosp, replace

*---------------
*for simplification, use 2014 data b/c all DRGs reported for each condition
use `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hosp_fy_drg_2014, clear
collapse (sum) tmcr_pmt, by(provid hrr_str cond)
gen cond = "AMI" if condition==1
replace cond = "HF" if condition==2
replace cond = "PN" if condition==3
replace cond = "HK" if condition==4
drop condition
rename cond condition
compress
save `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_2014, replace */
