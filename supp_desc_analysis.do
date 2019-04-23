* Supplemental descriptive statistics using newly obtained hospital-year level data from Huihui

loc gph "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data/"
loc int 2011

*first get a sample of hospitals used for the analysis
cd `dta'/Medicare
use ivpenalty_VI_agg3c_nosw, clear
keep provid fy bad
duplicates drop
tab fy
tempfile hosp
save `hosp'
*------------------------------
* % 3 condition discharges
*------------------------------
cd `dta'/Medicare/whole-TM-inpat-for-RR

insheet using "HOSP_MONTH_COND_PCT_0402109.CSV", clear comma
rename year dischyear
rename month dischmth
keep provid dischyear dischmth *vol
gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
format ym %tm
tab ym

*2012 June:
foreach v of varlist *vol {
  replace `v' = . if dischyear==2012 & dischmth==6
}

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2007/2016 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert fy!=.

collapse (sum) *vol, by(provid fy)
drop hk_vol
rename mi_vol AMI_vol
rename hf_vol HF_vol
rename pn_vol PN_vol

foreach c in "AMI" "HF" "PN" {
  gen `c'_pct = `c'_vol/tvol
  assert `c'_pct >=0 & `c'_pct <=1
}
egen three_cond_pct = rowtotal(AMI_pct HF_pct PN_pct)
keep if fy >= 2009

merge 1:1 provid fy using `hosp', keep(3) nogen
sum *_pct

*-------------------------------------------------------------
* total number of discharges for the sample hospitals
*-------------------------------------------------------------
foreach v of varlist *vol {
  egen tot_`v' = sum(`v')
}
format tot_tvol %9.0f

*-------------------------------------------------------------
* % discharges with hospital LOS < 3 days
*-------------------------------------------------------------

cd `dta'/Medicare/whole-TM-inpat-for-RR

insheet using "HOSP_MONTH_FLAG_LOS_0402109.CSV", clear comma
gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
format ym %tm
tab ym, summarize(pct_)

keep provid dischyear dischmth ym *vol pct_flag_los cond
drop if cond=="HK"

*2012 June:
foreach v of varlist *vol {
  replace `v' = . if dischyear==2012 & dischmth==6
}

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2007/2016 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert fy!=.

collapse (sum) tvol flag_los_vol, by(provid fy)
keep if fy >= 2009
gen pct_los_lt3days = 100*flag_los_vol/tvol
assert pct_los_lt3days >=0 & pct_los_lt3days <=1

merge 1:1 provid fy using `hosp', keep(3) nogen

sum pct_los_lt3days

*-------------------------------------------------------------
* hospital's % SNF referrals for 3 conditions to SNFs in the same HRR (HSA) as the hospital
*-------------------------------------------------------------
cd `dta'/Medicare

*get HRR and HSA for each SNF by merging the with hospital-SNF-year level data on
loc file `dta'/dartmouth/ZipHsaHrr14
insheet using `file'.csv, comma names clear
rename zip zip
tempfile zip_hrr
save `zip_hrr'

clear
forval y=2008/2016 {
  append using `dta'/pos/snfpos`y'
}

*restrict to SNF
gen sid = string(provid_pac, "%06.0f")
gen last4 = substr(sid,3,4)
destring last4, replace
assert last4 >= 5000 & last4 <= 6499

merge m:1 zip using `zip_hrr', keep(3) nogen
rename provid_pac pacprovid
keep pacprovid fy hrrnum hsanum
duplicates drop

merge 1:m pacprovid fy using SNFreferral_tchpy_nosw, keep(3) nogen
drop if cond=="HK"

*aggregate # referrals for each hospital-SNF pair across 3 conditions
collapse (sum) dischnum_pac, by(provid fy pacprovid hrrnum hsanum)
tempfile hosp_snf_lvl
save `hosp_snf_lvl'

use ivpenalty_VI_agg3c_nosw, clear
keep provid fy hrrnum hsanum
tempfile hosp_hrr_hsa
save `hosp_hrr_hsa'

*first restrict hospital-SNF level data to hospitals that appear in the regression analysis sample
use `hosp_snf_lvl', clear
merge m:1 provid fy using `hosp_hrr_hsa', keep(3) nogen
tempfile hosp_snf_lvl
save `hosp_snf_lvl'

* hospital's % SNF referrals for 3 conditions to SNFs in the same HRR as the hospital
use `hosp_snf_lvl', clear
*_m=3 means same HRR for both hospital and SNF
*_m=1 means different HRR
merge m:1 provid fy hrrnum using `hosp_hrr_hsa', keep(1 3)

gen same_hrr_both = _m==3

collapse (sum) dischnum_pac, by(provid fy same_hrr_both)
bys provid fy: egen tot_ref = sum(dischnum_pac)
gen pref_same_hrr = 100*dischnum_pac/tot_ref
sum pref_same_hrr if same_hrr_both

* hospital's % SNF referrals for 3 conditions to SNFs in the same HSA as the hospital
use `hosp_snf_lvl', clear
*_m=3 means same HSA for both hospital and SNF
*_m=1 means different HSA
merge m:1 provid fy hsanum using `hosp_hrr_hsa', keep(1 3)

gen same_hsa_both = _m==3

collapse (sum) dischnum_pac, by(provid fy same_hsa_both)
bys provid fy: egen tot_ref = sum(dischnum_pac)
gen pref_same_hsa = 100*dischnum_pac/tot_ref
sum pref_same_hsa if same_hsa_both
*-------------------------------------------------------------
