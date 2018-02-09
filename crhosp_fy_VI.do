*create hospital-FY level data containing the VI measures, hospital characteristics

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

*construct hosp-FY level data containing referral concentration for each PAC type (SNF/HHA)-condition
*aggregate to the hospital-PAC provider-FY level
*use referral_tchpm, clear
use PACreferral_tchpm, clear
*drop if pac==""
collapse (sum) dischnum_pac, by(pac condition provid fy pacprovid)

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys pac condition provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(pac condition provid fy)
assert refhhi <= 1

tempfile hosp_fy_ref
save `hosp_fy_ref'
*------------------------------------
* construct hosp-FY level data using the index admissions data

*restrict to hospitals that are subject to HRRP penalty; keep only hospitals that are penalized in FY 2015
use `dta'/hrrp_penalty, clear
keep if fy ==2013
keep provid
duplicates drop
destring provid, replace
tempfile hosp_keep
save `hosp_keep'

*tag hospitals whose mean number of discharges for all those conditions across FY is < 50
use index_admit_chm.dta, clear
keep if fy==2011
collapse (sum) dischnum, by(provid)
drop if dischnum < 50
/* collapse (sum) dischnum, by(provid fy)
tab dischnum
bys provid: egen mm = mean(dischnum)
drop if mm < 50 */
keep provid
duplicates drop
tempfile hosp_keep2
save `hosp_keep2'

*from the PAC referral data, create hospital-FY level data
use PACreferral_tchpm, clear
collapse (sum) dischnum_pac-samh90_pac, by(pac condition provid fy)
tempfile ref
save `ref'

use index_admit_chm, clear
keep if fy > 2010
merge m:1 provid using `hosp_keep', keep(3) nogen
merge m:1 provid using `hosp_keep2', keep(3) nogen

collapse (sum) dischnum-read90 (mean) ses_score, by(condition provid fy)

expand 2
bys condition provid fy: gen pac = "SNF" if _n==1
bys condition provid fy: replace pac = "HHA" if _n==2

merge 1:1 pac condition provid fy using `ref', keep(1 3)
foreach v of varlist *_pac {
  replace `v' = 0 if _m==1
}
drop _m

*merge with referral HHI data
merge 1:1 pac condition provid fy using `hosp_fy_ref', keep(1 3) nogen

*merge with hospital chars data from cost report
gen prov_num = string(provid, "%06.0f")
rename fy fyear
merge m:1 prov_num fyear using `dta'/costreport/hosp_chars_cr, keep(1 3)
rename fyear fy
drop vi_renal-vi_asc vi_ipf vi_swbsnf-vi_rhc vi_hospice vi_nf state

sort pac cond provid fy
foreach v of varlist vi_snf - urban {
    bys pac cond provid: replace `v' = `v'[_n-1] if `v' >= .
}
gsort pac cond provid -fy
foreach v of varlist vi_snf - urban {
    bys pac cond provid: replace `v' = `v'[_n-1] if `v' >= .
}

drop _m

*merge with inpatient Medicare payment data for each condition
merge m:1 cond provid using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_2014, keep(1 3)

sort pac cond provid fy
list pac cond provid fy tot_pat_rev tmcr_pmt _m in 1/30

/* preserve
use `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hk_hosp, clear
restore */

* exclude hospitals with small # beds
bys provid: egen a = min(beds)
sum beds if a >= 30
drop if a < 30
drop a
sum beds

merge 1:1 pac cond provid dischyear qtr fy using `refhhi', nogen

*get PAC market concentration

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using /ifs/home/kimk13/VI/data/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
capture drop _m
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  merge m:1 `g' provid fy using `dta'/`g0'hhi, keep(1 3) nogen
}

tempfile init
save `init'

*------------------------------------
*merge with HRRP data
use `dta'/hrrp_penalty, clear
drop if fy > 2016
assert err_hk!=. if fy>=2015
foreach d in "ami" "hf" "pn" "hk" "copd" {
    di "`d'"
    sum n_`d' if err_`d'==0
    replace err_`d' = . if err_`d'==0
}
*ERR = 0 if # cases for the condition is < 25

*get distribution of the number of cases

destring provid, replace

tempfile penalty
save `penalty'

*------------------------------------
*merge the VI data with penalty & hosp chars data
use `init', clear
merge m:1 provid fy using `penalty', keep(1 3) nogen
*note: some hospitals are matched to penalty data only for after 2013 - identify these by finding pnltr==.

/* *append pre-2011 data from the index admissions data
preserve
use `index2_fy', clear
keep if fy < 2011
tempfile index2_fy_pre2011
save `index2_fy_pre2011'
restore

append using `index2_fy_pre2011' */

*------------------------------------
*get PAC market concentration

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using `dta'/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  merge m:1 `g' fy provid using `dta'/`g0'hhi, keep(1 3)
  assert `g'==. | fy < 2011 if _m==1
  drop _merge
}

*get # PAC providers in the state - more exogenous?



*---------------------

*restrict to hospitals that have # discharges >= 30 in FY 2011
foreach c in "AMI" "HF" "PN" "HK" {
  gen x = dischnum if fy==2011 & cond=="`c'"
  bys provid: egen xx = max(x)
  drop if xx < 10
  drop x xx
}

*restrict to hospitals that have # discharges >= 30 in FY 2011
foreach c in "AMI" "HF" "PN" "HK" {
  sum dischnum if fy==2011 & cond=="`c'"
}
tab fy if cond=="AMI" & pac=="SNF"


merge m:1 provid using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hk_hosp, keep(1 3)



compress
save hosp_fy_VI, replace
