*create hospital-quarter level data on VI and penalty

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

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
keep if fy==2010
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

use index_admit_chm, clear
merge m:1 provid using `hosp_keep', keep(3) nogen
merge m:1 provid using `hosp_keep2', keep(3) nogen

*merge with PAC referral data
merge 1:m cond provid ym dischyear dischmth qtr fy using PACreferral_tchpm, keep(1 3) nogen

compress
save referral_tchpm, replace
*save match_pnlthosp, replace
*------------------------------------
*get hosp chars data from the cost report & AHA data
insheet using `dta'/costreport/hosp_vi.csv, comma names clear
keep if pac=="hha" | pac=="snf"
drop if fy > 2016 | fy < 2011
sort prov_num fy pac
keep prov_num fyear state pac vi teaching urban own_* beds dischrg size
reshape wide vi, i(prov_num fyear state) j(pac) string
rename vihha own_hha
rename visnf own_snf

*recode the outlier # beds as missing and use the previous FY's value
replace beds = . if beds > 3000 & beds!=.
sort prov fy
bys prov: replace beds = beds[_n-1] if beds >=.

rename prov_num provid
rename fyear fy
duplicates drop

duplicates tag provid fy, gen(dup)
assert dup==0
drop dup

*reconstruct size category b/c missing values in bed is filled with previously available values
*create hospital size category
drop size
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 500
replace size = 3 if beds > 500
assert size!=.
replace size = . if beds==.

sort provid fy
tempfile costreport
save `costreport'
*--------------------------------------------------
*create hospital-FY level cost report data for # beds, ownership type
use `dta'/pos/pos_panel, clear
foreach v of varlist own_* {
  rename `v' `v'_pos
}
drop fac_name
tempfile pos
save `pos'
*--------------------------------------------------
*create hospital-month level data
use match_pnlthosp, clear

*total # PAC referrals for each hospital-month
collapse (sum) *_pac, by(provid dischyear dischmth fy qtr pac cond)

*merge with hospital-month level # discharges, readmits, etc.
merge m:1 condition provid dischyear dischmth using `index2', keep(3) nogen

*merge with hosp chars data
merge m:1 provid fy using `costreport', nogen keep(1 3)
drop state
merge m:1 provid fy using `pos', nogen keep(1 3)

*fill in missing values of hospital characteristics
preserve
keep provid fy own_hha-own_gv_pos
duplicates drop
sort provid fy
list in 1/30
foreach v of varlist own_hha-own_gv_pos {
    bys provid: replace `v' = `v'[_n-1] if `v' >=.
}
*trust cost report data more
foreach t in "np" "gv" "fp" {
  replace own_`t' = own_`t'_pos if own_`t'==.
}
replace beds = crtfd_bed if beds==.
/* gen diff = beds - crtfd_bed
list provid fy *bed* diff if diff < -500 */

drop size
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 500
replace size = 3 if beds > 500
assert size!=.
replace size = . if beds==.

* exclude hospitals with small # beds
bys provid: egen x = min(beds)
*tab beds

*tag beds < 30
gen smallbeds = x < 30
tab smallbeds
*93 hospitals
drop x crtfd_bed_cnt own*pos

tempfile smallbeds
save `smallbeds'
restore

drop own_hha-own_gv_pos
merge m:1 provid fy using `smallbeds', nogen
drop if smallbeds==1
drop smallbeds

*drop 2012 June b/c we don't have data for the month
drop if dischyear==2012 & dischmth==6

compress
save referral_hm, replace
* this is a balanced panel where every month appears for each pac-cond pair

*----------------------------------------------
*create hospital-quarter level data
use referral_hm, clear
*create quarterly data
collapse (sum) *_pac dischnum-read90 (mean) ses_score, by(pac cond provid dischyear qtr fy own* teaching urban beds dischrg size)

compress
save referral_hq, replace

*-----------
*construct for each hospital-quarter, the referral concentration
use match_pnlthosp, clear

*aggregate to hosp-PAC provider-quarter level
collapse (sum) dischnum_pac, by(pac cond provid pacprovid dischyear qtr fy)
rename dischnum_pac ref_hpq

merge m:1 pac cond provid dischyear qtr fy using referral_hq, keep(2 3) nogen
gen sh2 = (ref_hpq / dischnum_pac)^2
collapse (sum) refhhi_hq = sh2, by(pac cond provid dischyear qtr fy)
sum refhhi

tempfile refhhi
save `refhhi'
*-----------
use referral_hq, clear
merge 1:1 pac cond provid dischyear qtr fy using `refhhi', nogen

*get PAC market concentration

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using /ifs/home/kimk13/VI/data/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  merge m:1 `g' fy provid using `dta'/`g0'hhi, keep(1 3)
  assert `g'==. | fy < 2011 if _m==1
  drop _merge
}

compress
save referral_hq, replace
