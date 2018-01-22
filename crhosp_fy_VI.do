*create hospital-FY level data containing the VI measures, hospital characteristics

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls

cd /ifs/home/kimk13/VI/data/Medicare

loc f index_freq_no16.csv
insheet using `f', comma names clear
tempfile index
save `index'

*aggregate across destinations (ddest=06 for HHC, 03 for SNF)
use `index', clear
collapse (mean) dischnum white black read* ses_score, by(condition provid dischyear dischmth)
tempfile index2
save `index2'

loc f match_freq_no16.csv
insheet using `f', comma names clear
rename provider pacprovid

*are all the month-year present?
preserve
keep dischyear dischmth
duplicates drop
tab dischyear
*2010/6 - 2015/12
restore

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2011/2015 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}

tempfile match
save `match'

*restrict to hospitals that are subject to HRRP penalty; keep only hospitals that are penalized in FY 2015
use /ifs/home/kimk13/VI/data/hrrp_penalty, clear
keep if fy==2015
keep provid
duplicates drop
destring provid, replace
tempfile hosp_keep
save `hosp_keep'

use `match', clear
merge m:1 provid using `hosp_keep', keep(3) nogen
*for 2013-2015, about 240-246 hospitals in the penalty data unmatched (_m=2)
*fy 2016-2018 unmatched - fine b/c i don't have them

*drop for now July-Dec 2015 b/c they belong to FY 2016
drop if dischyear ==2015 & dischm > 6
assert fy!=.

*rename vars before merging with index admissions data
foreach v of varlist dischnum-samh90 {
  rename `v' `v'_pac
}

sort pac condition provid dischyear dischmth pacprovid

tempfile match_pnlthosp
save `match_pnlthosp'

*------------------------------------
*construct hosp-FY level data containing referral concentration for each PAC type (SNF/HHA)-condition
*aggregate to the hospital-PAC provider-FY level
use `match_pnlthosp', clear
collapse (sum) dischnum_pac-samh90_pac, by(pac condition provid pacprovid fy)

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys pac condition provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq dischnum_pac-samh90_pac, by(pac condition provid fy)
assert refhhi <= 1

tempfile hosp_fy_ref
save `hosp_fy_ref'
*------------------------------------
* construct hosp-FY level data using the index admissions data
use `index2', clear

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2008/2015 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert dischyear==2015 if fy==.
tab dischm if fy==.

*drop for now July-Dec 2015 b/c they belong to FY 2016
drop if dischyear ==2015 & dischm > 6
assert fy!=.

collapse (sum) dischnum-read90 (mean) ses_score, by(condition provid fy)

tempfile index2_fy
save `index2_fy'

*merge with index admissions data
use `hosp_fy_ref', clear
merge m:1 condition provid fy using `index2_fy', keep(1 3) nogen
* all merged

tempfile init
save `init'

*------------------------------------
*merge with HRRP data
use /ifs/home/kimk13/VI/data/hrrp_penalty, clear
drop if fy > 2015
assert err_hk!=. if fy>=2015
foreach d in "ami" "hf" "pn" "hk" "copd" {
    di "`d'"
    sum n_`d' if err_`d'==0
}
*ERR = 0 if # cases for the condition is < 25

foreach d in "ami" "hf" "pn" "hk" "copd" {
    di "`d'"
    sum err_`d' if n_`d' > 25
}

*get distribution of the number of cases

destring provid, replace

tempfile penalty
save `penalty'
*------------------------------------
*get hosp chars data from the cost report & AHA data
insheet using /ifs/home/kimk13/VI/data/costreport/hosp_vi.csv, comma names clear
keep if pac=="hha" | pac=="snf"
drop if fy > 2015 | fy < 2011
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

sort provid fy
tempfile costreport
save `costreport'
*------------------------------------
*merge the VI data with penalty & hosp chars data
use `init', clear
merge m:1 provid fy using `penalty', keep(1 3) nogen
merge m:1 provid fy using `costreport', keep(1 3) nogen

*append pre-2011 data from the index admissions data
preserve
use `index2_fy', clear
keep if fy < 2011
tempfile index2_fy_pre2011
save `index2_fy_pre2011'
restore

append using `index2_fy_pre2011'

*------------------------------------
*get PAC market concentration

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using /ifs/home/kimk13/VI/data/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  merge m:1 `g' fy using /ifs/home/kimk13/VI/data/`g0'hhi, keep(1 3)
  assert `g'==. | fy < 2011 if _m==1
  drop _merge
}

*get # PAC providers in the state - more exogenous?

compress
save hosp_fy_VI, replace
