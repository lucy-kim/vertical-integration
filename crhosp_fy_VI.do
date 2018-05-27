*create hospital-FY level data containing the VI measures, hospital characteristics

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

*construct hosp-FY level data containing referral concentration for each PAC type (SNF/HHA)-condition
*aggregate to the hospital-PAC provider-FY level

*construct the referral HHI lumping all 3 conditions: AMI, HF, PN
use SNFreferral_tchpm.dta, clear
drop if cond=="HK"
collapse (sum) dischnum_pac, by(cond provid fy pacprovid)
assert dischnum_pac==0 if pacprovid==.

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys cond provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(cond provid fy)
assert refhhi <= 1

rename refhhi refhhi_
reshape wide refhhi, i(provid fy) j(cond) string

tempfile hosp_fy_ref
save `hosp_fy_ref'

*construct the referral HHI lumping all 3 conditions: AMI, HF, PN
use SNFreferral_tchpm, clear
drop if cond=="HK"
collapse (sum) dischnum_pac, by(provid fy pacprovid)
assert dischnum_pac==0 if pacprovid==.

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(provid fy)
assert refhhi <= 1
rename refhhi refhhi3

tempfile hosp_fy_ref3
save `hosp_fy_ref3'
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

/* *tag hospitals whose number of discharges for each of those conditions in 2008 is >= 50
use index_admit_chm.dta, clear
sum fy
keep if fy==r(min)
collapse (sum) dischnum, by(provid)
drop if dischnum < 50
keep provid
duplicates drop
tempfile hosp_keep2
save `hosp_keep2' */

*tag hospitals whose number of referrals for all those conditions in the first year available is < 10
use SNFreferral_tchpm, clear
drop if cond=="HK"
list cond dischnum_pac if provid==330224 & fy==2015

*from the PAC referral data, create hospital-FY level data
collapse (sum) dischnum_pac-samh90_pac, by(provid fy)
tempfile ref
save `ref'

use index_admit_chm, clear
drop if cond=="HK"
merge m:1 provid using `hosp_keep', keep(3) nogen

collapse (sum) dischnum-read90 (mean) ses_score, by(provid fy)

merge 1:1 provid fy using `ref', keep(1 3)
foreach v of varlist *_pac {
  replace `v' = 0 if _m==1
}
drop _m

*merge with referral HHI data for 3 initial conditions lumped
merge 1:1 provid fy using `hosp_fy_ref', keep(1 3) nogen
merge 1:1 provid fy using `hosp_fy_ref3', keep(1 3) nogen
*199 hospitals have no referrals

tempfile tmp
save `tmp'

*----------------
*merge with hospital chars data from cost report

*fill in missing values for variables in the cost report
/* use `dta'/costreport/hosp_chars_cr, clear
drop vi_renal-vi_asc vi_ipf vi_swbsnf-vi_rhc vi_hospice vi_nf
destring prov_num, gen(provid)
rename fyear fy
drop prov_num

sort provid fy
foreach v of varlist vi_snf-urban {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}
gsort provid -fy
foreach v of varlist vi_snf-urban {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}
drop if beds==.

tempfile cr
save `cr'

*for each hospital, add a row for 2016 and replace with 2015 values
keep if fy==2015
duplicates tag provid, gen(dup)
assert dup==0
drop dup
expand 2
sort provid
bys provid: replace fy = 2016 if _n==2
drop if fy==2015
tempfile cr2016
save `cr2016'

*for now, for each hospital, add rows for 2008-2010 and replace with 2011 values
use `cr', clear
keep if fy==2011
duplicates tag provid, gen(dup)
assert dup==0
drop dup
expand 4
sort provid
bys provid: replace fy = 2007+ _n
assert fy >= 2008 & fy <=2011
drop if fy==2011
tempfile cr_pre11
save `cr_pre11'

*append
use `cr', clear
append using `cr_pre11'
append using `cr2016'
tempfile crappended
save `crappended'

use `cr', clear
keep provid
duplicates drop
expand 2016-2007
bys provid: gen fy = 2008+_n-1
merge 1:1 provid fy using `crappended'
drop state

sort provid fy
foreach v of varlist vi_snf-urban {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}
gsort provid -fy
drop state
foreach v of varlist vi_snf-urban {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}

*redefine hospital size by # bed (roughly based on https://www.hcup-us.ahrq.gov/db/vars/hosp_bedsize/nisnote.jsp)
drop size
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 300
replace size = 3 if beds > 300
assert size!=.
replace size = . if beds==.

tempfile crfinal
save `crfinal' */

*merge the referral data with cost report data
use `tmp', clear
merge 1:1 provid fy using `dta'/costreport/hosp_chars_cr, keep(1 3)
drop vi_renal-vi_asc vi_ipf vi_swbsnf-vi_rhc vi_hospice vi_nf

gsort provid fy -_m
foreach v of varlist vi_snf-size {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}
gsort provid -fy
foreach v of varlist vi_snf-size {
  bys provid: replace `v'=`v'[_n-1] if `v'>=.
}
drop _m

/* * exclude hospitals with small # beds
bys provid: egen a = min(beds)
sum beds if a >= 30
drop if a < 30
drop a
sum beds */

*merge with PAC market concentration
*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using /ifs/home/kimk13/VI/data/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
merge 1:1 provid fy using `dta'/pac_mkt_hhi, keep(1 3) nogen

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
*merge the VI data with penalty data
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

*---------------------

/* *restrict to hospitals that have # discharges >= 30 in FY 2011
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
 */

*---------------------
*merge with inpatient Medicare payment data for each condition
*before merging, aggregate the Medicare payments across conditions
preserve
use `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_chy, clear
drop if cond=="HK"
collapse (sum) mcr_pmt, by(provid fy)
tempfile mcrpmt
save `mcrpmt'
restore

merge 1:1 provid fy using `mcrpmt', keep(1 3) nogen

/* merge m:1 cond provid using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_2014, keep(1 3)
merge m:1 provid using `dta'/Medicare/inpat-utilization-and-payment-PUF/inpat_pmt_hk_hosp, keep(1 3) */

tempfile tmp2
save `tmp2'

*--------------------------------
* create 3 more outcomes: % of previous SNFs getting referrals, % HRR-level SNFs getting referrals, referral HHI among previously used SNFs for each year
use SNFreferral_tchpm, clear
drop if cond=="HK"

*obtain HRR for each hospital
*merge with HRR data
rename provid provider
merge m:1 provider using `dta'/dartmouth/hosp_hrr_xwalk, keep(1 3) nogen

collapse (sum) dischnum_pac, by(hrrnum provid fy pacprovid)

*in the hospital's HRR in each year, how many SNFs are there
preserve
keep hrrnum fy pacprovid
duplicates drop
gen i = pacprovid!=.
collapse (sum) nsnf_hrr_fy = i, by(hrrnum fy)
replace nsnf_hrr_fy = . if hrrnum==.
tempfile nsnf
save `nsnf'
restore

*how many PAC providers does a hospital refer to in each FY?
gen use = dischnum_pac > 0
bys provid fy: egen nsnf_used = sum(use)

merge m:1 hrrnum fy using `nsnf', keep(1 3) nogen

* % SNFs in the HRR used by the hospital-FY for each condition
gen shsnf_used = nsnf_used/nsnf_hrr_fy
sum shsnf_used, de
assert shsnf_used ==. if hrrnum==.

tempfile shsnf_used
save `shsnf_used'

* calculate % of previous SNFs getting referrals: for previous SNFs, look at the SNFs used in the previous FY (for 2009, look at only the 2nd half of 2008)

*create a balanced yearly panel for each hospital-SNF pair
preserve
keep provid pacprovid
duplicates drop
expand 2016-2007
bys provid pacprovid: gen fy = 2008+_n-1
assert fy!=. & fy >= 2008 & fy <= 2016
tempfile balanced_pair
save `balanced_pair'
restore

merge 1:1 provid pacprovid fy using `balanced_pair'
replace dischnum_pac = 0 if _m==2

sort provid pacprovid fy
bys provid pacprovid: gen used_before = dischnum_pac[_n-1] > 0 & dischnum_pac[_n-1]!=.
replace used_before = . if fy==2008

* count # SNFs used previously
bys provid fy: egen nsnf_used_before = sum(used_before)

*share of previously used SNFs that are used again this year
gen used_again = used_before * (dischnum_pac > 0)
bys provid fy: egen nsnf_used_again = sum(used_again)
gen shsnf_used_again = nsnf_used_again / nsnf_used_before
sum shsnf_used_again
assert shsnf_used_again >= 0 & shsnf_used_again <=1 if shsnf_used_again!=.

gen rat_nsnf_used_now_before = nsnf_used / nsnf_used_before

*calculate the referral HHI among previously used SNFs for each year
preserve
keep if used_again==1
bys provid fy: egen tot = sum(dischnum_pac)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(provid fy)
assert refhhi <= 1
rename refhhi refhhi_prevSNFs

tempfile ref_oldSNF
save `ref_oldSNF'
restore

merge m:1 provid fy using `ref_oldSNF', keep(1 3) nogen

sort provid pacprovid fy
list provid pacprovid fy dischnum_pac used_before used_again shsnf_used_again in 1/10

collapse (max) shsnf_used nsnf_used nsnf_hrr_fy nsnf_used_again  nsnf_used_before refhhi_prevSNFs rat_nsnf_used shsnf_used_ag, by(provid fy)

lab var shsnf_used_again "Share of previously used SNFs used again"
lab var rat_nsnf_used_now_before "Ratio of # SNFs used in current year to previous year"
lab var shsnf_used "Share of SNFs in the HRR used by the hospital-FY"
lab var nsnf_hrr_fy "Total number of SNFs being used by hospitals in the HRR"
lab var nsnf_used "Number of SNFs used"
lab var nsnf_used_again "Number of SNFs used again"
lab var nsnf_used_bef "Number of SNFs used in the previous year"
lab var refhhi_prevSNFs "Referral HHI among SNFs used in the previous year"

rename provider provid

tempfile newoutcomes
save `newoutcomes'
*---------------------

use `tmp2', clear
merge 1:1 provid fy using `newoutcomes', keep(1 3) nogen

*create share of discharges referred to SNF
gen shref = dischnum_pac / dischnum
lab var shref "Share of discharges referred to SNF"

compress
save hosp_fy_VI_agg3c, replace


*-----------------------------

/* *Send example of 3% hospital-FY-condition obs where # referrals > # discharges
use SNFreferral_tchpm, clear
collapse (sum) dischnum_pac, by(provid cond dischyear dischmth ym fy)
merge 1:1 provid cond dischyear dischmth ym fy using index_admit_chm, keepusing(dischnum)
replace dischnum_pac = 0 if _m==2
count if dischnum < dischnum_pac */
