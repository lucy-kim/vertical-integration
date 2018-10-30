*create hospital-FY level data containing the VI measures, hospital characteristics

/* loc dta /ifs/home/kimk13/VI/data */
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"

cd `dta'/Medicare

*construct the referral HHI lumping all 3 conditions: AMI, HF, PN
use SNFreferral_tchpy_nosw.dta, clear
/* drop if cond=="HK" */
assert dischnum_pac==0 if pacprovid==.

tempfile snf_fy
save `snf_fy'

*construct the referral HHI lumping all 3 conditions: AMI, HF, PN
use `snf_fy', clear
collapse (sum) dischnum_pac, by(provid fy pacprovid cond)
assert dischnum_pac==0 if pacprovid==.

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys provid fy cond: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(provid fy cond)
assert refhhi <= 1
rename refhhi refhhi3

tempfile hosp_fy_ref3
save `hosp_fy_ref3'

*------------------------------------
*restrict to hospitals that are subject to HRRP penalty; keep only hospitals that are penalized in FY 2015
use `dta'/hrrp_penalty, clear
keep if fy ==2013
keep provid
duplicates drop
destring provid, replace
tempfile hosp_keep
save `hosp_keep'

*------------------------------------
*aggregate across referral sources and create hospital-FY-condition level data
use `snf_fy', clear
collapse (sum) dischnum_pac-black_pac, by(provid fy cond)
tempfile ref
save `ref'

*------------------------------------
* construct hosp-FY-condition level data using the index admissions data
use index_admit_chy, clear
/* drop if cond=="HK" */
merge m:1 provid using `hosp_keep', keep(3) nogen

collapse (sum) dischnum-read90 (mean) ses_score, by(provid fy cond)

merge 1:1 provid fy cond using `ref', keep(1 3)
*keep only the hospitals for which we have matched referrals for all conditions for all years
bys provid: egen max = max(_m)
bys provid: egen min = min(_m)
keep if max==3 & min==3
drop min max
assert dischnum!=.
assert dischnum_pac!=.

foreach v of varlist dischnum_pac-black_pac {
  replace `v' = 0 if dischnum==0
}

*merge with referral HHI data for 3 initial conditions lumped
merge 1:1 provid fy cond using `hosp_fy_ref3', keep(1 3) nogen
*199 hospitals have no referrals

tempfile tmp
save `tmp'

*----------------
*merge with hospital chars data from cost report
use `tmp', clear
capture drop _m
merge m:1 provid fy using `dta'/costreport/hosp_chars_cr, keep(1 3)
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

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using `dta'/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*----------------
*merge with dual-eligibility count data
preserve
use index_admit_dual_chy_nosw, clear
/* drop if cond=="HK" */
collapse (sum) *_dual snf_count, by(provid fy cond)
tempfile index_admit_dual_chy
save `index_admit_dual_chy'
restore

merge 1:1 provid fy cond using `index_admit_dual_chy', keep(1 3) nogen

*Calculate dual rate
loc v index_dual
gen `v'_rate = `v' / dischnum
loc v snf_dual
gen `v'_rate = `v' / dischnum_pac

foreach v of varlist index_dual snf_dual {
  assert `v'_rate >= 0 &  `v'_rate <=1 if `v'_rate !=.
}

*----------------
*merge with comorbidity count data
preserve
use index_admit_comorbid_chy, clear
/* drop if cond=="HK" */
collapse (sum) metaca-hipfrac, by(provid fy cond)
tempfile index_admit_comorbid_chy
save `index_admit_comorbid_chy'
restore

merge 1:1 provid fy cond using `index_admit_comorbid_chy', keep(1 3) nogen

*----------------
*merge with comorbidity count data among SNF-referred patients
preserve
use SNFreferral_comorbid_chy_nosw, clear
/* drop if cond=="HK" */
collapse (sum) metaca-hipfrac, by(provid fy cond)
foreach v of varlist metaca-hipfrac {
  rename `v' `v'_snf
}
tempfile SNFreferral_comorbid_chy
save `SNFreferral_comorbid_chy'
restore

merge 1:1 provid fy cond using `SNFreferral_comorbid_chy', keep(1 3) nogen

tempfile init
save `init'

*--------------------------------
* create more outcomes:

*create % of patients accounted for by the top SNF
use `snf_fy', clear
keep provid pacprovid fy cond dischnum_pac
bys provid fy cond: egen maxref = max(dischnum_pac)

*count # SNFs getting the largest # referrals
gen x = maxref==dischnum_pac
bys provid fy cond: egen sx = sum(x)
tab sx
* range from 1 (94%)-13
bys provid fy cond: egen totref = sum(dischnum_pac)
gen shref_bytopSNF =  dischnum_pac/totref if x==1

*create # SNFs that received 80% of referrals
gsort cond provid fy -dischnum_pac
bys cond provid fy: gen cumref = sum(dischnum_pac)
gen cumshref = cumref/totref
gen below80 = cumshref <= 0.8
bys cond provid fy: egen reqSNF_80pct = sum(below80)
replace reqSNF_80pct = reqSNF_80pct + 1

*# SNF used
gen nsnf_used = dischnum_pac > 0

collapse (sum) nsnf_used (mean) reqSNF_80pct (max) shref_bytopSNF, by(provid fy cond)
tempfile newoutcome
save `newoutcome'

*--------------------------------
*number of SNFs in the same HRR as hospital

*using the SNF POS data, count # SNFs in each HRR
*get # available SNFs in the hospital's HRR (who receives at least one referral from a hospital in the market)
*create # SNFs in each year-HRR
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

sort hrrnum fy
gen nsnf = beds > 0

collapse (sum) nsnf, by(hrrnum fy)
tempfile nsnf
save `nsnf'

*--------------------------------

use `init', clear

*merge with HRR-year level data on # SNF in each HRR
merge m:1 fy hrrnum using `nsnf', keep(1 3) nogen
rename nsnf nsnf_samehrr

*merge with more outcomes
merge 1:1 provid fy cond using `newoutcome', keep(1 3) nogen

*create SNF referral density := # referrals/# SNFs used
gen refdensity = dischnum_pac/nsnf_used

*create share of discharges referred to SNF
gen shref = dischnum_pac / dischnum
lab var shref "Share of discharges referred to SNF"

*count hospitals if # admissions = 0 at any point in time
gen x = dischnum==0
bys provid: egen zeroadmit = max(x)
drop if zeroadmit==1
drop x zeroadmit

assert shref >=0 & shref <=1

*drop Maryland hospitals
gen str6 provid_str = string(provid, "%06.0f")
gen st = substr(provid_str, 1,2)
count if st=="21" | st=="80"
*https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf
drop if st=="21" | st=="80"
drop st

drop if beds==.
* 18 hospitals

*referral HHI should be missing, not zero, if dischnum_pac = 0
foreach v of varlist refhhi* {
  replace `v' = . if dischnum_pac==0
}

foreach v of varlist white black read30 read60 read90 {
  replace `v' = `v' / dischnum
  assert `v' >=0 & `v' <= 1 if `v'!=.
}
foreach v of varlist white_pac black_pac read30_pac read60_pac read90_pac samh30_pac samh60_pac samh90_pac {
  replace `v' = `v' / dischnum_pac
  assert `v' >=0 & `v' <= 1 if `v'!=.
}

* get share of patients in each comorbidity category
foreach v of varlist metacancer_ct-hipfracture_ct {
  di "`v'------------"
  qui replace `v' = `v'/dischnum

  qui count if `v' > 1 & `v'!=.
  di "`v': `r(N)' obs have the rate > 1"
  assert `v' >=0 & `v' <= 1 if `v'!=.
  sum `v'
  di ""
}

*for SNF refered patients
*list provid fy dischnum_pac copd*f if provid==370097
foreach v of varlist metacancer_ct_snf-hipfracture_ct_snf {
  qui replace `v' = `v'/dischnum_pac

  qui count if `v' > 1 & `v'!=.
  di "`v': `r(N)' obs have the rate > 1"
  assert `v' >=0 & `v' <= 1 if `v'!=.
  qui sum `v'
  di ""
}

loc int 2011
gen post`int' = fy >=`int'

*create laggeed formally owning SNF
sort provid fy
bys provid: gen vi_snf_l = vi_snf[_n-1]
tab fy, summarize(vi_snf_l)
lab var vi_snf_l "Owns SNF, lagged"

*merge with penalty pressure data
merge m:1 provid using predict_pnltprs, keep(3) nogen
drop if ppr==.

*-----------------------
*add participation in other value-based reforms
*-----------------------

*EHR
merge m:1 provid fy using EHR-MU/ehr, keep(1 3)
sort provid fy
replace EHRstage1=0 if _m==1
replace EHRstage2=0 if _m==1
assert EHRstage1!=. & EHRstage2!=.
drop _m

*VBP
merge m:1 provid fy using VBP/vbp, keep(1 3)
tab fy if _m==1
*list cond provid fy _m vbp_adjf if provid==670055
replace vbp_adjf=1 if _m==1
assert vbp_adjf!=.
drop _m

*HAC
merge m:1 provid fy using HAC/hac, keep(1 3)
tab fy if _m==1
*2015 & 2016 all matched
replace HACstatus = 0 if _m==1
assert HACstatus!=.
drop _m HACscore

*BPCI
gen CCN =string(provid, "%06.0f")
merge m:1 CCN fy using BPCI/hosp_bpci_participant, keep(1 3)
tab fy if _m==1
replace bpci = 0 if _m==1
assert bpci!=.
drop _m

*ACO
capture drop pionACO
merge m:1 provid fy using ../costreport/hosp_chars_cr, keep(1 3) keepusing(pionACO)
tab fy if _m==1
bys provid: egen mm = max(pionACO)
replace pionACO = mm if _m==1 & pionACO==.
replace pionACO = 0 if fy < 2011
assert pionACO!=.
drop mm _m

lab var dischnum_pac "Number of referrals to SNF"
lab var refhhi3 "SNF referral concentration"
lab var vi_snf "Formally own SNF"
lab var beds "Number of beds"
lab var dischnum "Total number of discharges"
lab var white "Number of white patients"
lab var black "Number of black patients"
lab var read30 "30-day readmission rate, all discharges"
lab var read60 "31 to 60-day readmission rate, all discharges"
lab var read90 "61 to 90-day readmission rate, all discharges"
lab var read30_pac "30-day readmission rate, discharges to SNF"
lab var read60_pac "31 to 60-day readmission rate, discharges to SNF"
lab var read90_pac "61 to 90-day readmission rate, discharges to SNF"
lab var samh30_pac "30-day readmission rate to the same hospital, discharges to SNF"
lab var samh60_pac "31 to 60-day readmission rate to the same hospital, discharges to SNF"
lab var samh90_pac "61 to 90-day readmission rate to the same hospital, discharges to SNF"
lab var ses_score "Mean SES score"
lab var teaching "Teaching"
lab var urban "Urban"
lab var own_fp "For profit"
lab var own_np "Not for profit"
lab var own_gv "Government owned"
lab var uncomp1 "Uncompensated care"
lab var dissh "DSH payment"
lab var tot_pat_rev "Total patient revenue ($)"
lab var vi_snf "Formally owns SNF"
lab var urban "Urban"
lab var teaching "Teaching"
lab var own_fp "For profit"
lab var own_np "Not for profit"
lab var own_gv "Government owned"
lab var refhhi "Referral concentration"
lab var refdensity "Referral density"
lab var shref "Probability of referring to SNF"

gen lnreqSNF_80pct = ln(reqSNF_80pct)
lab var lnreqSNF_80pct "Ln # SNFs required to account for 80% referrals"
gen lnrefdensity = ln(refdensity)
lab var lnrefdensity "Ln referral density"
lab var shref_bytopSNF "% referrals accounted for by leading SNF"

compress
save hosp_fy_VI_cond_nosw, replace


*-----------------------------

/* *Send example of 3% hospital-FY-condition obs where # referrals > # discharges
use SNFreferral_tchpm, clear
collapse (sum) dischnum_pac, by(provid cond dischyear dischmth ym fy)
merge 1:1 provid cond dischyear dischmth ym fy using index_admit_chm, keepusing(dischnum)
replace dischnum_pac = 0 if _m==2
count if dischnum < dischnum_pac */
