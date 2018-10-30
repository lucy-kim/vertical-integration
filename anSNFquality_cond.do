*Do high-quality SNFs receive more referrals?

set maxvar 20000
set matsize 11000
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"

cd `dta'/Medicare

* as a quality measure, use the first deficiency count available for each SNF (i.e. time-constant) and multiply it with share of referrals from each hospital to each SNF
use `dta'/Medicare/snf-compare/snf_deficiency.dta, clear
drop if defcnt==.
sort pacprovid fy
bys pacprovid: egen first = min(fy)
tab first
gen x = defcnt if fy==first
bys pacprovid: egen firstqual = max(x)
gen change = defcnt - firstqual
tab fy, summarize(change)
sort pacprovid fy
*list in 1/30

*tag new SNF that entered after 2008
gen new = first> 2008

keep if fy==first
keep pacprovid defcnt new zip
*keep pacprovid defcnt fy firstqual new

tempfile snfqual
save `snfqual'

use SNFreferral_tchpy_nosw.dta, clear
/* drop if cond=="HK" */
drop if pacprovid==.

collapse (sum) dischnum_pac read30_pac samh30_pac, by(provid pacprovid fy cond)
tempfile ref
save `ref'

*-----------

*calculate raw readmission rate in 2008 := [#readmissions for all hospitals] / [# referrals from all hospitals] for each SNF-condition
use `ref', clear
keep if fy==2008
collapse (sum) dischnum_pac read30_pac , by(pacprovid cond)
gen read30_pac_rate = read30_pac/dischnum_pac
keep pacprovid read30_pac_rate cond
tempfile read08
save `read08'

use `read08', clear
rename pacprovid pacprovid_n
gen pacprovid = string(pacprovid_n, "%06.0f")

*merge with deficiency counts
merge m:1 pacprovid using `snfqual', nogen
gen lndefcnt = log(defcnt+1)

*merge with 5-star rating: '10'='*', '50'='*****'
merge m:1 pacprovid using `dta'/Medicare/snf-compare/ratings2009, keep(1 3) nogen

tempfile tmp_qual
save `tmp_qual'

*-------------------------
*at the SNF's HRR level, standardize score of the SNF quality

*get HRR for each SNF
loc file `dta'/dartmouth/ZipHsaHrr14
insheet using `file'.csv, comma names clear
rename zip zipcode
tempfile zip_hrr
save `zip_hrr'

use `tmp_qual', clear
merge m:1 zipcode using `zip_hrr', keepusing(hrrnum) keep(1 3)

preserve
use snf_hrr_xwalk, clear
collapse (min) hrrnum, by(pacprovid)
tostring pacprovid, format(%06.0f) replace
tempfile hrr_snf
save `hrr_snf'
restore

preserve
keep if _m==1
drop hrrnum
merge m:1 pacprovid using `hrr_snf', keep(1 3) nogen
tempfile m1
save `m1'
restore

drop if _m==1
append using `m1'
drop _m
count if hrrnum==.
tab pacprovid if hrrnum==.
tab zip if hrrnum==.

foreach v of varlist read30_pac_rate lndefcnt {
  replace `v' = -1 * `v'
}

foreach v of varlist read30_pac_rate lndefcnt {
  capture drop mean sd
  bys cond hrrnum: egen mean = mean(`v')
  bys cond hrrnum: egen sd = sd(`v')
  gen std_`v' = (`v'-mean)/sd
}
drop if hrrnum==.
*27 SNF dropped

*SNF level quality data
keep std_* pacprovid read30_pac_rate lndefcnt defcnt new overall_rating hrrnum cond

*drop providers containing alphabet
gen x = real(pacprovid)
drop if x==.
drop x
destring pacprovid, replace

merge 1:m pacprovid cond using `ref', keep(2 3) nogen

*get hospital-specific SNF quality measured by return to the same hospital
preserve
keep if fy==2008
duplicates tag pacprovid provid cond, gen(dup)
assert dup==0
gen samh30_pac_rate = samh30_pac/dischnum_pac

*standardize within the hospital
loc v samh30_pac_rate
replace `v' = -1 * `v'
capture drop mean sd
bys cond provid: egen mean = mean(`v')
bys cond provid: egen sd = sd(`v')
gen std_`v' = (`v'-mean)/sd

keep provid pacprovid samh30_pac_rate std_samh30_pac_rate cond
tempfile samh30_pac_rate
save `samh30_pac_rate'
restore

merge m:1 provid pacprovid cond using `samh30_pac_rate', nogen

*create share of referrals from each hospital to each SNF
bys cond provid fy: egen totref = sum(dischnum_pac)
gen reffrac = dischnum_pac/totref
assert reffrac >= 0 & reffrac <=1

*aggregate up to the hospital-year level by taking the weighted average of # deficiency counts using the referral share as the weights
gen wgt_def = std_lndef * reffrac
gen wgt_def2 = std_lndef * reffrac if new==0
gen wgt_read = std_read30 * reffrac
gen wgt_samh = std_samh30 * reffrac
gen wgt_star = overall_rating * reffrac

collapse (sum) qual_def = wgt_def qual_def_old = wgt_def2 qual_read = wgt_read qual_samh = wgt_samh qual_star = wgt_star (mean) defcnt, by(provid fy cond)

compress
save qualindex_nosw_cond, replace



*-----------
*how many hospitals were paired with a SNF
use `ref', clear
keep if fy==2008
assert dischnum_pac !=.
keep if dischnum_pac > 0

preserve
sort pacprovid provid
gen i = 1
collapse (sum) nhosp_paired=i, by(pacprovid)
gen onlyoneHosp = nhosp_paired==1
tempfile snf
save `snf'
restore

merge m:1 pacprovid using `snf', nogen

*# referrals in each hospital-SNF pair
bys onlyoneHosp: egen totreferral = sum(dischnum_pac)
* 23155 /130612 = 17.7% = total % referrals

*create share of referrals from each hospital to each SNF
bys provid fy: egen totref = sum(dischnum_pac)
gen reffrac = dischnum_pac/totref

*calculate SNF's readmission rate from all hospitals
preserve
collapse (sum) dischnum_pac read30_pac, by(pacprovid)
gen readmit_snf = read30_pac/dischnum_pac
keep pacprovid readmit_snf
tempfile snf_read
save `snf_read'
restore

merge m:1 pacprovid using `snf_read', nogen

hist dischnum_pac if only==1, frac xtitle(# referrals) subti(SNFs that received referrals from only one hospital)
graph export `reg'/dischnum_pac_onlyonehosp.eps, replace


hist reffrac if only==1, frac xtitle(Fraction of hospital's referrals) subti(SNFs that received referrals from only one hospital)
graph export `reg'/reffrac_onlyonehosp.eps, replace

hist reffrac if only==0, frac xtitle(Fraction of hospital's referrals) subti(SNFs that received referrals from multiple hospitals)
graph export `reg'/reffrac_multihosp.eps, replace


hist readmit_snf if only==1, frac xtitle(SNF's readmission rate from all hospitals) subti(SNFs that received referrals from only one hospital)
graph export `reg'/readmit_snf_onlyonehosp.eps, replace

hist readmit_snf if only==0, frac xtitle(SNF's readmission rate from all hospitals) subti(SNFs that received referrals from multiple hospitals)
graph export `reg'/readmit_snf_multihosp.eps, replace


preserve
sum dischnum_pac
tab dischnum_pac
restore
*------------------------------

/* *run regression at the SNF-hospital level
use SNFreferral_tchpm.dta, clear
drop if cond=="HK"

*use only 2008 data
keep if fy==2008

*aggregate across 3 conditions
collapse (sum) *_pac, by(provid pacprovid dischmth)
*
assert dischnum_pac==0 if pacprovid==.
tempfile snf_fy
save `snf_fy'

/* *restrict to hospitals in the main sample
use ivpenalty_VI_agg3c, clear
keep provid
duplicates drop
tempfile hosp_keep
save `hosp_keep' */

*run hosp-SNF level regression
use `snf_fy', clear
*merge m:1 provid using `hosp_keep', keep(3) nogen
drop if dischnum_pac==0
drop samh* read60_pac read90_pac

*create readmission rate
bys provid: egen tot = sum(dischnum_pac)

replace read30_pac = read30_pac/tot
sum read30_pac
assert read30_pac>=0 & read30_pac <=1

bys provid: gen nSNF = _N
bys pacprovid: gen nHosp = _N
bys provid pacprovid: gen cellsize = _N

/*
* histogram of # SNFs paired with each hospital
preserve
keep provid nSNF
duplicates drop
hist nSNF, width(1) frac ti(# SNFs paired with each hospital)
graph export `out'/hist_nSNF.eps, replace
restore

* histogram of # SNFs paired with each SNF
preserve
keep pacprovid nHosp
duplicates drop
hist nHosp, width(1) frac ti(# hospitals paired with each SNF)
graph export `out'/hist_nHosp.eps, replace
restore */

* use the FWL theorem to get residuals from regressing readmission rate on hospital FEs
areg read30_pac, absorb(provid) cluster(provid)
predict res, residual

reg res i.pacprovid if nHosp > 3 [aw = dischnum_pac], cluster(pacprovid)

areg read30_pac i.pacprovid if nHosp > 3 [aw = dischnum_pac], cluster(provid) absorb(provid)
parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(SNFfe, replace) */
