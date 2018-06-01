*Do high-quality SNFs receive more referrals?

set maxvar 20000
set matsize 11000
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

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
keep pacprovid defcnt new
*keep pacprovid defcnt fy firstqual new

tempfile snfqual
save `snfqual'

use SNFreferral_tchpy.dta, clear
drop if cond=="HK"
drop if pacprovid==""

collapse (sum) dischnum_pac read30_pac samh30_pac, by(provid pacprovid fy)
tempfile ref
save `ref'

*calculate raw readmission rate in 2008 := [#readmissions for all hospitals] / [# referrals from all hospitals]
use `ref'
keep if fy==2008
collapse (sum) dischnum_pac read30_pac samh30_pac, by(pacprovid)
gen read30_pac_rate = read30_pac/dischnum_pac
gen samh30_pac_rate = samh30_pac/dischnum_pac
keep pacprovid read30_pac_rate samh30_pac_rate
tempfile read08
save `read08'

use `read08', clear
merge 1:1 pacprovid using `snfqual', keep(3) nogen
gen lndefcnt = log(defcnt+1)

*at the SNF's HRR level, standardize score of the SNF quality
preserve
use snf_hrr_xwalk, clear
collapse (min) hrrnum, by(pacprovid)
tostring pacprovid, format(%06.0f) replace
tempfile hrr_snf
save `hrr_snf'
restore

merge 1:1 pacprovid using `hrr_snf', keep(3) nogen

foreach v of varlist read30_pac_rate samh30_pac_rate lndefcnt {
  replace `v' = -1 * `v'
}
foreach v of varlist read30_pac_rate samh30_pac_rate lndefcnt {
  capture drop mean sd
  bys hrrnum: egen mean = mean(`v')
  bys hrrnum: egen sd = sd(`v')
  gen std_`v' = (`v'-mean)/sd
}

keep std_* pacprovid read30_pac_rate samh30_pac_rate lndefcnt defcnt new

merge 1:m pacprovid using `ref'

/* *merge m:1 pacprovid using `snfqual', keep(1 3)
*47K have unmatched
*sort pacprovid fy provid
*list in 1/10
merge m:1 pacprovid using `snfqual', keep(1 3) nogen
gen lndefcnt = log(defcnt+1)
*764 unmatched SNFs - ignore them and they will drop out

merge m:1 pacprovid using `read08', keep(1 3) nogen */

*create share of referrals from each hospital to each SNF
bys provid fy: egen totref = sum(dischnum_pac)
gen reffrac = dischnum_pac/totref
assert reffrac >= 0 & reffrac <=1

*aggregate up to the hospital-year level by taking the weighted average of # deficiency counts using the referral share as the weights
gen wgt_def = std_lndef * reffrac
gen wgt_def2 = std_lndef * reffrac if new==0
gen wgt_read = std_read30 * reffrac
gen wgt_samh = std_samh30 * reffrac

collapse (sum) qual_def = wgt_def qual_def_old = wgt_def2 qual_read = wgt_read qual_samh = wgt_samh (mean) defcnt, by(provid fy)

compress
save qualindex, replace

*--------------------------------
* regress the performance index on the HRRP X Post

use ivpenalty_VI_agg3c, clear

merge 1:1 provid fy using qualindex, keep(1 3) nogen
lab var qualindex "SNF low-performance index"

loc file1 ols_spp_pv_qual
loc file2 ols_spp_ci_qual
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci


loc int 2011
loc pnltprs1 sppXpost`int'
loc comorbid metacancer_ct-hipfracture_ct
loc reform EHRstage1 EHRstage2 vbp_adjf HACstatus
loc sp _Ify_* own_* urban teaching _Isize* lnnsnf_mkt_samehrr black white `comorbid' `reform'

forval sn = 1/2 {
  foreach yv of varlist qualindex {

    loc out "outreg2 using `reg'/`file`sn''.xls, append label ctitle(`l`yv'') `stat`sn''"

    forval n = 1/1 {
      xtreg `yv' `pnltprs`n'' `sp' vi_snf_l , cluster(provid) fe

      *mean dep var
      sum `yv' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
    }
  }
}

*-------------
*for all the outcomes, when using the specification with interaction with year dummies, let's just plot the coefficients
loc pnltprs2 sppX20*

foreach yv of varlist qualindex {
  loc n 2
    xtreg `yv' `pnltprs`n'' `sp' vi_snf_l, fe cluster(provid)
  tempfile `yv'
  parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)
}

use `qualindex', clear
gen gp = "qualindex"
assert gp!=""
keep if regexm(parm, "spp")
gen year = substr(parm, -4,4)
tab gp
drop dof t

compress
outsheet using `reg'/coef_DiD_static_quality.csv, replace names comma


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
