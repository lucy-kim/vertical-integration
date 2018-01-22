*descriptive analysis of the trend of vertical integration over time
loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls

cd /ifs/home/kimk13/VI/data/Medicare

loc f index_freq_no16.csv
insheet using `f', comma names clear
tempfile index
save `index'

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
merge m:1 provid using `hosp_keep'
*for 2013-2015, about 240-246 hospitals in the penalty data unmatched (_m=2)
*fy 2016-2018 unmatched - fine b/c i don't have them

preserve
keep if _m==2 & fy < 2016
keep provid
duplicates drop
compress
save hosp_notinMedicare, replace
restore

keep if _merge==3
drop _m

*drop for now July-Dec 2015 b/c they belong to FY 2016
drop if dischyear ==2015 & dischm > 6
assert fy!=.

tempfile match_pnlthosp
save `match_pnlthosp'

*-------------------------
*get number of discharges per month, quarter, year, respectively

loc tunit_mo dischyear dischmth
loc tunit_qtr dischyear qtr
loc tunit_fy fy

*monthly
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "mo"
*reshape wide first
reshape wide dischnum-samh90, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace sum(log) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*quarterly
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "qtr"
*aggregate to the quarter level
collapse (sum) dischnum, by(condition provid pac pacprovid dischyear qtr)

*reshape wide first
reshape wide dischnum, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace  sum(detail) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*by FY
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "fy"
*aggregate to the quarter level
collapse (sum) dischnum, by(condition provid pac pacprovid fy)

*reshape wide first
reshape wide dischnum, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace  sum(detail) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*----------------------------
*at the FY level, number & growth rate of referrals to all PAC providers & # PAC referral partners & growth of PAC partners by condition

loc t "fy"
foreach p in "HHA" "SNF" {
  use `match_pnlthosp', clear
  keep if pac=="`p'"

  *aggregate to the hospital-PAC provider-FY level
  collapse (sum) dischnum, by(condition provid pacprovid fy)

  *aggregate to the hospital-FY level
  gen i = 1
  collapse (sum) dischnum npac = i, by(condition provid fy)

  * by condition, number & growth rate of referrals to PAC across time

  *compute growth rate in # referrals & # referral sources
  sort condition provid fy
  foreach v of varlist dischnum npac {
    bys condition provid: gen `v'_l = `v'[_n-1]
    gen gr_`v' = 100*(`v'-`v'_l)/`v'_l
  }
  drop *_l

  lab var dischnum "Mean number of referrals to `p'"
  lab var gr_dischnum "Mean growth (%) of referrals to `p'"
  lab var npac "Mean number of `p' referral partners"
  lab var gr_npac "Mean growth (%) of `p' referral partners"

  foreach c in "AMI" "HF" "PN" "HK" {
    preserve
    keep if cond=="`c'"
    drop provid
    order dischnum gr_dischnum npac gr_npac
    bysort fy: outreg2 using `reg'/reftrend1_`p'.xls, append sum(log) eqkeep(N mean) label
    restore
  }
}

*----------------------------
*construct referral HHI & growth in referral HHI at the FY level

foreach p in "HHA" "SNF" {
  use `match_pnlthosp', clear
  keep if pac=="`p'"

  *aggregate to the hospital-PAC provider-FY level
  collapse (sum) dischnum, by(condition provid pacprovid fy)

  * by condition-FY, level & growth rate of referral concentration among PAC providers

  bys condition provid fy: egen tot = sum(dischnum)
  gen refshsq = (dischnum/tot)^2

  *aggregate to the hospital-FY level
  collapse (sum) refhhi = refshsq, by(condition provid fy)
  assert refhhi <= 1

  *compute growth rate in # referrals & # referral sources
  sort condition provid fy
  foreach v of varlist refhhi {
    bys condition provid: gen `v'_l = `v'[_n-1]
    gen gr_`v' = 100*(`v'-`v'_l)/`v'_l
  }
  drop *_l

  lab var refhhi "HHI of Referrals to `p'"
  lab var gr_refhhi "Growth (%) of HHI of Referrals to `p'"

  foreach c in "AMI" "HF" "PN" "HK" {
    preserve
    keep if cond=="`c'"
    drop provid
    order refhhi gr_refhhi
    bysort fy: outreg2 using `reg'/reftrend2_`p'.xls, append sum(log) eqkeep(N mean) label
    restore
  }

  tempfile `p'_refhhi
  save ``p'_refhhi'
}


*identify hospitals penalized for HK
use /ifs/home/kimk13/VI/data/hrrp_penalty, clear
keep if fy==2015
sum err_hk , de
gen HKpnlty = 1 if err_hk < 1
sum err_hk if err_hk >= 1, de
loc pp25 = r(p25)
loc pp50 = r(p25)
loc pp75 = r(p75)
replace HKpnlty = 2 if err_hk >= 1 & err_hk < `pp75'
*replace HKpnlty = 3 if err_hk >= `pp25' & err_hk < `pp75'
*replace HKpnlty = 4 if err_hk >= `pp50' & err_hk < `pp75'
replace HKpnlty = 3 if err_hk >= `pp75'
assert HKpnlty!=.

*lab define status 1 "Not penalized for HK" 2 "Penalized for HK & bottom quartile" 3 "Penalized for HK & middle 2 quartiles" 4 "Penalized for HK & top quartile"
lab define status 1 "Not penalized for HK" 2 "Penalized for HK & bottom 3 quartiles" 3 "Penalized for HK & top quartile", replace
lab val HKpnlty status
tab HKpnlty

keep provid HKpnlty err_hk
duplicates drop
destring provid, replace
tempfile hosp_pnlt
save `hosp_pnlt'


loc p SNF
use ``p'_refhhi', clear
*scatterplot of referral HHI in 2012 vs 2013 by condition across FY
*reshape wide first
reshape wide refhhi gr_refhhi, i(condition provid) j(fy)

forval y=2011/2015 {
    lab var refhhi`y' "Referral HHI in `y'"
}

*merge with 2015 hospital penalty status
merge m:1 provid using `hosp_pnlt', nogen keep(3)


sc refhhi2013 refhhi2012 || sc refhhi2012 refhhi2012, title("`p' Referral HHI in 2013 vs `p' Referral HHI in 2012")
graph export `gph'/`p'_sc_refhhi_12_13.eps, replace

sc refhhi2013 refhhi2011 || sc refhhi2011 refhhi2011, title("`p' Referral HHI in 2013 vs `p' Referral HHI in 2011")
graph export `gph'/`p'_sc_refhhi_11_13.eps, replace

loc c1 "black"
loc c2 "dkgreen"
loc c3 "red"

/*
g c= HKpnlty*255
qui levels of c, loc(cs)

su err_hk, meanonly
g c=round((err_hk-r(min))/(r(max)-r(min))*255)
qui levels of c, loc(cs) */

/* loc g
loc x = 0
foreach c of loc cs {
  loc x = `x' + 1
  loc g `"`g' || sc refhhi2013 refhhi2012 if HKpnlty==`x', ms(S) msize(vsmall) mc("`c' 100 100")"'
} */

loc g
forval x=1/3 {
  loc g `"`g' || sc refhhi2013 refhhi2012 if HKpnlty==`x', ms(S) msize(vsmall) mc(`c`x'')"'
}
tw `g' leg(order(1 "Not penalized for HK" 2 "Penalized for HK & bottom 3 quartiles" 3 "Penalized for HK & top quartile") col(1))
graph export `gph'/`p'_sc_refhhi_12_13_bypnlt.eps, replace

*contour plot takes long
/* tw contour err_hk refhhi2013 refhhi2012
graph export `gph'/test2.eps, replace */
