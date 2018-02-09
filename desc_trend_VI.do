*descriptive analysis of the trend of vertical integration over time
loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

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
forval x = 2011/2016 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert fy!=.

tempfile match
save `match'

*restrict to hospitals that are subject to HRRP penalty; keep only hospitals that are penalized in FY 2015
use `dta'/hrrp_penalty, clear
keep if fy ==2013
keep provid
duplicates drop
destring provid, replace

tempfile hosp_keep
save `hosp_keep'

use `match', clear

merge m:1 provid using `hosp_keep'

*for 2013-2015, about 240-246 hospitals in the penalty data unmatched (_m=2) - drop these hosps
gen x = _m==2
bys provid: egen bad = max(x)
drop if bad==1
drop x bad
*fy 2016-2018 unmatched - fine b/c i don't have them
drop _m

*rename vars before merging with index admissions data
foreach v of varlist dischnum-samh90 {
  rename `v' `v'_pac
}
compress
save match_pnlthosp, replace
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

* 1) create hospital-time level data

*create hospital-month level data
use match_pnlthosp, clear

*total # PAC referrals for each hospital-month
collapse (sum) *_pac, by(provid dischyear dischmth fy qtr pac cond)

*merge with hospital-month level # discharges, readmits, etc.
merge m:1 condition provid dischyear dischmth using `index2', keep(3) nogen

*merge with hosp chars data
merge m:1 provid fy using `costreport', nogen keep(1 3)

* exclude hospitals with small # beds
preserve
collapse (min) beds, by(provid)
*tab beds

*tag beds < 30
gen smallbeds = beds < 30
tab smallbeds
*93 hospitals

drop beds
tempfile smallbeds
save `smallbeds'
restore

merge m:1 provid using `smallbeds', nogen
drop if smallbeds==1
drop smallbeds

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
  merge m:1 `g' fy using `dta'/`g0'hhi, keep(1 3)
  assert `g'==. | fy < 2011 if _m==1
  drop _merge
}

compress
save referral_hq, replace

outsheet using referral_hq.csv, comma names replace

/* keep dischyear pac cond
duplicates drop
sort pac cond dischyear
expand 12
bys pac cond dischyear: gen dischmth = _n
drop if dischyear==2010 & dischmth < 7
drop if dischyear==2015 & dischmth > 6

merge 1:m pac cond dischyear dischmth using `referral_hm' */

*-------------------------
* 2) analyze the number of PAC referrals by each hospital in each quarter
*histogram of the quarterly # referrals / % referrals to PAC by PAC type & condition

use referral_hq, clear
foreach pac in "HHA" "SNF" {
  tw hist dischnum_pac if pac=="`pac'", discrete by(cond) frac xtitle("Quarterly number of referrals to `pac' by condition", size(medium)) ysc(r(0 0.4))
  graph export `gph'/dischnum_pac_`pac'.eps, replace
}

use referral_hq, clear
foreach pac in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    di ""
    di "`pac' & `c'---------------"
    tab dischnum_pac if pac=="`pac'" & cond=="`c'"
  }
}

*Distribution of the quarterly number of referrals across hospital-quarter pairs by condition X PAC type
use referral_hq, clear
keep pac cond dischnum_pac provid dischyear qtr
gen i = 1
collapse (sum) i, by(dischnum_pac condition pac dischyear qtr)
collapse (sum) i, by(dischnum_pac condition pac)
bys condition pac: egen tot = sum(i)
gen pct_hq = 100* i / tot
sort pac cond dischnum
tempfile tmp
save `tmp'

use `tmp', clear
drop i
gen num = 1 if dischnum_pac==1
replace num = 2 if dischnum_pac > 1 & dischnum_pac < 6
replace num = 3 if dischnum_pac > 5 & dischnum_pac < 11
replace num = 4 if dischnum_pac > 10 & dischnum_pac < 21
replace num = 5 if dischnum_pac > 20 & dischnum_pac < 31
replace num = 6 if dischnum_pac > 30
assert num!=.
lab define ll 1 "1" 2 "2-5" 3 "6-10" 4 "11-20" 5 "21-30" 6 "31+"
lab val num ll
collapse (sum) pct_hq, by(cond pac num tot)
list
rename pct_hq pct_hq_
rename tot tot_
reshape wide pct_hq tot_, i(num cond) j(pac) string
reshape wide pct_hq* tot*, i(num) j(condition) string
preserve
drop tot*
outsheet using `reg'/dist_ref_hq.csv, replace comma names
restore
*--------------------------

* 3) analyze the share of PAC referrals out of total # hospital discharges by each hospital in each quarter

use referral_hq, clear

*create the share of PAC referrals out of total # hospital discharges
gen shref = dischnum_pac / dischnum
sum shref
assert shref >= 0 & shref <= 1

tempfile an
save `an'

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use `an', clear

    gen yqd = yq(dischyear, qtr)
    format yqd %tq

    loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
    areg shref `x_ht' i.qtr i.yqd if pac=="`p'" & cond=="`c'", absorb(provid) cluster(provid)

    *save coefficients to plot
    tempfile coef_`p'_`c'
    parmest, saving(`coef_`p'_`c'')

    use `coef_`p'_`c'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "yqd")
    split parm, p(".")
    drop if estimate==0

    gen a = mod(_n+3, 4)
    replace a = 4 if a==0
    rename a qtr
    gen yr = 2011 + floor((_n-2)/4)

    keep estimate min95 max95 yr qtr
    rename estimate coef
    gen cond = "`c'"
    tempfile coef_`p'_`c'2
    save `coef_`p'_`c'2'
  }

  use `coef_`p'_AMI2', clear
  foreach c in "HF" "PN" "HK" {
    append using `coef_`p'_`c'2'
  }
  compress
  outsheet using `reg'/coef_shref_`p'_qtradj.csv, replace comma names
}

*just plot the raw data

foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  keep if pac=="`p'"

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh
  replace refhhi = refhhi*100

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean % hospital discharges to `p', size(small))
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of share of hospital discharges to `p', size(small))
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*--------------------------
* 4) analyze the PAC referral concentration in each hospital in each quarter

*regression using interaction of penalty status & FY dummies to plot DiD coefficients

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use referral_hq, clear

    gen yqd = yq(dischyear, qtr)
    format yqd %tq

    loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
    areg refhhi `x_ht' i.qtr i.yqd if pac=="`p'" & cond=="`c'" & dischnum_pac!=1, absorb(provid) cluster(provid)

    *save coefficients to plot
    tempfile coef_`p'_`c'
    parmest, saving(`coef_`p'_`c'')

    use `coef_`p'_`c'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "yqd")
    split parm, p(".")
    drop if estimate==0

    gen a = mod(_n+3, 4)
    replace a = 4 if a==0
    rename a qtr
    gen yr = 2011 + floor((_n-2)/4)

    keep estimate min95 max95 yr qtr
    rename estimate coef
    gen cond = "`c'"
    tempfile coef_`p'_`c'2
    save `coef_`p'_`c'2'
  }

  use `coef_`p'_AMI2', clear
  foreach c in "HF" "PN" "HK" {
    append using `coef_`p'_`c'2'
  }
  compress
  outsheet using `reg'/coef_VI_`p'_qtradj2.csv, replace comma names
}

*just plot the raw data

foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  keep if pac=="`p'"

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean `p' referral concentration, size(medium))
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of `p' referral concentration, size(medium))
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*only among large hospitals
foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  bys size: sum beds
  keep if pac=="`p'" & size==3 & dischnum_pac!=1

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean `p' referral concentration, size(medium)) subti(Large hospitals with 500+ beds)
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of `p' referral concentration, size(medium)) subti(Large hospitals with 500+ beds)
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*--------------------------
*plot referral HHI
use referral_hq, clear
gen yqd = yq(dischyear, qtr)
tab yqd
*202 - 221
format yqd %tq

loc pac HHA
loc c AMI
binscatter refhhi yqd if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1, linetype(qfit) rd(206 210 214 218) absorb(provid) controls(qtr i.size own* ses_score teaching urban) ysc(r(0 1)) xlab(202(1)221)

sc refhhi yqd if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1, connect()
graph export `gph'/test.eps, replace

foreach pac in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
  }
}



*--------------------------
*plot referral HHI density curves across FY
use referral_hq, clear
gen yqd = yq(dischyear, qtr)
tab yqd
*202 - 221
format yqd %tq

loc c "AMI"
loc pac "SNF"

preserve
keep if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1

kdensity refhhi , nograph gen(x fx)
forval y = 2011/2015 {
  kdensity refhhi if fy==`y' , nograph gen(fx`y') at(x)
  lab var fx`y' "`y'"
}
line fx2011 fx2012 fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle(`pac' referral concentration in each hospital-quarter for `c')
graph export `gph'/test.eps, replace
restore

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
