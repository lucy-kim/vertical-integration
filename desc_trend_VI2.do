* summary statistics for hospital-year level distribution of referral HHI, # referrals a hospital makes to any SNF, # SNF a hospital discharges to, %discharges that go to SNF for each condition-FY

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use hosp_fy_VI, clear
drop if beds==.
* 18 hospitals

duplicates tag pac cond provid fy, gen(dup)
assert dup==0
drop dup

*drop hospital-conditions that have mean # PAC referrals throughout the sample period is 0
bys pac cond provid : egen mm = mean(dischnum_pac)
tab provid  if mm==0
sort pac cond provid fy
list pac cond provid fy dischnum_pac mm if provid==530010
drop if mm==0
*drops hospitals for some conditions but not for other conditions
drop mm

*drop hospitals if in 2008 # index admission discharges < 25 for all 4 conditions
preserve
keep if fy==2008
keep provid cond dischnum
duplicates drop
reshape wide dischnum, i(provid) j(cond) string
codebook dischnum*
foreach v of varlist disch* {
  replace `v' = 0 if `v'==.
}

count if dischnumAMI < 25 & dischnumHF <25 & dischnumPN < 25
*84 hospitals
count if dischnumAMI < 25 & dischnumHF <25 & dischnumPN < 25 & dischnumHK < 25
*38 hospitals
drop if dischnumAMI < 25 & dischnumHF <25 & dischnumPN < 25 & dischnumHK < 25
foreach v of varlist disch* {
  rename `v' `v'08
}
tempfile hosp_keep
save `hosp_keep'
restore

merge m:1 provid using `hosp_keep', nogen keep(3)

*create the share of PAC referrals out of total # hospital discharges
gen shref = dischnum_pac / dischnum
sum shref
*assert shref >= 0 & shref <= 1
*there are small # obs with share of referrals > 1

tempfile an
save `an'
*----------------------------------------
*get number of SNFs a hospital discharges to for each condition-FY
use PACreferral_tchpm, clear
keep if pac=="SNF"
*drop if pac==""

collapse (sum) dischnum_pac, by(condition provid fy pacprovid)
list if provid==10005 & fy==2011 & condition=="HK"

gen i = 1 if pacprovid!=. & dischnum_pac!=0
collapse (sum) nsnf = i, by(cond provid fy)

tempfile nsnf
save `nsnf'

*get number of SNFs a hospital discharges to for 3 conditions & each FY
use PACreferral_tchpm, clear
keep if pac=="SNF"
drop if cond=="HK"
*drop if pac==""

collapse (sum) dischnum_pac, by(provid fy pacprovid)

gen i = 1 if pacprovid!=. & dischnum_pac!=0
collapse (sum) nsnf_c3 = i, by(provid fy)

tempfile nsnf_c3
save `nsnf_c3'

use `an', clear
keep if pac=="SNF"

merge 1:1 cond provid fy using `nsnf', keep(1 3) nogen
merge m:1 provid fy using `nsnf_c3', keep(1 3) nogen
count if shref > 1
*2924 / 87758 (3%) obs have # referrals > # index admissions
drop if shref > 1

tempfile an2
save `an2'
*----------------------------------------
*summary stats by condition & FY
use `an2', clear

*for hospital-year level distribution of referral HHI, # referrals a hospital makes to any SNF, # SNF a hospital discharges to, %discharges that go to SNF for each condition-FY

keep dischnum dischnum_pac shref nsnf refhhi cond fy
order dischnum dischnum_pac shref nsnf refhhi cond fy
replace shref = 100*shref

lab var dischnum_pac "# referrals from a hospital to any SNF"
lab var dischnum "Total # Discharges from a hospital"
lab var shref "% discharges a hospital refers to any SNF"
lab var nsnf "# SNFs a hospital refers to"
lab var refhhi "SNF referral HHI"

gen cond2 = 1 if condition=="AMI"
replace cond2 = 2 if condition=="HF"
replace cond2 = 3 if condition=="PN"
replace cond2 = 4 if condition=="HK"
lab def cl 1 "AMI" 2 "HF" 3 "PN" 4 "HK"
lab val cond2 cl

bysort cond2 fy: outreg2 using `reg'/summstats.xls, replace sum(detail) eqkeep(N mean sd min max p10 p25 p50 p75 p90) label

*histogram by pre vs post penalty (=>2012)
drop cond2

gen post = fy >= 2012
lab var post "FY 2012 or later"
lab define lpost 0 "Pre 2012" 1 "2012 or later"
lab val post lpost

replace post = fy >=2014 if condition=="HK"

foreach c in "AMI" "HF" "PN" {
  foreach v of varlist dischnum dischnum_pac shref nsnf refhhi {
    sum `v' if cond=="`c'"
    loc w = (r(max) - r(min))/20

    tw hist `v' if cond=="`c'", by(post) frac width(`w') title(`c')
    graph export `gph'/`c'_`v'.eps, replace
  }
}

foreach c in "HK" {
  lab var post "FY 2014 or later"
  lab define lpost 0 "Pre 2014" 1 "2014 or later", replace
  lab val post lpost

  foreach v of varlist dischnum dischnum_pac shref nsnf refhhi {
    sum `v' if cond=="`c'"
    loc w = (r(max) - r(min))/20

    tw hist `v' if cond=="`c'", by(post) frac width(`w') title(`c')
    graph export `gph'/`c'_`v'.eps, replace
  }
}

*----------------------------------------
* for all 3 conditions together
use `an2', clear
drop if cond=="HK"

collapse (sum) dischnum dischnum_pac (mean) nsnf_c3 refhhi3, by(provid fy)

gen shref = dischnum_pac / dischnum
replace shref = 100*shref

order dischnum dischnum_pac shref nsnf refhhi  fy
drop provid

lab var dischnum_pac "# referrals from a hospital to any SNF"
lab var dischnum "Total # Discharges from a hospital"
lab var shref "% discharges a hospital refers to any SNF"
lab var nsnf "# SNFs a hospital refers to"
lab var refhhi "SNF referral HHI"

bysort fy: outreg2 using `reg'/summstats2.xls, replace sum(detail) eqkeep(N mean sd min max p10 p25 p50 p75 p90) label

*histogram by pre vs post penalty (=>2012)
gen post = fy >= 2012
lab var post "FY 2012 or later"
lab define lpost 0 "Pre 2012" 1 "2012 or later"
lab val post lpost

foreach v of varlist dischnum dischnum_pac shref nsnf refhhi {
  sum `v'
  loc w = (r(max) - r(min))/20

  tw hist `v', by(post) frac width(`w') title("AMI, HF, PN")
  graph export `gph'/c3_`v'.eps, replace
}


*----------------------------------------
* scatter plot matrix of referral HHI, # referrals a hospital makes to any SNF, # SNF a hospital discharges to, %discharges that go to SNF across conditions by pre vs post penalty
use `an2', clear

*for hospital-year level distribution of referral HHI, # referrals a hospital makes to any SNF, # SNF a hospital discharges to, %discharges that go to SNF for each condition-FY

keep dischnum dischnum_pac shref nsnf refhhi cond fy provid
order dischnum dischnum_pac shref nsnf refhhi cond fy provid
replace shref = 100*shref

*reshape wide conditions
reshape wide dischnum dischnum_pac shref nsnf refhhi , i(provid fy) j(cond) string

gen post = fy >= 2012
lab var post "FY 2012 or later"
lab define lpost 0 "Pre 2012" 1 "2012 or later"
lab val post lpost
*replace post = fy >=2014 if condition=="HK"

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum_pac`c' "`c'"
  lab var dischnum`c' "`c'"
  lab var shref`c' "`c'"
  lab var nsnf`c' "`c'"
  lab var refhhi`c' "`c'"
}

loc ldischnum_pac "# referrals from a hospital to any SNF"
loc ldischnum "Total # Discharges from a hospital"
loc lshref "% discharges a hospital refers to any SNF"
loc lnsnf "# SNFs a hospital refers to"
loc lrefhhi "SNF referral HHI"

foreach v in "dischnum" "dischnum_pac" "shref" "nsnf" "refhhi" {
  graph matrix `v'AMI `v'HF `v'PN `v'HK, by(post) half ti(`l`v'') ms(p)
  *maxes(xsc(r(0 `r(max)')) ysc(r(0 `r(max)')))
  graph export `gph'/scm_`v'.eps, replace
}

*----------------------------------------
*summary stats of hospital characteristics by FY
use `an', clear
collapse (mean) beds own* teaching urban uncomp dissh vi* hsahhi_SNF_* ses_score , by(provid fy)

lab var beds "# Beds"
lab var own_fp "For-profit ownership"
lab var own_np "Not-for-profit ownership"
lab var own_gv "Government ownership"
lab var teaching "Teaching"
lab var urban "Urban"
lab var uncomp "Uncompensated care"
lab var dissh "Receive DSH payment"
lab var vi_snf "Formally own SNF"
lab var vi_hha "Formally own HHA"
lab var vi_irf "Formally own IRF"
lab var hsahhi_SNF_AMI "HSA-level SNF market HHI for AMI"
lab var hsahhi_SNF_HF "HSA-level SNF market HHI for HF"
lab var hsahhi_SNF_HK "HSA-level SNF market HHI for HK"
lab var hsahhi_SNF_PN "HSA-level SNF market HHI for PN"
lab var ses_score "Mean SES score"

drop provid

bysort fy: outreg2 using `reg'/summstats2.xls, replace sum(detail) eqkeep(N mean sd min max p10 p25 p50 p75 p90) label
