*construct the hospital sample by applying some sample restriction rules & matching penalized and non-penalized hospitals on baseline characteristics

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use hosp_fy_VI, clear

*drop FY 2008 since only second half of the year available
drop if fy==2008

*drop Maryland hospitals
gen str6 provid_str = string(provid, "%06.0f")
gen st = substr(provid_str, 1,2)
count if st=="21" | st=="80"
*https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf
drop if st=="21"

keep if pac=="SNF"

drop if beds==.
* 18 hospitals

duplicates tag pac cond provid fy, gen(dup)
assert dup==0
drop dup

*restrict to hospital-conditions whose mean # referrals to SNF over 2009-2011 is at least 15
capture drop mdischnum_pac x
bys provid cond: egen x = mean(dischnum_pac) if fy < 2012
bys provid cond: egen mdischnum_pac = max(x)
tab cond fy if mdischnum_pac > 15
sort provid cond fy
list provid cond fy dischnum_pac mdisch in 100/200

*restrict to hospital-conditions whose mean # discharges over 2009-2011 is at least 25
capture drop x
bys provid cond: egen x = mean(dischnum) if fy < 2012
bys provid cond: egen mdischnum = max(x)
tab cond fy if mdischnum > 25

*drop small hospitals whose min # bed < 30
capture drop x
bys provid: egen x = min(beds)
tab cond fy if x >= 30
drop if x < 30

*also restrict to hospitals that appeared throught 2008-2016
capture drop x
gen x = dischnum > 0
bys provid cond: egen sx = sum(x)
tab sx
tab cond fy if sx < 8

keep if mdischnum_pac >= 15 & sx==8 & mdischnum >= 25
drop sx mdischnum_pac mdischnum

tab cond fy

*drop hospital-conditions if # referrals==1 or 0 for any one of the years
capture drop x
bys pac cond provid: egen x = min(dischnum_pac)
tab x
tab provid if x < 2
sort provid cond fy
list provid cond fy dischnum_pac if provid==140034
drop if x < 2

*referral HHI should be missing, not zero, if dischnum_pac = 0
replace refhhi = . if dischnum_pac==0

tempfile tmp
save `tmp'
*--------------------------------
*match penalized hospitals with non-penalized hospitals

*use the sample of hospitals that were penalized & regress the actual penalty rate in 2013 on baseline hospital characteristics
use `tmp', clear
rename err*, upper

capture drop x
gen x = pnltr if fy==2013
bys provid: egen pnltr2013 = max(x)

gen err2013 = .
foreach c in "AMI" "HF" "PN" {
  replace err2013 = ERR_`c' if cond=="`c'" & fy==2013
}
bys provid cond: egen err2013_2 = max(err2013)
drop err2013
rename err2013_2 err2013

foreach v of varlist err2013 pnltr2013 {
  tab fy, summarize(`v')
}

foreach v of varlist beds dischnum mcr_pmt dischnum_pac {
  gen ln_`v' = log(`v' + 1)
}

gen sh_mcr_pmt = mcr_pmt/tot_pat_rev

*use 2009 hosp chars for baseline chars
foreach v of varlist ln_beds ln_dischnum ln_dischnum_pac ln_mcr_pmt teaching urban own* vi_snf sh_mcr_pmt {
  capture drop x
  gen x = `v' if fy==2009
  bys provid cond: egen `v'2009 = max(`v')
}

loc hospchars *2009
keep cond err2013 `hospchars' provid pnltr2013
duplicates drop
count

gen pnltr2013_c = (err2013 > 1) * (err2013-1) * (pnltr2013 > 0)
sum pnltr2013_c

gen penalized2013_c = pnltr2013_c > 0 & pnltr2013_c!=.

tempfile tmp2
save `tmp2'

foreach c in "AMI" "HF" "PN" {
  use `tmp2', clear
  keep if cond=="`c'"

  *get deciles of volume with unique hospital-level obs
  foreach v of varlist ln_beds2009 ln_dischnum2009 ln_dischnum_pac2009 ln_mcr_pmt {
    sum `v', de
    xtile gp_`v' = `v', n(10)
  }

  *regress the penalty rate in 2013 on baseline hospital characteristics using the penalized hospitals for that condition
  *betafit pnlt_rate_c2011 if penalized2013_c==1, mu(`hospchars' )
  loc hospchars teaching2009 urban2009 own*2009 vi_snf2009 i.gp_ln_beds2009 i.gp_ln_dischnum2009 i.gp_ln_dischnum_pac2009 i.gp_ln_mcr_pmt2009
  glm pnltr2013_c `hospchars' if penalized2013_c, family(binomial) link(logit) vce(robust) nolog

  *logit pnlt_rate_c2011 `hospchars' if penalized2013_c==1
  *pscore penalized2013_c `hospchars', pscore(mypscore) numblo(5) level(0.005) logit comsup
  *psmatch2 penalized2013_c, outcome(pcs) pscore(mypscore) neighbor(1)

  *generate predicted penalty shocks for the full sample of hospitals
  predict ppnltr2013_c

  tempfile predicted_`c'
  save `predicted_`c''

  *find the nearest matched hospital based on the predicted penalty shock based on baseline characteristics

  *want to create hospital pair data where a penalized hosp is paired with a non-penalized hosp
  use `predicted_`c'', clear
  keep provid
  duplicates drop
  rename provid provid1
  gen provid2 = provid
  fillin provid1 provid2

  *drop if paired with itself
  drop if _f==0

  rename provid1 provid
  merge m:1 provid using `predicted_`c'', keepusing(penalized2013_c ppnltr2013_c) nogen
  rename ppnltr2013_c ppnltr2013_c1
  rename penalized2013_c treat1
  rename provid provid1
  drop if treat1==0

  rename provid2 provid
  merge m:1 provid using `predicted_`c'', keepusing(penalized2013_c ppnltr2013_c) nogen
  rename ppnltr2013_c ppnltr2013_c2
  rename penalized2013_c treat2
  rename provid provid2
  drop if treat2==1

  *calculate difference
  sort provid1 provid2
  gen diff = abs(ppnltr2013_c1 - ppnltr2013_c2)
  bys provid1: egen mindiff = min(diff)
  gen nearest = diff==mindiff

  *is the pair unique?
  duplicates tag provid2 if nearest==1, gen(dup)
  tab dup
  *no

  keep if nearest==1

  *reshape long
  preserve
  keep provid2
  rename provid2 provid
  tempfile control
  save `control'
  restore

  keep provid1
  rename provid1 provid
  append using `control'

  merge m:1 provid using `predicted_`c''
  *137 hospitals drop out
  keep if _m==3
  drop _m

  *let's calculate the inverse probability of a hospital to be matched as a control group (i.e. nonpenalized)
  gen n = _N
  bys provid: gen smpld = _N
  gen prob = smpld / n
  duplicates drop
  gen invpr = 1/prob

  keep provid invpr cond
  tempfile `c'
  save ``c''
}

*---------------------
*keep only the matched control hospitals among non-penalized ones
clear
foreach c in "AMI" "HF" "PN" {
  append using ``c''
}

merge 1:m provid cond using `tmp'
tab cond fy if _m==2
tab cond fy if _m==3
gen matched = _m==3
drop _m
tab cond fy
tab cond fy if matched==1

compress
save VI_hospsmpl, replace

*---------------------
*compare the full sample with matched sample in terms of size, referral HHI
use VI_hospsmpl, clear
drop if cond=="HK"

preserve
keep if cond=="AMI"
keep provid fy beds match
duplicates drop
tw (kdensity beds) (kdensity beds if match==1)
graph export `gph'/test.eps, replace

bys cond: sum beds dischnum dischnum_pac
bys cond: sum beds dischnum dischnum_pac if match==1
