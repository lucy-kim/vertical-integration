*Examine whether condition-specific penalty pressure changes referral concentration

*packages for local linear regression
net install ivqte, from("https://sites.google.com/site/blaisemelly/")
net install st0026_2, from(http://www.stata-journal.com/software/sj5-3)
ssc install moremata

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

*create size of Medicare payment for each hospital-fy t where t = 2012, 2013, ... using the sum of Medicare payment for the period {t-3, t-2, t-1}
capture drop x
foreach v of varlist mcr_pmt tot_pat_rev {
  capture drop x_`v'
  gen x_`v' = .
  forval t = 2009 /2016 {
    loc fy = `t'-3
    loc ly = `t'-1
    bys cond provid: egen x = sum(`v') if fy >= `fy' & fy <= `ly'
    bys cond provid: egen mx = max(x)
    replace x_`v' = mx if fy==`t'
    drop x mx
  }
  assert x_`v'!=. if fy >= 2011
}
rename x_mcr_pmt deptsize
rename x_tot_pat_rev totpatrev

sort cond provid fy
list cond provid fy mcr_pmt tot_pat_rev deptsize totpatrev in 1/30

*get risk-standardized readmissions rate (RSRR) for each t := max[0, ERR-1] where ERR corresponds to ERR for t+1 based on {t-3,t-2,t-1}, e.g. RSRR for 2012 should be ERR for 2013 which is based on the base period 2009-2011

*let's remerge with penalty rate data after reshape wide the penalty rate
preserve
use `dta'/hrrp_penalty, clear
reshape wide payadjr pnltr n_* err_* , i(provid) j(fy)
tempfile penalty
save `penalty'
restore

drop payadjr-pnltr
merge m:1 provid using `penalty', keep(1 3) nogen


foreach c in "ami" "hf" "hk" "pn" {
  loc v err_`c'
  capture drop `v'
  gen `v' = .
  gen `v'2 = .
  gen penalized_`c'_t2 = .
  gen act_pnltr_`c'_t2 = .

  * in 2012, look at own performance during {2009-2011} (used for 2013 penalty)
  forval t = 2011/2016 {
    loc t2 =`t'+1
    di `t2'
    gen pos = `v'`t2' > 1 & pnltr`t2' > 0
    replace `v' = (`v'`t2'-1)*pos if fy==`t'
    replace `v'2 = (`v'`t2'-1) if fy==`t'
    replace penalized_`c'_t2 = pos if fy==`t'
    replace act_pnltr_`c'_t2 = pnltr`t2' if fy==`t'
    drop pos
  }
}
rename err_ami pnlt_AMI_rate
rename err_pn pnlt_PN_rate
rename err_hf pnlt_HF_rate
rename err_hk pnlt_HK_rate

rename err_ami2 pnlt_AMI_rate2
rename err_pn2 pnlt_PN_rate2
rename err_hf2 pnlt_HF_rate2
rename err_hk2 pnlt_HK_rate2

sort cond provid fy
list cond provid fy deptsize totpatrev pnlt_AMI_rate in 1/30
list cond provid fy pnlt_AMI_rate err_ami* n_ami* in 1/10

sort provid fy cond
list provid fy cond deptsize totpatrev pnlt_AMI_rate in 1/30

*calculate the dollar size of penalty for each condition
gen pnlt_rate_c = .
gen pnlt_rate_c2 = .
foreach c in "AMI" "HF" "HK" "PN" {
  replace pnlt_rate_c = pnlt_`c'_rate if cond=="`c'"
  drop pnlt_`c'_rate

  replace pnlt_rate_c2 = pnlt_`c'_rate2 if cond=="`c'"
  drop pnlt_`c'_rate2
}
gen pnlt_dollar_c = deptsize * pnlt_rate_c

*gen sh_pmt = (mcr_pmt/tot_pat_rev)

compress
save penalty_VI_bycond, replace

tempfile an
save `an'
*--------------------------------

* regress the referral concentration on hospital FE, penalty pressure, fiscal year FE
use penalty_VI_bycond, clear
loc int 2011

drop if cond=="HK"


foreach v of varlist pnlt_dollar_c pnlt_rate_c pnlt_rate_c2 {
  replace `v' = 0 if fy < `int'
}

* use the 2012 readmissions penalty pressure as influence of penalty
foreach v of varlist pnlt_rate_c pnlt_rate_c2 pnlt_dollar_c deptsize {
  capture drop `v'`int'
  capture drop x
  gen x = `v' if fy==`int'
  bys cond provid: egen `v'`int' = max(x)
}

gen penalized2013 = pnltr2013 > 0
bys penalized2013: sum beds dischnum dischnum_pac


foreach v of varlist dischnum_pac pnlt_dollar_c* deptsize* {
  gen ln_`v' = ln(`v' + 1)
}

xi i.fy
gen post = fy >=`int'

gen pnlt_rate_cXlndeptsize`int' = pnlt_rate_c`int' * ln_deptsize`int'
gen pnlt_rate_c2Xlndeptsize`int' = pnlt_rate_c2`int' * ln_deptsize`int'

tab fy , summarize(pnlt_rate_cXlndeptsize`int')
tab fy , summarize(pnlt_rate_c2Xlndeptsize`int')

lab var pnlt_rate_c`int' "Predicted `int' excess readmission rate"
lab var pnlt_rate_c2`int' "Predicted `int' risk-adjusted readmission rate"
lab var ln_deptsize`int' "Predicted `int' Ln(Medicare revenues for condition)"
lab var pnlt_rate_cXlndeptsize`int' "Predicted `int' risk-adjusted readmission rate X Ln(Medicare revenues)"
lab var ln_pnlt_dollar_c`int' "Predicted `int' Ln(expected penalty ($))"
loc lln_pnlt_dollar_c`int' "Ln(Predicted `int' penalty ($))"
loc lpnlt_rate_c`int' "Predicted `int' excess readmission rate"
loc lln_deptsize`int' "Predicted `int' Ln(Medicare revenues for condition)"
loc lpnlt_rate_cXlndeptsize`int' "Predicted `int' excess readmission rate X Ln(Medicare revenues for condition)"
loc lpnlt_rate_c2`int' "Predicted `int' risk-adjusted readmission rate"
loc lln_deptsize`int' "Predicted `int' Ln(Medicare revenues for condition)"
loc lpnlt_rate_c2Xlndeptsize`int' "Predicted `int' risk-adjusted readmission rate X Ln(Medicare revenues for condition)"

*cross-sectional variation across hospitals by `int' penalty pressure based on 2009-2011 performance
foreach v of varlist ln_pnlt_dollar_c`int' pnlt_rate_c`int' pnlt_rate_c2`int' ln_deptsize`int' pnlt_rate_cXlndeptsize`int' pnlt_rate_c2Xlndeptsize`int' {
  tab fy, summarize(`v')

  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  lab var `v'Xpost "`l`v'' X Post"

  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
    lab var `v'X`t' "`l`v'' X `t'"
  }
}

gen nonbw = dischnum - white - black
gen rblack = black / dischnum
gen rnonbw = nonbw / dischnum
foreach v of varlist ses_score rblack rnonbw {
  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
  }
}

* create share of SNF referrals outcome
gen shref = dischnum_pac/dischnum

*create de
preserve
keep provid fy beds dischnum
duplicates drop
sum beds dischnum, de
restore

tempfile an
save `an'

*--------------------------------
*match penalized hospitals with non-penalized hospitals
*use the sample of hospitals that were penalized & regress the penalty rate in 2013 on baseline hospital characteristics
use `an', clear

foreach v of varlist beds dischnum {
  gen ln_`v' = log(`v' + 1)
}

*use 2009 hosp chars for baseline chars
foreach v of varlist ln_beds ln_dischnum ln_dischnum_pac teaching urban own* vi_snf {
  capture drop x
  gen x = `v' if fy==2009
  bys provid cond: egen `v'2009 = max(`v')
}

loc hospchars *2009
keep cond pnlt_rate_c2011 `hospchars' provid penalized2013
duplicates drop
count

tempfile an2
save `an2'

foreach c in "AMI" "HF" "PN" {
  use `an2', clear
  keep if cond=="`c'"

  *get deciles of volume with unique hospital-level obs
  foreach v of varlist ln_beds2009 ln_dischnum2009 ln_dischnum_pac2009 {
    sum `v', de
    xtile gp_`v' = `v', n(10)
  }

  gen penalized_`c' = pnlt_rate_c2011 > 0 & penalized2013 > 0

  *regress the penalty rate in 2013 on baseline hospital characteristics using the penalized hospitals for that condition
  *betafit pnlt_rate_c2011 if penalized_`c'==1, mu(`hospchars' )
  loc hospchars teaching2009 urban2009 own*2009 vi_snf2009 i.gp_ln_beds2009 i.gp_ln_dischnum2009 i.gp_ln_dischnum_pac2009
  glm pnlt_rate_c2011 `hospchars' if penalized_`c'==1, family(binomial) link(logit) vce(robust) nolog

  *logit pnlt_rate_c2011 `hospchars' if penalized_`c'==1
  *pscore penalized_`c' `hospchars', pscore(mypscore) numblo(5) level(0.005) logit comsup
  *psmatch2 penalized_`c', outcome(pcs) pscore(mypscore) neighbor(1)

  *generate predicted penalty shocks for the full sample of hospitals
  predict ppnlt_rate_c2011_`c'

  tempfile predicted
  save `predicted'

  *find the nearest matched hospital based on the predicted penalty shock based on baseline characteristics

  *want to create hospital pair data where a penalized hosp is paired with a non-penalized hosp
  use `predicted', clear
  keep provid
  duplicates drop
  rename provid provid1
  gen provid2 = provid
  fillin provid1 provid2

  *drop if paired with itself
  drop if _f==0

  rename provid1 provid
  merge m:1 provid using `predicted', keepusing(penalized_`c' ppnlt_rate_c2011_`c') nogen
  rename ppnlt_rate_c2011_`c' ppnlt_rate_c2011_`c'1
  rename penalized_`c' treat1
  rename provid provid1
  drop if treat1==0

  rename provid2 provid
  merge m:1 provid using `predicted', keepusing(penalized_`c' ppnlt_rate_c2011_`c') nogen
  rename ppnlt_rate_c2011_`c' ppnlt_rate_c2011_`c'2
  rename penalized_`c' treat2
  rename provid provid2
  drop if treat2==1

  *calculate difference
  sort provid1 provid2
  gen diff = abs(ppnlt_rate_c2011_`c'1 - ppnlt_rate_c2011_`c'2)
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

  merge m:1 provid using `predicted'
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

merge 1:m provid cond using `an'
tab cond fy if _m==2
keep if _m==3
drop _m

compress
save penalty_VI_bycond2, replace

*--------------------------------
* create 2 more outcomes: % of previous SNFs getting referrals, % HRR-level SNFs getting referrals,

use penalty_VI_bycond2, clear

use PACreferral_tchpm, clear
keep if

*---------------------------
*use the cross-sectional variation across hospitals by `int' penalty pressure based on 2009-2011 performance

*use pre-HRRP excess readmission rate & excess readmission rate X condition revenue size as penalty pressure
foreach y of varlist refhhi {
  loc file ols_`y'1
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' pnlt_rate_c`int'X20* i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    areg `y' pnlt_rate_c`int'X20* ln_deptsize`int'X20* pnlt_rate_cXlndeptsize`int'X20* i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  * use post dummy
  loc file ols_`y'1.2
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' pnlt_rate_c`int'Xpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    areg `y' pnlt_rate_c`int'Xpost ln_deptsize`int'Xpost pnlt_rate_cXlndeptsize`int'Xpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  *use penalty in dollars
  loc file ols_`y'2
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' ln_pnlt_dollar_c`int'X20* i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    areg `y' ln_pnlt_dollar_c`int'X20* i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  * use post dummy
  loc file ols_`y'2.2
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' ln_pnlt_dollar_c`int'Xpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    areg `y' ln_pnlt_dollar_c`int'Xpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }
}



*---------------------------
*use both cross-hospital & within-hospital cross-time variation in penalty pressure
use `an', clear
foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c ln_deptsize {
  replace `v' = 0 if fy < `int'
  tab fy, summarize(`v')
}

gen pnlt_rate_cXlndeptsize =  pnlt_rate_c * ln_deptsize
gen ln_pnlt_dollar_cXlndeptsize =  ln_pnlt_dollar_c * ln_deptsize

lab var ln_pnlt_dollar_cXlndeptsize "Predicted penalty ($) X Ln(Medicare revenues for condition)"
lab var pnlt_rate_cXlndeptsize "Predicted penalty rate X Ln(Medicare revenues for condition)"
lab var pnlt_rate_c "Predicted penalty rate"
lab var pnlt_rate_c2 "Predicted risk-adjusted readmission rate"
lab var ln_deptsize "Predicted Ln(Medicare revenues for condition)"
lab var pnlt_rate_cXlndeptsize "Predicted risk-adjusted readmission rate X Ln(Medicare revenues for condition)"
lab var ln_pnlt_dollar_c "Ln(Predicted penalty ($))"

loc lln_pnlt_dollar_c "Ln(Predicted penalty ($))"
loc lpnlt_rate_c "Predicted excess readmission rate"
loc lln_deptsize "Predicted Ln(Medicare revenues for condition)"
loc lpnlt_rate_cXlndeptsize "Predicted excess readmission rate X Ln(Medicare revenues for condition)"
loc lpnlt_rate_c2 "Predicted risk-adjusted readmission rate"
loc lln_deptsize "Predicted Ln(Medicare revenues for condition)"
loc lpnlt_rate_c2Xlndeptsize "Predicted risk-adjusted readmission rate X Ln(Medicare revenues for condition)"
loc lln_pnlt_dollar_cXlndeptsize "Predicted penalty ($) X Ln(Medicare revenues for condition)"


foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c ln_deptsize pnlt_rate_cXlndeptsize ln_pnlt_dollar_cXlndeptsize {
  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  lab var `v'Xpost "`l`v'' X Post"

  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
    lab var `v'X`t' "`l`v'' X `t'"
  }
}
tab fy, summarize(ln_pnlt_dollar_c)


*use pre-HRRP excess readmission rate & excess readmission rate X condition revenue size as penalty pressure
foreach y of varlist refhhi shref {
  loc file ols_`y'3
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    loc xvar pnlt_rate_cX`int' pnlt_rate_cX2013 pnlt_rate_cX2014 pnlt_rate_cX2015 pnlt_rate_cX2016
    areg `y' `xvar' i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    loc xvar pnlt_rate_cX2012 pnlt_rate_cX2013 pnlt_rate_cX2014 pnlt_rate_cX2015 pnlt_rate_cX2016 ln_deptsizeX2012 ln_deptsizeX2013 ln_deptsizeX2014 ln_deptsizeX2015 ln_deptsizeX2016 pnlt_rate_cXlndeptsizeX2012 pnlt_rate_cXlndeptsizeX2013 pnlt_rate_cXlndeptsizeX2014 pnlt_rate_cXlndeptsizeX2015 pnlt_rate_cXlndeptsizeX2016
    areg `y' `xvar' i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  * use post dummy
  loc file ols_`y'3.2
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' pnlt_rate_cXpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)

    areg `y' pnlt_rate_cXpost ln_deptsizeXpost pnlt_rate_cXlndeptsizeXpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  *use penalty in dollars
  loc file ols_`y'4
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    loc xvar ln_pnlt_dollar_cX2012 ln_pnlt_dollar_cX2013 ln_pnlt_dollar_cX2014 ln_pnlt_dollar_cX2015 ln_pnlt_dollar_cX2016
    areg `y' `xvar' i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }

  * use post dummy
  loc file ols_`y'4.2
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c')"
    loc sp i.size ln_dischnum_pac own_* urban teaching vi_snf hrrhhi_SNF_`c'

    areg `y' ln_pnlt_dollar_cXpost i.fy `sp' if cond=="`c'" , absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.4f `r(mean)'

    `out' addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(4) fmt(fc)
  }
}

*---------



*first stage
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c {
    loc y `v'Xpost
    areg `y' ses_scoreXpost rblackXpost i.fy i.size ln_dischnum_pac own_* urban teaching vi_snf if cond=="`c'" & fy < 2015, cluster(provid) absorb(provid)
    test
  }
}

loc y refhhi
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c {
    ivreg2 `y' i.fy i.size ln_dischnum_pac own_* urban teaching vi_snf i.provid (`v'X20* = ses_scoreX20* rblackX20*) if cond=="`c'" & fy < 2015, cluster(provid) first partial(i.provid)
  }
}


loc nend 3

*endogenous set
loc end pnlt_rate_cX20*
loc ev1 pnlt_rate_cX2012
loc ev2 pnlt_rate_cX2013
loc ev3 pnlt_rate_cX2014

*IV set
loc iv ses_scoreX20* rblackX20*
*loc iv worked altabsent_short altvac altabsent_long worked_oo

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
foreach ev of varlist `end' {
  loc fsr_`ev'
}
*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc sp1 i.fy i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' i.fy `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

    estimates dir
    estimates save iv_e_n`n', replace

    mat fstat_n`n' = e(first)

    *for each first stage, save in a separate file
    forval j = 1/`nend' {
      estadd scalar fs_`ev`j'' = fstat_n`n'[4,`j'] : f_n`n'_`ev`j''
    }

    foreach ev of varlist `end' {
      loc fsr_`ev' `fsr_`ev'' f_n`n'_`ev'
    }
    loc ssr `ssr' y_n`n'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      reg `ev' `iv' `sp`n'', cluster(offid_nu)
      estadd scalar fr2_`ev' = `e(r2)' : f_n`n'_`ev'
    }
  }
  *for each first stage, save in a separate file
  foreach ev of varlist `end' {
    loc file iv1s_`ev'_prate_`c'
    esttab `fsr_`ev'' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(4)) se(par fmt(4))) keep(`iv') order(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }

  *save 2nd stage reg
  loc file iv2s_prate_`c'
  esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(4)) se(par fmt(4))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)
}


*use size of penalty in dollars

*endogenous set
loc end ln_pnlt_dollar_cX20*
loc ev1 ln_pnlt_dollar_cX2012
loc ev2 ln_pnlt_dollar_cX2013
loc ev3 ln_pnlt_dollar_cX2014

*IV set
loc iv ses_scoreX20* rblackX20*
*loc iv worked altabsent_short altvac altabsent_long worked_oo

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
foreach ev of varlist `end' {
  loc fsr_`ev'
}
*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc sp1 i.fy i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' i.fy `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

    estimates dir
    estimates save iv_e_n`n', replace

    mat fstat_n`n' = e(first)

    *for each first stage, save in a separate file
    forval j = 1/`nend' {
      estadd scalar fs_`ev`j'' = fstat_n`n'[4,`j'] : f_n`n'_`ev`j''
    }

    foreach ev of varlist `end' {
      loc fsr_`ev' `fsr_`ev'' f_n`n'_`ev'
    }
    loc ssr `ssr' y_n`n'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      reg `ev' `iv' `sp`n'', cluster(offid_nu)
      estadd scalar fr2_`ev' = `e(r2)' : f_n`n'_`ev'
    }
  }
  *for each first stage, save in a separate file
  foreach ev of varlist `end' {
    loc file iv1s_`ev'_pdollar_`c'
    esttab `fsr_`ev'' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(4)) se(par fmt(4))) keep(`iv') order(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }

  *save 2nd stage reg
  loc file iv2s_pdollar_`c'
  esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(4)) se(par fmt(4))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)
}



/*
*--------------------------------
*construct cross-sectional variation  penalty pressure for each hospital-condition using 2009-2011 in 4 ways
*1) Atul (2017): f(r_t-3, r_t-2, r_t-1) where r is raw readmission rate
*2) my proposal: f(r_t-3, r_t-2, r_t-1, sh_t-3, sh_t-2, sh_t-3) where sh is share of Medicare payment
*3) ERR in 2013
*4) overall penalty rate in 2013

forval t=2014/2018 {
loc t1 = `t'-5
loc t2 = `t'-3

*get raw readmission rates during t-3 - t-1
use `an2', clear
keep if fy >= `t1' & fy <= `t2'
tab fy

collapse (sum) read30 dischnum mcr_pmt tot_pat_rev, by(cond provid)
gen rra = read30/dischnum

*create share of Medicare payment for each hospital-fy-condition
gen sh_pmt = (mcr_pmt/tot_pat_rev)

keep provid rra sh_pmt cond

tempfile rra
save `rra'

*get penalty status in t + 2
use `dta'/hrrp_penalty, clear
keep if fy==`t'
keep pnltr provid err_ami err_hf err_pn
duplicates drop
gen fy=`t'

reshape long err_, i(provid) j(condition) string
replace cond = upper(cond)

*create penalized indicator for each condition if ERR > 1
*gen penalized = pnltr > 0 & err_ > 1
gen penalized = pnltr > 0

merge 1:1 cond provid using `rra', keep(3) nogen

gen pos = err-1 > 0
gen prod = (err-1) * sh_pmt
*assert prod>=0
drop pos

*local linear regression of penalty status on raw readmissions only
capture drop phat*
gen phat_rra = .
gen phat_errsh = .

foreach c in "AMI" "HF" "PN" {
locreg penalized if cond=="`c'", continuous(rra) gen(x, replace) logit
replace phat_rra = x if cond=="`c'"

locreg penalized if cond=="`c'", continuous(prod) gen(x, replace) logit
replace phat_errsh = x if cond=="`c'"
}
drop x

tempfile phat`t'
save `phat`t''
}

*append phat's across 2014-2016
clear
forval t = 2014/2018 {
append using `phat`t''
}
des
tab fy
sum phat*
tempfile phats
save `phats'


*----------------------------------
*local linear regression of penalty status on raw readmissions only
use `phats', clear

keep if fy==2014

foreach c in "AMI" "HF" "PN" {
preserve
keep if cond=="`c'"
tw (line phat_rra rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) sort) (scatter penalized rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) xti("Raw 30-day readmission rate during 2009-2011") yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital is penalized") col(1)))
graph export `gph'/pnlt_rra_`c'.eps, replace

tw (line phat_errsh prod, sort) (scatter penalized prod,  xti("(ERR -1) X Medicare payment share during 2009-2011") yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital is penalized") col(1)))
graph export `gph'/pnlt_errsh_`c'.eps, replace
restore
}
*----------------------------------

*plot the referral concentration trend across FY for high-penalty vs low-penalty hospitals
use `an2', clear
capture drop x
gen x = pnltr if fy==2013
bys provid: egen pnltr13= max(x)
*keep cond provid fy refhhi pnltr13 size

gen fy0 = fy
replace fy = fy + 2
merge m:1 cond provid fy using `phats', keep(1 3) nogen
drop fy
rename fy0 fy

capture drop x
gen x = prod if fy==2013
bys cond provid: egen errsh13= max(x)

*divide into 3 equal sized groups
preserve
keep if fy==2014
keep provid pnltr13 phat_rra phat_errsh cond
duplicates drop

gen gp = .
gen gp_phat = .
gen gp_errsh13 = .

foreach c in "AMI" "HF" "PN" {
capture drop x
xtile x = pnltr13 if cond=="`c'", nq(3)
replace gp = x if cond=="`c'"
table gp, c(mean pnlt)

capture drop x
xtile x = phat_rra if cond=="`c'", nq(3)
replace gp_phat = x if cond=="`c'"
table gp_phat, c(mean phat_rra)

capture drop x
xtile x = phat_errsh if cond=="`c'", nq(3)
replace gp_errsh13 = x if cond=="`c'"
table gp_errsh13, c(mean phat_errsh)
}

tempfile gp
save `gp'
restore

merge m:1 cond provid using `gp', keepusing(gp*) nogen

*get referral HHI for 2009-11
preserve
use PACreferral_tchpm, clear
drop if cond=="HK"
keep if pac=="SNF"
keep if fy > 2008

gen t = 1 if fy >= 2009 & fy <= 2011
replace t = 2 if fy >= 2012 & fy <= 2014
replace t = 3 if fy >= 2015 & fy <= 2016
assert t!=.
collapse (sum) dischnum_pac, by(cond provid t pacprovid)

bys cond provid t: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(cond provid t)
assert refhhi <= 1

reshape wide refhhi, i(cond provid) j(t)
gen change_f0911_t1214 = refhhi2 - refhhi1
gen change_f0911_t1516 = refhhi3 - refhhi1

rename refhhi1 refhhi_0911
rename refhhi2 refhhi_1214
rename refhhi3 refhhi_1516

tempfile tmp
save `tmp'
restore

*merge with referral HHI during 2009-2011
merge m:1 cond provid using `tmp', keep(3) nogen

*calculate change in referral HHI from 2009-2011 to 2012, 2013, ...
gen gr_refhhi = 100*(refhhi - refhhi_0911)/refhhi_0911
replace gr_refhhi=. if fy < 2012
tab fy, summarize(gr_refhhi)

tempfile an3
save `an3'

* distribution of referral HHI by condition
use `an3', clear
foreach c in "AMI" "HF" "PN" {
sum refhhi  if cond=="`c'", de
drop if refhhi > r(p99)
}
tw (kdensity refhhi if cond=="AMI") (kdensity refhhi if cond=="HF") (kdensity refhhi if cond=="PN"), leg(order(1 "AMI" 2 "HF" 3 "PN")) xti(SNF Referral HHI) yti(Kernel density)
graph export `gph'/kd_refhhi_bycond.eps, replace

*by penalty groups, plot trend of referral HHI across time for each condition

*different ways to measure penalty level
loc lgp "overall penalty rate"
loc lgp_phat "probability of penalty based on raw readmission rates"
loc lgp_errsh13 "probability of penalty based on relative performance and size"

foreach g in "gp" "gp_phat" "gp_errsh13" {
use `an3', clear
bys provid: egen minsize =min(size)
keep if minsize==3
*165 hospitals per fy
collapse (mean) refhhi gr_refhhi, by(cond `g' fy)
tempfile gd
save `gd'

foreach c in "AMI" "HF" "PN" {
use `gd', clear
keep if cond=="`c'"

tab `g' , summarize(refhhi)

tw (conected refhhi fy if `g'==1) (conected refhhi fy if `g'==2) (conected refhhi fy if `g'==3), yti(Mean SNF referral HHI) xti(FY) xline(2010.5) xline(2011.5) xlab(2008(1)2016) leg(order(1 "Low penalty" 2 "Medium penalty" 3 "High penalty") col(1)) subti("Large hospitals (300+ beds)") ysc(r(0 0.2)) ylab(0(0.05)0.2) ti("Trend of mean SNF referral HHI for `c'" "by `l`g''", size(medium))
graph export `gph'/refhhi_fy_by`g'_`c'.eps, replace

drop if fy < 2012
tw (conected gr_refhhi fy if `g'==1) (conected gr_refhhi fy if `g'==2) (conected gr_refhhi fy if `g'==3), yti(Mean growth of SNF referral HHI (%)) xti(FY) xlab(2012(1)2016) leg(order(1 "Low penalty" 2 "Medium penalty" 3 "High penalty") col(1)) subti("Large hospitals (300+ beds)") ysc(r(0 100)) ylab(0(10)100) ti("Trend of mean growth of SNF referral HHI for `c' from 2009-2011" "by `l`g''", size(medium))
graph export `gph'/gr_refhhi_fy_by`g'_`c'.eps, replace
}
}

*---------------------------
*understand how the # SNFs are associated with referral HHI
use PACreferral_tchpm, clear
keep if pac=="SNF"
drop if cond=="HK"
*drop if pac==""

collapse (sum) dischnum_pac, by(condition provid fy pacprovid)
list if provid==10005 & fy==2011 & condition=="HK"

gen i = 1 if pacprovid!=. & dischnum_pac!=0
collapse (sum) nsnf = i, by(cond provid fy)

tempfile nsnf
save `nsnf'

use `an3', clear
bys provid: egen minsize =min(size)
keep if minsize==3
drop minsize
merge 1:1 cond provid fy using `nsnf', keep(3) nogen

binscatter refhhi nsnf if nsnf < 40, by(cond) line(qfit) xsc(r(0 40)) xlab(0(10)40) ysc(r(0 0.3)) ylab(0(0.05)0.3) leg(order(1 "AMI" 2 "HF" 3 "PN") col(3)) xti(Number of referred SNFs) yti(SNF referral HHI) ti(Relationship between the number of SNFs and SNF referral HHI, size(medium))
graph export `gph'/test.eps, replace

collapse (mean) refhhi , by(cond nsnf)
sc refhhi nsnf if nsnf < 40, by(cond) xsc(r(0 40)) xlab(0(10)40) ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN") col(3)) xti(Number of referred SNFs) yti(Mean SNF referral HHI)
graph export `gph'/test2.eps, replace */
