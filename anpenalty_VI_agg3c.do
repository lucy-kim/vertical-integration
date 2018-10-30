*Examine whether penalty pressure for all 3 initial conditions changes referral concentration
*regression analysis of the penalty pressure on vertical integration

/* *packages for local linear regression
net install ivqte, from("https://sites.google.com/site/blaisemelly/")
net install st0026_2, from(http://www.stata-journal.com/software/sj5-3)
ssc install moremata */

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011

cd `dta'/Medicare

*-----------------------
*first construct data containing interaction terms of static and dynamic penalty pressure with time variables separately

use hosp_fy_VI_agg3c_nosw, clear

*sample restriction
drop if fy==2008

*drop small hospitals whose min # bed < 30
bys provid: egen x = min(beds)
tab fy if x >= 30
drop if x < 30
drop x
tab fy
*1722 hospitals

*restrict to hospital-conditions whose mean # discharges over 2009-2010 is at least 50
capture drop x
bys provid: egen x = mean(dischnum) if fy < 2011
bys provid: egen mdischnum = max(x)
tab fy if mdischnum > 50
keep if mdischnum >= 50
drop x mdischnum

bys fy: sum dischnum_pac


*tag hospitals if the # referrals = 0 or 1 at any point in time - don't exclude them in the analysis of concentration measures
capture drop x
capture drop bad
gen x = dischnum_pac < 2
bys provid: egen bad = max(x)
*tab fy if bad==1
drop x

*tag hospitals if the mean # referrals < 15 in the pre-HRRP
capture drop mdischnum_pac
bys provid: egen x = mean(dischnum_pac) if fy < 2011
bys provid: egen mdischnum_pac = max(x)
*sum dischnum* refhhi if mdischnum_pac >= 10
*tab fy if mdischnum_pac < 10
replace bad = 1 if mdischnum_pac < 15
*tab fy bad
drop x mdischnum_pac


*------------------------
*create treatment X Post interaction terms

loc yv vi_snf refhhi refdensity lnreqSNF_80pct shref_bytopSNF shref read30*
foreach v of varlist `yv' {
  loc l`v' "`: var label `v''"
}

xi i.fy

loc int 2011
gen spp = ppr
loc pp spp

tab fy, summarize(`pp')

gen sppXpost`int' = `pp'*post`int'
lab var sppXpost`int' "Static penalty pressure X Post `int'"

gen post13 = fy >= 2013
gen sppXpost13 = `pp'*post13
lab var sppXpost13 "Static penalty pressure X Post 2013"

gen post13trend = 0
replace post13trend = fy - 2012 if fy >= 2013
gen sppXpost13trend = `pp'*post13trend
lab var sppXpost13trend "Static penalty pressure X Post-2013 trend"

gen post11trend = 0
replace post11trend = fy -2010 if fy >= 2011
gen sppXpost11trend = `pp'*post11trend
lab var sppXpost11trend "Static penalty pressure X Post-2011 trend"

forval t = 2010/2016 {
  gen sppX`t' = `pp'*_Ify_`t'
  lab var sppX`t' "Static penalty pressure X `t'"
}

table fy, contents(mean ppr sd ppr p75 ppr p25 ppr max ppr)

tsset provid fy

tempfile an
save `an'

*------------------------
*contruct IVs

/* *to match the sample used for IV analysis, exclude hospitals with missing data in Medicaid ratio for 2000-2008 and % black in 2008
use hosp_fy_VI_agg3c, clear
keep if fy ==2008
collapse (mean) read* ses_score black SSIratio, by(provid hrrnum)
keep provid ses_score black SSIratio hrrnum read*

*if > 1, recode ratio as share
replace SSIratio = SSIratio/100 if SSIratio > 1

foreach n of numlist 30 60 90 {
  areg read`n' black, absorb(hrrnum) cluster(hrrnum)
  predict read`n'_black, xb
}

keep provid read*_black ses_score black SSIratio

foreach v of varlist read*_black ses_score black SSIratio {
  rename `v' `v'08
}

tempfile iv
save `iv' */
*--------------------------
use `an', clear
*merge m:1 provid using `iv', keep(1 3) nogen

loc int 2011
foreach v of varlist read*_black08 ses_score08 black08 SSIratio08 {
  capture drop `v'Xpost`int'
  gen `v'Xpost`int' = `v'*post`int'
  capture drop `v'Xpost11trend
  gen `v'Xpost11trend = `v'*post11trend
  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
  }
}

lab var ses_score08Xpost`int' "Mean SES score X Post `int'"
lab var black08Xpost`int' "Share of black patients X Post `int'"
lab var SSIratio08Xpost`int' "SSI recipient patient days ratio X Post `int'"
lab var SSI_ratio08Xpost`int' "SSI recipient patient days ratio X Post `int'"

forval t=2010/2016 {
  lab var ses_score08X`t' "Mean SES score X `t'"
  lab var black08X`t' "Share of black patients X `t'"
  lab var SSIratio08X`t' "SSI recipient patient days ratio X `t'"
  lab var read30_black08X`t' "Race and income-based 30-day readmission rate X `t'"
  lab var read60_black08X`t' "Race and income-based 31-60 day readmission rate X `t'"
  lab var read90_black08X`t' "Race and income-based 61-90 day readmission rate X `t'"
}

capture drop x
sum vi_snf_l if fy==2009
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
drop x

sum `pp' if fy==`int', de
loc pp_sd: di %9.3f `r(sd)'

*create logged variables
gen lnnsnf_mkt_samehrr = ln(nsnf_samehrr)

xi i.fy i.size

foreach v of varlist defcnt qual_def* qual_read qual_star qual_samh {
  capture drop `v'
}
merge 1:1 provid fy using qualindex_nosw, keep(1 3) nogen
lab var qual_def "Referral share to low-deficiency SNFs"
lab var qual_def_old "Referral share to low-deficiency existing SNFs"
lab var defcnt "Mean deficiency counts"
lab var qual_read "Referral share to low-readmission SNF"
lab var qual_samh "Referral share to low-readmission SNF (same hospital)"
lab var qual_star "Referral share to high-star SNF"

*also get the sum of patient comorbidities among SNF referred patients
egen comorbidsum = rowtotal(metacancer_ct_snf-hipfracture_ct_snf)
replace comorbidsum = comorbidsum/ dischnum_pac
sum comorbidsum
lab var comorbidsum "Average comorbidity counts among SNF referred patients"

tempfile tmp
save `tmp'

*get condition-specific probability of referrals
use SNFreferral_tchpy_nosw.dta, clear
collapse (sum) dischnum_pac, by(cond provid fy)
tempfile cond1
save `cond1'

use index_admit_chy, clear
collapse (sum) dischnum, by(cond provid fy)
merge 1:1 provid fy cond using `cond1', keep(1 3)
replace dischnum_pac = 0 if _m==1
drop _m

gen shref = dischnum_pac/dischnum
assert dischnum==0 if shref==.
assert shref >= 0 & shref <=1 if shref!=.

reshape wide shref dischnum*, i(provid fy) j(cond) st
tempfile cond2
save `cond2'

use `tmp', clear
merge 1:1 provid fy using `cond2', keep(1 3) nogen

*drop hospitals if there are missing values in any of the covariates / outcomes

*for 2 hospital-year pairs, index_dual is missing - code it as zero
replace index_dual_rate = 0 if index_dual_rate==. & index_dual==.
assert index_dual_rate!=.

assert dischnum_pac==0 if snf_dual_rate==.
sum snf_dual dischnum_pac if snf_dual_rate==.

*generate readmission rate, # comorbidity counts, # dual eligibles, # blacks, # whites among all other non-SNF referred patients
gen other_denom = dischnum - dischnum_pac
foreach v of varlist read30 black white metacancer_ct-hipfracture_ct {
  gen `v'_lvl = `v'*dischnum
}
foreach v of varlist read30_pac black_pac white_pac metacancer_ct_snf-hipfracture_ct_snf {
  gen `v'_lvl = `v'*dischnum_pac
  replace `v'_lvl = 0 if dischnum_pac==0
  assert `v'_lvl!=.
}

foreach v of varlist read30 black white {
  gen `v'_other = `v'_lvl - `v'_pac_lvl
}
foreach v of varlist metacancer_ct-hipfracture_ct {
  gen `v'_other = `v'_lvl - `v'_snf_lvl
}
replace snf_dual = 0 if snf_dual==.
replace index_dual = 0 if index_dual==.
gen dual_other = index_dual - snf_dual
assert dual_other!=.

foreach v of varlist *_other {
  replace `v' = `v'/ other_denom
  assert `v' >= 0 & `v' <=1 if `v'!=.
}

* aweight by # discharges in the first year
sort provid fy
foreach v of varlist dischnum beds dischnum_pac other_denom {
  bys provid: gen `v'1 = `v' if _n==1
  bys provid: replace `v'1 = `v'1[_n-1] if `v'1>=.
}

lab var read30 "30-day readmission rate"
lab var read30_pac "30-day readmission rate after referred to SNFs"

*create the % patients in each age group among all discharges
preserve
use index_admit_agebin_chy.dta, clear
drop if cond=="HK"
collapse (sum) _65_74-total, by(provid fy)
tempfile agebin
save `agebin'
restore

merge 1:1 provid fy using `agebin', keep(1 3) nogen
assert total_index_count==dischnum
drop total_index_count

foreach v of varlist _65_74-_95p {
  gen `v'_sh = `v'/dischnum
  assert `v'_sh!=. & `v'_sh>=0 & `v'_sh<=1
}

*create the % patients in each age group among referrals only
preserve
use SNFreferral_agebin_tchpy_nosw.dta, clear
drop if cond=="HK"
collapse (sum) _75_84_pac-_95p_pac, by(provid fy)
tempfile agebin2
save `agebin2'
restore

merge 1:1 provid fy using `agebin2', keep(1 3) nogen

foreach v of varlist _75_84_pac-_95p_pac {
  capture drop `v'_sh
  gen `v'_sh = `v'/dischnum_pac
  assert dischnum_pac==0 if `v'_sh ==.
  assert `v'_sh>=0 & `v'_sh<=1 if `v'_sh!=.
}

*create the % patients in each age group among non-SNF referred patients only
foreach aa in "_65_74" "_75_84" "_85_94" "_95p" {
  gen `aa'_other = `aa' - `aa'_pac
  gen `aa'_other_sh = `aa'_other/other_denom
  sum `aa'_other `aa'_other_sh
  loc v `aa'_other
  assert `v'_sh>=0 & `v'_sh<=1 if `v'_sh!=.
}

tempfile tmp2
save `tmp2'

*add % Medicare share of discharges from the cost report
use `dta'/costreport/hosp_chars_cr, clear
egen x = rowtotal(mcre_dischrg mcaid_dischrg ti5_dischrg)
count if x > dischrg
*~20 hospitals during 2000-2011 have more discharges from each of the segments than total # discharges

*create balanced panel
keep provid
duplicates drop
gen g = 2016 - 1999
expand g
sort provid
bys provid: gen fy = 2000 + _n-1

merge 1:1 provid fy using `dta'/costreport/hosp_chars_cr

keep provid fy dischrg mcre_dischrg

*hospitals with zero discharges, recode using previous values
replace dischrg = . if dischrg==0
replace mcre_dischrg = . if mcre_dischrg==0

foreach v of varlist mcre_dischrg dischrg {
    sort provid fy
    bys provid: replace `v' = `v'[_n-1] if `v'>=.
    gsort provid -fy
    bys provid: replace `v' = `v'[_n-1] if `v'>=.
}
tempfile cr
save `cr'

*use 2008 Medicare share to construct the penalty bite
loc yr 2010
keep if fy==`yr'
gen mcre_dischrg`yr' = mcre_dischrg
gen dischrg`yr' = dischrg
keep provid mcre_dischrg`yr' dischrg`yr'
duplicates drop
tempfile mcre_dischrg`yr'
save `mcre_dischrg`yr''

*hospitals with zero discharges, recode using previous values
use `tmp2', clear
drop dischrg
merge 1:1 provid fy using `cr', keepusing(mcre_dischrg dischrg) nogen keep(1 3)
assert mcre_dischrg!=.
assert dischrg!=.

merge m:1 provid using `mcre_dischrg`yr'', nogen keep(1 3)

*use 2008 Medicare share to construct the penalty bite
gen mcre_bite = mcre_dischrg`yr' / dischrg`yr'
assert mcre_bite>=0 & mcre_bite <=1

gen pbite = spp * mcre_bite
gen pbiteXpost2011 = pbite*post2011

gen pbite_actual = pnltr2013 * mcre_bite
gen pbite_actualXpost2011 = pbite_actual*post2011

gen pnltr2013Xpost2011 = pnltr2013*post2011

/* *rescale the quality metrics so that >= 0
foreach v of varlist qual_read qual_def qual_def_old qual_star {
  sum `v'
  replace `v' = `v' - `r(min)'
  assert `v' >= -0.000000001
} */

gen closeSNF = 1-vi_snf

compress
save ivpenalty_VI_agg3c_nosw, replace


*--------------------------
* data for Meng (as of 5/17/18)
use ivpenalty_VI_agg3c_nosw, clear

xi i.fy i.size

loc reform EHRstage1 EHRstage2 HACstatus bpci
loc sp_hosp _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr vi_snf_l

keep provid fy sppXpost2011 `reform' `sp_hosp'
order provid fy sppXpost2011 `reform' `sp_hosp'

compress
outsheet using hosp_yr_6.4.18.csv, comma names replace

*-------------------------
loc comorbid_other black_other white_other dual_other metacancer_ct_other-hipfracture_ct_other
loc comorbid_hosp black white index_dual_rate metacancer_ct-hipfracture_ct
*for now use hosp-comorbidities
loc comorbid_snf black_pac white_pac snf_dual_rate metacancer_ct_snf-hipfracture_ct_snf
*loc comorbid_snf snf_dual metacancer_ct_snf-hipfracture_ct_snf
loc reform EHRstage1 EHRstage2 HACstatus bpci
*omitted group = _65_74
loc ages_snf _75_84_pac_sh _85_94_pac_sh _95p_pac_sh
loc ages_hosp _75_84_sh _85_94_sh _95p_sh
loc ages_other _75_84_other_sh _85_94_other_sh _95p_other_sh
loc sp_hosp _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_hosp' `ages_hosp'
loc sp_other _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_other' `ages_other'
loc sp_snf _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_snf' `ages_snf'

loc pnltprs2 sppXpost`int' sppX2010 sppXpost11trend
loc pnltprs3 sppX20*

use ivpenalty_VI_agg3c_nosw, clear

loc wgt [aw=dischnum1]

loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci
loc n 1

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  foreach yv of varlist shref {
    *shrefAMI shrefHF shrefPN
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  foreach yv of varlist refhhi shref_bytopSNF lnreqSNF_80pct qual_read  {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------

  loc yv read30_pac
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv read30_other
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
}


*-------------
*for all the outcomes, when using the specification with interaction with year dummies, let's just plot the coefficients
use ivpenalty_VI_agg3c_nosw, clear

loc pptype pbite
*pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc pnltprs2 `pptype'X20*

forval yy=2010/2016 {
  gen pbiteX`yy' = pbite*_Ify_`yy'
}

loc yv read30_pac
xtreg `yv' `pnltprs2' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
tempfile `yv'
parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)

loc yv read30_other
xtreg `yv' `pnltprs2' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
tempfile `yv'
parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)

sum `pptype', de
loc p25 = `r(p25)'
loc p75 = `r(p75)'
loc delta = `p75' - `p25'

use `read30_pac', clear
gen gp = "read30_pac"
foreach yv in "read30_other"  {
  append using ``yv''
  replace gp = "`yv'" if gp==""
}
assert gp!=""
keep if regexm(parm, "`pptype'")
gen year = substr(parm, -4,4)
tab gp
drop dof t

compress
outsheet using `reg'/coef_DiD_static_reform.csv, replace names comma

*--------------------------
* interpret results

*25th & 75th percentile of penalty pressure
sum spp, de
loc p75 = `r(p75)'
loc p25 = `r(p25)'
*p75 = .4343617
*p25 = .1282808
loc epsilon = 0.0000001

tab provid if spp>= `p75'-`epsilon' & spp <= `p75'+`epsilon'
*420015
tab provid if spp>= `p25'-`epsilon' & spp <= `p25'+`epsilon'
*310073

sum read30_pac if provid==420015 & fy==2009
*.25
sum read30_pac if provid==310073 & fy==2009
*.33


sum vi_snf

sum refhhi shref_bytopSNF

*---------------------------------
* use the

*---------------------------------
*use actual penalty rate

use ivpenalty_VI_agg3c, clear

tsset provid fy

replace pnltr2013 = 100*pnltr2013
gen pnltr2013Xpost2011 = pnltr2013*post2011

loc pnltprs1 pnltr2013Xpost`int'

loc file1 ols_spp_reform_pv
loc file2 ols_spp_reform_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
  capture erase `reg'/`file`sn''.tex
}
loc stat1 pv
loc stat2 ci

lab var read30 "30-day readmission rate"
lab var read30_pac "30-day readmission rate after referred to SNFs"

forval sn = 1/2 {
  foreach yv of varlist read30 read30_pac shref vi_snf  refhhi shref_bytopSNF lnreqSNF_80pct den_nsnf_used {

    loc out "outreg2 using `reg'/`file`sn''.xls, append label ctitle(`l`yv'') `stat`sn''"

    forval n = 1/1 {
      *loc pnltprs pnltprsXpost`int'
      *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
      *[aw=nsnf_mkt_samehrr]

      if "`yv'"=="vi_snf" {
        xtreg `yv' `pnltprs`n'' `sp' if vi_snf2008==0, cluster(provid) fe
      }
      else {
        xtreg `yv' `pnltprs`n'' `sp' vi_snf_l , cluster(provid) fe
        *areg `yv' `pnltprs`n'' `sp`sn'' , cluster(provid) absorb(provid)
      }

      *mean dep var
      sum `yv' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
    }
  }
}
*-------------
*reduced form model

use ivpenalty_VI_agg3c, clear

tsset provid fy

loc pnltprs1 rblackXpost`int'

loc file1 ols_spp_reform_pv
loc file2 ols_spp_reform_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
  capture erase `reg'/`file`sn''.tex
}
loc stat1 pv
loc stat2 ci

lab var read30 "30-day readmission rate"
lab var read30_pac "30-day readmission rate after referred to SNFs"

forval sn = 1/2 {
  foreach yv of varlist read30 read30_pac shref vi_snf  refhhi shref_bytopSNF lnreqSNF_80pct den_nsnf_used {

    loc out "outreg2 using `reg'/`file`sn''.xls, append label ctitle(`l`yv'') `stat`sn''"

    forval n = 1/1 {
      *loc pnltprs pnltprsXpost`int'
      *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
      *[aw=nsnf_mkt_samehrr]

      if "`yv'"=="vi_snf" {
        xtreg `yv' `pnltprs`n'' `sp' if vi_snf2008==0, cluster(provid) fe
      }
      else {
        xtreg `yv' `pnltprs`n'' `sp' vi_snf_l , cluster(provid) fe
        *areg `yv' `pnltprs`n'' `sp`sn'' , cluster(provid) absorb(provid)
      }

      *mean dep var
      sum `yv' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
    }
  }
}
*--------------------------
*inertia in the outcomes
use `an', clear
sort provid fy
foreach v of varlist read30 read30_pac refhhi shref_bytopSNF lnreqSNF_80pct gini den_nsnf_used shref {
  capture drop `v'_l
  bys provid: gen `v'_l = `v'[_n-1]
}

foreach v of varlist read30 read30_pac refhhi shref_bytopSNF lnreqSNF_80pct gini den_nsnf_used shref {
  corr `v' `v'_l
}
*--------------------------
*send Meng static penalty pressure data

use `an', clear

xi i.gp_beds i.fy
loc sp1 _Ify_* own_* urban teaching _Igp_beds* lnnsnf_mkt_samehrr vi_snf_l

loc int 2011
loc pnltprs1 sppXpost`int' sppX2010
loc pnltprs2 sppXpost`int' sppX2010 sppXpost11trend
loc pnltprs3 sppX20*
keep provid fy `sp1' `pnltprs2' `pnltprs3' dppXpost`int' dppXpost11trend dppX20*

compress
outsheet using `reg'/hosp_041818.csv, comma names replace

*--------------------------
* for referral HHI outcome

use `an', clear

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr i.gp_beds _Ify_*

loc yv refhhi
forval n = 1/2 {
  loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

  *loc pnltprs pnltprsXpost`int'
  *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
  xtreg `yv' `pnltprs`n'' `sp', fe cluster(provid)

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'
  loc rr: di %9.3f `e(ar2)'

  `out' keep(`pnltprs`n'') addtext(Adjusted R-squared, `rr', Mean dep. var., `mdv', Std dev of penalty pressure, `pp_sd', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
}

use `an_ts', clear

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr i.gp_beds _Ify_*

loc yv refhhi
forval n = 1/2 {
  loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

  *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
  xtreg `yv' `pnltprs`n'' `sp', fe cluster(provid)

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
}

*--------------------------
* for Gini coefficient of SNF referral outcome

use `an', clear

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr i.gp_beds _Ify_*

loc yv gini
forval n = 1/2 {
  loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

  *loc pnltprs pnltprsXpost`int'
  *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
  xtreg `yv' `pnltprs`n'' `sp', fe cluster(provid)

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'
  loc rr: di %9.3f `e(ar2)'

  `out' keep(`pnltprs`n'') addtext(Adjusted R-squared, `rr', Mean dep. var., `mdv', Std dev of penalty pressure, `pp_sd', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
}

use `an_ts', clear
loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr i.gp_beds _Ify_*

loc yv gini
forval n = 1/2 {
  loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

  *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
  xtreg `yv' `pnltprs`n'' `sp', fe cluster(provid)

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
}


*--------------------------
* for spread of SNF per probability outcome
loc yv read30_pac ln_den_nsnf_used_pp lnnsnf_used shref refhhi

loc int 2011
loc pnltprs1 pnltprsXpost`int' pnltprsX2010
loc pnltprs2 pnltprsX20*
loc pnltprs3 pnltprsX2010 pnltprsXpost`int' pnltprsXpost11trend

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr

loc file ols_ln_den_nsnf_used_pp
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex

use `an', clear
foreach yv of varlist ln_den_nsnf_used_pp {
  forval n = 1/2 {
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

    *loc pnltprs pnltprsXpost`int'
    *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
    xtreg `yv' `pnltprs`n'' _Ify_* i.gp_beds `sp', cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Std dev of penalty pressure, `pp_sd', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)

    if `n'==3 {
      *save coefficients & 95% CI
      tempfile `y'_`pp'
      parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``y'_`pp'', replace)
    }
  }
}

use `an_ts', clear
foreach yv of varlist ln_den_nsnf_used_pp {
  forval n = 1/3 {
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

    *loc pnltprs pnltprsXpost`int'
    *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
    xtreg `yv' `pnltprs`n'' _Ify_* i.gp_beds `sp', cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Std dev of penalty pressure, `pp_sd', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
  }
}


*-----------------------------------------------------------------------------------------
*persistance of penatly pressure

*standardize the penalty pressure in each each
use anpenalty_VI_agg3c, clear

*create a z-score of penalty pressure
loc pp ppr
gen z_`pp' = .
forval yr = 2011/2016 {
  qui sum `pp' if fy==`yr'
  replace z_`pp' = (ppr - `r(mean)')/`r(sd)' if fy==`yr'
}
bys fy: sum z_`pp'

keep provid fy z_`pp'
drop if fy < 2011
reshape wide z_`pp', i(provid) j(fy)

forval t = 2011/2016 {
  lab var z_`pp'`t' "`t'"
}
graph matrix z_`pp'2011 z_`pp'2012 z_`pp'2013 z_`pp'2014 z_`pp'2015 z_`pp'2016, half ms(p) maxes(ysc(r(-1 6)) ylab(-1(1)6) xsc(r(-1 6)) xlab(-1(1)6)) ti(Standardized score of predicted penalty rate)
graph export `gph'/std_pp_scmatrix.eps, replace

*--------------------------
*trend of probability of referring toSNF
use `an', clear

graph bar (mean) dischnum, over(fy)
graph export `gph'/trd_dischnum.eps, replace

graph bar (mean) dischnum_pac, over(fy)
graph export `gph'/trd_dischnum_pac.eps, replace

*--------------------------
*inertia in the outcome within a hospital
use anpenalty_VI_agg3c, clear

*plot the referral concentration in 2011 vs referral concentration in 2012
sort provid fy
bys provid: gen refhhi_l = refhhi[_n-1]

foreach v of varlist refhhi refhhi_l {
  bys provid: egen m`v' = mean(`v')
  gen `v'_dm = `v'-m`v'
}
tw (sc refhhi refhhi_l if fy > 2009, by(fy p75_ppr2012) msize(small)) (sc refhhi refhhi if fy > 2009, by(fy p75_ppr2012) msize(small))
graph export `gph'/test.eps, replace

tw (sc refhhi_dm refhhi_l_dm if fy != 2009, by(fy p75_ppr2012) msize(small)) (sc refhhi_dm refhhi_dm if fy != 2009, by(fy p75_ppr2012) msize(small))
graph export `gph'/test2.eps, replace

*--------------------------
*understanding the penalty pressure measures
use anpenalty_VI_agg3c, clear

forval int = 2011/2011 {
  sum ppr if fy==`int', de
  loc p75_`int' = `r(p75)'
  loc p25_`int' = `r(p25)'
  di "`int': 75th percentile of predicted penalty rate = `p75_`int''"
  di "`int': 25th percentile of predicted penalty rate = `p25_`int''"
}

forval int = 2011/2011 {
  loc pp ppr`int'
  gen gp = .
  replace gp = 1 if p75_`pp'==1
  replace gp = 2 if p25_75_`pp'==1
  replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
  assert gp!=.
  rename gp gp`int'

  gen diff_`pp' = ppr - `pp'
}

preserve
keep gp2011 fy diff_ppr2011 ppr2011 pnltrate_t2

replace pnltrate_t2 = 100* pnltrate_t2
replace pnltrate_t2 = 0 if fy < 2011

outsheet using `gph'/diff_ppr.csv, comma names replace
restore

*if the hospital had a high predicted penalty rate in year 2011, 2012, respectively, what was the predicted penalty rate for each year before and after then?
loc pp ppr
preserve
keep if p75_

/*
preserve
keep gp fy diff_ppr`int' ppr`int'
outsheet using `gph'/diff_ppr`int'.csv, comma names replace
restore */

preserve
keep provid fy ppr pnltrate_t2
replace pnltrate_t2 = 100* pnltrate_t2
replace pnltrate_t2 = 0 if fy < 2011

rename pnltrate_t2 realp
*reshape wide ppr realp, i(provid) j(fy)
*drop *2009 *2010

outsheet using `gph'/ppr_vs_realp_byfy2.csv, comma names replace
restore

tab p75_ppr2012 p75_ppr2011

*how good are the penalty pressure measures for predicting the actual penalty rate?
tw (sc pnltr2013 ppr2011 if fy==2013, msize(small) msymbol(circle hollow)) (line ppr2011 ppr2011 if fy==2013, lc(black) sort)
graph export `gph'/pnltr2013_v_ppr2011.eps, replace

*plot the penalty pressure (based on rate) in 2012 vs 2011
tw (sc ppr2012 ppr2011 if fy==2012, msize(small)) (line ppr2012 ppr2012 if fy==2012, lc(black) sort)
*yline(`p75_2012' `p25_2012') xline(`p75_2011' `p25_2011'))

qqplot ppr2012 ppr2011 if fy==2012, msize(small) msymbol(circle hollow) xsc(r(0 2)) xlab(0(0.5)2) xti("`lppr2011' (%)") yti("`lppr2012' (%)")
graph export `gph'/qqplot.eps, replace

*--------------------------
*test for serial correlation
use anpenalty_VI_agg3c, clear

xtset provid fy
tsset provid fy

xi i.fy i.gp_beds

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr
loc y refhhi
loc pp ppr`int'

replace ppr`int' = 0.02 if ppr`int' > 0.02
forval t = 2010/2016 {
  gen p75_pnltprsX`t' = p75_`pp'X`t'
  lab var p75_pnltprsX`t' "Top quartile X `t'"
  gen p25_75_pnltprsX`t' = p25_75_`pp'X`t'
  lab var p25_75_pnltprsX`t' "Middle 2 quartiles X `t'"
  gen pnltprsX`t' = `pp'X`t'
  lab var pnltprsX`t' "Penalty pressure X `t'"
}
loc pnltprs pnltprsX20*
*loc pnltprs p75_pnltprsX20* p25_75_pnltprsX20*
areg `y' `pnltprs' _Ify_* i.gp_beds `sp' , absorb(provid) cluster(provid)
xtreg `y' `pnltprs' _Ify_* i.gp_beds `sp' ,  fe cluster(provid)
xtserial `y' `pnltprs' _Ify_* _Igp_beds* `sp' ,  output

*---------------------------------------------
*regression using time-varying penalty pressure


use `an_ts', clear

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr

loc yv ln_den_nsnf_used_pp
loc pnltprs1 pnltprsXpost`int'
loc pnltprs2 pnltprsX2011 pnltprsX2012 pnltprsX2013 pnltprsX2014 pnltprsX2015 pnltprsX2016
loc pnltprs3 pnltprsX2011 pnltprsX2012 pnltprsXpost13
loc pnltprs4 pnltprsX2011 pnltprsX2012 pnltprsXpost13trend
loc pnltprs5 pnltprsX2011 pnltprsX2012 pnltprsXpost13 pnltprsXpost13trend

foreach yv of varlist read30_pac samh30_pac ln_den_nsnf_used_pp lnnsnf_used shref refhhi {
  loc file ols_ts_post`int'_`yv'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

  forval n = 1/5 {
    *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
    xtreg `yv' `pnltprs`n'' _Ify_* i.gp_beds `sp', fe cluster(provid)

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
  }
}

*for vi_snf outcome, restrict to hospitals that didn't have own SNF in 2008
use `an_ts', clear
sum vi_snf_l if fy==2009
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
tab vi_snf2008
keep if vi_snf2008==0

loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr _Ify_* i.gp_beds

loc yv vi_snf
loc file ols_ts_post`int'_`yv'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`yv'')"

forval n = 1/5 {
  *loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
  xtreg `yv' `pnltprs`n'' `sp', fe cluster(provid)

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y) dec(3) fmt(fc)
}


*-----------------------

*data for Lily
loc int 2011
loc pp ppr`int'
/* use anpenalty_VI_agg3c, clear

xi i.fy i.gp_beds
loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr
keep provid fy `sp' pnltprsX20* _Ify_* _Igp_beds*


tempfile cs
save `cs' */

*time-varying penalty pressure data
use anpenalty_VI_agg3c, clear

forval yr = 2011/2016 {
  replace ppr = `pnltmax`yr'' if ppr > `pnltmax`yr'' & fy==`yr'
}
replace ppr = ppr*100
table fy, contents(max ppr)

loc pp ppr
forval t = 2011/2016 {
  gen pnltprsX`t' = `pp'*_Ify_`t'
  lab var pnltprsX`t' "Dynamic penalty pressure X `t'"
  rename pnltprsX`t' dynappX`t'
}
table fy, contents(mean dynappX2011 mean dynappX2012)

loc int 2011
loc pp ppr`int'
replace ppr`int' = `pnltmax`int'' if ppr`int' > `pnltmax`int''
replace ppr`int' = ppr`int'*100
sum ppr`int'

forval t = 2010/2016 {
  gen pnltprsX`t' = `pp'*_Ify_`t'
  lab var pnltprsX`t' "Static penalty pressure X `t'"
  rename pnltprsX`t' statppX`t'
}
table fy, contents(mean statppX2011 mean statppX2012)

xi i.fy i.gp_beds
loc sp vi_snf_l own_* urban teaching lnnsnf_mkt_samehrr
keep provid fy `sp' statppX20* dynappX20* _Ify_* _Igp_beds*

compress
outsheet using anpenalty_VI_agg3c_040518.csv, replace comma names

*-----------------------
*why 2012 have much fewer SNF referrals?
use SNFreferral_tchpm, clear
drop if cond=="HK"

keep provid dischmth dischyr dischnum_pac
collapse (sum) dischnum_pac, by(provid ym fy)
tempfile tmp
save `tmp'

use `an', clear
keep provid
duplicates drop
merge 1:m provid using `tmp', keep(3) nogen

gen sym = string(ym, "%tm")
labmask ym, values(sym)
graph bar (mean) dischnum_pac, over(ym, gap(*15) label(angle(90) labsize(tiny)))
graph export `gph'/trd_dischnum_pac_monthly.eps, replace


use index_admit_chm, clear
drop if cond=="HK"

keep provid dischmth dischyr dischnum_pac
collapse (sum) dischnum_pac, by(provid ym fy)
tempfile tmp2
save `tmp2'

use `an', clear
keep provid
duplicates drop
merge 1:m provid using `tmp', keep(3) nogen


use `an', clear
qqplot refhhi gini
graph export `gph'/test.eps, replace

*-----------------------
*export to R to visualize the trend in outcomes for top, middle 2 and bottom quartiles of penalty pressure
*-----------------------
* for every year, exclude June & reconstruct outcomes

*1) referral HHI
loc yv refhhi shref nsnf_used read* samh*

use SNFreferral_tchpm, clear
drop if cond=="HK"

drop if dischmth==6

tempfile pac_nojune
save `pac_nojune'

collapse (sum) dischnum_pac, by(provid fy pacprovid)
assert dischnum_pac==0 if pacprovid==.

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq (mean) dischnum_pac= tot, by(provid fy)
assert refhhi <= 1
rename refhhi refhhi3

tempfile hosp_fy_ref3
save `hosp_fy_ref3'

use `an', clear
keep provid fy
duplicates drop
merge 1:1 provid fy using `hosp_fy_ref3', keep(3) nogen
graph bar (mean) dischnum_pac, over(fy)
graph export `gph'/trd_dischnum_pac_nj.eps, replace

*2) number of SNFs being referred to
use `pac_nojune', clear
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

collapse (mean) nsnf_used nsnf_hrr_fy, by(provider fy hrrnum)
rename provider provid

* % SNFs in the HRR used by the hospital-FY for each condition
gen shsnf_used = nsnf_used/nsnf_hrr_fy
sum shsnf_used, de
assert shsnf_used ==. if hrrnum==.

tempfile shsnf_used
save `shsnf_used'

*3) readmission rate among SNF referrals
use `pac_nojune', clear
collapse (sum) *_pac, by(provid fy)
tempfile volume
save `volume'

*4) probability of referral
use index_admit_chm, clear
drop if cond=="HK"
drop if dischmth==6

collapse (sum) dischnum-read90 (mean) ses_score, by(provid fy)
merge 1:1 provid fy using `volume', keep(3) nogen

gen shref = dischnum_pac / dischnum
lab var shref "Share of discharges referred to SNF"

*merge other outcomes
merge 1:1 provid fy using `shsnf_used', keep(3) nogen
merge 1:1 provid fy using `hosp_fy_ref3', keep(3) nogen

rename refhhi3 refhhi

loc vl
foreach v of varlist dischnum-refhhi {
  loc vl "`vl' `v'"
}
tempfile outc_nojune
save `outc_nojune'

*-----------------------


use `an', clear
keep provid fy ppr2011 den_nsnf_used_pp post2011
binscatter den_nsnf_used_pp ppr2011 if ppr2011 <1, by(post2011) subti(Number of SNFs being referred to per probability of referral) yti("") xti(Predicted penalty rate in 2011) leg(order(1 "2009-2010" 2 "2011-2016"))
graph export `gph'/test.eps, replace


use `an_ts', clear
keep provid fy ppr den_nsnf_used_pp post2011
binscatter den_nsnf_used_pp ppr, by(post2011) subti(Number of SNFs being referred to per probability of referral) yti("") xti(Predicted penalty rate in each year) leg(order(1 "2009-2010" 2 "2011-2016"))
graph export `gph'/test2.eps, replace


*no June
use `an', clear

loc int 2011
loc pp ppr`int'
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
assert gp!=.

foreach v of varlist `vl' {
  loc l_`v' : var lab `v'
  drop `v'
}

merge 1:1 provid fy using `outc_nojune', keep(3) nogen
foreach v of varlist `vl' {
  lab var `v' "`l_`v''"
}

*create # SNFs referred to per probability of SNF referral
foreach v of varlist den_nsnf_used {
  capture drop `v'_pp
  gen `v'_pp = nsnf_used / shref
  capture drop ln_`v'_pp
  gen ln_`v'_pp = ln(`v'_pp)
  sum `v'_pp, de
  des `v'_pp
}
preserve
loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh*
collapse (mean) `yv', by(fy gp)

tempfile out1
save `out1'
restore

*for vi_snf outcome, restrict to hospitals that didn't have own SNF in 2008
preserve
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
tab vi_snf2008
keep if vi_snf2008==0

loc yv vi_snf
collapse (mean) `yv', by(fy gp)

merge 1:1 fy gp using `out1', nogen
outsheet using `gph'/yv_mean_se_nojune.csv, comma names replace
restore


use `an', clear

loc int 2011
loc pp ppr`int'
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
assert gp!=.

preserve
loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh* gini
collapse (mean) `yv', by(fy gp)

tempfile out1
save `out1'
restore

*for vi_snf outcome, restrict to hospitals that didn't have own SNF in 2008
preserve
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
tab vi_snf2008
keep if vi_snf2008==0

loc yv vi_snf
collapse (mean) `yv', by(fy gp)

merge 1:1 fy gp using `out1', nogen
outsheet using `gph'/yv_mean_se.csv, comma names replace
restore

*export outcomes by time-varying penalty pressure
use `an_ts', clear
loc pp ppr
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.
tab fy

foreach v of varlist `vl' {
  loc l_`v' : var lab `v'
  drop `v'
}

merge 1:1 provid fy using `outc_nojune', keep(3) nogen
foreach v of varlist `vl' {
  lab var `v' "`l_`v''"
}

*create # SNFs referred to per probability of SNF referral
foreach v of varlist den_nsnf_used {
  capture drop `v'_pp
  gen `v'_pp = nsnf_used / shref
  capture drop ln_`v'_pp
  gen ln_`v'_pp = ln(`v'_pp)
  sum `v'_pp, de
  des `v'_pp
}


preserve
keep if fy <2011

*get the mean of outcome by grouping based on the 2011 predicted penalty rate
loc pp ppr`int'
drop gp
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.

loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh*
collapse (mean) `yv', by(fy gp)
tab fy gp
tempfile pre11
save `pre11'
restore

preserve
keep if fy >=2011
loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh*
collapse (mean) `yv', by(fy gp )
append using `pre11'

keep gp fy `yv'
tempfile out1_ts
save `out1_ts'
restore

loc yv vi_snf
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
tab vi_snf2008
keep if vi_snf2008==0

preserve
keep if fy < 2011

*get the mean of outcome by grouping based on the 2011 predicted penalty rate
loc pp ppr`int'
drop gp
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.

collapse (mean) vi_snf, by(fy gp)
tab fy gp
tempfile pre11_2
save `pre11_2'
restore

preserve
keep if fy >=2011
collapse (mean) vi_snf, by(fy gp)
append using `pre11_2'

merge 1:1 fy gp using `out1_ts', nogen

outsheet using `gph'/yv_mean_se_ts_nojune.csv, comma names replace
restore


use `an_ts', clear
loc pp ppr
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.
tab fy

preserve
keep if fy <2011

*get the mean of outcome by grouping based on the 2011 predicted penalty rate
loc pp ppr`int'
drop gp
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.

loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh* gini
collapse (mean) `yv', by(fy gp)
tab fy gp
tempfile pre11
save `pre11'
restore

preserve
keep if fy >=2011
loc yv ln_den_nsnf_used_pp refhhi shref nsnf_used den_nsnf_used_pp read* samh* gini
collapse (mean) `yv', by(fy gp )
append using `pre11'

keep gp fy `yv'
tempfile out1_ts
save `out1_ts'
restore

loc yv vi_snf
gen x = vi_snf_l if fy==2009
bys provid: egen vi_snf2008 = max(x)
tab vi_snf2008
keep if vi_snf2008==0

preserve
keep if fy < 2011

*get the mean of outcome by grouping based on the 2011 predicted penalty rate
loc pp ppr`int'
drop gp
gen gp = .
replace gp = 1 if p75_`pp'==1
replace gp = 2 if p25_75_`pp'==1
replace gp = 3 if p75_`pp'==0 & p25_75_`pp'==0
replace gp = . if `pp'==.

collapse (mean) vi_snf, by(fy gp)
tab fy gp
tempfile pre11_2
save `pre11_2'
restore

preserve
keep if fy >=2011
collapse (mean) vi_snf, by(fy gp)
append using `pre11_2'

merge 1:1 fy gp using `out1_ts', nogen

outsheet using `gph'/yv_mean_se_ts.csv, comma names replace
restore


loc yv vi_snf
ciplot `yv', by(fy) yti("") subti(`l`yv'') ysc(r(0 0.5)) ylab(0(0.1)0.5) caption("")
graph export `gph'/des_`yv'.eps, replace

*-----------
*how should i understand the time-varying penalty pressure vs time-contsant penalty pressure?
use anpenalty_VI_agg3c, clear

forval yr = 2011/2016 {
  replace ppr = 100*`pnltmax`yr'' if ppr > `pnltmax`yr'' & fy==`yr'
}
replace ppr2012 = 100*`pnltmax2012' if ppr2012 > `pnltmax2012'
sum ppr2012 ppr


tw (sc pnltr2014 ppr2012 if fy==2013, msize(vsmall))
graph export `gph'/test.eps, replace

tw (sc ppr ppr2012 if fy==2013, msize(vsmall))
graph export `gph'/test.eps, replace

*-----------

*export regression coefficients to CSV file for visualization in R
foreach y in "refhhi" "lnnsnf_used" "refhhi_prevSNFs" "shref_usedbefore" "shsnf_used_again" "shsnf_used" "shref" "ratio_nsnf" {
  foreach pp in "ppst`int'" "ppr`int'" "lppd`int'" {
    use ``y'_`pp'', clear
    keep if regexm(parm, "20")
    gen fy = substr(parm, -4, 4)
    destring fy, replace
    gen gp = .
    replace gp = 1 if regexm(parm, "p75")
    replace gp = 2 if regexm(parm, "p25_75_")
    replace gp = 3 if regexm(parm, "_Ify_")
    lab define lgp 1 "Top quartile" 2 "Middle 2 quartiles" 3 "Bottom quartile", replace
    lab val gp lgp
    keep gp fy estimate min95 max95
    gen outcome = "`y'"
    gen pp = "`pp'"
    *tw (rcap max95 min95 fy if gp < 3, by(gp) ysc(r(-0.02 0.02)) ) (scatter estimate fy if gp <3 )
    *graph export `gph'/test.eps, replace
    duplicates drop
    tempfile `y'_`pp'2
    save ``y'_`pp'2'
  }
}

clear
foreach y in "refhhi" "refhhi_prevSNFs" "shref_usedbefore" "shsnf_used_again" "shsnf_used" "shref" "ratio_nsnf" {
  foreach pp in "ppst`int'" "ppr`int'" "lppd`int'" {
    append using ``y'_`pp'2'
  }
}
outsheet using `reg'/est_pp2012.csv, comma names replace





*---------------------------
*use within-hospital cross-time variation in penalty pressure

use anpenalty_VI_agg3c, clear

loc lshref "Share of discharges referred to SNF"
loc lrefhhi "SNF referral concentration"
loc lrefhhi_samehrr "Referral concentration among SNFs in the same HRR"
loc lshsnf_used "Share of SNFs in the market used"
loc lratio_nsnf "Ratio of # SNFs used in current year to previous year"
loc lshref_usedbefore "Share of SNF referrals to previouly used SNFs"
loc lshsnf_used_ag "Share of previously used SNFs used again"
loc lrefhhi_prevSNFs "Referral HHI among SNFs used in the previous year"

*loc file ols_`y'_ts_post
loc file ols_ts_post1
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex

foreach y of varlist shref refhhi shsnf_used ratio_nsnf {
  *foreach y of varlist shref_usedbefore shsnf_used_ag refhhi_prevSNFs {
  * use post dummy
  loc sp i.gp_beds vi_snf_l own_* urban teaching pac_hhi_hrr

  *ppr lppd
  foreach pp of varlist ppr  {
    *loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`pp'')"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`y'')"

    use anpenalty_VI_agg3c, clear
    gen p75_pnltprsXpost`int' = p75_`pp'Xpost`int'
    lab var p75_pnltprsXpost`int' "Top quartile X post`int'"
    gen p25_75_pnltprsXpost`int' = p25_75_`pp'Xpost`int'
    lab var p25_75_pnltprsXpost`int' "Middle 2 quartiles X post`int'"
    gen pnltprsXpost`int' = `pp'Xpost`int'
    lab var pnltprsXpost`int' "Penalty pressure X post`int'"

    *loc pnltprs pnltprsXpost`int'
    loc pnltprs p75_pnltprsXpost`int' p25_75_pnltprsXpost`int'
    areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching pac_hhi_hrr) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
  }
}

loc file ols_ts_post2
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex

foreach y of varlist shref_usedbefore shsnf_used_ag refhhi_prevSNFs {
  * use post dummy
  loc sp i.gp_beds vi_snf_l own_* urban teaching pac_hhi_hrr

  *ppr lppd
  foreach pp of varlist ppst  {
    *loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`pp'')"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`y'')"

    use anpenalty_VI_agg3c, clear
    gen p75_pnltprsXpost = p75_`pp'Xpost
    lab var p75_pnltprsXpost "Top quartile X Post"
    gen p25_75_pnltprsXpost = p25_75_`pp'Xpost
    lab var p25_75_pnltprsXpost "Middle 2 quartiles X Post"
    gen pnltprsXpost = `pp'Xpost
    lab var pnltprsXpost "Penalty pressure X Post"

    *loc pnltprs pnltprsXpost
    loc pnltprs p75_pnltprsXpost p25_75_pnltprsXpost
    areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching pac_hhi_hrr) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
  }
}

loc file ols_ts_fy1
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex

foreach y of varlist shref refhhi shsnf_used ratio_nsnf {
  *use FY indicators

  loc sp i.gp_beds vi_snf_l own_* urban teaching pac_hhi_hrr

  * ppr lppd
  foreach pp of varlist ppst {
    *loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`pp'')"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`y'')"

    use anpenalty_VI_agg3c, clear

    forval t = 2010/2016 {
      gen p75_pnltprsX`t' = p75_`pp'X`t'
      lab var p75_pnltprsX`t' "Top quartile X `t'"
      gen p25_75_pnltprsX`t' = p25_75_`pp'X`t'
      lab var p25_75_pnltprsX`t' "Middle 2 quartiles X `t'"
      gen pnltprsX`t' = `pp'X`t'
      lab var pnltprsX`t' "Penalty pressure X `t'"
    }

    *loc pnltprs pnltprsX20*
    loc pnltprs p75_pnltprsX20* p25_75_pnltprsX20*
    areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching pac_hhi_hrr) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
  }
}

loc file ols_ts_fy2
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex

foreach y of varlist shref_usedbefore shsnf_used_ag refhhi_prevSNFs {
  *use FY indicators

  loc sp i.gp_beds vi_snf_l own_* urban teaching pac_hhi_hrr

  * ppr lppd
  foreach pp of varlist ppst {
    *loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`pp'')"
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`l`y'')"

    use anpenalty_VI_agg3c, clear

    forval t = 2010/2016 {
      gen p75_pnltprsX`t' = p75_`pp'X`t'
      lab var p75_pnltprsX`t' "Top quartile X `t'"
      gen p25_75_pnltprsX`t' = p25_75_`pp'X`t'
      lab var p25_75_pnltprsX`t' "Middle 2 quartiles X `t'"
      gen pnltprsX`t' = `pp'X`t'
      lab var pnltprsX`t' "Penalty pressure X `t'"
    }

    *loc pnltprs pnltprsX20*
    loc pnltprs p75_pnltprsX20* p25_75_pnltprsX20*
    areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

    *mean dep var
    sum `y' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching pac_hhi_hrr) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
  }
}


*---------------------------
* IV regression

use hosp_fy_VI, clear
keep if fy ==2008
gen rblack = black / dischnum
keep provid cond ses_score rblack dissh uncomp1
duplicates drop
foreach v of varlist ses_score rblack dissh uncomp1 {
  ren `v' `v'08
}
tempfile iv08
save `iv08'

use `an2', clear
merge m:1 provid cond using `iv08', keep(1 3) nogen

foreach v of varlist ses_score08 rblack08 dissh08 uncomp108 {
  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
  }
}

compress
save penalty_VI_bycond, replace

loc ivpost ses_score08Xpost rblack08Xpost dissh08Xpost uncomp108Xpost
loc ivpost ses_score08Xpost rblack08Xpost dissh08Xpost uncomp108Xpost

*first stage
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach pp of varlist ppst ppr lppd {
    loc y `pp'Xpost
    areg `y' `iv' _Ify_* `sp' if cond=="`c'", cluster(provid) absorb(provid)
    test
  }
}

loc y refhhi
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach pp of varlist ppst ppr lppd {
    ivreg2 `y' _Ify_* `sp' i.provid (`pp'Xpost = `ivpost') if cond=="`c'", cluster(provid) first partial(i.provid)
  }
}



loc y refhhi
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c {
    ivreg2 `y' _Ify_* i.size ln_dischnum_pac `sp' i.provid (`v'X20* = ses_scoreX20* rblackX20*) if cond=="`c'" & fy < 2015, cluster(provid) first partial(i.provid)
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

loc sp1 _Ify_* i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' _Ify_* `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

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

loc sp1 _Ify_* i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' _Ify_* `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

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
gen change_f0911_t1516 = refhhi - refhhi1

rename refhhi1 refhhi_0911
rename refhhi2 refhhi_1214
rename refhhi refhhi_1516

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

*--------------------------------
*summary statistics: compare characteristics of penalzied & non-penalized hospitals
use anpenalty_VI_agg3c, clear

loc outc dischnum_pac shref refhhi shsnf_used ratio_nsnf shref_usedbefore shsnf_used_ag refhhi_prevSNFs
loc hospchar beds dischnum-ses_score read30_pac-samh90_pac vi_snf_l own_* urban teaching pac_hhi_hrr nsnf_hrr_fy uncomp1 dissh tot_pat_rev mcr_pmt
des `outc' `hospchar'

foreach v of varlist mcr_pmt {
  replace `v' = `v'/1000
}
foreach v of varlist tot_pat_rev {
  replace `v' = `v'/100000
}
lab var mcr_pmt "Medicare DRG payment ($1,000s)"
lab var tot_pat_rev "Total patient revenue ($100,000s)"

keep `outc' `hospchar' post penalized2013
order `outc' `hospchar'

outreg2 using `reg'/summ_all.xls, label replace sum(log) eqkeep(N mean) tex

bysort post penalized2013: outreg2 using `reg'/summ.xls, label replace sum(log) eqkeep(N mean) tex
