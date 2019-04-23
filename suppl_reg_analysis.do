* robustness check analyses to respond to R&R comments

loc gph "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data/"
loc int 2011

cd `dta'/Medicare

loc comorbid_other black_other white_other dual_other metacancer_ct_other-hipfracture_ct_other
loc comorbid_hosp black white index_dual_rate metacancer_ct-hipfracture_ct
*for now use hosp-comorbidities
loc comorbid_snf black_pac white_pac snf_dual_rate metacancer_ct_snf-hipfracture_ct_snf
*loc comorbid_snf snf_dual metacancer_ct_snf-hipfracture_ct_snf
loc reform EHRstage1 EHRstage2 HACstatus bpci vbp_adjf
*omitted group = _65_74
loc ages_snf _75_84_pac_sh _85_94_pac_sh _95p_pac_sh
loc ages_hosp _75_84_sh _85_94_sh _95p_sh
loc ages_other _75_84_other_sh _85_94_other_sh _95p_other_sh
loc sp_hosp _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_hosp' `ages_hosp'
loc sp_other _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_other' `ages_other'
loc sp_snf _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr `comorbid_snf' `ages_snf'

*------------------------------------
* restrict the post-HRRP period up to 2012, inclusive
*------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

*drop FY 2013 or later because new conditions were added to HRRP starting 2013
keep if fy <= 2012

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
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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
* mean and SD of each outcome
use ivpenalty_VI_agg3c_nosw, clear
keep if fy<=2012
foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}
loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
*--------------------
loc yv shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac
sum `yv' if bad==0
*--------------------
loc yv shref read30_other
sum `yv'

*----------------------------------------------------
* unweighted results (replication of Table 2)
*----------------------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

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
  sum `v'
}

forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0, cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0, cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0, cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0, cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l, cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0, cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv read30_pac
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0, cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv read30_other
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_other' vi_snf_l, cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
}

*----------------------------------------------------
* use expected penalty rates alone (without share of Medicare discharges) and replicate Table 2
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

loc wgt [aw=dischnum1]

loc int 2011
loc pptype spp
loc delta = 0.000001
assert pbite>=(spp*mcre_bite-`delta') & pbite<=(spp*mcre_bite+`delta')
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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
*----------------------------------------------------
* Sensitivity analysis when restricting to hospitals with at least 50 SNF referrals in every year
*----------------------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

*tag hospitals if the mean # referrals < 15 in the pre-HRRP
capture drop mdischnum_pac x
bys provid: egen x = min(dischnum_pac)
drop if x < 50
tab fy bad
drop x

loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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

* mean and SD of each outcome
loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
*--------------------
loc yv shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac
sum `yv' if bad==0
*--------------------
loc yv shref read30_other
sum `yv'

*----------------------------------------------------
* heterogeneity analysis 1: small vs large hospitals
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

*find hospitals that stayed small (large) throughout the sample period
bys provid: egen msize = mean(size)
bys provid: egen sdsize = sd(size)
tab fy if msize==3 & sdsize==0

*restrict to small/large hospitals
keep if (msize==3|msize==1) & sdsize==0
gen large = msize==3 & sdsize==0
tab fy large

keep if large==1
loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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

* mean and SD of each outcome
use ivpenalty_VI_agg3c_nosw, clear

*find hospitals that stayed small (large) throughout the sample period
bys provid: egen msize = mean(size)
bys provid: egen sdsize = sd(size)

*restrict to small/large hospitals
keep if (msize==3|msize==1) & sdsize==0
gen large = msize==3 & sdsize==0

forval nn=0/1 {
  preserve
  keep if large==`nn'
  loc x = 0
  gen varname = ""
  foreach v of varlist shref_bytopSNF vi_snf qual_read refhhi lnreqSNF_80pct shref comorbidsum read30_pac read30_other {
    di `x'
    loc x = `x'+1
    replace varname = "`v'" if _n==`x'
  }
  di "total number of outcomes: `x'"
  *9

  gen mean = .
  gen sd = .

  loc yv vi_snf
  sum `yv' if vi_snf2008==0 & bad==0
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
  *--------------------
  foreach yv of varlist shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac {
    sum `yv' if bad==0
    replace mean = `r(mean)' if varname=="`yv'"
    replace sd = `r(sd)' if varname=="`yv'"
  }
  *--------------------
  foreach yv of varlist shref read30_other {
    sum `yv'
    replace mean = `r(mean)' if varname=="`yv'"
    replace sd = `r(sd)' if varname=="`yv'"
  }
  list varname mean sd in 1/`x'
  gen mean2 = string(mean, "%9.2f")
  gen sd2 = string(sd, "%9.1f")
  gen stat = mean2+" ("+sd2+")"
  keep if _n <=`x'
  keep varname stat
  outsheet using `reg'/summstats_large`nn'.csv, comma names replace
  restore
}
*----------------------------------------------------
* heterogeneity analysis 2: BPCI hospitals vs others
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

*mark hospitals that later participate in BPCI at some point
bys provid: egen join_bpci = max(bpci)
tab fy join_bpci
tab fy join_bpci if bad==0
tab fy join_bpci if vi_snf2008==0 & bad==0

*separately run for each BPCI or not case and create new regression output file of the same name
keep if join_bpci==0
loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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
* mean and SD of each outcome
loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
*--------------------
loc yv shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac
sum `yv' if bad==0
*--------------------
loc yv shref read30_other
sum `yv'


*----------------------------------------------------
* heterogeneity analysis 3: by whether the hospital was in a market with above or below median number of SNFs in the HRR [median: 60]
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

* tag whether the hospital’s mean number of SNFs in the HRR was above or below median number of SNFs in the HRR
bys provid: egen m_nsnf_samehrr = mean(nsnf_samehrr)
sum nsnf_samehrr, de
gen large_snf_mkt = m_nsnf_samehrr > `r(p50)'
tab fy large_snf_mkt
tab fy large_snf_mkt if bad==0
tab fy large_snf_mkt if vi_snf2008==0 & bad==0

*separately run for each BPCI or not case and create new regression output file of the same name
keep if large_snf_mkt==1
loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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

* mean and SD of each outcome
preserve
loc x = 0
gen varname = ""
foreach v of varlist shref_bytopSNF vi_snf qual_read refhhi lnreqSNF_80pct shref comorbidsum read30_pac read30_other {
  di `x'
  loc x = `x'+1
  replace varname = "`v'" if _n==`x'
}
di "total number of outcomes: `x'"
*9

gen mean = .
gen sd = .

loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
replace mean = `r(mean)' if varname=="`yv'"
replace sd = `r(sd)' if varname=="`yv'"
*--------------------
foreach yv of varlist shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac {
  sum `yv' if bad==0
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
*--------------------
foreach yv of varlist shref read30_other {
  sum `yv'
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
list varname mean sd in 1/`x'
gen mean2 = string(mean, "%9.2f")
gen sd2 = string(sd, "%9.1f")
gen stat = mean2+" ("+sd2+")"
keep if _n <=`x'
keep varname stat
outsheet using `reg'/summstats.csv, comma names replace
restore

*----------------------------------------------------
* heterogeneity analysis 4: rural vs urban
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

bys provid: egen murban = mean(urban)
bys provid: egen sdurban = sd(urban)
gen urban_hosp = murban==1 & sdurban==0

tab fy urban_hosp
tab fy urban_hosp if bad==0
tab fy urban_hosp if vi_snf2008==0 & bad==0

*separately run for each case and create new regression output file of the same name
keep if urban_hosp==0
loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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

* mean and SD of each outcome
preserve
loc x = 0
gen varname = ""
foreach v of varlist shref_bytopSNF vi_snf qual_read refhhi lnreqSNF_80pct shref comorbidsum read30_pac read30_other {
  di `x'
  loc x = `x'+1
  replace varname = "`v'" if _n==`x'
}
di "total number of outcomes: `x'"
*9

gen mean = .
gen sd = .

loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
replace mean = `r(mean)' if varname=="`yv'"
replace sd = `r(sd)' if varname=="`yv'"
*--------------------
foreach yv of varlist shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac {
  sum `yv' if bad==0
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
*--------------------
foreach yv of varlist shref read30_other {
  sum `yv'
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
list varname mean sd in 1/`x'
gen mean2 = string(mean, "%9.2f")
gen sd2 = string(sd, "%9.1f")
gen stat = mean2+" ("+sd2+")"
keep if _n <=`x'
keep varname stat
outsheet using `reg'/summstats.csv, comma names replace
restore

*----------------------------------------------------
* heterogeneity analysis 5: teaching vs non-teaching
*----------------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

bys provid: egen mteaching = mean(teaching)
bys provid: egen sdteaching = sd(teaching)
gen teaching_hosp = mteaching==1 & sdteaching==0

tab fy teaching_hosp
tab fy teaching_hosp if bad==0
tab fy teaching_hosp if vi_snf2008==0 & bad==0

*separately run for each case and create new regression output file of the same name
keep if teaching_hosp==1
loc wgt [aw=dischnum1]
loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc n 1

loc file1 ols_`pptype'_pv
loc file2 ols_`pptype'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

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
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

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

* mean and SD of each outcome
preserve
loc x = 0
gen varname = ""
foreach v of varlist shref_bytopSNF vi_snf qual_read refhhi lnreqSNF_80pct shref comorbidsum read30_pac read30_other {
  di `x'
  loc x = `x'+1
  replace varname = "`v'" if _n==`x'
}
di "total number of outcomes: `x'"
*9

gen mean = .
gen sd = .

loc yv vi_snf
sum `yv' if vi_snf2008==0 & bad==0
replace mean = `r(mean)' if varname=="`yv'"
replace sd = `r(sd)' if varname=="`yv'"
*--------------------
foreach yv of varlist shref_bytopSNF qual_read refhhi lnreqSNF_80pct comorbidsum read30_pac {
  sum `yv' if bad==0
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
*--------------------
foreach yv of varlist shref read30_other {
  sum `yv'
  replace mean = `r(mean)' if varname=="`yv'"
  replace sd = `r(sd)' if varname=="`yv'"
}
list varname mean sd in 1/`x'
gen mean2 = string(mean, "%9.2f")
gen sd2 = string(sd, "%9.1f")
gen stat = mean2+" ("+sd2+")"
keep if _n <=`x'
keep varname stat
outsheet using `reg'/summstats.csv, comma names replace
restore
