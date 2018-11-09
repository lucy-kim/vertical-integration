*robustness check analysis:
*1) rerun using the hospitals’ actual penalty rate
*2) adding a quadratic term for penalty rate
*3) shorten the post-intervention period to one year after the HRRP program
*4) estimate the penalty pressure interacted with each year dummy

loc gph "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"
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

*--------------------------------------------
*1) use Actual penalty * Medicare share
*--------------------------------------------
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

use ivpenalty_VI_agg3c_nosw, clear

loc wgt [aw=dischnum1]

loc int 2011
loc pptype pbite_actual
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'

loc file1 ols_`pptype'_pv
loc file2 ols_`pptypes'_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci
loc digit1 dec(3)
loc digit2 dec(2)

loc n 1

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
}

forval sn = 1/2 {
  *--------------------
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_pac
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_other
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
}

*--------------------------------------------
*2) rerun using the hospitals’ actual penalty rate
*--------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

assert pnltr2013!=.

loc int 2011
gen pnltr2013Xpost`int' = pnltr2013* post`int'
loc pnltprs1 pnltr2013Xpost`int'

loc wgt [aw=dischnum1]

loc file1 ols_actual_pv
loc file2 ols_actual_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci
loc digit1 dec(3)
loc digit2 dec(2)

lab var read30 "30-day readmission rate"
lab var read30_pac "30-day readmission rate after referred to SNFs"
loc n 1

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  sum `v'
}

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  *--------------------
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_pac
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_other
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
}

*--------------------------------------------
*3) shorten the post-intervention period to one year after the HRRP program
*--------------------------------------------
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

use ivpenalty_VI_agg3c_nosw, clear

keep if fy <= 2013

loc wgt [aw=dischnum1]

loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'

loc file1 ols_sppshort_pv
loc file2 ols_sppshort_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci
loc digit1 dec(3)
loc digit2 dec(2)

loc n 1

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
}

forval sn = 1/2 {
  *--------------------
  loc yv shref_bytopSNF
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv qual_read
  *qual_samh qual_star qual_def
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  *--------------------
  foreach yv of varlist refhhi lnreqSNF_80pct {
    *qual_samh qual_star qual_def
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
  }
  *--------------------
  loc yv shref
  *shrefAMI shrefHF shrefPN
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_pac
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)

  *--------------------
  loc yv read30_other
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) `digit`sn'' fmt(fc)
}

*--------------------------------------------
*4) estimate the penalty pressure interacted with each year dummy
*--------------------------------------------
use ivpenalty_VI_agg3c_nosw, clear

sum shref comorbidsum vi_snf refhhi shref_bytopSNF lnreqSNF_80pct qual_read read30_pac read30_other

foreach v of varlist shref shref_bytopSNF read30_pac read30_other {
  replace `v' = `v'*100
  assert `v' >=0 & `v' <=100 if `v'!=.
}

loc pptype pbite
*pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'
loc pnltprs2 `pptype'X20*

forval yy=2010/2016 {
  gen pbiteX`yy' = pbite*_Ify_`yy'
}
loc n = 2

loc file1 ols_`pptype'yr_pv
loc file2 ols_`pptype'yr_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci
loc digit1 dec(3)
loc digit2 dec(2)

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/1 {
  foreach yv of varlist shref {
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l `wgt', cluster(provid) fe
    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

    tempfile `yv'
    parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)
  }

  *--------------------
  loc yv comorbidsum
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  tempfile `yv'
  parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)

  *--------------------
  loc yv vi_snf
  loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 [aw=dischnum_pac1], cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  tempfile `yv'
  parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)
  *--------------------
  foreach yv of varlist refhhi shref_bytopSNF lnreqSNF_80pct qual_read {
    loc out "outreg2 using `reg'/`file`sn''.xls, append label `stat`sn''"
    xtreg `yv' `pnltprs`n'' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe

    *mean dep var
    sum `yv' if e(sample)
    loc mdv: display %9.3f `r(mean)'

    `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

    tempfile `yv'
    parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)
  }
  loc yv read30_pac
  xtreg `yv' `pnltprs2' `sp_snf' vi_snf_l if bad==0 [aw=dischnum_pac1], cluster(provid) fe
  tempfile `yv'
  parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)

  loc yv read30_other
  xtreg `yv' `pnltprs2' `sp_other' vi_snf_l [aw=other_denom1], cluster(provid) fe
  tempfile `yv'
  parmest,format(estimate min95 max95 %8.4f p %8.3f) saving(``yv'', replace)
}

sum `pptype', de
loc p25 = `r(p25)'
loc p75 = `r(p75)'
loc delta = `p75' - `p25'

use `shref', clear
gen gp = "shref"
foreach yv in "comorbidsum" "vi_snf" "refhhi3" "shref_bytopSNF" "lnreqSNF_80pct" "qual_read" "read30_pac" "read30_other" {
  append using ``yv''
  replace gp = "`yv'" if gp==""
}
assert gp!=""
keep if regexm(parm, "`pptype'")
gen year = substr(parm, -4,4)
tab gp
drop dof t

compress
outsheet using `reg'/coef_DiD_static_refoutc.csv, replace names comma


*--------------------------------------------
*4) using the sample of hospitals without SNF at baseline, re-estimate the models
*--------------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

keep if vi_snf2008==0


loc wgt [aw=dischnum1]

loc int 2011
loc pptype pbite
*pbite pbite_actual pnltr2013
loc pnltprs1 `pptype'Xpost`int'

loc file1 ols_`pptype'_noSNF_pv
loc file2 ols_`pptype'_noSNF_ci
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




*--------------------------------------------
*5) estimate the dynamic penalty pressure interacted with post (with pre-penalty value set to 2011 expected penatly rate)
*--------------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

*merge with dynamimc penalty rate
assert ppr==spp
drop ppr
merge 1:1 provid fy using predict_pnltprs_dynamic, keep(1 3)
*_m=1 for years 2009-11

gen dpp = spp if fy<=2011
replace dpp = ppr if _m==3 & fy > 2011
assert dpp!=.

loc int 2011
gen dppXpost`int' = dpp* post`int'
loc pnltprs1 dppXpost`int'

loc wgt [aw=dischnum1]

loc file1 ols_dpp_pv
loc file2 ols_dpp_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

loc n 1

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  foreach yv of varlist shref {
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
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 , cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  foreach yv of varlist refhhi qual_read {
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

*--------------------------------------------
*5) estimate the dynamic penalty pressure interacted with post (with only the post-2011 data and no interaction with dpp and post)
*--------------------------------------------

use ivpenalty_VI_agg3c_nosw, clear

*merge with dynamimc penalty rate
assert ppr==spp
drop ppr
merge 1:1 provid fy using predict_pnltprs_dynamic, keep(1 3)
*_m=1 for years 2009-11

gen dpp = spp if fy<=2011
replace dpp = ppr if _m==3 & fy > 2011
assert dpp!=.
sum `outcome' if fy >=2011

loc int 2011
gen dppXpost`int' = dpp* post`int'
loc pnltprs1 dpp
*loc pnltprs1 dppXpost`int'

keep if fy >= 2011

loc wgt [aw=dischnum1]

loc file1 ols_dpp_pv
loc file2 ols_dpp_ci
forval sn = 1/2 {
  capture erase `reg'/`file`sn''.xls
  capture erase `reg'/`file`sn''.txt
}
loc stat1 pv
loc stat2 ci

loc n 1

*for different outcomes, use hospital- vs SNF-level specification
forval sn = 1/2 {
  foreach yv of varlist shref {
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
  xtreg `yv' `pnltprs`n'' `sp_hosp' if vi_snf2008==0 & bad==0 , cluster(provid) fe
  *mean dep var
  sum `yv' if e(sample)
  loc mdv: display %9.3f `r(mean)'

  `out' keep(`pnltprs`n'') addtext(Mean dep. var., `mdv', Hospital FE, Y, Year FE, Y, Hospital and SNF market characteristics, Y , Lagged SNF ownership, Y , Patient democratics and comorbidity, Y) dec(3) fmt(fc)

  *--------------------
  foreach yv of varlist refhhi qual_read {
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

*How much year/year variation do we have in the penalty rate within a hospital?
bys provid: egen mean = mean(dpp)
bys provid: egen sd = sd(dpp)
gen cov = sd / mean

preserve
collapse (mean) cov, by(provid)

hist cov, xti(Coefficient of variation) ti("Coefficient of variation in dynamic penalty pressure, 2011-16" "in each hospital", size(medium)) frac
graph export `gph'/cov_dpp.eps, replace
restore
