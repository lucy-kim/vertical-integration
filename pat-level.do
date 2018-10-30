* patient-level analysis: probability of referral to SNF
* note remove brackets [] in all the code below

ssc install outreg2

cd [directory address where your data are, e.g. Meng/patient-analysis/...]

*please save your individual-level SAS data file as CSV file first
insheet using [data-file-name.csv], comma names clear

*define macros
loc reform EHRstage1 EHRstage2 HACstatus bpci
loc sp_hosp _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr vi_snf_l

loc dv [put a dependent variable = 0/1 indicator for being referred to SNF]
loc idv _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr vi_snf_l ... [in the "..." part, put a list of patient-level variables without commas]

*drop if the patient has LOS < 3 days: assuming discharge date variable name = "discharge_date", admission date variable name = "admit_date"
drop if discharge_date – admit_date < 3

*provid is a hospital provider ID
areg `dv' sppXpost2011 `idv', cluster(provid) absorb(provid)
sum `yv' if e(sample)
loc mdv: display %9.3f `r(mean)'

outreg2 using pat_reg_pv.xls, append label pv addtext(Mean dep. var., `mdv')
outreg2 using pat_reg_ci.xls, append label ci addtext(Mean dep. var., `mdv')
