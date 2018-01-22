*create hospital panel data for each calendar year

cd /home/hcmg/kunhee/Labor/Bayada_data

*append all years of Cost report data
use hospcr2010_2552_10, clear
append using hospcr2011_2552_10
forval y = 2012/2016 {
    append using hospcr`y'
}
replace prov_num = prvdr_num if prov_num==""
assert prov_num==prvdr_num
drop prvdr_num
assert fyear!=.

duplicates tag prov_num fyear, gen(dup)
tab dup

*recode some vars as 0/1 dummies
lab define provtype 1 "general short-term" 2 "general long-term" 3 "cancer" 4 "psychiatric" 5 "rehab" 6 "religious non-medical" 7 "children" 8 "alcohol/drug" 9 "other", replace

foreach f in "hosp" "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  *convert date variable to Stata date
  loc v `f'_dtcert
  capture split `v', destring parse(/)
  capture gen a = mdy(`v'1, `v'2, `v'3)
  capture drop `v'*
  capture rename a `v'
  capture format `v' %d

  capture lab var `f'_name "`f' name"
  capture lab var `f'_ccn "`f' CMS provider ID"
  capture lab var `f'_cbsa "`f' CBSA code"
  capture lab var `f'_dtcert "`f' date of certification"
  capture lab var `f'_provtype "`f' provider type"
  capture lab val `f'_provtype provtype
}

foreach v of varlist cr_start cr_end {
  split `v', destring parse(/)
  gen a = mdy(`v'1, `v'2, `v'3)
  drop `v'*
  rename a `v'
  format `v' %d
}

foreach v of varlist teaching ltch ltch_part cah req_malprins dissh uncomp1 uncomp2 {
  tab `v'
  assert `v'=="Y" | `v'=="N" | `v'==""

  gen a = `v'=="Y"
  replace a = . if `v'==""
  drop `v'
  rename a `v'
  lab var `v' "=1 if hosp is `v'"
}
lab var uncomp1 "=1 if receive uncompensated care payment before 10/1 during the cost reporting period"
lab var uncomp2 "=1 if receive uncompensated care payment on/after 10/1 during the cost report period"
lab var ltch_part "=1 if ltch co-located within another hospital during the CR period"

loc v urban
tab `v'
gen a = `v'==1
replace a = . if `v'==.
drop `v'
rename a `v'
lab var `v' "=1 if hosp is `v'"

*variable labels
lab define ll 1 "claim-made" 2 "occurrence", replace
lab val malprins_policy ll
lab var malprins_policy "is malpractice insurance claims-made or occurrence policy"
lab var req_malprins "=1 if hosp legally required to carry malpractice insurance"
lab var beds "# beds"
lab var dischrg "# discharges"
lab var totepi_st "total number of episodes (standard/nonoutlier) in hosp-based HHA"
lab var totepi_out "otal number of outlier episodes in hosp-based HHA"
lab var malprins_premium "amount of malpractice insurance premiums"
lab var malprins_paidloss "amount of malpractice insurance paid losses"
lab var malprins_selfins "amount of malpractice insurance self insurance"
lab var cr_start "cost reporting period start"
lab var cr_end "cost reporting period end"
lab var dissh "dummy for disproportionate share hospital adjustment"

*drop variables with mostly missing values
drop fy_end_dt fy_bgn_dt

*drop unnecessary vars
drop adr_vndr_cd fi_num trnsmtl_num rpt_stus_cd

drop if state=="PR" | state=="MP" | state=="GU" | state=="VI"

*even if there are duplicates per prov_num-FY, it seems to cover different CR periods (see cr_start, cr_end vars), so don't worry
drop dup


compress
save hospcr_panel_2010_2016, replace
