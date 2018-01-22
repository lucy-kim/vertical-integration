*create PAC market concentration (HHI) at the hospital HRR / HSA level using the referral data created from the Medicare claims data

loc path /ifs/home/kimk13/VI/data
cd `path'/Medicare

loc f match_freq_no16.csv
insheet using `f', comma names clear
rename provider pacprovid

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

*drop for now July-Dec 2015 b/c they belong to FY 2016
drop if dischyear ==2015 & dischm > 6
assert fy!=.

collapse (sum) dischnum, by(pac condition provid pacprovid fy)

*merge with HRR data
rename provid provider
merge m:1 provider using `path'/dartmouth/hosp_hrr_xwalk, keep(1 3) nogen

tempfile match
save `match'

*aggregate across hospitals the # referrals to the PAC provider-FY-market level for each PAC type & condition
foreach g of varlist hrrnum hsanum {
  use `match', clear
  collapse (sum) dischnum, by(pac condition pacprovid fy `g')
  drop if `g'==.

  *total market size for each year
  bys pac condition `g' fy: egen tot = sum(dischnum)
  gen sh = (dischnum / tot)^2
  collapse (sum) pac_mkt_hhi = sh, by(pac condition `g' fy)
  assert pac_mkt_hhi >= 0 & pac_mkt_hhi <=1
  tempfile hhi_`g'
  save `hhi_`g''
}

*reshape to HRR (HSA)-FY level data
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  use `hhi_`g'', clear
  des
  rename pac_mkt_hhi `g0'hhi_
  reshape wide `g0'hhi_, i(condition `g' fy) j(pac) string

  foreach v of varlist *SNF *HHA {
    rename `v' `v'_
  }

  reshape wide *hhi*, i(`g' fy) j(condition) string

  compress
  save `path'/`g0'hhi, replace
}
