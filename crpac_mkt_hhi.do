*create PAC market concentration (HHI) at the hospital HRR / HSA level using the referral data created from the Medicare claims data (exclude the hospitals' own referrals when calculating the HHI)

loc path /ifs/home/kimk13/VI/data
cd `path'/Medicare

use SNFreferral_tchpm.dta, clear
drop if cond=="HK"

*fiscal year level HHI
collapse (sum) dischnum, by(provid pacprovid fy)

*merge with HRR data
rename provid provider
merge m:1 provider using `path'/dartmouth/hosp_hrr_xwalk, keep(3) nogen

tempfile match
save `match'

*for each hospital, leave out the own hospital's # referrals, so within the same HRR, different hospitals have different PAC market concentration
foreach g0 in "hrr" "hsa" {
  loc g `g0'num

  use `match', clear
  bys fy `g' pacprovid: egen tot = sum(dischnum)
  gen tot_hout = tot - dischnum
  *list fy `g' pacprovid provid dischnum tot* in 1/30
  drop if `g'==.

  *total market size for each year
  bys fy `g': egen tot2 = sum(dischnum)
  *how much contributed by each hospital
  bys fy `g' provid: egen tot2_h = sum(dischnum)
  gen tot2_hout = tot2 - tot2_h
  gen sh = (tot_hout/ tot2_hout)^2
  collapse (sum) pac_mkt_hhi = sh, by(fy `g' provid)
  assert pac_mkt_hhi >= 0 & pac_mkt_hhi <=1
  rename pac_mkt_hhi pac_hhi_`g0'

  tempfile hhi_`g'
  save `hhi_`g''
}

*reshape to HRR (HSA)-FY-hospital level data
use `hhi_hrrnum', clear
merge 1:1 provid fy using `hhi_hsanum', nogen

rename provider provid

compress
save `path'/pac_mkt_hhi, replace

/*
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
*/
