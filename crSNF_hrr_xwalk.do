*get HRR & HSA for each SNF in the Medicare claims data using POS data

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

cd `dta'/pos
forval y=2008/2016 {
  use pos`y', clear

  *keep hospitals
  capture rename prov0075 prvdr_ctgry_cd
  keep if prvdr_ctgry_cd=="02"

  * prov1680 prov2905 ssamsacd fipstate fipcnty prov3225 prov2885 prov0740 prov0755
  capture rename prov1680 provid_pac
  capture rename prvdr_num provid_pac

  capture rename prov2905 zip
  capture rename zip_cd zip

  capture rename prov0755 beds
  capture rename crtfd_bed_cnt beds

  capture rename prov2885 ownertype
  capture rename gnrl_cntl_typ ownertype

  keep provid_pac zip beds ownertype
  duplicates drop

  capture destring ownertype, replace
  gen own_fp = (ownertype >=1 & ownertype <= 3) | ownertype==13
  gen own_np = ownertype >=4 & ownertype <= 6
  gen own_gv = ownertype >=7 & ownertype <= 12
  drop ownertype

  capture destring provid, replace
  capture destring zip, replace

  gen fy = `y'

  des
  compress
  save snfpos`y', replace
}

*---------------------------
loc file `dta'/dartmouth/ZipHsaHrr14
insheet using `file'.csv, comma names clear
rename zip zip
tempfile zip_hrr
save `zip_hrr'

*---------------------------
clear
forval y=2008/2016 {
  append using snfpos`y'
}

merge m:1 zip using `zip_hrr', keep(1 3)
forval y=2008/2016 {
  qui count if _merge==1 & fy==`y'
  di "In Year `y': `r(N)' SNFs have unmatched ZIP codes."
}
keep if _m==3
drop _m

keep provid fy hrrnum
duplicates drop

preserve
keep provid
duplicates drop
expand 2016 - 2007
bys provid: gen fy = 2008 + _n-1
tempfile base
save `base'
restore

merge 1:1 provid fy using `base'
sort provid fy
bys provid: replace hrrnum = hrrnum[_n-1] if hrrnum >=.
gsort provid -fy
bys provid: replace hrrnum = hrrnum[_n-1] if hrrnum >=.
drop _m

rename provid_pac pacprovid

compress
save `dta'/Medicare/snf_hrr_xwalk.dta, replace




/* use SNFreferral_tchpm.dta, clear
keep */
