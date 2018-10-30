*create SNF quality (deficiency counts) data from the SNF Compare archive database
* source: 1) NHC Provider and Deficiency Files 2006-2011 from https://www.cms.gov/medicare/provider-enrollment-and-certification/certificationandcomplianc/fsqrs.html
*2) NHC Provider and Deficiency File 2012 from https://www.cms.gov/Medicare/Provider-Enrollment-and-Certification/CertificationandComplianc/Downloads/NHCPrvdrAndDef.zip
*3) deficiency files for 2013-2016 (annual files) from  https://data.medicare.gov/data/archives/nursing-home-compare

*see technical guide for 5-star rating: https://www.cms.gov/Medicare/Provider-Enrollment-and-Certification/CertificationandComplianc/Downloads/usersguide.pdf

loc out /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/snf-compare

*use 2008-2011 data

forval y=2007/2011 {
  loc f prov`y'_nodupkits.xls
  import excel using `f', clear firstrow

  *use the earliest possible survey data if multiple records exist for a provider
  destring SURVEY, replace
  bys PROVNUM: egen smaller = min(SURVEY)
  bys PROVNUM: gen nn = _N
  drop if nn > 1 & SURVEY > smaller

  *CURHDEF = Total number of health deficiencies on current survey
  *CURLDEF = Total number of life safety code defs on current survey
  assert CURHDEF+CURLDEF==DEFNUM
  keep PROVNUM PROVNUM CURHDEF ZIP

  *use CURHDEF (health deficiencies) because they're reported in later years too
  renvars PROVNUM CURHDEF \ provid defcnt
  duplicates drop

  *gen x =real(provid)
  *drop if x==.
  *drop x
  *destring provid, replace

  gen yr = `y'

  duplicates tag provid, gen(dup)
  assert dup==0
  drop dup

  gen ll =length(provid)
  assert ll==6
  drop ll

  tempfile snfqual`y'
  save `snfqual`y''
}
*-----------------------
*2012
loc y 2012
loc f prov`y'_nodupkits.xlsx
import excel using `f', clear firstrow

destring DEFNUM CURHDEF CURLDEF SURVEY, replace

*use the earliest possible survey data if multiple records exist for a provider
bys PROVNUM: egen smaller = min(SURVEY)
bys PROVNUM: gen nn = _N
drop if nn > 1 & SURVEY > smaller

*CURHDEF = Total number of health deficiencies on current survey
*CURLDEF = Total number of life safety code defs on current survey
assert CURHDEF+CURLDEF==DEFNUM
keep PROVNUM PROVNUM CURHDEF ZIP

*use CURHDEF (health deficiencies) because they're reported in later years too
renvars PROVNUM CURHDEF \ provid defcnt
duplicates drop

*gen x =real(provid)
*drop if x==.
*drop x
*destring provid, replace

gen yr = `y'

duplicates tag provid, gen(dup)
assert dup==0
drop dup

gen ll =length(provid)
assert ll==6
drop ll

tempfile snfqual`y'
save `snfqual`y''
*-----------------------
*2013-2016: use cycle 1 (most recent period) total # health deficiency counts from the standard survey (excludes complaint survey deficienciy counts)
*these data are in the quarterly level: use Q1 data

forval y = 2013/2016 {
  loc f ProviderInfo_`y'.csv
  insheet using `f', comma names clear
  capture keep if quarter=="`y'Q1"
  capture keep if filedate=="`y'-01-01"
  *assert cycle_1_nfromdefs+ cycle_1_nfromcomp==cycle_1_defs

  keep provnum cycle_1_nfromdefs zip
  renvars provnum cycle_1_nfromdefs \ provid defcnt

  *gen x =real(provid)
  *drop if x==.
  *drop x
  *destring provid, replace

  gen yr= `y'

  gen ll =length(provid)
  assert ll==6
  drop ll

  tempfile snfqual`y'
  save `snfqual`y''
}

*-----------------------
clear
forval y = 2007/2016 {
  append using `snfqual`y''
}
destring ZIP, replace
gen zipcode = ZIP if ZIP!=.
replace zipcode = zip if zip!=.
assert zipcode!=.

tab yr
keep provid yr defcnt zipcode

renvars provid yr \ pacprovid fy

duplicates drop

duplicates tag pacprovid fy, gen(dup)
assert dup==0
drop dup

tab fy

compress
save snf_deficiency.dta, replace
