*create hospital characteristics data from the cost report using 2010-2017 files

cd /home/hcmg/kunhee/Labor/Bayada_data

*append all years of Cost report data
use hospcr2010_2552_10, clear
append using hospcr2011_2552_10
forval y = 2012/2017 {
    append using hospcr`y'
}

*restrict to short-term general hospitals by looking at the last 4 digits fo provider number
*0001-0879 from https://www.resdac.org/sites/resdac.umn.edu/files/Provider%20Number%20Table.txt
gen last4 = substr(prov_num,3,4)
gen nlast4 = real(last4)
gen stg = nlast4 >= 0 & nlast4 <= 879
tab fyear if stg==1
keep if stg==1
drop stg *last4
drop if cah=="Y"

* dummy for VI with each type of PAC
foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  gen vi_`f' = `f'_ccn!=""
  destring `f'_ccn, replace
}
sum vi*

*for # beds, recode to missing if > 3000 (only 4 hosps)
replace beds = . if beds > 3000

foreach v of varlist teaching uncomp* dissh {
  gen x_`v' = `v'=="Y"
  replace x_`v' = . if `v'==""
  drop `v'
  rename x_`v' `v'
}
drop uncomp2

*urban = 1 for urban ; 2 for rural
gen x_urban = urban==1
replace x_urban = . if urban==.
drop urban
rename x_urban urban

*use fyear as the year basis
collapse (max) vi_* teaching urban own_* uncomp* dissh (mean) *rev* *inc* beds dischrg snfdays swbsnfdays totepi_st totepi_out , by(prov_num state fyear)

*create hospital size category
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 500
replace size = 3 if beds > 500
assert size!=.
replace size = . if beds==.

keep if fy > 2010 & fy < 2016

compress
saveold hosp_chars_cr, replace
