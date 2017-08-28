* for hospital-CY, get vertical integration measure
* dummy for VI, mix of PACs if VI=1, # providers per PAC type

loc reg /home/hcmg/kunhee/Labor/regresults

cd /home/hcmg/kunhee/Labor/Bayada_data

use hospcr_panel, clear

keep prov_num hospname state fyear urban beds dischrg cr_* *_name *_ccn teaching ltch* cah uncomp* dissh own_* totepi_st totepi_out hosp_provtype

* dummy for VI with each type of PAC

foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  gen vi_`f' = `f'_ccn!=""
  destring `f'_ccn, replace
}

*do we have data for the entire period covered for each hospital?
sort prov_num cr_start cr_end
gen gap = 0
bys prov_num: replace gap = cr_start != cr_end[_n-1] + 1 if _n > 1

*if the previous month is Feb & the next month is march, it's continuous
bys prov_num: replace gap = 0 if month(cr_start)==3 & month(cr_end[_n-1])==2 & gap==1

*how many hospitals have a gap in coverage?
bys prov_num: egen discont = max(gap)
preserve
keep prov_num discont
duplicates drop
tab discont
* 32 / 6,361 hospitals do not have the whole period continuously covered
restore

*If the hospital skips one year and resume in the next year, then tag it as ok
bys prov_num: gen ok = (cr_start==cr_end[_n-1] + 365 | cr_start==cr_end[_n-1] + 366 | cr_start==cr_end[_n-1] + 367) & gap==1

tempfile an
save `an'

*drop 32 hospitals for now that have periods discontinuously covered
use `an', clear
drop if discont==1


/* * append rows so that each hospital has at least one row per fyear
use `an', clear
keep prov_num
duplicates drop
loc ss 2011
loc ee 2016
loc dd = `ee' - `ss' + 1
expand `dd'
bys prov_num: gen fyear = `ss' + _n-1
tempfile fy
save `fy'

use `an', clear
merge m:1 prov_num fyear using `fy'

* recode the newly added years as (not) having a VI PAC if the year before and after have the VI indicator = 1 (0); otherwise, just recode as 0
sort prov_num fyear cr_start cr_end
foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  bys prov_num: gen same = (vi_`f'[_n-1]==1 & vi_`f'[_n+1]==1) | (vi_`f'[_n-1]==0 & vi_`f'[_n+1]==0) & _n > 1
  replace vi_`f' = vi_`f'[_n-1] if _merge==2 & same==1
  replace vi_`f' = 0 if _merge==2 & same==0
  drop same
}
*fill in the cost reporting period start and end
bys prov_num: replace cr_start = cr_end[_n-1]+1 if _merge==2
bys prov_num: replace cr_end = cr_start[_n+1]-1 if _merge==2
assert cr_start!=.

*fill in other variables
sort prov_num
bys prov_num: replace

*drop 2016 b/c incomplete
drop if fyear==2016 */

* want to have a hospital appear once per year; to do this, spread out to daily observations and create a hospital-CY level dataset
keep prov_num cr_start cr_end vi_* ltch* beds cah teaching own_* hosp_provtype

duplicates drop
gen diff = cr_end - cr_start + 1
expand diff
bys prov_num cr_start cr_end: gen date = cr_start + _n - 1
format date %d
gen cy = year(date)

list prov_num cr_* date cy *snf* *hha *irf* *ltch* in 1/30

tempfile an2
save `an2'

collapse (mean) vi_* beds ltch* cah teaching own_* hosp_provtype, by(prov_num cy)

tempfile an3
save `an3'

use `an3', clear

*restrict to general short-term hospital
keep if hosp_provtype ==1
drop hosp_provtype

* if had a VI for at least 1 quarter during the CY, recode as being VI (this affects only a minority; mostly either have VI or no VI throughout the entire CY)
foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  gen dvi_`f' = vi_`f' >= 0.25
  assert dvi_`f'==0 | dvi_`f'==1
  drop vi_`f'
  rename dvi_`f' vi_`f'
}

foreach v of varlist ltch ltch_part teaching own_* cah {
  gen d`v' = `v' >= 0.25
  replace d`v' = . if `v'==.
  drop `v'
  rename d`v' `v'
}

*restrict to CY 2011 - 2016 which are usually covered entirely by the 2011-2016 CR data
tab cy, summarize(vi_snf)
keep if cy >= 2011 & cy <= 2016

*for each year, % hospitals with each PAC type
bys cy: sum vi_* ltch ltch_part teaching own* cah beds

*across years, the trend in VI for each PAC type

*get state for each hospital
preserve
use hospcr_panel, clear
keep if hosp_provtype==1
keep prov_num state
duplicates drop
duplicates tag prov_num, gen(dd)
assert dd==0
drop dd
tempfile state
save `state'
restore

merge m:1 prov_num using `state', keep(3) nogen

*reshape long
reshape long vi_, i(prov_num cy state) j(pac) string
rename vi_ vi
tab pac, summarize(vi)
sum ltch*
* none of them have ltch_part = 1 b/c i restricted to short-term general hospitals

compress

preserve
outsheet using hosp_vi.csv, replace names comma
restore

save hospvi, replace

*---------
*Q. Does a hospital change a vertical integration status with a PAC type during the sample period?

use hospvi, clear
*4858 general short-term hospitals
*restrict to 4 PAC types
keep if pac=="hha" | pac=="hospice" | pac=="irf" | pac=="snf"

sort prov_num pac cy
list prov_num cy pac vi in 1/30

collapse (mean) vi, by(prov_num pac)
*--------
*Q. Does the same hospital typically vertically integrate with more than one PAC?

use hospvi, clear
*4858 general short-term hospitals
*restrict to 4 PAC types
keep if pac=="hha" | pac=="hospice" | pac=="irf" | pac=="snf"

keep if vi==1

*throughout the sample period, what are the integrated PACs for each hospital?
keep prov_num pac
duplicates drop

*within a hospital, sort PAC type in an alphabetical order
forval x = 1/4 {
  sort prov_num pac
  bys prov_num: gen pac`x' = pac if _n==`x'
  loc v pac`x'
  gsort prov_num -`v'
  bys prov_num: replace `v' = `v'[_n-1] if `v'==""
}

*concatenate the discipline combination
gen pac_comb = pac1 + "-" + pac2 + "-" + pac3 + "-" + pac4

keep prov_num pac_comb
duplicates drop

tab pac_comb, sort

label var pac_comb "Vertically integrated post-acute care combination"

estpost tabulate pac_comb, sort
esttab using `reg'/pac_comb_dist.csv, cells("b(label(Counts)) pct(label(Percent) fmt(2)) cumpct(label(Cumulative %) fmt(2))") varlabels(, blist(Total "{hline @width}{break}")) nonumber nomtitle noobs replace label
