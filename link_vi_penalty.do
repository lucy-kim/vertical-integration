* link each hospital in the cost report data with VI measure for each PAC, with the readmission penalty data

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'
loc gph /home/hcmg/kunhee/Labor/gph

*first, import the raw hospital-level overall readmission penalty rate data into Stata
insheet using HRRP/readmissions-year-3.csv, comma names clear
gen str6 prvdr_num = string(providerid, "%06.0f")
drop providerid
count
*3383 hospitals in total

* recode "not assessed" values in the penalty rate to missing value before destringing
tab fy2013readmis if regexm(fy2013readmis, "[^0-9 .]")
foreach v of varlist fy201?readmis* {
  replace `v' = "" if `v'=="Not Assessed"
}
destring fy201?readmis* , replace ignore("%")

* drop hospitals not subject to penalty, i.e. penalty = missing value
forval y=2013/2015 {
  drop if fy`y'readmis==.
  rename fy`y'readmis penalty`y'
}

tempfile penalty
save `penalty'

use `penalty', clear
*merge with hospital-level vertical integration data
rename prvdr_num prov_num
merge 1:m prov_num using hospvi
* 62 have _m=1; 140K have _m=2; 495K have _m=3

keep if _m==3
drop _merge

tempfile an
save `an'
*-----------
*Q. Does a hospital change a vertical integration status with a PAC type during the sample period?
use `an', clear
keep if pac=="hha" | pac=="hospice" | pac=="irf" | pac=="snf" | pac=="swbsnf"

*create a switcher dummy for each hospital
preserve
collapse (mean) vi, by(prov_num pac)
gen vi_sw = vi!=0 & vi!=1
drop vi
tab pac, summarize(vi_sw)
tempfile vi_sw
save `vi_sw'
restore

merge m:1 prov_num pac using `vi_sw', nogen

tempfile an2
save `an2'
*-----------
*Q. Does a hospital change a vertical integration status by penalty level?

*keep years 2013-2014
use `an2', clear

*focus on one PAC at a time
loc pp hha
keep if pac=="`pp'"

*reshape wide so that I have separate variables for VI in 2013, 2014, ...
keep prov_num fyear vi penalty*
reshape wide vi, i(prov_num penalty*) j(fyear)

*when you had no VI in 2010 (ACA enacted), how likely will you become VI afterwards?
forval y = 2011/2015 {
    tab vi`y' if vi2010==0
}

*when you already had VI in 2010 (ACA enacted), how likely will you remain VI afterwards?
forval y = 2011/2015 {
    tab vi`y' if vi2010==1
}

*----
*separately for hospitals that were VI or not VI in 2013, what's the probability of being VI in 2014?

*when you had no VI in 2013 (first penalty applied), how likely will you become VI afterwards?
forval y = 2014/2015 {
    tab vi`y' if vi2013==0
}

*when you already had VI in 2013 (first penalty applied), how likely will you remain VI afterwards?
forval y = 2014/2015 {
    tab vi`y' if vi2013==1
}

* distribution of penalty by whether the hospital was VI in 2010
forval y = 2013/2015 {
  twoway kdensity penalty`y' if vi2010==0 || kdensity penalty`y' if vi2010==1,  legend(order(1 "VI-`pp' = 0 in 2010" 2 "VI-`pp' = 1 in 2010")) yti("Density") xti("`y' Readmission penalty rate (%)")
  graph export `gph'/penalty`y'_vi_`pp'2010.eps, replace
}

sum penalty2013 if vi2013==0 & vi2014==1
sum penalty2013 if vi2013==0 & vi2014==0

sum penalty2013 if vi2013==0 & vi2015==1
sum penalty2013 if vi2013==0 & vi2015==0

*-----
*Q. scatterplot of penalty and hospital-based PAC volume measure
use `an', clear
keep if pac=="hha" | pac=="snf" | pac=="swbsnf"

gen tothhepi = totepi_st +totepi_out

* HHA
preserve
loc p "hha"
keep if pac=="`p'"
drop if fyear < 2013

loc y_`p' tothhepi
loc `y_`p''_lab "Number of home health episodes"
loc `y_`p''_ti "Total number of home health episodes in each FY and penalty rate"
forval y = 2013/2015 {
  qui sum `y_`p'' if fyear==`y', de
  loc p99 = `r(p99)'
  drop if `y_`p'' > `p99' & fyear==`y'
}
tab fyear
binscatter `y_`p'' penalty2013, by(fyear) xti("2013 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2013) label(2 2014) label(3 2015) col(3))
graph export `gph'/sc_vol_penalty2013_`p'.eps, replace

binscatter `y_`p'' penalty2014 if fyear >=2014, by(fyear) xti("2014 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2014) label(2 2015) col(2))
graph export `gph'/sc_vol_penalty2014_`p'.eps, replace

binscatter `y_`p'' penalty2015 if fyear >=2015, by(fyear) xti("2015 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2015) )
graph export `gph'/sc_vol_penalty2015_`p'.eps, replace
restore

* SNF
preserve
loc p "snf"
keep if pac=="`p'"
drop if fyear < 2013

loc y_`p' snfdays
loc `y_`p''_lab "Number of SNF days"
loc `y_`p''_ti "Number of SNF days in each FY and penalty rate"
forval y = 2013/2015 {
  qui sum `y_`p'' if fyear==`y', de
  loc p99 = `r(p99)'
  drop if `y_`p'' > `p99' & fyear==`y'
}
tab fyear
binscatter `y_`p'' penalty2013, by(fyear) xti("2013 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2013) label(2 2014) label(3 2015) col(3))
graph export `gph'/sc_vol_penalty2013_`p'.eps, replace

binscatter `y_`p'' penalty2014 if fyear >=2014, by(fyear) xti("2014 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2014) label(2 2015) col(2))
graph export `gph'/sc_vol_penalty2014_`p'.eps, replace

binscatter `y_`p'' penalty2015 if fyear >=2015, by(fyear) xti("2015 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2015) )
graph export `gph'/sc_vol_penalty2015_`p'.eps, replace
restore


* swing bed SNF
preserve
loc p "swbsnf"
keep if pac=="`p'"
drop if fyear < 2013

loc y_`p' swbsnfdays
loc `y_`p''_lab "Number of swing bed-SNF days"
loc `y_`p''_ti "Number of swing bed-SNF days in each FY and penalty rate"
forval y = 2013/2015 {
  qui sum `y_`p'' if fyear==`y', de
  loc p99 = `r(p99)'
  drop if `y_`p'' > `p99' & fyear==`y'
}
tab fyear
binscatter `y_`p'' penalty2013, by(fyear) xti("2013 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2013) label(2 2014) label(3 2015) col(3))
graph export `gph'/sc_vol_penalty2013_`p'.eps, replace

binscatter `y_`p'' penalty2014 if fyear >=2014, by(fyear) xti("2014 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2014) label(2 2015) col(2))
graph export `gph'/sc_vol_penalty2014_`p'.eps, replace

binscatter `y_`p'' penalty2015 if fyear >=2015, by(fyear) xti("2015 Penalty rate (%)") yti("``y_`p''_lab'") ti("``y_`p''_ti'", size(medium)) legend(label(1 2015) )
graph export `gph'/sc_vol_penalty2015_`p'.eps, replace
restore
