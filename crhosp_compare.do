*use hospital compare data for hospital level risk-adjusted readmission rate data during a 3-year window period

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/hosp-compare

* get ERRs based on 2008-2010 data
loc f "hospcompare_read_201107"
insheet using `f'.csv, clear comma names

keep if regexm(measure, "Readmission")

foreach v of varlist read lower upper n_pats {
  replace `v'="" if `v'=="N/A"
  destring `v', replace
}
destring provid, replace ig("'")

tab provid if regexm(provid, "F")
tab hospname if regexm(provid, "F")
assert regexm(hospname, "VA")==1 if regexm(provid, "F")
drop if regexm(provid, "F")

sum n_pats if read==.
*below 25

replace cond = "AMI" if cond=="Heart Attack"
replace cond = "HF" if cond=="Heart Failure"
replace cond = "PN" if cond=="Pneumonia"

drop measure footnote category
destring provid, ig("'") replace

gen fy = 2011
tempfile read0810
save `read0810'

*-----------------------
*national readmission rate for 2008-2010

loc f "hospcompare_read_201107_national"
insheet using `f'.csv, clear comma names

replace cond = "AMI" if cond=="Heart Attack"
replace cond = "HF" if cond=="Heart Failure"
replace cond = "PN" if cond=="Pneumonia"

keep if regexm(measure, "Readmission")
keep cond rate

merge 1:m cond using `read0810', nogen

gen err = read / rate
rename read numer
rename rate denom

keep cond provid err fy n_pats numer denom
duplicates drop
rename n_pats n

compress
save err2011, replace
*-----------------------
* get ERRs for CABG 2015 based on 2012-2014 data (from 2015 data)

loc f "Readmissions and Deaths - Hospital"
insheet using "HOSArchive_Revised_FlatFiles_20150716/`f'.csv", clear comma
tab measurename if regexm(measurename, "admission")
keep if regexm(measurename, "admission")

*provider with "F" are VAs - drop them
gen x = real(provid)
drop if x==.

keep provid measurename measureid measure*date score

gen cond = "AMI" if regexm(measureid, "AMI")
replace cond = "CABG" if regexm(measureid, "CABG")
replace cond = "COPD" if regexm(measureid, "COPD")
replace cond = "HF" if regexm(measureid, "HF")
replace cond = "PN" if regexm(measureid, "PN")
replace cond = "HK" if regexm(measureid, "HIP_KNEE")

keep if cond!=""
rename provid provid
rename score read
keep provid cond read

destring read, replace ig("Not Available")

tempfile numer
save `numer'

loc f "Readmissions and Deaths - National"
insheet using "HOSArchive_Revised_FlatFiles_20150716/`f'.csv", clear comma
tab measurename if regexm(measurename, "admission")
keep if regexm(measurename, "admission")

keep measurename measureid measure*date nationalrate

gen cond = "AMI" if regexm(measureid, "AMI")
replace cond = "CABG" if regexm(measureid, "CABG")
replace cond = "COPD" if regexm(measureid, "COPD")
replace cond = "HF" if regexm(measureid, "HF")
replace cond = "PN" if regexm(measureid, "PN")
replace cond = "HK" if regexm(measureid, "HIP_KNEE")

rename nationalrate rate
keep cond rate
keep if cond!=""
destring rate, replace

merge 1:m cond using `numer', nogen

gen err = read / rate
rename read numer
rename rate denom
gen fy = 2015
destring provid, replace

compress
save err2015, replace
