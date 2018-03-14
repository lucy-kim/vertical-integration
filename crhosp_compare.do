*use hospital compare data for hospital level risk-adjusted readmission rate data during a 3-year window period

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/hosp-compare

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

gen fy = 2011
tempfile read0810
save `read0810'

*-----------------------

loc f "hospcompare_read_201307"
insheet using `f'.csv, clear comma names

gen condition = "HF" if measurename=="Heart Failure (HF) 30-Day Readmissions"
replace cond = "AMI" if measurename=="Acute Myocardial Infarction (AMI) 30-Day Readmissions"
replace cond = "PN" if measurename=="Pneumonia (PN) 30-Day Readmissions"
drop measurename

foreach v of varlist excessreadmis-numberofreadm {
  replace `v' = "" if `v'=="N/A"
  capture destring `v', replace
}

drop footnote
