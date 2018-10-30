*create index admission and SNF referral data on number of patients in each age bin for each condition-hospital-month

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/6-7

loc f index_agebins_June_7th.csv
insheet using `f', comma names clear

gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
*576 - 677
format ym %tm

collapse (sum) _65_74-_95p total_, by(cond provid ym dischyear dischmth)

tempfile tmp
save `tmp'

sort cond provid ym

*create a base monthly list & expand
keep condition provid
duplicates drop
loc g = `lm' - `fm' + 1
expand `g'
bys cond provid: gen ym = `fm' + _n-1

merge 1:m cond provid ym using `tmp'

*fill in zero's if unmatched
sort cond provid ym
foreach v of varlist _65_74-total {
  replace `v' = 0 if _m==1
  *2012 June: exclude
  replace `v' = . if (dischyear==2012 & dischmth==6) | ym==ym(2012,6)
}

gen date = dofm(ym)
replace dischyear = year(date) if dischyear==.
replace dischmth = month(date) if dischmth==.

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2007/2016 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert fy!=.

drop _m date

compress
save `dta'/Medicare/index_admit_agebin_chm, replace

collapse (sum) _65_74-total, by(cond provid fy)

replace cond = upper(cond)
replace cond = "AMI" if cond=="MI"

compress
save `dta'/Medicare/index_admit_agebin_chy, replace

*-------------------------------
loc f snf_agebins_June_6th.csv
insheet using `f', comma names clear
rename provider pacprovid

*rename vars before merging with index admissions data
foreach v of varlist _75_84-_95p {
  rename `v' `v'_pac
}

gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
*606 - 671
format ym %tm

tempfile tmp2
save `tmp2'

*create a base monthly list & expand
keep pac condition provid
duplicates drop
loc g = `lm' - `fm' + 1
di `g'
expand `g'
sort pac cond provid
bys pac cond provid: gen ym = `fm' + _n-1
format ym %tm

merge 1:m pac cond provid ym using `tmp2'
*if _m==1, pacprovid is missing

*fill in zero's if unmatched; if there is no PAC referral, the # referral should be 0 for that PAC type-condition-month cell
sort pac cond provid ym pacprovid
foreach v of varlist *_pac {
  replace `v' = 0 if _m==1
  *2012 June: exclude
  replace `v' = . if (dischyear==2012 & dischmth==6) | ym==ym(2012,6)
}
assert _85_94_pac==0 if _m==1
assert _85_94_pac==0 if pacprovid==.

gen date = dofm(ym)
replace dischyear = year(date) if dischyear==.
replace dischmth = month(date) if dischmth==.

*create quarters using months
gen qtr = .
forval x = 1/4 {
  replace qtr = `x' if dischmth >= 1+(`x'-1)*3 & dischmth <= 3*`x'
}

*create FY (ending in June) using months
gen fy = .
forval x = 2007/2016 {
  loc y = `x'-1
  replace fy = `x' if (dischyear==`y' & qtr>=3 & qtr <=4) | (dischyear==`x' & qtr>=1 & qtr <=2)
}
assert fy!=.

drop _m date

replace cond = upper(cond)
replace cond = "AMI" if cond=="MI"

*restrict to SNF (https://www.resdac.org/sites/resdac.umn.edu/files/Provider%20Number%20Table.txt)
gen sid = string(pacprovid, "%06.0f")
gen last4 = substr(sid,3,4)
destring last4, replace
assert last4 >= 5000 & last4 <= 6499
keep if last4 >= 5000 & last4 <= 6499
drop sid last4

compress
save `dta'/Medicare/SNFreferral_agebin_tchpm_nosw, replace

collapse (sum) *_pac, by(cond provid pacprovid fy)

compress
save `dta'/Medicare/SNFreferral_agebin_tchpy_nosw, replace
