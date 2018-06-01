*create PAC referral data showing # referrals (i.e. hosp discharges appear in the PAC claims data within 5 days from hosp discharge) for each PAC type (SNF/HHA), condition, hospital, PAC provider, month

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/results_index_snf_counts_May_25

*for SNF
loc f "3.match_freq_SNF_May_30.csv"
insheet using `f', comma names clear
rename provider pacprovid

*PAC provider # is not numeric, contains alphabet - destring
/* gen x = real(pacprovid)
gen l = length(pacprovid)
assert l==6
gen alpha = substr(pacprovid,3,1)
tab alpha
drop if x==.
drop l x alpha
destring pacprovid, replace
*/
*U W Y Z "?"
*CCN containing "Z" :  Swing-Bed Designation for Critical Access Hospitals  https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf

*rename vars before merging with index admissions data
foreach v of varlist dischnum-black {
  rename `v' `v'_pac
}

gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
*606 - 671
format ym %tm

tempfile tmp
save `tmp'

*create a base monthly list & expand
keep pac condition provid
duplicates drop
loc g = `lm' - `fm' + 1
di `g'
expand `g'
sort pac cond provid
bys pac cond provid: gen ym = `fm' + _n-1
format ym %tm

merge 1:m pac cond provid ym using `tmp'
*if _m==1, pacprovid is missing

*fill in zero's if unmatched; if there is no PAC referral, the # referral should be 0 for that PAC type-condition-month cell
sort pac cond provid ym pacprovid
foreach v of varlist *_pac {
  replace `v' = 0 if _m==1
  *2012 June: exclude
  replace `v' = . if dischyear==2012 & dischmth==6
}
assert dischnum_pac==0 if _m==1
assert dischnum_pac==0 if pacprovid==.

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

replace cond = "AMI" if cond=="MI"

/* *restrict to SNF (https://www.resdac.org/sites/resdac.umn.edu/files/Provider%20Number%20Table.txt)
gen sid = string(pacprovid, "%06.0f")
gen last4 = substr(sid,3,4)
destring last4, replace
count if last4 >= 5000 & last4 <= 6499
keep if last4 >= 5000 & last4 <= 6499
drop sid last4 */

compress
save `dta'/Medicare/SNFreferral_tchpm, replace

*-------------------
*aggregate up to the year level
use `dta'/Medicare/SNFreferral_tchpm, clear

collapse (sum) dischnum_pac-black_pac, by(cond provid pacprovid fy)

compress
save `dta'/Medicare/SNFreferral_tchpy, replace
