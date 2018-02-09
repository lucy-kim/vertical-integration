*create PAC referral data showing # referrals (i.e. hosp discharges appear in the PAC claims data within 5 days from hosp discharge) for each PAC type (SNF/HHA), condition, hospital, PAC provider, month


loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

loc f match_freq_no16.csv
insheet using `f', comma names clear
rename provider pacprovid

*rename vars before merging with index admissions data
foreach v of varlist dischnum-samh90 {
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
expand `g'
bys pac cond provid: gen ym = `fm' + _n-1
format ym %tm

merge 1:m pac cond provid ym using `tmp'

*fill in zero's if unmatched; if there is no PAC referral, the # referral should be 0 for that PAC type-condition-month cell
sort pac cond provid ym pacprovid
foreach v of varlist *_pac {
  replace `v' = 0 if _m==1
  *2012 June: exclude
  replace `v' = . if dischyear==2012 & dischmth==6
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
save PACreferral_tchpm, replace
