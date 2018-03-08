*create index admission data for each condition-hospital-FY-DRG

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

loc f index_freq_DRG.csv
insheet using `f', comma names clear
tempfile index
save `index'

*aggregate across months to the hospital-FY level
use `index', clear

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

collapse (sum) count, by(condition provid fy drgcd)

sum fy
loc fm = `r(min)'
loc lm = `r(max)'

tempfile tmp
save `tmp'

*create a base FY list & expand
keep condition provid drgcd
duplicates drop
loc g = `lm' - `fm' + 1
expand `g'
bys cond provid drgcd: gen fy = `fm' + _n-1

merge 1:1 cond provid drgcd fy using `tmp'

*fill in zero's if unmatched
sort cond provid drgcd fy
replace count = 0 if _m==1

drop _m

compress
save index_admit_DRG_chy, replace
