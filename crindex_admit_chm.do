*create index admission data for each condition-hospital-month

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

loc f index_freq.csv
insheet using `f', comma names clear
tempfile index
save `index'

*aggregate across destinations (ddest=06 for HHC, 03 for SNF)
use `index', clear
collapse (mean) dischnum white black read* ses_score, by(condition provid dischyear dischmth)

gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
*576 - 671
format ym %tm

tempfile tmp
save `tmp'

*create a base monthly list & expand
keep condition provid
duplicates drop
loc g = `lm' - `fm' + 1
expand `g'
bys cond provid: gen ym = `fm' + _n-1

merge 1:1 cond provid ym using `tmp'

*fill in zero's if unmatched
sort cond provid ym
replace dischnum = 0 if _m==1

*2012 June:
foreach v of varlist dischnum-ses_score {
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
save index_admit_chm, replace
