*create index admission data for each condition-hospital-month-discharge destination code

loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/corrected-hosplevel/results_index_snf_counts_May_25

loc f "1.index_freq_May25.csv"
insheet using `f', comma names clear
tempfile index
save `index'

gen ym = ym(dischyear, dischmth)
sum ym
loc fm = `r(min)'
loc lm = `r(max)'
*576 - 677
format ym %tm

tempfile tmp
save `tmp'

*create a base monthly list & expand
keep condition provid ddest
duplicates drop
loc g = `lm' - `fm' + 1
expand `g'
bys cond provid ddest: gen ym = `fm' + _n-1

merge 1:1 cond provid ym ddest using `tmp'

*fill in zero's if unmatched
sort cond provid ym ddest
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
save `dta'/Medicare/index_admit_chm_ddest, replace
