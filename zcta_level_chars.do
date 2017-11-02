* create ZCTA-level cross-sectional data on various characteristics using data from Census Bureau American FactFinder

cd /home/hcmg/kunhee/vertical-int/data/census

*total popluation & pop over age 65 for each ZCTA
loc x B01001
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01 hd01_vd20 hd01_vd21 hd01_vd22 hd01_vd23 hd01_vd24 hd01_vd25 hd01_vd44 hd01_vd45 hd01_vd46 hd01_vd47 hd01_vd48 hd01_vd49

drop in 1/1

destring geoid2, gen(zcta5)
rename hd01_vd01 totpop

foreach v of varlist hd* totpop {
    destring `v', replace
}

egen pop_gt65 = rowtotal(hd01_vd20 hd01_vd21 hd01_vd22 hd01_vd23 hd01_vd24 hd01_vd25 hd01_vd44 hd01_vd45 hd01_vd46 hd01_vd47 hd01_vd48 hd01_vd49)

gen pct_pop_gt65 = pop_gt65 / totpop

drop hd* geoid2

tempfile pop_gt65
save `pop_gt65'

*-------------------------
loc x B02001
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01 hd01_vd02 hd01_vd03 hd01_vd05 hd01_vd08

drop in 1/1

destring geoid2, gen(zcta5)

foreach v of varlist hd* {
    destring `v', replace
}
egen sum = rowtotal(hd01_vd02 hd01_vd03 hd01_vd05 hd01_vd08)
gen pct = sum / hd01_vd01
sum pct

rename hd01_vd01 totpop
rename hd01_vd02 white
rename hd01_vd03 black
rename hd01_vd05 asian
rename hd01_vd08 multirace
drop pct sum geoid2

foreach v of varlist white black asian multirace {
  gen pct_`v' = `v' / totpo
}

tempfile race
save `race'

*-------------------------
loc x B11007
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01 hd01_vd02 hd01_vd03

drop in 1/1

destring geoid2, gen(zcta5)
drop geoid2

foreach v of varlist hd* {
    destring `v', replace
}
rename hd01_vd01 tothh
rename hd01_vd02 hh_gt65
rename hd01_vd03 hh_gt65_1person

foreach v of varlist hh* {
  gen pct_`v' = `v' / tothh
}

tempfile hh_gt65
save `hh_gt65'

*-------------------------
loc x B17001
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01 hd01_vd02
drop in 1/1

destring geoid2, gen(zcta5)
drop geoid2

foreach v of varlist hd* {
    destring `v', replace
}
rename hd01_vd01 tot
rename hd01_vd02 belowpoverty

foreach v of varlist belowpoverty {
  gen pct_`v' = `v' / tot
}

tempfile belowpoverty
save `belowpoverty'


*-------------------------
loc x B19113
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01
drop in 1/1

destring geoid2, gen(zcta5)
drop geoid2

foreach v of varlist hd* {
    destring `v', replace
}
rename hd01_vd01 medfamincome

tempfile medfamincome
save `medfamincome'

*-------------------------
loc x B19301
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01
drop in 1/1

destring geoid2, gen(zcta5)
drop geoid2

replace hd01 = "" if hd01=="-"

foreach v of varlist hd* {
    destring `v', replace
}
rename hd01_vd01 pcincome

tempfile pcincome
save `pcincome'

*-------------------------
loc x B27010
insheet using ACS_15_5YR_`x'_with_ann.csv, comma names clear

keep geoid2 hd01_vd01 hd01_vd02 hd01_vd03 hd01_vd10 hd01_vd18 hd01_vd19 hd01_vd26 hd01_vd34 hd01_vd35 hd01_vd42 hd01_vd51 hd01_vd52 hd01_vd58

drop in 1/1

destring geoid2, gen(zcta5)
drop geoid2

foreach v of varlist hd* {
    destring `v', replace
}

rename hd01_vd01 tot
rename hd01_vd02 tot_lt18
egen ins_lt18 = rowtotal(hd01_vd03 hd01_vd10)
rename hd01_vd18 tot_18_34
egen ins_18_34 = rowtotal(hd01_vd19 hd01_vd26)
rename hd01_vd34 tot_35_64
egen ins_35_64 = rowtotal(hd01_vd35 hd01_vd42)
rename hd01_vd51 tot_gt65
egen ins_gt65 = rowtotal(hd01_vd52 hd01_vd58)

drop hd*

foreach v in "lt18" "18_34" "35_64" "gt65" {
  gen pct_ins_`v' = ins_`v' / tot_`v'
}

tempfile ins
save `ins'
*-------------------------

*combine all data by zcta5
use `pop_gt65', clear

foreach f in "race" "hh_gt65" "belowpoverty" "medfamincome" "pcincome" "ins" {
    merge 1:1 zcta5 using ``f'', keep(1 3) nogen
}
keep zcta5 totpop pct_* *income

*destring variables
gen x = real(medfamincome)
replace medfamincome = "" if x==.
drop x

foreach v of varlist * {
  capture destring `v', replace
}

foreach v of varlist pct* {
  replace `v' = 100*`v'
}
foreach v in "white" "black" "asian" "multirace" {
    lab var pct_`v' "% `v' population"
}
lab var pct_pop_gt65 "% age 65+ population"
lab var pct_hh_gt65 "% households with age 65+"
lab var pct_hh_gt65_1 "% 1-person HHs with age 65+"
lab var pct_belowpove "% pop below poverty"
lab var medfamincome "Median family income ($)"
lab var pcincome "Per capita income ($)"
lab var totpop "Total population"
lab var pct_ins_lt18 "% insured under age 18 pop"
lab var pct_ins_18_34 "% insured, age 18-34"
lab var pct_ins_35_64 "% insured, age 35-64"
lab var pct_ins_gt65 "% insured, age 65+"

compress
save zcta_level_chars, replace
