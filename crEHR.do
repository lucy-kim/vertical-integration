*create data for meaning use of EHR at the hospital-year level

loc dta /ifs/home/kimk13/VI/data
cd `dta'/Medicare/EHR-MU

insheet using MU_REPORT.csv, comma names clear

assert ccn!=. if provider_type=="Hospital"
keep if provider_type=="Hospital"

keep if hospital_type=="General"

keep ccn provider_stage_number program_year
*program year 2011-16
*provider stage : create indicators for stage 1 & 2, respectively

renvars ccn provider_stage_number program_year \ provid stage fy
gen EHRstage1 = stage=="Stage 1"
gen EHRstage2 = stage=="Stage 2"
drop stage
duplicates drop

sort provid fy

tab fy, summarize(EHRstage1)
*much fewer hospitals before 2013; all hospitals in data have stage 1 = 1 & stage 2 = 0 during 2011-2013
tab fy, summarize(EHRstage2)

*3 hospitals have both stage 1 & 2 -> only keep stage 2 = 1
duplicates tag provid fy, gen(dup)
bys provid: egen mm = max(EHRstage2)
drop if EHRstage1==1 & EHRstage2==0 & dup > 0 & mm==1

drop dup mm
duplicates tag provid fy, gen(dup)
assert dup ==0
drop dup

compress
save ehr, replace
