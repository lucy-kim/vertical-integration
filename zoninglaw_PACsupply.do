* impact of the zoning law restrictiveness on the number of SNFs, HHAs (vertically integrated or not)

loc reg /home/hcmg/kunhee/vertical-int/regresults
loc gph /home/hcmg/kunhee/vertical-int/gph
loc home /home/hcmg/kunhee/vertical-int
cd `home'

* use the zoning law
use "`home'/data/WHARTON LAND REGULATION DATA_1_24_2008.dta", clear
drop if WRLURI==.

sum WRLURI
*standardized

*ID is a unique ID for each row; name is also unique except 7 obs

list id stateid ufips msa99 msastate stateid stacode WRLURI weight* name type in 1/20
* ufips is ANSI Codes for Places (https://www.census.gov/geo/reference/codes/place.html)

* get the ANSI codes for places for state (because the ufips is unique if combined with state code)
preserve
import delimited `home'/data/geodata/national_places.txt, delim("|") clear
tempfile fips
save `fips'

keep state*
duplicates drop
tempfile stfips
save `stfips'
restore

assert statename!=""

*attach state fips by statename
rename state state_full
rename statename state
merge m:1 state using `stfips', keep(1 3) nogen

tostring statefp ufips, replace
gen fips = statefp + "-"+ ufips

*standardize name
replace name = "St Augustine, FL" if name=="City of St. Augustine, FL"

* create fips place code-level index (2 duplicate obs per fips)
collapse (mean) WRLURI [pw=weight], by(state ufips statefp fips name)

duplicates tag fips, gen(dd)
assert dd==0
drop dd
*keep WRLURI state fips statefp ufips

*aggregate to the state level
/* collapse (mean) WRLURI [pw=weight], by(statename)
* get distribution of WRLURI
hist WRLURI */

compress
tempfile zoning
save `zoning'

*-------------------------
* source: https://www.census.gov/geo/maps-data/data/zcta_rel_download.html
import delimited `home'/data/geodata/2010_zcta_to_place.txt, delim(",") clear
keep zcta5 state place zpoppct
*zpoppct is the % ZIP-code population in the place

*create a ZIP code level
rename state statefp
rename place ufips
tostring statefp ufips, replace
gen fips = statefp + "-"+ ufips

merge m:1 fips using `zoning', keep(2 3)
* 5% places in zoning law data unmatched -> drop them because we don't know their ZIP codes
drop if _m==2
drop _m

* create weightd mean index per ZIP code
gen wgted = (zpoppct/100) * WRLURI
collapse (sum) WRLURI = wgted, by(zcta5 state)

duplicates tag zcta5, gen(dd)
assert dd==0
drop dd

tempfile ziplvl_idx
save `ziplvl_idx'
*merge m:1 zcta5 using `zip_zcta'

*-------------------------

* create ZIP code to ZCTA xwalk & ZCTA to fips code xwalk
import excel using `home'/data/geodata/zip_to_zcta_2017.xlsx, clear first
rename STATE state
rename ZCTA zcta5
rename ZIP_CODE zip_cd
destring zcta , replace

duplicates tag zcta5, gen(dd)
tab dd
drop dd
* not unique at the zcta5 level but unique at the zip code level

tempfile zip_zcta
save `zip_zcta'

*-------------------------

* get the number of SNFs, HHAs, and other types of PAC facilities in each state-year from POS data
use `home'/data/POS/pos2016, clear

/*keep if prvdr_ctgry_cd=="02" | prvdr_ctgry_cd=="03" | prvdr_ctgry_cd=="04" | prvdr_ctgry_cd=="05" | prvdr_ctgry_cd=="06" | prvdr_ctgry_cd=="10"*/

* provider-level data on number of beds within entity
keep sbunit_cnt prvdr_num fac_name gnrl_fac_type gnrl_cntl_typ brnch_cnt city_name zip_cd crtfd_bed_cnt bed_cnt mdcd_nf_bed_cnt mdcr_snf_bed_ mdcr_mdcd_snf state_cd prvdr_ctgry_cd pgm_prtcptn_cd zip_cd

drop if state_cd=="PR" | state_cd=="AS" | state_cd=="GU" | state_cd=="AK" | state_cd=="HI" | state_cd=="MP" | state_cd=="VI" | state_cd=="AK" | state_cd=="DC"

*merge with ZIP to ZCTA xwalk
merge m:1 zip_cd using `zip_zcta', keep(3) nogen
*1000 unmatched and had _m=1

*merge with ZCTA-level zoning regulation data
merge m:1 zcta5 using `ziplvl_idx', keep(1 3)
* 50% of obs have zcta5 unmatched to the zoning index data - a lot!!

*count number of unmatched ZCTAs in each state
bys state_cd: egen unmatch = sum(_m==1)
bys state_cd: egen totzcta = sum(1)
preserve
keep state_cd unmatch totzcta
duplicates drop
gen pct_unmatched = 100*unmatch / totzcta
restore

*drop ZCTAs that are not matched to the zoning law data
drop if _m==1
drop _me

rename state_cd statename

* US territories + DC not matched

*merge with state-level CON status data
preserve
insheet using `home'/data/CONs_state.csv, names clear
drop note
drop if state==""

keep state* con*
save `home'/data/con_state_hha_nh, replace
restore

rename statename state_ab
merge m:1 state_ab using `home'/data/con_state_hha_nh, keep(3) nogen

* merge with CR data - only a half of providers are matched because we dropped a half of ZCTAs with no zoning regulation data

* get the HHA cost report data on the volume of care provided
merge 1:1 prvdr_num using `home'/data/HCRIS/hhacr_panel, keep(1 3) nogen

* get the SNF cost report data on the volume of care provided
merge 1:1 prvdr_num using `home'/data/HCRIS/snfcr_panel, keep(1 3) nogen

* drop hospital, portable x-ray supplier, ESRD facility, ASC, hospice, organ procurement org
destring prvdr_ctgry_cd, replace
drop if prvdr_ctgry_cd==1 | prvdr_ctgry_cd==7 | prvdr_ctgry_cd==9 | prvdr_ctgry_cd==15 | prvdr_ctgry_cd==16 | prvdr_ctgry_cd==17

* prvdr_ctgry_cd: https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/index.html

* group SNF & NF and create new provider categories
gen cat = 1 if prvdr_ctgry_cd==2 | prvdr_ctgry_cd==3 | prvdr_ctgry_cd==4 | prvdr_ctgry_cd==10
replace cat = 2  if prvdr_ctgry_cd==5
replace cat = prvdr_ctgry_cd if cat==.
tab cat
lab def provcode 1 "Nursing home" 2 "HHA" 6 "Psych" 8 "Therapy" 11 "ICF/MR" 12 "RHC" 14 "CORF" 19 "CMHC" 21 "FQHC", replace
lab val cat provcode

* create a single CON status indicator
gen con = con_hha if cat==2
replace con = con_nh if cat==1
assert con!=. if cat==1 | cat==2

gen WRLURI_X_con = WRLURI * con

label var WRLURI "Restrictive zoning law"
label var con "CON"
label var WRLURI_X_con "Restrictive zoning law X CON"

*import state-level total number of Medicare beneficiaries by state for CY 2015 (downloaded from https://www.kff.org/medicare/state-indicator/total-medicare-beneficiaries/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D)
preserve
insheet using `home'/data/mcre_benes_bystate2015.csv, comma names clear
drop in 1/3
drop in 52/66
rename v2 mcre_cnt
rename titletotalnum state
drop v3
tempfile mcre_cnt
save `mcre_cnt'
restore

merge m:1 state using `mcre_cnt', keep(1 3) nogen
drop state
destring mcre_cnt, replace

*lump variables for SNF & NF
gen unmatched = snf_nbeds==. & nf_nbeds==.
foreach vv in "nbeds" "days_tot" "disch_tot" "admit_tot" {
  replace snf_`vv' = 0 if unmatched==0 & snf_`vv'==.
  replace nf_`vv' = 0 if unmatched==0 & nf_`vv'==.
  gen nh_`vv' = snf_`vv' + nf_`vv'
}

rename sn_visit_all hha_snv
rename undup_pat_cnt_all hha_patcnt
drop hhaname

tab nh_nbeds if cat==1

tab cat
sum hha* if cat==2

*merge with ZCTA-level characteristics
merge m:1 zcta5 using `home'/data/census/zcta_level_chars, keep(1 3) nogen

foreach v of varlist totpop *income {
  capture destring
  gen ln`v' = ln(`v')
}
lab var lntotpop "ln(population)"
lab var lnmedfamincome "ln(median family income)"
lab var lnpcincome "ln(per capita income)"

tempfile provdata
save `provdata'

/** skip except for state-level analysis
foreach x in "nh" "hha" {
  foreach y of varlist `x'* {
    * volume per 100,000 Medicare bene's
    replace `y' = (`y' / mcre_cnt)*100000
  }
}*/
*--------------------------
* ZCTA-level data on number of providers
use `provdata', clear
gen i = 1
collapse (sum) num=i , by(cat state_ab con* zcta WRLURI* totpop pct_* *income ln*)

sort zcta cat

preserve
keep zcta WRLURI
duplicates drop
xtile gp = WRLURI , n(2)
tempfile abovemedian
save `abovemedian'
restore

merge m:1 zcta using `abovemedian', nogen

tempfile zctadata
save `zctadata'

*-----------------
* provider level
use `provdata', clear

merge m:1 zcta using `abovemedian', nogen

* exclude outlier?
keep if cat==1 | cat==2
tab nh_disch if cat==1
drop if nh_disch > 10000 & cat==1
tab hha_patcnt if cat==2
drop if hha_patcnt > 10000 & cat==2
tab hha_snv if cat==2
drop if hha_snv > 100000 & cat==2

loc y_nh_nbeds "Number of NH beds"
loc y_nh_days_tot "Number of NH days"
loc y_nh_disch_tot "Number of NH discharges"
loc y_nh_admit_tot "Number of NH admissions"
loc y_hha_snv "Number of HHA nurse visits"
loc y_hha_patcnt "Number of HHA patient counts"

loc nh_i = 1
loc hha_i = 2
loc y_nh "Nursing homes"
loc y_hha "HHAs"

loc file zoning2
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

loc indep lntotpop pct_pop_gt65 lnmedfamincome pct_white pct_black pct_asian pct_ins*

areg hha_patcnt WRLURI `indep' if cat==2 & con==0, absorb(state_ab)

foreach x in "nh" "hha" {
  foreach y of varlist `x'* {
    di "`y'"
    areg `y' WRLURI `indep' if cat==``x'_i', absorb(state_ab)
    sum `y' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`y'')

    areg `y' WRLURI `indep' if cat==``x'_i' & con==1, absorb(state_ab)
    sum `y' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`y'')

    areg `y' WRLURI `indep' if cat==``x'_i' & con==0, absorb(state_ab)
    sum `y' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`y'')
  }
}

*summary stats for 2 X 2 groupings: restrictive vs CON
foreach x in "nh" "hha" {
  foreach y of varlist `x'* {
    * lab var `y' `y_`y''
    preserve
    replace con = con* (-1)
    replace gp = gp * (-1)
    tab gp con if cat==``x'_i', summarize(`y')
    restore
  }
}

foreach x in "nh" "hha" {
  foreach y of varlist `x'* {
    di "`y'"
    binscatter `y' WRLURI if cat==``x'_i', by(con) xti("Mean restrictiveness of zoning law in state") yti("`y_`y''") legend(order(1 "No CON" 2 "CON")) title("`y_`y'' per 100,000 Medicare enrollees")
    graph export `gph'/`y'.pdf, replace
  }
}

*-----------------
*competitivenss in HHA/SNF market at the state level
keep if cat==1 | cat==2

loc nhvol disch_tot
loc hhavol patcnt
foreach x in "nh" "hha" {
  bys zcta cat: egen `x'_``x'vol'_st = sum(`x'_``x'vol')
  gen `x'_``x'vol'_sh2 = (`x'_``x'vol'/`x'_``x'vol'_st)^2
  bys zcta cat: egen `x'_hhi = sum(`x'_``x'vol'_sh2)
}
collapse (mean) *hhi, by(zcta cat)
lab var nh_hhi "Nursing home market concentration"
lab var hha_hhi "HHA market concentration"

tempfile hhi
save `hhi'
*-------------------------
* ZCTA-level analysis: impact of zoning law & CON law on # providers
use `zctadata', clear

*drop ZCTAs that span across states
duplicates tag zcta cat, gen(dd)
tab dd
* 6 obs
drop if dd > 0
drop dd

lab var WRLURI "Restrictive zoning law"
lab var num "Number of providers"

merge 1:1 zcta cat using `hhi' , keep(1 3) nogen
* only matched to NH & HHA

* number of providers per 100,000 Medicare bene's
*gen num_per = (num / mcre_cnt)*100000
/*tab cat, summarize(num_per)
bys con: sum num_per if cat==1
bys con: sum num_per if cat==2*/


loc file zoning
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex append label"

*run regression on # providers & HHI
foreach x in "nh" "hha" {
  areg num WRLURI `indep' if cat==``x'_i', absorb(state_ab)
  sum num if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')

  areg num WRLURI `indep' if cat==``x'_i' & con==1, absorb(state_ab)
  sum num if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')

  areg num WRLURI `indep' if cat==``x'_i' & con==0, absorb(state_ab)
  sum num if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')


  loc y `x'_hhi
  areg `y' WRLURI `indep' if cat==``x'_i', absorb(state_ab)
  sum `y' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')

  areg `y' WRLURI `indep' if cat==``x'_i' & con==1, absorb(state_ab)
  sum `y' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')

  areg `y' WRLURI `indep' if cat==``x'_i' & con==0, absorb(state_ab)
  sum `y' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  `out' addtext(Mean dep. var., `mdv') dec(3) fmt(fc) ctitle(`y_`x'')
}


foreach x in "nh" "hha" {
  binscatter num_per WRLURI if cat==``x'_i', by(con) xti("Mean restrictiveness of zoning law in state") yti("Number of `y_`x''") legend(order(1 "No CON" 2 "CON")) title("Number of `y_`x'' per 100,000 beneficiaries")
  graph export `gph'/num_`x'.pdf, replace

  binscatter `x'_hhi WRLURI if cat==``x'_i', by(con) xti("Mean restrictiveness of zoning law in state") yti("HHI of `y_`x''") legend(order(1 "No CON" 2 "CON")) title("Consolidation of `y_`x''")
  graph export `gph'/hhi_`x'.pdf, replace
}

*summary stats for 2 X 2 groupings: restrictive vs CON
merge m:1 state_ab using `abovemedian', nogen

foreach x in "nh" "hha" {
  foreach y of varlist `x'* {
    * lab var `y' `y_`y''
    preserve
    replace con = con* (-1)
    replace gp = gp * (-1)
    tab gp con if cat==``x'_i', summarize(`y')
    restore
  }
}

foreach x in "nh" "hha" {
  preserve
  replace con = con* (-1)
  replace gp = gp * (-1)
  tab gp con if cat==``x'_i', summarize(num_per)
  restore
}
