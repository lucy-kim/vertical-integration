*why is there a dip in probability of referral in Jan 2011-May 2012?

loc dta /ifs/home/kimk13/VI/data
loc out /ifs/home/kimk13/VI/output

cd `dta'/Medicare

*use the newest # SNF referrals after meng corrected the hic number issue
use index_admit_chm_ddest, clear
keep if cond=="AMI"

sort provid dischyear dischmth
list provid dischyear dischmth dischnum ddest count in 1/30

preserve
collapse (max) dischnum (sum) count, by(provid dischyear dischmth)
assert dischnum==count if dischnum!=.
drop count
replace dischnum = 0 if dischnum==.
tempfile admit
save `admit'

collapse (sum) dischnum, by(dischyear dischmth)
tempfile admit2
save `admit2'
restore

preserve
keep if ddest==3
collapse (sum) count, by(dischyear dischmth)
rename count ddest_ct_orig
tempfile ddest3
save `ddest3'
restore

insheet using SNF_index_FREQ_fixed_0521_v3_AMI.csv, comma names clear
keep dischyear dischmth ddest_ct snf_ct
tempfile totalsnf
save `totalsnf'

merge 1:1 dischyear dischmth using `ddest3', keep(1 3) nogen
merge 1:1 dischyear dischmth using `admit2', keep(1 3) nogen

gen ratio =  snf_ct/dischnum

gen ym = ym(dischyear, dischmth)
format ym %tm

tw bar ratio ym, ti(Share of total # discharges from all hospitals referred to SNF) xti("Year-Month") tlabel(2008m1(4)2016m6, angle(45)) xline(612) xline(629) caption("Note. Referral defined as being matched to SNF data and starting SNF within 2 days" "from hospital discharge.", size(small)) yti("") ysc(r(0.1 0.3)) ylab(0.1(0.05)0.3)
graph export `out'/test.eps, replace

*-------------------
*did the reduction happen only among a few states?
*------------
use SNFreferral_tchpm, clear
drop if cond=="HK"
*merge m:1 provid using `uniqhosp', keep(3) nogen

*keep if ym >= ym(2010,10) & ym <= ym(2012,9)

gen yq = yq(dischyear, qtr)
format yq %tq

collapse (sum) dischnum_pac, by(provid yq)

gen ccn = string(provid, "%06.0f")
gen st = substr(ccn,1,2)
gen last4 = substr(ccn,3,4)
destring last4, replace
tab last4 if !(last4 >= 1 & last4 <=879)
*mostly critcal access hospitals
keep if last4 >= 1 & last4 <=879

*merge with state label xwalk for the first 2 digits of CCN
preserve
insheet using `dta'/Medicare/ccn_state_cd_xwalk.csv, comma names clear
tempfile xwalk
save `xwalk'
restore

destring st, replace
rename st state_cd
merge m:1 state_cd using `xwalk', keep(1 3)
tab state_cd if _m==1
*https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf
replace state = "Texas" if state_cd==67
assert state!=""
drop _m

sort provid yq
bys provid: gen first = dischnum if _n==1
bys provid: replace first = first[_n-1] if first >=.
gen diff = dischnum - first

tempfile pac
save `pac'

*merge with total # discharges with ddest = 3 (SNF) & take the ratio of # discharges to SNF by merging with SNF data to # discharges to SNF by discharge destination code
use index_admit_chm_ddest, clear
drop if cond=="HK"
gen yq = yq(dischyear, qtr)
format yq %tq

bys provid yq: egen totdisch = sum(count)

/* sort provid ym cond ddest
list provid ym cond ddest count dischnum totdisch in 1/50 */

keep if ddest==3
collapse (sum) count (mean) totdisch, by(provid yq)
rename count dischnum
assert dischnum <= totdisch

tempfile ddest3
save `ddest3'

use `pac', clear
merge 1:1 provid yq using `ddest3', keep(3) nogen

gen ratio = dischnum_pac / dischnum
gen prob = dischnum_pac / totdisch

tempfile tmp
save `tmp'

* replicate previous graphs showing the mean across all states: plot across quarters, the ratio of  # discharges to SNF by merging with SNF data to # discharges to SNF by discharge destination code across all hospitals
use `tmp', clear
drop if state=="Maryland"
merge m:1 provid using `uniqhosp', keep(1 3)

keep if _m==3
collapse (mean) prob dischnum* diff ratio totdisch, by(yq)

tw bar ratio yq if yq >= yq(2009,1) & yq <= yq(2016,2), title("All 3 cond's: Mean # referrals by matching with SNF data / by discharge destination", size(small)) yti("") xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)  ylab(0(0.2)1) ysc(r(0 1))
graph export `out'/ratio.eps, replace

tw sc prob yq if yq >= yq(2009,1) & yq <= yq(2016,2), yti("") title("All 3 cond's: Mean probability of referrals", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(0(0.1)0.25) ysc(r(0 0.25))
graph export `out'/prob.eps, replace

*----------

* plot by state
use `tmp', clear
drop if state=="Maryland"
merge m:1 provid using `uniqhosp', keep(1 3)

keep if _m==3
collapse (mean) prob dischnum* diff ratio totdisch, by(state yq)

tw bar ratio yq if yq >= yq(2009,1) & yq <= yq(2016,2), by(st) ytitle("3 cond's: Mean # referrals by matching / by discharge destination", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(0(0.2)1) ysc(r(0 1))
graph export `out'/ratio_bystate.eps, replace

tw bar ratio yq if yq >= yq(2009,1) & yq <= yq(2016,2) & state!="Alaska", by(st) ytitle("3 cond's: Mean # referrals by matching / by discharge destination", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(0(0.2)1.5, angle(360)) ysc(r(0 1.5))
graph export `out'/ratio_bystate_noAK.eps, replace

tw bar dischnum yq if yq >= yq(2009,1) & yq <= yq(2016,2)  & state!="Alaska", by(st) ytitle("3 cond's: Mean # discharges destined to SNF", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)
*ylab(0(0.2)0.5) ysc(r(0 0.5))
graph export `out'/dischnum_bystate.eps, replace

tw bar dischnum_pac yq if yq >= yq(2009,1) & yq <= yq(2016,2) & state!="Alaska", by(st) ytitle("3 cond's: Mean # discharges to SNF by matching w/ SNF data", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)
*ylab(0(0.2)0.5) ysc(r(0 0.5))
graph export `out'/dischnum_pac_bystate.eps, replace

* CT
tw bar ratio yq if yq >= yq(2009,1) & yq <= yq(2016,2) & state=="Connecticut", by(st) ytitle("3 cond's: Mean # referrals by matching / by discharge destination", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(0(0.2)1.5, angle(360)) ysc(r(0 1.5))
graph export `out'/ratio_CT.eps, replace

tw bar dischnum yq if yq >= yq(2009,1) & yq <= yq(2016,2)  & state=="Connecticut", by(st) ytitle("3 cond's: Mean # discharges destined to SNF", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)
*ylab(0(0.2)0.5) ysc(r(0 0.5))
graph export `out'/dischnum_CT.eps, replace

tw bar dischnum_pac yq if yq >= yq(2009,1) & yq <= yq(2016,2) & state=="Connecticut", by(st) ytitle("3 cond's: Mean # discharges to SNF by matching w/ SNF data", size(small)) xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)
*ylab(0(0.2)0.5) ysc(r(0 0.5))
graph export `out'/dischnum_pac_CT.eps, replace

*-----------
*zoom in on CT
use `tmp', clear
keep if state=="Connecticut"
duplicates tag provid yq, gen(dup)
assert dup==0
drop dup

keep provid
duplicates drop
tempfile hosp_ct
save `hosp_ct'

use SNFreferral_tchpm, clear
drop if cond=="HK"
merge m:1 provid using `hosp_ct', keep(3) nogen

gen yq = yq(dischyear, qtr)
format yq %tq

collapse (sum) dischnum_pac, by(provid yq pacprovid)

gen ccn = string(pacprovid, "%06.0f")
gen st = substr(ccn,1,2)
gen last4 = substr(ccn,3,4)
destring last4, replace
*for SNF, the last 4 digits should be 5000-6499
assert last4 >= 5000 & last4 <= 6499 if pacprovid!=.

*count # SNFs being referred to
assert dischnum_pac!=.
gen refer = dischnum_pac > 0

preserve
collapse (sum) nreferredSNF =refer (mean) dischnum_pac, by(provid yq)

*mean # SNFs being referred to & mean number of referrals to each SNF per quarter across hospitals
collapse (mean) nreferredSNF dischnum_pac, by(yq)

tw bar nreferredSNF yq if yq >= yq(2009,1) & yq <= yq(2016,2), ti("3 cond's: Mean # SNFs being referred to per quarter among CT hospitals", size(medium)) yti("")  xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(10(2)16) ysc(r(10 16))
graph export `out'/nreferredSNF_CT.eps, replace

tw bar dischnum_pac yq if yq >= yq(2009,1) & yq <= yq(2016,2), ti("3 cond's: Mean # referrals to each SNF per quarter across CT hospitals", size(medium)) yti("")  xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209) ylab(2(1)4) ysc(r(1.9 4))
graph export `out'/dischnum_pac_eachSNF_CT.eps, replace
restore

preserve
collapse (sum) nreferredSNF =refer totreferral = dischnum_pac (mean) dischnum_pac, by(provid yq)

keep if yq==yq(2010,1) | yq==yq(2011,1)
sort provid yq
bys provid : gen diff = nreferredSNF - nreferredSNF[_n-1]
list if diff!=.
list if diff < 0
restore

sort provid pacprovid yq
list if provid==70036
count if pacprovid==75044 & yq==yq(2011,1)

*-----------------
*for H/K, does the ratio look more normal?
use SNFreferral_tchpm, clear
keep if cond=="HK"
*merge m:1 provid using `uniqhosp', keep(3) nogen

*keep if ym >= ym(2010,10) & ym <= ym(2012,9)

gen yq = yq(dischyear, qtr)
format yq %tq

collapse (sum) dischnum_pac, by(provid yq)

gen ccn = string(provid, "%06.0f")
gen st = substr(ccn,1,2)
gen last4 = substr(ccn,3,4)
destring last4, replace
tab last4 if !(last4 >= 1 & last4 <=879)
*mostly critcal access hospitals
keep if last4 >= 1 & last4 <=879

*merge with state label xwalk for the first 2 digits of CCN
preserve
insheet using `dta'/Medicare/ccn_state_cd_xwalk.csv, comma names clear
tempfile xwalk
save `xwalk'
restore

destring st, replace
rename st state_cd
merge m:1 state_cd using `xwalk', keep(1 3)
tab state_cd if _m==1
*https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf
replace state = "Texas" if state_cd==67
assert state!=""
drop _m

sort provid yq
bys provid: gen first = dischnum if _n==1
bys provid: replace first = first[_n-1] if first >=.
gen diff = dischnum - first

tempfile pac
save `pac'

*merge with total # discharges with ddest = 3 (SNF) & take the ratio of # discharges to SNF by merging with SNF data to # discharges to SNF by discharge destination code
preserve
use index_admit_chm_ddest, clear
keep if cond=="HK"
gen yq = yq(dischyear, qtr)
format yq %tq

bys provid yq: egen totdisch = sum(count)

/* sort provid ym cond ddest
list provid ym cond ddest count dischnum totdisch in 1/50 */

keep if ddest==3
collapse (sum) count (mean) totdisch, by(provid yq)
rename count dischnum
assert dischnum <= totdisch

tempfile ddest3
save `ddest3'
restore

merge 1:1 provid yq using `ddest3', keep(3) nogen

gen ratio = dischnum_pac / dischnum
gen prob = dischnum_pac / totdisch

tempfile tmp_hk
save `tmp_hk'

use `tmp_hk', clear
drop if state=="Maryland"
merge m:1 provid using `uniqhosp', keep(1 3)

keep if _m==3
collapse (mean) prob dischnum* diff ratio totdisch, by(yq)

tw bar ratio yq if yq >= yq(2009,1) & yq <= yq(2016,2), title("H/K only: Mean # referrals by matching with SNF data / by discharge destination", size(small)) yti("") xti("Year-Quarter") tlabel(2009q1(4)2016q2, angle(45)) xline(204) xline(209)  ylab(0(0.2)1) ysc(r(0 1))
graph export `out'/ratio_HK.eps, replace



/* *ddest
1 home
3 SNF
6 HHC*/
