*create hospital-FY level data containing measures of integration & hospital characteristics

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011
loc pnltmax2016 0.03
loc pnltmax2015 0.03
loc pnltmax2014 0.03
loc pnltmax2013 0.03
loc pnltmax2012 0.02
loc pnltmax2011 0.01

cd `dta'/Medicare

*cap the penalty rate to max level
use predict_pnltprs_agg3c, clear

foreach v of varlist ppr ppr_c3 {
  forval yr = 2011/2016 {
    replace `v' = `pnltmax`yr'' if `v' > `pnltmax`yr'' & fy==`yr'
  }
  replace `v' = `v'*100
}

table fy, contents(mean ppr sd ppr p75 ppr p25 ppr max ppr)
table fy, contents(mean ppr_c3 sd ppr_c3 p75 ppr_c3 p25 ppr_c3 max ppr_c3)

loc int 2011
drop *c3`int'
foreach v of varlist ppr`int' {
  replace `v' = `pnltmax`int'' if `v' > `pnltmax`int''
  replace `v' = `v'*100
  sum `v'
}

*create deciles of # beds
preserve
keep provid fy beds
duplicates drop
sum beds, de
xtile gp_beds = beds, n(10)
tab gp_beds, summarize(beds)
drop beds
tempfile gp_beds
save `gp_beds'
restore

merge m:1 provid fy using `gp_beds', keep(1 3) nogen

*char fy[omit] 2011
xi i.fy
des _Ify*


gen post`int' = fy >=`int'

local lppst "Predicted likelihood of penalty"
local lppr "Predicted penalty rate"
local llppd "Log Predicted penalty amount ($)"

local lppst`int' "Predicted likelihood of penalty in `int'"
local lppr`int' "Predicted penalty rate in `int'"
local llppd`int' "Log Predicted penalty amount ($) in `int'"

loc ppst_c3 "Predicted likelihood of penalty based on AMI, HF, PN"
loc ppr_c3 "Predicted penalty rate based on AMI, HF, PN"
loc lppd_c3 "Log Predicted penalty amount ($) based on AMI, HF, PN"

*interaction of continuous penalty pressure and post`int' period indicators
foreach v of varlist ppst* ppr* lppd* {
  tab fy, summarize(`v')

  capture drop `v'Xpost`int'
  gen `v'Xpost`int' = `v'*post`int'
  lab var `v'Xpost`int' "`l`v'' X Post `int'"

  *foreach t of numlist 2009 2010 2012 2013 2014 2015 2016 {
  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
    lab var `v'X`t' "`l`v'' X `t'"
  }
}

tempfile tmp
save `tmp'

/* *get indicators for top quartlie & middle 2 quartiles
use `tmp', clear
forval int = 2011/2012 {
  preserve
  keep provid ppst`int' ppr`int' lppd`int' ppst`int' ppr`int' lppd`int'
  duplicates drop
  foreach pp of varlist ppst`int' ppr`int' lppd`int' {
    egen x_p75_`pp' = pctile(`pp'), p(75)
    egen x_p25_`pp' = pctile(`pp'), p(25)
    gen p75_`pp' = `pp' > x_p75_`pp'
    gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
  }
  drop x_* ppst ppr lppd
  tempfile quartiles`int'
  save `quartiles`int''
  restore
}

use `tmp', clear
forval int = 2011/2012 {
  merge m:1 provid using `quartiles`int'', keep(1 3) nogen

  *create interaction of penalty pressure with indicators for the top quartile, middle 2 quatiles in the penalty pressure
  foreach pp of varlist ppst`int' ppr`int' lppd`int' {
    *foreach t of numlist 2009 2010 2012 2013 2014 2015 2016 {
    forval t=2010/2016 {
      gen p75_`pp'X`t' = p75_`pp' * _Ify_`t'
      lab var p75_`pp'X`t' "Top quartile in `l`pp'' X `t'"
      gen p25_75_`pp'X`t' = p25_75_`pp' * _Ify_`t'
      lab var p25_75_`pp'X`t' "Middle 2 quartiles in `l`pp'' X `t'"
    }
    gen p75_`pp'Xpost`int' = p75_`pp' * post`int'
    gen p25_75_`pp'Xpost`int' = p25_75_`pp' * post`int'
    lab var p75_`pp'Xpost`int' "Top quartile X post`int'"
    lab var p25_75_`pp'Xpost`int' "Middle 2 quartiles X post`int'"
  }
  tab fy, summarize(p75_ppst`int'Xpost`int')
}

*get indicators for top quartlie & middle 2 quartiles
foreach pp of varlist ppst ppr lppd {
  bys fy: egen x_p75_`pp' = pctile(`pp'), p(75)
  bys fy: egen x_p25_`pp' = pctile(`pp'), p(25)
  gen p75_`pp' = `pp' > x_p75_`pp'
  gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
}
drop x_*

tab fy, summarize(p75_ppst)

*create interaction terms with indicators for the top quartile, middle 2 quatiles in the penalty pressure and post`int' indicators
foreach pp of varlist ppst ppr lppd {
  *foreach t of numlist 2009 2010 2012 2013 2014 2015 2016 {
  forval t=2010/2016 {
    gen p75_`pp'X`t' = p75_`pp' * _Ify_`t'
    lab var p75_`pp'X`t' "Top quartile in `l`pp'' X `t'"
    gen p25_75_`pp'X`t' = p25_75_`pp' * _Ify_`t'
    lab var p25_75_`pp'X`t' "Middle 2 quartiles in `l`pp'' X `t'"
    *gen `pp'X`t' = `pp' * _Ify_`t'
    *lab var `pp'X`t' "`l`pp'' X `t'"
  }
  forval int =2011/2012 {
    gen p75_`pp'Xpost`int' = p75_`pp' * post`int'
    gen p25_75_`pp'Xpost`int' = p25_75_`pp' * post`int'
    lab var p75_`pp'Xpost`int' "Top quartile X post`int'"
    lab var p25_75_`pp'Xpost`int' "Middle 2 quartiles X post`int'"
    *gen `pp'Xpost`int' = `pp' * post`int'
    *lab var `pp'Xpost`int' "`l`pp'' X post`int'"
  }
}
des p75* p25_75**/

*create laggeed formally owning SNF
preserve
use hosp_fy_VI_agg3c, clear
sort provid fy
bys provid: gen vi_snf_l = vi_snf[_n-1]
tab fy, summarize(vi_snf_l)
lab var vi_snf_l "Owns SNF, lagged"
keep provid fy vi_snf_l
duplicates drop
tempfile vi_snf_l
save `vi_snf_l'
restore

merge 1:1 provid fy using `vi_snf_l', keep(1 3) nogen
tab fy, summarize(vi_snf_l)

lab var vi_snf "Formally owns SNF"
lab var pac_hhi_hrr "SNF market concentration"
lab var urban "Urban"
lab var teaching "Teaching"
lab var own_fp "For profit"
lab var own_np "Not for profit"
lab var own_gv "Government owned"

tempfile an
save `an'

*add share of discharges referred to SNFs previously used
* currently, i only count as having used before if used in the prior year (should it be cumulative?)
use SNFreferral_tchpm.dta, clear
drop if cond=="HK"
collapse (sum) dischnum_pac, by(provid fy pacprovid)

*create a balanced yearly panel for each hospital-SNF pair
preserve
keep provid pacprovid
duplicates drop
expand 2016-2007
bys provid pacprovid: gen fy = 2008+_n-1
assert fy!=. & fy >= 2008 & fy <= 2016
tempfile balanced_pair
save `balanced_pair'
restore

merge 1:1 provid pacprovid fy using `balanced_pair'
replace dischnum_pac = 0 if _m==2

sort provid pacprovid fy
bys provid pacprovid: gen used_before = dischnum_pac[_n-1] > 0 & dischnum_pac[_n-1]!=.
replace used_before = . if fy==2008

collapse (sum) dischnum_pac, by(provid fy used_before)
drop if fy==2008
rename dischnum_pac dischnum_pac_usedSNF
reshape wide dischnum_pac, i(provid fy) j(used_before)

tempfile dischnum_pac_usedSNF
save `dischnum_pac_usedSNF'

use `an', clear
merge 1:1 provid fy using `dischnum_pac_usedSNF', keep(1 3) nogen

gen shref_usedbefore = dischnum_pac_usedSNF1 / dischnum_pac
lab var shref_usedbefore "Share of SNF referrals to previouly used SNFs"

gen lnnsnf_hrr_fy = ln(1+nsnf_hrr_fy)
lab var lnnsnf_hrr_fy "Ln Number of SNFs in the HRR"

rename rat_nsnf_used ratio_nsnf

lab var refhhi_prevSNFs "Referral HHI among previously used SNFs"

gen lnnsnf_used = ln(nsnf_used + 1)
lab var lnnsnf_used "Ln Number of SNFs used"

*create quintiles of # discharges
preserve
keep provid fy dischnum
duplicates drop
sum dischnum, de
xtile gp_dischnum = dischnum, n(10)
tab gp_dischnum, summarize(dischnum)
drop dischnum
tempfile gp_dischnum
save `gp_dischnum'
restore

merge m:1 provid fy using `gp_dischnum', keep(1 3) nogen

tempfile an2
save `an2'


* redefine the referral HHI among SNFs in the same HRR as the hospital
use `an2', clear
keep provid hrrnum
duplicates drop
tempfile uniqhosp
save `uniqhosp'

use `uniqhosp', clear
merge 1:m provid using SNFreferral_tchpm, keep(3) nogen

drop if cond=="HK"
collapse (sum) dischnum_pac, by(provid fy pacprovid hrrnum)
assert dischnum_pac==0 if pacprovid==.

*merge with data on HRR of SNF
merge m:1 pacprovid fy hrrnum using snf_hrr_xwalk, keep(1 3)

*tag SNF not in the same HRR : they have _m=1
gen snf_samehrr = _m==3

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys provid fy snf_samehrr: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

gen nsnf = dischnum_pac > 0

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq nsnf (mean) tot, by(provid fy snf_samehrr)
assert refhhi <= 1

bys provid fy: egen tot_hy = sum(tot)
gen shref = tot/tot_hy
drop tot_hy

reshape wide refhhi shref tot nsnf, i(provid fy) j(snf_samehrr)

foreach v of varlist refhhi* shref* tot* nsnf* {
  replace `v' = 0 if `v'==.
  assert `v'!=.
}

loc l1 "_samehrr"
loc l0 "_diffhrr"
forval x=0/1 {
  foreach v in "refhhi" "tot" "shref" "nsnf" {
    rename `v'`x' `v'`l`x''
  }
}
tempfile refhhi_samehrr
save `refhhi_samehrr'

*get # available SNFs in the hospital's HRR (who receives at least one referral from a hospital in the market)
*create # SNFs in each year-HRR
loc file `dta'/dartmouth/ZipHsaHrr14
insheet using `file'.csv, comma names clear
rename zip zip
tempfile zip_hrr
save `zip_hrr'

clear
forval y=2008/2016 {
  append using `dta'/pos/snfpos`y'
}
merge m:1 zip using `zip_hrr', keep(3) nogen

sort hrrnum fy
gen nsnf = beds > 0

collapse (sum) nsnf, by(hrrnum fy)
tempfile nsnf
save `nsnf'


use `an2', clear
merge 1:1 provid fy using `refhhi_samehrr', keep(1 3) nogen
lab var nsnf_samehrr "Number of same-HRR SNFs a hospital referred to"
gen lnnsnf_samehrr = log(nsnf_samehrr + 1)
lab var lnnsnf_samehrr "Log Number of same-HRR SNFs being referred to"
lab var shref_samehrr "Share of referrals to the same-HRR SNFs"

merge m:1 hrrnum fy using `nsnf', keep(1 3) nogen
gen lnsnf = log(nsnf + 1)
lab var lnsnf "Log # SNFs in the same HRR"
lab var nsnf "# SNFs in the same HRR"
rename lnsnf lnnsnf_mkt_samehrr
rename nsnf nsnf_mkt_samehrr

rename refhhi3 refhhi

*calculate growth rate in referral HHI
sort provid fy
bys provid: gen refhhi_l = refhhi[_n-1]
bys provid: gen refhhi_samehrr_l = refhhi_samehrr[_n-1]

gen refhhi_gr = 100*(refhhi - refhhi_l)/refhhi_l
gen refhhi_same_gr = 100*(refhhi_samehrr - refhhi_samehrr_l)/refhhi_samehrr_l

lab var nsnf_samehrr "Number of same-HRR SNFs being referred to"
lab var nsnf_used "Number of SNFs being referred to"

*create # SNFs referred to per SNF referral
foreach v of varlist nsnf_samehrr nsnf_used {
  capture drop den_`v'
  gen den_`v'=`v'/dischnum_pac
  sum den_`v', de
  des `v'
  di "`: var label `v''"
  lab var den_`v' "`: var label `v'' per case referred"
}
sum nsnf_used den_nsnf_used

*create # SNFs referred to per probability of SNF referral
foreach v of varlist den_nsnf_samehrr den_nsnf_used {
  capture drop `v'_pp
  gen `v'_pp =`v'*dischnum
  capture drop ln_`v'_pp
  gen ln_`v'_pp = ln(`v'_pp)
  sum `v'_pp, de
  des `v'_pp
}
foreach v of varlist nsnf_samehrr nsnf_used {
  di "`: var label `v''"
  lab var den_`v'_pp "`: var label `v'' per probability of referral"
  lab var ln_den_`v'_pp "Log `: var label `v'' per probability of referral"
  des den_`v'_pp
  sum den_`v'_pp, de
}

loc yv lnnsnf_used den_nsnf_used ln_den_nsnf_used_pp lnnsnf_samehrr den_nsnf_samehrr ln_den_nsnf_samehrr_pp refhhi refhhi_samehrr
des `yv'

lab var lnnsnf_used "Log Number of SNFs being referred to"
lab var refhhi "Referral concentration"
lab var refhhi_samehrr "Referral concentration in the same area"

*drop if ppr is missing (136 hospitals have missing ppr for 2011)
gen miss = ppr==.
bys provid: egen mmiss = max(miss)
tab mmiss
drop if mmiss==1
drop miss mmiss


*get indicators for top quartlie & middle 2 quartiles
loc int 2011
preserve
keep provid ppr`int'
duplicates drop
foreach pp of varlist ppr`int' {
  egen x_p75_`pp' = pctile(`pp'), p(75)
  egen x_p25_`pp' = pctile(`pp'), p(25)
  gen p75_`pp' = `pp' > x_p75_`pp'
  gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
}
drop x_* ppr`int'
tempfile quartiles`int'
save `quartiles`int''
restore

merge m:1 provid using `quartiles`int'', keep(1 3) nogen

*get indicators for top quartlie & middle 2 quartiles
foreach pp of varlist ppr {
  bys fy: egen x_p75_`pp' = pctile(`pp'), p(75)
  bys fy: egen x_p25_`pp' = pctile(`pp'), p(25)
  gen p75_`pp' = `pp' > x_p75_`pp'
  gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
}
drop x_*

*-----------------------
*create Gini coefficient

preserve
keep provid
duplicates drop
merge 1:m provid using SNFreferral_tchpm, keep(3) nogen
drop if cond=="HK"
collapse (sum) dischnum_pac, by(provid fy pacprovid)

drop if pacprovid==.
capture drop N
bys provid fy: gen N = _N
sum N
forval i = 1/`r(max)' {
    bys provid fy: gen diff`i' = abs(dischnum_pac[`i'] - dischnum_pac)
}
egen rs = rowtotal(diff*)
bys provid fy: egen numer = sum(rs)
bys provid fy: egen tot = sum(dischnum_pac)
gen denom = 2 * N * tot

gen gini = numer/denom
assert gini >=0 & gini <=1

collapse (mean) gini , by(provid fy)

tempfile gini
save `gini'
restore

merge 1:1 provid fy using `gini', keep(3) nogen
sort fy dischnum_pac
*create % of patients accounted for by the top SNF
preserve
use SNFreferral_tchpm, clear
collapse (sum) dischnum_pac, by(provid pacprovid fy)
bys provid fy: egen maxref = max(dischnum_pac)

*count # SNFs getting the largest # referrals
gen x = maxref==dischnum_pac
bys provid fy: egen sx = sum(x)
tab sx
* range from 1 (94%)-13
bys provid fy: egen totref = sum(dischnum_pac)
gen shref_bytopSNF =  dischnum_pac/totref if x==1

*create # SNFs that received 80% of referrals
gsort provid fy -dischnum_pac
bys provid fy: gen cumref = sum(dischnum_pac)
gen cumshref = cumref/totref
gen below80 = cumshref <= 0.8
bys provid fy: egen reqSNF_80pct = sum(below80)
replace reqSNF_80pct = reqSNF_80pct + 1

collapse (mean) reqSNF_80pct (max) shref_bytopSNF, by(provid fy)
tempfile newoutcome
save `newoutcome'
restore

merge 1:1 provid fy using `newoutcome', keep(1 3) nogen
lab var shref_bytopSNF "Share of referrals by the top SNF"
lab var reqSNF_80pct "Number of SNFs required to account for 80% of SNF referrals"
gen lnreqSNF_80pct = log(reqSNF_80pct+1)
lab var lnreqSNF_80pct "Log Number of SNFs required to account for 80% of SNF referrals"
*tab provid if nsnf_used > 63

tempfile tmp2
save `tmp2'

*-----------------------
*get H/K condition outcomes : den_nsnf_used refhhi lnreqSNF_80pct shref_bytopSNF read30 read30_pac & mean ses_score & % blacks

*refhhi den_nsnf_used read30_pac
use SNFreferral_tchpm, clear
keep if cond=="HK"
collapse (sum) dischnum_pac read30_pac, by(provid fy pacprovid)

bys provid fy: egen tot = sum(dischnum_pac)
gen refshsq = (dischnum/tot)^2

gen nsnf = dischnum_pac > 0

bys provid fy: egen totread = sum(read30_pac)

*create % of patients accounted for by the top SNF
bys provid fy: egen maxref = max(dischnum_pac)

*count # SNFs getting the largest # referrals
gen x = maxref==dischnum_pac
bys provid fy: egen sx = sum(x)
tab sx
* range from 1 (94%)-13
bys provid fy: egen totref = sum(dischnum_pac)
gen shref_bytopSNF =  dischnum_pac/totref if x==1

*create # SNFs that received 80% of referrals
gsort provid fy -dischnum_pac
bys provid fy: gen cumref = sum(dischnum_pac)
gen cumshref = cumref/totref
gen below80 = cumshref <= 0.8
bys provid fy: egen reqSNF_80pct = sum(below80)
replace reqSNF_80pct = reqSNF_80pct + 1

collapse (sum) refhhi = refshsq nsnf dischnum_pac (mean) totread reqSNF_80pct (max) shref_bytopSNF, by(provid fy)
assert refhhi <= 1

gen den_nsnf_used = nsnf / dischnum_pac
gen read30_pac = totread / dischnum_pac
drop nsnf totread

tempfile hk1
save `hk1'

*use HK index admissions data
use index_admit_chm, clear
keep if cond=="HK"
collapse (sum) black white read30 dischnum (mean) ses_score, by(provid fy)
merge 1:1 provid fy using `hk1', keep(3) nogen

replace read30 = read30/dischnum
gen shref = dischnum_pac / dischnum
foreach v of varlist white black {
  replace `v' = `v'/dischnum
}
lab var shref_bytopSNF "Share of referrals by the top SNF"
lab var reqSNF_80pct "Number of SNFs required to account for 80% of SNF referrals"
gen lnreqSNF_80pct = log(reqSNF_80pct+1)
lab var lnreqSNF_80pct "Log Number of SNFs required to account for 80% of SNF referrals"
gen cond = "HK"

tempfile hk2
save `hk2'

use `tmp2', clear
foreach v of varlist black white read30 dischnum ses_score refhhi dischnum_pac reqSNF_80pct shref_bytopSNF den_nsnf_used read30_pac shref lnreqSNF_80pct {
  drop `v'
}
tab fy
merge 1:1 provid fy using `hk2', keep(3) nogen

append using `tmp2'
replace cond = "Initial" if cond==""

tab fy cond
sort provid fy cond
bys provid fy : gen nn = _N
keep if nn==2
drop nn

*create change in referral HHI from 2008 level
*first get 2008 referral HHI
preserve
use SNFreferral_tchpm, clear
keep if fy==2008
gen c2 = "Initial" if condition!="HK"
replace c2 = "HK" if condition=="HK"
collapse (sum) dischnum_pac, by(c2 provid pacprovid fy)
assert dischnum_pac==0 if pacprovid==.

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys c2 provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq (mean) tot, by(c2 provid)
assert refhhi <= 1
replace refhhi = . if refhhi ==0 & tot==0
drop tot

rename refhhi refhhi_2008
rename c2 condition

tempfile refhhi08
save `refhhi08'
restore

rename cond condition
merge m:1 provid condition using `refhhi08', keep(1 3) nogen

gen refhhi_chng08 = 100*(refhhi - refhhi_2008)/refhhi_2008
lab var refhhi_chng08 "Percent change from 2008 SNF referral concentration"

* add number of patients in each comorbidity category
preserve
use index_admit_comorbid_chy, clear
collapse (sum) metacancer-hipfracture_ct, by(provid fy)
tempfile cnt
save `cnt'
restore

merge m:1 provid fy using `cnt', keep(1 3) nogen
foreach v of varlist metacancer_ct-hipfracture_ct {
  di "`v'------------"
  qui replace `v' = `v'/dischnum if cond=="Initial"
  *replace `v' = . if cond=="HK"

  qui count if `v' > 1 & `v'!=. & cond=="Initial"
  di "`v': `r(N)' obs have the rate > 1"
  sum `v' if cond=="Initial"
  di ""
}

*-----------------------
*add participation in other value-based reforms
*-----------------------
/* keep if cond=="Initial"
drop cond */

*EHR
merge m:1 provid fy using EHR-MU/ehr, keep(1 3)
sort cond provid fy
replace EHRstage1=0 if _m==1
replace EHRstage2=0 if _m==1
assert EHRstage1!=. & EHRstage2!=.
drop _m

*VBP
merge m:1 provid fy using VBP/vbp, keep(1 3)
tab fy if _m==1 & cond=="Initial"
*list cond provid fy _m vbp_adjf if provid==670055
replace vbp_adjf=1 if _m==1
assert vbp_adjf!=.
drop _m

*HAC
merge m:1 provid fy using HAC/hac, keep(1 3)
tab fy if _m==1 & cond=="Initial"
*2015 & 2016 all matched
replace HACstatus = 0 if _m==1
assert HACstatus!=.
drop _m HACscore

*ACO
merge m:1 provid fy using ../costreport/hosp_chars_cr, keep(1 3) keepusing(pionACO)
tab fy if _m==1 & cond=="Initial"
bys provid: egen mm = max(pionACO)
replace pionACO = mm if _m==1 & pionACO==.
replace pionACO = 0 if fy < 2011
assert pionACO!=.
drop mm _m


compress
save anpenalty_VI_agg3c, replace

*-----------------------
*to send to Lily, only get hospital-FY level penalty pressure data
use anpenalty_VI_agg3c, clear

xi i.gp_beds i.fy
loc sp _Igp_beds* vi_snf_l own_* urban teaching pac_hhi_hrr
keep provid fy ppst* ppr* lppd* p25* p75* _Ify* post `sp'
order provid fy ppst* ppr* lppd* p25* p75* _Ify* post `sp'

compress
save anpenalty_VI_agg3c_lily, replace

outsheet using anpenalty_VI_agg3c_lily.csv, replace comma names
