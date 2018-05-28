*descriptive analysis of the trend in vertical integration over time

set matsize 11000
loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use penalty_VI_bycond, clear
keep if match==1
keep provid fy refhhi3 beds
duplicates drop
xtile gp_beds = beds, n(10)
tab gp_beds, summarize(beds)

gen x = beds if fy==2009
bys provid: egen beds09 = max(x)

areg refhhi3 i.fy i.gp_beds, absorb(provid) cluster(provid)

tempfile tmp
save `tmp'


loc Large "if beds09 >= 300"
loc Medium "if beds09 >= 100 & beds09 < 300"
loc Small "if beds09 < 100"
loc Largesc  ysc(r(0 0.15)) ylab(0(0.05)0.15)
loc Mediumsc ysc(r(0 0.25)) ylab(0(0.05)0.25)
loc Smallsc ysc(r(0 0.5)) ylab(0(0.1)0.5)

*plot mean by size
use `tmp', clear
gen size = .
loc n = 0
foreach h in  "Small" "Medium" "Large" {
  replace size = `n'+1 ``h''
  loc n = `n'+1
}
assert size!=.
preserve
statsby upper=r(ub) lower=r(lb), clear by(size fy): ci refhhi3
tempfile ci
save `ci'
restore

collapse (mean) mean=refhhi3, by(size fy)
merge 1:1 size fy using `ci', nogen

loc spec xti(Year ending in June, size(medium)) yti("Mean SNF referral concentration", size(medium)) leg(order(4 "Small" 5 "Medium" 6 "Large") cols(3)) xsc(r(2009 2016)) xlab(2009(1)2016) note("Notes. The SNF referral concentration among AMI, HF, and PN patients is plotted.") ysc(r(0 0.5)) ylab(0(0.1)0.5)

tw (rarea upper lower fy if size==1, color(gs14)) (rarea upper lower fy if size==2, color(gs14)) (rarea upper lower fy if size==3, color(gs14)) (line mean fy if size==1, lpattern(dash) lcolor(black)) (line mean fy if size==2, lcolor(black) lpattern(solid)) (line mean fy if size==3, lcolor(black) lpattern(dot)) (scatter mean fy if size==1, mcolor(black)) (scatter mean fy if size==2, mcolor(black)) (scatter mean fy if size==3, mcolor(black)) , `spec' ``h'sc'
graph export `gph'/refhhi3_bysize.eps, replace

loc spec xti(Year ending in June, size(medium)) yti("SNF referral concentration", size(medium)) leg(order(2 "75th percentile" 3 "Median" 4 "25th percentile") cols(3)) xsc(r(2009 2016)) xlab(2009(1)2016) note("Notes. The SNF referral concentration among AMI, HF, and PN patients is plotted.")

foreach h in "Large" "Medium" "Small" {
  use `tmp', clear
  keep ``h''
  collapse (p25) p25=refhhi3 (p50) p50=refhhi3 (p75) p75=refhhi3 , by(fy)
  tw (rarea p75 p25 fy, color(gs14)) (line p75 fy, lcolor(black) lpattern(dash) lwidth(medthick) ) (line p50 fy, lcolor(black) lwidth(medthick) ) (line p25 fy, lcolor(black) lpattern(dot) lwidth(medthick)) (scatter p75 fy, mcolor(black)) (scatter p50 fy, mcolor(black)) (scatter p25 fy, mcolor(black)), `spec' subti(`h' hospitals) ``h'sc'
  graph export `gph'/bs_refhhi3_`h'.eps, replace
}

binscatter refhhi3 fy if beds09 >= 300, absorb(provid) discrete xsc(r(2008 2016)) xlab(2009(1)2016) ysc(r(0 0.15)) ylab(0(0.05)0.15) `spec' subti(Large hospitals)
graph export `gph'/bs_refhhi3_lg.eps, replace

binscatter refhhi3 fy if beds09 >= 100 & beds09 < 300, absorb(provid) discrete xsc(r(2008 2016)) xlab(2009(1)2016) ysc(r(0 0.25)) ylab(0(0.05)0.25) `spec' subti(Medium hospitals)
graph export `gph'/bs_refhhi3_med.eps, replace

binscatter refhhi3 fy if beds09 < 100, absorb(provid) discrete xsc(r(2008 2016)) xlab(2009(1)2016) ysc(r(0 0.5)) ylab(0(0.1)0.5) `spec' subti(Small hospitals)
graph export `gph'/bs_refhhi3_sm.eps, replace
*----------------------------------------
*construct cross-sectional variation  penalty pressure for each hospital-condition using 2009-2011 in 4 ways
*1) Atul (2017): f(r_t-3, r_t-2, r_t-1) where r is raw readmission rate
*2) my proposal: f(r_t-3, r_t-2, r_t-1, sh_t-3, sh_t-2, sh_t-3) where sh is share of Medicare payment
*3) ERR in 2013
*4) overall penalty rate in 2013

forval t=2014/2018 {
  loc t1 = `t'-5
  loc t2 = `t'-3

  *get raw readmission rates during t-3 - t-1
  use `an2', clear
  keep if fy >= `t1' & fy <= `t2'
  tab fy

  collapse (mean) read30 dischnum sh_pmt, by(cond provid fy)
  drop if cond=="HK"
  collapse (sum) read30 dischnum sh_pmt, by(provid fy)
  collapse (sum) read30 dischnum (mean) sh_pmt, by(provid)
  gen rra = read30/dischnum

  keep provid rra sh_pmt
  gen prod = rra * sh_pmt

  tempfile rra
  save `rra'

  *get penalty status in t + 2
  use `dta'/hrrp_penalty, clear
  keep if fy==`t'
  keep pnltr provid
  duplicates drop
  gen penalized = pnltr > 0
  gen fy=`t'

  merge 1:1 provid using `rra', keep(3) nogen

  *local linear regression of penalty status on raw readmissions only
  locreg penalized, continuous(rra) gen(phat, replace) logit

  tempfile phat`t'
  save `phat`t''
}

*append phat's across 2014-2016
clear
forval t = 2014/2018 {
  append using `phat`t''
}
des
tab fy
sum phat
tempfile phats
save `phats'

/* foreach c in "ami" "pn" "hf" {
    gen poserr_`c' = err_`c' >=1
    gen comp_`c' = poserr_`c' * (err_`c' -1) * sh_pmt_`c'/100
}
egen pnltr_redone = rowtotal(comp_*)
replace pnltr_redone = pnltr_redone
sum pnltr*

*compare actual penalty rate and my construct using the Medicare payment share and ERR's
sort pnltr14
list provid pnltr* comp* err* n_* sh_pmt* in 1/100

sc pnltr14 pnltr_redone if pnltr_redone < 0.021 || sc  pnltr14  pnltr14
graph export `gph'/test.eps, replace
*/



*----------------------------------
*local linear regression of penalty status on raw readmissions only
use `phats', clear

keep if fy==2014

tw (line phat rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) sort) (scatter penalized rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) xti("Raw 30-day readmission rate during 2009-2011") yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital's actual penalty status") col(1)))
graph export `gph'/pnlt_rra.eps, replace

tw (kdensity rra if penalized==0, yti(Kernel density)) (kdensity rra if penalized==1), leg(order(1 "Non-penalized hospitals" 2 "Penalized hospitals" )) xti(Raw 30-day readmission rate during 2009-2011)
graph export `gph'/density_rra_bypnlty.eps, replace

*local linear regression of penalty status on raw readmissions & share
replace prod = prod/100
locreg penalized, continuous(prod) gen(phat2, replace) logit

tw (line phat2 prod, xsc(r(0 0.4)) xlab(0(0.05)0.4) sort)  (scatter penalized prod, xsc(r(0 0.4)) xlab(0(0.05)0.4)  xti("Raw 30-day readmission rate X Share of Medicare payment in 2009-2011", size(small)) yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital's actual penalty status") col(1)))
graph export `gph'/pnlt_rraXsh.eps, replace

*----------------------------------
* was there any mean reversion in the referral concentration?

*plot referral HHI for 3 initial conditions in 2008 [x-axis] vs change in referral HHI between 2009-11 and 2012-14 across hospitals [y-axis]
use `an2', clear
keep provid fy refhhi3
duplicates drop
keep if fy==2008
ren refhhi3 refhhi3_08
drop fy
tempfile v08
save `v08'

*get referral HHI for 2009-11 and 2012-14
use PACreferral_tchpm, clear
drop if cond=="HK"
keep if pac=="SNF"
keep if fy > 2008

gen t = 1 if fy >= 2009 & fy <= 2011
replace t = 2 if fy >= 2012 & fy <= 2014
replace t = 3 if fy >= 2015 & fy <= 2016
assert t!=.
collapse (sum) dischnum_pac, by(provid t pacprovid)

bys provid t: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(provid t)
assert refhhi <= 1

reshape wide refhhi, i(provid) j(t)
gen change1 = refhhi2 - refhhi1
gen change2 = refhhi3 - refhhi1

merge 1:1 provid using `v08', keep(3) nogen

xtile gp = refhhi3_08, nq(25)

rename refhhi1 refhhi_0911
rename refhhi2 refhhi_1214
rename refhhi3 refhhi_1516

tempfile tmp
save `tmp'

*QQ plot of referral HHI in 2009-11 vs referral HHI in 2012-2014
qqplot refhhi_1214 refhhi_0911, xti(SNF referral HHI in 2009-2011) yti(SNF referral HHI in 2012-2014)
graph export `gph'/qqplot_refhhi_0911vs1214.eps, replace

qqplot refhhi_1516 refhhi_0911, xti(SNF referral HHI in 2009-2011) yti(SNF referral HHI in 2015-2016)
graph export `gph'/qqplot_refhhi_0911vs1516.eps, replace

qqplot refhhi_1516 refhhi_1214, xti(SNF referral HHI in 2012-2014) yti(SNF referral HHI in 2015-2016)
graph export `gph'/qqplot_refhhi_1214vs1516.eps, replace


qqplot days_ho days_nho, xti(Readmission not followed by a handoff, size(medium)) yti(Readmission followed by a handoff, size(medium) height(5)) title("Quantile-Quantile plot" "Number of days to readmission from the last nurse visit", size(medium))
graph export `gph'/days2readmit3.eps, replace



*mean change in referrals vs referral in 2008
collapse (mean) refhhi3_08 change1 change2, by(gp)

sc change1 change2 refhhi3_08, xti(SNF referral HHI for 3 conditions in 2008) yti("Change in SNF referral HHI, After - Before") ysc(r(-0.1 0.1)) ylab(-0.1(0.05)0.1) leg(order(1 "Change from 2009-2011 to 2012-2014" 2 "Change from 2009-2011 to 2015-2016") col(1))
graph export `gph'/chng_VI_by08value.eps, replace

*plot the referral concentration trend across FY for high-penalty vs low-penalty hospitals
use `an2', clear
gen x = pnltr if fy==2013
bys provid: egen pnltr13= max(x)
keep provid fy refhhi3 pnltr13 size
duplicates drop

gen fy0 = fy
replace fy = fy + 2
merge m:1 provid fy using `phats', keep(1 3) nogen
drop fy
rename fy0 fy

*divide into 3 equal sized groups
preserve
keep if fy==2014
keep provid pnltr13 phat
duplicates drop
xtile gp = pnltr13, nq(3)
table gp, c(mean pnlt)

xtile gp_phat = phat, nq(3)
table gp_phat, c(mean phat)
tempfile gp
save `gp'
restore

merge m:1 provid using `gp', keepusing(gp*) nogen

*binscatter change in referral concentration in 2012, 2013, 2014, 2015, 2016 from 2009-2011 level vs phat
*merge with referral HHI during 2009-2011
merge m:1 provid using `tmp', keepusing(refhhi1 refhhi3_08) keep(3) nogen

tab fy, summarize(phat)
forval yr = 2012/2016 {
  gen chng_to_`yr' = 100*(refhhi3 - refhhi1)/refhhi1
}

binscatter refhhi1 phat, by(fy)
graph export `gph'/test.eps, replace

forval yr = 2012/2016 {
  binscatter chng_to_`yr' phat if fy==`yr', controls(i.size)
  graph export `gph'/test`yr'.eps, replace
}


binscatter refhhi3 fy if gp==1, absorb(provid) controls(i.size) rd(2012) line(qfit)
graph export `gph'/test.eps, replace






preserve
collapse (mean) refhhi3, by(gp fy)

tab gp, summarize(refhhi3)

tw (connected refhhi3 fy if gp==1) (connected refhhi3 fy if gp==2) (connected refhhi3 fy if gp==3), yti(Mean SNF referral HHI for 3 conditions) xti(FY) xline(2010.5) xline(2011.5) xlab(2008(1)2016) leg(order(1 "Low penalty" 2 "Medium penalty" 3 "High penalty") col(1)) ysc(r(0.15 0.25)) ylab(0.15(0.01)0.25)
graph export `gph'/refhhi3_fy_bypnlt13.eps, replace
restore

preserve
collapse (mean) refhhi3, by(gp_phat fy)
tab gp, summarize(refhhi3)

tw (connected refhhi3 fy if gp==1) (connected refhhi3 fy if gp==2) (connected refhhi3 fy if gp==3), yti(Mean SNF referral HHI for 3 conditions) xti(FY) xline(2011.5) xlab(2008(1)2016) leg(order(1 "Low probability of penalty" 2 "Medium probability of penalty" 3 "High probability of penalty") col(1)) ysc(r(0.15 0.28)) ylab(0.15(0.01)0.28)
graph export `gph'/refhhi3_fy_byphat.eps, replace
restore
*----------------------------------
*run regression of probability of penalty X post
use `an', clear
drop if cond=="HK"
collapse (mean) ses_score vi* refhhi3 size own* urban teaching hsahhi_SNF_* (sum) white black dischnum dischnum_pac, by(provid fy)
egen hsahhi_SNF = rowmean(hsahhi_SNF_AMI hsahhi_SNF_HF hsahhi_SNF_PN)

*keep provid fy refhhi3 size own* urban teaching vi* ses_score white black dischnum
duplicates drop
gen whiter = white/dischnum
gen blackr = black/dischnum
gen ln_dischnum = ln(dischnum +1)
gen ln_dischnum_pac = ln(dischnum_pac +1)
gen shref = dischnum_pac / dischnum

gen fy0 = fy
replace fy = fy + 2
merge m:1 provid fy using `phats', keep(1 3) nogen
drop fy
rename fy0 fy

capture drop post phatXpost
gen post = fy >=2012
gen phatXpost = phat * post
replace phatXpost = 0 if post==0

gen phat14 = phat if fy==2014
bys provid: egen x = max(phat14)
drop phat14
rename x phat14

loc sp1 i.size ln_dischnum ln_dischnum_pac
loc sp2 `sp1' own* urban teaching
loc sp3 `sp2' hsahhi_SNF
loc sp4 `sp3' vi*

tab fy , summarize(phatXpost)

areg refhhi3 i.fy c.phat14##i.fy `sp2', absorb(provid)
areg refhhi3 i.fy phatXpost `sp2', absorb(provid)

loc sp1 i.size ln_dischnum
loc sp2 `sp1' own* urban teaching
loc sp3 `sp2' hsahhi_SNF
loc sp4 `sp3' vi*

areg shref i.fy c.phat14##i.fy `sp2', absorb(provid)
areg shref i.fy phatXpost `sp2', absorb(provid)


ivreg2 refhhi3 i.fy phatXpost `sp2', absorb(provid)

preserve
gen x = pnltr if fy==2013
bys provid: egen pnltr13 = mean(x)
keep if fy < 2012
keep provid cond pnltprs pnltr13

collapse (mean) pnltprs pnltr13, by(provid cond)

gen cond2= "3c" if cond!="HK"
replace cond2 = "HK" if cond2==""
collapse (sum) pnltprs (mean) pnltr13, by(provid cond2)

rename pnltprs pnltprs_
reshape wide pnltprs_ , i(provid pnltr13) j(cond2) string

sum pnlt*, de

foreach v of varlist pnlt* {
  sum `v', de
  gen `v'_p75 = `v' >= `r(p75)'
  replace `v'_p75 = . if `v'==.
}

tempfile pnltprs
save `pnltprs'
restore

merge m:1 provid using  `pnltprs', nogen

tempfile an
save `an'

drop read*_pac samh*pac *rev *inc hrrhhi* hsahhi* dischnum*08 read* beds-totepi_out hrrnum hsanum hrr_str

compress
outsheet using hosp_fy_VI.csv, replace names comma

*----------------------------------------
*	# admissions over time
use `an', clear

keep provid fy dischnum cond size beds
duplicates drop
bys provid: egen min = min(size)
bys provid: egen max = max(size)
table provid if min!=max, contents(mean min mean max)
sort cond provid fy
list if provid==10011

tab cond fy

preserve
keep if cond!="HK"
graph box dischnum, over(fy, label(angle(45))) by(cond) yti(Total number of discharges) ti(Box plot, size(medium)) subti(Total number of discharges by FY)
graph export `gph'/dischnum_ahp.eps, replace




* 1) create hospital-time level data

*create hospital-month level data
use match_pnlthosp, clear

*total # PAC referrals for each hospital-month
collapse (sum) *_pac, by(provid dischyear dischmth fy qtr pac cond)

*merge with hospital-month level # discharges, readmits, etc.
merge m:1 condition provid dischyear dischmth using `index2', keep(3) nogen

*merge with hosp chars data
merge m:1 provid fy using `costreport', nogen keep(1 3)

* exclude hospitals with small # beds
preserve
collapse (min) beds, by(provid)
*tab beds

*tag beds < 30
gen smallbeds = beds < 30
tab smallbeds
*93 hospitals

drop beds
tempfile smallbeds
save `smallbeds'
restore

merge m:1 provid using `smallbeds', nogen
drop if smallbeds==1
drop smallbeds

compress
save referral_hm, replace
* this is a balanced panel where every month appears for each pac-cond pair

*----------------------------------------------
*create hospital-quarter level data
use referral_hm, clear
*create quarterly data
collapse (sum) *_pac dischnum-read90 (mean) ses_score, by(pac cond provid dischyear qtr fy own* teaching urban beds dischrg size)

compress
save referral_hq, replace

*-----------
*construct for each hospital-quarter, the referral concentration
use match_pnlthosp, clear

*aggregate to hosp-PAC provider-quarter level
collapse (sum) dischnum_pac, by(pac cond provid pacprovid dischyear qtr fy)
rename dischnum_pac ref_hpq

merge m:1 pac cond provid dischyear qtr fy using referral_hq, keep(2 3) nogen
gen sh2 = (ref_hpq / dischnum_pac)^2
collapse (sum) refhhi_hq = sh2, by(pac cond provid dischyear qtr fy)
sum refhhi

tempfile refhhi
save `refhhi'
*-----------
use referral_hq, clear
merge 1:1 pac cond provid dischyear qtr fy using `refhhi', nogen

*get PAC market concentration

*get HRR, HSA of hospital from dartmouth atlas data
rename provid provider
merge m:1 provid using /ifs/home/kimk13/VI/data/dartmouth/hosp_hrr_xwalk, keepusing(hrrnum hsanum) keep(1 3) nogen
rename provider provid

*merge with HRR (HSA)-FY level PAC market HHI data
foreach g0 in "hrr" "hsa" {
  loc g `g0'num
  merge m:1 `g' fy using `dta'/`g0'hhi, keep(1 3)
  assert `g'==. | fy < 2011 if _m==1
  drop _merge
}

compress
save referral_hq, replace

outsheet using referral_hq.csv, comma names replace

/* keep dischyear pac cond
duplicates drop
sort pac cond dischyear
expand 12
bys pac cond dischyear: gen dischmth = _n
drop if dischyear==2010 & dischmth < 7
drop if dischyear==2015 & dischmth > 6

merge 1:m pac cond dischyear dischmth using `referral_hm' */

*-------------------------
* 2) analyze the number of PAC referrals by each hospital in each quarter
*histogram of the quarterly # referrals / % referrals to PAC by PAC type & condition

use referral_hq, clear
foreach pac in "HHA" "SNF" {
  tw hist dischnum_pac if pac=="`pac'", discrete by(cond) frac xtitle("Quarterly number of referrals to `pac' by condition", size(medium)) ysc(r(0 0.4))
  graph export `gph'/dischnum_pac_`pac'.eps, replace
}

use referral_hq, clear
foreach pac in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    di ""
    di "`pac' & `c'---------------"
    tab dischnum_pac if pac=="`pac'" & cond=="`c'"
  }
}

*Distribution of the quarterly number of referrals across hospital-quarter pairs by condition X PAC type
use referral_hq, clear
keep pac cond dischnum_pac provid dischyear qtr
gen i = 1
collapse (sum) i, by(dischnum_pac condition pac dischyear qtr)
collapse (sum) i, by(dischnum_pac condition pac)
bys condition pac: egen tot = sum(i)
gen pct_hq = 100* i / tot
sort pac cond dischnum
tempfile tmp
save `tmp'

use `tmp', clear
drop i
gen num = 1 if dischnum_pac==1
replace num = 2 if dischnum_pac > 1 & dischnum_pac < 6
replace num = 3 if dischnum_pac > 5 & dischnum_pac < 11
replace num = 4 if dischnum_pac > 10 & dischnum_pac < 21
replace num = 5 if dischnum_pac > 20 & dischnum_pac < 31
replace num = 6 if dischnum_pac > 30
assert num!=.
lab define ll 1 "1" 2 "2-5" 3 "6-10" 4 "11-20" 5 "21-30" 6 "31+"
lab val num ll
collapse (sum) pct_hq, by(cond pac num tot)
list
rename pct_hq pct_hq_
rename tot tot_
reshape wide pct_hq tot_, i(num cond) j(pac) string
reshape wide pct_hq* tot*, i(num) j(condition) string
preserve
drop tot*
outsheet using `reg'/dist_ref_hq.csv, replace comma names
restore
*--------------------------

* 3) analyze the share of PAC referrals out of total # hospital discharges by each hospital in each quarter

use referral_hq, clear

*create the share of PAC referrals out of total # hospital discharges
gen shref = dischnum_pac / dischnum
sum shref
assert shref >= 0 & shref <= 1

tempfile an
save `an'

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use `an', clear

    gen yqd = yq(dischyear, qtr)
    format yqd %tq

    loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
    areg shref `x_ht' i.qtr i.yqd if pac=="`p'" & cond=="`c'", absorb(provid) cluster(provid)

    *save coefficients to plot
    tempfile coef_`p'_`c'
    parmest, saving(`coef_`p'_`c'')

    use `coef_`p'_`c'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "yqd")
    split parm, p(".")
    drop if estimate==0

    gen a = mod(_n+3, 4)
    replace a = 4 if a==0
    rename a qtr
    gen yr = 2011 + floor((_n-2)/4)

    keep estimate min95 max95 yr qtr
    rename estimate coef
    gen cond = "`c'"
    tempfile coef_`p'_`c'2
    save `coef_`p'_`c'2'
  }

  use `coef_`p'_AMI2', clear
  foreach c in "HF" "PN" "HK" {
    append using `coef_`p'_`c'2'
  }
  compress
  outsheet using `reg'/coef_shref_`p'_qtradj.csv, replace comma names
}

*just plot the raw data

foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  keep if pac=="`p'"

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh
  replace refhhi = refhhi*100

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean % hospital discharges to `p', size(small))
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of share of hospital discharges to `p', size(small))
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*--------------------------
* 4) analyze the PAC referral concentration in each hospital in each quarter

*regression using interaction of penalty status & FY dummies to plot DiD coefficients

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use referral_hq, clear

    gen yqd = yq(dischyear, qtr)
    format yqd %tq

    loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
    areg refhhi `x_ht' i.qtr i.yqd if pac=="`p'" & cond=="`c'" & dischnum_pac!=1, absorb(provid) cluster(provid)

    *save coefficients to plot
    tempfile coef_`p'_`c'
    parmest, saving(`coef_`p'_`c'')

    use `coef_`p'_`c'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "yqd")
    split parm, p(".")
    drop if estimate==0

    gen a = mod(_n+3, 4)
    replace a = 4 if a==0
    rename a qtr
    gen yr = 2011 + floor((_n-2)/4)

    keep estimate min95 max95 yr qtr
    rename estimate coef
    gen cond = "`c'"
    tempfile coef_`p'_`c'2
    save `coef_`p'_`c'2'
  }

  use `coef_`p'_AMI2', clear
  foreach c in "HF" "PN" "HK" {
    append using `coef_`p'_`c'2'
  }
  compress
  outsheet using `reg'/coef_VI_`p'_qtradj2.csv, replace comma names
}

*just plot the raw data

foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  keep if pac=="`p'"

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean `p' referral concentration, size(medium))
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of `p' referral concentration, size(medium))
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*only among large hospitals
foreach p in "HHA" "SNF" {
  *foreach c in "AMI" "HF" "PN" "HK" {
  use `an', clear
  bys size: sum beds
  keep if pac=="`p'" & size==3 & dischnum_pac!=1

  gen yqd = yq(dischyear, qtr)
  format yqd %tq

  sort pac cond provid yqd
  bys pac cond provid: gen lsh = refhhi[_n-1]
  gen grrefhhi = 100*(refhhi-lsh)/lsh

  collapse (mean) refhhi grrefhhi, by(cond yqd)

  loc y refhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean `p' referral concentration, size(medium)) subti(Large hospitals with 500+ beds)
  graph export `gph'/`y'_`p'_yqd.eps, replace
  *}

  loc y grrefhhi
  tw (connected `y' yqd if cond=="AMI")  (connected `y' yqd if cond=="HF") (connected `y' yqd if cond=="PN") (connected `y' yqd if cond=="HK"), ysc(r(0 100)) ylab(0(10)100) leg(order(1 "AMI" 2 "HF" 3 "PN" 4 "HK") col(4)) xti(Quarter) yti(Percentage) ti(Mean growth (%) of `p' referral concentration, size(medium)) subti(Large hospitals with 500+ beds)
  graph export `gph'/`y'_`p'_yqd.eps, replace
}

*--------------------------
*plot referral HHI
use referral_hq, clear
gen yqd = yq(dischyear, qtr)
tab yqd
*202 - 221
format yqd %tq

loc pac HHA
loc c AMI
binscatter refhhi yqd if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1, linetype(qfit) rd(206 210 214 218) absorb(provid) controls(qtr i.size own* ses_score teaching urban) ysc(r(0 1)) xlab(202(1)221)

sc refhhi yqd if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1, connect()
graph export `gph'/test.eps, replace

foreach pac in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
  }
}



*--------------------------
*plot referral HHI density curves across FY
use referral_hq, clear
gen yqd = yq(dischyear, qtr)
tab yqd
*202 - 221
format yqd %tq

loc c "AMI"
loc pac "SNF"

preserve
keep if pac=="`pac'" & cond=="`c'" & dischnum_pac!=1

kdensity refhhi , nograph gen(x fx)
forval y = 2011/2015 {
  kdensity refhhi if fy==`y' , nograph gen(fx`y') at(x)
  lab var fx`y' "`y'"
}
line fx2011 fx2012 fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle(`pac' referral concentration in each hospital-quarter for `c')
graph export `gph'/test.eps, replace
restore

*-------------------------
*get number of discharges per month, quarter, year, respectively

loc tunit_mo dischyear dischmth
loc tunit_qtr dischyear qtr
loc tunit_fy fy

*monthly
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "mo"

*reshape wide first
reshape wide dischnum-samh90, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace sum(log) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*quarterly
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "qtr"
*aggregate to the quarter level
collapse (sum) dischnum, by(condition provid pac pacprovid dischyear qtr)

*reshape wide first
reshape wide dischnum, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace  sum(detail) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*by FY
use `match_pnlthosp', clear
*bys condition: sum dischnum

loc t "fy"
*aggregate to the quarter level
collapse (sum) dischnum, by(condition provid pac pacprovid fy)

*reshape wide first
reshape wide dischnum, i(provid pacprovid `tunit_`t'') j(condition) string

foreach c in "AMI" "HF" "PN" "HK" {
  lab var dischnum`c' "`c'"
}

foreach p in "SNF" "HHA" {
  preserve
  keep if pac=="`p'"
  keep dischnumAMI dischnumHF dischnumPN dischnumHK
  order dischnumAMI dischnumHF dischnumPN dischnumHK
  outreg2 using `reg'/nreferral_`p'_`t'.xls, replace  sum(detail) eqkeep(N mean sd min p25 p50 p75 max) label
  restore
}

*----------------------------
*at the FY level, number & growth rate of referrals to all PAC providers & # PAC referral partners & growth of PAC partners by condition

loc t "fy"
foreach p in "HHA" "SNF" {
  use `match_pnlthosp', clear
  keep if pac=="`p'"

  *aggregate to the hospital-PAC provider-FY level
  collapse (sum) dischnum, by(condition provid pacprovid fy)

  *aggregate to the hospital-FY level
  gen i = 1
  collapse (sum) dischnum npac = i, by(condition provid fy)

  * by condition, number & growth rate of referrals to PAC across time

  *compute growth rate in # referrals & # referral sources
  sort condition provid fy
  foreach v of varlist dischnum npac {
    bys condition provid: gen `v'_l = `v'[_n-1]
    gen gr_`v' = 100*(`v'-`v'_l)/`v'_l
  }
  drop *_l

  lab var dischnum "Mean number of referrals to `p'"
  lab var gr_dischnum "Mean growth (%) of referrals to `p'"
  lab var npac "Mean number of `p' referral partners"
  lab var gr_npac "Mean growth (%) of `p' referral partners"

  foreach c in "AMI" "HF" "PN" "HK" {
    preserve
    keep if cond=="`c'"
    drop provid
    order dischnum gr_dischnum npac gr_npac
    bysort fy: outreg2 using `reg'/reftrend1_`p'.xls, append sum(log) eqkeep(N mean) label
    restore
  }
}

*----------------------------
*construct referral HHI & growth in referral HHI at the FY level

foreach p in "HHA" "SNF" {
  use `match_pnlthosp', clear
  keep if pac=="`p'"

  *aggregate to the hospital-PAC provider-FY level
  collapse (sum) dischnum, by(condition provid pacprovid fy)

  * by condition-FY, level & growth rate of referral concentration among PAC providers

  bys condition provid fy: egen tot = sum(dischnum)
  gen refshsq = (dischnum/tot)^2

  *aggregate to the hospital-FY level
  collapse (sum) refhhi = refshsq, by(condition provid fy)
  assert refhhi <= 1

  *compute growth rate in # referrals & # referral sources
  sort condition provid fy
  foreach v of varlist refhhi {
    bys condition provid: gen `v'_l = `v'[_n-1]
    gen gr_`v' = 100*(`v'-`v'_l)/`v'_l
  }
  drop *_l

  lab var refhhi "HHI of Referrals to `p'"
  lab var gr_refhhi "Growth (%) of HHI of Referrals to `p'"

  foreach c in "AMI" "HF" "PN" "HK" {
    preserve
    keep if cond=="`c'"
    drop provid
    order refhhi gr_refhhi
    bysort fy: outreg2 using `reg'/reftrend2_`p'.xls, append sum(log) eqkeep(N mean) label
    restore
  }

  tempfile `p'_refhhi
  save ``p'_refhhi'
}


*identify hospitals penalized for HK
use /ifs/home/kimk13/VI/data/hrrp_penalty, clear
keep if fy==2015
sum err_hk , de
gen HKpnlty = 1 if err_hk < 1
sum err_hk if err_hk >= 1, de
loc pp25 = r(p25)
loc pp50 = r(p25)
loc pp75 = r(p75)
replace HKpnlty = 2 if err_hk >= 1 & err_hk < `pp75'
*replace HKpnlty = 3 if err_hk >= `pp25' & err_hk < `pp75'
*replace HKpnlty = 4 if err_hk >= `pp50' & err_hk < `pp75'
replace HKpnlty = 3 if err_hk >= `pp75'
assert HKpnlty!=.

*lab define status 1 "Not penalized for HK" 2 "Penalized for HK & bottom quartile" 3 "Penalized for HK & middle 2 quartiles" 4 "Penalized for HK & top quartile"
lab define status 1 "Not penalized for HK" 2 "Penalized for HK & bottom 3 quartiles" 3 "Penalized for HK & top quartile", replace
lab val HKpnlty status
tab HKpnlty

keep provid HKpnlty err_hk
duplicates drop
destring provid, replace
tempfile hosp_pnlt
save `hosp_pnlt'


loc p SNF
use ``p'_refhhi', clear
*scatterplot of referral HHI in 2012 vs 2013 by condition across FY
*reshape wide first
reshape wide refhhi gr_refhhi, i(condition provid) j(fy)

forval y=2011/2015 {
  lab var refhhi`y' "Referral HHI in `y'"
}

*merge with 2015 hospital penalty status
merge m:1 provid using `hosp_pnlt', nogen keep(3)


sc refhhi2013 refhhi2012 || sc refhhi2012 refhhi2012, title("`p' Referral HHI in 2013 vs `p' Referral HHI in 2012")
graph export `gph'/`p'_sc_refhhi_12_13.eps, replace

sc refhhi2013 refhhi2011 || sc refhhi2011 refhhi2011, title("`p' Referral HHI in 2013 vs `p' Referral HHI in 2011")
graph export `gph'/`p'_sc_refhhi_11_13.eps, replace

loc c1 "black"
loc c2 "dkgreen"
loc c3 "red"

/*
g c= HKpnlty*255
qui levels of c, loc(cs)

su err_hk, meanonly
g c=round((err_hk-r(min))/(r(max)-r(min))*255)
qui levels of c, loc(cs) */

/* loc g
loc x = 0
foreach c of loc cs {
loc x = `x' + 1
loc g `"`g' || sc refhhi2013 refhhi2012 if HKpnlty==`x', ms(S) msize(vsmall) mc("`c' 100 100")"'
} */

loc g
forval x=1/3 {
  loc g `"`g' || sc refhhi2013 refhhi2012 if HKpnlty==`x', ms(S) msize(vsmall) mc(`c`x'')"'
}
tw `g' leg(order(1 "Not penalized for HK" 2 "Penalized for HK & bottom 3 quartiles" 3 "Penalized for HK & top quartile") col(1))
graph export `gph'/`p'_sc_refhhi_12_13_bypnlt.eps, replace

*contour plot takes long
/* tw contour err_hk refhhi2013 refhhi2012
graph export `gph'/test2.eps, replace */
