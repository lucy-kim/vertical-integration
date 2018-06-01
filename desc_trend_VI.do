*descriptive analysis of the trend in vertical integration over time

set matsize 11000
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use ivpenalty_VI_agg3c, clear
sum qual_read
replace qual_read = qual_read -`r(min)'
assert qual_read >=0

*restrict to sample used in regression (N=20,312)

loc int 2011
*sppX2010
loc pnltprs1 sppXpost`int'
loc pnltprs2 sppXpost`int' sppX2010 sppXpost11trend
loc pnltprs3 sppX20*

loc comorbid_hosp index_dual metacancer_ct-hipfracture_ct
*for now use hosp-comorbidities
*loc comorbid_snf snf_dual metacancer_ct_snf-hipfracture_ct_snf
loc reform EHRstage1 EHRstage2 vbp_adjf HACstatus
loc sp_hosp _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr black white `comorbid_hosp'
loc sp_snf _Ify_* own_* urban teaching _Isize* `reform' lnnsnf_mkt_samehrr black white `comorbid_snf'

loc yv read30
loc n 1
xtreg `yv' `pnltprs`n'' `sp_hosp' vi_snf_l, cluster(provid) fe

gen insmpl = e(sample)
assert insmpl==1
drop insmp

*keep provid fy read30 read30_pac refhhi shref_bytopSNF lnreqSNF_80pct den_nsnf_used shref vi_snf vi_snf2008

bys fy: egen p25 = pctile(spp), p(25)
bys fy: egen p75 = pctile(spp), p(75)

keep if spp <= p25 | spp >= p75
gen high = spp >= p75

loc outcome shref comorbidsum vi_snf refhhi qual_read
loc k = 0
foreach v of varlist `outcome' {
  loc k = `k' +1
  loc yv`k' `v'
}

preserve
*read30 read30_pac  refhhi shref_bytopSNF lnreqSNF_80pct
collapse (mean) shref, by(fy high)
tempfile tmp
save `tmp'
restore

preserve
keep if bad==0
collapse (mean) comorbidsum refhhi qual_read, by(fy high)
tempfile tmp2
save `tmp2'
restore

preserve
keep if vi_snf2008==0 & bad==0
collapse (mean) vi_snf , by(fy high)

merge 1:1 fy high using `tmp', nogen
merge 1:1 fy high using `tmp2', nogen

order fy high `outcome'
sum `outcome'
des
renvars shref-qual_read \ v1-v`k'
reshape long v, i(fy high) j(gp)

gen y = ""
forval x=1/`k' {
  replace y = "`yv`x''" if gp==`x'
}
drop gp
rename y gp
assert gp!=""
rename v mean

compress
outsheet using `reg'/trend_byquartile.csv, comma names replace
restore

*--------------------------
*descriptive stats
use ivpenalty_VI_agg3c, clear
sum qual_read
replace qual_read = qual_read -`r(min)'
assert qual_read >=0

sum spp pnltr2013, de
*keep provid fy read30 read30_pac refhhi shref_bytopSNF lnreqSNF_80pct den_nsnf_used shref vi_snf vi_snf2008

capture drop comorbidsum_admit
egen comorbidsum_admit = rowtotal(metacancer_ct-hipfracture_ct)
lab var comorbidsum_admit "Sum of comorbidity shares among all patients"

loc comorbid comorbidsum_admit
loc reform EHRstage1 EHRstage2 vbp_adjf HACstatus

gen small = size==1
gen medium = size==2
gen big = size==3
lab var small "Beds <= 100"
lab var medium "Beds 101-300"
lab var big "Beds 301+"

*mean characteristics for each group of hospitals pre & post-penalty
loc outcome read30 read30_pac shref comorbidsum vi_snf refhhi qual_read
loc otherchars small medium big own_* teaching urban black white black_pac white_pac index_dual_rate snf_dual_rate comorbidsum_admit nsnf_samehrr
*comorbidity?

lab var spp "Penalty pressure"
lab var read30 "30-day readmission rate"
lab var read30_pac "Readmission rate after referred to SNF"
lab var shref "Probability of referring to SNF"
lab var vi_snf "Probability of acquiring SNF"
lab var refhhi "SNF referral HHI"
lab var shref_bytopSNF "Share of referrals by top referred SNF"
lab var den_nsnf_used "Num. SNFs referred / num. referrals"
lab var own_np "Not-for-profit"
lab var own_fp "For-profit"
lab var own_gv "Government owned"
lab var urban "Urban"
lab var teaching "Teaching"
lab var black "Share of black patients"
lab var white "Share of white patients"
lab var black_pac "Share of black among SNF referred patients"
lab var white_pac "Share of white among SNF referred patients"
lab var EHRstage1 "Adopted meaning use of EHR stage 1"
lab var EHRstage2 "Adopted meaning use of EHR stage 2"
lab var vbp_adjf "VBP payment adjustment factor"
lab var HACstatus "Penalized for hospital-acquired condition reduction program"
lab var lnnsnf_mkt_samehrr "Log number of SNFs in the same HRR"
lab var index_dual_rate "Share of dual-eligible discharges"
lab var snf_dual_rate "Share of dual-eligible SNF referrals"

* all hospitals
sum spp pnltr2013, de
bys fy: sum `reform'

sum EHRstage1 if fy > 2010
sum EHRstage2 if fy > 2013
sum vbp_adjf if fy > 2012
sum HACstatus if fy > 2014

preserve
loc outcome read30_pac read30_other shref
keep spp pnltr2013 `outcome' `otherchars' `reform'
order spp pnltr2013 `outcome' `otherchars' `reform'

outreg2 using `reg'/summstats_all1.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep if bad==0
loc outcome comorbidsum
keep `outcome'
order `outcome'

outreg2 using `reg'/summstats_all2.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep if vi_snf2008==0 & bad==0
loc outcome vi_snf
keep `outcome'
order `outcome'

outreg2 using `reg'/summstats_all3.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep if bad==0
loc outcome refhhi qual_read
keep `outcome'
order `outcome'

outreg2 using `reg'/summstats_all4.xls, replace sum(log) eqkeep(N mean) label
restore

*------------------
*by low and high penalty pressure & by pre & post-HRRP
/*
bys fy: egen p25 = pctile(spp), p(25)
bys fy: egen p75 = pctile(spp), p(75)

keep if spp <= p25 | spp >= p75
gen high = spp >= p75

gen gp = "high" if spp >= p75
replace gp = "medium" if spp > p25 & spp < p75
replace gp = "small" if spp <= p25
assert gp!="" */

preserve
keep if fy < 2011
keep spp `outcome' `otherchars' `reform' gp
order spp `outcome' `otherchars' `reform' gp

bys gp: outreg2 using `reg'/summstats_pre.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep if fy >= 2011
keep spp `outcome' `otherchars' `reform' gp
order spp `outcome' `otherchars' `reform' gp

bys gp: outreg2 using `reg'/summstats_post.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep spp `outcome' `otherchars' `reform' gp
order spp `outcome' `otherchars' `reform' gp

outreg2 using `reg'/summstats_all.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep if vi_snf2008==0
keep spp `outcome' `otherchars' `reform' gp
order spp `outcome' `otherchars' `reform' gp

outreg2 using `reg'/summstats_noVI08.xls, replace sum(log) eqkeep(N mean) label
restore

*-------------------------
corr spp pnltr2013
sum spp pnltr2013

bys fy: egen p33 = pctile(spp), p(33)
bys fy: egen p66 = pctile(spp), p(66)
sum p33 p66
*p33 = 0.12%, p66 = .32%

sum read30*
sum vi_snf if vi_snf2008==0
