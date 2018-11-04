*descriptive analysis of the trend in vertical integration over time

set matsize 11000
loc reg "~/Dropbox/Research/sunita-lucy/Phoenix/VI/output"
loc dta "~/Dropbox/Research/sunita-lucy/Phoenix/VI/data"

cd `dta'/Medicare

*-------------------------------------

*total # discharges, referrals
use ivpenalty_VI_agg3c_nosw, clear
egen tot = sum(dischnum)
egen tot2 = sum(dischnum_pac)
sum tot*
*-------------------------------------
* relationship between expected and actual penalty rates
use ivpenalty_VI_agg3c_nosw, clear
keep if fy==2011
corr spp pnltr2013
gen diff = pnltr2013-spp

sum spp pnltr2013 diff
*-------------------------------------
*distribution of HHI
use ivpenalty_VI_agg3c_nosw, clear
keep if bad==0
hist refhhi3, frac
graph export `reg'/hist_refhhi3.eps, replace

lab var nsnf_used "Number of SNFs referred"
sc refhhi nsnf_used
graph export `reg'/sc_refhhi3_nsnf_used.eps, replace

*-------------------------------------
*scatterplot of expected penalty rate vs medicare share
use ivpenalty_VI_agg3c_nosw, clear

sc mcre_bite spp if fy==2011
graph export `reg'/sc_mcrebite_spp.eps, replace

qqplot mcre_bite spp if fy==2011, xti(Expected penalty rate in the first year of HRRP) yti(Share of Medicare discharges in 2010) graphregion(color(white)) bgcolor(white)
*
graph export `reg'/qq_mcrebite_spp.eps, replace

qqplot pbite spp if fy==2011, xti(Expected penalty rate in the first year of HRRP) yti() subti(Penalty bite = Expected penatly rate X 2010 Medicare discharge share) graphregion(color(white)) bgcolor(white)
*
graph export `reg'/qq_pbite_spp.eps, replace

*did large urban hospitals have higher spp and lower medicare share?
bys size: sum spp pnltr2013 mcre_bite if urban==1

*-------------------------------------
*stability of penalty pressure

use ivpenalty_VI_agg3c_nosw, clear


gen mcre_bite_t = mcre_dischrg/dischrg
assert mcre_bite_t>=0 & mcre_bite_t <=1
tab fy, summarize(mcre_bite_t)

gen pnltr = .
forval yy = 2013/2018 {
  loc y1 = `yy'-2
  replace pnltr = pnltr`yy' if fy==`y1'
}

gen pbite_actual_t = pnltr*

*for each year 2011-2016, get quartiles of penalty pressure

I think we want stability in relative penalty pressure across all groups, not just the top quartile.

What is the % of hospitals in each quartile that were in the same quartile in the subsequent year? I think that might be  a more compelling statistic

Also, penalty pressure rather than penalty rate may be more relevant.


*-------------------------------------

*plot trend in outcomes by quartile
use ivpenalty_VI_agg3c_nosw, clear
sum qual_read

*restrict to sample used in regression (N=20,312)
loc int 2011
*sppX2010
loc pnltprs1 sppXpost`int'
loc pnltprs2 sppXpost`int' sppX2010 sppXpost11trend
loc pnltprs3 sppX20*

loc comorbid_hosp index_dual metacancer_ct-hipfracture_ct
*for now use hosp-comorbidities
*loc comorbid_snf snf_dual metacancer_ct_snf-hipfracture_ct_snf
loc reform EHRstage1 EHRstage2 HACstatus bpci vbp_adjf
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

loc outcome read30_pac read30_other shref comorbidsum vi_snf refhhi shref_bytopSNF reqSNF_80pct qual_read
loc k = 0
foreach v of varlist `outcome' {
  loc k = `k' +1
  loc yv`k' `v'
}

preserve
*read30 read30_pac  refhhi shref_bytopSNF lnreqSNF_80pct
collapse (mean) read30_other shref, by(fy high)
tempfile tmp
save `tmp'
restore

preserve
keep if bad==0
collapse (mean) read30_pac comorbidsum refhhi shref_bytopSNF reqSNF_80pct qual_read, by(fy high)
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
renvars read30_pac-qual_read \ v1-v`k'
reshape long v, i(fy high) j(gp)

gen y = ""
forval x=1/`k' {
  replace y = "`yv`x''" if gp==`x'
}
drop gp
rename y gp
assert gp!=""
rename v mean

tab gp, summarize(mean)

foreach v in "shref" "shref_bytopSNF" {
    replace mean = 100*mean if gp=="`v'"
}

compress
outsheet using `reg'/trend_byquartile.csv, comma names replace
restore

*--------------------------
*descriptive stats
use ivpenalty_VI_agg3c_nosw, clear

gen small = size==1
gen medium = size==2
gen big = size==3
lab var small "Beds <= 100"
lab var medium "Beds 101-300"
lab var big "Beds 301+"
lab var spp "Penalty pressure"
lab var read30 "30-day readmission rate"
lab var read30_pac "Readmission rate after referred to SNF"
lab var shref "Probability of referring to SNF"
lab var vi_snf "Probability of acquiring SNF"
lab var refhhi "SNF referral HHI"
lab var shref_bytopSNF "Share of referrals by top referred SNF"
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
lab var HACstatus "Penalized for hospital-acquired condition reduction program"
lab var lnnsnf_mkt_samehrr "Log number of SNFs in the same HRR"
lab var index_dual_rate "Share of dual-eligible discharges"
lab var snf_dual_rate "Share of dual-eligible SNF referrals"

loc reform EHRstage1 EHRstage2 HACstatus bpci vbp_adjf
loc hospchar small medium big own_np own_fp own_gv teaching urban black_pac black_other white_pac white_other snf_dual_rate dual_other _65_74_pac_sh _65_74_other_sh _75_84_pac_sh _75_84_other_sh _85_94_pac_sh _85_94_other_sh _95p_pac_sh _95p_other_sh `reform'
loc hospchar2 dischnum dischnumAMI dischnumHF dischnumPN shref shrefAMI shrefHF shrefPN `hospchar' nsnf_samehrr
loc outcome shref comorbidsum vi_snf refhhi shref_bytopSNF reqSNF_80pct lnreqSNF_80pct qual_read read30_pac read30_other

foreach v of varlist mcre_bite shref shrefAMI shrefHF shrefPN shref_bytopSNF read30_pac read30_other `hospchar' {
  if "`v'"!="vbp_adjf" {
    replace `v' = `v'*100
    di "`v'"
    assert `v' >= 0 & `v' <= 100 if `v'!=.
  }
}

preserve
keep `hospchar2'
order `hospchar2'
outreg2 using `reg'/summstats_all1.xls, replace sum(log) eqkeep(N mean sd) label
restore

sum shref
sum comorbidsum if bad==0
sum vi_snf if vi_snf2008==0 & bad==0
sum refhhi shref_bytopSNF reqSNF_80pct lnreqSNF_80pct qual_read read30_pac if bad==0
sum read30_other
sum vbp_adjf if fy >= 2013

preserve
keep `outcome'
order `outcome'
outreg2 using `reg'/summstats_outc.xls, replace sum(log) eqkeep(N mean sd) label
restore

sum pbite spp mcre_bite pbite_actual pnltr2013 , de
*keep provid fy read30 read30_pac refhhi shref_bytopSNF lnreqSNF_80pct den_nsnf_used shref vi_snf vi_snf2008

*-------------------------
*by 25th and 75th percentile penalty pressure hospitals, change in the actual penalty rate during 2013-2016

use ivpenalty_VI_agg3c_nosw, clear

loc pp spp
bys fy: egen p25 = pctile(`pp'), p(25)
bys fy: egen p50 = pctile(`pp'), p(50)
bys fy: egen p75 = pctile(`pp'), p(75)

gen q1 = `pp' <= p25
gen q2 = `pp' > p25 & spp <= p50
gen q3 = `pp' > p50 & spp <= p75
gen q4 = `pp' > p75

/* forval yy = 2013/2013 {
  sum pnltr`yy', de
  loc p25 = `r(p25)'
  loc p50 = `r(p50)'
  loc p75 = `r(p75)'
  gen q1_`yy' = pnltr`yy' <=`p25'
  gen q2_`yy' = pnltr`yy' >`p25' & pnltr`yy' <= `p50'
  gen q3_`yy' = pnltr`yy' >`p50' & pnltr`yy' <= `p75'
  gen q4_`yy' = pnltr`yy' >`p75'
}

loc yy 2013
forval x = 1/4 {
  tab q`x' if q`x'_`yy'==1
} */

gen qtl = .
forval x = 1/4 {
  replace qtl = `x' if q`x'==1 & qtl==.
}
keep provid qtl pnltr201?
duplicates drop
reshape long pnltr, i(provid qtl) j(fy)

/* keep if spp <= p25 | spp >= p75
gen high = spp >= p75

keep provid high pnltr*
duplicates drop

reshape long pnltr, i(provid high) j(fy) */

compress
outsheet using `reg'/pnltr_byhigh.csv, comma names replace
*-------------------------




capture drop comorbidsum_admit
egen comorbidsum_admit = rowtotal(metacancer_ct-hipfracture_ct)
lab var comorbidsum_admit "Sum of comorbidity shares among all patients"


*get mean outcomes among bottom quartile (0.09)
*probability of referral for the patient-level analysis
preserve
keep if spp > 0.09-0.00001 & spp < 0.09 + 0.00001

foreach d in "AMI" "HF" "PN" "HK" {
  capture drop tot tot2
  capture drop sh`d'
  egen tot = sum(dischnum`d')
  egen tot2 = sum(dischnum_pac`d')
  gen sh`d' = tot2/tot
  sum sh`d'
}

*for hospital-level analysis
sum shref shrefAMI shrefHF shrefPN
sum comorbidsum if bad==0
sum vi_snf if vi_snf2008==0 & bad==0
sum closeSNF if vi_snf2008==1 & bad==0
sum refhhi shref_bytopSNF lnreqSNF_80pct dischnum dischnum_pac qual_read qual_samh qual_star qual_def read30_pac if bad==0
sum read30_other

restore


* all hospitals
sum spp pnltr2013, de
bys fy: sum `reform'

sum EHRstage1 if fy > 2010
sum EHRstage2 if fy > 2013
sum vbp_adjf if fy > 2012
sum HACstatus if fy > 2014
sum bpci if fy > 2013
sum vbp_adjf if fy >= 2013

*get condition-specific # discharges
preserve
use index_admit_chy, clear
drop if cond=="HK"
keep dischnum cond provid fy
reshape wide dischnum*, i(provid fy) j(cond) st

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
*desc stats by low and high penalty pressure & by pre & post-HRRP
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


preserve
ci pnltr
collapse (mean) pnltr, by(high fy)

tw (sc pnltr fy if high==1) (sc pnltr fy if high==0), leg(order(1 "High penalty pressure" 2 "Low penalty pressure") col(1)) xti(Year ending June) yti(Actual penalty rate (%)) graphregion(fcolor(white))
graph export `gph'/pnltr_byhigh.eps, replace

corr spp pnltr2013
sum spp pnltr2013

bys fy: egen p33 = pctile(spp), p(33)
bys fy: egen p66 = pctile(spp), p(66)
sum p33 p66
*p33 = 0.12%, p66 = .32%

sum read30*
sum vi_snf if vi_snf2008==0

*------------------
*during 2008, for the top quartile and bottom quartile for penalty pressure,
