*create hospital-FY level data containing measures of integration & hospital characteristics

loc gph /ifs/home/kimk13/VI/output
loc reg /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data
loc int 2011

cd `dta'/Medicare

*cap the penalty rate to max level
use predict_pnltprs_agg3c, clear

gen post`int' = fy >=`int'

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



*-----------------------
*add participation in other value-based reforms
*-----------------------

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
