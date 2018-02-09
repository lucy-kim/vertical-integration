*regression analysis of the penalty pressure on integration

set matsize 11000
loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

*drop hospitals that have mean # PAC referrals throughout the sample period is 0
use hosp_fy_VI, clear
duplicates tag pac cond provid fy, gen(dup)
tab dup
bys pac cond provid : egen mm = mean(dischnum_pac)
tab provid  if mm==0
list pac cond provid fy dischnum_pac if provid==670073

drop if mm==0
*drops hospitals for some conditions but not for other conditions
drop mm

*count distribution of hospital-condition level # discharges
tw hist dischnum_pac , discrete by(pac cond)

preserve
keep cond provid fy dischnum
duplicates drop
tab cond fy
tw hist dischnum , discrete by(cond) frac xtitle("Number of hospital discharges per hospital-FY by condition") width(30)
graph export `gph'/hist_dischnum_byc.eps, replace

collapse (mean) dischnum, by(cond provid)
tw hist dischnum , by(cond) frac xtitle("Mean number of hospital discharges per hospital by condition") width(30)
graph export `gph'/hist_dischnum_byc2.eps, replace

foreach c in "AMI" "HF" "PN" "HK" {
  tab dischnum if con=="`c'"
}

bys cond: outreg2 using `reg'/dischnum.xls, replace sum(detail) eqkeep(N mean sd min max p1 p5 p10 p25 p50 p75 p90 p95 p99)

tab cond if dischnum >= 25
keep if dischnum >= 25
keep cond provid
tempfile hosp_keep
save `hosp_keep'
restore

merge m:1 cond provid using `hosp_keep', keep(3) nogen

tempfile an
save `an'
*----------------------------------------
*construct the referral HHI lumping all 3 conditions: AMI, HF, PN
use `an', clear
keep provid fy
duplicates drop
tempfile hospkeep
save `hospkeep'

use PACreferral_tchpm, clear
drop if cond=="HK"
merge m:1 provid fy using `hospkeep', keep(3) nogen
collapse (sum) dischnum_pac, by(pac provid fy pacprovid)

* by condition-FY, level & growth rate of referral concentration among PAC providers
bys pac provid fy: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(pac provid fy)
assert refhhi <= 1
rename refhhi refhhi3

tempfile hosp_fy_ref
save `hosp_fy_ref'

use `hosp_fy_ref', clear
gen prov_num = string(provid, "%06.0f")
rename fy fyear
merge m:1 prov_num fyear using `dta'/costreport/hosp_chars_cr, keep(1 3)
rename fyear fy
drop vi_renal-vi_asc vi_ipf vi_swbsnf-vi_rhc vi_hospice vi_nf state

sort pac provid fy
list pac provid fy beds size if provid==520051
foreach v of varlist vi_snf-urban {
  bys pac provid: replace `v' = `v'[_n-1] if `v'>=.
}
gsort pac provid -fy
foreach v of varlist vi_snf-urban {
  bys pac provid: replace `v' = `v'[_n-1] if `v'>=.
}
tab provid if beds==.

loc lab1 Small
loc lab2 Medium
loc lab3 Large

capture drop grrefhhi3
sort pac provid fy
bys pac provid: gen grrefhhi3 = 100*(refhhi - refhhi[_n-1])/refhhi[_n-1]

foreach p in "HHA" "SNF" {
  binscatter grrefhhi fy if pac=="`p'" , discrete linetype(qfit) title("Growth in `p' referral concentration for AMI, HF, PN altogether", size(medium)) xti(Year) yti(Growth in Referral HHI (%)) ysc(r(0 25))  ylab(0(5)25)
  graph export `gph'/refhhi3_`p'.eps, replace

  forval x = 1/3 {
    binscatter grrefhhi fy if pac=="`p'" & size==`x',  discrete linetype(qfit) xti(Year) yti(Growth in Referral HHI (%)) ysc(r(0 25))  ylab(0(5)25) subti("`lab`x'' hospitals") saving(g`x', replace)
  }
  graph combine g1.gph g2.gph g3.gph, title("Growth in `p' referral concentration for AMI, HF, PN altogether", size(medium))
  graph export `gph'/refhhi3_`p'_size.eps, replace
}

*plot growth in referral HHI for H/K
use `an', clear
keep if cond=="HK"

capture drop grrefhhi3
sort pac provid fy
bys pac provid: gen grrefhhi3 = 100*(refhhi - refhhi[_n-1])/refhhi[_n-1]

foreach p in "HHA" "SNF" {
  binscatter grrefhhi fy if pac=="`p'" , discrete linetype(qfit) title("Growth in `p' referral concentration for HK", size(medium)) xti(Year) yti(Growth in Referral HHI (%)) ysc(r(0 25))  ylab(0(5)25)
  graph export `gph'/refhhi3_`p'_hk.eps, replace

  forval x = 1/3 {
    binscatter grrefhhi fy if pac=="`p'" & size==`x',  discrete linetype(qfit) xti(Year) yti(Growth in Referral HHI (%)) ysc(r(0 25))  ylab(0(5)25) subti("`lab`x'' hospitals") saving(g`x', replace)
  }
  graph combine g1.gph g2.gph g3.gph, title("Growth in `p' referral concentration for HK", size(medium))
  graph export `gph'/refhhi3_`p'_size_hk.eps, replace
}
*----------------------------------------
*using 2011-2013, construct the penalty pressure = ERR X condition-specific Medicare payment / total patient revenue
use `an', clear
keep if fy < 2014 & cond=="HK"
sort cond provid fy pac
keep provid fy tot_pat_rev tmcr_pmt
duplicates drop
collapse (mean) tot_pat_rev tmcr_pmt, by(provid)
gen bite = 100*tmcr_pmt / tot_pat_rev
tw hist bite, frac xti(% mean total Medicare payment for H/K out of total patient revenue) ti(Relative importance of Medicare revenue for H/K for the hospital, size(medium))
graph export `gph'/bite.eps, replace
tempfile bite
save `bite'


use `dta'/hrrp_penalty, clear
keep if fy==2015
sum err_hk, de
drop fy
sum n_hk if err_hk==0
keep provid pnltr err_hk n_hk
merge 1:1 provid using `bite', keep(3) nogen

binscatter err_hk bite, ti("Binscatterplot of H/K excess readmission ratio" "vs Relative importance of H/K revenue", size(medium)) xti(% mean total Medicare payment for H/K out of total patient revenue) yti(H/K excess readmission ratio) yscale(titlegap(*20))
graph export `gph'/err_hk_bite.eps, replace

gen pnltprs =  err_hk * bite

drop n_hk

foreach v of varlist pnltprs err_hk bite {
  egen p66_`v' = pctile(`v'), p(66)
  gen top3rd_`v' = `v' > p66_`v'
  replace top3rd_`v' = . if `v'==.
}

tw hist pnltprs, frac xti([H/K ERR] X [% H/K payment out of total patient revenue], size(medium)) ti(Readmissions penalty pressure for H/K) xline(.43) note(Note: 66th percentile among hospitals = 0.43) bfcolor(none)
graph export `gph'/pnltprs.eps, replace

tempfile pnltprs
save `pnltprs'

use `an', clear
merge m:1 provid using `pnltprs', keep(1 3)
tab cond _m
drop _m
*nearly all matched for H/K , makes sense b/c i only focused on H/K hospitals above

gen hk = cond=="HK"
xi i.fy

foreach v in "pnltprs" "err_hk" "bite" {
  foreach vv of varlist _Ify* {
    gen top_`v'_X_`vv'_hk= top3rd_`v'*`vv'*hk
  }
  gen top_`v'_X_hk= top3rd_`v'*hk
}
foreach vv of varlist _Ify* {
  gen `vv'_X_hk = `vv'*hk
}

sort provid fy
bys provid: gen refhhigr = 100*(refhhi - refhhi[_n-1]) / refhhi[_n-1]

tempfile an2
save `an2'

foreach p in "HHA" "SNF" {
  foreach y of varlist refhhi refhhigr {
    binscatter `y' fy if pac=="`p'" & cond=="HK", absorb(provid)
    graph export `gph'/bs_`p'_`y'.eps, replace
  }
}

foreach p in "HHA" "SNF" {
  foreach y of varlist refhhi refhhigr {
    use `an2', clear
    keep if pac=="`p'"

    loc v pnltprs
    loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_HK
    areg `y' top_`v'_X_* _Ify*_hk hk i.fy i.size, absorb(provid) cluster(provid)

    *save coefficients to plot
    tempfile `p'_`y'
    parmest, saving(``p'_`y'')
  }
}

foreach p in "HHA" "SNF" {
  foreach y in "refhhi" "refhhigr" {
    use ``p'_`y'', clear
    list parm estimate t p, noobs clean
    keep if regexm(parm, "top") | regexm(parm, "_Ify_")
    drop if parm=="top_pnltprs_X_hk"

    gen x = "DDDest" if regexm(parm, "top")
    replace x = "lowpenalty" if !regexm(parm, "top")

    split parm, p("Ify_")
    replace parm2 = subinstr(parm2, "_hk", "",.)
    replace parm2 = subinstr(parm2, "_X", "",.)
    drop if estimate==0
    rename parm2 fy
    rename x est

    keep estimate min95 max95 fy est
    rename estimate coef
    gen pac = "`p'"
    gen y = "`y'"
    tempfile `p'_`y'2
    save ``p'_`y'2'
  }
}

clear
foreach p in "HHA" "SNF" {
  foreach y in "refhhi" "refhhigr" {
    append using ``p'_`y'2'
  }
}

compress
outsheet using `reg'/coef_DiD_fy_pnltprs_hk.csv, replace comma names


*focus on other conditions
use `an', clear
keep if fy ==2011 & cond!="HK"
sort cond provid fy pac
keep cond provid tot_pat_rev tmcr_pmt
duplicates drop
gen bite = 100*tmcr_pmt / tot_pat_rev
tw hist bite, frac xti(% mean total Medicare payment out of total patient revenue) ti(Relative importance of Medicare revenue for the hospital, size(medium)) by(cond)
graph export `gph'/bite.eps, replace

reshape wide tot_pat_rev tmcr_pmt bite , i(provid) j(cond) string
tempfile bite2
save `bite2'

use `dta'/hrrp_penalty, clear
keep if fy==2013
sum err_ami, de
drop fy
sum n_hk if err_hk==0
keep provid pnltr err_ami err_hf err_pn
merge 1:1 provid using `bite2', keep(3) nogen

loc c PN
loc lowAMI "ami"
loc lowHF "hf"
loc lowPN "pn"
foreach c in "AMI" "HF" "PN" {
  binscatter err_`low`c'' bite`c', ti("Binscatterplot of `c' excess readmission ratio" "vs Relative importance of `c' revenue", size(medium)) xti(% mean total Medicare payment for `c' out of total patient revenue) yti(`c' excess readmission ratio) yscale(titlegap(*20))
  graph export `gph'/err_`c'_bite.eps, replace

  gen pnltprs_`c' =  err_`low`c'' * bite`c'
}

foreach v of varlist pnltprs* {
  egen p66_`v' = pctile(`v'), p(66)
  gen top3rd_`v' = `v' > p66_`v'
  replace top3rd_`v' = . if `v'==.
}
/*
tw hist pnltprs, frac xti([H/K ERR] X [% H/K payment out of total patient revenue], size(medium)) ti(Readmissions penalty pressure for H/K) xline(.43) note(Note: 66th percentile among hospitals = 0.43) bfcolor(none)
graph export `gph'/pnltprs.eps, replace */

tempfile pnltprs
save `pnltprs'


use `an', clear
merge m:1 provid using `pnltprs', keep(1 3)
tab cond _m
drop _m
*nearly all matched for H/K , makes sense b/c i only focused on H/K hospitals above

xi i.fy

foreach c in "AMI" "HF" "PN" {
  loc v pnltprs_`c'

  foreach vv of varlist _Ify* {
    gen top_`v'_X_`vv'= top3rd_`v'*`vv'
  }
}

sort provid fy
bys provid: gen refhhigr = 100*(refhhi - refhhi[_n-1]) / refhhi[_n-1]

tempfile an3
save `an3'

foreach c in "AMI" "HF" "PN" {
  foreach p in "HHA" "SNF" {
    foreach y of varlist refhhi refhhigr {
      use `an3', clear
      keep if pac=="`p'" & cond=="`c'"

      loc v pnltprs_`c'
      loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_HK
      areg `y' top_`v'_X_* i.fy i.size, absorb(provid) cluster(provid)

      *save coefficients to plot
      tempfile `p'_`y'_`c'
      parmest, saving(``p'_`y'_`c'')
    }
  }
}

foreach c in "AMI" "HF" "PN" {
  foreach p in "HHA" "SNF" {
    foreach y in "refhhi" "refhhigr" {
      use ``p'_`y'_`c'', clear
      list parm estimate t p, noobs clean
      keep if regexm(parm, "top")

      split parm, p("Ify_")
      drop if estimate==0
      rename parm2 fy

      keep estimate min95 max95 fy
      rename estimate coef
      gen pac = "`p'"
      gen y = "`y'"
      gen cond = "`c'"
      tempfile `p'_`y'_`c'2
      save ``p'_`y'_`c'2'
    }
  }
}

clear
foreach c in "AMI" "HF" "PN" {
  foreach p in "HHA" "SNF" {
    foreach y in "refhhi" "refhhigr" {
      append using ``p'_`y'_`c'2'
    }
  }
}
compress
outsheet using `reg'/coef_DiD_fy_pnltprs_other.csv, replace comma names




binscatter refhhi fy if pac=="SNF" & cond=="HK", rd(2013.5) absorb(provid) by(top3rd_pnltprs)
graph export `gph'/bs_refhhi_fy_HK_snf.eps, replace

binscatter refhhi fy if pac=="HHA" & cond=="HK", rd(2013.5) absorb(provid) by(top3rd_pnltprs)
graph export `gph'/bs_refhhi_fy_HK_hha.eps, replace




*------------
*count the rate of readmissions, non-white population, black pop, ses_score in the first year in the data (2008)
use index_admit_chm, clear
keep if dischyear==2008
collapse (sum) dischnum white black read* (mean) ses_score, by(condition provid)

*create rate of readmissions, non-white population, black pop
foreach v of varlist white black read* {
  gen r`v' = `v'/dischnum
}
gen rnonwhite = 1-rwhite

*90-day readmission should be the sum of the 3 variables
gen rreadmit90 = rread30 + rread60 + rread90

*for each condition, create indicator for whether the hospital is in the top third of the distribution
foreach v of varlist rnonwhite rblack rread30 ses_score {
  bys condition: egen p66_`v' = pctile(`v'), p(66)
  gen top3rd_`v' = `v' > p66_`v'
}

foreach v of varlist rnonwhite rblack rread30 ses_score {
  bys cond top3rd_`v': sum `v'
}

keep provid cond p66* top3rd* rnonwhite rblack rread30 ses_score

compress
save top3rd_iv_hosp2008, replace

*------------------------------------
*are you more likely to be penalized because of the 3 variables?
use `dta'/hrrp_penalty, clear
destring provid, replace
reshape long n_ err_ , i(provid payadjr fy pnltr) j(condition) string
replace cond = upper(cond)
merge m:1 provid cond using top3rd_iv_hosp2008, keep(3) nogen

compress
outsheet using `dta'/penaltystatus_iv.csv, replace comma names

tw (sc err rblack if cond=="AMI" & err!=0), by(fy)
graph export `gph'/test.eps, replace

*------------------------------------
*quarterly level
use referral_hq, clear
merge m:1 provid cond using top3rd_iv_hosp2008, keep(1 3) nogen
list cond pac provid dischyear qtr dischnum read30* in 1/30

*30-day readmission rate among patients referred to PAC
gen rread30_pac = read30_pac / dischnum_pac
rename rread30_pac rread30p

gen yqd = yq(dischyear, qtr)
format yqd %tq

xi i.yqd

foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {
  foreach vv of varlist _Iyqd* {
    gen top_`v'_X_`vv'= top3rd_`v'*`vv'
  }
}

rename refhhi_hq refhhi
tempfile an
save `an'

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use `an', clear
    keep if pac=="`p'" & cond=="`c'" & dischnum_pac!=1

    foreach y of varlist rread30p refhhi {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {

        loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
        areg `y' top_`v'_X_* i.qtr i.yqd , absorb(provid) cluster(provid)

        *save coefficients to plot
        tempfile `y'`p'`c'`v'
        parmest, saving(``y'`p'`c'`v'')
      }
    }
  }
}

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    foreach y in "rread30p" "refhhi" {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {
        use ``y'`p'`c'`v'', clear
        list parm estimate t p, noobs clean
        keep if regexm(parm, "top")

        split parm, p("Iyqd_")
        drop if estimate==0

        gen a = mod(_n+3, 4)
        replace a = 4 if a==0
        rename a qtr
        gen yr = 2011 + floor((_n-2)/4)

        keep estimate min95 max95 yr qtr
        rename estimate coef
        gen cond = "`c'"
        gen pac = "`p'"
        gen y = "`y'"
        gen iv = "`v'"

        tempfile `y'`p'`c'`v'2
        save ``y'`p'`c'`v'2'
      }
    }
  }
}

clear
foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    foreach y in "rread30p" "refhhi" {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {
        append using ``y'`p'`c'`v'2'
      }
    }
  }
}
compress
outsheet using `reg'/coef_DiD_qtr_iv.csv, replace comma names

*-----------------------------
*year level
use referral_hq, clear
merge m:1 provid cond using top3rd_iv_hosp2008, keep(1 3) nogen
list cond pac provid dischyear qtr dischnum read30* in 1/30

*30-day readmission rate among patients referred to PAC
gen rread30p = read30_pac / dischnum_pac
gen rread30n = 1-rread30p

xi i.fy

foreach v in "rblack" {
  foreach vv of varlist _Ify* {
    gen top_`v'_X_`vv'= top3rd_`v'*`vv'
  }
}

rename refhhi_hq refhhi
tempfile an
save `an'

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    use `an', clear
    keep if pac=="`p'" & cond=="`c'" & dischnum_pac!=1

    foreach y of varlist rread30p refhhi {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {

        loc x_ht own_fp own_np own_gv i.size ses_score teaching urban hrrhhi_`p'_`c'
        areg `y' top_`v'_X_* i.qtr i.yqd `x_ht', absorb(provid) cluster(provid)

        *save coefficients to plot
        tempfile `y'`p'`c'`v'
        parmest, saving(``y'`p'`c'`v'')
      }
    }
  }
}

foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    foreach y in "rread30p" "refhhi" {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {
        use ``y'`p'`c'`v'', clear
        list parm estimate t p, noobs clean
        keep if regexm(parm, "top")

        split parm, p("Iyqd_")
        drop if estimate==0

        gen a = mod(_n+3, 4)
        replace a = 4 if a==0
        rename a qtr
        gen yr = 2011 + floor((_n-2)/4)

        keep estimate min95 max95 yr qtr
        rename estimate coef
        gen cond = "`c'"
        gen pac = "`p'"
        gen y = "`y'"
        gen iv = "`v'"

        tempfile `y'`p'`c'`v'2
        save ``y'`p'`c'`v'2'
      }
    }
  }
}

clear
foreach p in "HHA" "SNF" {
  foreach c in "AMI" "HF" "PN" "HK" {
    foreach y in "rread30p" "refhhi" {
      foreach v in "rnonwhite" "rblack" "rreadmit90" "ses_score" {
        append using ``y'`p'`c'`v'2'
      }
    }
  }
}
compress
outsheet using `reg'/coef_DiD_qtr_iv.csv, replace comma names




loc y refhhi_hq
loc p "HHA"
loc c "AMI"
use ``y'`p'`c'rate_readmit90', clear




areg refhhi_hq if pac=="`p'" & cond=="`c'", absorb(provid) cluster(provid)
}
}

ivreg2
