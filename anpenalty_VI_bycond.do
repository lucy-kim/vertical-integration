*Examine whether condition-specific penalty pressure changes referral concentration

*packages for local linear regression
net install ivqte, from("https://sites.google.com/site/blaisemelly/")
net install st0026_2, from(http://www.stata-journal.com/software/sj5-3)
ssc install moremata

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use predict_pnltprs, clear

rename pred_pnltstatus_t2_err ppst
rename pred_pnltrate_t2_err ppr
rename pred_pnltdollar_t2_err ppd

keep if matched==1

table fy if cond=="AMI", content(mean penalized_t2 mean pnltrate_t2 mean pnltdollar_t2)
table fy if cond=="AMI", content(mean ppst mean ppr mean ppd)

*for pre-years, recode predicted penalty rate to 0
loc int 2012
foreach v of varlist ppst ppr ppd {
  replace `v' = 0 if fy <  & `v'==.
}
table fy if cond=="AMI", content(mean ppst mean ppr mean ppd)

replace ppd = ppd*100000

foreach v of varlist ppd {
  gen l`v' = ln(`v' + 1)
}

*use the predicted penalty likelihood, rate, dollar amounts as key indep vars in the regression

* 1) cross-sectional variation: use the 2012 readmissions penalty pressure based on 2009-2011 performance
foreach v of varlist ppst ppr lppd  {
  capture drop x
  gen x = `v' if fy==`int'
  bys cond provid: egen `v'`int' = max(x)
}

*char fy[omit] 2016
xi i.fy
gen post = fy >=`int'

*interaction of penalty pressure and post period indicators
local lppst`int' "Predicted likelihood of penalty in `int'"
local lppr`int' "Predicted penalty rate in `int'"
local llppd`int' "Log Predicted penalty amount ($) in `int'"

foreach v of varlist ppst`int' ppr`int' lppd`int' {
  tab fy, summarize(`v')

  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  lab var `v'Xpost "`l`v'' X Post"

  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
    lab var `v'X`t' "`l`v'' X `t'"
  }
}

lab var ppst "Predicted likelihood of penalty"
lab var ppr "Predicted penalty rate"
lab var ppd "Predicted penalty amount ($)"
lab var lppd "Log Predicted penalty amount ($)"

lab var ppst`int' "Predicted likelihood of penalty in `int'"
lab var ppr`int' "Predicted penalty rate in `int'"
lab var lppd`int' "Log Predicted penalty amount ($) in `int'"

* create share of SNF referrals outcome
gen shref = dischnum_pac/dischnum

*create quintiles of # beds
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

gen hrrhhi_SNF = .
foreach c in "AMI" "HF" "PN" {
  replace hrrhhi_SNF = hrrhhi_SNF_`c' if cond=="`c'"
  drop hrrhhi_SNF_`c'
}

tempfile tmp
save `tmp'

*get indicators for top quartlie & middle 2 quartiles
use `tmp', clear
keep cond provid ppst`int' ppr`int' lppd`int'
duplicates drop
foreach pp of varlist ppst`int' ppr`int' lppd`int' {
  bys cond: egen x_p75_`pp' = pctile(`pp'), p(75)
  bys cond: egen x_p25_`pp' = pctile(`pp'), p(25)
  gen p75_`pp' = `pp' > x_p75_`pp'
  gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
}
drop x_* ppst ppr lppd
tempfile quartiles
save `quartiles'

use `tmp', clear
merge m:1 cond provid using `quartiles', keep(1 3) nogen

*create indicators for the top quartile, middle 2 quatiles in the penalty pressure
foreach pp of varlist ppst`int' ppr`int' lppd`int' {
  forval t=2010/2016 {
    gen p75_`pp'X`t' = p75_`pp' * _Ify_`t'
    lab var p75_`pp'X`t' "Top quartile in `l`pp'' X `t'"
    gen p25_75_`pp'X`t' = p25_75_`pp' * _Ify_`t'
    lab var p25_75_`pp'X`t' "Middle 2 quartiles in `l`pp'' X `t'"
  }
  gen p75_`pp'Xpost = p75_`pp' * post
  gen p25_75_`pp'Xpost = p25_75_`pp' * post
  lab var p75_`pp'Xpost "Top quartile X Post"
  lab var p25_75_`pp'Xpost "Middle 2 quartiles X Post"
}
des p75* p25_75*

tab fy, summarize(p75_ppst`int'Xpost)

*create laggeed formally owning SNF
preserve
use hosp_fy_VI, clear
keep if pac=="SNF"
sort cond provid fy
bys cond provid: gen vi_snf_l = vi_snf[_n-1]
tab fy, summarize(vi_snf_l)
lab var vi_snf_l "Owns SNF, lagged"
keep cond provid fy vi_snf_l
duplicates drop
tempfile vi_snf_l
save `vi_snf_l'
restore

merge 1:1 cond provid fy using `vi_snf_l', keep(1 3) nogen
tab fy, summarize(vi_snf_l)

lab var vi_snf "Formally owns SNF"
lab var hrrhhi_SNF "SNF market concentration"
lab var urban "Urban"
lab var teaching "Teaching"
lab var own_fp "For profit"
lab var own_np "Not for profit"
lab var own_gv "Government owned"

tempfile an
save `an'

* regress the referral concentration on hospital FE, penalty pressure, fiscal year FE


*---------------------------
*use the cross-sectional variation across hospitals by  penalty pressure based on 2009-2011 performance

loc sp i.gp_beds vi_snf_l own_* urban teaching hrrhhi_SNF

foreach y of varlist refhhi refhhi_prevSNFs shsnf_used_ag shsnf_used rat_nsnf_used shref {
  * use post dummy
  loc file ols_`y'_cs_post
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    *xtile gp_dischnum=dischnum, n(5)

    di "Condition `c'------------------------"

    loc file ols_`y'_cs_post
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c' `l`pp'')"

    foreach pp of varlist ppst`int' ppr`int' lppd`int' {
      use `an', clear
      keep if cond=="`c'"
      table fy, contents(mean ppst`int' mean ppr`int' mean lppd`int')

      gen p75_pnltprsXpost = p75_`pp'Xpost
      lab var p75_pnltprsXpost "Top quartile X Post"
      gen p25_75_pnltprsXpost = p25_75_`pp'Xpost
      lab var p25_75_pnltprsXpost "Middle 2 quartiles X Post"
      gen pnltprsXpost = `pp'Xpost
      lab var pnltprsXpost "Penalty pressure X Post"
      tab fy, summarize(p75_pnltprsXpost)

      *loc pnltprs pnltprsXpost
      loc pnltprs p75_pnltprsXpost p25_75_pnltprsXpost
      areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

      *mean dep var
      sum `y' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching hrrhhi_SNF) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
    }
  }

  *use FY indicators
  loc file ols_`y'_cs_fy
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"

    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c' `l`pp'')"

    foreach pp of varlist ppst`int' ppr`int' lppd`int' {
      use `an', clear
      keep if cond=="`c'"

      forval t = 2010/2016 {
        gen p75_pnltprsX`t' = p75_`pp'X`t'
        lab var p75_pnltprsX`t' "Top quartile X `t'"
        gen p25_75_pnltprsX`t' = p25_75_`pp'X`t'
        lab var p25_75_pnltprsX`t' "Middle 2 quartiles X `t'"
        gen pnltprsX`t' = `pp'X`t'
        lab var pnltprsX`t' "Penalty pressure X `t'"
      }

      *loc pnltprs pnltprsX20*
      loc pnltprs p75_pnltprsX20* p25_75_pnltprsX20*
      areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

      *mean dep var
      sum `y' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching hrrhhi_SNF) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
    }
  }
}




*---------------------------
*use both cross-hospital & within-hospital cross-time variation in penalty pressure
use `an', clear

*get indicators for top quartlie & middle 2 quartiles
foreach pp of varlist ppst ppr lppd {
  bys cond fy: egen x_p75_`pp' = pctile(`pp'), p(75)
  bys cond fy: egen x_p25_`pp' = pctile(`pp'), p(25)
  gen p75_`pp' = `pp' > x_p75_`pp'
  gen p25_75_`pp' = `pp' > x_p25_`pp' & `pp' <= x_p75_`pp'
}
drop x_*

tab fy, summarize(p75_ppst)

local lppst "Predicted likelihood of penalty"
local lppr "Predicted penalty rate"
local llppd "Log Predicted penalty amount ($)"

*create interaction terms with indicators for the top quartile, middle 2 quatiles in the penalty pressure and post indicators
foreach pp of varlist ppst ppr lppd {
  forval t=2010/2016 {
    gen p75_`pp'X`t' = p75_`pp' * _Ify_`t'
    lab var p75_`pp'X`t' "Top quartile in `l`pp'' X `t'"
    gen p25_75_`pp'X`t' = p25_75_`pp' * _Ify_`t'
    lab var p25_75_`pp'X`t' "Middle 2 quartiles in `l`pp'' X `t'"
    gen `pp'X`t' = `pp' * _Ify_`t'
    lab var `pp'X`t' "`l`pp'' X `t'"
  }
  gen p75_`pp'Xpost = p75_`pp' * post
  gen p25_75_`pp'Xpost = p25_75_`pp' * post
  lab var p75_`pp'Xpost "Top quartile X Post"
  lab var p25_75_`pp'Xpost "Middle 2 quartiles X Post"
  gen `pp'Xpost = `pp' * post
  lab var `pp'Xpost "`l`pp'' X Post"
}
des p75* p25_75*

lab var ppst "Predicted likelihood of penalty"
lab var ppr "Predicted penalty rate"
lab var ppd "Predicted penalty amount ($)"
lab var lppd "Log Predicted penalty amount ($)"

tempfile an2
save `an2'

foreach y of varlist refhhi refhhi_prevSNFs shsnf_used_ag shsnf_used rat_nsnf_used shref {
  * use post dummy
  loc file ols_`y'_ts_post
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    *xtile gp_dischnum=dischnum, n(5)

    di "Condition `c'------------------------"

    loc file ols_`y'_ts_post
    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c' `l`pp'')"
    loc sp i.gp_beds vi_snf_l own_* urban teaching hrrhhi_SNF

    foreach pp of varlist ppst ppr lppd {
      use `an2', clear
      keep if cond=="`c'"

      gen p75_pnltprsXpost = p75_`pp'Xpost
      lab var p75_pnltprsXpost "Top quartile X Post"
      gen p25_75_pnltprsXpost = p25_75_`pp'Xpost
      lab var p25_75_pnltprsXpost "Middle 2 quartiles X Post"
      gen pnltprsXpost = `pp'Xpost
      lab var pnltprsXpost "Penalty pressure X Post"

      loc pnltprs pnltprsXpost
      *loc pnltprs p75_pnltprsXpost p25_75_pnltprsXpost
      areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

      *mean dep var
      sum `y' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching hrrhhi_SNF) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
    }
  }

  *use FY indicators
  loc file ols_`y'_ts_fy
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex

  foreach c in "AMI" "HF" "PN" {
    di "Condition `c'------------------------"

    loc out "outreg2 using `reg'/`file'.xls, tex append label ctitle(`c' `l`pp'')"
    loc sp i.gp_beds vi_snf_l own_* urban teaching hrrhhi_SNF

    foreach pp of varlist ppst ppr lppd {
      use `an2', clear
      keep if cond=="`c'"

      forval t = 2010/2016 {
        gen p75_pnltprsX`t' = p75_`pp'X`t'
        lab var p75_pnltprsX`t' "Top quartile X `t'"
        gen p25_75_pnltprsX`t' = p25_75_`pp'X`t'
        lab var p25_75_pnltprsX`t' "Middle 2 quartiles X `t'"
        gen pnltprsX`t' = `pp'X`t'
        lab var pnltprsX`t' "Penalty pressure X `t'"
      }

      loc pnltprs pnltprsX20*
      *loc pnltprs p75_pnltprsX20* p25_75_pnltprsX20*
      areg `y' `pnltprs' _Ify_* `sp', absorb(provid) cluster(provid)

      *mean dep var
      sum `y' if e(sample)
      loc mdv: display %9.3f `r(mean)'

      `out' keep(`pnltprs' _Ify_* vi_snf_l own_* urban teaching hrrhhi_SNF) addtext(Mean dep. var., `mdv', Hospital FE, Y) dec(3) fmt(fc)
    }
  }
}


*---------------------------
* IV regression

use hosp_fy_VI, clear
keep if fy ==2008
gen rblack = black / dischnum
keep provid cond ses_score rblack dissh uncomp1
duplicates drop
foreach v of varlist ses_score rblack dissh uncomp1 {
  ren `v' `v'08
}
tempfile iv08
save `iv08'

use `an2', clear
merge m:1 provid cond using `iv08', keep(1 3) nogen

foreach v of varlist ses_score08 rblack08 dissh08 uncomp108 {
  capture drop `v'Xpost
  gen `v'Xpost = `v'*post
  forval t=2010/2016 {
    capture drop `v'X`t'
    gen `v'X`t' = `v'*_Ify_`t'
  }
}

compress
save penalty_VI_bycond, replace

loc ivpost ses_score08Xpost rblack08Xpost dissh08Xpost uncomp108Xpost
loc ivpost ses_score08Xpost rblack08Xpost dissh08Xpost uncomp108Xpost

*first stage
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach pp of varlist ppst ppr lppd {
    loc y `pp'Xpost
    areg `y' `iv' _Ify_* `sp' if cond=="`c'", cluster(provid) absorb(provid)
    test
  }
}

loc y refhhi
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach pp of varlist ppst ppr lppd {
    ivreg2 `y' _Ify_* `sp' i.provid (`pp'Xpost = `ivpost') if cond=="`c'", cluster(provid) first partial(i.provid)
  }
}



loc y refhhi
foreach c in "PN" {
  di "Condition `c'------------------------"
  foreach v of varlist ln_pnlt_dollar_c pnlt_rate_c {
    ivreg2 `y' _Ify_* i.size ln_dischnum_pac `sp' i.provid (`v'X20* = ses_scoreX20* rblackX20*) if cond=="`c'" & fy < 2015, cluster(provid) first partial(i.provid)
  }
}


loc nend 3

*endogenous set
loc end pnlt_rate_cX20*
loc ev1 pnlt_rate_cX2012
loc ev2 pnlt_rate_cX2013
loc ev3 pnlt_rate_cX2014

*IV set
loc iv ses_scoreX20* rblackX20*
*loc iv worked altabsent_short altvac altabsent_long worked_oo

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
foreach ev of varlist `end' {
  loc fsr_`ev'
}
*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc sp1 _Ify_* i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' _Ify_* `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

    estimates dir
    estimates save iv_e_n`n', replace

    mat fstat_n`n' = e(first)

    *for each first stage, save in a separate file
    forval j = 1/`nend' {
      estadd scalar fs_`ev`j'' = fstat_n`n'[4,`j'] : f_n`n'_`ev`j''
    }

    foreach ev of varlist `end' {
      loc fsr_`ev' `fsr_`ev'' f_n`n'_`ev'
    }
    loc ssr `ssr' y_n`n'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      reg `ev' `iv' `sp`n'', cluster(offid_nu)
      estadd scalar fr2_`ev' = `e(r2)' : f_n`n'_`ev'
    }
  }
  *for each first stage, save in a separate file
  foreach ev of varlist `end' {
    loc file iv1s_`ev'_prate_`c'
    esttab `fsr_`ev'' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(4)) se(par fmt(4))) keep(`iv') order(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }

  *save 2nd stage reg
  loc file iv2s_prate_`c'
  esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(4)) se(par fmt(4))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)
}


*use size of penalty in dollars

*endogenous set
loc end ln_pnlt_dollar_cX20*
loc ev1 ln_pnlt_dollar_cX2012
loc ev2 ln_pnlt_dollar_cX2013
loc ev3 ln_pnlt_dollar_cX2014

*IV set
loc iv ses_scoreX20* rblackX20*
*loc iv worked altabsent_short altvac altabsent_long worked_oo

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
foreach ev of varlist `end' {
  loc fsr_`ev'
}
*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc sp1 _Ify_* i.provid
loc sp2 `sp1' i.size ln_dischnum_pac
loc sp3 `sp2' own_* urban teaching
loc sp4 `sp3' vi_snf
loc y refhhi

foreach c in "AMI" "HF" "PN" {
  forval n = 1/4 {
    di ""
    di "-------- Endog. Var & Spec `n' ----------"
    eststo y_n`n': ivreg2 `y' _Ify_* `sp`n'' (`end' = `iv') if cond=="`c'" & fy < 2015, cluster(provid) first savefirst savefprefix(f_n`n'_) partial(i.provid) gmm2s

    estimates dir
    estimates save iv_e_n`n', replace

    mat fstat_n`n' = e(first)

    *for each first stage, save in a separate file
    forval j = 1/`nend' {
      estadd scalar fs_`ev`j'' = fstat_n`n'[4,`j'] : f_n`n'_`ev`j''
    }

    foreach ev of varlist `end' {
      loc fsr_`ev' `fsr_`ev'' f_n`n'_`ev'
    }
    loc ssr `ssr' y_n`n'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      reg `ev' `iv' `sp`n'', cluster(offid_nu)
      estadd scalar fr2_`ev' = `e(r2)' : f_n`n'_`ev'
    }
  }
  *for each first stage, save in a separate file
  foreach ev of varlist `end' {
    loc file iv1s_`ev'_pdollar_`c'
    esttab `fsr_`ev'' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(4)) se(par fmt(4))) keep(`iv') order(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }

  *save 2nd stage reg
  loc file iv2s_pdollar_`c'
  esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(4)) se(par fmt(4))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)
}



/*
*--------------------------------
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

collapse (sum) read30 dischnum mcr_pmt tot_pat_rev, by(cond provid)
gen rra = read30/dischnum

*create share of Medicare payment for each hospital-fy-condition
gen sh_pmt = (mcr_pmt/tot_pat_rev)

keep provid rra sh_pmt cond

tempfile rra
save `rra'

*get penalty status in t + 2
use `dta'/hrrp_penalty, clear
keep if fy==`t'
keep pnltr provid err_ami err_hf err_pn
duplicates drop
gen fy=`t'

reshape long err_, i(provid) j(condition) string
replace cond = upper(cond)

*create penalized indicator for each condition if ERR > 1
*gen penalized = pnltr > 0 & err_ > 1
gen penalized = pnltr > 0

merge 1:1 cond provid using `rra', keep(3) nogen

gen pos = err-1 > 0
gen prod = (err-1) * sh_pmt
*assert prod>=0
drop pos

*local linear regression of penalty status on raw readmissions only
capture drop phat*
gen phat_rra = .
gen phat_errsh = .

foreach c in "AMI" "HF" "PN" {
locreg penalized if cond=="`c'", continuous(rra) gen(x, replace) logit
replace phat_rra = x if cond=="`c'"

locreg penalized if cond=="`c'", continuous(prod) gen(x, replace) logit
replace phat_errsh = x if cond=="`c'"
}
drop x

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
sum phat*
tempfile phats
save `phats'


*----------------------------------
*local linear regression of penalty status on raw readmissions only
use `phats', clear

keep if fy==2014

foreach c in "AMI" "HF" "PN" {
preserve
keep if cond=="`c'"
tw (line phat_rra rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) sort) (scatter penalized rra, xsc(r(0 0.4)) xlab(0(0.05)0.4) xti("Raw 30-day readmission rate during 2009-2011") yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital is penalized") col(1)))
graph export `gph'/pnlt_rra_`c'.eps, replace

tw (line phat_errsh prod, sort) (scatter penalized prod,  xti("(ERR -1) X Medicare payment share during 2009-2011") yti(Probability of penalty in 2014) msymbol(circle_hollow) leg(order(1 "Probability of penalty" 2 "Hospital is penalized") col(1)))
graph export `gph'/pnlt_errsh_`c'.eps, replace
restore
}
*----------------------------------

*plot the referral concentration trend across FY for high-penalty vs low-penalty hospitals
use `an2', clear
capture drop x
gen x = pnltr if fy==2013
bys provid: egen pnltr13= max(x)
*keep cond provid fy refhhi pnltr13 size

gen fy0 = fy
replace fy = fy + 2
merge m:1 cond provid fy using `phats', keep(1 3) nogen
drop fy
rename fy0 fy

capture drop x
gen x = prod if fy==2013
bys cond provid: egen errsh13= max(x)

*divide into 3 equal sized groups
preserve
keep if fy==2014
keep provid pnltr13 phat_rra phat_errsh cond
duplicates drop

gen gp = .
gen gp_phat = .
gen gp_errsh13 = .

foreach c in "AMI" "HF" "PN" {
capture drop x
xtile x = pnltr13 if cond=="`c'", nq(3)
replace gp = x if cond=="`c'"
table gp, c(mean pnlt)

capture drop x
xtile x = phat_rra if cond=="`c'", nq(3)
replace gp_phat = x if cond=="`c'"
table gp_phat, c(mean phat_rra)

capture drop x
xtile x = phat_errsh if cond=="`c'", nq(3)
replace gp_errsh13 = x if cond=="`c'"
table gp_errsh13, c(mean phat_errsh)
}

tempfile gp
save `gp'
restore

merge m:1 cond provid using `gp', keepusing(gp*) nogen

*get referral HHI for 2009-11
preserve
use PACreferral_tchpm, clear
drop if cond=="HK"
keep if pac=="SNF"
keep if fy > 2008

gen t = 1 if fy >= 2009 & fy <= 2011
replace t = 2 if fy >= 2012 & fy <= 2014
replace t = 3 if fy >= 2015 & fy <= 2016
assert t!=.
collapse (sum) dischnum_pac, by(cond provid t pacprovid)

bys cond provid t: egen tot = sum(dischnum)
gen refshsq = (dischnum/tot)^2

*aggregate to the hospital-FY level
collapse (sum) refhhi = refshsq, by(cond provid t)
assert refhhi <= 1

reshape wide refhhi, i(cond provid) j(t)
gen change_f0911_t1214 = refhhi2 - refhhi1
gen change_f0911_t1516 = refhhi3 - refhhi1

rename refhhi1 refhhi_0911
rename refhhi2 refhhi_1214
rename refhhi3 refhhi_1516

tempfile tmp
save `tmp'
restore

*merge with referral HHI during 2009-2011
merge m:1 cond provid using `tmp', keep(3) nogen

*calculate change in referral HHI from 2009-2011 to 2012, 2013, ...
gen gr_refhhi = 100*(refhhi - refhhi_0911)/refhhi_0911
replace gr_refhhi=. if fy < 2012
tab fy, summarize(gr_refhhi)

tempfile an3
save `an3'

* distribution of referral HHI by condition
use `an3', clear
foreach c in "AMI" "HF" "PN" {
sum refhhi  if cond=="`c'", de
drop if refhhi > r(p99)
}
tw (kdensity refhhi if cond=="AMI") (kdensity refhhi if cond=="HF") (kdensity refhhi if cond=="PN"), leg(order(1 "AMI" 2 "HF" 3 "PN")) xti(SNF Referral HHI) yti(Kernel density)
graph export `gph'/kd_refhhi_bycond.eps, replace

*by penalty groups, plot trend of referral HHI across time for each condition

*different ways to measure penalty level
loc lgp "overall penalty rate"
loc lgp_phat "probability of penalty based on raw readmission rates"
loc lgp_errsh13 "probability of penalty based on relative performance and size"

foreach g in "gp" "gp_phat" "gp_errsh13" {
use `an3', clear
bys provid: egen minsize =min(size)
keep if minsize==3
*165 hospitals per fy
collapse (mean) refhhi gr_refhhi, by(cond `g' fy)
tempfile gd
save `gd'

foreach c in "AMI" "HF" "PN" {
use `gd', clear
keep if cond=="`c'"

tab `g' , summarize(refhhi)

tw (conected refhhi fy if `g'==1) (conected refhhi fy if `g'==2) (conected refhhi fy if `g'==3), yti(Mean SNF referral HHI) xti(FY) xline(2010.5) xline(2011.5) xlab(2008(1)2016) leg(order(1 "Low penalty" 2 "Medium penalty" 3 "High penalty") col(1)) subti("Large hospitals (300+ beds)") ysc(r(0 0.2)) ylab(0(0.05)0.2) ti("Trend of mean SNF referral HHI for `c'" "by `l`g''", size(medium))
graph export `gph'/refhhi_fy_by`g'_`c'.eps, replace

drop if fy < 2012
tw (conected gr_refhhi fy if `g'==1) (conected gr_refhhi fy if `g'==2) (conected gr_refhhi fy if `g'==3), yti(Mean growth of SNF referral HHI (%)) xti(FY) xlab(2012(1)2016) leg(order(1 "Low penalty" 2 "Medium penalty" 3 "High penalty") col(1)) subti("Large hospitals (300+ beds)") ysc(r(0 100)) ylab(0(10)100) ti("Trend of mean growth of SNF referral HHI for `c' from 2009-2011" "by `l`g''", size(medium))
graph export `gph'/gr_refhhi_fy_by`g'_`c'.eps, replace
}
}

*---------------------------
*understand how the # SNFs are associated with referral HHI
use PACreferral_tchpm, clear
keep if pac=="SNF"
drop if cond=="HK"
*drop if pac==""

collapse (sum) dischnum_pac, by(condition provid fy pacprovid)
list if provid==10005 & fy==2011 & condition=="HK"

gen i = 1 if pacprovid!=. & dischnum_pac!=0
collapse (sum) nsnf = i, by(cond provid fy)

tempfile nsnf
save `nsnf'

use `an3', clear
bys provid: egen minsize =min(size)
keep if minsize==3
drop minsize
merge 1:1 cond provid fy using `nsnf', keep(3) nogen

binscatter refhhi nsnf if nsnf < 40, by(cond) line(qfit) xsc(r(0 40)) xlab(0(10)40) ysc(r(0 0.3)) ylab(0(0.05)0.3) leg(order(1 "AMI" 2 "HF" 3 "PN") col(3)) xti(Number of referred SNFs) yti(SNF referral HHI) ti(Relationship between the number of SNFs and SNF referral HHI, size(medium))
graph export `gph'/test.eps, replace

collapse (mean) refhhi , by(cond nsnf)
sc refhhi nsnf if nsnf < 40, by(cond) xsc(r(0 40)) xlab(0(10)40) ysc(r(0 1)) ylab(0(0.1)1) leg(order(1 "AMI" 2 "HF" 3 "PN") col(3)) xti(Number of referred SNFs) yti(Mean SNF referral HHI)
graph export `gph'/test2.eps, replace */
