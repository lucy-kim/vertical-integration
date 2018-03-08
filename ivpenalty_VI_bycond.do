*IV analysis of the effect of penalty pressure on referral concentration

loc gph /ifs/home/kimk13/VI/gph
loc reg /ifs/home/kimk13/VI/tbls
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare

use penalty_VI_bycond, clear

lab var ses_score08Xpost "Mean SES score X Post"
lab var rblack08Xpost "Share of black patients X Post"
lab var dissh08Xpost "DSH payment dummy X Post"
lab var uncomp108Xpost "Uncompensated care X Post"

local lppst "Predicted likelihood of penalty"
local lppr "Predicted penalty rate"
local llppd "Log Predicted penalty amount ($)"

loc sp i.gp_beds vi_snf_l own_* urban teaching hrrhhi_SNF
loc ivpost ses_score08Xpost rblack08Xpost
*dissh08Xpost uncomp108Xpost
loc ivfy ses_score08X20* rblack08X20* dissh08X20* uncomp108X20*

areg refhhi _Ify_* `sp' ppstXpost `ivpost' if cond=="AMI", cluster(provid) absorb(provid)

/* *first stage
foreach c in "PN" {
di "Condition `c'------------------------"
foreach pp of varlist ppst ppr lppd {
loc y `pp'Xpost
areg `y' `iv' _Ify_* `sp' if cond=="`c'", cluster(provid) absorb(provid)
test
}
} */

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
loc fsr

*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc nend 1
loc iv `ivpost'

loc y refhhi
loc n = 0

/* foreach c in "PN" {
    di "Condition `c'------------------------"
     foreach pp of varlist ppst ppr lppd {
       ivreg2 `y' _Ify_* `sp' i.provid (`pp'Xpost = `ivpost') if cond=="`c'", cluster(provid) first partial(i.provid)
     }
  } */

foreach c in "AMI" "HF" "PN" {
  di "Condition `c'------------------------"

  foreach pp of varlist ppst ppr lppd {
    loc n = `n' + 1
    di "-------- Endog. Var `ee' = `l`pp'' ----------"

    capture drop pnltprsXpost
    gen pnltprsXpost = `pp'Xpost
    lab var pnltprsXpost "Penalty pressure X Post"
    loc end pnltprsXpost
    des `end' `iv'
    *loc ev1 `pp'Xpost

    eststo y_n`n': ivreg2 `y' _Ify_* `sp' i.provid (`end' = `iv') if cond=="`c'", cluster(provid) first savefirst savefprefix(f_n`n') partial(i.provid)

    estimates dir
    estimates save iv_e_n`n', replace

    mat fstat_n`n' = e(first)

    *for each first stage, save in a separate file
    forval j = 1/`nend' {
      estadd scalar fs = fstat_n`n'[4,`j'] : f_n`n'`end'
    }

    *foreach ev of varlist `end' {
      loc fsr `fsr' f_n`n'`end'
    *}
    loc ssr `ssr' y_n`n'
    di "`ssr'"

    *get R-squared from first stage OLS regression
    foreach ev of varlist `end' {
      areg `ev' `iv' _Ify_* `sp' if cond=="`c'", cluster(provid) absorb(provid)
      qui test
      estadd scalar fr2_`ev' = `e(r2)' : f_n`n'`ev'
    }
  }
}

*for each first stage, save in a separate file
/* foreach ev of varlist `end' { */
    loc file iv1s_`y'
    esttab `fsr' using `reg'/`file'.tex, booktabs replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`iv') order(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
/* } */

*save 2nd stage reg
loc file iv2s_`y'
esttab `ssr' using `reg'/`file'.tex, booktabs replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(pnltprsXpost) label starlevels( * 0.10 ** 0.05 *** 0.010)
