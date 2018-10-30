
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

use anpenalty_VI_agg3c, clear

forval yr = 2011/2016 {
  replace ppr = `pnltmax`yr'' if ppr > `pnltmax`yr'' & fy==`yr'
}
replace ppr2012 = `pnltmax2012' if ppr2012 > `pnltmax2012'
sum ppr2012 ppr


tw (sc pnltr2014 ppr2012 if fy==2012 )
graph export `gph'/test.eps, replace
