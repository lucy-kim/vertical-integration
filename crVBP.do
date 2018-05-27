* create data for VBP risk adjustment factor (proxy) at the hospital-year level

set linesize 150

loc dta /ifs/home/kimk13/VI/data
cd `dta'/Medicare/VBP

loc dir13 FY_13_FR_Table_16
loc file13 CMS-1588-F Table 16_Proxy_VBP Factors_August 2012.txt

loc dir14 FY_14_FR_Table_16A
loc file14 Table 16A_FR14.txt

loc dir15 FY2015-FR-Table-16A-16B
loc file15 CMS-1607-F Table 16A.txt

loc dir16 FY2016-CMS-1632-FR-Table-16
loc file16 CMS-1632-F Table 16A - FY2016.txt

forval y = 13/16 {
  import delim using "`dir`y''/`file`y''", clear
  des
  list in 1/10
  drop if _n < 3 | v1=="" | v2==""
  renvars v1 v2 \ provid adjf
  keep provid adjf
  gen fy = 2000 + `y'
  destring provid adjf, replace

  tempfile f`y'
  save `f`y''
}

clear
forval y = 13/16 {
  append using `f`y''
}

rename adjf vbp_adjf
lab var vbp_adjf "proxy VBP adjustment factor"

compress
save vbp, replace
