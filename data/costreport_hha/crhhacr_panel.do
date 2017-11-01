*create HHA panel data from the Cost report

cd /home/hcmg/kunhee/vertical-int/data/HCRIS

*append all years of Cost report data
use hhacr2016.dta, clear

keep sn_visit_all undup_pat_cnt_all state city hhaname prvdr_num own* fyear

drop if sn_visit_all==. | undup_pat_cnt_all==.

drop if state=="PR" | state=="MP" | state=="GU" | state=="VI"

duplicates drop
duplicates tag prvdr_num , gen(dup)
*40 obs have dup > 0

* drop these for now
drop if dup > 0
drop dup

compress
save hhacr_panel, replace
