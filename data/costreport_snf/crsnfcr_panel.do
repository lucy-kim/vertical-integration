*create hospital panel data for each calendar year

cd /home/hcmg/kunhee/vertical-int/data/HCRIS

*append all years of Cost report data
use snfcr2016_2540_10, clear

keep nf_ccn prvdr_num own* fyear *nbeds *tot

duplicates tag prvdr_num fyear, gen(dup)
tab dup
drop if dup > 0
drop dup

compress
save snfcr_panel, replace
