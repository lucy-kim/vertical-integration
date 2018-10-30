*create SNF quality (rating) data from the SNF Compare archive database
*use average of 2009 ratings across months for each SNF

loc out /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/snf-compare

import excel using ratings2009.xlsx, firstrow clear
sum year month
keep if month==1

keep PROVNUM *rating
duplicates drop

rename PROVNUM pacprovid

*drop '70' = 'Too New to Rate', '90' = 'Data Not available'
drop if overall_rating=="70" | overall_rating=="90"

destring *rating, replace

compress
save ratings2009, replace
