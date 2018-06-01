*create SNF quality (rating) data from the SNF Compare archive database
*use average of 2009 ratings across months for each SNF

loc out /ifs/home/kimk13/VI/output
loc dta /ifs/home/kimk13/VI/data

cd `dta'/Medicare/snf-compare

import excel using ratings2009.xlsx, firstrow clear

keep provnum overall_rating year month
duplicates drop

collapse (mean) overall_rating, by(provnum)
