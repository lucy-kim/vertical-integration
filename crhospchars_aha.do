*create hospital-FY level data containing hospital characteristics from AHA data for 2005-2015

cd /ifs/home/kimk13/VI/data/AHA

insheet using "AHA_2005-2015.csv", comma clear


compress
save hospchars_aha, clear
