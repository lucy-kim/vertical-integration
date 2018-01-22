*create hospital-HRR/HSA xwalk using the Dartmouth atlas data downloaded from http://www.dartmouthatlas.org/tools/downloads.aspx?tab=39

cd /ifs/home/kimk13/VI/data/dartmouth

insheet using hosp_hsa_hrr_2014.csv, comma names clear
keep provider hsanum hrrnum hrrstate

compress
save hosp_hrr_xwalk, replace
