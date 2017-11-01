*explore the health care M&A data from Levin Associates

loc home /home/hcmg/kunhee/Labor/Bayada_data
cd `home'

*does it include joint venture, not full M&A?

insheet using mna/levinassoc_mna_2005_2015.csv, comma names clear
des

tostring date_announce, replace
gen yr = substr(date_announce,1,4)
gen mo = substr(date_announce,5,2)
gen day = substr(date_announce,7,2)
destring yr mo day, replace
gen date_announce2 = mdy(mo,day,yr)
format date_announce2 %d
drop date_announcement yr mo day
rename date_announce2 date_announce

list date_announce acquirer_name target_name target_sector if regexm(target_name, "Temple")
list date_announce acquirer_name target_name target_sector if regexm(acquirer_name, "Temple")
