*create hospital penal data from Medicare provider of services data for each FY 2011-2016

loc dta /ifs/home/kimk13/VI/data
cd `dta'/pos

forval yr = 2011/2016 {
    use pos`yr', clear
    *keep hospitals
    keep if prvdr_ctgry_cd=="01"

    *keep short-term hosptials
    keep if prvdr_ctgry_s=="1"

    keep prvdr_num fac_name crtfd_bed_cnt gnrl_cntl_typ
    rename prvdr_num provid
    destring provid, replace

    gen fy = `yr'

    duplicates drop
    duplicates tag provid, gen(dup)
    assert dup==0
    drop dup

    tempfile y`yr'
    save `y`yr''
}

* Append POS data across FY
use `y2011', clear
forval yr = 2012/2016 {
    append using `y`yr''
}

gen own_fp = gnrl_cntl_typ=="04"
gen own_np = gnrl_cntl_typ=="02"
gen own_gv = gnrl_cntl_typ=="05" | gnrl_cntl_typ=="06" | gnrl_cntl_typ=="07"
drop gnrl_cntl_typ

compress
save pos_panel, replace
