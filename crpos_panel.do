*create hospital penal data from Medicare provider of services data for each FY 2011-2016

loc dta /ifs/home/kimk13/VI/data
cd `dta'/pos

*---------------------------------
*hospital POS panel data
forval yr = 2005/2016 {
    di "Year `yr'----------------------------------------"
    use pos`yr', clear

    *keep hospitals
    capture rename prov0075 prvdr_ctgry_cd
    keep if prvdr_ctgry_cd=="01"

    *keep short-term hosptials
    capture rename prov0085 prvdr_ctgry_sbtyp_cd
    keep if prvdr_ctgry_sbtyp_cd=="01" | prvdr_ctgry_sbtyp_cd=="1"

    capture rename prov1680 provid
    capture rename prvdr_num provid
    capture rename prov0475 fac_name
    capture rename prov0755 crtfd_bed_cnt
    capture rename prov2885 gnrl_cntl_typ

    keep provid fac_name crtfd_bed_cnt gnrl_cntl_typ
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
clear
forval yr = 2005/2016 {
    append using `y`yr''
}

gen own_fp = gnrl_cntl_typ=="04"
gen own_np = gnrl_cntl_typ=="02"
gen own_gv = gnrl_cntl_typ=="05" | gnrl_cntl_typ=="06" | gnrl_cntl_typ=="07"
drop gnrl_cntl_typ

compress
save pos_panel_hosp.dta, replace
