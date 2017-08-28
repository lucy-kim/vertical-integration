options nocenter ;
*Extract some variables from the HHA ALPHA file from CMS HHA cost report data;
*Reference: hosp_chars_formedstat.sas
http://www.nber.org/docs/hcris/extract_nmrc2552-10.sas;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
libname out "/home/hcmg/kunhee/Labor/Bayada_data";

%macro loop (fyear=,lyear=);

%do year=&fyear. %to &lyear.;

    %let dataset=nmrc&year.;

    proc sql;
       **select appropriate worksheets;
       create table &dataset. as
       select *
       from library.hosp_nmrc_rpt&year._long
       where  (
       ( WKSHT_CD EQ "S300001"  )
       or ( WKSHT_CD EQ "S410000" )
       or ( WKSHT_CD EQ "S200001" )
       )
       ;

       **add varname;
       data out.&dataset.;
           set &dataset;
           length varname $32. ;
           ;                                                                                      *32-character varname maximum;
           *                                                                                      *12345678901234567892123456789312;
           select;
           *hospital-based PAC;
           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00300") varname="hosp_name"; * hosp name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00300") varname="hosp_ccn"; * hosp CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00300") varname="hosp_cbsa"; * hosp BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00300") varname="hosp_provtype"; * hosp provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00300") varname="hosp_dtcert"; * hosp date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00400") varname="ipf_name"; * ipf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00400") varname="ipf_ccn"; * ipf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00400") varname="ipf_cbsa"; * ipf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00400") varname="ipf_provtype"; * ipf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00400") varname="ipf_dtcert"; * ipf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00500") varname="irf_name"; * irf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00500") varname="irf_ccn"; * irf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00500") varname="irf_cbsa"; * irf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00500") varname="irf_provtype"; * irf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00500") varname="irf_dtcert"; * irf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00700") varname="swbsnf_name"; * swbsnf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00700") varname="swbsnf_ccn"; * swbsnf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00700") varname="swbsnf_cbsa"; * swbsnf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00700") varname="swbsnf_provtype"; * swbsnf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00700") varname="swbsnf_dtcert"; * swbsnf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00800") varname="swbnf_name"; * swbnf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00800") varname="swbnf_ccn"; * swbnf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00800") varname="swbnf_cbsa"; * swbnf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00800") varname="swbnf_provtype"; * swbnf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00800") varname="swbnf_dtcert"; * swbnf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="00900") varname="snf_name"; * snf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="00900") varname="snf_ccn"; * snf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="00900") varname="snf_cbsa"; * snf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00900") varname="snf_provtype"; * snf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="00900") varname="snf_dtcert"; * snf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01000") varname="nf_name"; * nf name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01000") varname="nf_ccn"; * nf CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01000") varname="nf_cbsa"; * nf BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01000") varname="nf_provtype"; * nf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01000") varname="nf_dtcert"; * nf date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01100") varname="oltc_name"; * oltc name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01100") varname="oltc_ccn"; * oltc CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01100") varname="oltc_cbsa"; * oltc BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01100") varname="oltc_provtype"; * oltc provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01100") varname="oltc_dtcert"; * oltc date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01200") varname="hha_name"; * hha name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01200") varname="hha_ccn"; * hha CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01200") varname="hha_cbsa"; * hha BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01200") varname="hha_provtype"; * hha provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01200") varname="hha_dtcert"; * hha date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01300") varname="asc_name"; * asc name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01300") varname="asc_ccn"; * asc CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01300") varname="asc_cbsa"; * asc BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01300") varname="asc_provtype"; * asc provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01300") varname="asc_dtcert"; * asc date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01400") varname="hospice_name"; * hospice name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01400") varname="hospice_ccn"; * hospice CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01400") varname="hospice_cbsa"; * hospice BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01400") varname="hospice_provtype"; * hospice provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01400") varname="hospice_dtcert"; * hospice date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01500") varname="rhc_name"; * rhc name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01500") varname="rhc_ccn"; * rhc CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01500") varname="rhc_cbsa"; * rhc BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01500") varname="rhc_provtype"; * rhc provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01500") varname="rhc_dtcert"; * rhc date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01600") varname="fqhc_name"; * fqhc name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01600") varname="fqhc_ccn"; * fqhc CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01600") varname="fqhc_cbsa"; * fqhc BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01600") varname="fqhc_provtype"; * fqhc provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01600") varname="fqhc_dtcert"; * fqhc date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01700") varname="cmhc_name"; * cmhc name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01700") varname="cmhc_ccn"; * cmhc CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01700") varname="cmhc_cbsa"; * cmhc BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01700") varname="cmhc_provtype"; * cmhc provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01700") varname="cmhc_dtcert"; * cmhc date certified;

           when (WKSHT_CD="S200001" and CLMN_NUM="0010" and LINE_NUM="01800") varname="renal_name"; * renal name;
           when (WKSHT_CD="S200001" and CLMN_NUM="0020" and LINE_NUM="01800") varname="renal_ccn"; * renal CCN number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0030" and LINE_NUM="01800") varname="renal_cbsa"; * renal BSA number;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="01800") varname="renal_provtype"; * renal provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0050" and LINE_NUM="01800") varname="renal_dtcert"; * renal date certified;

           *other hospital characteristics;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="02000") varname="cr_start"; * cost reporting period start;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0020" and LINE_NUM="02000") varname="cr_end"; * cost reporting period end;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="02200") varname="dissh"; * dummy for disproportionate share hospital adjustment;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="02201") varname="uncomp1"; * dummy for uncompensated care before 10/1 during the cost reporting period;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0020" and LINE_NUM="02201") varname="uncomp2"; * dummy for uncompensated care on/after 10/1 during the cost reporting period;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="02600") varname="urban"; * urban/rural classification;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="05600") varname="teach"; *teaching dummy;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="08000") varname="ltch"; * dummy for LTCH;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="10500") varname="cah"; * dummy for CAH;

           *malpractice;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11700") varname="req_malprins"; * dummy for being legally required to carry malpractice insurance;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11800") varname="malprins_policy"; * is malpractice insurance claims-made or occurrence policy;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11801") varname="malprins_premium"; * malpractice insurance premiums;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0020" and LINE_NUM="11801") varname="malprins_paidloss"; * malpractice insurance paid losses;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0030" and LINE_NUM="11801") varname="malprins_selfins"; * malpractice insurance self insurance;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11900") varname="malprins_liablim1"; * malpractice insurance liability limit per lawsuit;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0020" and LINE_NUM="11900") varname="malprins_liablim2"; * malpractice insurance liability limit per policy year;

           when (WKSHT_CD EQ "S300001" and CLMN_NUM="0020" and LINE_NUM="01400") varname="beds"; *# beds;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="0150" and LINE_NUM="01400") varname="dischrg"; *# discharges;

           when (WKSHT_CD EQ "S410000" and CLMN_NUM="0050" and LINE_NUM="03600") varname="totepi_st"; *total number of episodes (standard/nonoutlier) in hosp-based HHA;
           when (WKSHT_CD EQ "S410000" and CLMN_NUM="0050" and LINE_NUM="03700") varname="totepi_out"; *total number of outlier episodes in hosp-based HHA;
           otherwise;
             end;
         run;

         proc contents;
             proc print data=out.&dataset ( obs = 50 );
               proc freq data=out.&dataset;
                 table varname;
               run;
     %end;
 %mend;
 %loop(fyear=2011,lyear=2016);
