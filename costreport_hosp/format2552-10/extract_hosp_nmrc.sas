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
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00300") varname="hosp_provtype"; * hosp provider type;

           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00400") varname="ipf_provtype"; * ipf provider type;
           when (WKSHT_CD="S200001" and CLMN_NUM="0040" and LINE_NUM="00500") varname="irf_provtype"; * irf provider type;

           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="02600") varname="urban"; * urban/rural classification;

           *malpractice;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11800") varname="malprins_policy"; * is malpractice insurance claims-made or occurrence policy;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0010" and LINE_NUM="11801") varname="malprins_premium"; * malpractice insurance premiums;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0020" and LINE_NUM="11801") varname="malprins_paidloss"; * malpractice insurance paid losses;
           when (WKSHT_CD EQ "S200001" and CLMN_NUM="0030" and LINE_NUM="11801") varname="malprins_selfins"; * malpractice insurance self insurance;

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
