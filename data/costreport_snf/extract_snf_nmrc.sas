options nocenter ;
*Extract some variables from the HHA ALPHA file from CMS HHA cost report data;
*Reference: snf_chars_formedstat.sas
http://www.nber.org/docs/hcris/extract_nmrc2552-10.sas;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";
libname out "/home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loop (fyear=,lyear=);

%do year=&fyear. %to &lyear.;

    %let dataset=nmrc&year.;

    proc sql;
       **select appropriate worksheets;
       create table &dataset. as
       select *
       from library.snf_nmrc_rpt&year._long
       where  (
       ( WKSHT_CD EQ "S300001"  )
       )
       ;

       **add varname;
       data out.&dataset.;
           set &dataset;
           length varname $32. ;
           ;                                                                                      *32-character varname maximum;
           *                                                                                      *12345678901234567892123456789312;
           select;
           *snfital-based PAC;
           when (WKSHT_CD="S300001" and CLMN_NUM="0010" and LINE_NUM="00100") varname="snf_nbeds"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0010" and LINE_NUM="00200") varname="nf_nbeds"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0070" and LINE_NUM="00100") varname="snf_days_tot"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0070" and LINE_NUM="00200") varname="nf_days_tot"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0120" and LINE_NUM="00100") varname="snf_disch_tot"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0120" and LINE_NUM="00200") varname="nf_disch_tot"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0210" and LINE_NUM="00100") varname="snf_admit_tot"; * snf provider type;
           when (WKSHT_CD="S300001" and CLMN_NUM="0210" and LINE_NUM="00200") varname="nf_admit_tot"; * snf provider type;
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
 %loop(fyear=2016,lyear=2016);
