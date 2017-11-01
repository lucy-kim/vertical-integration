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
       or ( WKSHT_CD EQ "S200000" )
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
           when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01900") varname="hosp_provtype"; * hosp provider type;

           when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="02000") varname="subpr_provtype"; * subprovider provider type;

           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="02104") varname="urban"; * urban/rural classification;

           *malpractice;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="05400") varname="malprins_premium"; * malpractice insurance premiums;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0200" and LINE_NUM="05400") varname="malprins_paidloss"; * malpractice insurance paid losses;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0300" and LINE_NUM="05400") varname="malprins_selfins"; * malpractice insurance self insurance;

           when (WKSHT_CD EQ "S300001" and CLMN_NUM="0100" and LINE_NUM="01200") varname="beds"; *# beds;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="1500" and LINE_NUM="01200") varname="dischrg"; *# discharges;

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
 %loop(fyear=2010,lyear=2011);
