options nocenter ;
*Extract some variables from the HHA ALPHA file from CMS HHA cost report data;
*Reference: snf_chars_formedstat.sas
http://www.nber.org/docs/hcris/extract_alpha2552-10.sas;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";
libname out "/home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loop(fyear=,lyear=);

    %do year=&fyear. %to &lyear.;

        %let dataset=alpha&year.;

        proc sql;
          **select appropriate worksheets;
          create table &dataset. as
          select *
          from library.snf_alpha&year._long
          where  (
          WKSHT_CD EQ "S200001"
          )
          ;

          **add varname;
          data out.&dataset.;
            set &dataset.;
            length varname $32. ;
            ;                                                                                        *32-character varname maximum;
            select;                                                                                  *12345678901234567892123456789312;

            when ( WKSHT_CD EQ "S200001" and CLMN_NUM = "0010" and LINE_NUM = "00400" ) varname = "snfname" ;
            when ( WKSHT_CD EQ "S200001" and CLMN_NUM = "0010" and LINE_NUM = "00500" ) varname = "nfname" ;
            when ( WKSHT_CD EQ "S200001" and CLMN_NUM = "0020" and LINE_NUM = "00400" ) varname = "snf_ccn" ;
            when ( WKSHT_CD EQ "S200001" and CLMN_NUM = "0020" and LINE_NUM = "00500" ) varname = "nf_ccn" ;
            otherwise;
          end;
        run;
        proc contents;
          proc print data=out.&dataset ( obs = 50 );
            *where WKSHT_CD EQ "S200000" ;
            proc freq data=out.&dataset;
              table varname;
            run;

          %end;
        %mend;
        %loop(fyear=2016,lyear=2016);
