options nocenter ;
*by Kunhee Kim, kunhee@wharton.upenn.edu, 2015-03-11 ;
*Read HHA cost report numeric file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt_nmrc.sas ;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loop;
%do year=2016 %to 2016;
    proc printto log  ="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt_nmrc&year..log";
    proc printto print="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt_nmrc&year..lst";

        FILENAME datafile "/home/hcmg/kunhee/vertical-int/data/HCRIS/hha_nmrc1728_94_&year._long.csv";

        %let dataset = hha_nmrc_rpt&year._long ;

    data library.&dataset.;
*HCRIS_Data_model.pdf reports lengths;
        LENGTH rpt_rec_num 6. wksht_cd $7. line_num $5. clmn_num $4. itm_val_num 7. ;
* '2C' is hexadecimal for decimal 44 which represents ',' ;
* '0D' is hexadecimal for decimal 13 which represents '\r', which is the carriage return character;
        infile datafile dsd delimiter='2C0D'x  ;
        INPUT
            rpt_rec_num
            wksht_cd  $
            line_num  $
            clmn_num  $
            itm_val_num
            ;
    proc means data=library.&dataset. n max;
        title "Inspect maximums:  Four bytes retain six significant digits" ;
    run;
    proc freq data=library.&dataset.;
        title "" ;
        tables wksht_cd clmn_num ;
    proc sort data=library.&dataset. nodupkey out = &dataset. ;
        by wksht_cd line_num clmn_num  ;
    proc print data=&dataset. noobs ;
        var wksht_cd clmn_num line_num;

        %end;
%mend;
%loop;
