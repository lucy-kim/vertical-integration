options nocenter ;
*by Kunhee Kim, kunhee@wharton.upenn.edu, 2015-03-11 ;
*Read HHA cost report alpha-numeric file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt_alphnmrc.sas ;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loop;
%do year=2016 %to 2016;
    proc printto log  ="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt_alphnmrc&year..log";
    proc printto print="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt_alphnmrc&year..lst";
    FILENAME datafile "/home/hcmg/kunhee/vertical-int/data/HCRIS/hha_alpha1728_94_&year._long.csv";

    %let dataset=hha_alpha&year._long ;

    data library.&dataset.;
        length wksht_cd $7. line_num $5. clmn_num $4. itm_alphnmrc_itm_txt $40. default = 4  ;
* '2C' is hexadecimal for decimal 44 which represents ',' ;
* '0D' is hexadecimal for decimal 13 which represents '\r', which is the carriage return character;
        infile datafile dsd delimiter='2C0D'x  ;
        INPUT
            rpt_rec_num
            wksht_cd $
            line_num $
            clmn_num $
            itm_alphnmrc_itm_txt $
            ;
    proc means data=library.&dataset. max n;
        title "Inspect maximums:  Four bytes will retain six significant digits" ;
        title2 "&dataset.";
    proc freq data=library.&dataset.;
        title "";
        tables WKSHT_CD LINE_NUM CLMN_NUM ;
    proc contents data=library.&dataset.;
    data &dataset.;
        set library.&dataset.;
    proc sort data=&dataset. nodupkey;
        by wksht_cd clmn_num line_num  ;
    proc print data=&dataset. noobs;
        var wksht_cd clmn_num line_num;

        %end;
%mend;

%loop;
