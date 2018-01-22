options nocenter;
*Transpose to wide shape (multiple variables per report number) & merge alphanumeric & numeric data to create data on the ownership of HHAs for each snfital;
*Reference: snf_chars_formedstat.sas
http://www.nber.org/docs/hcris/transpose_alpha.sas
http://www.nber.org/docs/hcris/transpose_nmrc.sas;

libname old "/home/hcmg/kunhee/vertical-int/data/HCRIS";
libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loopalpha(fyear=,lyear=);
    %do year=&fyear. %to &lyear.;

        proc sort data=library.alpha&year. out=xx;
            by rpt_rec_num;

        proc print data=xx (obs=6);
            title "Year &year.";

        proc transpose data=xx out=library.alpha&year.t (drop = _NAME_);
            by rpt_rec_num;
            var alphnmrc_itm_txt;
            id varname;
        run;

        proc print data=library.alpha&year.t (obs=6);
        run;

    %end;
%mend;

%macro loopnmrc(fyear=,lyear=);
    %do year=&fyear. %to &lyear.;
        proc sort data=library.nmrc&year. out=xx;
            by rpt_rec_num;

        proc print data=xx (obs=6);

        proc transpose data=xx out=library.nmrc&year.t (drop = _NAME_);
            by rpt_rec_num;
            var itm_val_num;
            id varname;
        run;

        proc print data=library.nmrc&year.t (obs=6);
        run;
    %end;
%mend;

* merge;
%macro loop(fyear=,lyear=);

%do year=&fyear. %to &lyear.;

* Work with the RPT file first;
%let rpt = rpt&year.;

data &rpt.;
    set old.snf_rpt2540_10_&year.;

    *restrict to snfitals where Bayada operates;
    if prvdr_num eq "" then delete;
    *ownership status;
    length own_np 3.;
    own_np=0;
    length own_fp 3.;
    own_fp=0;
    length own_gv 3.;
    own_gv=0;
    if (prvdr_ctrl_type_cd>=1 and prvdr_ctrl_type_cd<=2) or prvdr_ctrl_type_cd=6 then own_np=1;
    if (prvdr_ctrl_type_cd>=3 and prvdr_ctrl_type_cd<=5) then own_fp=1;
    if (prvdr_ctrl_type_cd>=7 and prvdr_ctrl_type_cd<=13) then own_gv=1;
    if prvdr_ctrl_type_cd=. then delete;
run;

* Get rid of duplicates by report id;
proc sort nodupkey;
    by rpt_rec_num;

data merged;
    merge library.alpha&year.t (sortedby=rpt_rec_num) &rpt. (in=recent);
    by rpt_rec_num;
    fyear=&year.;
    if recent;

    data hcris&year.;
        merge library.nmrc&year.t (sortedby=rpt_rec_num) merged (in=recent);
        by rpt_rec_num;
        if recent;
    run;

    proc sort data=hcris&year. out=library.snfcr&year. ;
        by prvdr_num rpt_rec_num;

    proc print data=library.snfcr&year. (obs=6);
    proc means data=library.snfcr&year. median  ;
    options nolabel;
    proc means data=library.snfcr&year. ;
    options label;
    proc contents data=library.snfcr&year.;

    *stat-transfer;
    x "qrsh -now no st snfcr&year..sas7bdat snfcr&year..dta -y";
%end;
%mend;

%loopalpha(fyear=2016,lyear=2016);
%loopnmrc(fyear=2016,lyear=2016);
%loop(fyear=2016,lyear=2016);

x "mv snfcr2016.dta snfcr2016_2540_10.dta";
