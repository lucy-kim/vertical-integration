options nocenter ;
*Read HHA cost report RPT file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt.sas ;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

PROC FORMAT CNTLOUT=library.fhha_rpt;
**************************************************;
** The PROC FORMAT statements will store the formats in a sas data set;
** Use the stored formats in subsequent programs like this: ;
** proc format cntlin=library.fhha_rpt;
** PROC freq;
**        tables adr_vndr_cd util_cd ;
**        format adr_vndr_cd adr_vndr_cd.;
**        format util_cd $util_cd.;
** Consult the SAS Procedures Guide PROC FORMAT section;
**************************************************;
    VALUE $prvdr_ctrl_type_cd
       "1"     =  "Voluntary Nonprofit, Church"
       "2"     =  "Voluntary Nonprofit, Other"
       "3"     =  "Proprietary, Sole Proprietor"
       "4"     =  "Proprietary, Partnership"
       "5"     =  "Proprietary, Corporation"
       "6"     =  "Private, Non-profit"
       "7"     =  "Governmental & Private Combination"
       "8"     =  "Governmental, Federal"
       "9"     =  "Governmental, State"
       "10"    =  "Governmental, City"
       "11"    =  "Governmental, City-County"
       "12"    =  "Governmental, County"
       "13"    =  "Governmental Hospital District"
;
VALUE $rpt_stus_cd
       "1"     =  "As Submitted"
       "2"     =  "Settled w/o Audit"
       "3"     =  "Settled with Audit"
       "4"     =  "Reopened"
       "5"     =  "Amended"
;
VALUE $initl_rpt_sw
       "Y"   =  "first cost report filed for this provider"
       "N"   =  "2nd+ report for this provider"
;
VALUE $last_rpt_sw
       "Y"   =  "last cost report filed for this provider"
       "N"   =  "not last report for this provider"
;
VALUE $adr_vndr_cd
       "2"     =  "E & Y"
       "3"     =  "KPMG"
       "4"     =  "HFS"
;
VALUE $util_cd
       "L"   =  "Low Medicare Util"
       "N"   =  "No Medicare Util"
       "F"   =  "Full Medicare Util"
       ""    =  "Full Medicare Util"
;

%macro loop;
%do year=2016 %to 2016;
    proc printto log  ="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt&year..log";
    proc printto print="/home/hcmg/kunhee/vertical-int/data/HCRIS/read_hha_rpt&year..lst";

    FILENAME datafile "/home/hcmg/kunhee/vertical-int/data/HCRIS/hha_rpt1728_94_&year..csv";


    %let dataset=library.hha_rpt&year.;

    data &dataset.;
        LENGTH
            rpt_rec_num 6 prvdr_ctrl_type_cd $2 prvdr_num $6
            rpt_stus_cd $1 initl_rpt_sw $1 last_rpt_sw $1
            trnsmtl_num $2 fi_num $5 adr_vndr_cd $1 util_cd $1 spec_ind $1
            default = 4;
        infile datafile dsd delimiter='2C0D'x  ;
**  the ":" is a format modifier that reads data values
    that need additional instructions from an informat;
        INPUT
            rpt_rec_num
            prvdr_ctrl_type_cd $
            prvdr_num $
            npi
            rpt_stus_cd $
            fy_bgn_dt : mmddyy10.
            fy_end_dt : mmddyy10.
            proc_dt : mmddyy10.
            initl_rpt_sw $
            last_rpt_sw $
            trnsmtl_num $
            fi_num $
            adr_vndr_cd $
            fi_creat_dt : mmddyy10.
            util_cd $
            npr_dt : mmddyy10.
            spec_ind $
            fi_rcpt_dt : mmddyy10.
            ;
        LABEL
            rpt_rec_num="Report Record Number"
            prvdr_ctrl_type_cd ="Provider Control Type Code"
            prvdr_num ="Provider Number"
            npi="National Provider Identifier"
            rpt_stus_cd="Report Status Code"
            fy_bgn_dt="Fiscal Year Begin Date"
            fy_end_dt="Fiscal Year End Date"
            proc_dt  ="HCRIS Process Date"
            initl_rpt_sw="Initial Report Switch"
            last_rpt_sw="Last Report Switch"
            trnsmtl_num="Transmittal Number"
            fi_num ="Fiscal Intermediary Number"
            adr_vndr_cd="Automated Desk Review Vendor Code"
            fi_creat_dt="Fiscal Intermediary Create Date"
            util_cd    ="Utilization Code"
            npr_dt     ="Notice of Program Reimbursement Date"
            spec_ind="Special Indicator"
            fi_rcpt_dt="Fiscal Intermediary Receipt Date"
            ;
        FORMAT
            fy_bgn_dt   MMDDYYS10.
            fy_end_dt   MMDDYYS10.
            proc_dt     MMDDYYS10.
            fi_creat_dt MMDDYYS10.
            npr_dt      MMDDYYS10.
            fi_rcpt_dt  MMDDYYS10.
            ;
    proc sort data=&dataset.;
        by rpt_rec_num;

    proc means DATA=&dataset. max n;
        title "Inspect maximums:  Using a length of 4 bytes (default) retains 6 significant digits" ;
    proc freq DATA=&dataset. ;
        title "" ;
        tables prvdr_ctrl_type_cd rpt_stus_cd initl_rpt_sw last_rpt_sw trnsmtl_num fi_num adr_vndr_cd util_cd spec_ind ;
        FORMAT
            prvdr_ctrl_type_cd $prvdr_ctrl_type_cd.
            rpt_stus_cd $rpt_stus_cd.
            initl_rpt_sw $initl_rpt_sw.
            last_rpt_sw $last_rpt_sw.
            adr_vndr_cd $adr_vndr_cd.
            util_cd $util_cd.
            ;
    proc print DATA=&dataset. (obs=6);
    proc contents DATA=&dataset.;

        %end;
%mend;

%loop;
