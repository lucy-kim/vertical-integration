options nocenter;

options linesize=70;
*Extract some variables from the HHA NMRC file from CMS HHA cost report data to construct the HHA cost report files for each HHA and for FY 1994-2014 (apply special conditions to 2000 since PPS effective on 10/01/2000);
*Revised extract_hha_nmrc.sas to extract salary for more refined categories: e.g. HH aide salary for supervisors and for aides using Wksh A-1
*Reference: hosp_chars_formedstat.sas, extract_hha_alpha.sas
             http://www.nber.org/docs/hcris/extract_nmrc2552-10.sas;

libname library "/home/hcmg/kunhee/vertical-int/data/HCRIS";
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

%macro loop(fyear=,lyear=);

    %do year=&fyear. %to &lyear.;

        %let dataset=nmrc&year.;

        proc sql;
        *select appropriate worksheets;
        create table &dataset. as
            select *
            from library.hha_nmrc_rpt&year._long
            where  (
               (wksht_cd='S200000' and line_num='00100' and clmn_num='0100')
            or (wksht_cd='S200000' and line_num='00101' and clmn_num='0100')
            or (wksht_cd='S200000' and line_num='00101' and clmn_num='0200')
            or (wksht_cd='S200000' and line_num='00101' and clmn_num='0300')

            or (wksht_cd='S200000' and line_num='00200' and clmn_num='0100')
            or (wksht_cd='S200000' and line_num='00200' and clmn_num='0200')
            or (wksht_cd='S200000' and line_num='00200' and clmn_num='0300')

            or (wksht_cd='S200000' and line_num='01800' and clmn_num='0100')
            or (wksht_cd='S200000' and line_num='02000' and clmn_num='0100')

            or wksht_cd='S300000'

            or (wksht_cd='A000000' and line_num='00600')
            or (wksht_cd='A000000' and line_num='00700')
            or (wksht_cd='A000000' and line_num='00800')
            or (wksht_cd='A000000' and line_num='00900')
            or (wksht_cd='A000000' and line_num='01000')
            or (wksht_cd='A000000' and line_num='01100')
            or (wksht_cd='A000000' and line_num='01200')
            or (wksht_cd='A000000' and line_num='01300')
            or (wksht_cd='A000000' and line_num='01320')
            or (wksht_cd='A000000' and line_num='01400')

            or (wksht_cd='A100000' and line_num='00600')
            or (wksht_cd='A100000' and line_num='00700')
            or (wksht_cd='A100000' and line_num='00800')
            or (wksht_cd='A100000' and line_num='00900')
            or (wksht_cd='A100000' and line_num='01000')
            or (wksht_cd='A100000' and line_num='01100')
            )
            ;

        * Add varname;
        data library.&dataset.;
            set &dataset.;
            length varname $32. ;
            ;                                                                            *32-character varname maximum;
            select;                                                                      *12345678901234567892123456789312;
            when (wksht_cd='S200000' and line_num='00100' and clmn_num='0100') varname = "street" ;
            when (wksht_cd='S200000' and line_num='00101' and clmn_num='0100') varname = "city";
            when (wksht_cd='S200000' and line_num='00101' and clmn_num='0200') varname = "state";
            when (wksht_cd='S200000' and line_num='00101' and clmn_num='0300') varname = "zip";
            when (wksht_cd='S200000' and line_num='00200' and clmn_num='0100') varname = "hhaname";
            when (wksht_cd='S200000' and line_num='00200' and clmn_num='0200') varname = "prov_num";
            when (wksht_cd='S200000' and line_num='00200' and clmn_num='0300') varname = "dt_certf";
            when (wksht_cd='S200000' and line_num='01800' and clmn_num='0100') varname = "cease";
            when (wksht_cd='S200000' and line_num='02000' and clmn_num='0100') varname = "smhha";

            * Number of visits & patients by payer;

            * Medicare ;
            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0100') varname = "sn_visit_mcr";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0100') varname = "pt_visit_mcr";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0100') varname = "ot_visit_mcr";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0100') varname = "sp_visit_mcr";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0100') varname = "mss_visit_mcr";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0100') varname = "aide_visit_mcr";
            when (wksht_cd='S300000' and line_num='00900' and clmn_num='0100') varname = "aide_hrs_mcr";

            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0200') varname = "sn_pat_mcr";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0200') varname = "pt_pat_mcr";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0200') varname = "ot_pat_mcr";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0200') varname = "sp_pat_mcr";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0200') varname = "mss_pat_mcr";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0200') varname = "aide_pat_mcr";

            * Other payers;
            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0300') varname = "sn_visit_oth";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0300') varname = "pt_visit_oth";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0300') varname = "ot_visit_oth";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0300') varname = "sp_visit_oth";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0300') varname = "mss_visit_oth";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0300') varname = "aide_visit_oth";
            when (wksht_cd='S300000' and line_num='00900' and clmn_num='0300') varname = "aide_hrs_oth";
            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0400') varname = "sn_pat_oth";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0400') varname = "pt_pat_oth";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0400') varname = "ot_pat_oth";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0400') varname = "sp_pat_oth";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0400') varname = "mss_pat_oth";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0400') varname = "aide_pat_oth";

            * All payers;
            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0500') varname = "sn_visit_all";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0500') varname = "pt_visit_all";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0500') varname = "ot_visit_all";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0500') varname = "sp_visit_all";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0500') varname = "mss_visit_all";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0500') varname = "aide_visit_all";
            when (wksht_cd='S300000' and line_num='00900' and clmn_num='0500') varname = "aide_hrs_all";
            when (wksht_cd='S300000' and line_num='00100' and clmn_num='0600') varname = "sn_pat_all";
            when (wksht_cd='S300000' and line_num='00200' and clmn_num='0600') varname = "pt_pat_all";
            when (wksht_cd='S300000' and line_num='00300' and clmn_num='0600') varname = "ot_pat_all";
            when (wksht_cd='S300000' and line_num='00400' and clmn_num='0600') varname = "sp_pat_all";
            when (wksht_cd='S300000' and line_num='00500' and clmn_num='0600') varname = "mss_pat_all";
            when (wksht_cd='S300000' and line_num='00600' and clmn_num='0600') varname = "aide_pat_all";
            when (wksht_cd='S300000' and line_num='00900' and clmn_num='0600') varname = "aide_hrs_all";

            when (wksht_cd='S300000' and line_num='01000' and clmn_num='0200') varname = "undup_pat_cnt_mcr";
            when (wksht_cd='S300000' and line_num='01000' and clmn_num='0400') varname = "undup_pat_cnt_oth";
            when (wksht_cd='S300000' and line_num='01000' and clmn_num='0600') varname = "undup_pat_cnt_all";

            * Number of hours in normal work week for staff, contract, & total;

            * Direct nursing service;
            when (wksht_cd='S300000' and line_num='01400' and clmn_num='0100') varname = "hrs_dns_staff";
            when (wksht_cd='S300000' and line_num='01400' and clmn_num='0200') varname = "hrs_dns_contract";
            when (wksht_cd='S300000' and line_num='01400' and clmn_num='0300') varname = "hrs_dns_tot";

            * Nursing supervisor;
            when (wksht_cd='S300000' and line_num='01500' and clmn_num='0100') varname = "hrs_nsup_staff";
            when (wksht_cd='S300000' and line_num='01500' and clmn_num='0200') varname = "hrs_nsup_contract";
            when (wksht_cd='S300000' and line_num='01500' and clmn_num='0300') varname = "hrs_nsup_tot";

            * Physical Therapy service;
            when (wksht_cd='S300000' and line_num='01600' and clmn_num='0100') varname = "hrs_pts_staff";
            when (wksht_cd='S300000' and line_num='01600' and clmn_num='0200') varname = "hrs_pts_contract";
            when (wksht_cd='S300000' and line_num='01600' and clmn_num='0300') varname = "hrs_pts_tot";

            * Physical Therapy supervisor;
            when (wksht_cd='S300000' and line_num='01700' and clmn_num='0100') varname = "hrs_ptsup_staff";
            when (wksht_cd='S300000' and line_num='01700' and clmn_num='0200') varname = "hrs_ptsup_contract";
            when (wksht_cd='S300000' and line_num='01700' and clmn_num='0300') varname = "hrs_ptsup_tot";

            * Occupational Therapy service;
            when (wksht_cd='S300000' and line_num='01800' and clmn_num='0100') varname = "hrs_ots_staff";
            when (wksht_cd='S300000' and line_num='01800' and clmn_num='0200') varname = "hrs_ots_contract";
            when (wksht_cd='S300000' and line_num='01800' and clmn_num='0300') varname = "hrs_ots_tot";

            * Occupational Therapy supervisor;
            when (wksht_cd='S300000' and line_num='01900' and clmn_num='0100') varname = "hrs_otsup_staff";
            when (wksht_cd='S300000' and line_num='01900' and clmn_num='0200') varname = "hrs_otsup_contract";
            when (wksht_cd='S300000' and line_num='01900' and clmn_num='0300') varname = "hrs_otsup_tot";

            * Speech Pathology service;
            when (wksht_cd='S300000' and line_num='02000' and clmn_num='0100') varname = "hrs_sps_staff";
            when (wksht_cd='S300000' and line_num='02000' and clmn_num='0200') varname = "hrs_sps_contract";
            when (wksht_cd='S300000' and line_num='02000' and clmn_num='0300') varname = "hrs_sps_tot";

            * Speech Pathology supervisor;
            when (wksht_cd='S300000' and line_num='02100' and clmn_num='0100') varname = "hrs_spsup_staff";
            when (wksht_cd='S300000' and line_num='02100' and clmn_num='0200') varname = "hrs_spsup_contract";
            when (wksht_cd='S300000' and line_num='02100' and clmn_num='0300') varname = "hrs_spsup_tot";

            * Medical Social service;
            when (wksht_cd='S300000' and line_num='02200' and clmn_num='0100') varname = "hrs_mss_staff";
            when (wksht_cd='S300000' and line_num='02200' and clmn_num='0200') varname = "hrs_mss_contract";
            when (wksht_cd='S300000' and line_num='02200' and clmn_num='0300') varname = "hrs_mss_tot";

            * Medical Social supervisor;
            when (wksht_cd='S300000' and line_num='02300' and clmn_num='0100') varname = "hrs_mssup_staff";
            when (wksht_cd='S300000' and line_num='02300' and clmn_num='0200') varname = "hrs_mssup_contract";
            when (wksht_cd='S300000' and line_num='02300' and clmn_num='0300') varname = "hrs_mssup_tot";

            * Home Health Aide;
            when (wksht_cd='S300000' and line_num='02400' and clmn_num='0100') varname = "hrs_aide_staff";
            when (wksht_cd='S300000' and line_num='02400' and clmn_num='0200') varname = "hrs_aide_contract";
            when (wksht_cd='S300000' and line_num='02400' and clmn_num='0300') varname = "hrs_aide_tot";

            * Home Health Aide supervisor;
            when (wksht_cd='S300000' and line_num='02500' and clmn_num='0100') varname = "hrs_aidesup_staff";
            when (wksht_cd='S300000' and line_num='02500' and clmn_num='0200') varname = "hrs_aidesup_contract";
            when (wksht_cd='S300000' and line_num='02500' and clmn_num='0300') varname = "hrs_aidesup_tot";
            * MSA & CBSA codes in which Medicare covered HH services were provided;

            when (wksht_cd='S300000' and line_num='02800' and clmn_num='0100') varname = "n_msa";
            when (wksht_cd='S300000' and line_num='02800' and clmn_num='0101') varname = "n_cbsa";

            when (wksht_cd='S300000' and line_num='02900' and clmn_num='0100') varname = "msa1";
            when (wksht_cd='S300000' and line_num='02901' and clmn_num='0100') varname = "msa2";
            when (wksht_cd='S300000' and line_num='02902' and clmn_num='0100') varname = "msa3";
            when (wksht_cd='S300000' and line_num='02903' and clmn_num='0100') varname = "msa4";
            when (wksht_cd='S300000' and line_num='02904' and clmn_num='0100') varname = "msa5";
            when (wksht_cd='S300000' and line_num='02905' and clmn_num='0100') varname = "msa6";
            when (wksht_cd='S300000' and line_num='02906' and clmn_num='0100') varname = "msa7";
            when (wksht_cd='S300000' and line_num='02907' and clmn_num='0100') varname = "msa8";
            when (wksht_cd='S300000' and line_num='02908' and clmn_num='0100') varname = "msa9";
            when (wksht_cd='S300000' and line_num='02909' and clmn_num='0100') varname = "msa10";

            when (wksht_cd='S300000' and line_num='02900' and clmn_num='0101') varname = "cbsa1";
            when (wksht_cd='S300000' and line_num='02901' and clmn_num='0101') varname = "cbsa2";
            when (wksht_cd='S300000' and line_num='02902' and clmn_num='0101') varname = "cbsa3";
            when (wksht_cd='S300000' and line_num='02903' and clmn_num='0101') varname = "cbsa4";
            when (wksht_cd='S300000' and line_num='02904' and clmn_num='0101') varname = "cbsa5";
            when (wksht_cd='S300000' and line_num='02905' and clmn_num='0101') varname = "cbsa6";
            when (wksht_cd='S300000' and line_num='02906' and clmn_num='0101') varname = "cbsa7";
            when (wksht_cd='S300000' and line_num='02907' and clmn_num='0101') varname = "cbsa8";
            when (wksht_cd='S300000' and line_num='02908' and clmn_num='0101') varname = "cbsa9";
            when (wksht_cd='S300000' and line_num='02909' and clmn_num='0101') varname = "cbsa10";

            * Number of episodes for each type of service under PPS;

            when (wksht_cd='S300000' and line_num='03000' and clmn_num='0700') varname = "sn_visit_tot";
            when (wksht_cd='S300000' and line_num='03100' and clmn_num='0700') varname = "sn_chrg_tot";
            when (wksht_cd='S300000' and line_num='03200' and clmn_num='0700') varname = "pt_visit_tot";
            when (wksht_cd='S300000' and line_num='03300' and clmn_num='0700') varname = "pt_chrg_tot";
            when (wksht_cd='S300000' and line_num='03400' and clmn_num='0700') varname = "ot_visit_tot";
            when (wksht_cd='S300000' and line_num='03500' and clmn_num='0700') varname = "ot_chrg_tot";
            when (wksht_cd='S300000' and line_num='03600' and clmn_num='0700') varname = "sp_visit_tot";
            when (wksht_cd='S300000' and line_num='03700' and clmn_num='0700') varname = "sp_chrg_tot";
            when (wksht_cd='S300000' and line_num='03800' and clmn_num='0700') varname = "mss_visit_tot";
            when (wksht_cd='S300000' and line_num='03900' and clmn_num='0700') varname = "mss_chrg_tot";
            when (wksht_cd='S300000' and line_num='04000' and clmn_num='0700') varname = "aide_visit_tot";
            when (wksht_cd='S300000' and line_num='04100' and clmn_num='0700') varname = "aide_chrg_tot";
            when (wksht_cd='S300000' and line_num='04500' and clmn_num='0700') varname = "tot_n_epi_tot";
            * Pay to workers working for HHA reimbursable services;

            * Skilled nursing care;
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0100') varname = "sn_salary";
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0200') varname = "sn_benefit";
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0300') varname = "sn_transp";
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0400') varname = "sn_purchase";
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0500') varname = "sn_othercost";
            when (wksht_cd='A000000' and line_num='00600' and clmn_num='0600') varname = "sn_tot";

            * Physical therapy;
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0100') varname = "pt_salary";
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0200') varname = "pt_benefit";
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0300') varname = "pt_transp";
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0400') varname = "pt_purchase";
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0500') varname = "pt_othercost";
            when (wksht_cd='A000000' and line_num='00700' and clmn_num='0600') varname = "pt_tot";

            * Occupational therapy;
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0100') varname = "ot_salary";
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0200') varname = "ot_benefit";
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0300') varname = "ot_transp";
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0400') varname = "ot_purchase";
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0500') varname = "ot_othercost";
            when (wksht_cd='A000000' and line_num='00800' and clmn_num='0600') varname = "ot_tot";

            * Speech pathology;
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0100') varname = "sp_salary";
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0200') varname = "sp_benefit";
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0300') varname = "sp_transp";
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0400') varname = "sp_purchase";
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0500') varname = "sp_othercost";
            when (wksht_cd='A000000' and line_num='00900' and clmn_num='0600') varname = "sp_tot";

            * Medical Social Services;
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0100') varname = "mss_salary";
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0200') varname = "mss_benefit";
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0300') varname = "mss_transp";
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0400') varname = "mss_purchase";
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0500') varname = "mss_othercost";
            when (wksht_cd='A000000' and line_num='01000' and clmn_num='0600') varname = "mss_tot";

            * Home Health Aide;
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0100') varname = "aide_salary";
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0200') varname = "aide_benefit";
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0300') varname = "aide_transp";
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0400') varname = "aide_purchase";
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0500') varname = "aide_othercost";
            when (wksht_cd='A000000' and line_num='01100' and clmn_num='0600') varname = "aide_tot";

            * More refined-group level salary info;
            otherwise;
            end;

        if varname = "" then delete;
      run;
      proc contents;
        proc print data=library.&dataset (obs=50);
          proc freq data=library.&dataset;
            table varname;
          %end;
        %mend;

%loop(fyear=2016,lyear=2016);
