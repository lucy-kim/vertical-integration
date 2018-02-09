options nocenter ;
*Download & unzip the CMS HRIS hospital cost report data for each year;

%macro download(t1, t2, firstyr, lastyr);
  %do year = &firstyr. %to &lastyr.;
    %do qtr = &t1. %to &t2.;
      * Get files;
      x "cd /ifs/home/kimk13/VI/data/Medicare/BPCI";
      x "wget -nv -N https://innovation.cms.gov/Files/x/bpci-analyticfile-q&qtr.&year..xlsx";
      x "wget -nv -N https://innovation.cms.gov/Files/x/bpci-analyticfile-q&qtr.&year..zip";
      x "wget -nv -N https://innovation.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..zip";
      x "wget -nv -N https://innovation.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..xlsx";
      x "wget -nv -N https://innovation.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..xlsx";

      x "wget -nv -N https://downloads.cms.gov/Files/x/bpci-analyticfile-q&qtr.&year..xlsx";
      x "wget -nv -N https://downloads.cms.gov/Files/x/bpci-analyticfile-q&qtr.&year..zip";
      x "wget -nv -N https://downloads.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..zip";
      x "wget -nv -N https://downloads.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..xlsx";
      x "wget -nv -N https://downloads.cms.gov/files/cmmi/bpci-analyticfile-q&qtr.&year..xlsx";
    %end;
  %end;
  %mend download;

  %download(1,4,2014,2016);
  %download(4,4,2013,2013);
  %download(1,2,2017,2017);

*one file has a unique pattern;
  x "wget -nv -N https://downloads.cms.gov/files/cmmi/bpci-analyticfile-q12015v2.zip"

*unzip if zip file;
x "unzip bpci-analyticfile-q22017.zip";
x "unzip unzip bpci-analyticfile-q12015v2.zip";
x "unzip bpci-analyticfile-q42014.zip";

*rename unzipped files;
x "mv BPCI_Analytic_File_2014_Q4_508.xlsx bpci-analyticfile-q42014.xlsx";
x "mv BPCI_Analytic_File_2015_Q1_V2_508.xlsx bpci-analyticfile-q12015.xlsx";
x "mv BPCI_Analytic_File_2017_Q2_508.xlsx bpci-analyticfile-q22017.xlsx";
x "rm -f *zip";
