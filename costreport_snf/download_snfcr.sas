options nocenter ;
*Download & unzip the CMS HRIS snf cost report data for each year;

%macro download(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

x "wget -nv -N http://downloads.cms.gov/Files/hcris/snf10FY&year..zip";
x "unzip -u snf10FY&year..zip snf10_&year._ALPHA.CSV snf10_&year._NMRC.CSV snf10_&year._ROLLUP.CSV snf10_&year._RPT.CSV";

*change filenames;
x "mv snf10_&year._RPT.CSV snf_rpt2540_10_&year..csv";
x "mv snf10_&year._NMRC.CSV snf_nmrc2540_10_&year._long.csv";
x "mv snf10_&year._ALPHA.CSV snf_alpha2540_10_&year._long.csv";
x "rm -f snf10_&year._ROLLUP.CSV";

%end;
%mend download;

%download(2016,2016);

* NBER CR;
/* x "wget -nv -N http://www.nber.org/hcris/2540-10/snf_rpt2540_10_&year..csv";
x "wget -nv -N http://www.nber.org/hcris/2540-10/snf_nmrc2540_10_&year._long.csv";
x "wget -nv -N http://www.nber.org/hcris/2540-10/snf_alpha2540_10_&year._long.csv"; */
