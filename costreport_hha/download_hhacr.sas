options nocenter ;
*Download & unzip the CMS HRIS hha cost report data for each year;

%macro download(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/vertical-int/data/HCRIS";

x "wget -nv -N http://downloads.cms.gov/Files/hcris/hhaFY&year..zip";
x "unzip -u hhaFY&year..zip hha_&year._ALPHA.CSV hha_&year._NMRC.CSV hha_&year._ROLLUP.CSV hha_&year._RPT.CSV";

*change filenames;
x "mv hha_&year._RPT.CSV hha_rpt1728_94_&year..csv";
x "mv hha_&year._NMRC.CSV hha_nmrc1728_94_&year._long.csv";
x "mv hha_&year._ALPHA.CSV hha_alpha1728_94_&year._long.csv";
x "rm -f hha_&year._ROLLUP.CSV";

%end;
%mend download;

%download(2016,2016);

/*
*NBER CR;
x "wget -nv -N http://www.nber.org/hcris/1728-94/hha_rpt1728_94_&year..csv
";
x "wget -nv -N http://www.nber.org/hcris/1728-94/hha_nmrc1728_94_&year._long.csv";
x "wget -nv -N http://www.nber.org/hcris/1728-94/hha_alpha1728_94_&year._long.csv
"; */
