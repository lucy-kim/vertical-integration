options nocenter ;
*Download & unzip the CMS Provider of Services data for each year;

%macro download(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /ifs/home/kimk13/VI/data/pos";
x "wget -nv -N http://www.nber.org/pos/&year./pos&year..dta.zip";
x "unzip pos&year..dta.zip";
x "rm -f *zip";

%end;
%mend download;

%download(2005,2016);
