options nocenter ;
*Download & unzip the CMS Provider of Services data for each year;

%macro download(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/vertical-int";
x "wget -nv -N http://www.nber.org/pos/&year./pos&year..dta.zip";

%end;
%mend download;

%download(2016,2016);
