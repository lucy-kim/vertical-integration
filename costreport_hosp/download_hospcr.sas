options nocenter ;
*by Kunhee Kim, kunhee@wharton.upenn.edu, 2015-03-11 ;
*Download & unzip the CMS HRIS hospital cost report data for 2011-2015;

%macro downloadhospcr(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_rpt2552_10_&year..csv";
x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_nmrc2552_10_&year._long.csv";
x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_alpha2552_10_&year._long.csv";

%end;
%mend downloadhospcr;

%downloadhospcr(2011,2016);
