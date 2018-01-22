libname out "V:\Bundle Payment";

%macro inpp(infile);
PROC IMPORT OUT= analytic_file_&infile. 
            DATAFILE= "V:\Bundle Payment\raw files\BPCI_Analytic_File_&infile._508.xlsx" 
            DBMS=EXCEL REPLACE;
			
     GETNAMES=YES;
RUN;
data analytic_file_&infile.;
 set analytic_file_&infile.(rename=(ccn=_ccn));
 period="&infile";
/* if Participant^='';*/
 length ccn $10.;
 ccn=strip(_ccn);
 drop address _ccn;
run;
%mend;

%inpp(2013_Q4);
%inpp(2014_Q1);
%inpp(2014_Q2);
%inpp(2014_Q3);
%inpp(2015_Q1);
%inpp(2015_Q2);
%inpp(2015_Q3);
%inpp(2015_Q4);
%inpp(2016_Q1);
%inpp(2016_Q2);
%inpp(2016_Q3);
%inpp(2016_Q4);
%inpp(2017_Q1);
%inpp(2017_Q2);

/***************figure out 2014Q1 ids************************/
/*proc sql; create table ccn14q2 as select a.ccn, a.participant*/
/*from Analytic_file_2014_q2 as a where ccn^=.;*/
/*quit;*/
/*proc sql; create table ccn14q3 as select distinct a.Participant_Name, a.City, a.participant*/
/*from Analytic_file_2014_q3 as a ;*/
/*quit;*/
/*proc sort; by Participant_Name city participant; quit;*/
/*data qc;*/
/* set ccn14q3;*/
/*  by Participant_Name city participant;*/
/*  if first.city;*/
/*run;*/
/*proc sort data=Analytic_file_2014_q1; by ccn ; quit;*/
/*data Analytic_file_2014_q1_m;*/
/* merge Analytic_file_2014_q1(drop=participant) ccn14q3;*/
/* by ccn;*/
/*run;*/
/*proc sql; create table Analytic_file_2014_q1_m as select a.*, b.participant*/
/*from Analytic_file_2014_q1(drop=participant) as a,*/
/* ccn14q3 as b where a.ccn=b.ccn; */
/*quit;*/
/*proc sort; by ccn participant;quit;*/
/**/
/*data Analytic_file_2014_q1_f;*/
/* set Analytic_file_2014_q1_m Analytic_file_2014_q1(where=(participant^=''));*/
/*run;*/
/*******end***********/

data Analytic_file_2014_q2;
 set Analytic_file_2014_q2;
 if state in ("AL","AK","AZ","AR","CA","CO","CT","DC","DE") and ccn^='' and substr(ccn,1,1)^='0' then ccn="0"||ccn;
run;

data Analytic_file_2013_q4;
 set Analytic_file_2013_q4;
 if Participant^='';
 drop F:;
run;

data Analytic_file_2017_q1;
 set Analytic_file_2017_q1(rename=(Episode_Length=_Episode_Length Discount_Rate=_Discount_Rate));
 length Episode_Length $20. Discount_Rate $4.;
 Discount_Rate=put(_Discount_Rate,BEST12.); 
 Episode_Length=put(_Episode_Length,BEST12.);
 Episode_Length=_Episode_Length;
 Discount_Rate=_Discount_Rate;
 drop _Episode_Length _Discount_Rate;
run; 

data all;
 set  Analytic_file_2013_q4 Analytic_file_2014_q1  Analytic_file_2014_q2 Analytic_file_2014_q3
Analytic_file_2015_q1 Analytic_file_2015_q2 Analytic_file_2015_q3 Analytic_file_2015_q4
Analytic_file_2016_q1 Analytic_file_2016_q2 Analytic_file_2016_q3 Analytic_file_2016_q4
 Analytic_file_2017_q1 Analytic_file_2017_q2 ;
/* keep Participant Model Episode_PoP_Start_Date Episode_PoP_End_Date Period */
/* Episode_Status Episode_Name Precedence_Date;*/
 format Episode_PoP_Start_Date Episode_PoP_End_Date Precedence_Date DATE9.;
 /***********added on 6/7/17 for refining******************/
 if substr(Episode_Name,1,5)="Acute" then Episode_Name="Acute myocardial infarction";/*1*/
 else if Episode_Name in ("ACID generator or lead", "AICD generator or lead","Automatic implantable cardiac defibrillator generator or lead")
                                     then Episode_Name="AICD generator or lead";
 else if Episode_Name in ("Back & Neck Except Spinal Fusion", "Back & neck except spinal fusion")
                                     then Episode_Name="Back & neck except spinal fusion";
 else if Episode_Name in ("Complex non-Cervical spinal fusion", "Complex non-cervical spinal fusion ", "Complex not-cervical spinal fusion"
 , "Complex non-cervical spinal fusion ", "Complex Non-Cervical Spinal Fusion")
                                     then Episode_Name="Complex non-Cervical spinal fusion";
else if Episode_Name in ("Cervical Spinal Fusion", "Cervical spinal fusion")
                                     then Episode_Name="Cervical spinal fusion";
else if Episode_Name in ("Fractures femur and hip/pelvis", "Fractures of the femur and hip or pelvis")
                                     then Episode_Name="Fractures femur and hip/pelvis";
 else if Episode_Name in ("Hip & Femur procedures except major joint", "Hip & femur procedures except major joint")
                                     then Episode_Name="Hip & femur procedures except major joint";
 else if Episode_Name in ("Hip & Femur procedures except major joint", "Hip & femur procedures except major joint")
                                     then Episode_Name="Hip & femur procedures except major joint";
 else if Episode_Name in ("Major Cardiovascular procedure", "Major cardiovascular procedure")
                                     then Episode_Name="Major cardiovascular procedure";
 else if Episode_Name in ("Major Joint replacement of the lower extremity", "Major joint replacement of the lower extremity")
                                     then Episode_Name="Major joint replacement of the lower extremity";
 else if Episode_Name in ("Medical non-infectious orthopedic", "Medical non-infectious orthopedic ")
                                     then Episode_Name="Medical non-infectious orthopedic";
else if Episode_Name in ("Pacemaker device replacement or revision", "Pacemaker Device replacement or revision")
                                     then Episode_Name="Pacemaker device replacement or revision";
else if Episode_Name in ("Red Blood cell disorders", "Red blood cell disorders")
                                     then Episode_Name="Red blood cell disorders";
else if Episode_Name in ("Spinal fusion (non-Cervical)", "Spinal fusion (non-cervical)")
                                     then Episode_Name="Spinal fusion (non-cervical)";
else if Episode_Name in ("revision of the hip or knee", "Revision of the hip or knee")
                                     then Episode_Name="Revision of the hip or knee";
if Episode_Status="blank" then Episode_Status='';
 /**********end******************/
 if participant ^='';
 proc sort; by model participant Period Episode_PoP_Start_Date Episode_PoP_End_Date Episode_Name;
run; 

data model1 model2 model3 model4;
 set all;
 if model=1 then output model1;
 if model=2 then output model2;
 if model=3 then output model3;
 if model=4 then output model4;
run;

%macro models(num);
proc sort data=model&num.; by Episode_Status Episode_Name Participant Episode_PoP_Start_Date Episode_PoP_End_Date;
quit;
data out.model&num._1;
 set model&num.;
 by Episode_Status Episode_Name Participant Episode_PoP_Start_Date Episode_PoP_End_Date;
 if last.Participant;
 proc sort; by participant Episode_Name Episode_Status; 
run;

%mend;

%models(1);
%models(2);
%models(3);
%models(4);
/**/
/*%let d=drop=Episode_length Discount_rate;*/
/*data all2;*/
/* set */
/*Analytic_file_2015_q1(&d.) Analytic_file_2015_q2(&d.) Analytic_file_2015_q3(&d.) Analytic_file_2015_q4(&d.)*/
/*Analytic_file_2016_q1(&d.) Analytic_file_2016_q2(&d.) Analytic_file_2016_q3(&d.) Analytic_file_2016_q4(&d.)*/
/* Analytic_file_2017_q1(&d.) Analytic_file_2017_q2(&d.) ;*/
/*/* keep Participant Model Episode_PoP_Start_Date Episode_PoP_End_Date Period */*/
/*/* Episode_Status Episode_Name Precedence_Date;*/*/
/* format Episode_PoP_Start_Date Episode_PoP_End_Date Precedence_Date DATE9.;*/
/* if participant ^='';*/
/* proc sort; 
/*by model participant Period Episode_PoP_Start_Date Episode_PoP_End_Date Episode_Name;*/
/*run; */
/**/
/*data Cvalve4;*/
/* set all2;*/
/* where Episode_Name in ("All DRGs", "Cardiac valve") and episode_status="Phase II";*/
/* proc sort; 
/*by participant participant_name city period; */
/*quit;*/
/*run;*/
/*data Cvalve5;*/
/* set Cvalve4;*/
/* by participant participant_name city period; */
/* if last.city;*/
/*run;*/
/**/
/*data Cvalve6;*/
/* set Cvalve5;*/
/* where ccn^="";*/
/*run;*/

data allc;
 set  Analytic_file_2013_q4 Analytic_file_2014_q1  Analytic_file_2014_q2 Analytic_file_2014_q3
Analytic_file_2015_q1 Analytic_file_2015_q2 Analytic_file_2015_q3 Analytic_file_2015_q4
Analytic_file_2016_q1 Analytic_file_2016_q2 Analytic_file_2016_q3 Analytic_file_2016_q4
 Analytic_file_2017_q1 Analytic_file_2017_q2 ;
 format Episode_PoP_Start_Date Episode_PoP_End_Date Precedence_Date DATE9.;
 if ccn^='';
  /***********added on 6/7/17 for refining******************/
 if substr(Episode_Name,1,5)="Acute" then Episode_Name="Acute myocardial infarction";/*1*/
 else if Episode_Name in ("ACID generator or lead", "AICD generator or lead","Automatic implantable cardiac defibrillator generator or lead")
                                     then Episode_Name="AICD generator or lead";
 else if Episode_Name in ("Back & Neck Except Spinal Fusion", "Back & neck except spinal fusion")
                                     then Episode_Name="Back & neck except spinal fusion";
 else if Episode_Name in ("Complex non-Cervical spinal fusion", "Complex non-cervical spinal fusion ", "Complex not-cervical spinal fusion"
 , "Complex non-cervical spinal fusion ", "Complex Non-Cervical Spinal Fusion")
                                     then Episode_Name="Complex non-Cervical spinal fusion";
else if Episode_Name in ("Cervical Spinal Fusion", "Cervical spinal fusion")
                                     then Episode_Name="Cervical spinal fusion";
else if Episode_Name in ("Fractures femur and hip/pelvis", "Fractures of the femur and hip or pelvis")
                                     then Episode_Name="Fractures femur and hip/pelvis";
 else if Episode_Name in ("Hip & Femur procedures except major joint", "Hip & femur procedures except major joint")
                                     then Episode_Name="Hip & femur procedures except major joint";
 else if Episode_Name in ("Hip & Femur procedures except major joint", "Hip & femur procedures except major joint")
                                     then Episode_Name="Hip & femur procedures except major joint";
 else if Episode_Name in ("Major Cardiovascular procedure", "Major cardiovascular procedure")
                                     then Episode_Name="Major cardiovascular procedure";
 else if Episode_Name in ("Major Joint replacement of the lower extremity", "Major joint replacement of the lower extremity")
                                     then Episode_Name="Major joint replacement of the lower extremity";
 else if Episode_Name in ("Medical non-infectious orthopedic", "Medical non-infectious orthopedic ")
                                     then Episode_Name="Medical non-infectious orthopedic";
else if Episode_Name in ("Pacemaker device replacement or revision", "Pacemaker Device replacement or revision")
                                     then Episode_Name="Pacemaker device replacement or revision";
else if Episode_Name in ("Red Blood cell disorders", "Red blood cell disorders")
                                     then Episode_Name="Red blood cell disorders";
else if Episode_Name in ("Spinal fusion (non-Cervical)", "Spinal fusion (non-cervical)")
                                     then Episode_Name="Spinal fusion (non-cervical)";
else if Episode_Name in ("revision of the hip or knee", "Revision of the hip or knee")
                                     then Episode_Name="Revision of the hip or knee";

if Episode_Status="blank" then Episode_Status='';
 /**********end******************/
 proc sort; by model ccn Period Episode_PoP_Start_Date Episode_PoP_End_Date Episode_Name;
run; 
/*data allc;*/
/* set  out.Analytic_file_2013_q4 out.Analytic_file_2014_q1  out.Analytic_file_2014_q2 out.Analytic_file_2014_q3*/
/*out.Analytic_file_2015_q1 out.Analytic_file_2015_q2 out.Analytic_file_2015_q3 out.Analytic_file_2015_q4*/
/*out.Analytic_file_2016_q1 out.Analytic_file_2016_q2 out.Analytic_file_2016_q3 out.Analytic_file_2016_q4*/
/* out.Analytic_file_2017_q1 out.Analytic_file_2017_q2 ;*/
/* format Episode_PoP_Start_Date Episode_PoP_End_Date Precedence_Date DATE9.;*/
/* if ccn^='';*/
/* proc sort; by model ccn Period Episode_PoP_Start_Date Episode_PoP_End_Date Episode_Name;*/
/*run; */

data model1c model2c model3c model4c;
 set allc;
 if model=1 then output model1c;
 if model=2 then output model2c;
 if model=3 then output model3c;
 if model=4 then output model4c;
run;

%macro modelsc(num);
proc sort data=model&num.c; by Episode_Status Episode_Name ccn Episode_PoP_Start_Date Episode_PoP_End_Date;
quit;
data out.model&num._1c;
 set model&num.c;
 by Episode_Status Episode_Name ccn Episode_PoP_Start_Date Episode_PoP_End_Date;

 if last.ccn;
 proc sort; by ccn Episode_Name Episode_Status; 
run;

%mend;

%modelsc(1);
%modelsc(2);
%modelsc(3);
%modelsc(4);

data out.all_par;
 set out.model1_1 out.model2_1 out.model3_1 out.model4_1;
 proc sort; by model participant Episode_Name Episode_Status; quit;
run;


data out.all_ccn;
 set out.model1_1c out.model2_1c out.model3_1c out.model4_1c;
 proc sort; by model ccn Episode_Name Episode_Status; quit;
run;

%macro ext(dat);
PROC EXPORT DATA= OUT.&dat. 
            OUTFILE= "V:\Bundle Payment\&dat..xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="aa"; 
RUN;
%mend;

%ext(model1_1c);
%ext(model2_1c);
%ext(model3_1c);
%ext(model4_1c);
%ext(model1_1);
%ext(model2_1);
%ext(model3_1);
%ext(model4_1);
%ext(all_par);
%ext(all_ccn);

/*************more refining on June 7th,2017*********************************/

proc freq data=all;
 table Episode_Name model episode_status;
quit;
proc freq data=allc;
 table Episode_Name model episode_status;
quit;

