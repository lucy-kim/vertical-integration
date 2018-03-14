# vertical-integration
Research on the vertical integration between hospital and post-acute care (PAC)

## Use the CMS Hospital Cost Report data to obtain hospital characteristics

1. Figure out which variables you want from the Cost Reports and where they are. This [website](https://www.costreportdata.com/worksheet_formats.html) is useful to navigate which worksheet to refer to for specific variables (see 2010 format for years 2011-current, and 1996 format for previous years). Locate 1) worksheet, 2) line number, 3) column number that contain the variable of your interest.

2. Write SAS or Stata codes to extract data from those specific locations. The [NBER HCRIS data](http://www.nber.org/data/hcris.html) page provides template codes as well as raw data. My codes are adapted from those.

We share my codes that achieve creating hospital-level panel data for 2011-2016 from the cost reports (as of 8/23/16, 2016 seems incomplete). They contain only the select variables of my interest. To run these codes,

1. Set the working directory to the cloned repository directory or wherever the following bash script file is.
```
cd vertical-integration/costreport_hosp
```
2. Run the following shell script that contains all the codes from downloading raw data to extracting relevant variables, and creating a final panel data file in the Stata format
```bash
qsub hospcr.sh
```
Note: Change the working directory inside individual code files.

## Setting up aggregate Medicare data
We use hospital-month-condition level index admissions data (for the hospital readmissions penalty) and hospital-month-condition-PAC provider level referral data (where referrals are defined as starting PAC within 5 days from hospital discharge).

1. `crindex_admit_chm.do`
  - Import hospital-month-condition level index admissions data
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)
2. `crPACreferral_tchpm.do`
  - Import hospital-month-condition-PAC provider level referral data separately for HHA and SNF
  - HHA data available for 2010/7 - 2016/6 except 2012/6 (just not avail.)
  - SNF data available for 2008/1 - 2016/6 except 2012/6 (just not avail.)
4. `crindex_admit_DRG_chy.do`
  - Create hospital-FY-condition-DRG level index admissions data
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)
5. `crinpat_pmt_hosp_fy_drg.do`
  - Create hospital-FY-condition level data on Medicare inpatient payment by combining the DRG-level counts from our internal Medicare data with the public DRG-level average payment payment data from CMS
6. `hospcr.sh` in `costreport_hosp` directory
  - Get hospital characteristics and total patient revenues for each FY from CMS Cost Report data
7. `crpac_mkt_hhi.do`
  - Create PAC market concentration (HHI) at the hospital HRR / HSA level using the referral data created from the Medicare claims data (exclude the hospitals' own referrals when calculating the HHI)
1. `crhosp_compare.do`
  - Use hospital compare data for hospital level risk-adjusted readmission rate data during a 3-year window period


## Describe the trend of hospital-PAC vertical integration
1. `desc_trend_VI.do`
  - descriptive analysis of the trend of vertical integration over time
2. `desc_trend_VI2.do`


## Examine the impact of hospital readmissions penalty on hospital-PAC vertical integration
3. `crhosp_fy_VI.do`
  - Create hospital-FY level data containing total admission volume, PAC referral concentration, hospital characteristics
  - hospital characteristics from Hospital cost reports data
1. `crVI_hospsmpl.do`
2. `predict_pnltprs.do`
1. `anpenalty_VI_bycond.do`
4. `ivpenalty_VI_bycond.do`
