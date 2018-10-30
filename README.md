# vertical-integration
Research on the vertical integration between hospital and post-acute care (PAC)

## Use the CMS Hospital Cost Report data to obtain hospital characteristics

1. _Note_: For years 2010 and later, use the form 2552-10; for years before-2011 (2010-2011 data appear in both forms), use the form 2552-96.
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
We use hospital-month-condition level index admissions data and hospital-month-condition-PAC provider level referral data (where referrals are defined as starting PAC within 2 days from hospital discharge after matched).

1. `crindex_admit_chm.do`
  - create hospital-month-condition level index admissions data from the raw CSV file
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)
2. `crindex_admit_chm_ddest.do`
  - create hospital-month-condition-discharge destination level index admissions data from the raw CSV file
2. `crPACreferral_tchpm.do`
  - Import hospital-month-condition-PAC provider level referral data separately for SNF
  - SNF data available for 2008/1 - 2016/6 except 2012/6 (just not avail.)
1. `crindex_admit_comorbid_chm.do`
  - create index admission data for each condition-hospital-FY-comorbidity group
3. `crSNFreferral_comorbid_chm.do`
  - create SNF referral data for each condition-hospital-SNF-FY-comorbidity group
1. `crindex_admit_dual_chy.do`
  - create # index admissions and SNF referrals that are dual-eligible for each condition-hospital-FY
6. `hospcr.sh` in `costreport_hosp` directory
  - Get hospital characteristics and total patient revenues for each FY from CMS Cost Report data
7. `crhosp_chars_cr.do`
  - create hospital characteristics data from the HCRIS hospital cost report data for 2000-2015
1. `crhosp_compare.do`
  - Use hospital compare data to obtain risk-adjusted readmission rate for each hospital & national average during a 3-year window period (2008-2010)
1. `crSNF_hrr_xwalk.do`
  - get HRR & HSA for each SNF in the Medicare claims data
1. `crsnf_deficiency.do`
  - create SNF quality (deficiency counts) data from the SNF Compare archive database
1. `crsnf_rating.do`
  - create SNF quality (rating) data from the SNF Compare archive database
1. `agebins.do`
  - create index admission-level and SNF-level data on the # patients in each 5-age bin
4. `crindex_admit_DRG_chm.do`
  - Create hospital-FY-condition-DRG level index admissions data
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)

## Programs to obtain and clean data for hospital participation in other pay for performance programs
1. `crhosp_bpci_participant.do`
  - use BPCI data files from the quarter 3 of each CY to create hospital-year level status of their participation in BPCI using files from https://innovation.cms.gov/initiatives/Bundled-Payments/Archived-Materials.html
2. `crVBP.do`
  - create data for VBP risk adjustment factor (proxy) at the hospital-year level
3. `crEHR.do`
  - create data for meaning use of EHR at the hospital-year level
4. `crHAC.do`
  - create data for an indicator of being in the bottom quartile of the HAC score at the hospital-year level for FY = 2015-2016


## Examine the impact of hospital readmissions penalty on hospital-PAC vertical integration
2. `predict_pnltprs.do`
  - in each year t = 2011, predict the likelihood of penalty, penalty rate, penalty dollar amount in t+2 using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}
3. `crhosp_fy_VI.do`
  - Create hospital-FY level data containing total admission volume, PAC referral concentration, hospital characteristics
  - hospital characteristics from Hospital cost reports data
2. `anSNFquality.do`
  - Do high-quality SNFs receive more referrals?
3. `anpenalty_VI_agg3c.do`
4. `tableformat.do`
  - reshape coefficient estimate output into a formatted table
1. `desc_trend_VI.do`
  - descriptive analysis of the trend of vertical integration over time
1. `robustcheck.do`
  - robustness check analysis

## Additional analyses done separately for each condition
1. `crhosp_fy_VI_cond.do`
2. `anSNFquality_cond.do`
3. `anpenalty_VI_cond.do`




2. `predict_pnltprs_dynamic.do`
    - create dynamic penalty pressure: in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}
4. `ivpenalty_VI_bycond.do`
5. `andynamicpp_VI_agg3c.do`
  - analyze the impact of dynamic penalty pressure on the integration outcome
6. `dpm.do`
  - dynamic panel data model estimation: analyze the impact of dynamic penalty pressure on the integration outcome with a lagged outcome as a regressor
1. `persistence.do`
  - persistence of penalty pressure and integration within a hospital

## diagnostic files
1. `analyze_2012dip.do`
  - why is there a dip in probability of referral in Jan 2011-May 2012?

## Describe the trend of hospital-PAC vertical integration

  2. `desc_trend_VI2.do` - drop

## Misc. files _not_ to be used for final analysis
3. `crpenalty_VI_bycond.do`
1. `anpenalty_VI_bycond.do`
3. `anpenalty_VI_tripleDD.do`

5. `crinpat_pmt_hosp_fy_drg.do`
  - Create hospital-FY-condition level data on Medicare inpatient payment by combining the DRG-level counts from our internal Medicare data with the public DRG-level average payment payment data from CMS
1. `crVI_hospsmpl.do` - skip
2. `crpenalty_VI_agg3c.do` - skip
7. `crpac_mkt_hhi.do` - drop ?
  - Create PAC market concentration (HHI) at the hospital HRR / HSA level using the referral data created from the Medicare claims data (exclude the hospitals' own referrals when calculating the HHI)
