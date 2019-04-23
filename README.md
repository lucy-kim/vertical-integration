# Changes in hospital referral patterns to skilled nursing facilities under the Hospital Readmissions Reduction Program
I describe the codes used to investigate the impact of the Hospital Readmissions Reduction Program (HRRP) vertical integration between hospital and post-acute care (PAC).

Run the following codes chronologically.

### Use CMS Hospital Cost Report data to obtain hospital characteristics

Generally, to use the CMS Hospital Cost Report data, I recommend:

1. Figure out which variables you want from the Cost Reports and where they are.
  - This [website](https://www.costreportdata.com/worksheet_formats.html) is useful to navigate which worksheet to refer to for specific variables
  - Use the form `2552-10` for years 2010-current (2010-2011 data appear in both forms), and `2552-96` for previous years
  - Locate 1) worksheet, 2) line number, 3) column number that contain the variable of your interest.
2. Write SAS or Stata codes to extract data from those specific locations.
  - The [NBER HCRIS data](http://www.nber.org/data/hcris.html) page provides template codes as well as raw data. My codes are adapted from those.

Specifically for this project, run the following shell script that contains all the codes from downloading raw data to extracting relevant variables and creating a final data file in the Stata format
```bash
cd vertical-integration/costreport_hosp
qsub hospcr.sh
```
Note: You may have to change the working directory inside individual code files.


### Construct aggregate hospital-level Medicare data
We use hospital-month-condition level index admissions data and hospital-month-condition-PAC provider level referral data (where referrals are defined as starting PAC within 2 days from hospital discharge after matched).

The final data constructed are hospital-level panel data for 2009-2016.

1. `crindex_admit_chm.do`
  - create hospital-month-condition level index admissions data from the raw CSV file
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)
2. `crindex_admit_chm_ddest.do`
  - create hospital-month-condition-discharge destination level index admissions data from the raw CSV file
3. `crPACreferral_tchpm.do`
  - Import hospital-month-condition-PAC provider level referral data separately for SNF
  - SNF data available for 2008/1 - 2016/6 except 2012/6 (just not avail.)
4. `crindex_admit_comorbid_chm.do`
  - create index admission data for each condition-hospital-FY-comorbidity group
5. `crSNFreferral_comorbid_chm.do`
  - create SNF referral data for each condition-hospital-SNF-FY-comorbidity group
6. `crindex_admit_dual_chy.do`
  - create # index admissions and SNF referrals that are dual-eligible for each condition-hospital-FY
7. `hospcr.sh` in `costreport_hosp` directory
  - Get hospital characteristics and total patient revenues for each FY from CMS Cost Report data
8. `crhosp_chars_cr.do`
  - create hospital characteristics data from the HCRIS hospital cost report data for 2000-2015
9. `crhosp_compare.do`
  - Use hospital compare data to obtain risk-adjusted readmission rate for each hospital & national average during a 3-year window period (2008-2010)
10. `crSNF_hrr_xwalk.do`
  - get HRR & HSA for each SNF in the Medicare claims data
11. `crsnf_deficiency.do`
  - create SNF quality (deficiency counts) data from the SNF Compare archive database
12. `crsnf_rating.do`
  - create SNF quality (rating) data from the SNF Compare archive database
13. `agebins.do`
  - create index admission-level and SNF-level data on the # patients in each 5-age bin
14. `crindex_admit_DRG_chm.do`
  - Create hospital-FY-condition-DRG level index admissions data
  - available for 2008/1 -  2016/6 except 2012/6 (just not avail.)

### Obtain data on hospital participation in other pay for performance programs
1. `crhosp_bpci_participant.do`
  - use BPCI data files from the quarter 3 of each CY to create hospital-year level status of their participation in BPCI using files from https://innovation.cms.gov/initiatives/Bundled-Payments/Archived-Materials.html
2. `crVBP.do`
  - create data for VBP risk adjustment factor (proxy) at the hospital-year level
3. `crEHR.do`
  - create data for meaning use of EHR at the hospital-year level
4. `crHAC.do`
  - create data for an indicator of being in the bottom quartile of the HAC score at the hospital-year level for FY = 2015-2016

### Main analysis
1. `predict_pnltprs.do`
  - in each year t = 2011, predict the likelihood of penalty, penalty rate, penalty dollar amount in t+2 using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}
2. `crhosp_fy_VI.do`
  - Create hospital-FY level data containing total admission volume, PAC referral concentration, hospital characteristics
  - hospital characteristics from Hospital cost reports data
3. `anSNFquality.do`
  - Do high-quality SNFs receive more referrals?
4. `anpenalty_VI_agg3c.do`
5. `tableformat.do`
  - reshape coefficient estimate output into a formatted table
6. `desc_trend_VI.do`
  - descriptive analysis of the trend of vertical integration over time


### Robustness check analysis
1. `robustcheck.do`
  - Initial robustness check analysis using alternative penalty pressure measures, shorter post-HRRP period (up to 2013), and a flexible form of difference-in-differences model using yearly indicators interacted with penalty pressure
1. `DiD_penalty_VI.Rmd`
  - create figures, including the difference-in-differences estimates on yearly indicators interacted with penalty pressure
2. `supp_desc_analysis.do`
  - % 3 condition discharges out of all discharges
  - % discharges with hospital LOS < 3 days
  - hospital's % SNF referrals for 3 conditions to SNFs in the same HRR (HSA) as the hospital
3. `suppl_reg_analysis.do`
  - unweighted estimation, re-estimate using expected penalty rates alone (without share of Medicare discharges)
  - Sensitivity analysis when restricting to hospitals with at least 50 SNF referrals in every year
  - All the heterogeneity analyses by hospital characteristics: hospital size, urban, teaching, BPCI participation status, SNF market size

### Condition-specific analysis (part of heterogeneity analysis)
1. `crhosp_fy_VI_cond.do`
2. `anSNFquality_cond.do`
3. `anpenalty_VI_cond.do`




<!-- 2. `predict_pnltprs_dynamic.do`
    - create dynamic penalty pressure: in each year t = 2011, 2012, 2013, ..., predict the likelihood of penalty, penalty rate, penalty dollar amount using the own performance (raw readmission rate, excess readmission rate) during {t-3,t-2,t-1}
4. `ivpenalty_VI_bycond.do`
5. `andynamicpp_VI_agg3c.do`
  - analyze the impact of dynamic penalty pressure on the integration outcome
6. `dpm.do`
  - dynamic panel data model estimation: analyze the impact of dynamic penalty pressure on the integration outcome with a lagged outcome as a regressor
1. `persistence.do`
  - persistence of penalty pressure and integration within a hospital -->

<!-- ## diagnostic files
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
  - Create PAC market concentration (HHI) at the hospital HRR / HSA level using the referral data created from the Medicare claims data (exclude the hospitals' own referrals when calculating the HHI) -->
