# vertical-integration
Research on the vertical integration between hospital and post-acute care (PAC)

## Use the CMS Hospital Cost Report data to obtain hospital characteristics

1. Figure out which variables you want from the Cost Reports and where they are. This [website](https://www.costreportdata.com/worksheet_formats.html) is useful to navigate which worksheet to refer to for specific variables (see 2010 format for years 2011-current, and 1996 format for previous years). Locate 1) worksheet, 2) line number, 3) column number that contain the variable of your interest.

2. Write SAS or Stata codes to extract data from those specific locations. The [NBER HCRIS data](http://www.nber.org/data/hcris.html) page provides template codes as well as raw data. My codes are adapted from those.

I share my codes that achieve creating hospital-level panel data for 2011-2016 from the cost reports (as of 8/23/16, 2016 seems incomplete). They contain only the select variables of my interest. To run these codes,

1. Set the working directory to the cloned repository directory or wherever the following bash script file is.
```
cd vertical-integration/costreport_hosp
```
2. Run the following shell script that contains all the codes from downloading raw data to extracting relevant variables, and creating a final panel data file in the Stata format
```bash
qsub hospcr.sh
```
Note: Change the working directory inside individual code files.

## Describe the trend of hospital-PAC vertical integration


## Examine the impact of hospital readmissions penalty on hospital-PAC vertical integration 
