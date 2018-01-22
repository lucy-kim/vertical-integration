#!/bin/bash
#$ -N sasjob01
#$ -j y
#$ -m e
# extract from the CMS HHA cost report data

cd /home/hcmg/kunhee/vertical-int/

# Download raw data
sas -nodms -noterminal download_hhacr.sas

# Read hhaital cost report main, numeric, alphanumeric files
sas -nodms -noterminal read_hha_rpt.sas
sas -nodms -noterminal read_hha_rpt_nmrc.sas
sas -nodms -noterminal read_hha_rpt_alphnmrc.sas

# Extract some variables from the numeric, alpha file
sas -nodms -noterminal extract_hha_nmrc.sas
sas -nodms -noterminal extract_hha_alpha.sas

# # Transpose to wide shape (multiple variables per report number) & merge alphanumeric & numeric data to create hha-level data
sas -nodms -noterminal crhhacr.sas

# Create HHA panel data for each calendar year
stata-se -q -b do crhhacr_panel.do
