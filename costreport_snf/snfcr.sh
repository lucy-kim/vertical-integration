#!/bin/bash
#$ -N sasjob01
#$ -j y
#$ -m e
# extract from the CMS SNF cost report data

cd /home/hcmg/kunhee/vertical-int/

# Download raw data
sas -nodms -noterminal download_snfcr.sas

# Read snfital cost report main, numeric, alphanumeric files
sas -nodms -noterminal read_snf_rpt.sas
sas -nodms -noterminal read_snf_rpt_nmrc.sas
sas -nodms -noterminal read_snf_rpt_alphnmrc.sas

# Extract some variables from the numeric, alpha file
sas -nodms -noterminal extract_snf_nmrc.sas
sas -nodms -noterminal extract_snf_alpha.sas

# Transpose to wide shape (multiple variables per report number) & merge alphanumeric & numeric data to create snfital-level data
sas -nodms -noterminal crsnfcr.sas

# # Create snfital panel data for each calendar year
stata-se -q -b do crsnfcr_panel.do
