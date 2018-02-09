#!/bin/bash
#$ -N sasjob01
#$ -j y
#$ -m e

cd /home/hcmg/kunhee/vertical-int

# Download raw data
sas -nodms -noterminal download_pos.sas
