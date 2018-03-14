#!/bin/bash
#$ -l mem_free=10G

module load stata/13
# module load sas/9.4

cd /ifs/home/kimk13/VI

# sas -nodms -noterminal download_pos.sas
#$ -N sasjob01 #$ -j y

stata-se -q -b do crhosp_fy_VI.do
