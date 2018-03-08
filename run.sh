#!/bin/bash
#$ -l mem_free=10G
#$ -N sasjob01 #$ -j y

# module load stata/13
module load sas/9.4

cd /ifs/home/kimk13/VI

sas -nodms -noterminal download_pos.sas

# stata-se -q -b do ivpenalty_VI_bycond.do
