#!/bin/bash
#$ -l mem_free=10G

module load stata/13
# module load sas/9.4

cd /ifs/home/kimk13/VI

stata-se -q -b do iv2.do

#$ -N sasjob01 #$ -j y
# sas -nodms -noterminal download_pos.sas
