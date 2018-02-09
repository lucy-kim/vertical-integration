#!/bin/bash

module load stata/13
# module load sas/9.4

cd /ifs/home/kimk13/VI

# sas -nodms -noterminal download_bpci.sas

stata-se -q -b do crhosp_bpci_participant.do

  #$ -N sasjob01 #$ -j y
