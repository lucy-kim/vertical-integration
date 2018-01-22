#!/bin/bash

module load stata/13

cd /ifs/home/kimk13/VI

stata-se -q -b do crhosp_fy_VI.do
