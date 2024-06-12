#!/bin/bash

mod="lr"
#mod="lgbm"
#reco_list="oct ite csbu tebu"
reco_list="csbu"
nn_list="free sntz osqp"
res_list="is"
str_list="DB SB"
zero_list="1 0"

for reco in $reco_list 
do
  for nn in $nn_list 
  do
    for res in $res_list
    do
      for str in $str_list 
      do
        for zer in $zero_list 
        do
        Rscript ./reconciliation.R  mod=\'$mod\' \
                                    mod_reco=\'$reco\' \
                                    nn_reco=\'$nn\' \
                                    res_reco=\'$res\' \
                                    str_reco=\'$str\' \
                                    zero=$zer
        done
      done
    done
  done
done