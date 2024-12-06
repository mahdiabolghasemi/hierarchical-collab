#!/bin/bash

mod_list="lgbm lr" # Base forecasts models
reco_list="oct ite csbu tebu" # Reconciliation approaches 
nn_list="sntz" # Set negative to zero approach 
res_list="is vl" # In-sample (is) or validation (vl) errors
str_list="SB DB" # Statistical- (SB) or decision-based (DB)
zero_list="0" # Set negative base forecasts to 0 before reconciliation

for $mod in $mod_list 
do
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
          Rscript ./R/reconciliation.R  mod=\'$mod\' \
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
done

read -rsp $'Press enter to continue...\n'
