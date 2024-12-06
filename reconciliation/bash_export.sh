#!/bin/bash

mod_list="lgbm lr"

for mod in $mod_list 
do
  Rscript ./R/export_is.R $mod
  Rscript ./R/export_vl.R $mod
done

read -rsp $'Press enter to continue...\n'