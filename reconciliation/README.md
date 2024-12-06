# Cross-temporal Wind Forecast Reconciliation

This folder contains all the files and scripts required to organize, reconcile, and analyze forecasts generated using **Linear Regression (LR)** and **LightGBM (LGBM)** models. Follow the instructions below to utilize the various components of the project.

## Organization and Pre-Reconciliation
The previously computed base forecasts must be organized in the following folders:

- **Base/Linear Reg results/In-sample-error-LR**: Contains forecasts for the test set (`fc_m1_lr_*.rds`) and in-sample residuals (`res_fit_all_lr_*.rds`) generated with the LR model.
- **Base/LGBM results/In-sample-error-lgbm**: Contains forecasts for the test set (`fc_m1_lgbm_*.rds`) and in-sample residuals (`res_fit_all_lgbm_*.rds`) generated with the LGBM model.
- **Base/Linear Reg results/Validation-error-LR**: Contains forecasts for the validation set (`fc_lr_m2_*.rds`) generated with the LR model.
- **Base/LGBM results/Validation-error-lgbm**: Contains forecasts for the validation set (`fc_lgbm_m2_*.rds`) generated with the LGBM model.

## Exporting Base Files
To export in-sample errors, validation errors, and base forecasts, run the bash file:
```bash
bash bash_export.sh
```
This script depends on the following R files:
- `export_is.R`: Exports in-sample errors.
- `export_vl.R`: Exports validation errors and base forecasts.

## Reconciliation
To perform reconciliation, run the bash file:
```bash
bash bash_reco.sh
```
This script depends on the R file `reconciliation.R`.

## Calculating Scores
To calculate scores (accuracy and costs):

1. Run the following command to compute the forecasts accuracy scores:
   ```bash
   Rscript ./R/scores.R
   ```

2. Run the following command to compute the decision costs:
   ```bash
   Rscript ./R/index.R
   ```

## Tables and Plots
To generate tables and plots for analysis:

1. Generate tables with:
   ```bash
   Rscript ./R/tables.R
   ```

2. Generate MCB plots with:
   ```bash
   Rscript ./R/mcb.R
   ```

3. Generate a polar plot with:
   ```bash
   Rscript ./R/polar_plot.R
   ```

4. For cost-related tables:
   ```bash
   Rscript ./R/tables_cost.R
   ```

## Folder Structure
Here is an overview of the folder structure:

```
├── Base
│   ├── Linear Reg results
│   │   ├── In-sample-error-LR
│   │   └── Validation-error-LR
│   └── LGBM results
│       ├── In-sample-error-lgbm
│       └── Validation-error-lgbm
├── bash_export.sh
├── bash_reco.sh
└── R
    ├── export_is.R
    ├── export_vl.R
    ├── reconciliation.R
    ├── scores.R
    ├── index.R
    ├── tables.R
    ├── mcb.R
    ├── polar_plot.R
    └── tables_cost.R
```

> [!IMPORTANT]  
> - Ensure that all required dependencies are installed before running the scripts.
> - Verify that the folder paths match the described structure.

