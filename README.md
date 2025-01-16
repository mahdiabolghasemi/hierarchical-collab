# hierarchical-collab

This folder contains all the files and scripts required to generate forecasts using Linear Regression (LR) and LightGBM (LGBM) models. Base forecasts are generated using LR and LGBM models, and forecast errors are recorded in two scenarios: i) in-sample forecast errors, ii) validation sample errors. 
CT-LGBM-insample.R contains the forecasts generated using LGBM as forecasting model, and includes the in-sample errors which will be used for reoconciliation. CT-LR-insample.R contains the forecasts generated using LR as forecasting model, and includes the in-sample errors which will be used for reoconciliation.
