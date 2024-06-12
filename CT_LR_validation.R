#### Import everything and prepare data ####

library(dplyr)
library(tsibble)
#library("readxl")
library(fpp3)
library(zoo)
library(stats)
library(foreach)
library(doParallel)
library(lightgbm)
library(distributional)
library(progress)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

#### Load .rds files ####
min10_power <- readRDS(file = "min10.rds")
min20_power <- readRDS(file = "min20.rds")
min30_power <- readRDS(file = "min30.rds")
min40_power <- readRDS(file = "min40.rds")
min60_power <- readRDS(file = "hr1.rds")
min80_power <- readRDS(file = "min80.rds")
min120_power <- readRDS(file = "min120.rds")
min160_power <- readRDS(file = "min160.rds")
min240_power <- readRDS(file = "min240.rds")
min480_power <- readRDS("min480.rds")

#### Benchmark models - 10 minutely ####
# initialize our accuracy tibble
fc10_benchmark <- NULL;

# compute number of entries
N_10 = nrow(min10_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2020-10-01"))
N_10_all = nrow(min10_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_10,N_10_all-1,1)) {
  # take training set from elements 1 up to i
  min10_tr_benchmark <- min10_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min10_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=1) %>% 
    bind_cols(Actual= min10_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc10_benchmark <- bind_rows(fc10_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc10_benchmark, "fc10_benchmark.rds")
saveRDS(fitted, file = "fc10_benchmark_fitted.rds")
saveRDS(residuals, file = "fc10_benchmark_residuals.rds")




#### Benchmark models - 20 minutely ####
# initialize our accuracy tibble
fc20_benchmark <- NULL;

# compute number of entries
N_20 = nrow(min20_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min20_n %>% filter(as.Date(Time) < "2020-10-01"))
N_20_all = nrow(min20_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min20_n%>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_20,N_20_all-24,24)) {
  # take training set from elements 1 up to i
  min20_tr_benchmark <- min20_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min20_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=24) %>% 
    bind_cols(Actual= min20_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+24)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc20_benchmark <- bind_rows(fc20_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc20_benchmark, "fc20_benchmark.rds")
saveRDS(fitted, file = "fc20_benchmark_fitted.rds")
saveRDS(residuals, file = "fc20_benchmark_residuals.rds")



#### Benchmark models - 30 minutely ####
# initialize our accuracy tibble
fc30_benchmark <- NULL;

# compute number of entries
N_30 = nrow(min30_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min30_n %>% filter(as.Date(Time) < "2020-10-01"))
N_30_all = nrow(min30_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min30_n%>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_30,N_30_all-16,16)) {
  # take training set from elements 1 up to i
  min30_tr_benchmark <- min30_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min30_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=16) %>% 
    bind_cols(Actual= min30_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+16)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc30_benchmark <- bind_rows(fc30_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc30_benchmark, "fc30_benchmark.rds")
saveRDS(fitted, file = "fc30_benchmark_fitted.rds")
saveRDS(residuals, file = "fc30_benchmark_residuals.rds")




#### Benchmark models - 40 minutely ####
# initialize our accuracy tibble
fc40_benchmark <- NULL;

# compute number of entries
N_40 = nrow(min40_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min40_n %>% filter(as.Date(Time) < "2020-10-01"))
N_40_all = nrow(min40_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min40_n%>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_40,N_40_all-12,12)) {
  # take training set from elements 1 up to i
  min40_tr_benchmark <- min40_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min40_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=12) %>% 
    bind_cols(Actual= min40_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+12)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc40_benchmark <- bind_rows(fc40_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc40_benchmark, "fc40_benchmark.rds")
saveRDS(fitted, file = "fc40_benchmark_fitted.rds")
saveRDS(residuals, file = "fc40_benchmark_residuals.rds")





#### Benchmark models - 1 hourly ####
# initialize our accuracy tibble
fc60_benchmark <- NULL;

# compute number of entries
N_60 = nrow(min60_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min60_n %>% filter(as.Date(Time) < "2020-10-01"))
N_60_all = nrow(min60_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min60_n%>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_60,N_60_all-8,8)) {
  # take training set from elements 1 up to i
  min60_tr_benchmark <- min60_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min60_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=8) %>% 
    bind_cols(Actual= min60_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+8)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc60_benchmark <- bind_rows(fc60_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc60_benchmark, "fc60_benchmark.rds")
saveRDS(fitted, file = "fc60_benchmark_fitted.rds")
saveRDS(residuals, file = "fc60_benchmark_residuals.rds")


#### Benchmark models - 80 minutely ####
# initialize our accuracy tibble
fc80_benchmark <- NULL;

# compute number of entries
N_80 = nrow(min80_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min80_n %>% filter(as.Date(Time) < "2020-10-01"))
N_80_all = nrow(min80_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min80_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_80,N_80_all-6,6)) {
  # take training set from elements 1 up to i
  min80_tr_benchmark <- min80_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min80_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=6) %>% 
    bind_cols(Actual= min80_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+6)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc80_benchmark <- bind_rows(fc80_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc80_benchmark, "fc80_benchmark.rds")
saveRDS(fitted, file = "fc80_benchmark_fitted.rds")
saveRDS(residuals, file = "fc80_benchmark_residuals.rds")


#### Benchmark models - 120 minutely ####
# initialize our accuracy tibble
fc120_benchmark <- NULL;

# compute number of entries
N_120 = nrow(min120_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min120_n %>% filter(as.Date(Time) < "2020-10-01"))
N_120_all = nrow(min120_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min120_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_120,N_120_all-4,4)) {
  # take training set from elements 1 up to i
  min120_tr_benchmark <- min120_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min120_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=4) %>% 
    bind_cols(Actual= min120_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+4)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc120_benchmark <- bind_rows(fc120_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc120_benchmark, "fc120_benchmark.rds")
saveRDS(fitted, file = "fc120_benchmark_fitted.rds")
saveRDS(residuals, file = "fc120_benchmark_residuals.rds")



#### Benchmark models - 160 minutely ####
# initialize our accuracy tibble
fc160_benchmark <- NULL;

# compute number of entries
N_160 = nrow(min160_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min160_n %>% filter(as.Date(Time) < "2020-10-01"))
N_160_all = nrow(min160_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min160_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_160,N_160_all-3,3)) {
  # take training set from elements 1 up to i
  min160_tr_benchmark <- min160_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min160_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=3) %>% 
    bind_cols(Actual= min160_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+3)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc160_benchmark <- bind_rows(fc160_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc160_benchmark, "fc160_benchmark.rds")
saveRDS(fitted, file = "fc160_benchmark_fitted.rds")
saveRDS(residuals, file = "fc160_benchmark_residuals.rds")


#### Benchmark models - 240 minutely ####
# initialize our accuracy tibble
fc240_benchmark <- NULL;

# compute number of entries
N_240 = nrow(min240_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min240_n %>% filter(as.Date(Time) < "2020-10-01"))
N_240_all = nrow(min240_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min240_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_240,N_240_all-2,2)) {
  # take training set from elements 1 up to i
  min240_tr_benchmark <- min240_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min240_tr_benchmark %>%
    #group_by(Group, Subgroup) %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=2) %>% 
    bind_cols(Actual= min240_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1): (i+2)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc240_benchmark <- bind_rows(fc240_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc240_benchmark, "fc240_benchmark.rds")
saveRDS(fitted, file = "fc240_benchmark_fitted.rds")
saveRDS(residuals, file = "fc240_benchmark_residuals.rds")


#### Benchmark models - 480 minutely ####
# initialize our accuracy tibble
fc480_benchmark <- NULL;

# compute number of entries
N_480 = nrow(min480_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min480_n %>% filter(as.Date(Time) < "2020-10-01"))
N_480_all = nrow(min480_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min480_n %>% filter(as.Date(Time) < "2021-01-01"))

for (i in seq(N_480,N_480_all-1,1)) {
  # take training set from elements 1 up to i
  min480_tr_benchmark <- min480_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i)
  
  fit_m2 <- min480_tr_benchmark %>%
    model(naive_model = NAIVE(Power))
  
  # initialize the accuracy tibble
  fc_benchmark <- fit_m2 %>%
    forecast(h=1) %>% 
    bind_cols(Actual= min480_n %>%
                group_by(Group, Subgroup) %>%
                dplyr::slice((i+1)) %>% pull(Power)) %>% 
    as_tibble()
  fc_benchmark= bind_cols(fc_benchmark, iteration=i)
  fc480_benchmark <- bind_rows(fc480_benchmark,fc_benchmark)
  
  # extract fitted and residuals
  fitted <- fit_m2 %>%
    fitted()
  residuals <- fit_m2 %>%
    residuals()
}

saveRDS(fc480_benchmark, "fc480_benchmark.rds")
saveRDS(fitted, file = "fc480_benchmark_fitted.rds")
saveRDS(residuals, file = "fc480_benchmark_residuals.rds")


#### Time series linear regression with feature engineering - 10 minutely ####
min10_n <- min10_power

# load .rds file
min10_n <- readRDS(file = "min10n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_10 = nrow(min10_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2020-10-01"))
N_10_all = nrow(min10_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2021-01-01"))


# initialise objects to save results
fc_lr_m2_10 <- NULL;
res_fit_all_m2_10 <- new_data_all_10 <- NULL

start_time_10 <- Sys.time()
pb <- progress_bar$new(
  total = length(seq(N_10, N_10_all, 48)),
  format = "[:bar] :percent :elapsedfull"
)
gc()
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_10,N_10_all,48)) {
  # compute fit
  variables <- setdiff(names(min10_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  gc()
  fit_m2 <- min10_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  new_data <- NULL
  # h=48 steps ahead for 8 hours forecasts
  for (h in 1: 48) {
    new_data= bind_rows(new_data, 
                        bind_cols(min10_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min10_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_10 <- bind_rows(new_data_all_10, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data  %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min10_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+48)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_10 <- bind_rows(fc_lr_m2_10,fc)
  
  # extract fitted and residuals
  fitted_m2_10 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_10 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_10 <- bind_rows(res_fit_all_m2_10,bind_cols(fitted_m2_10,res=residuals_m2_10$.resid,iteration=i))
pb$tick()
  }

end_time_10 <- Sys.time()

#saveRDS(fitted_m2_10, file = "fc10_m2_lr_fitted.rds")
#saveRDS(residuals_m2_10, file = "fc10_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_10, "res_fit_all_m2_10.rds")
saveRDS(fc_lr_m2_10, "fc_lr_m2_10.rds")

res_fit_all_m2_10 <- readRDS("res_fit_all_m2_10.rds")
fc_lr_m2_10 <- readRDS("fc_lr_m2_10.rds")

#### Time series linear regression with feature engineering - 20 minutely ####
min20_n <- min20_power

# load .rds file
min20_n <- readRDS(file = "min20n_features.rds") %>% select(-"Wind_Speed")


# compute number of entries
N_20 = nrow(min20_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min20_n %>% filter(as.Date(Time) < "2020-10-01"))
N_20_all = nrow(min20_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min20_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_20 <- NULL;
res_fit_all_m2_20 <- NULL
new_data_all_20 <- NULL

# Start timing
start_time_20 <- Sys.time()
gc()
pb <- progress_bar$new(
  total = length(seq(N_20, N_20_all, 24)),
  format = "[:bar] :percent :elapsedfull"
)
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_20,N_20_all,24)) {
  variables <- setdiff(names(min20_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min20_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=24 steps ahead for 8 hours forecasts
  for (h in 1: 24) {
    new_data= bind_rows(new_data, 
                        bind_cols(min20_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min20_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_20 <- bind_rows(new_data_all_20, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min20_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+24)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_20 <- bind_rows(fc_lr_m2_20,fc)
  
  # extract fitted and residuals
  fitted_m2_20 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_20 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_20 <- bind_rows(res_fit_all_m2_20,bind_cols(fitted_m2_20,res=residuals_m2_20$.resid,iteration=i))
  pb$tick()
}

end_time_20 <- Sys.time()

saveRDS(res_fit_all_m2_20, "res_fit_all_m2_20.rds")
saveRDS(fc_lr_m2_20, "fc_lr_m2_20.rds")

res_fit_all_m2_20 <- readRDS("res_fit_all_m2_20.rds")
fc_lr_m2_20 <- readRDS("fc_lr_m2_20.rds")

#### Time series linear regression with feature engineering - 30 minutely ####
min30_n <- min30_power

# load .rds file
min30_n <- readRDS(file = "min30n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_30 = nrow(min30_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min30_n %>% filter(as.Date(Time) < "2020-10-01"))
N_30_all = nrow(min30_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min30_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_30 <- NULL;
res_fit_all_m2_30 <- NULL
new_data_all_30 <- NULL

start_time_30 <- Sys.time()
# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_30, N_30_all, 16)),
  format = "[:bar] :percent :elapsedfull"
)

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_30,N_30_all,16)) {
  variables <- setdiff(names(min30_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min30_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=16 steps ahead for 8 hours forecasts
  for (h in 1: 16) {
    new_data= bind_rows(new_data, 
                        bind_cols(min30_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min30_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_30 <- bind_rows(new_data_all_30, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min30_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+16) ) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_30 <- bind_rows(fc_lr_m2_30,fc)
  
  # extract fitted and residuals
  fitted_m2_30 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_30 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_30 <- bind_rows(res_fit_all_m2_30,bind_cols(fitted_m2_30,res=residuals_m2_30$.resid,iteration=i))
  pb$tick()
}
end_time_30 <- Sys.time()

#saveRDS(fitted_m2_30, file = "fc30_m2_lr_fitted.rds")
#saveRDS(residuals_m2_30, file = "fc30_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_30, "res_fit_all_m2_30.rds")
saveRDS(fc_lr_m2_30, "fc_lr_m2_30.rds")

#fc_lr_m2_30 <- readRDS("fc_lr_m2_30.rds")

#### Time series linear regression with feature engineering - 40 minutely ####
min40_n <- min40_power
# %>%  filter_index("2021-06-30")

min40_n <- readRDS(file = "min40n_features.rds") %>% select(-"Wind_Speed")


# compute number of entries
N_40 = nrow(min40_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min40_n %>% filter(as.Date(Time) < "2020-10-01"))
N_40_all = nrow(min40_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min40_n%>% filter(as.Date(Time) < "2021-01-01"))


# initialise objects to save results
fc_lr_m2_40 <- NULL;
res_fit_all_m2_40 <- NULL
new_data_all_40 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_40,N_40_all,12)) {
  variables <- setdiff(names(min40_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min40_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=12 steps ahead for 8 hours forecasts
  for (h in 1: 12) {
    new_data= bind_rows(new_data, 
                        bind_cols(min40_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min40_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_40 <- bind_rows(new_data_all_40, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min40_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+12)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_40 <- bind_rows(fc_lr_m2_40,fc)
  
  # extract fitted and residuals
  fitted_m2_40 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_40 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_40 <- bind_rows(res_fit_all_m2_40,bind_cols(fitted_m2_40,res=residuals_m2_40$.resid,iteration=i))
  
}


saveRDS(res_fit_all_m2_40, "res_fit_all_m2_40.rds")
saveRDS(fc_lr_m2_40, "fc_lr_m2_m2_40.rds")



#### Time series linear regression with feature engineering - 60 minutely ####
min60_n <- min60_power
# %>%  filter_index("2021-06-30")

# load .rds file
min60_n <- readRDS(file = "min60n_features.rds") %>% select(-"Wind_Speed")


# compute number of entries
N_60 = nrow(min60_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min60_n %>% filter(as.Date(Time) < "2020-10-01"))
N_60_all = nrow(min60_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min60_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_60 <- NULL;
res_fit_all_m2_60 <- NULL
new_data_all_60 <- NULL

start_time_60 <- Sys.time()
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_60,N_60_all,8)) {
  variables <- setdiff(names(min60_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min60_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=8 steps ahead for 8 hours forecasts
  for (h in 1: 8) {
    new_data= bind_rows(new_data, 
                        bind_cols(min60_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min60_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_60 <- bind_rows(new_data_all_60, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min60_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+8)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  
  fc_lr_m2_60 <- bind_rows(fc_lr_m2_60,fc)
  
  # extract fitted and residuals
  fitted_m2_60 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_60 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_60 <- bind_rows(res_fit_all_m2_60,bind_cols(fitted_m2_60,res=residuals_m2_60$.resid,iteration=i))
  
}

end_time_60 <- Sys.time()

#saveRDS(fitted_m2_60, file = "fc60_m2_lr_fitted.rds")
#saveRDS(residuals_m2_60, file = "fc60_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_60, "res_fit_all_m2_60.rds")
saveRDS(fc_lr_m2_60, "fc_lr_m2_60.rds")


#### Time series linear regression with feature engineering - 80 minutely ####
min80_n <- min80_power

# load .rds file
min80_n <- readRDS(file = "min80n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_80 = nrow(min80_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min80_n %>% filter(as.Date(Time) < "2020-10-01"))
N_80_all = nrow(min80_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min80_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_80 <- NULL;
res_fit_all_m2_80 <- NULL
new_data_all_80 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_80,N_80_all,6)) {
  variables <- setdiff(names(min80_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min80_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=4 steps ahead for 8 hours forecasts
  for (h in 1: 6) {
    new_data= bind_rows(new_data, 
                        bind_cols(min80_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min80_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_80 <- bind_rows(new_data_all_80, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data  %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min80_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+6)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_80 <- bind_rows(fc_lr_m2_80,fc)
  
  # extract fitted and residuals
  fitted_m2_80 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_80 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_80 <- bind_rows(res_fit_all_m2_80,bind_cols(fitted_m2_80,res=residuals_m2_80$.resid,iteration=i))
  
}

#saveRDS(fitted_m2_80, file = "fc80_m2_lr_fitted.rds")
#saveRDS(residuals_m2_80, file = "fc80_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_80, "res_fit_all_m2_80.rds")
saveRDS(fc_lr_m2_80, "fc_lr_m2_80.rds")

#### Time series linear regression with feature engineering - 120 minutely ####

min120_n <- min120_power

# load .rds file
min120_n <- readRDS(file = "min120n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_120 = nrow(min120_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min120_n %>% filter(as.Date(Time) < "2020-10-01"))
N_120_all = nrow(min120_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min120_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_120 <- NULL;
res_fit_all_m2_120 <- NULL
new_data_all_120 <- NULL

start_time_120 <- Sys.time()

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_120, N_120_all, 4)) {
  variables <- setdiff(names(min120_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min120_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=4 steps ahead for 8 hours forecasts
  for (h in 1: 4) {
    new_data= bind_rows(new_data, 
                        bind_cols(min120_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min120_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_120 <- bind_rows(new_data_all_120, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min120_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1, i+2, i+3, i+4) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_120 <- bind_rows(fc_lr_m2_120,fc)
  
  # extract fitted and residuals
  fitted_m2_120 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_120 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_120 <- bind_rows(res_fit_all_m2_120,bind_cols(fitted_m2_120,res=residuals_m2_120$.resid,iteration=i))
  
}

end_time_120 <- Sys.time()

#saveRDS(fitted_m2_120, file = "fc120_m2_lr_fitted.rds")
#saveRDS(residuals_m2_120, file = "fc120_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_120, "res_fit_all_m2_120.rds")
saveRDS(fc_lr_m2_120, "fc_lr_m2_120.rds")

#### Time series linear regression with feature engineering - 160 minutely ####

min160_n <- min160_power
# %>%  filter_index("2021-06-30")

# load .rds file
min160_n <- readRDS(file = "min160n_features.rds") %>% select(-"Wind_Speed")


# compute number of entries
N_160 = nrow(min160_n %>% filter(as.Date(Time) <"2020-10-01"))/n_keys(min160_n%>% filter(as.Date(Time) <"2020-10-01"))
N_160_all = nrow(min160_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min160_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_160 <- NULL;
res_fit_all_m2_160 <- NULL
new_data_all_160 <- NULL
# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_160, N_160_all-3, 3)),
  format = "[:bar] :percent :elapsedfull"
)
start_time_160 <- Sys.time()

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_160, N_160_all, 3)) {
  variables <- setdiff(names(min160_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min160_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=3 steps ahead for 8 hours forecasts
  for (h in 1: 3) {
    new_data= bind_rows(new_data, 
                        bind_cols(min160_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min160_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_160 <- bind_rows(new_data_all_160, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    as_tibble() %>% bind_cols(Actual= min160_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+3)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_160 <- bind_rows(fc_lr_m2_160,fc)
  
  # extract fitted and residuals
  fitted_m2_160 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_160 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_160 <- bind_rows(res_fit_all_m2_160,bind_cols(fitted_m2_160,res=residuals_m2_160$.resid,iteration=i))
  
  pb$tick()
}

# Start timing
end_time_160 <- Sys.time()

#saveRDS(fitted_m2_160, file = "fc160_m2_lr_fitted.rds")
#saveRDS(residuals_m2_160, file = "fc160_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_160, "res_fit_all_m2_160.rds")
saveRDS(fc_lr_m2_160, "fc_lr_m2_160.rds")

#### Time series linear regression with feature engineering - 240 minutely ####
min240_n <- min240_power
# %>%  filter_index("2021-06-30")

# load .rds file
min240_n <- readRDS(file = "min240n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_240 = nrow(min240_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min240_n %>% filter(as.Date(Time) < "2020-10-01"))
N_240_all = nrow(min240_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min240_n%>% filter(as.Date(Time) < "2021-01-01"))


fc_lr_m2_240 <- NULL;
res_fit_all_m2_240 <- NULL
new_data_all_240 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_240,N_240_all,2)) {
  variables <- setdiff(names(min240_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min240_n %>%
    dplyr::group_by(Group, Subgroup) %>%
    dplyr::slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  new_data <- NULL
  # h=2 steps ahead for 8 hours forecasts
  for (h in 1: 2) {
    new_data= bind_rows(new_data, 
                        bind_cols(min240_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1+(h-1)) %>% select(Power, contains("is")), 
                                  min240_n %>%  group_by(Group, Subgroup) %>% dplyr::slice(i+1) %>% as_tibble() %>%
                                    select(-Power, -Time, -Group, -Subgroup, -contains("is"))) %>% select(Group,  Subgroup, Time,Power, everything()) 
    )
  }
  new_data_all_240 <- bind_rows(new_data_all_240, new_data)
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data %>% select(-Power)) %>%
    #= min240_n %>%
    #   group_by(Group, Subgroup) %>%
    #   dplyr::slice(i+1, i+2)) %>%
    as_tibble() %>% bind_cols(Actual= min240_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1, i+2) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_240 <- bind_rows(fc_lr_m2_240,fc)
  
  # extract fitted and residuals
  fitted_m2_240 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_240 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_240 <- bind_rows(res_fit_all_m2_240,bind_cols(fitted_m2_240,res=residuals_m2_240$.resid,iteration=i))
  
}

#saveRDS(fitted_m2_240, file = "fc240_m2_lr_fitted.rds")
#saveRDS(residuals_m2_240, file = "fc2400_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_240, "res_fit_all_m2_240.rds")
saveRDS(fc_lr_m2_240, "fc_lr_m2_240.rds")

#### Time series linear regression with feature engineering - 480 minutely ####
min480_n <- min480_power
# load .rds file
min480_n <- readRDS(file = "min480n_features.rds") %>% select(-"Wind_Speed")

# compute number of entries
N_480 = nrow(min480_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min480_n %>% filter(as.Date(Time) < "2020-10-01"))
N_480_all = nrow(min480_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min480_n %>% filter(as.Date(Time) < "2021-01-01"))


fc_lr_m2_480 <- NULL;
res_fit_all_m2_480 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_480,N_480_all-1,1)) {
  # compute fit
  variables <- setdiff(names(min480_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  fit_m2 <- min480_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    fabletools::model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min480_n %>%
               group_by(Group, Subgroup) %>%
               #select(-Power) %>%
               dplyr::slice(i+1)) %>%
    as_tibble() %>% bind_cols(Actual= min480_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lr_m2_480 <- bind_rows(fc_lr_m2_480,fc)
  
  # extract fitted and residuals
  fitted_m2_480 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_480 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_480 <- bind_rows(res_fit_all_m2_480,bind_cols(fitted_m2_480,res=residuals_m2_480$.resid,iteration=i))
  
}

#saveRDS(fitted_m2_480, file = "fc480_m2_lr_fitted.rds")
#saveRDS(residuals_m2_480, file = "fc480_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_480,"res_fit_all_m2_480.rds")
saveRDS(fc_lr_m2_480,"fc_lr_m2_480.rds")

