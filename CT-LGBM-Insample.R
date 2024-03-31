####Optimize the hyperparameters of LightGBM model#####

library(dplyr)
library(tsibble)
library("readxl")
library(fpp3)
library(zoo)
library(stats)
library(foreach)
library(doParallel)
library(lightgbm)
library(distributional)
library(rBayesianOptimization)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

# set the proportion of training set
TrainingProportion <- 0.90
# set number of threads
numThreads = 1;

#### Load .rds files ####
min10_power <- readRDS(file = "min10.rds")
min20_power <- readRDS(file = "min20.rds")
min30_power <- readRDS(file = "min30.rds")
min40_power <- readRDS(file = "min40.rds")
min60_power <- readRDS(file = "min60.rds")
min80_power <- readRDS(file = "min80.rds")
min120_power <- readRDS(file = "min120.rds")
min160_power <- readRDS(file = "min160.rds")
min240_power <- readRDS(file = "min240.rds")
min480_power <- readRDS("min480.rds")

min480_n <- readRDS(file = "min480n_features.rds") %>% select(-c("Wind_Speed"))
min240_n <- readRDS(file = "min240n_features.rds")%>% select(-c("Wind_Speed"))
min160_n <- readRDS(file = "min160n_features.rds")%>% select(-c("Wind_Speed"))
min120_n <- readRDS(file = "min120n_features.rds")%>% select(-c("Wind_Speed"))
min80_n <- readRDS(file = "min80n_features.rds")%>% select(-c("Wind_Speed"))
min60_n <- readRDS(file = "min60n_features.rds")%>% select(-c("Wind_Speed"))
min40_n <- readRDS(file = "min40n_features.rds")%>% select(-c("Wind_Speed"))
min30_n <- readRDS(file = "min30n_features.rds")%>% select(-c("Wind_Speed"))
min20_n <- readRDS(file = "min20n_features.rds")%>% select(-c("Wind_Speed"))
min10_n <- readRDS(file = "min10n_features.rds")%>% select(-c("Wind_Speed"))


#### Define auxiliary functions for LightGBM regression ####
# function to convert x_data (i.e., all regressors) tsibble into matrix for lightgbm
to_x <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(-c(Group, Subgroup, Time, Energy, Wind_Speed)) %>%
           as.matrix())
}

# function to convert y data (i.e., Energy) tsibble into matrix for lightgbm
to_y <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(Energy) %>%
           as.matrix())
}


### Bayesian optimisation to optimize hyperparameters
library(lightgbm)
library(rBayesianOptimization)

# Define the objective function to be optimized
optimize_function <- function(learning_rate, max_depth, num_leaves, min_data_in_leaf) {
  params <- list(
    objective = "regression",
    metric = "rmse",
    learning_rate = learning_rate,
    max_depth = as.integer(max_depth),
    num_leaves = as.integer(num_leaves),
    min_data_in_leaf = as.integer(min_data_in_leaf)
  )
  
  # Explicitly filter numeric columns and ensure no dimension mismatch
  numeric_train_data <- Filter(is.numeric, train_data)
  numeric_valid_data <- Filter(is.numeric, valid_data)
  
  numeric_train_lable <- Filter(is.numeric, train_label)
  numeric_valid_lable <- Filter(is.numeric, valid_label)
  
  
  # Sanity check: Ensure no dimension mismatch
  if (ncol(numeric_train_data) * nrow(numeric_train_data) != length(unlist(numeric_train_data)) ||
      ncol(numeric_valid_data) * nrow(numeric_valid_data) != length(unlist(numeric_valid_data))) {
    stop("Dimension mismatch in data conversion to matrix.")
  }
  
  # Convert to matrix
  numeric_train_data <- as.matrix(numeric_train_data)
  numeric_valid_data <- as.matrix(numeric_valid_data)
  train_label_data <- (numeric_train_lable)
  valid_label_data <- (numeric_valid_lable)
  
  
  tryCatch({
    dtrain <- lgb.Dataset(data = numeric_train_data, label = train_label_data)
    dvalid <- lgb.Dataset(data = numeric_valid_data, label = valid_label_data)
    
    model <- lgb.train(params = params, data = dtrain, nrounds = 100, early_stopping_rounds = 10,
                       valids = list(val = dvalid))
    
    pred <- predict(model, numeric_valid_data)
    # Check predictions for non-finite values
    if (any(!is.finite(pred))) {
      pred <- 1
    }
    
    loss <- sqrt(mean((valid_label - pred)^2))  #  RMSE for minimisation (negative rmse for maximisa)
    
    # Ensure loss is finite
    if (!is.finite(loss)) {
      loss <- 1e6
    }
  })
  return(list(Score = -loss, Pred = pred))
}


# Define the bounds for the hyperparameters
bounds <- list(learning_rate = c(0.01, 0.1),
               max_depth = c(6L, 10L),
               num_leaves = c(50L, 200L),
               min_data_in_leaf = c(100L, 250L))



# Perform Bayesian Optimization

### Min power data 480 ####

train_data<- min480_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min480_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min480_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min480_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)



bayes_result_480 <- BayesianOptimization(optimize_function,
                                         bounds = bounds,
                                         init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                         n_iter = 10,  # Number of iterations to perform
                                         acq = "ucb",  # Acquisition function
                                         verbose = TRUE)

bayes_opt_result_480 <- bayes_result_480$Best_Par

### 240 hyperparameters ####
train_data<- min240_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min240_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min240_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min240_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_240 <- BayesianOptimization(optimize_function,
                                             bounds = bounds,
                                             init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                             n_iter = 10,  # Number of iterations to perform
                                             acq = "ucb",  # Acquisition function
                                             verbose = TRUE)

bayes_opt_result_240 <- bayes_result_240$Best_Par

### 160 hyperparameters optimisation####
train_data<- min160_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min160_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min160_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min160_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_160 <- BayesianOptimization(optimize_function,
                                         bounds = bounds,
                                         init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                         n_iter = 10,  # Number of iterations to perform
                                         acq = "ei",  # Acquisition function
                                         verbose = TRUE)

bayes_opt_result_160 <- bayes_result_160$Best_Par

### 120 hyperparameters optimisation####
train_data<- min120_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min120_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min120_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min120_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_120 <- BayesianOptimization(optimize_function,
                                         bounds = bounds,
                                         init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                         n_iter = 10,  # Number of iterations to perform
                                         acq = "ei",  # Acquisition function
                                         verbose = TRUE)

bayes_opt_result_120 <- bayes_result_120$Best_Par

### 80 hyperparameters optimisation####
train_data<- min80_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min80_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min80_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min80_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_80 <- BayesianOptimization(optimize_function,
                                         bounds = bounds,
                                         init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                         n_iter = 10,  # Number of iterations to perform
                                         acq = "ei",  # Acquisition function
                                         verbose = TRUE)

bayes_opt_result_80 <- bayes_result_80$Best_Par


### 60 hyperparameters optimisation####
train_data<- min60_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min60_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min60_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min60_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_60 <- BayesianOptimization(optimize_function,
                                        bounds = bounds,
                                        init_points = 2,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                        n_iter = 1,  # Number of iterations to perform
                                        acq = "ucb",  # Acquisition function
                                        verbose = TRUE)

bayes_opt_result_60 <- bayes_result_60$Best_Par

### 40 hyperparameters optimisation####
train_data<- min40_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min40_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min40_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min40_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_40 <- BayesianOptimization(optimize_function,
                                        bounds = bounds,
                                        init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                        n_iter = 15,  # Number of iterations to perform
                                        acq = "ucb",  # Acquisition function
                                        verbose = TRUE)

bayes_opt_result_40 <- bayes_result_40$Best_Par

### 30 hyperparameters optimisation####
train_data<- min30_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min30_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min30_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min30_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_30 <- BayesianOptimization(optimize_function,
                                        bounds = bounds,
                                        init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                        n_iter = 15,  # Number of iterations to perform
                                        acq = "ei",  # Acquisition function
                                        verbose = TRUE)

bayes_opt_result_30 <- bayes_result_30$Best_Par

### 20 hyperparameters optimisation####
train_data<- min20_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min20_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min20_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min20_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_20 <- BayesianOptimization(optimize_function,
                                        bounds = bounds,
                                        init_points = 20,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                        n_iter = 15,  # Number of iterations to perform
                                        acq = "ei",  # Acquisition function
                                        verbose = TRUE)

bayes_opt_result_20 <- bayes_result_20$Best_Par


### 10 hyperparameters optimisation ####
train_data<- min10_n %>% filter(Time < as.Date("2020-10-01")) %>% filter(!is_aggregated(Subgroup))
train_label<- min10_n %>% filter(Time < as.Date("2020-10-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

valid_data <- min10_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup))
valid_label<- min10_n %>% filter(Time > as.Date("2020-10-01") & Time < as.Date("2021-01-01"))  %>% filter(!is_aggregated(Subgroup)) %>% pull(Power)

bayes_result_10 <- BayesianOptimization(optimize_function,
                                        bounds = bounds,
                                        init_points = 25,  # Number of randomly chosen points to sample the target function before fitting the Gaussian process (GP)
                                        n_iter = 15,  # Number of iterations to perform
                                        acq = "ucb",  # Acquisition function
                                        verbose = TRUE)

bayes_opt_result_10 <- bayes_result_10$Best_Par




#### Time series LightGBM with feature engineering - base forecasts ####

###lgbm forecast 10 ####
# compute number of entries
N_10 = nrow(min10_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2021-01-01"))
N_10_all = nrow(min10_n)/n_keys(min10_n)

fitted_m1_lgbm_10 <- NULL;
res_fit_all_lgbm_10 <- NULL
fc_lgbm_10 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element

# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_10, N_10_all-48, 48)),
  format = "[:bar] :percent :elapsedfull"
)

for (i in seq(N_10,N_10_all-48,48)) {
  
  variables <- setdiff(names(min10_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  
  # compute fit
  fit_m1 <- min10_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_10)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+7)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+8)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+9)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+10)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+11)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+12)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+13)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+14)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+15)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+16)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+17)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+18)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+19)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+20)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+21)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+22)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+23)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+24)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+25)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+26)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+27)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+28)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+29)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+30)),
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+31)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+32)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+33)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+34)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+35)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+36)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+37)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+38)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+39)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+40)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+41)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+42)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+43)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+44)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+45)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+46)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+47)), 
                    fit_m1 %>%
                      forecast(new_data = min10_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+48)))%>%
    as_tibble() %>% bind_cols(Actual= min10_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+48)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_10 <- bind_rows(fc_lgbm_10,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_10 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_10 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_10 <- bind_rows(res_fit_all_lgbm_10,bind_cols(fitted_m1_lgbm_10,res=residuals_m1_lgbm_10$.resid,iteration=i))
  pb$tick()
}

saveRDS(fitted_m1_lgbm_10, file = "fc10_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_10, file = "fc10_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_10,"res_fit_all_lgbm_10.rds")
saveRDS(fc_lgbm_10,"fc_m1_lgbm_10.rds")




###lgbm forecast 20 ####
# compute number of entries
N_20 = nrow(min20_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min20_n %>% filter(as.Date(Time) < "2021-01-01"))
N_20_all = nrow(min20_n)/n_keys(min20_n)

fitted_m1_lgbm_20 <- NULL;
res_fit_all_lgbm_20 <- NULL
fc_lgbm_20 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element

# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_20, N_20_all-24, 24)),
  format = "[:bar] :percent :elapsedfull"
)

for (i in seq(N_20,N_20_all-24,24)) {
  
  variables <- setdiff(names(min20_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  
  # compute fit
  fit_m1 <- min20_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_20)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)),
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+7)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+8)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+9)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+10)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+11)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+12)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+13)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+14)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+15)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+16)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+17)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+18)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+19)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+20)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+21)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+22)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+23)), 
                    fit_m1 %>%
                      forecast(new_data = min20_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+24)))%>%
    as_tibble() %>% bind_cols(Actual= min20_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+24)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_20 <- bind_rows(fc_lgbm_20,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_20 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_20 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_20 <- bind_rows(res_fit_all_lgbm_20,bind_cols(fitted_m1_lgbm_20,res=residuals_m1_lgbm_20$.resid,iteration=i))
  pb$tick()
}

saveRDS(fitted_m1_lgbm_20, file = "fc20_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_20, file = "fc20_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_20,"res_fit_all_lgbm_20.rds")
saveRDS(fc_lgbm_20,"fc_m1_lgbm_20.rds")


###lgbm forecast 30 ####
# compute number of entries
N_30 = nrow(min30_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min30_n %>% filter(as.Date(Time) < "2021-01-01"))
N_30_all = nrow(min30_n)/n_keys(min30_n)

fitted_m1_lgbm_30 <- NULL;
res_fit_all_lgbm_30 <- NULL
fc_lgbm_30 <- NULL

# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_30, N_30_all-16, 16)),
  format = "[:bar] :percent :elapsedfull"
)

for (i in seq(N_30,N_30_all-16,16)) {
  
  variables <- setdiff(names(min30_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  
  # compute fit
  fit_m1 <- min30_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_30)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)),
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+7)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+8)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+9)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+10)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+11)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+12)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+13)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+14)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+15)), 
                    fit_m1 %>%
                      forecast(new_data = min30_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+16)))%>%
    as_tibble() %>% bind_cols(Actual= min30_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+16)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_30 <- bind_rows(fc_lgbm_30,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_30 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_30 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_30 <- bind_rows(res_fit_all_lgbm_30,bind_cols(fitted_m1_lgbm_30,res=residuals_m1_lgbm_30$.resid,iteration=i))
  pb$tick()
}

saveRDS(fitted_m1_lgbm_30, file = "fc30_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_30, file = "fc30_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_30,"res_fit_all_lgbm_30.rds")
saveRDS(fc_lgbm_30,"fc_m1_lgbm_30.rds")

###lgbm forecast 40 ####
# compute number of entries
N_40 = nrow(min40_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min40_n %>% filter(as.Date(Time) < "2021-01-01"))
N_40_all = nrow(min40_n)/n_keys(min40_n)

fitted_m1_lgbm_40 <- NULL;
res_fit_all_lgbm_40 <- NULL
fc_lgbm_40 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element

# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_40, N_40_all-12, 12)),
  format = "[:bar] :percent :elapsedfull"
)

for (i in seq(N_40,N_40_all-12,12)) {
  
  variables <- setdiff(names(min40_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  
  # compute fit
  fit_m1 <- min40_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(lgbm(formula=formula, para_bayes= bayes_opt_result_40)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)),
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+7)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+8)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+9)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+10)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+11)), 
                    fit_m1 %>%
                      forecast(new_data = min40_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+12)))%>%
    as_tibble() %>% bind_cols(Actual= min40_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+12)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_40 <- bind_rows(fc_lgbm_40,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_40 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_40 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_40 <- bind_rows(res_fit_all_lgbm_40,bind_cols(fitted_m1_lgbm_40,res=residuals_m1_lgbm_40$.resid,iteration=i))
  pb$tick()
}

saveRDS(fitted_m1_lgbm_40, file = "fc40_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_40, file = "fc40_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_40,"res_fit_all_lgbm_40.rds")
saveRDS(fc_lgbm_40,"fc_m1_lgbm_40.rds")



###lgbm forecast 60 ####
# compute number of entries
N_60 = nrow(min60_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min60_n %>% filter(as.Date(Time) < "2021-01-01"))
N_60_all = nrow(min60_n)/n_keys(min60_n)

fitted_m1_lgbm_60 <- NULL;
res_fit_all_lgbm_60 <- NULL
fc_lgbm_60 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_60,N_60_all-8,8)) {
  
  variables <- setdiff(names(min60_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  

  # compute fit
  fit_m1 <- min60_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_60)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)),
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+7)), 
                    fit_m1 %>%
                      forecast(new_data = min60_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+8)))%>%
    as_tibble() %>% bind_cols(Actual= min60_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+8)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_60 <- bind_rows(fc_lgbm_60,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_60 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_60 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_60 <- bind_rows(res_fit_all_lgbm_60,bind_cols(fitted_m1_lgbm_60,res=residuals_m1_lgbm_60$.resid,iteration=i))
}

saveRDS(fitted_m1_lgbm_60, file = "fc60_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_60, file = "fc60_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_60,"res_fit_all_lgbm_60.rds")
saveRDS(fc_lgbm_60,"fc_m1_lgbm_60.rds")


###lgbm forecast 80 ####
# compute number of entries
N_80 = nrow(min80_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min80_n %>% filter(as.Date(Time) < "2021-01-01"))
N_80_all = nrow(min80_n)/n_keys(min80_n)

fitted_m1_lgbm_80 <- NULL;
res_fit_all_lgbm_80 <- NULL
fc_lgbm_80 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_80,N_80_all-6,6)) {
  
  variables <- setdiff(names(min80_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  #dtrain <- lgb.Dataset(data = numeric_train_data, label = train_label_data)
  #dvalid <- lgb.Dataset(data = numeric_valid_data, label = valid_label_data)
  
  
  # compute fit
  fit_m1 <- min80_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_80)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min80_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),fit_m1 %>%
                      forecast(new_data = min80_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min80_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                        forecast(new_data = min80_n %>%
                                   group_by(Group, Subgroup) %>%
                                   dplyr::slice(i+4)), 
                    fit_m1 %>%
                      forecast(new_data = min80_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+5)), 
                    fit_m1 %>%
                      forecast(new_data = min80_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+6)))%>%
    as_tibble() %>% bind_cols(Actual= min80_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1) : (i+6)) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_80 <- bind_rows(fc_lgbm_80,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_80 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_80 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_80 <- bind_rows(res_fit_all_lgbm_80,bind_cols(fitted_m1_lgbm_80,res=residuals_m1_lgbm_80$.resid,iteration=i))
}

saveRDS(fitted_m1_lgbm_80, file = "fc80_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_80, file = "fc80_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_80,"res_fit_all_lgbm_80.rds")
saveRDS(fc_lgbm_80,"fc_m1_lgbm_80.rds")

###lgbm forecast 120 ####
# compute number of entries
N_120 = nrow(min120_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min120_n %>% filter(as.Date(Time) < "2021-01-01"))
N_120_all = nrow(min120_n)/n_keys(min120_n)

fitted_m1_lgbm_120 <- NULL;
res_fit_all_lgbm_120 <- NULL
fc_lgbm_120 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_120,N_120_all-4,4)) {
  
  variables <- setdiff(names(min120_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  #dtrain <- lgb.Dataset(data = numeric_train_data, label = train_label_data)
  #dvalid <- lgb.Dataset(data = numeric_valid_data, label = valid_label_data)
  
  
  # compute fit
  fit_m1 <- min120_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_120)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min120_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),fit_m1 %>%
                      forecast(new_data = min120_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2)), 
                    fit_m1 %>%
                      forecast(new_data = min120_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+3)),
                    fit_m1 %>%
                      forecast(new_data = min120_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+4)))  %>%
    as_tibble() %>% bind_cols(Actual= min120_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1, i+2, i+3, i+4) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_120 <- bind_rows(fc_lgbm_120,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_120 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_120 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_120 <- bind_rows(res_fit_all_lgbm_120,bind_cols(fitted_m1_lgbm_120,res=residuals_m1_lgbm_120$.resid,iteration=i))
}

saveRDS(fitted_m1_lgbm_120, file = "fc120_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_120, file = "fc120_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_120,"res_fit_all_lgbm_120.rds")
saveRDS(fc_lgbm_120,"fc_m1_lgbm_120.rds")


###lgbm forecast 160 ####
# compute number of entries
N_160 = nrow(min160_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min160_n %>% filter(as.Date(Time) < "2021-01-01"))
N_160_all = nrow(min160_n)/n_keys(min160_n)

fitted_m1_lgbm_160 <- NULL;
res_fit_all_lgbm_160 <- NULL
fc_lgbm_160 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element

# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_160, N_160_all-3, 3)),
  format = "[:bar] :percent :elapsedfull"
)

for (i in seq(N_160,N_160_all-3,3)) {
  
  variables <- setdiff(names(min160_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  #dtrain <- lgb.Dataset(data = numeric_train_data, label = train_label_data)
  #dvalid <- lgb.Dataset(data = numeric_valid_data, label = valid_label_data)
  
  
  # compute fit
  fit_m1 <- min160_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_160)) 
  
  # forecast with new data
  fc <-  bind_rows(fit_m1 %>%
    forecast(new_data = min160_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice(i+1)),fit_m1 %>%
      forecast(new_data = min160_n %>%
                 group_by(Group, Subgroup) %>%
                 dplyr::slice(i+2)), 
               fit_m1 %>%
                 forecast(new_data = min160_n %>%
                            group_by(Group, Subgroup) %>%
                            dplyr::slice(i+3)))  %>%
    as_tibble() %>% bind_cols(Actual= min160_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1, i+2, i+3) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_160 <- bind_rows(fc_lgbm_160,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_160 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_160 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_160 <- bind_rows(res_fit_all_lgbm_160,bind_cols(fitted_m1_lgbm_160,res=residuals_m1_lgbm_160$.resid,iteration=i))
pb$tick()
}

saveRDS(fitted_m1_lgbm_160, file = "fc160_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_160, file = "fc160_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_160,"res_fit_all_lgbm_160.rds")
saveRDS(fc_lgbm_160,"fc_m1_lgbm_160.rds")




###lgbm forecast 240 ####
# compute number of entries
N_240 = nrow(min240_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min240_n %>% filter(as.Date(Time) < "2021-01-01"))
N_240_all = nrow(min240_n)/n_keys(min240_n)

fitted_m1_lgbm_240 <- NULL;
res_fit_all_lgbm_240 <- NULL
fc_lgbm_240 <- NULL
# do our TSCV manually, starting from N% of the dataset up to the second last element
# Initialize progress bar
pb <- progress_bar$new(
  total = length(seq(N_240, N_240_all-2, 2)),
  format = "[:bar] :percent :elapsedfull"
)
for (i in seq(N_240,N_240_all-2,2)) {
  
  variables <- setdiff(names(min240_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  # compute fit
  fit_m1 <- min240_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_240)) 
  
  # forecast with new data
  fc <-   bind_rows(fit_m1 %>%
                      forecast(new_data = min240_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+1)),
                    fit_m1 %>%
                      forecast(new_data = min240_n %>%
                                 group_by(Group, Subgroup) %>%
                                 dplyr::slice(i+2))) %>%
    as_tibble() %>% bind_cols(Actual= min240_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1, i+2) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_240 <- bind_rows(fc_lgbm_240,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_240 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_240 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_240 <- bind_rows(res_fit_all_lgbm_240,bind_cols(fitted_m1_lgbm_240,res=residuals_m1_lgbm_240$.resid,iteration=i))
pb$tick()
}

saveRDS(fitted_m1_lgbm_240, file = "fc240_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_240, file = "fc240_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_240,"res_fit_all_lgbm_240.rds")
saveRDS(fc_lgbm_240,"fc_m1_lgbm_240.rds")


##### lgbm forecast 480 ####
N_480 = nrow(min480_n  %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min480_n %>% filter(as.Date(Time) < "2021-01-01"))
N_480_all = nrow(min480_n)/n_keys(min480_n)

fitted_m1_lgbm_480 <- NULL;
res_fit_all_lgbm_480 <- NULL
fc_lgbm_480 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_480,N_480_all-1,1)) {
  
  variables <- setdiff(names(min480_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  #dtrain <- lgb.Dataset(data = numeric_train_data, label = train_label_data)
  #dvalid <- lgb.Dataset(data = numeric_valid_data, label = valid_label_data)
  
  
  # compute fit
  fit_m1 <- min480_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(formula=formula, para_bayes= bayes_opt_result_240)) 

    # forecast with new data
  fc <-   fit_m1 %>%
    forecast(new_data = min480_n %>%
               group_by(Group, Subgroup) %>%
               #select(-Power) %>%
               dplyr::slice(i+1)) %>%
    as_tibble() %>% bind_cols(Actual= min480_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice(i+1) %>% pull(Power))
  
  fc= cbind(fc, iteration=i)
  fc_lgbm_480 <- bind_rows(fc_lgbm_480,fc)
  
  # extract fitted and residuals
  fitted_m1_lgbm_480 <- fit_m1 %>% fitted() %>% as_tibble()
  residuals_m1_lgbm_480 <- fit_m1 %>%  residuals() %>% as_tibble()
  res_fit_all_lgbm_480 <- bind_rows(res_fit_all_lgbm_480,bind_cols(fitted_m1_lgbm_480,res=residuals_m1_lgbm_480$.resid,iteration=i))
}

saveRDS(fitted_m1_lgbm_480, file = "fc480_m1_lgbm_fitted.rds")
saveRDS(residuals_m1_lgbm_480, file = "fc480_m1_lgbm_residuals.rds")

saveRDS(res_fit_all_lgbm_480,"res_fit_all_lgbm_480.rds")
saveRDS(fc_lgbm_480,"fc_m1_lgbm_480.rds")
