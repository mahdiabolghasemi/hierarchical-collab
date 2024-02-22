#### Import everything and prepare data ####

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
# %>%  filter_index("1021-06-10")
#min10_n <- min10_n %>% rename(Time = Time_n)

# compute features on data set
min10_n <- min10_n %>% mutate(
  `WMA48` = slider::slide_dbl(Wind_Speed, mean,
                              .before = 48, .after = -1, .complete = TRUE),
  `WMSD48` = slider::slide_dbl(Wind_Speed, sd,
                               .before = 48, .after = -1, .complete = TRUE),
  `PMA48` = slider::slide_dbl(Power, mean,
                              .before = 48, .after = -1, .complete = TRUE),
  `PMSD48` = slider::slide_dbl(Power, sd,
                               .before = 48, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_wind10` = lag(Wind_Speed, 10),
  `lag_wind11` = lag(Wind_Speed, 11),
  `lag_wind12` = lag(Wind_Speed, 12),
  `lag_wind13` = lag(Wind_Speed, 13),
  `lag_wind14` = lag(Wind_Speed, 14),
  `lag_wind15` = lag(Wind_Speed, 15),
  `lag_wind16` = lag(Wind_Speed, 16),
  `lag_wind17` = lag(Wind_Speed, 17),
  `lag_wind18` = lag(Wind_Speed, 18),
  `lag_wind19` = lag(Wind_Speed, 19),
  `lag_wind20` = lag(Wind_Speed, 20),
  `lag_wind21` = lag(Wind_Speed, 21),
  `lag_wind22` = lag(Wind_Speed, 22),
  `lag_wind23` = lag(Wind_Speed, 23),
  `lag_wind24` = lag(Wind_Speed, 24),
  `lag_wind25` = lag(Wind_Speed, 25),
  `lag_wind26` = lag(Wind_Speed, 26),
  `lag_wind27` = lag(Wind_Speed, 27),
  `lag_wind28` = lag(Wind_Speed, 28),
  `lag_wind29` = lag(Wind_Speed, 29),
  `lag_wind30` = lag(Wind_Speed, 30),
  `lag_wind31` = lag(Wind_Speed, 31),
  `lag_wind32` = lag(Wind_Speed, 32),
  `lag_wind33` = lag(Wind_Speed, 33),
  `lag_wind34` = lag(Wind_Speed, 34),
  `lag_wind35` = lag(Wind_Speed, 35),
  `lag_wind36` = lag(Wind_Speed, 36),
  `lag_wind37` = lag(Wind_Speed, 37),
  `lag_wind38` = lag(Wind_Speed, 38),
  `lag_wind39` = lag(Wind_Speed, 39),
  `lag_wind40` = lag(Wind_Speed, 40),
  `lag_wind41` = lag(Wind_Speed, 41),
  `lag_wind42` = lag(Wind_Speed, 42),
  `lag_wind43` = lag(Wind_Speed, 43),
  `lag_wind44` = lag(Wind_Speed, 44),
  `lag_wind45` = lag(Wind_Speed, 45),
  `lag_wind46` = lag(Wind_Speed, 46),
  `lag_wind47` = lag(Wind_Speed, 47),
  `lag_wind48` = lag(Wind_Speed, 48),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `lag_power10` = lag(Power, 10),
  `lag_power11` = lag(Power, 11),
  `lag_power12` = lag(Power, 12),
  `lag_power13` = lag(Power, 13),
  `lag_power14` = lag(Power, 14),
  `lag_power15` = lag(Power, 15),
  `lag_power16` = lag(Power, 16),
  `lag_power17` = lag(Power, 17),
  `lag_power18` = lag(Power, 18),
  `lag_power19` = lag(Power, 19),
  `lag_power20` = lag(Power, 20),
  `lag_power21` = lag(Power, 21),
  `lag_power22` = lag(Power, 22),
  `lag_power23` = lag(Power, 23),
  `lag_power24` = lag(Power, 24),
  `lag_power25` = lag(Power, 25),
  `lag_power26` = lag(Power, 26),
  `lag_power27` = lag(Power, 27),
  `lag_power28` = lag(Power, 28),
  `lag_power29` = lag(Power, 29),
  `lag_power30` = lag(Power, 30),
  `lag_power31` = lag(Power, 31),
  `lag_power32` = lag(Power, 32),
  `lag_power33` = lag(Power, 33),
  `lag_power34` = lag(Power, 34),
  `lag_power35` = lag(Power, 35),
  `lag_power36` = lag(Power, 36),
  `lag_power37` = lag(Power, 37),
  `lag_power38` = lag(Power, 38),
  `lag_power39` = lag(Power, 39),
  `lag_power40` = lag(Power, 40),
  `lag_power41` = lag(Power, 41),
  `lag_power42` = lag(Power, 42),
  `lag_power43` = lag(Power, 43),
  `lag_power44` = lag(Power, 44),
  `lag_power45` = lag(Power, 45),
  `lag_power46` = lag(Power, 46),
  `lag_power47` = lag(Power, 47),
  `lag_power48` = lag(Power, 48),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
  `is_23` = as.integer(hour(Time) == 23)
)

# save .rds file
saveRDS(min10_n, file = "min10n_features.rds")

# load .rds file
min10_n <- readRDS(file = "min10n_features.rds")

# compute number of entries
N_10 = nrow(min10_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2020-10-01"))
N_10_all = nrow(min10_n %>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min10_n %>% filter(as.Date(Time) < "2021-01-01"))


# initialise objects to save results
fc_lr_m2_10 <- NULL;
res_fit_all_m2_10 <- NULL

start_time_10 <- Sys.time()

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_10,N_10_all,48)) {
  # compute fit
  variables <- setdiff(names(min10_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  
  fit_m2 <- min10_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min10_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1):(i+48))) %>%
    as_tibble() %>% bind_cols(Actual= min10_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1):(i+48)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_10 <- bind_rows(fc_lr_m2_10,fc)
  
  # extract fitted and residuals
  fitted_m2_10 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_10 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_10 <- bind_rows(res_fit_all_m2_10,bind_cols(fitted_m2_10,res=residuals_m2_10$.resid,iteration=i))
}

end_time_10 <- Sys.time()

#saveRDS(fitted_m2_10, file = "fc10_m2_lr_fitted.rds")
#saveRDS(residuals_m2_10, file = "fc10_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_10, "res_fit_all_m2_10.rds")
saveRDS(fc_lr_m2_10, "fc_lr_m2_10.rds")


#### Time series linear regression with feature engineering - 20 minutely ####
min20_n <- min20_power
# %>%  filter_index("2021-06-20")

# compute features on data set
min20_n <- min20_n %>% mutate(
  `WMA24` = slider::slide_dbl(Wind_Speed, mean,
                              .before = 24, .after = -1, .complete = TRUE),
  `WMSD24` = slider::slide_dbl(Wind_Speed, sd,
                               .before = 24, .after = -1, .complete = TRUE),
  `PMA24` = slider::slide_dbl(Power, mean,
                              .before = 24, .after = -1, .complete = TRUE),
  `PMSD24` = slider::slide_dbl(Power, sd,
                               .before = 24, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_wind10` = lag(Wind_Speed, 10),
  `lag_wind11` = lag(Wind_Speed, 11),
  `lag_wind12` = lag(Wind_Speed, 12),
  `lag_wind13` = lag(Wind_Speed, 13),
  `lag_wind14` = lag(Wind_Speed, 14),
  `lag_wind15` = lag(Wind_Speed, 15),
  `lag_wind16` = lag(Wind_Speed, 16),
  `lag_wind17` = lag(Wind_Speed, 17),
  `lag_wind18` = lag(Wind_Speed, 18),
  `lag_wind19` = lag(Wind_Speed, 19),
  `lag_wind20` = lag(Wind_Speed, 20),
  `lag_wind21` = lag(Wind_Speed, 21),
  `lag_wind22` = lag(Wind_Speed, 22),
  `lag_wind23` = lag(Wind_Speed, 23),
  `lag_wind24` = lag(Wind_Speed, 24),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `lag_power10` = lag(Power, 10),
  `lag_power11` = lag(Power, 11),
  `lag_power12` = lag(Power, 12),
  `lag_power13` = lag(Power, 13),
  `lag_power14` = lag(Power, 14),
  `lag_power15` = lag(Power, 15),
  `lag_power16` = lag(Power, 16),
  `lag_power17` = lag(Power, 17),
  `lag_power18` = lag(Power, 18),
  `lag_power19` = lag(Power, 19),
  `lag_power20` = lag(Power, 20),
  `lag_power21` = lag(Power, 21),
  `lag_power22` = lag(Power, 22),
  `lag_power23` = lag(Power, 23),
  `lag_power24` = lag(Power, 24),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
  `is_23` = as.integer(hour(Time) == 23)
)

# save .rds file
saveRDS(min20_n, file = "min20n_features.rds")

# load .rds file
min20_n <- readRDS(file = "min20n_features.rds")


# compute number of entries
N_20 = nrow(min20_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min20_n %>% filter(as.Date(Time) < "2020-10-01"))
N_20_all = nrow(min20_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min20_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_20 <- NULL;
res_fit_all_m2_20 <- NULL

# Start timing
start_time_20 <- Sys.time()

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_20,N_20_all,24)) {
  variables <- setdiff(names(min20_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min20_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min20_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1): (i+24))) %>%
    as_tibble() %>% bind_cols(Actual= min20_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+24)) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_20 <- bind_rows(fc_lr_m2_20,fc)
  
  # extract fitted and residuals
  fitted_m2_20 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_20 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_20 <- bind_rows(res_fit_all_m2_20,bind_cols(fitted_m2_20,res=residuals_m2_20$.resid,iteration=i))
  
}

end_time_20 <- Sys.time()

#saveRDS(fitted_m2_20, file = "fc20_m2_lr_fitted.rds")
#saveRDS(residuals_m2_20, file = "fc20_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_20, "res_fit_all_m2_20.rds")
saveRDS(fc_lr_m2_20, "fc_lr_m2_20.rds")


#### Time series linear regression with feature engineering - 30 minutely ####
min30_n <- min30_power
# %>%  filter_index("2021-06-30")

# compute features on data set
min30_n <- min30_n %>% mutate(
  `WMA16` = slider::slide_dbl(Wind_Speed, mean,
                              .before = 16, .after = -1, .complete = TRUE),
  `WMSD16` = slider::slide_dbl(Wind_Speed, sd,
                               .before = 16, .after = -1, .complete = TRUE),
  `PMA16` = slider::slide_dbl(Power, mean,
                              .before = 16, .after = -1, .complete = TRUE),
  `PMSD16` = slider::slide_dbl(Power, sd,
                               .before = 16, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_wind10` = lag(Wind_Speed, 10),
  `lag_wind11` = lag(Wind_Speed, 11),
  `lag_wind12` = lag(Wind_Speed, 12),
  `lag_wind13` = lag(Wind_Speed, 13),
  `lag_wind14` = lag(Wind_Speed, 14),
  `lag_wind15` = lag(Wind_Speed, 15),
  `lag_wind16` = lag(Wind_Speed, 16),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `lag_power10` = lag(Power, 10),
  `lag_power11` = lag(Power, 11),
  `lag_power12` = lag(Power, 12),
  `lag_power13` = lag(Power, 13),
  `lag_power14` = lag(Power, 14),
  `lag_power15` = lag(Power, 15),
  `lag_power16` = lag(Power, 16),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
  `is_23` = as.integer(hour(Time) == 23)
)

# save .rds file
saveRDS(min30_n, file = "min30n_features.rds")

# load .rds file
min30_n <- readRDS(file = "min30n_features.rds")

# compute number of entries
N_30 = nrow(min30_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min30_n %>% filter(as.Date(Time) < "2020-10-01"))
N_30_all = nrow(min30_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min30_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_30 <- NULL;
res_fit_all_m2_30 <- NULL

start_time_30 <- Sys.time()
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_30,N_30_all,16)) {
  variables <- setdiff(names(min30_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min30_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min30_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1): (i+16))) %>%
    as_tibble() %>% bind_cols(Actual= min30_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+16) ) %>% pull(Power))
  fc= cbind(fc, iteration=i)
  fc_lr_m2_30 <- bind_rows(fc_lr_m2_30,fc)
  
  # extract fitted and residuals
  fitted_m2_30 <- fit_m2 %>% fitted() %>% as_tibble()
  residuals_m2_30 <- fit_m2 %>%  residuals() %>% as_tibble()
  res_fit_all_m2_30 <- bind_rows(res_fit_all_m2_30,bind_cols(fitted_m2_30,res=residuals_m2_30$.resid,iteration=i))
  
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
min40_n <- min40_n %>% rename(Time= Time_n)

# compute features on data set
min40_n <- min40_n %>% mutate(
  `WMA12` = slider::slide_dbl(Wind_Speed, mean,
                              .before = 12, .after = -1, .complete = TRUE),
  `WMSD12` = slider::slide_dbl(Wind_Speed, sd,
                               .before = 12, .after = -1, .complete = TRUE),
  `PMA12` = slider::slide_dbl(Power, mean,
                              .before = 12, .after = -1, .complete = TRUE),
  `PMSD12` = slider::slide_dbl(Power, sd,
                               .before = 12, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_wind10` = lag(Wind_Speed, 10),
  `lag_wind11` = lag(Wind_Speed, 11),
  `lag_wind12` = lag(Wind_Speed, 12),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `lag_power10` = lag(Power, 10),
  `lag_power11` = lag(Power, 11),
  `lag_power12` = lag(Power, 12),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
  `is_23` = as.integer(hour(Time) == 23)
)

# save .rds file
saveRDS(min40_n, file = "min40n_features.rds")

# load .rds file
min40_n <- readRDS(file = "min40n_features.rds")


# compute number of entries
N_40 = nrow(min40_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min40_n %>% filter(as.Date(Time) < "2020-10-01"))
N_40_all = nrow(min40_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min40_n%>% filter(as.Date(Time) < "2021-01-01"))


# initialise objects to save results
fc_lr_m2_40 <- NULL;
res_fit_all_m2_40 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_40,N_40_all,12)) {
  variables <- setdiff(names(min40_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min40_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min40_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1) : (i+12))) %>%
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

#saveRDS(fitted_m2_40, file = "fc40_m2_lr_fitted.rds")
#saveRDS(residuals_m2_40, file = "fc40_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_40, "res_fit_all_m2_40.rds")
saveRDS(fc_lr_m2_40, "fc_lr_m2_m2_40.rds")



#### Time series linear regression with feature engineering - 60 minutely ####
min60_n <- min60_power
# %>%  filter_index("2021-06-30")

# compute features on data set
min60_n <- min60_n %>% mutate(
  `WMA8` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 8, .after = -1, .complete = TRUE),
  `WMSD8` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 8, .after = -1, .complete = TRUE),
  `PMA8` = slider::slide_dbl(Power, mean,
                             .before = 8, .after = -1, .complete = TRUE),
  `PMSD8` = slider::slide_dbl(Power, sd,
                              .before = 8, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
  `is_23` = as.integer(hour(Time) == 23)
)

# save .rds file
saveRDS(min60_n, file = "min60n_features.rds")

# load .rds file
#min60_n <- readRDS(file = "min60n_features.rds")


# compute number of entries
N_60 = nrow(min60_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min60_n %>% filter(as.Date(Time) < "2020-10-01"))
N_60_all = nrow(min60_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min60_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_60 <- NULL;
res_fit_all_m2_60 <- NULL

start_time_60 <- Sys.time()
# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_60,N_60_all,8)) {
  variables <- setdiff(names(min60_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min60_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min60_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1): (i+8))) %>%
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

saveRDS(fitted_m2_60, file = "fc60_m2_lr_fitted.rds")
saveRDS(residuals_m2_60, file = "fc60_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_60, "res_fit_all_m2_60.rds")
saveRDS(fc_lr_m2_60, "fc_lr_m2_60.rds")


#### Time series linear regression with feature engineering - 80 minutely ####
min80_n <- min80_power
# %>%  filter_index("2021-06-30")
min80_n <- min80_n %>% rename(Time = Time_n)

# compute features on data set
min80_n <- min80_n %>% mutate(
  `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 6, .after = -1, .complete = TRUE),
  `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 6, .after = -1, .complete = TRUE),
  `PMA6` = slider::slide_dbl(Power, mean,
                             .before = 6, .after = -1, .complete = TRUE),
  `PMSD6` = slider::slide_dbl(Power, sd,
                              .before = 6, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22),
)

# save .rds file
saveRDS(min80_n, file = "min80n_features.rds")

# load .rds file
min80_n <- readRDS(file = "min80n_features.rds")


# compute number of entries
N_80 = nrow(min80_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min80_n %>% filter(as.Date(Time) < "2020-10-01"))
N_80_all = nrow(min80_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min80_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_80 <- NULL;
res_fit_all_m2_80 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_80,N_80_all,6)) {
  variables <- setdiff(names(min80_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min80_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min80_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1):(i+6))) %>%
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
# %>%  filter_index("2021-06-30")
min120_n <- min120_n %>% rename(Time = Time_n)

# compute features on data set
min120_n <- min120_n %>% mutate(
  `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 4, .after = -1, .complete = TRUE),
  `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 4, .after = -1, .complete = TRUE),
  `PMA4` = slider::slide_dbl(Power, mean,
                             .before = 4, .after = -1, .complete = TRUE),
  `PMSD4` = slider::slide_dbl(Power, sd,
                              .before = 4, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_02` = as.integer(hour(Time) == 2),
  `is_04` = as.integer(hour(Time) == 4),
  `is_06` = as.integer(hour(Time) == 6),
  `is_08` = as.integer(hour(Time) == 8),
  `is_10` = as.integer(hour(Time) == 10),
  `is_12` = as.integer(hour(Time) == 12),
  `is_14` = as.integer(hour(Time) == 14),
  `is_16` = as.integer(hour(Time) == 16),
  `is_18` = as.integer(hour(Time) == 18),
  `is_20` = as.integer(hour(Time) == 20),
  `is_22` = as.integer(hour(Time) == 22)
)

# save .rds file
saveRDS(min120_n, file = "min120n_features.rds")

# load .rds file
min120_n <- readRDS(file = "min120n_features.rds")

# compute number of entries
N_120 = nrow(min120_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min120_n %>% filter(as.Date(Time) < "2020-10-01"))
N_120_all = nrow(min120_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min120_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_120 <- NULL;
res_fit_all_m2_120 <- NULL

start_time_120 <- Sys.time()

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_120, N_120_all, 4)) {
  variables <- setdiff(names(min120_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min120_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min120_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1):(i+4))) %>%
    as_tibble() %>% bind_cols(Actual= min120_n %>%
                                group_by(Group, Subgroup) %>%
                                dplyr::slice((i+1): (i+4)) %>% pull(Power))
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
min160_n <- min160_n %>% rename(Time=Time_n)
# compute features on data set: 

### FROM HERE THE FEATURES ARE A BIT DIFFERENT###
## WE CONSIDER MORE THAN OUR TYPICAL APPROACH SO FAR, JUST TO HAVE ENOUGH FEATURES FOR LOW FREQUENCY DATA##
min160_n <- min160_n %>% mutate(
  `WMA9` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 9, .after = -1, .complete = TRUE),
  `WMSD9` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 9, .after = -1, .complete = TRUE),
  `PMA9` = slider::slide_dbl(Power, mean,
                             .before = 9, .after = -1, .complete = TRUE),
  `PMSD9` = slider::slide_dbl(Power, sd,
                              .before = 9, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_02` = as.integer(hour(Time) == 2),
  `is_05` = as.integer(hour(Time) == 5),
  `is_08` = as.integer(hour(Time) == 8),
  `is_10` = as.integer(hour(Time) == 10),
  `is_13` = as.integer(hour(Time) == 13),
  `is_16` = as.integer(hour(Time) == 16),
  `is_18` = as.integer(hour(Time) == 18),
  `is_21` = as.integer(hour(Time) == 21),
)

# save .rds file
saveRDS(min160_n, file = "min160n_features.rds")

# load .rds file
min160_n <- readRDS(file = "min160n_features.rds")


# compute number of entries
N_160 = nrow(min160_n %>% filter(as.Date(Time) <"2020-10-01"))/n_keys(min160_n%>% filter(as.Date(Time) <"2020-10-01"))
N_160_all = nrow(min160_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min160_n%>% filter(as.Date(Time) < "2021-01-01"))

# initialise objects to save results
fc_lr_m2_160 <- NULL;
res_fit_all_m2_160 <- NULL

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
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min160_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice((i+1): (i+3))) %>%
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

min240_n <- min240_n %>% rename(Time= Time_n)

# compute features on data set
min240_n <- min240_n %>% mutate(
  `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 6, .after = -1, .complete = TRUE),
  `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 6, .after = -1, .complete = TRUE),
  `PMA6` = slider::slide_dbl(Power, mean,
                             .before = 6, .after = -1, .complete = TRUE),
  `PMSD6` = slider::slide_dbl(Power, sd,
                              .before = 6, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_wind4` = lag(Wind_Speed, 4),
  `lag_wind5` = lag(Wind_Speed, 5),
  `lag_wind6` = lag(Wind_Speed, 6),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_04` = as.integer(hour(Time) == 4),
  `is_08` = as.integer(hour(Time) == 8)
)

# save .rds file
saveRDS(min240_n, file = "min240n_features.rds")

# load .rds file
min240_n <- readRDS(file = "min240n_features.rds")

# compute number of entries
N_240 = nrow(min240_n %>% filter(as.Date(Time) < "2020-10-01"))/n_keys(min240_n %>% filter(as.Date(Time) < "2020-10-01"))
N_240_all = nrow(min240_n%>% filter(as.Date(Time) < "2021-01-01"))/n_keys(min240_n%>% filter(as.Date(Time) < "2021-01-01"))


fc_lr_m2_240 <- NULL;
res_fit_all_m2_240 <- NULL

# do our TSCV manually, starting from N% of the dataset up to the second last element
for (i in seq(N_240,N_240_all,2)) {
  variables <- setdiff(names(min240_n), c("Power", "Group", "Subgroup", "Time"))
  formula <- as.formula(paste("Power ~", paste(variables, collapse = " + ")))
  # compute fit
  fit_m2 <- min240_n %>%
    group_by(Group, Subgroup) %>%
    slice_head(n = i) %>%
    model(mod1 = TSLM(formula))
  
  # forecast with new data
  fc <-   fit_m2 %>%
    forecast(new_data = min240_n %>%
               group_by(Group, Subgroup) %>%
               dplyr::slice(i+1, i+2)) %>%
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
# %>%  filter_index("2021-06-30")
min480_n <- min480_n %>% rename(Time = Time_n)

# compute features on data set
min480_n <- min480_n %>% mutate(
  `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                             .before = 3, .after = -1, .complete = TRUE),
  `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                              .before = 3, .after = -1, .complete = TRUE),
  `PMA3` = slider::slide_dbl(Power, mean,
                             .before = 3, .after = -1, .complete = TRUE),
  `PMSD3` = slider::slide_dbl(Power, sd,
                              .before = 3, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_08` = as.integer(hour(Time) == 8)
)

# save .rds file
saveRDS(min480_n, file = "min480n_features.rds")

# load .rds file
min480_n <- readRDS(file = "min480n_features.rds")

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
    model(mod1 = TSLM(formula))
  
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

saveRDS(fitted_m2_480, file = "fc480_m2_lr_fitted.rds")
saveRDS(residuals_m2_480, file = "fc480_m2_lr_residuals.rds")

saveRDS(res_fit_all_m2_480,"res_fit_all_m2_480.rds")
saveRDS(fc_lr_m2_480,"fc_lr_m2_480.rds")

