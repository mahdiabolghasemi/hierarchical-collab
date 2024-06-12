suppressWarnings(suppressMessages(library(tidyverse, quietly = TRUE)))
suppressMessages(library(fable, quietly = TRUE))
suppressMessages(library(progress, quietly = TRUE))
suppressMessages(library(FoReco, quietly = TRUE))
suppressMessages(library(purrr, quietly = TRUE))
#source("fun.R")
args <- commandArgs(TRUE)
eval(parse(text=args))

if(!exists("mod")){
  mod <- "lr"
}

if(!exists("mod_reco")){
  mod_reco <- "oct"
}

if(!exists("nn_reco")){
  nn_reco <- "sntz"
}

if(!exists("res_reco")){
  res_reco <- "is"
}

if(!exists("str_reco")){
  str_reco <- "DB"
}

if(!exists("zero")){
  zero <- 0
}

if(str_reco == "DB"){
  vec <- c(60, 30, 20, 10)
}else{
  vec <- c(480, 240, 160, 120, 80, 60, 40, 30, 20, 10)
}

if(zero == 0){
  drop_zero <- TRUE
}else{
  drop_zero <- FALSE
}

if(mod_reco == "oct"){
  comb_vec <- c("ols", "str", "wlsv", "bdshr", "shr", "acov")
}else if(mod_reco == "ite"){
  eg <- expand.grid(c("wlsv", "acov", "sar1", "strar1", "har1"), 
                    c("wls", "shr"))
  comb_vec <- paste0(eg[,1], "-", eg[,2])
}else if(mod_reco == "csbu"){
  comb_vec <- c("bu", "ols", "str", "wls", "shr")
}else if(mod_reco == "tebu"){
  comb_vec <- c("ols", "str", "wlsv", "acov", "sar1", "strar1", "har1")
}

if(mod == "lr"){
  path <- file.path("Base", "Linear Reg results", "In-sample-error-LR")
}else if(mod == "lgbm"){
  path <- file.path("Base", "LGBM results", "In-sample-error-lgbm")
}
files_list <- list.files(path, full.names = T)

vec_full <- c(480, 240, 160, 120, 80, 60, 40, 30, 20, 10)
walk(vec_full, function(j){
  idf <- grep(paste0("^", path, "/fc*[[:graph:]]*_",j,".rds"),
              files_list)
  
  out <- readRDS(files_list[idf]) |>
    as_tsibble(index = "Time", key = c("Group", "Subgroup")) |>
    mutate(ite = as.numeric(factor(iteration, ordered = TRUE)))
  
  assign(paste0("fc_", j), out, 
         envir = .GlobalEnv)
})

A <- rbind(rep(1, 20),
           c(rep(1, 14), rep(0, 6)),
           c(rep(0, 14), rep(1, 6)))

if(nn_reco == "free"){
  nn_reco <- NULL
}
walk(vec_full, function(j){
  assign(paste0("rf_", j), NULL, envir = .GlobalEnv)
})

vec_ite <- get(paste0("fc_", max(vec))) |> pull(ite) |> unique()
time <- tibble(i = numeric(), 
               mod = character(),
               res = character(),
               reco = character(),
               te = character(),
               comb = character(),
               time = numeric())
pb <- progress_bar$new(format = paste0(mod, ifelse(drop_zero, 0, ""),
                                       " ", mod_reco, " ", 
                                       ifelse(is.null(nn_reco), "free", nn_reco), 
                                       " ", res_reco, " ", str_reco, 
                                       " [:bar] :percent eta: :eta tot: :elapsed"),
                       total = max(vec_ite), clear = FALSE, width= 80)
for(i in 1:max(vec_ite)){
  base <- lapply(vec, function(j){
    get(paste0("fc_", j)) |>
      filter(ite == i) |>
      select(Time, Group, Subgroup, .mean) |>
      as_tibble() |>
      mutate(Time = format(Time, "%Y-%m-%d %H:%M:%S")) |>
      pivot_wider(names_from = Time, values_from = .mean) |>
      mutate(Group = factor(Group, c("<aggregated>", "A", "B"), ordered = TRUE),
             Subgroup = factor(Subgroup, c("<aggregated>", paste0("A", 1:15), 
                                           paste0("B", 1:6)), ordered = TRUE)) |>
      arrange(Subgroup, Group) |>
      select(-Subgroup, -Group) |>
      as.matrix()
  })
  base <- do.call("cbind", base)
  
  if(drop_zero){
    base[base<0] <- 0
  }
  
  res <- lapply(vec, function(j){
    readRDS(file.path("Errors", mod, res_reco, j,
                      paste0(formatC(i, width = nchar(max(vec_ite)), 
                                     format = "d", flag = "0"), 
                             paste0("_", j, "_", mod, "_", res_reco, ".rds"))))
  })
  res <- do.call("cbind", res)
  
  walk(vec_full, function(j){
    out <- get(paste0("fc_", j)) |>
      as_tibble() |>
      filter(ite == i) |>
      select(Time, Group, Subgroup, iteration, 
             ite, .mean, Actual) |>
      mutate(Group2 = factor(Group, 
                             c("<aggregated>", "A", "B"), 
                             ordered = TRUE),
             Subgroup2 = factor(Subgroup, 
                                c("<aggregated>", 
                                  paste0("A", 1:15), 
                                  paste0("B", 1:6)), 
                                ordered = TRUE)) |>
      arrange(Subgroup2, Group2) |>
      rename(base_none = .mean) |>
      select(-Subgroup2, -Group2)
    if(drop_zero){
      out$base_none[out$base_none < 0] <- 0
    }
    assign(paste0("tmp_", j), out, envir = .GlobalEnv)
  })
  
  for(comb in comb_vec){
    if(mod_reco == "oct"){
      start <- Sys.time()
      reco <- ctrec(base, agg_mat = A, agg_order = vec/10,
                    comb = comb, res = res, nn = nn_reco)
      end <- Sys.time()
    }else if(mod_reco == "ite"){
      if(is.null(nn_reco)){
        start <- Sys.time()
        reco <- iterec(telist = list(comb = strsplit(comb, "-")[[1]][1], 
                                     agg_order = vec/10),
                       cslist = list(comb = strsplit(comb, "-")[[1]][2], 
                                     agg_mat = A), 
                       res = res, base = base, verbose = FALSE)
        end <- Sys.time()
      }else if(nn_reco == "sntz"){
        start <- Sys.time()
        reco <- iterec(telist = list(comb = strsplit(comb, "-")[[1]][1], 
                                     agg_order = vec/10),
                       cslist = list(comb = strsplit(comb, "-")[[1]][2], 
                                     agg_mat = A), 
                       res = res, base = base, verbose = FALSE)
        cord <- sapply(colnames(reco), function(x){
          strsplit(x, split = " ")[[1]][1]
        })
        bts <- reco[-c(1:NROW(A)),cord == "k-1"]
        reco <- ctbu(bts, agg_mat = A, agg_order = vec/10, sntz = TRUE)
        end <- Sys.time()
      }else{
        start <- Sys.time()
        reco <- iterec(telist = list(comb = strsplit(comb, "-")[[1]][1], 
                                     agg_order = vec/10, nn = nn_reco),
                       cslist = list(comb = strsplit(comb, "-")[[1]][2], 
                                     agg_mat = A, nn = nn_reco), 
                       res = res, base = base, verbose = FALSE)
        end <- Sys.time()
      }
    }else if(mod_reco == "csbu"){
      base <- get(paste0("fc_", min(vec))) |>
        filter(ite == i) |>
        select(Time, Group, Subgroup, .mean) |>
        as_tibble() |>
        mutate(Time = format(Time, "%Y-%m-%d %H:%M:%S")) |>
        pivot_wider(names_from = Time, values_from = .mean) |>
        mutate(Group = factor(Group, c("<aggregated>", "A", "B"), ordered = TRUE),
               Subgroup = factor(Subgroup, c("<aggregated>", paste0("A", 1:15), 
                                             paste0("B", 1:6)), ordered = TRUE)) |>
        arrange(Subgroup, Group) |>
        select(-Subgroup, -Group) |>
        as.matrix() |>
        t()
      
      if(drop_zero){
        base[base<0] <- 0
      }
      
      if(comb == "bu"){
        start <- Sys.time()
        reco <- ctbu(t(base[, -c(1:NROW(A))]), agg_mat = A, 
                     agg_order = vec/10)
        end <- Sys.time()
      }else{
        res <- readRDS(file.path("Errors", mod, res_reco, min(vec),
                                 paste0(formatC(i, width = nchar(max(vec_ite)), 
                                                format = "d", flag = "0"), 
                                        paste0("_", min(vec), "_", mod, "_", 
                                               res_reco, ".rds"))))
        res <- t(res)
        start <- Sys.time()
        hfts <- t(csrec(base = base, agg_mat = A, res = res, 
                        nn = nn_reco, comb = comb))
        reco <- ctbu(hfts[-c(1:NROW(A)),], agg_mat = A, 
                     agg_order = vec/10)
        end <- Sys.time()
      }
    }else if(mod_reco == "tebu"){
      start <- Sys.time()
      bts <- t(sapply((NROW(A)+1):sum(dim(A)), function(x){
        terec(base = base[x,], agg_order = vec/10, res = res[x,], 
              nn = nn_reco, comb = comb)
      }))
      cord <- sapply(colnames(bts), function(x){
        strsplit(x, split = " ")[[1]][1]
      })
      bts <- bts[,cord == "k-1"]
      reco <- ctbu(bts, agg_mat = A, agg_order = vec/10)
      end <- Sys.time()
    }
    time <- time |>
      add_row(i = i, 
              mod = paste0(mod, ifelse(drop_zero, 0, "")),
              res = res_reco,
              reco = mod_reco,
              te = str_reco,
              comb = comb,
              time = as.numeric(end-start, units = "secs"))
    
    
    reco <- ctbu(t(FoReco2matrix(reco)$"k-1"[, -c(1:NROW(A))]), 
                 agg_mat = A, agg_order = vec_full/10)
    reco <- FoReco2matrix(reco)
    walk(vec_full, function(j){
      out <- get(paste0("tmp_", j)) |>
        add_column(!!paste0(mod_reco, "_", comb) := as.vector(reco[[paste0("k-", j/10)]]))
      assign(paste0("tmp_", j), out, envir = .GlobalEnv)
    })
  }
  
  walk(vec_full, function(j){
    assign(paste0("rf_", j), bind_rows(get(paste0("rf_", j)), 
                                       get(paste0("tmp_", j))), envir = .GlobalEnv)
  })
  pb$tick()
}

dir.create(file.path("Reco", paste0(mod, ifelse(drop_zero, 0, ""))), 
           recursive = TRUE, showWarnings = FALSE)
save(list = c(paste0("rf_", vec_full), "time"), 
     file = paste0("Reco/", mod, ifelse(drop_zero, 0, ""), 
                   "/", str_reco, "_", mod, ifelse(drop_zero, 0, ""),
                   "_", mod_reco, "_", 
                   ifelse(is.null(nn_reco), "free", nn_reco), "_", 
                   res_reco, ".RData"))
