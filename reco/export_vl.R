suppressWarnings(suppressMessages(library(tidyverse, quietly = TRUE)))
suppressMessages(library(fable, quietly = TRUE))
suppressMessages(library(progress, quietly = TRUE))

mod <- "lr"

if(mod == "lr"){
  path_val <- file.path("Base", "Linear Reg results", "Validation-error-LR")
  path_tes <- file.path("Base", "Linear Reg results", "In-sample-error-LR")
}else if(mod == "lgbm"){
  path_val <- file.path("Base", "LGBM results", "Validation-error-lgbm")
  path_tes <- file.path("Base", "LGBM results", "In-sample-error-lgbm")
}

files_val <- list.files(path_val, full.names = T)
files_tes <- list.files(path_tes, full.names = T)

# LR
#j = 480
for(j in c("480", "240", "160", "120", "80", "60", "40", "30", "20", "10")){
  idf_val <- grep(paste0("^", path_val, "/fc*[[:graph:]]*_",j,".rds"),
                  files_val)
  idf_tes <- grep(paste0("^", path_tes, "/fc*[[:graph:]]*_",j,".rds"),
                  files_tes)
  
  testfc <- readRDS(files_tes[idf_tes]) |>
    as_tibble() |>
    mutate(res = .mean-Actual) |>
    select(Group, Subgroup, Time, res, iteration)
  
  valfc <- readRDS(files_val[idf_val]) |>
    as_tibble() |>
    mutate(res = .mean-Actual) |>
    select(Group, Subgroup, Time, res, iteration) |>
    filter(Time < min(testfc$Time))
  
  val_err <- bind_rows(valfc, testfc)|>
    mutate(iteration = as.numeric(factor(iteration, ordered = TRUE))) |>
    arrange(iteration)
  
  Nval <- length(unique(valfc$iteration))
  Ntest <- length(unique(testfc$iteration))
  
  pb <- progress_bar$new(
    format = paste0(j, " [:bar] :percent eta: :eta"),
    total = Ntest, clear = FALSE, width= 60)
  dir.create(file.path("Errors", mod, "vl", j), recursive = TRUE,
             showWarnings = FALSE)
  for(i in 1:Ntest){
    mat <- val_err |>
      filter(iteration >= i & iteration < i + Nval) |>
      select(Time, Group, Subgroup, res) |>
      as_tibble() |>
      mutate(Time = format(Time, "%Y-%m-%d %H:%M:%S")) |>
      pivot_wider(names_from = Time, values_from = res) |>
      mutate(Group = factor(Group, c("<aggregated>", "A", "B"), ordered = TRUE),
             Subgroup = factor(Subgroup, c("<aggregated>", paste0("A", 1:15), 
                                           paste0("B", 1:6)), ordered = TRUE)) |>
      arrange(Subgroup, Group) |>
      select(-Subgroup, -Group) |>
      as.matrix() 
    
    saveRDS(mat, file = file.path("Errors", mod, "vl", j,
                                  paste0(formatC(i, width = nchar(Ntest), 
                                                 format = "d", flag = "0"), 
                                         "_", j, "_", mod, "_vl.rds")))
    pb$tick()
  }
}