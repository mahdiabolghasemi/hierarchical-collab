suppressWarnings(suppressMessages(library(tidyverse, quietly = TRUE)))
suppressMessages(library(fable, quietly = TRUE))
suppressMessages(library(progress, quietly = TRUE))

args <- commandArgs(TRUE)

if(length(args)==0){
  mod <- "lgbm"
}else{
  mod <- args[1]
}

if(mod == "lr"){
  path <- file.path("Base", "Linear Reg results", "In-sample-error-LR")
}else if(mod == "lgbm"){
  path <- file.path("Base", "LGBM results", "In-sample-error-lgbm")
}

files_list <- list.files(path, full.names = T)

for(j in c("480", "240", "160", "120", "80", "60", "40", "30", "20", "10")){
  idf <- grep(paste0("^", path, "/res*[[:alpha:][:punct:]⁠]*",j,".rds"),
              files_list)
  res <- readRDS(files_list[idf]) |>
    mutate(ite = as.numeric(factor(iteration, ordered = TRUE)))
  
  pb <- progress_bar$new(
    format = paste0(j, " [:bar] :percent eta: :eta"),
    total = max(res$ite), clear = FALSE, width= 60)
  dir.create(file.path("Errors", mod, "is", j), recursive = TRUE,
             showWarnings = FALSE)
  for(i in 1:max(res$ite)){
    mat <- res |>
      filter(ite == i) |>
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
    
    saveRDS(mat, file = file.path("Errors", mod, "is", j,
                                  paste0(formatC(i, width = nchar(max(res$ite)), 
                                                 format = "d", flag = "0"), 
                                         "_", j, "_", mod, "_is.rds")))
    pb$tick()
  }
}

