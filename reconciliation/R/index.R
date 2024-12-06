suppressWarnings(suppressMessages(library(tidyverse, quietly = TRUE)))
suppressMessages(library(fable, quietly = TRUE))
suppressMessages(library(progress, quietly = TRUE))
dir.create("./scores", recursive = TRUE, showWarnings = FALSE)

for(k in c(10, 60, 480){
  fc_tmp <- readRDS(paste0("Base/Naive-Benchmarks/fc", k, 
                           "_benchmark.rds"))
  
  list_files = list.files("Reco", recursive = TRUE, full.names = TRUE)
  
  results_files <- unname(sapply(list_files, 
                                 function(x)
                                   strsplit(strsplit(basename(x), "[.]")[[1]][1], "_")[[1]]))
  total_score <- NULL
  pb <- progress_bar$new(format = paste0(" [:bar] :percent eta: :eta tot: :elapsed"),
                         total = NCOL(results_files), clear = FALSE, width= 80)
  for(i in which(results_files[4,] == "sntz")){
    load(list_files[i])
    tmp <- fc_tmp |>
      select(Group, Subgroup, Time, .mean, Actual) |>
      rename(Naive = .mean) |>
      full_join(get(paste0("rf_", k)),
                by = c("Group", "Subgroup", "Time", "Actual")) |>
      filter(as.character(Group) == "<aggregated>", as.character(Subgroup) == "<aggregated>") |>
      select(-iteration)|>
      pivot_longer(-c("Group", "Subgroup", "Time", "ite", "Actual")) |>
      mutate(delta = list(c(0, 0.5, 1, 2.5, 5)/100)) |>
      unnest(delta) |>
      mutate(e = Actual-value,
             minval = pmin(value, Actual),
             tol = pmax(delta*minval, 0.0001),
             erel = e/minval,
             fines = ifelse(e<(-tol), abs(e/minval), 0.001),
             nogain = ifelse(e>tol, abs(e/minval), 0.001),
             probf = ifelse(e<(-tol), 1, 0),
             probng = ifelse(e>tol, 1, 0)) |>
      group_by(Group, Subgroup, name, delta) |>
      summarise(finesI = exp(mean(log(fines), na.rm = TRUE)),
                nogainI = exp(mean(log(nogain), na.rm = TRUE)),
                pobfI = sum(probf)/length(probf),
                probngI = sum(probng)/length(probng),
                .groups = "drop")|>
      add_column(k = k,
                 p = results_files[1,i],
                 mod = results_files[2,i],
                 nn = results_files[4,i],
                 res = results_files[5,i], .before = 1)
    total_score <- bind_rows(total_score, tmp)
    pb$tick()
  }
  
  index = total_score |>
    mutate(name = as.character(recode(factor(name), 
                                      !!!c("Naive" = "naive", "base_none" = "base"))), 
           Group = as.character(Group),
           Subgroup = as.character(Subgroup)) |>
    mutate(res = ifelse(name %in% c("base", "naive"), "  ", res),
           res = ifelse(str_detect(name, 'ols$|str$'), "  ", res),
           p = ifelse(str_detect(name, 'csbu'), "  ", p),
           p = ifelse(name %in% c("base", "naive"), "  ", p),
           mod = ifelse(name %in% c("naive"), "naive", mod),
           name = ifelse(name %in% c("naive"), "base", name),
           name = ifelse(name %in% c("csbu_bu"), "ctbu", name),
           name_all = paste(mod, name, res, p, sep = " ")) |>
    unique()
  
  save(index, file = paste0("scores/index", k, ".RData"))
}