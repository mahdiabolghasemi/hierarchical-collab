suppressWarnings(suppressMessages(library(tidyverse, quietly = TRUE)))
suppressMessages(library(fable, quietly = TRUE))
suppressMessages(library(progress, quietly = TRUE))
dir.create("./scores", recursive = TRUE, showWarnings = FALSE)

vec <- c(480, 240, 160, 120, 80, 60, 40, 30, 20, 10)
purrr::walk(vec, function(j){
  assign(paste0("fc_", j), readRDS(paste0("Base/Naive-Benchmarks/fc", j, 
                                          "_benchmark.rds")), 
         envir = .GlobalEnv)
})

list_files = list.files("Reco", recursive = TRUE, full.names = TRUE)

results_files <- unname(sapply(list_files, 
                               function(x)
  strsplit(strsplit(basename(x), "[.]")[[1]][1], "_")[[1]]))
total_score <- NULL
pb <- progress_bar$new(format = paste0(" [:bar] :percent eta: :eta tot: :elapsed"),
                       total = NCOL(results_files), clear = FALSE, width= 80)
for(i in 1:NCOL(results_files)){
  load(list_files[i])
  
  purrr::walk(vec, function(j){
    out <- get(paste0("fc_", j)) |>
      select(Group, Subgroup, Time, .mean, Actual) |>
      rename(Naive = .mean) |>
      full_join(get(paste0("rf_", j)),
                by = c("Group", "Subgroup", "Time", "Actual")) |>
      select(-iteration)|>
      pivot_longer(-c("Group", "Subgroup", "Time", "ite", "Actual")) |>
      group_by(Group, Subgroup, name) |>
      summarise(SE = mean(((value-Actual)^2)),
                AE = mean(abs(value-Actual)), 
                .groups = "drop") |>
      add_column(k = j,
                 p = results_files[1,i],
                 mod = results_files[2,i],
                 nn = results_files[4,i],
                 res = results_files[5,i], .before = 1)
    assign(paste0("tmp_", j), out, envir = .GlobalEnv)
  })
  tmp <- do.call("bind_rows", lapply(paste0("tmp_", vec), get))
  total_score <- bind_rows(total_score, tmp)
  pb$tick()
}


total_score = total_score |>
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

save(total_score, file = "scores/total_score.RData")



