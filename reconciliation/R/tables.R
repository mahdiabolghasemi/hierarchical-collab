library(tidyverse)
load("./scores/total_score.RData")
err <- c("AE", "SE")

dir.create("./tables", recursive = TRUE, showWarnings = FALSE)

total_score <- total_score |>
  pivot_longer(cols = any_of(err), names_to = "err") |>
  mutate(res = ifelse(name %in% c("base", "naive"), "  ", res),
         res = ifelse(str_detect(name, 'ols$|str$'), "  ", res),
         p = ifelse(str_detect(name, 'csbu'), "  ", p),
         p = ifelse(name %in% c("base", "naive"), "  ", p),
         mod = ifelse(name %in% c("naive"), "naive", mod),
         name = ifelse(name %in% c("naive"), "base", name),
         name = ifelse(name %in% c("csbu_bu"), "ctbu", name),
         name_all = paste(mod, name, res, p, sep = " ")) |>
  unique() |>
  group_by(across(-c("value"))) |>
  summarise(value = value[1], .groups = "drop")

total_score_bench <- total_score |>
  filter(trimws(name_all) %in% "naive base") |>
  rename("bench" = "value") |>
  select(-name, -mod, -res, -p, -name_all)
tab <- total_score |>
  #filter(name_all %in% input$comb,
  #       k %in% input$te_order,
  #       nn %in% input$nn_sel,
  #       err %in% input$errors) |>
  unique() |>
  left_join(total_score_bench, 
            relationship = "many-to-many",
            by = join_by(k, nn, Group, Subgroup, err)) |>
  mutate(k = factor(k))

out <- tab |>
  group_by(k, p, mod, nn, res, name, name_all, err) |>
  summarise(value = exp(mean(log(value/bench))), .groups = "drop")
out0 <- tab |>
  group_by(p, mod, nn, res, name, name_all, err) |>
  summarise(k = "All", value = exp(mean(log(value/bench))), .groups = "drop")
out <- bind_rows(out, out0) |>
  arrange(k)


tab <- out |>
  filter(mod %in% c("lr0", "lgbm0"), nn == "sntz",
         !grepl("lasso", name, fixed = TRUE),
         name %in% c("base", "csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", "oct_acov", "ite_acov-shr")) |>
  mutate(#p = ifelse(p == "  ", "SB", p),
         #res = ifelse(res == "  ", "is", res),
         k = factor(k, c(seq(10, 480, 10), "All"), ordered = TRUE),
         mod = factor(mod, c("lr0", "lgbm0"), ordered = TRUE),
         mod = recode(mod, "lgbm0"="LGBM", "lr0" = "LR"),
         Errors = recode(res, "is"="in-sample", "vl" = "validation"),
         Strategy = recode(p, "DB"="Decision-based", "SB" = "Statistical-based"),
         name = factor(name, c("base", "csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", 
                               "oct_acov", "ite_acov-shr"), 
                       ordered = TRUE),
         name = recode(name, 
                       "csbu_shr"= "$\\text{pbu}$", 
                       "csbu_shr"= "$\\text{cs}(shr)+\\text{te}(bu)$", 
                       "oct_str" = "$\\text{ct}(str)$", 
                       "oct_wlsv" = "$\\text{ct}(wlsv)$", 
                       "oct_bdshr" = "$\\text{ct}(bdshr)$", 
                       "oct_acov" = "$\\text{ct}(acov)$", 
                       #"ite_acov-shr" = "$\\text{ite}(acov_{\\text{te}}, shr_{\\text{cs}})$",
                       "ite_acov-shr" = "$\\text{ite}$")) |>
  arrange(mod, name, k)

library(kableExtra)
for(errid in c("SE")){
  for(pid in c("DB", "SB")){
    for(rid in c("is", "vl")){
      dfk <- tab |>
        filter(p %in% c(pid, "  "),
               res %in% c(rid, "  "),
               err == errid) |>
        select(k, mod, name, value) |>
        group_by(k, mod) |>
        mutate(rank1 = min(value)) |>
        ungroup() |>
        group_by(k) |>
        mutate(rank2 = min(value)) |>
        ungroup() |>
        mutate(value = cell_spec(sprintf("%.3f", value), "latex", 
                                  bold = value == rank2, 
                                  italic = value == rank1)) |>
        select(-rank1, -rank2) |>
        pivot_wider(names_from = k)
      
      mod_index <- table(dfk$mod)
      names(mod_index) <- paste0(names(mod_index), " base models")
      dfk[, -1] |>
        kbl(format = "latex", digits = 3, booktabs = TRUE, 
            linesep = "",
            align = "lcccccccccc",
            col.names = c("\\multicolumn{1}{l|}{\\textbf{Approach}}", colnames(dfk)[-c(1:2)]),
            escape = FALSE) |>
        add_header_above(c("", "Temporal aggregation orders" = NCOL(dfk)-2), 
                         escape = TRUE, bold = TRUE, line_sep = 0) |>
        pack_rows(index = mod_index,
                  latex_gap_space = "0.3em", 
                  indent = FALSE, escape = FALSE, latex_align = "l", bold = TRUE) |>
        column_spec (1, border_right = T) |>
        save_kable(paste0("./tables/tab_", pid, "_", rid, "_", errid, ".tex"), 
                   self_contained = FALSE)
    }
  }
}


  