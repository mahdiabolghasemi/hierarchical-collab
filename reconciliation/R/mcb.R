library(tidyverse)
load("./scores/total_score.RData")
err <- c("AE", "SE")

dir.create("./img", recursive = TRUE, showWarnings = FALSE)

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


tab <- total_score |>
  filter(mod %in% c("lr0", "lgbm0"), nn == "sntz",
         !grepl("lasso", name, fixed = TRUE),
         name %in% c("base", "csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", "oct_acov", "ite_acov-shr"))

nemenyi_fun <- function(data){
  nemenyi <- tsutils::nemenyi(data, plottype = "none")
  df_plot <- full_join(as_tibble(nemenyi$means, rownames = "name"), 
                       full_join(rename(as_tibble(nemenyi$means-nemenyi$cd/2, rownames = "name"), "l" = "value"),
                                 rename(as_tibble(nemenyi$means+nemenyi$cd/2, rownames = "name"), "u" = "value"), 
                                 by = "name"), by = "name") |>
    arrange(value) |>
    mutate(#name = gsub(" ", "", name),
      name = paste0(name, " - ", format(round(value, 2), width = 5, nsmall = 2))) |>
    add_column(fpval = nemenyi$fpval,
               fH = nemenyi$fH)
  df_plot$col <- df_plot$l <= df_plot$u[1]
  
  as_tibble(df_plot)
}

dfk <- NULL
for(errid in c("SE")){
  for(pid in c("DB", "SB")){
    for(rid in c("is", "vl")){
      dfk <- bind_rows(dfk, tab |>
        filter(p %in% c(pid, "  "),
               res %in% c(rid, "  "),
               err == errid) |>
        select(k, mod, Group, Subgroup, name, value)|>
        #group_by(mod) |>
        nest() |>
        mutate(data = map(data, pivot_wider, names_from = c(mod, name), names_sep = " "),
               data = map(data, function(x) x[, -c(1:3)]),
               data = map(data, nemenyi_fun)) |>
        unnest(cols = c(data)) |>
        ungroup() |>
        arrange(value) |>
        mutate(#name = factor(name, unique(name), ordered = TRUE),
          p = pid, res = rid, err = errid,
          pch_name = str_detect(name, "base") | str_detect(name, "naive")))
      
    }
  }
}

out <- dfk |>
  separate(name, into = c("mod", "name"), sep = " ") |>
  mutate(name = recode(name, 
                       "csbu_shr"= "pbu", 
                       #"csbu_shr"= "cs(shr)+te(bu)", 
                       "oct_str" = "ct(str)", 
                       "oct_wlsv" = "ct(wlsv)", 
                       "oct_bdshr" = "ct(bdshr)", 
                       "oct_acov" = "ct(acov)", 
                       #"ite_acov-shr" = "ite(acov[te]*','~shr[cs])",
                       "ite_acov-shr" = "ite"),
         mod = recode(mod, "lgbm0"="LGBM", "lr0" = "LR"),
         name_all = paste0(name, "~", "bold(", mod, ")~", format(round(value, 2), width = 5, nsmall = 2))) |>
  arrange(value) |>
  mutate(name_all = factor(name_all, unique(name_all), ordered = TRUE)) 

plotmcb <- out |> 
  mutate(mod = "all", 
         Errors = recode(res, "is"="in-sample", "vl" = "validation"),
         Strategy = recode(p, "DB"="Decision-based", "SB" = "Statistical-based"),
         facet = paste0("bold('", Strategy, "')~'with'~bold('", Errors, "')~'errors'")) |>
  filter(err == "SE") |>
    ggplot() + 
    geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
              data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                           u = min(u), .groups = "drop"))+
    geom_segment(aes(x = l, xend = u, yend = name_all, y = name_all), linewidth = 1) + 
    geom_point(aes(x = l, y = name_all), pch = "|", size = 3) + 
    geom_point(aes(x = u, y = name_all), pch = "|", size = 3) + 
    geom_point(aes(x = value, fill = col, y = name_all, pch = pch_name), size = 3) +
    geom_label(data = function(x) select(x, facet, fpval) |>
                 mutate(text = paste0("Friedman test p-value ", 
                                      ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
               aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", 
               size = 2.5,  label.size = NA) + 
    scale_shape_manual(values=c(21, 24))+
    facet_wrap(facet~., scales = "free", 
               labeller = label_parsed)+
    labs(y = NULL, x = NULL) + 
  scale_y_discrete(labels = function(l) parse(text=l))+
    theme_minimal()+
    theme(legend.title = element_blank(),
          legend.position = "none",
          #text = element_text(size = utils$font),
          #strip.text = element_text(size = utils$font),
          legend.margin = margin())

ggsave(plot = plotmcb, file = "./img/mcb.pdf", width = 9, height = 6)








