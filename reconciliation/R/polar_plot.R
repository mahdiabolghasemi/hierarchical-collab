library(tidyverse)
library(ggpubr)
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
  filter(err == "SE") |>
  group_by(k, err) |>
  mutate(value = rank(value)) |>
  group_by(p, mod, nn, res, name, name_all, err) |>
  summarise(value = mean(value)) |>
  arrange(value)

for(errid in c("SE")){
  scaleFUN <- function(x) sprintf("%.0f", x)
  p1 <- out |>
    filter(mod %in% c("lgbm0"), nn == "sntz", err == errid,
           !grepl("lasso", name, fixed = TRUE),
           name %in% c("csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", "oct_acov", "ite_acov-shr")) |>
    mutate(p = ifelse(p == "  ", "SB", p),
           res = ifelse(res == "  ", "is", res),
           k = factor(k, c(seq(10, 480, 10), "All"), ordered = TRUE),
           mod = recode(mod, "lgbm0"="LGBM", "lr0" = "LR"),
           Errors = recode(res, "is"="in-sample", "vl" = "validation"),
           Strategy = recode(p, "DB"="Decision-based", "SB" = "Statistical-based"),
           name = factor(name, c("csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", 
                                 "oct_acov", "ite_acov-shr"), 
                         ordered = TRUE),
           name = recode(name, 
                         #"csbu_shr"= "cs(shr)+te(bu)", 
                         "csbu_shr"= "pbu", 
                         "oct_str" = "ct(str)", 
                         "oct_wlsv" = "ct(wlsv)", 
                         "oct_bdshr" = "ct(bdshr)", 
                         "oct_acov" = "ct(acov)", 
                         #"ite_acov-shr" = "ite(acov[te], shr[cs])",
                         "ite_acov-shr" = "ite")) |> 
    group_by(mod, name, nn, k, err) |>
    mutate(#mnv = min(value),
      #value = value-mnv,
      mxv = max(value),
      value = 100*(1-value/mxv)) |>
    ggplot(aes(x = k, y = value, col = Errors, 
               linetype = Strategy, pch = Strategy, group = interaction(p, res))) +
    geom_line() +
    #geom_polygon(fill=NA) + 
    #ylim(c(0.875, 1)) +
    geom_point(stat='identity') +
    scale_y_continuous(labels=scaleFUN)+
    scale_x_discrete(expand = c(0,0))+
    #coord_polar(theta = "x", direction = -1) +
    coord_radial(rotate.angle = FALSE, start = -1 * pi, end = 0.65 * pi, inner.radius = 0.15, expand = TRUE)+
    #coord_polar(start =-pi* 1/11) +
    #facet_grid(name~err, scales = "free_y")
    labs(y = "LGBM base models\n", x = NULL)+
    scale_color_manual(values = c("#0083ff", "#ba0101")) +
    facet_wrap(.~name, scales = "free_y", labeller = label_parsed)+
    theme_minimal()+
    theme(axis.text.y = element_text(size = 6),
          #axis.ticks.y = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          panel.spacing.x = unit(1, "lines"),
          legend.margin = margin())
  
  
  p2 <- out |>
    filter(mod %in% c("lr0"), nn == "sntz", err == errid,
           !grepl("lasso", name, fixed = TRUE),
           name %in% c("csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", "oct_acov", "ite_acov-shr")) |>
    mutate(p = ifelse(p == "  ", "SB", p),
           res = ifelse(res == "  ", "is", res),
           k = factor(k, c(seq(10, 480, 10), "All"), ordered = TRUE),
           mod = recode(mod, "lgbm0"="LGBM", "lr0" = "LR"),
           Errors = recode(res, "is"="in-sample", "vl" = "validation"),
           Strategy = recode(p, "DB"="Decision-based", "SB" = "Statistical-based"),
           name = factor(name, c("csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", 
                                 "oct_acov", "ite_acov-shr"), 
                         ordered = TRUE),
           name = recode(name, 
                         #"csbu_shr"= "cs(shr)+te(bu)", 
                         "csbu_shr"= "pbu", 
                         "oct_str" = "ct(str)", 
                         "oct_wlsv" = "ct(wlsv)", 
                         "oct_bdshr" = "ct(bdshr)", 
                         "oct_acov" = "ct(acov)", 
                         #"ite_acov-shr" = "ite(acov[te], shr[cs])",
                         "ite_acov-shr" = "ite")) |> 
    group_by(mod, name, nn, k, err) |>
    mutate(#mnv = min(value),
      #value = value-mnv,
      mxv = max(value),
      value = 100*(1-value/mxv)) |>
    ggplot(aes(x = k, y = value, col = Errors, 
               linetype = Strategy, pch = Strategy, group = interaction(p, res))) +
    geom_line() +
    #geom_polygon(fill=NA) + 
    #ylim(c(0.875, 1)) +
    geom_point(stat='identity') +
    scale_y_continuous(labels=scaleFUN)+
    scale_x_discrete(expand = c(0,0))+
    #coord_polar(theta = "x", direction = -1) +
    coord_radial(rotate.angle = FALSE, start = -1 * pi, end = 0.65 * pi, inner.radius = 0.15, expand = TRUE)+
    #coord_polar(start =-pi* 1/11) +
    #facet_grid(name~err, scales = "free_y")
    scale_color_manual(values = c("#0083ff", "#ba0101")) +
    labs(y = "LR base models\n", x = NULL)+
    facet_wrap(.~name, scales = "free_y", labeller = label_parsed)+
    theme_minimal()+
    theme(axis.text.y = element_text(size = 6),
          #axis.ticks.y = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          panel.spacing.x = unit(1, "lines"),
          legend.margin = margin())
  plotsave <- ggarrange(p2, p1, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
  
  ggsave(plot = plotsave, 
         filename = paste0("./img/polar3_plot_", errid,".pdf"), width = 7.5, height = 10)
}