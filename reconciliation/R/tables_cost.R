library(tidyverse)
load("./scores/index10.RData")
index_score <- index
load("./scores/index60.RData")
index_score <- bind_rows(index_score, index)
load("./scores/index480.RData")
index_score <- bind_rows(index_score, index)

dir.create("./img", recursive = TRUE, showWarnings = FALSE)

err <- c("finesI", "nogainI", "pobfI", "probngI")

index_df <- index_score |>
  pivot_longer(cols = any_of(err), names_to = "err") |>
  mutate(res = ifelse(name %in% c("base", "naive"), "  ", res),
         res = ifelse(str_detect(name, 'ols$|str$'), "  ", res),
         p = ifelse(str_detect(name, 'csbu'), "  ", p),
         p = ifelse(name %in% c("base", "naive"), "  ", p),
         mod = ifelse(name %in% c("naive"), "naive", mod),
         name = ifelse(name %in% c("naive"), "base", name),
         name = ifelse(name %in% c("csbu_bu"), "ctbu", name),
         name_all = paste(mod, name, res, p, sep = " "),
         k = factor(k, ordered = TRUE),
         k = recode(k, "10" = "10 mins", "60"="1 hour", "480"="8 hours")) |>
  unique() |>
  group_by(across(-c("value"))) |>
  summarise(value = value[1], .groups = "drop") |>
  filter(Group == "<aggregated>", Subgroup == "<aggregated>")

plot_data <- index_df |>
  filter(mod %in% c("lr0", "lgbm0"), nn == "sntz",
         !grepl("lasso", name, fixed = TRUE),
         name %in% c("base", 
                     "csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", 
                     "oct_acov", "ite_acov-shr")) |>
  mutate(p = ifelse(p == "  ", "SB", p),
         res = ifelse(res == "  ", "is", res),
         mod = recode(mod, "lgbm0"="LGBM base models", "lr0" = "LR base models"),
         Errors = recode(res, "is"="in-sample", "vl" = "validation"),
         Strategy = recode(p, "DB"="Decision-based", "SB" = "Statistical-based"),
         name = factor(name, c("base", "ctbu", "csbu_shr", "oct_str", "oct_wlsv", "oct_bdshr", 
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
  pivot_wider(names_from = "err")


index_plot_rl <- plot_data |>
  filter(delta == 0.01) |>
  ggplot(aes(x = nogainI, y = probngI, col = name, #linetype = Errors,
             pch = interaction(Strategy, Errors, sep = " with "), 
             group = interaction(Strategy, name, Errors))) +
  geom_point(size = 2) +
  scale_color_manual(values = palette.colors(palette = "R4")[-7])+
  scale_shape_manual(values = c(1, 2, 3, 4)) +
  labs(y = "Probability of revenue loss", #expression("Fines and penalties loss, "*delta^{"-"}), 
       x = expression("Revenue loss, "*delta^{"+"}))+
  facet_grid(k~mod, scales = "free") +
  #xlim(0, 1)+
  theme_bw() +
  guides(pch = guide_legend(nrow = 1),
         col = guide_legend(nrow = 1))  + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.location = "plot",
        legend.box = "vertical", 
        legend.margin = margin(),
        legend.spacing.y = unit(0, "cm"),
        strip.background =element_rect(fill=NA, colour = NA), 
        strip.text = element_text(face = 'bold'))

ggsave(plot = index_plot_rl, 
       filename = paste0("./img/index_plot_rl.pdf"), width = 9, height = 8)


index_plot_fp <- plot_data |>
  filter(delta == 0.01) |>
  ggplot(aes(x = finesI, y = pobfI, col = name, #linetype = Errors,
             pch = interaction(Strategy, Errors, sep = " with "), 
             group = interaction(Strategy, name, Errors))) +
  geom_point(size = 2) + #c("#ba0101", "#0083ff", "#6C832E", "#452E83", "")
  scale_color_manual(values = palette.colors(palette = "R4")[-7])+
  labs(y = "Probability of fines and penalties", #
       x = expression("Fines and penalties loss, "*delta^{"-"}))+
  scale_shape_manual(values = c(1, 2, 3, 4)) +
  facet_grid(k~mod, scales = "free") +
  #xlim(0, 1)+
  theme_bw() +
  scale_x_log10() +
  guides(pch = guide_legend(nrow = 1),
         col = guide_legend(nrow = 1))  + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.location = "plot",
        legend.box = "vertical", 
        legend.margin = margin(),
        legend.spacing.y = unit(0, "cm"),
        strip.background =element_rect(fill=NA, colour = NA), 
        strip.text = element_text(face = 'bold'))

ggsave(plot = index_plot_fp, 
       filename = paste0("./img/index_plot_fp.pdf"), width = 9, height = 8)

index_plot_all <- plot_data |>
  filter(delta == 0.01) |>
  ggplot(aes(x = finesI, y = nogainI, col = name, #linetype = Errors,
             pch = interaction(Strategy, Errors, sep = " with "), 
             group = interaction(Strategy, name, Errors))) +
  geom_point(size = 2) + #c("#ba0101", "#0083ff", "#6C832E", "#452E83", "")
  scale_color_manual(values = palette.colors(palette = "R4")[-7])+
  labs(y = expression("Revenue loss, "*delta^{"+"}), 
       x = expression("Fines and penalties loss, "*delta^{"-"}))+
  scale_shape_manual(values = c(1, 2, 3, 4)) +
  facet_grid(k~mod, scales = "free") +
  #xlim(0, 1)+
  theme_bw() +
  scale_x_log10() +
  guides(pch = guide_legend(nrow = 1),
         col = guide_legend(nrow = 1))  + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.location = "plot",
        legend.box = "vertical", 
        legend.margin = margin(),
        legend.spacing.y = unit(0, "cm"),
        strip.background =element_rect(fill=NA, colour = NA), 
        strip.text = element_text(face = 'bold'))

ggsave(plot = index_plot_all, 
       filename = paste0("./img/index_plot_all.pdf"), width = 9, height = 8)


for(delta in c(0, 5, 10, 25, 50)){
  index_plot_all <- plot_data |>
    filter(delta == delta/1000) |>
    ggplot(aes(x = finesI, y = nogainI, col = name, #linetype = Errors,
               pch = interaction(Strategy, Errors, sep = " with "), 
               group = interaction(Strategy, name, Errors))) +
    geom_point(size = 2) + #c("#ba0101", "#0083ff", "#6C832E", "#452E83", "")
    scale_color_manual(values = palette.colors(palette = "R4")[-7])+
    labs(y = expression("Revenue loss, "*delta^{"+"}), 
         x = expression("Fines and penalties loss, "*delta^{"-"}))+
    scale_shape_manual(values = c(1, 2, 3, 4)) +
    facet_grid(k~mod, scales = "free") +
    #xlim(0, 1)+
    theme_bw() +
    scale_x_log10() +
    guides(pch = guide_legend(nrow = 1),
           col = guide_legend(nrow = 1))  + 
    theme(legend.title = element_blank(),
          legend.position = "bottom", 
          legend.location = "plot",
          legend.box = "vertical", 
          legend.margin = margin(),
          legend.spacing.y = unit(0, "cm"),
          strip.background =element_rect(fill=NA, colour = NA), 
          strip.text = element_text(face = 'bold'))
  
  ggsave(plot = index_plot_all, 
         filename = paste0("./img/index_plot_all", delta, ".pdf"), width = 9, height = 6)
}