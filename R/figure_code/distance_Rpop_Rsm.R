# Scaled distance between the smoothed ($\\Rsm$) and model-implied ($\\Rpop$)
# correlation matrices for the Higham (APA; 2002), Bentler-Yuan (BY; 2011), and
# Knol-Berger (KB; 1991) smoothing methods and when no smoothing was applied
# (None).
pacman::p_load(ggplot2,
               here,
               magrittr,
               dplyr,
               tidyr)
results_matrix_npd <- readRDS(here("Data", "results_matrix_npd.RDS"))
RpopRsm_data <- results_matrix_npd %>%
  dplyr::select(id:npd, smoothing_method, distance_Rpop_Rsm, 
         factors_rec:model_error_rec) %>%
  distinct()
  
distance_Rpop_Rsm_plot <- RpopRsm_data %>%
  ggplot(aes(x = smoothing_method, y = distance_Rpop_Rsm)) +
  geom_boxplot(outlier.size = 0.5, na.rm = TRUE, outlier.alpha = 0.5) +
  facet_grid(factor_loading_rec ~ subjects_per_item_rec * items_per_factor_rec) +
  labs(x = "Smoothing method",
       y = latex2exp::TeX("$D_s(\\mathbf{R}_{Sm}, \\mathbf{R}_{Pop})$")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save png; retina gives 320 dpi
ggsave(filename = "distance_Rpop_Rsm.png",
       plot = distance_Rpop_Rsm_plot,
       path = here("Text", "figs"),
       width = 8,
       height = 8,
       units = "in",
       device = png(),
       dpi = "retina")
