# Scaled distance between the smoothed ($\\Rsm$) and model-implied ($\\Rpop$)
# correlation matrices for the Higham (APA; 2002), Bentler-Yuan (BY; 2011), and
# Knol-Berger (KB; 1991) smoothing methods and when no smoothing was applied
# (None).
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
       path = "Text/figs",
       width = 7,
       height = 4.25,
       units = "in",
       device = png(),
       dpi = "retina")
