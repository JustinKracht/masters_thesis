# Box plots of $\\RMSE$ for all combinations of factor extraction method, factor
# loading, and number of subjects per item. The three factor extraction methods
# (unweighted least squares, maximum likelihood, and principal axes) are denoted
# by ULS, ML, and PA, respectively.
rmse_fa_method <- results_matrix_npd %>%
  filter(fa_convergence == TRUE) %>%
  ggplot(aes(x = fa_method_rec, 
             y = loading_rmsd)) +
  geom_boxplot(outlier.size = 0.5, na.rm = TRUE, outlier.alpha = 0.5) +
  facet_grid(subjects_per_item_rec ~ factor_loading_rec) +
  labs(x = "Extraction Method",
       y = TeX("$RMSE(\\mathbf{\\Lambda}, \\hat{\\mathbf{\\Lambda}})$")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save png; retina gives 320 dpi
ggsave(filename = "fa_method_boxplots.png",
       plot = rmse_fa_method,
       path = "Text/figs",
       width = 5,
       height = 7,
       units = "in",
       device = png(),
       dpi = "retina")
