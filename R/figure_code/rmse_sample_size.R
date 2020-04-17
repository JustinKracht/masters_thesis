# RMSE sample size scatterplot
rmse_sample_size <- results_matrix_npd %>%
  filter(fa_convergence == TRUE) %>%
  ggplot(aes(x = as.numeric(items_per_factor * factors * subjects_per_item), 
             y = log(loading_rmsd))) +
  geom_hex() +
  labs(x = "Sample size",
       y = TeX("$\\log \\; RMSE(\\mathbf{\\Lambda}, \\hat{\\mathbf{\\Lambda}})$")) +
  theme_minimal()

# Save png; retina gives 320 dpi
ggsave(filename = "rmse_sample_size.png",
       plot = rmse_sample_size,
       path = "Text/figs",
       width = 6.5,
       height = 5,
       units = "in",
       device = png(),
       dpi = "retina")