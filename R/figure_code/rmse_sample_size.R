# RMSE sample size scatter plot
pacman::p_load(ggplot2,
               here,
               magrittr,
               dplyr,
               tidyr,
               hexbin,
               latex2exp)
results_matrix_npd <- readRDS(here("Data", "results_matrix_npd.RDS"))

rmse_sample_size <- results_matrix_npd %>%
  filter(fa_convergence == TRUE) %>%
  ggplot(aes(x = as.numeric(items_per_factor * factors * subjects_per_item), 
             y = log(loading_rmsd))) +
  scale_fill_continuous(type = "viridis") +
  geom_hex(bins = 50) +
  geom_smooth(color = "grey50") +
  labs(x = "Sample size",
       y = TeX("$\\log \\; RMSE(\\mathbf{F}, \\hat{\\mathbf{F}})$"))

# Save png; retina gives 320 dpi
ggsave(filename = "rmse_sample_size.png",
       plot = rmse_sample_size,
       path = here("Text", "figs"),
       width = 6.5,
       height = 5,
       units = "in",
       device = png(),
       dpi = "retina")