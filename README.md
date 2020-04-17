# Factor Loading Recovery of Smoothed Non-positive Definite Correlation Matrices

This repo contains all of the code to reproduce the simulation, analyses, figures, and tables for my master's thesis (as well as the manuscript itself). To do so, follow these steps:

1. Clone the repo as a project in RStudio
2. In R, call `renv::restore()` to install the package versions used in the project
3. Run `R/simulation_main.R` --- this takes a long time (days) to complete
4. Run all R files `R/data_processing`
5. Knit the document `factor_loading_recovery.Rmd`