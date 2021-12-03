###################################################################################

# Loading legacy (w) parameters and preparing for trait modelling

###################################################################################


library(magrittr); library(tidyverse)

# source the w coeffients from the complete results list created in the loop.
# the results lists for 307 species (threshold = min 30 segments)
load(file = "C:/beyond_thesis/PUBLICATION/stan_results/test_run_11NOV/list_of_draws_a15_s30.RDS")
load(file = "C:/beyond_thesis/PUBLICATION/stan_results/test_run_11NOV/list_of_results_a15_s30.RDS")

C:/beyond_thesis/PUBLICATION/stan_results/test_run_11NOV/list_of_draws_a15_s30.RDS