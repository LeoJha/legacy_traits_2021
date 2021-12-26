###################################################################################

# Loading legacy (w) parameters and preparing for trait modelling

###################################################################################


library(magrittr); library(tidyverse)

library(readxl);library(dplyr); library(reshape); library(tidyverse); library(stringr)
library(bayesplot); library(ggplot2); library(rstanarm)
# library("rstan");library("loo"); library("cmdstanr"); library(posterior); 

# source the w coeffients from the complete results list created in the loop.
# the results lists for 307 species (threshold = min 30 segments)
load(file = "C:/beyond_thesis/PUBLICATION/stan_results/test_run_11NOV/list_of_draws_a15_s30.RDS")
load(file = "C:/beyond_thesis/PUBLICATION/stan_results/test_run_11NOV/list_of_results_a15_s30.RDS")

##################################
# look at the uniformity of the w draws of the results

result.list.names <- names(list.of.results)

# create a data frame of the posterior means of w for each species
w_results <- data.frame()
for (i in  result.list.names) {
  # extract the results for each species i, and convert the results tibble to a data frame
  result.df.thisAOU <- list.of.results %>% magrittr::extract2(i) %>% as.data.frame()
  # take the 3rd row of the df, the w param row
  result.df.w.thisAOU <- as.data.frame(result.df.thisAOU[3,])
  # extract the AOU number from the list result name
  AOU <- substr(i, start = 12, stop = 17)
  # add the AOU to the df of results w 
  result.df.w.thisAOU$AOU <- AOU
  w_results <- rbind(w_results, result.df.w.thisAOU)
}

# rename the rows quickly 
rownames(w_results) <- 1:220

# check how well the parameter converged
w_results_f <- filter(w_results, rhat <= 1.05) # %>% length() #147 sp failed convergence and chain attempts
# subset(w_results, rhat <= 1.05)  %>% length()

# check that the effective sample size is big enough
w_results_f <- filter(w_results_f, ess_bulk >= 400) #6 species dont cut it 

# 67 species remaining from 220! are they related somehow ???
success_AOUs <- w_results_f$AOU

mcmc_dens_overlay(list.of.draws$testdraws_1430) # have a look....
mcmc_dens_overlay(list.of.draws$testdraws_6090)
