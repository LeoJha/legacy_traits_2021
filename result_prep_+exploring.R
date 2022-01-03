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
success_AOUs <- as.data.frame(w_results_f$AOU) 
names(success_AOUs) <- "AOU"

# mcmc_dens_overlay(list.of.draws$testdraws_1430) # have a look....
# mcmc_dens_overlay(list.of.draws$testdraws_6090)

############### Extract 100 random draws from each species model posterior distribution 

# first get the names of the outputs in the list of draws
draw.list.names <- names(list.of.draws)

w_draws <- data.frame()

for (i in draw.list.names) {
  
  # extract the draws for each species i, and convert the draws_array to a vector
  draws.vector.thisAOU <- list.of.draws %>% magrittr::extract2(i) %>% as.vector()
  
  # 100 randomly selected draws of parameter w to pull
  w <- sample(draws.vector.thisAOU, 100)
  
  # extract the AOU number from the list result name
  AOU <- substr(i, start = 11, stop = 15)
  
  # vector of repeating species aou code
  AOU <- as.vector(rep(AOU,100))
  
  # add the contribution coeffs to this thing
  draws.AOU <- data.frame(cbind(w, AOU))
  
  # make sure the w draws are numbers!
  draws.AOU$w <- as.numeric(draws.AOU$w)
  
  # add the 100 draws of this species to the greater data frame of all species draws
  w_draws <- rbind(w_draws, draws.AOU)
}

## check how the means from the 100 draws compares to the full posterior mean
# w_draw_means <- w_draws %>%
#     group_by(AOU) %>%
#     summarise(Means=mean(w))


############## Now merge the trait data and the w draws

# source the taxa data 

# trait_taxa_df <- readRDS(file = "C:/beyond_thesis/Data/trait_taxa_df.rda")
trait_taxa_df <- read.csv(file = "C:/beyond_thesis/Data/trait_taxa_df_added_data.csv")

# write.csv(trait_taxa_df, file = "~/Leo/Data/trait_taxa_df.csv")
# trait_taxa_df <- read.csv("~/Leo/Data/trait_taxa_df_added_data.csv")
trait_taxa_df <- trait_taxa_df[-1]
# 
# get rid of the spaces and replace with _ in the variable names
# names(trait_taxa_df) <- str_replace_all(names(trait_taxa_df), c("." = "_" ))

# remove parenthesis from the names
# names(trait_taxa_df) <- gsub("[()]", "", names(trait_taxa_df))

# change all the variables to correct variable type
trait_taxa_df$maximum_lifespan_years <- as.numeric(trait_taxa_df$maximum_lifespan_years)
trait_taxa_df$diet <- as.factor(trait_taxa_df$diet)
trait_taxa_df$foraging <- as.factor(trait_taxa_df$foraging)
trait_taxa_df$migration <- as.factor(trait_taxa_df$migration)
trait_taxa_df$diet <- as.factor(trait_taxa_df$diet)
trait_taxa_df$number_of_brood_per_year <- as.numeric(trait_taxa_df$number_of_brood_per_year)
trait_taxa_df$nest_type <- as.factor(trait_taxa_df$nest_type)
trait_taxa_df$nest_location <- as.factor(trait_taxa_df$nest_location)
trait_taxa_df$main_habitat <- as.factor(trait_taxa_df$main_habitat)
trait_taxa_df$territoriality <- as.factor(trait_taxa_df$territoriality)
trait_taxa_df$developmental_mode <- as.factor(trait_taxa_df$developmental_mode)
trait_taxa_df$mating <- as.factor(trait_taxa_df$mating)
trait_taxa_df$breeding_system <- as.factor(trait_taxa_df$breeding_system)
trait_taxa_df$ORDER <- as.factor(trait_taxa_df$ORDER)
trait_taxa_df$Family <- as.factor(trait_taxa_df$Family)
trait_taxa_df$Genus <- as.factor(trait_taxa_df$Genus)


# correct the NAs in territoriality 
TrueTerritoriality <- as.data.frame(gsub(pattern = "NA", replacement = NA,  x= trait_taxa_df$territoriality))
TrueTerritoriality$territoriality <- TrueTerritoriality$`gsub(pattern = "NA", replacement = NA, x = trait_taxa_df$territoriality)`
trait_taxa_df$territoriality <- TrueTerritoriality$territoriality


# merge with legacy of one lct and remove all the other species from the list
# 
# w_TT_df <- merge(w_draws, trait_taxa_df, by.x = "AOU")
# 
# w_TT_df <- merge(w_TT_df, w_results, by.x = "AOU")

# save(w_TT_df, file = "~/Leo/Data/w_draws_TT_data/w_TT_df_307sp_1SEP_TerrNAs.RDS")

# ########################### GET THE DF OF Ws AND TRAIT INFO HERE
# load(file = "~/Leo/Data/w_draws_TT_data/w_TT_df_307sp_1SEP_TerrNAs.RDS")

##########################

### merge the data with the list of successful aous 67 sp. 


w_TT_df <- merge(w_results, success_AOUs, by.x = "AOU")
w_TT_df_sss <- merge(w_TT_df, trait_taxa_df, by.y = "AOU" )

# model selection not possible with haphazard NAs through out trait data

# check to see where there are NAs and hwo many
sum(is.na(w_TT_df$territoriality))
sum(is.na(w_TT_df$maximum_lifespan_years))

### REMOVE any remaining NAs
w_TT_df <- na.omit(w_TT_df)
# saveRDS(w_TT_df, file = "~/Leo/Data/w_draws_TT_data/w_TT_df_302sp_6SEP.RDS")
# saveRDS(w_TT_df, file = "~/Leo/Data/w_draws_TT_data/w_TT_df_299sp_6SEP_noNA.RDS")

# w_TT_df$species <- as.factor(w_TT_df$species)

w_TT_df <- readRDS(file = "~/Leo/Data/w_draws_TT_data/w_TT_df_299sp_6SEP_noNA.RDS")

