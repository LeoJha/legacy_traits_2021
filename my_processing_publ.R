#####################################################################

# collecting the coefficents from a set of species 

#####################################################################

library(readxl);library(dplyr); library(reshape); library(tidyverse); library(stringr)

library("rstan");library("loo"); library("cmdstanr"); library(posterior); library(bayesplot)
options(mc.cores = parallel::detectCores()); color_scheme_set("viridis")
cmdstan_path(); cmdstan_version()
# get the landcover data

landcover <- readRDS("~/Leo/Data/only_landcover.rda")


# start by taking the species matrix

load("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/3years_x_timepoint/vegan_spmatrix_t2_max.rda")
load("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/3years_x_timepoint/vegan_spmatrix_t1_max.rda")

spmatrix.t2 <- as.data.frame(spmatrix.t2)
spmatrix.t1 <- as.data.frame(spmatrix.t1)


# load the trait list (435 sp)

# trait.df <- read_excel("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/442_2017_3967_MOESM3_ESM.xlsx")
# 
# # load the species list and taxa data
# 
# SpeciesList <- read_excel("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/SpeciesList.xlsx")
# head(SpeciesList)
# SpeciesList <- SpeciesList[-(4:5)] # remove the french and spanish names
# 
# SpeciesList$species <- paste(SpeciesList$Genus, SpeciesList$Species, sep = "_") #add column of sp name
# 
# # create df with traits and taxa and AOU
# 
# trait_taxa_df <- merge(trait.df, SpeciesList, by.x = "species")
# saveRDS(trait_taxa_df, file = "~/Leo/Data/trait_taxa_df.rda")
# trait_taxa_df <- readRDS("~/Leo/Data/trait_taxa_df.rda")
# trait data set has 406 species included. 


################## CREATING lists of AOUs according to occurance thesholds #########################
# # extract a vector of the AOU codes

# AOUs <- sort(trait_taxa_df$AOU)
# AOUs.string <- as.character(AOUs)
# 
# # create vector of AOUs appearing in t1 and t2
# aous.t1ANDt2 <- c(colnames(spmatrix.t1), colnames(spmatrix.t2))
# 
# aous.t1ANDt2 <- as.factor(aous.t1ANDt2)
# aous.t1ANDt2 <- as.data.frame(aous.t1ANDt2)
# 
# occurs <- aous.t1ANDt2 %>%
#   group_by(aous.t1ANDt2) %>%
#   summarise(n=n())
# 
# occurs.both <- subset(occurs, n == 2)
# AOU.occurs.both <- occurs.both$aous.t1ANDt2
# 
# 
# # AOU.occur.both is a list of AOUs which at least have records in both time point,
# # even if those records are 0s. that is 508 species
# 
# AOU.occurs.both <- as.character(AOU.occurs.both)
# AOU.occurs.both <- as.numeric(AOU.occurs.both)
# AOU.occurs.both <- sort(AOU.occurs.both)
# AOU.occurs.both <- as.character(AOU.occurs.both)
# 
# # now take the full sp matrices and isolate the species for which there are records of both time points
# # making matrices for 508 sp that have records at t1 and t2
# spmatrix.t1.both <- spmatrix.t1[AOU.occurs.both]
# spmatrix.t2.both <- spmatrix.t2[AOU.occurs.both]
# 
# 
# # first figure out which trait species we have matching the co-occuring 508 sp.
# 
# occursORtraits <- c(AOUs.string, AOU.occurs.both)
# occursORtraits <- as.factor(occursORtraits)
# occursORtraits <- as.data.frame(occursORtraits)
# 
# occursANDtraits <- occursORtraits %>%
#   group_by(occursORtraits) %>%
#   summarise(n=n())
# 
# occursANDtraits <- subset(occursANDtraits, n==2)
# # there are 376 sp for which we have trait data and which occur at least once in either timepoint
# 
# AOU.occursANDtraits <- occursANDtraits$occursORtraits
# 
# AOU.occursANDtraits <- as.character(AOU.occursANDtraits)
# AOU.occursANDtraits <- as.numeric(AOU.occursANDtraits)
# AOU.occursANDtraits <- sort(AOU.occursANDtraits)
# AOU.occursANDtraits <- as.character(AOU.occursANDtraits)
# 
# # now create sp matrices with the 376 sp that have records in both times and have trait data
# 
# spmatrix.t1.376 <- spmatrix.t1[AOU.occursANDtraits]
# spmatrix.t2.376 <- spmatrix.t2[AOU.occursANDtraits]
# 
# 
# # combine the abundance counts at t1 and t2.
# spmatrix.t1t2.sum <- spmatrix.t1.376 + spmatrix.t2.376
# 
# # find the number of occurrences of each species.
# # ie the number of non-zero abundances
# 
# # head(spmatrix.t1t2.sum)
# 
# occurances.per.sp <- colSums(spmatrix.t1t2.sum != 0)
# 
# occurances.per.sp <- as.data.frame(occurances.per.sp)
# sort(occurances.per.sp$occurances.per.sp)

# select a threshold for occurrences and subset the most common ones.
# occurances.3900.sp <- subset(occurances.per.sp, occurances.per.sp >= 3900)
# occurances.3000.sp <- subset(occurances.per.sp, occurances.per.sp >= 3000)
# occurances.2000.sp <- subset(occurances.per.sp, occurances.per.sp >= 2000)
# occurances.1000.sp <- subset(occurances.per.sp, occurances.per.sp >= 1000)
# occurances.500.sp <- subset(occurances.per.sp, occurances.per.sp >= 500)
# occurances.100.sp <- subset(occurances.per.sp, occurances.per.sp >= 100)
# occurances.30.sp <- subset(occurances.per.sp, occurances.per.sp >= 30)
# 
# 
# AOUs.over.3900 <- row.names(occurances.3900.sp)
# AOUs.over.3000 <- row.names(occurances.3000.sp)
# AOUs.over.2000 <- row.names(occurances.2000.sp)
# AOUs.over.1000 <- row.names(occurances.1000.sp)
# AOUs.over.500 <- row.names(occurances.500.sp)
# AOUs.over.100 <- row.names(occurances.100.sp)
# AOUs.over.50 <- row.names(occurances.50.sp)
# AOUs.over.30 <- row.names(occurances.30.sp)

# save(AOUs.over.3900, file = "~/Leo/Data/AOU_thresholds/AOUs.over.3900.RDS")
# save(AOUs.over.3000, file = "~/Leo/Data/AOU_thresholds/AOUs.over.3000.RDS")
# save(AOUs.over.2000, file = "~/Leo/Data/AOU_thresholds/AOUs.over.2000.RDS")
# save(AOUs.over.1000, file = "~/Leo/Data/AOU_thresholds/AOUs.over.1000.RDS")
# save(AOUs.over.500, file = "~/Leo/Data/AOU_thresholds/AOUs.over.500.RDS")
 # save(AOUs.over.100, file = "~/Leo/Data/AOU_thresholds/AOUs.over.100.RDS")
 # save(AOUs.over.50, file = "~/Leo/Data/AOU_thresholds/AOUs.over.50.RDS")
 # save(occurances.per.sp, file = "~/Leo/Data/AOU_thresholds/occurances.per.sp.RDS")
 
 
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.3900.RDS")
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.3000.RDS")
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.2000.RDS")
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.1000.RDS")
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.500.RDS")
load(file = "~/Leo/Data/AOU_thresholds/AOUs.over.100.RDS")

# # subset the AOUs with nonzero abundanced of shared timepoints between 100 and 499

# save(AOUS.between100and499, file = "~/Leo/Data/AOU_thresholds/AOUS.between100and499.RDS")
load( file = "~/Leo/Data/AOU_thresholds/AOUS.between100and499.RDS")


# AOUs.over.3000.except.top2 <- AOUs.over.3000[-c(2, 6)]

# create a loop where we select the t2 abundance columns of the threshold species

# need a empty list to collect the results in 
# have an existing list outside the loop!!
list.of.results <- list()

# need a empty list to collect the DRAWS in 
# have an existing list outside the loop!!
list.of.draws <- list()

# declare the model (has response var "t2_AOU")
# GET THE MODEL.TEST
model.legacy <- cmdstan_model(stan_file="~/Leo/PUBLICATION/legacymodel_publ.stan")

# source the ordered list of partitions (the rows)
partitions.4800 <- row.names(spmatrix.t1)

# SET THE INITS
inits.list <- purrr::map(
  1:4, ~ list("intercept" = runif(1, -10, 10), 
              "w" = runif(1, 0, 1),
              
              # linear param
              "b_urban" = runif(1, 0, 2), "b_forest" = runif(1, 0, 2), "b_grass" = runif(1, 0, 2),
              "b_crop"= runif(1, 0, 2), "b_wet" = runif(1, 0, 2),
              
              # quadratic params
              "b2_urban" = runif(1, -2, 0), "b2_forest" = runif(1, -2, 0), "b2_grass" = runif(1, -2, 0),
              "b2_crop"= runif(1, -2, 0), "b2_wet" = runif(1, -2, 0),
              
              # # linear*linear interaction params
              # "b_urban_forest" = runif(1, -2, 2), "b_urban_grass" = runif(1, -2, 2), "b_urban_crop" = runif(1, -2, 2),
              # "b_forest_grass" = runif(1, -2, 2), "b_forest_crop" = runif(1, -2, 2), "b_grass_crop" = runif(1, -2, 2),
              # "b_wet_urban" = runif(1, -2, 2), "b_wet_forest" = runif(1, -2, 2), "b_wet_grass" = runif(1, -2, 2),
              # "b_wet_crop" = runif(1, -2, 2),
              
              # # linear*quadratic interaction params
              # "b_urban2_forest" = runif(1, -1, 1), "b_urban2_grass" = runif(1, -1, 1), "b_urban2_wet" = runif(1, -1, 1),
              # "b_urban2_crop" = runif(1, -1, 1), "b_forest2_urban" = runif(1, -1, 1), "b_forest2_crop" = runif(1, -1, 1),
              # "b_forest2_grass" = runif(1, -1, 1), "b_forest2_wet" = runif(1, -1, 1), "b_grass2_urban" = runif(1, -1, 1),
              # "b_grass2_forest" = runif(1, -1, 1), "b_grass2_crop" = runif(1, -1, 1), "b_grass2_wet" = runif(1, -1, 1),
              # "b_crop2_urban" = runif(1, -1, 1), "b_crop2_forest" = runif(1, -1, 1), "b_crop2_grass" = runif(1, -1, 1), 
              # "b_crop2_wet" = runif(1, -1, 1), "b_wet2_urban" = runif(1, -1, 1), "b_wet2_forest" = runif(1, -1, 1),
              # "b_wet2_grass" = runif(1, -1, 1), "b_wet2_crop" = runif(1, -1, 1),
              
              # # delay params
              # "c_pos_urban" = runif(1, 0, 1), "c_pos_forest" = runif(1, 0, 1), "c_pos_grass" = runif(1, 0, 1),
              # "c_pos_crop" = runif(1, 0, 1), "c_pos_wet" = runif(1, 0, 1), #"c_pos_lc_q1" = runif(1, 0, 1),
              # "c_neg_forest" = runif(1, 0, 1), "c_neg_grass" = runif(1, 0, 1), #"c_neg_urban"  = runif(1, 0, 1),
              # "c_neg_crop" = runif(1, 0, 1), "c_neg_wet" = runif(1, 0, 1) #"c_neg_lc_q1" = runif(1, 0, 1),
              
              # # # random effects and other params
              "sigma_observer" = runif(1, 0, 2),# "sigma_route" = runif(1, 0, 2),
              "b_time" = runif(1, -1, 1),  # "b_lc_q1" = runif(1, 0, 2), "b2_lc_q1" = runif(1, -2, 0),
              "b_temp" = runif(1, 0, 2), "b2_temp" = runif(1, -2, 0) 
  ))

# AOU.3420.3430 <- c("3420", "3430") # two AOUS with 554 and 404 obs respectively
AOU.over100.under110 <- c("3490", "2240") # two AOUs with 102 and 103 nonzero abundances in t2 only
# AOU.over500.under540 <- c("2010", "6220", "6730") # 534, 532, and 524 nonzero abundances in t2 only
# AOU.varied.testing <- c(AOU.3420.3430, AOU.over100.under110, AOUs.over.3900)

loop.progress <- 0 # track the looping progress with this 
for(each.aou in AOUs.over.30) {
  print(paste0("we now processing AOU of ", each.aou))
  # isolate the sp t2 and t1 abundance column
  t1_thisAOU <- select(spmatrix.t1,paste0(each.aou))[,1]
  t2_thisAOU <- select(spmatrix.t2,paste0(each.aou))[,1]
  
  # merge the t1 and t2 columns to find which partitions (rows) have no occurrences (0s)
  comb.data.thisAOU <- data.frame("partition" = partitions.4800, t1_thisAOU, t2_thisAOU)
  
  # isolate the partition for which there was ateast 1 recorded ind in either t1 OR t2.
  # for the analysis
  df.nozeros.thisAOU <- subset(comb.data.thisAOU, (t1_thisAOU+t2_thisAOU) != 0 )

  # add the corresponding LC values for the remaining partitions.
  df.nozeros.thisAOU.lc <- merge(df.nozeros.thisAOU, landcover, by.x = "partition") #merged by the landcover partition order.
  
  # ##### HOw about not removing any rows! keeping all 4800 obs for the species ####
  # df.all4800.thisAOU.lc <- merge(comb.data.thisAOU, landcover, by.x = "partition")
  
  # # try having a abundance list with NO 0s 
  # df.NO0s.thisAOU <- subset(comb.data.thisAOU, t2_thisAOU != 0)
  # df.NO0s.thisAOU.lc <- merge(df.NO0s.thisAOU, landcover, by.x = "partition")
  
  # INSTEAD: do this:
  data <- df.nozeros.thisAOU.lc
  # get total number of observations
  N <- as.numeric(nrow(data));# R <- as.numeric(nlevels(data$routeID));
  O <- as.numeric(nlevels(data$observerID))
  ###################################################################################
  
  # extract landscape variables that need transform
  vars2  <- c("urban.t1", "urban.t2", "forest.t1", "forest.t2", "grass.t1", "grass.t2", 
              "crop.t1", "crop.t2", "wet.t1", "wet.t2")# , "lc.q1.t1", "lc.q1.t2", "elev", "elev.sd")
  # for (i in vars){ assign(i, as.vector( data[[i]])) } # you can get the raw data
  for (i in vars2){ assign(i, as.vector( log(data[[i]]+1))) } # you can log the data here
  #for (i in vars){ assign(i, as.vector( (data[[i]] - mean(data[[i]]))/sd(data[[i]]) )) } # you can scale the data here
  
  # # extract landscape changes that do not need transform
  # vars3  <- c("delta.urban", "delta.forest", "delta.grass", "delta.crop",
  #             "delta.wet", "delta.lc.q1", "delta.temp", "delta.prec",
  #             "temp.t1", "temp.t2", "prec.t1", "prec.t2")
  # for (i in vars3){ assign(i, as.vector(data[[i]])) }

  
  
  data.list  <- list(
    # response and number data points
    "t2_AOU" = as.vector(data[,3]), "N" = N, 
    
    # land cover variables  
    "urban_t1"=urban.t1, "forest_t1"=forest.t1, "grass_t1"=grass.t1, "crop_t1"=crop.t1, "wet_t1"=wet.t1, #"lc_q1_t1"=lc.q1.t1,
    "urban_t2"=urban.t2, "forest_t2"=forest.t2, "grass_t2"=grass.t2, "crop_t2"=crop.t2, "wet_t2"=wet.t2, #"lc_q1_t2"=lc.q1.t2,
    "urban_squared_t1"= urban.t2^2, "forest_squared_t1"=forest.t1^2, "grass_squared_t1"=grass.t1^2,
    "crop_squared_t1"=crop.t1^2, "wet_squared_t1"=wet.t1^2, #"lc_q1_squared_t1"=lc.q1.t1^2,
    "urban_squared_t2"=urban.t2^2, "forest_squared_t2"=forest.t2^2, "grass_squared_t2"=grass.t2^2,
    "crop_squared_t2"=crop.t2^2, "wet_squared_t2"=wet.t2^2, #"lc_q1_squared_t2"=lc.q1.t2^2,
    
    # # land cover change variables
    # "delta_pos_forest"=delta.pos.forest, "delta_pos_crop"=delta.pos.cropland, "delta_pos_grass"=delta.pos.grassland,
    # "delta_pos_wet"=delta.pos.wetland, #"delta_pos_lc_q1"=delta.pos.lc.q1, #"delta.pos.temp"=delta.pos.temp,
    # "delta_neg_forest"=delta.neg.forest, "delta_neg_crop"=delta.neg.cropland, "delta_neg_grass"=delta.neg.grassland,
    # "delta_neg_wet"=delta.neg.wetland, #"delta_neg_lc_q1"=delta.neg.lc.q1, #"delta.neg.temp"=delta.neg.temp,
    # "delta_pos_urban"=delta.pos.urban
    
    # # random effects
    # # "R"=R, "routeID"=as.numeric(data$routeID),
    "O"=O, "observerID"=as.numeric(data$observerID)
    # #"S"=S, "segmentID"=segmentID, 
    
    # climatic, elevation & other variables
    ,"time"=data$time.t2
    #"temp_t1"=temp.t1, "prec_t1", "prec_t2", "elev",
    # "temp_t2"=temp.t2, "temp_squared_t2"=temp.t2^2 
  )
  
  # the inits are declared outside so they are the same for every AOU
  
  ## RUN IT 
  
  result <- model.test$sample(data = data.list,
                              init = inits.list,
                              refresh = 10,
                              chains = 4, parallel_chains = 4, #threads_per_chain = 2,
                              iter_warmup = 500,
                              iter_sampling = 2000,
                              adapt_delta = 0.9, 
                              max_treedepth = 10,
                              save_warmup = TRUE
                              # ,thin = 10
  )
  
  loop.progress <- loop.progress + 1
  print(paste0(" Finished and have saved AOU = ", each.aou))
  print(paste0("This AOU had ", N, " partitions included"))
  print(paste0("This was species ", loop.progress, ", out of the total = ", length(AOUs.over.30)))
  
  # save the result file as an object
  
  result$save_object(file = paste0("~/Leo/stan_models/test_results_1SEP/test_1SEP_",each.aou,".RDS"))
  
  result.for.this.AOU <- result$summary()[1:18,]
  list.of.results[[paste0("testresult_", each.aou)]] <- result.for.this.AOU
  
  draws.for.this.AOU <- result$draws(variables = c("w"))
  list.of.draws[[paste0("testdraws_", each.aou)]] <- draws.for.this.AOU
  
}

Sys.Date()
# save the list of tibbles we have created 

save(list.of.results, file = "~/Leo/stan_models/test_results_1SEP/test_1SEP_over30_list_of_results.RDS")
save(list.of.draws, file = "~/Leo/stan_models/test_results_1SEP/test_1SEP_over30_list_of_draws.RDS")


##############################################################
# trying to see some traceplots 

result$draws("w")
mcmc_hist(result$draws("w"))
mcmc_hist_by_chain(result$draws("w"))
mcmc_dens_overlay(result$draws("w"))
mcmc_trace(result$draws("intercept"))


mcmc_trace(list.of.draws$testdraws_22860)
mcmc_hist(list.of.draws$testdraws_22860)

list.of.results$testresult_2010
hist(data.list$t2_AOU, breaks = 25)

#################
