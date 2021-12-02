# New script for testing thresholds of abundance for species. 

# after combining the abundance of birds in each timepoint, 
# find a abundacne threshold that is meaningful. 
# (1 individual is not really meaningful, it could be there by accident)


# should i start from the +500 sp from both the matrices? the trait constraint does not really matter any more. 
# gonna prob source traits from other places instead. 

# start with all the +500 species of the matrices. 

# ALSO try using both mean of the 3 years and the max of the three years. 

# start by taking the species matrix

load("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/3years_x_timepoint/vegan_spmatrix_t2_mean.rda")
load("C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/Data/3years_x_timepoint/vegan_spmatrix_t1_mean.rda")

spmatrix.t2 <- as.data.frame(spmatrix.t2)
spmatrix.t1 <- as.data.frame(spmatrix.t1)

# create vector of AOUs appearing in t1 and t2
aous.t1ANDt2 <- c(colnames(spmatrix.t1), colnames(spmatrix.t2))

aous.t1ANDt2 <- as.factor(aous.t1ANDt2)
aous.t1ANDt2 <- as.data.frame(aous.t1ANDt2)

occurs <- aous.t1ANDt2 %>%
  group_by(aous.t1ANDt2) %>%
  summarise(n=n())

occurs.both <- subset(occurs, n == 2)
AOU.occurs.both <- occurs.both$aous.t1ANDt2


# AOU.occur.both is a list of AOUs which at least have records in both time point,
# even if those records are 0s. that is 508 species

AOU.occurs.both <- as.character(AOU.occurs.both)
AOU.occurs.both <- as.numeric(AOU.occurs.both)
AOU.occurs.both <- sort(AOU.occurs.both)
AOU.occurs.both <- as.character(AOU.occurs.both)

# now take the full sp matrices and isolate the species for which there are records of both time points
# making matrices for 508 sp that have records at t1 and t2
spmatrix.t1.both <- spmatrix.t1[AOU.occurs.both] %>% round(digits = 0)
spmatrix.t2.both <- spmatrix.t2[AOU.occurs.both] %>% round(digits = 0)

# add the abundances of t1 and t2 together

spmatrix.t1.t2 <- spmatrix.t1.both + spmatrix.t2.both


#######################################################

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
              
              # # # random effects and other params
              "sigma_observer" = runif(1, 0, 2),# "sigma_route" = runif(1, 0, 2),
              "b_time" = runif(1, -1, 1),  # "b_lc_q1" = runif(1, 0, 2), "b2_lc_q1" = runif(1, -2, 0),
              "b_temp" = runif(1, 0, 2), "b2_temp" = runif(1, -2, 0) 
  ))

#########################################################################
# now filter for the rows that have more than bla abundance 

# make loop
# test <- colSums(spmatrix.t1.t2 >= 50 )

# the number of rows (segments) that satisfy what ever criteria I am asking for 
# test <- as.data.frame(test)

# loop 
# 
# abundance_thesholds <- seq(30, 50, 20) # the number of birds in a segment
# segment_thesholds <- seq(30, 45, 15) # the number of segments that satisfy having > abundance theshold
# 

abundance_thesholds <- seq(5, 15, 10) # the number of birds in a segment
segment_thesholds <- seq(30, 30, 15) # the number of segments that satisfy having > abundance theshold


# # add all the different tests to a list
# thresholds_list <- list()

for (a in abundance_thesholds) {
  for (s in segment_thesholds) {
    segment_abund_count <- colSums( spmatrix.t1.t2 >= a) # count the num of segments for each sp with abund > theshold
    segment_abund_count <- as.data.frame(segment_abund_count) # dataframe from the named vector
    
    segment_abund_count <- filter(segment_abund_count, segment_abund_count > s )#remove the sp which have 0 segments above theshold
    assign( paste0("abund>=",a, "_segments>",s) , segment_abund_count )  
  }
}

#################################################################
library(readxl);library(dplyr); library(reshape); library(tidyverse); library(stringr)

library("rstan");library("loo"); library("cmdstanr"); library(posterior); library(bayesplot)
options(mc.cores = parallel::detectCores()); color_scheme_set("viridis")
cmdstan_path(); cmdstan_version()
# get the landcover data

landcover <- readRDS("~/Leo/Data/only_landcover.rda")

# lets start by testing different abundance thresholds and in at least 45 segments
loop.progress<- 0

for (a in abundance_thesholds) {
  for (s in segment_thesholds) {
    segment_abund_count <- colSums( spmatrix.t1.t2 >= a) # count the num of segments for each sp with abund > theshold
    segment_abund_count <- as.data.frame(segment_abund_count) # dataframe from the named vector
    
    segment_abund_count <- filter(segment_abund_count, segment_abund_count > s )#keep the sp which have >s segments above theshold
    assign( paste0("abund>=",a, "_segments>",s) , segment_abund_count )  
    experiment <- rownames(segment_abund_count)
    
    for(each.aou in experiment) {
      print(paste0("we now processing AOU of ", each.aou))
      # isolate the sp t2 and t1 abundance column
      t1_thisAOU <- select(spmatrix.t1,paste0(each.aou))[,1]
      t2_thisAOU <- select(spmatrix.t2,paste0(each.aou))[,1]
      
      # merge the t1 and t2 columns to find which partitions (rows) have no occurrences (0s)
      comb.data.thisAOU <- data.frame("partition" = partitions.4800, t1_thisAOU, t2_thisAOU)
      
      # isolate the segments for which there was at least 'a' recorded ind in t1 and t2 combined.
      df.nozeros.thisAOU <- subset(comb.data.thisAOU, (t1_thisAOU+t2_thisAOU) >= a ) 
      # add the corresponding LC values for the remaining partitions.
      df.nozeros.thisAOU.lc <- merge(df.nozeros.thisAOU, landcover, by.x = "partition") #merged by the landcover partition order.
      data <- df.nozeros.thisAOU.lc
      
      
      # # Option 5
      # # BUT NOW: must retain only segments 1,3,5 (But keep segments if they are the only one from their route)
      data <- data %>% filter(segmentID==1 | segmentID==3 | segmentID==5)
      
      
      
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
      vars3  <- c(
        # "delta.urban", "delta.forest", "delta.grass", "delta.crop",
        #             "delta.wet", "delta.lc.q1", "delta.temp", "delta.prec",
        "temp.t1", "temp.t2")
      for (i in vars3){ assign(i, as.vector(data[[i]])) }
      
      
      
      data.list  <- list(
        # response and number data points
        "t2_AOU" = as.vector(round(data[,3])), "N" = N, 
        
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
        ,"time"=data$time.t2,
        "temp_t1"=temp.t1, # "prec_t1", "prec_t2", "elev",
        "temp_t2"=temp.t2 , "temp_squared_t2"=temp.t2^2
      )
      
      # the inits are declared outside so they are the same for every AOU
      
      ## RUN IT 
      
      result <- model.legacy$sample(data = data.list,
                                    # init = inits.list,
                                    refresh = 10,
                                    chains = 4, parallel_chains = 4, #threads_per_chain = 2,
                                    iter_warmup = 500,
                                    iter_sampling = 1200,
                                    adapt_delta = 0.99, 
                                    max_treedepth = 15, #OG 10
                                    save_warmup = TRUE
                                    # ,thin = 10
      )
      
      loop.progress <- loop.progress + 1
      print(paste0(" Finished and have saved AOU = ", each.aou))
      print(paste0("This AOU had ", N, " partitions included"))
      print(paste0("This was species ", loop.progress, ", out of the total = ", length(experiment)))
      
      # not necesary to save the whole result file as an object
      # result$save_object(file = paste0("~/Leo/PUBLICATION/stan_results/test_12OCT_",each.aou,".RDS"))
      
      result.for.this.AOU <- result$summary()[1:20,]
      list.of.results[[paste0("testresult_", each.aou)]] <- result.for.this.AOU
      
      draws.for.this.AOU <- result$draws(variables = c("w"))
      list.of.draws[[paste0("testdraws_", each.aou)]] <- draws.for.this.AOU
      
    }
    save(list.of.results, file = paste0("~/Leo/PUBLICATION/stan_results/test_run_11NOV/list_of_results_a",a,"_s",s,".RDS"))
    save(list.of.draws, file = paste0("~/Leo/PUBLICATION/stan_results/test_run_11NOV/list_of_draws_a",a,"_s",s,".RDS"))
    # SEE OVER THIS!!! WHY DOES IT NOT SEEM TO SAVE THE RIGHT LISTS UNDER THE NAMES? 
    # all the lists have 32 items... ie they are all being overwritten by one of the lists ?? what 
  }
  
}

Sys.Date()
# save the list of tibbles we have created 

# load(file = "C:/Users/GlasgowGradient/OneDrive - University of Glasgow/Documents/Leo/PUBLICATION/stan_results/test_run_27OCT/list_of_draws_a50_s45.RDS")
