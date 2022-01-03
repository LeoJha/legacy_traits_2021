
w_TT_df <- readRDS(file = "C:/beyond_thesis/PUBLICATION/draws_traits_data/from_a15_s30/w_TT_df_59sp_3JAN.RDS")



########### exploratory ggplot plotting of the raw data ######## 
library(ggplot2)


############## LIFESPAN
ggplot(w_TT_df, mapping = aes(x = maximum_lifespan_years, y = w )) +
  geom_point(size = 1, alpha = 0.1)+
  # geom_boxplot()+
  theme(legend.position = "none") 
geom_smooth(method = "lm")

############## MASS
ggplot(w_TT_df, mapping = aes(x = log_transformed_body_mass_grams, y =w)) +
  geom_point(size = .1, alpha = 0.2)+
  theme(legend.position = "none") +
  geom_smooth(method = "lm")

############## WINGSPAN
ggplot(w_TT_df, mapping = aes(x = wingspan_inches, y =w, colour = AOU)) +
  geom_point(size = .1)+
  theme(legend.position = "none")


############# DIET
ss_diet <- w_TT_df %>%
  group_by(diet) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  # geom_jitter(mapping = aes(x = diet, y =w), size = 1, alpha = 0.5) +
  geom_violin(mapping = aes(x = diet, y =w), alpha = 1) +
  theme(legend.position = "none") +
  geom_text(ss_diet, mapping=aes(x=diet,y=-0.05, label=paste("n=",n)))
# ,position = position_dodge(width = 0.8))

############ FORAGING
ss_forage <- w_TT_df %>%
  group_by(foraging) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, # mapping = aes(x = foraging, y =w, colour = AOU)
) +
  # geom_jitter(size = 1, alpha = 0.5) +
  geom_violin(mapping = aes(x = foraging, y =w) ) +
  theme(legend.position = "none") +
  geom_text(ss_forage, mapping=aes(x=foraging,y=-0.05, label=paste("n =",n)))


############# MIGRATION
ss_mig <- w_TT_df %>%
  group_by(migration) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df,# mapping = aes(x = migration, y =w)
) +
  # geom_jitter(size = scale(w_TT_df$maximum_lifespan_years), alpha = 0.5) +
  geom_violin(mapping = aes(x = migration, y =w, fill = "blue") ) +
  theme(legend.position = "none") +
  geom_text(ss_mig, mapping=aes(x=migration,y=-0.05, label=paste("n =",n)))

############## N ANNUAL BROODS
ss_brd <- w_TT_df %>%
  group_by(as.factor(number_of_brood_per_year)) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, mapping = aes(x = number_of_brood_per_year, y =w))+
  geom_jitter( size = .1, alpha = 0.05)+
  geom_smooth(method = "lm")+
  geom_text(ss_brd, mapping=aes(x=as.factor(number_of_brood_per_year),y=-0.05, label=paste("n =",n)))

# geom_violin(mapping = aes(x = as.factor(number_of_brood_per_year), y =w ) ) +

theme(legend.position = "none") 

############# AVG CLUTCH SIZE
ggplot(w_TT_df, )+
  geom_jitter(mapping = aes(x = mean_clutch_size, y =w, colour = AOU), size = .1, alpha = 0.3)+
  # geom_violin(mapping = aes(x = mean_clutch_size, y =w ) ) +
  
  theme(legend.position = "none") 

############ NEST TYPE
ss_nesttype <- w_TT_df %>%
  group_by(nest_type) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  # geom_jitter( mapping = aes(x = nest_type, y =w), size = .1, alpha = 0.1) +
  geom_violin(mapping = aes(x = nest_type, y =w, fill = "blue") ) +
  theme(legend.position = "none") +
  geom_text(ss_nesttype, mapping=aes(x=nest_type,y=-0.05, label=paste("n =",n)))

############ NEST LOCATION
ss_nestloc <- w_TT_df %>%
  group_by(nest_location) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  # geom_jitter( mapping = aes(x = nest_location, y =w, colour = nest_type), size = .1, alpha = 0.5)+
  geom_violin(mapping = aes(x = nest_location, y =w, fill = "blue") ) +
  theme(legend.position = "none") +
  geom_text(ss_nestloc, mapping=aes(x=nest_location,y=-0.05, label=paste("n =",n)))

########### MAIN HABITAT
ss_mhab <- w_TT_df %>%
  group_by(main_habitat) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  geom_point( mapping = aes(x = main_habitat, y =w), size = 1, alpha = 0.01)+
  # geom_violin(mapping = aes(x = main_habitat, y =w) ) +
  theme(legend.position = "none") +
  geom_text(ss_mhab, mapping=aes(x=main_habitat,y=-0.05, label=paste("n =",n)))

########### TERRITORIALITY
ss_terr <- w_TT_df %>%
  group_by(territoriality) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  # geom_jitter( mapping = aes(x = territoriality, y =w, colour = nest_type), size = .1, alpha = 0.5)+
  geom_violin(mapping = aes(x = territoriality, y =w) ) +
  theme(legend.position = "none") +
  geom_text(ss_terr, mapping=aes(x=territoriality,y=-0.05, label=paste("n =",n)))

########## DEVELOPMENTAL MODE
ss_dmod <- w_TT_df %>%
  group_by(developmental_mode) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  geom_jitter( mapping = aes(x = developmental_mode, y =w, colour = main_habitat), size = 1, alpha = 0.5)+
  # geom_violin(mapping = aes(x = developmental_mode, y =w) ) +
  theme(legend.position = "none") +
  geom_text(ss_dmod, mapping=aes(x=developmental_mode,y=-0.05, label=paste("n =",n)))

########## MATING
ss_mating <- w_TT_df %>%
  group_by(mating) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  # geom_jitter( mapping = aes(x = mating, y =w, colour = main_habitat), size = 1, alpha = 0.5)+
  geom_violin(mapping = aes(x = mating, y =w) ) +
  theme(legend.position = "none") +
  geom_text(ss_mating, mapping=aes(x=mating,y=-0.05, label=paste("n =",n)))


############ BREEDING SYSTEM
ss_bsys <- w_TT_df %>%
  group_by(breeding_system) %>%
  summarise(n=length(unique(AOU)))

ggplot(w_TT_df, ) +
  geom_jitter( mapping = aes(x = breeding_system, y =w), size = 1, alpha = 0.04)+
  geom_boxplot(mapping = aes(x = breeding_system, y =w), alpha = 0, size = 1, colour = "red" ) +
  theme(legend.position = "none") 
geom_text(ss_bsys, mapping=aes(x=breeding_system,y=-0.05, label=paste("n =",n)))



