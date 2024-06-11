################################################################################
# File Name: 03_metapopulation_results                                         #
#                                                                              #
# Purpose:   Run the metapopulation model with different travel matrices.      #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format data                                           #
#            3. Run Simulations for each travel matrix                         #
#            4. Calculate epidemic statistics for each model result            #
#                                                                              #
# Project:   Mobility Commentary                                               #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Load metapopulation model functions
source('./mobility-commentary/02_metapopulation_model.R')

# Load libraries
library(tidyverse)

# Set the seed
set.seed(123)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/mobility-commentary-project')

###########################
# 2. LOAD AND FORMAT DATA #
###########################

# Load metapop data
load('./tmp/metapop_dat_probs.RData')

# Create x walk from patches to clusters
unit_x_walk <- patch_df %>% rename('patch_num' = 'patch',
                                   'cluster_num' = 'cluster')

# Create population vectors
# Patches
patch_pop_vec <- popsize
# Clusters
unit_x_walk$population <- popsize
cluster_pop <- unit_x_walk %>% group_by(cluster_num) %>%
  mutate(cluster_population = sum(population)) %>%
  distinct(cluster_num, cluster_population, .keep_all = FALSE) %>%
  arrange(cluster_num)
cluster_pop_vec = cluster_pop$cluster_population

# Create unit number vectors
patch_num_vec <- unit_x_walk$patch_num
cluster_num_vec <- cluster_pop$cluster_num

# Largest Population Patch = 6
# Patch 6 is located in cluster 2

#############################################
# 3. RUN SIMULATIONS FOR EACH TRAVEL MATRIX #
#############################################
# True Matrix
true_result <- run_sir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                   R_0 = 1.5, gamma = 1/3, prop_s = 0.90, unit_vec = patch_num_vec, 
                                   level = 'patch', pop_vec = patch_pop_vec, intro_num = 6, 
                                   unit_x_walk = unit_x_walk, travel_mat = prob_trips_total, 
                                   max_time = 365, time_step = 1, initial_num = 1)

# Adult Matrix
adult_result <- run_sir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                    R_0 = 1.5, gamma = 1/3, prop_s = 0.90, unit_vec = patch_num_vec, 
                                    level = 'patch', pop_vec = patch_pop_vec, intro_num = 6, 
                                    unit_x_walk = unit_x_walk, travel_mat = prob_trips_adults, 
                                    max_time = 365, time_step = 1, initial_num = 1)

# Censor Matrix
censor_result <- run_sir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/3, prop_s = 0.90, unit_vec = patch_num_vec, 
                                     level = 'patch', pop_vec = patch_pop_vec, intro_num = 6, 
                                     unit_x_walk = unit_x_walk, travel_mat = prob_trips_censor, 
                                     max_time = 365, time_step = 1, initial_num = 1)

# Weekly Matrix
weekly_result <- run_sir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/3, prop_s = 0.90, unit_vec = patch_num_vec, 
                                     level = 'patch', pop_vec = patch_pop_vec, intro_num = 6, 
                                     unit_x_walk = unit_x_walk, travel_mat = prob_trips_weekly, 
                                     max_time = 365, time_step = 1, initial_num = 1)

# Region Matrix
region_result <- run_sir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/3, prop_s = 0.90, unit_vec = cluster_num_vec, 
                                     level = 'cluster', pop_vec = cluster_pop_vec, intro_num = 2, 
                                     unit_x_walk = unit_x_walk, travel_mat = prob_trips_region, 
                                     max_time = 365, time_step = 1, initial_num = 1)

#####################################################
# 4. CALCULATE EPIDEMIC STATS FOR EACH MODEL RESULT #
#####################################################

# Loop through patch level results and calculate arrival times
result <- NULL
result[[1]] <- true_result; result[[2]] <- adult_result; 
result[[3]] <- censor_result; result[[4]] <- weekly_result; 
arrival_times_patch <- NULL
attack_rate_patch <- NULL
for (i in 1:4) {
  result_intro <- result[[i]] %>%
    # Calculate average infections across simulation
    group_by(time, patch_num) %>%
    mutate(avg_incid_I = mean(incid_I),
           sd_incid_I = sd(incid_I)) %>%
    distinct(time, patch_num, avg_incid_I, sd_incid_I, population) %>% 
    ungroup() %>%
    # Calculate 95% case range for each time step
    mutate(avg_incid_I_low = avg_incid_I - 1.96*(sd_incid_I/sqrt(500)),
           avg_incid_I_high = avg_incid_I + 1.96*(sd_incid_I/sqrt(500))) %>%
    # Calculate cumulative cases by patch
    group_by(patch_num) %>%
    # Indicate when cumulative cases > 5 for each patch
    mutate(cumu_I = cumsum(avg_incid_I),
           cumu_I_low = cumsum(avg_incid_I_low),
           cumu_I_high = cumsum(avg_incid_I_high),
           intro = ifelse(cumu_I > 5, 1, 0),
           intro_low = ifelse(cumu_I_low > 5, 1, 0),
           intro_high = ifelse(cumu_I_high > 5, 1, 0)) %>%
    # Indicate the first instance when cumulative > 5
    mutate(intro_first = intro == 1 & !duplicated(intro == 1),
           intro_first_low = intro_low == 1 & !duplicated(intro_low == 1),
           intro_first_high = intro_high == 1 & !duplicated(intro_high == 1)) 
  
  # Calculate attack rate
  attack_rate <- result_intro %>% group_by(patch_num) %>% 
    filter(time == 365) %>% mutate(attack_rate = cumu_I / population,
                                   attack_rate_high = cumu_I_high / population,
                                   attack_rate_low = cumu_I_low / population)
  
  # Extract arrival times
  result_intro_mean <- result_intro %>% filter(intro_first == TRUE)
  result_intro_low <- result_intro %>% filter(intro_first_low == TRUE)
  result_intro_high <- result_intro %>% filter(intro_first_high == TRUE)
  
  # Merge arrival times
  result_intro_merge <- left_join(result_intro_mean[, c('patch_num', 'time')], 
                                  result_intro_low[, c('time', 'patch_num')],
                                  by = c('patch_num' = 'patch_num'))
  result_intro_merge <- left_join(result_intro_merge, 
                                  result_intro_high[, c('time', 'patch_num')],
                                  by = c('patch_num' = 'patch_num'))
  result_intro_merge <- result_intro_merge %>% rename('time_mean' = 'time.x',
                                                      'time_slow' = 'time.y',
                                                      'time_fast' = 'time')
  # Fill the empty object with results
  arrival_times_patch[[i]] <- result_intro_merge
  attack_rate_patch[[i]] <- attack_rate
}

# Aggregate true matrix results into cluster level
true_result_cluster <- true_result %>%
  group_by(time, cluster_num, run_num) %>%
  mutate(incid_I = sum(incid_I)) %>%
  distinct(time, cluster_num, run_num, incid_I) %>% 
  ungroup()

# Loop through cluster level results and calculate arrival times
result_cluster <- NULL
result_cluster[[1]] <- true_result_cluster; result_cluster[[2]] <- region_result; 
arrival_times_cluster <- NULL
attack_rate_cluster <- NULL
for (i in 1:2) {
  result_intro <- result_cluster[[i]] %>%
    # Calculate average infections across simulation
    group_by(time, cluster_num) %>%
    mutate(avg_incid_I = mean(incid_I),
           sd_incid_I = sd(incid_I)) %>%
    distinct(time, cluster_num, avg_incid_I, sd_incid_I) %>% 
    ungroup() %>%
    # Calculate 95% case range for each time step
    mutate(avg_incid_I_low = avg_incid_I - 1.96*(sd_incid_I/sqrt(500)),
           avg_incid_I_high = avg_incid_I + 1.96*(sd_incid_I/sqrt(500))) %>%
    # Calculate cumulative cases by patch
    group_by(cluster_num) %>%
    # Indicate when cumulative cases > 50 for each patch
    mutate(cumu_I = cumsum(avg_incid_I),
           cumu_I_low = cumsum(avg_incid_I_low),
           cumu_I_high = cumsum(avg_incid_I_high),
           intro = ifelse(cumu_I > 50, 1, 0),
           intro_low = ifelse(cumu_I_low > 50, 1, 0),
           intro_high = ifelse(cumu_I_high > 50, 1, 0)) %>%
    # Indicate the first instance when cumulative > 50
    mutate(intro_first = intro == 1 & !duplicated(intro == 1),
           intro_first_low = intro_low == 1 & !duplicated(intro_low == 1),
           intro_first_high = intro_high == 1 & !duplicated(intro_high == 1)) 
  
  result_intro <- left_join(result_intro, cluster_pop, 
                            by = c('cluster_num' = 'cluster_num'))
  
  # Calculate attack rate
  attack_rate <- result_intro %>% group_by(cluster_num) %>% 
    filter(time == 365) %>% mutate(attack_rate = cumu_I / cluster_population,
                                   attack_rate_high = cumu_I_high / cluster_population,
                                   attack_rate_low = cumu_I_low / cluster_population)
  
  # Extract arrival times
  result_intro_mean <- result_intro %>% filter(intro_first == TRUE)
  result_intro_low <- result_intro %>% filter(intro_first_low == TRUE)
  result_intro_high <- result_intro %>% filter(intro_first_high == TRUE)
  
  # Merge arrival times
  result_intro_merge <- left_join(result_intro_mean[, c('cluster_num', 'time')], 
                                  result_intro_low[, c('time', 'cluster_num')],
                                  by = c('cluster_num' = 'cluster_num'))
  result_intro_merge <- left_join(result_intro_merge, 
                                  result_intro_high[, c('time', 'cluster_num')],
                                  by = c('cluster_num' = 'cluster_num'))
  result_intro_merge <- result_intro_merge %>% rename('time_mean' = 'time.x',
                                                      'time_slow' = 'time.y',
                                                      'time_fast' = 'time')
  # Fill the empty object with results
  arrival_times_cluster[[i]] <- result_intro_merge
  attack_rate_cluster[[i]] <- attack_rate
}

# Save arrival time results
arrival_times_true <- arrival_times_patch[[1]]
arrival_times_adult <- arrival_times_patch[[2]]
arrival_times_censor <- arrival_times_patch[[3]]
arrival_times_weekly <- arrival_times_patch[[4]]
arrival_times_true_cluster <- arrival_times_cluster[[1]]
arrival_times_region <- arrival_times_cluster[[2]]
attack_rate_true <- attack_rate_patch[[1]]
attack_rate_adult <- attack_rate_patch[[2]]
attack_rate_censor <- attack_rate_patch[[3]]
attack_rate_weekly <- attack_rate_patch[[4]]
attack_rate_true_cluster <- attack_rate_cluster[[1]]
attack_rate_region <- attack_rate_cluster[[2]]

save(list = c('arrival_times_true', 'arrival_times_adult', 'arrival_times_censor',
              'arrival_times_weekly', 'arrival_times_true_cluster', 'arrival_times_region'),
     file = './tmp/arrival_times.RData')
save(list = c('attack_rate_true', 'attack_rate_adult', 'attack_rate_censor',
              'attack_rate_weekly', 'attack_rate_true_cluster', 'attack_rate_region'),
     file = './tmp/attack_rate.RData')

# Save average true results for epi curve figure
avg_true_cluster <- true_result_cluster %>%
  group_by(time, cluster_num) %>%
  mutate(avg_incid_I = mean(incid_I)) %>%
  distinct(time, cluster_num, avg_incid_I) 
avg_true_patch <- true_result %>%
  group_by(time, patch_num) %>%
  mutate(avg_incid_I = mean(incid_I)) %>%
  distinct(time, patch_num, avg_incid_I)
save(list = c('avg_true_patch', 'avg_true_cluster'),
     file = './tmp/true_epi_curves.RData')

################################################################################
################################################################################
