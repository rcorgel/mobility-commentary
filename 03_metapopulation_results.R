################################################################################
# File Name: 03_metapopulation_results                                         #
#                                                                              #
# Purpose:   Run the metapopulation model with different travel matrices.      #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format data                                           #
#            3.                                 #
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
set.seed(12345)

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

# Largest population cluster = 1
# Largest population patch = 47 (located in cluster 1)

# Test metapopulation model
test <- run_sir_model(density_dep = FALSE , R_0 = 1.5, gamma = 1/3, prop_s = 1.0, 
                      unit_vec = patch_num_vec, level = 'patch', pop_vec = patch_pop_vec, 
                      intro_num = 47, unit_x_walk = unit_x_walk, travel_mat = prob_trips_total, 
                      max_time = 365, time_step = 1)


