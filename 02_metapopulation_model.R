################################################################################
# File Name: 02_metapopulation_model                                           #
#                                                                              #
# Purpose:   Create a metapopulation model to examine spatial invasion         #
#            timing.                                                           #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create metapopulation model function                           #
#            3. Create multi-run model function                                #
#                                                                              #
# Project:   Mobility Commentary                                               #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/mobility-commentary-project')

###########################################
# 2. CREATE METAPOPULATION MODEL FUNCTION #
###########################################

# For testing
# density_dep = FALSE; R_0 = 1.5; gamma = 1/3; prop_s = 1.0
# unit_vec = patch_num_vec; level = 'patch'; pop_vec = patch_pop_vec 
# intro_num = 47; unit_x_walk = unit_x_walk; travel_mat = prob_trips_total
# max_time = 365; time_step = 1; i = 2

# Create a discrete time sir function
run_sir_model <- function(density_dep , R_0, gamma, prop_s, unit_vec, 
                          level = c('patch', 'cluster'), pop_vec, intro_num, 
                          unit_x_walk, travel_mat, max_time, time_step) {
  
  # Set number of locations
  N <- length(unit_vec)
  
  # Set the time vector from the max time and time step 
  times <- seq(1, max_time, time_step)
  # Make an object for the number of time steps
  n_steps <- length(times)
  # Create vector of repeated times for output data
  time_vec <- rep(seq(1, max_time, time_step), each = N)
  
  # Confirm level set correctly
  level <- match.arg(level)
  
  # Convert population vector to round numbers
  pop_vec <- ceiling(pop_vec)
  
  # Set density divisor and beta coefficient
  # If model is density dependent, then divisor = 1 and beta = (R_0 * gamma) / N
  # If model is frequency dependent, then divisor = N and beta = R_0 * gamma
  if (density_dep == TRUE) {
    divisor <- 1
    # Mean admin population is used to calculate beta
    beta <- (R_0 * gamma) / mean(pop_vec)
  }
  if (density_dep == FALSE) {
    divisor <- pop_vec
    beta <- R_0 * gamma
  }
  
  # Create empty matrices to fill with each location's SIR results
  s_mat <- matrix(NA, N, n_steps)
  rownames(s_mat) <- unit_vec
  i_mat <- matrix(NA, N, n_steps)
  rownames(i_mat) <- unit_vec
  r_mat <- matrix(NA, N, n_steps)
  rownames(r_mat) <- unit_vec
  incid_i_mat <- matrix(NA, N, n_steps)
  rownames(incid_i_mat) <- unit_vec
  
  # Set initial states
  s_mat[, 1] <- ceiling(pop_vec*prop_s)          # set susceptible population
  i_mat[, 1] <- pop_vec*0                        # set no infected population
  r_mat[, 1] <- ceiling(pop_vec*(1 - prop_s))    # set number recovered/immune
  incid_i_mat[, 1] <- 0                          # set number of incident infections
  
  # Set introduction information
  # Place an infected individual in the admin unit
  i_mat[intro_num, 1] <- 100
  
  # Loop through time steps
  for (i in 2:n_steps) {
    # Replace NAs with 0 in travel matrix
    travel_mat[is.na(travel_mat)] <- 0
    # Create population matrix for travel stochastic element so trips are estimated based on population
    pop_mat <- matrix(ceiling(pop_vec), nrow = length(pop_vec), ncol = length(pop_vec))
    # Stochastic element of travel, pulling from a binomial distribution
    travel_mat_binom <- matrix(rbinom(length(travel_mat), pop_mat, travel_mat), 
                               nrow = nrow(travel_mat))
    # There should not be NAs, but replace just in case
    travel_mat_binom[is.na(travel_mat_binom)] <- 0
    # Normalize new travel matrix since output it no longer proportions
    travel_mat_binom_norm <- travel_mat_binom / rowSums(travel_mat_binom)
    
    # Estimate force of infection
    # Equation 2 from https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1006600
    prob_infect <- 1 - exp(-time_step * beta * travel_mat_binom_norm %*%
    (t(travel_mat_binom_norm) %*% (i_mat[, i-1] / divisor)) /
    ((t(travel_mat_binom_norm) %*% ((s_mat[, i-1] + r_mat[, i-1]) / divisor)) +
    (t(travel_mat_binom_norm) %*% (i_mat[, i-1] / divisor))))
    # Estimate the number of new infections, pulling from a binomial distribution 
    new_infect <- rbinom(N, as.vector(s_mat[, i-1]), as.vector(prob_infect))
    
    # Estimate the probability of recovery
    prob_recover <- 1 - exp(-time_step * gamma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_recover <- rbinom(N, as.vector(i_mat[, i-1]), as.vector(prob_recover))
    
    # Fill in each state accordingly
    # Susceptible
    s_mat[, i] <- s_mat[, i-1] - new_infect
    # Infections
    i_mat[, i] <- i_mat[, i-1] + new_infect - new_recover
    # Incident Infections
    incid_i_mat[, i] <- new_infect
    # Recoveries
    r_mat[, i] <- r_mat[, i-1] + new_recover
  }
  
  # Create single run data set
  # Merge together the matrices, this converts them from wide to long
  combine_mat <- cbind(c(time_vec), c(s_mat), c(i_mat), c(incid_i_mat), c(r_mat))
  # Convert to data frame
  combine_dat <- as.data.frame(combine_mat)
  # Assign variable names
  names(combine_dat) <- cbind('time', 'S', 'I', 'incid_I', 'R')
  # Add admin level variable
  combine_dat$level <- level
  # Add district name variable
  combine_dat$unit_num <- c(unit_vec)
  # Merge on higher level admin information
  if (level == 'cluster') {
    combine_dat <- combine_dat %>%
      dplyr::rename('cluster_num' = 'unit_num')
  }
  if (level == 'patch') {
    combine_dat <- combine_dat %>%
      dplyr::rename('patch_num' = 'unit_num')
    combine_dat <- left_join(combine_dat, unit_x_walk,  by = c('patch_num' = 'patch_num'))
  }
  
  # Return results
  return(combine_dat)
}

######################################
# 2. CREATE MULTI-RUN MODEL FUNCTION #
######################################

run_sir_model_multi <- function(n, density_dep, method = c('average', 'append', 'run_sum'),
                                R_0, gamma, prop_s, unit_vec, 
                                level = c('patch', 'cluster'), pop_vec, intro_num,
                                unit_x_walk, travel_mat, max_time, time_step) {
  
  # Loop through multiple runs of the SIR model
  for (i in 1:n) {
    # Run SIR model
    single_run <- run_sir_model(density_dep = density_dep, R_0 = R_0, gamma = gamma, 
                                prop_s = prop_s, unit_vec = unit_vec, level = level, 
                                pop_vec = pop_vec, unit_x_walk = unit_x_walk,
                                intro_num = intro_num, travel_mat = travel_mat, 
                                max_time = max_time, time_step = time_step)
    
    # Create run number variable
    single_run$run_num <- i
    
    # If first run, assign as multi_run
    if (i == 1) {
      multi_run <- single_run
    }
    
    # Add single_run to multi_run
    if (i != 1) {
      # Append runs together
      multi_run <- rbind(multi_run, single_run)
    }
  }
  
  # Confirm method set correctly
  method <- match.arg(method)
  
  # Separate out different methods
  # If append, keep as is
  if (method == 'append') {
  }
  # If run_sum, sum incident infections by run number
  if (method == 'run_sum') {
    multi_run <- multi_run %>%
      group_by(run_num) %>%
      mutate(incid_I_sum = sum(incid_I)) %>%
      distinct(run_num, incid_I_sum, .keep_all = FALSE)
  }
  # If average, average across time periods and admin units
  # Different by admin level
  if (method == 'average') {
    if (level == 'cluster') {
      multi_run <- multi_run %>%
        group_by(time, cluster_num) %>%
        mutate(S_avg = mean(S), 
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) %>%
        distinct(time, cluster_num, S_avg, I_avg, incid_I_avg, R_avg, level, 
                 .keep_all = FALSE)
    }
    if (level == 'patch') {
      multi_run <- multi_run %>%
        group_by(time, patch_num) %>%
        mutate(S_avg = mean(S), 
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) %>%
        distinct(time, patch_num, S_avg, I_avg, incid_I_avg, R_avg, level, 
                 cluster_num, .keep_all = FALSE)
    }
  }
  # Return output data
  return(multi_run)
}

################################################################################
################################################################################
