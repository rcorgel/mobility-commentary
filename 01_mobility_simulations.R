################################################################################
# File Name: 01_mobility_simulations                                           #
#                                                                              #
# Purpose:   Simulate mobility under various conditions (adult-only travel,    #
#            censoring, time-aggregated travel, spatially-aggregated travel)   #            
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Set initial conditions                                         #
#            3. Calculate number/proportion of trips for children and adults   #
#            4. Calculate combined number/proportion of trips                  #
#            5. Create censored travel matrix                                  #
#            6. Create weekly travel matrix                                    #             
#            7. Create region travel matrix                                    #
#            8. Save and plot                                                  #
#            9. Get coefficients from fitting mobility models                  #
#                                                                              #
# Project:   Mobility Commentary                                               #
# Author:    Natalya Kostandova                                                #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggsci)
library(dplyr)
library(tidyr)
library(cowplot)
library(mobility)

# Set the seed
set.seed(123)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/mobility-commentary-project')

#############################
# 2. SET INITIAL CONDITIONS #
#############################

# Set up initial patches
n = 50
xlocs<-runif(n,0,500)
ylocs<-runif(n,0,500)
popsize<-round(rgamma(50,1,2)*250000)
dists<-as.matrix(dist(cbind(xlocs,ylocs)))
diag(dists)<-1

# Parameters for children and adult travel
alpha_adult = 1.2
beta_adult = 1.5
gamma_adult = 40
theta_adult = 0.001
alpha_child = alpha_adult
beta_child = alpha_adult * 3
gamma_child = gamma_adult * 100
theta_child = theta_adult

# set proportion of population that's children
N_child_prop = runif(n, 0.40, 0.50)
N_child = round(N_child_prop * popsize)
N_adult = popsize - N_child

###################################################################
# 3. CALCULATE NUMBER/PROPORTION OF TRIPS FOR CHILDREN AND ADULTS #
###################################################################

# Generate trips for one day
trips_adults = matrix(NA, n, n)
for (i in 1:n){
  for (j in 1:n){
    trips_adults[i,j] = theta_adult *  N_adult[i]^alpha_adult*N_adult[j]^beta_adult/exp(-dists[i,j]/gamma_adult)
  }
}
trips_child = matrix(NA, n, n)
for (i in 1:n){
  for (j in 1:n){
    trips_child[i,j] = theta_child *  N_child[i]^alpha_child*N_child[j]^beta_child/exp(-dists[i,j]/gamma_child)
  }
}

# Get probabilities of travel
# get departure probabilities - assume proportional to population size
# prob_depart_child = 50 / N_child
# prob_depart_adult = 250 / N_adult
prob_depart_child = rnorm(n,  0.02, sd = 0.02)
prob_depart_child = replace(prob_depart_child, prob_depart_child<0.00001, 0.001)
prob_depart_child = prob_depart_child[order(prob_depart_child)]
prob_depart_child = prob_depart_child[rank(-N_child)]
prob_depart_adult = rnorm(n, 0.20, sd = 0.04)  # order by population size
prob_depart_adult = replace(prob_depart_adult, prob_depart_adult<0.00001, 0.001)
prob_depart_adult = prob_depart_adult[order(prob_depart_adult)]
prob_depart_adult = prob_depart_adult[rank(-N_adult)]

# Calculate proportion of travel for adults
diag(trips_adults) = NA
# scale them accordingly
prob_trips_adults = trips_adults
prob_trips_adults = prob_trips_adults / rowSums(prob_trips_adults, na.rm=TRUE)
for (i in 1:nrow(prob_trips_adults)){
  prob_trips_adults[i,] = prob_trips_adults[i,] * (prob_depart_adult[i])
  diag(prob_trips_adults)[i] = 1 - prob_depart_adult[i]
}
rowSums(prob_trips_adults)

# Calcule proportion of travel for children
diag(trips_child) = NA
# scale them accordingly
prob_trips_child = trips_child
prob_trips_child = prob_trips_child / rowSums(prob_trips_child, na.rm=TRUE)
for (i in 1:nrow(prob_trips_child)){
  prob_trips_child[i,] = prob_trips_child[i,] * (prob_depart_child[i])
  diag(prob_trips_child)[i] = 1 - prob_depart_child[i]
}
rowSums(prob_trips_child)

####################################################
# 4. CALCULATE COMBINED NUMBER/PROPORTION OF TRIPS #
####################################################

# Convert to trips using proportions and numbers of children/adults
child_trips_wdiag = prob_trips_child * N_child
adult_trips_wdiag = prob_trips_adults * N_adult
total_trips_wdiag = child_trips_wdiag + adult_trips_wdiag
# calculate probability of travel for total population
prob_trips_total = matrix(NA, nrow = nrow(total_trips_wdiag), ncol = ncol(total_trips_wdiag))
for (i in 1:nrow(prob_trips_total)){
  prob_trips_total[i,] = total_trips_wdiag[i,] / sum(total_trips_wdiag[i,])
}

####################################
# 5. CREATE CENSORED TRAVEL MATRIX #
####################################

censoring_cutoff = 900

# Censoring - drop trips <censoring_cutoff
censored_total_trips = total_trips_wdiag
censored_total_trips[censored_total_trips<censoring_cutoff]<-NA
# get probabilities
prob_trips_censor = matrix(NA, nrow = nrow(censored_total_trips), ncol = ncol(censored_total_trips))
for (i in 1:nrow(prob_trips_censor)){
  prob_trips_censor[i,] = censored_total_trips[i,] / sum(censored_total_trips[i,], na.rm=TRUE)
}
rowSums(prob_trips_censor, na.rm = TRUE)

##################################
# 5. CREATE WEEKLY TRAVEL MATRIX #
##################################

# Weekly probabilities
# get probability of departure 
dept_daily = 1 - diag(prob_trips_total)
dept_weekly = 1 - exp(-dept_daily * 7)
# distribute diffusion in the same way as before
prob_trips_weekly = prob_trips_total
diag(prob_trips_weekly) = NA
for (i in 1:nrow(prob_trips_weekly)){
  prob_trips_weekly[i,] = prob_trips_weekly[i,] / (sum(prob_trips_weekly[i,], na.rm=TRUE)) * (dept_weekly[i])
}
diag(prob_trips_weekly) = 1 - dept_weekly
# get total number of trips
total_trips_weekly = round(prob_trips_weekly * popsize)
rowSums(prob_trips_weekly)

##################################
# 5. CREATE REGION TRAVEL MATRIX #
##################################

# Regions
# Split into 5 regions using kmeans clustering
# Create the matrix of patch coordinates
patch_coords <- as.matrix(data.frame(x = xlocs, y = ylocs))
# Cluster the patches into 5 groups using k-means
kmeans_clusters <- kmeans(patch_coords, centers = 5)
patch_df = as.data.frame(patch_coords)
# Extract the cluster assignments from the k-means output
patch_df$cluster <- kmeans_clusters$cluster
patch_df = patch_df %>% mutate(patch = row_number())
# check that they split up ok
ggplot(patch_df) + geom_point(aes(x = x, y = y, color = as.factor(cluster))) + scale_color_nejm()
# now need to aggregate the patch matrix into region matrix
# make into data frame
df_og = as.data.frame(total_trips_wdiag)
df_og$patch_origin = rownames(df_og)
df_og = df_og %>% pivot_longer(cols = -c(patch_origin), names_to = "patch_dest", values_to = "trips")
# fix patch destination name to drop "V"
df_og$patch_dest = gsub("V", "", df_og$patch_dest)
# merge in region number
df_og= merge(df_og, patch_df %>% dplyr::select(cluster, patch), by.x = "patch_origin", by.y = "patch", all.x=TRUE)
df_og = df_og %>% rename(region_origin = cluster)
df_og= merge(df_og, patch_df %>% dplyr::select(cluster, patch), by.x = "patch_dest", by.y = "patch", all.x=TRUE)
df_og = df_og %>% rename(region_dest = cluster)
# now aggregate
df_og = df_og %>% group_by(region_origin, region_dest) %>% summarize(trips = sum(trips))
# make mobility matrix
trips_region = get_mob_matrix(orig = df_og$region_origin,
                              dest = df_og$region_dest,
                              value = df_og$trips)
trips_region = round(trips_region)
#get probability of travel
prob_trips_region= matrix(NA, nrow = nrow(trips_region), ncol = ncol(trips_region))
for (i in 1:nrow(prob_trips_region)){
  prob_trips_region[i,] = trips_region[i,] / sum(trips_region[i,])
}
rowSums(prob_trips_region)

####################
# 8. SAVE AND PLOT #
####################

# Save output
matrices_abs = list(total_trips_wdiag, adult_trips_wdiag, child_trips_wdiag,
                    censored_total_trips, total_trips_weekly, trips_region, popsize, patch_df)
matrices_prob = list(prob_trips_total, prob_trips_adults, prob_trips_child, 
                     prob_trips_censor, prob_trips_weekly, prob_trips_region, popsize, patch_df)
save(list = c('total_trips_wdiag', 'adult_trips_wdiag', 'child_trips_wdiag',
              'censored_total_trips', 'total_trips_weekly', 'trips_region', 
              'popsize', 'patch_df'), file = './tmp/metapop_dat_abs.RData')
save(list = c('prob_trips_total', 'prob_trips_adults', 'prob_trips_child', 
              'prob_trips_censor', 'prob_trips_weekly', 'prob_trips_region', 
              'popsize', 'patch_df'), file = './tmp/metapop_dat_probs.RData')

mean(diag(prob_trips_total))
mean(diag(prob_trips_adults))
mean(diag(prob_trips_child))
mean(diag(prob_trips_weekly))
mean(diag(prob_trips_region))
mean(diag(prob_trips_censor))

mean(prob_trips_total[row(prob_trips_total)!=col(prob_trips_total)])
mean(prob_trips_adults[row(prob_trips_adults)!=col(prob_trips_adults)])
mean(prob_trips_censor[row(prob_trips_censor)!=col(prob_trips_censor)], na.rm = TRUE)
mean(prob_trips_weekly[row(prob_trips_weekly)!=col(prob_trips_weekly)])
mean(prob_trips_region[row(prob_trips_region)!=col(prob_trips_region)])




# Plot
# breaks <- c(0.25, 0.5, 0.75, 1)
# plot_fun = function(dataset){
#   plot_df = ggplot(data=reshape2::melt(dataset)) +
#     geom_tile(aes(x=factor(Var2),
#                   y=factor(Var1),
#                   fill=log(value))) +
#     xlab('Destination') + ylab("Origin") +
#     theme_bw() + theme(axis.text.x=element_text(size=10),
#                        axis.text.y=element_text(size=10),
#                        axis.title.x=element_text(size=12, margin = margin(t = 15)),
#                        axis.title.y=element_text(size=12, margin = margin(r = 15)),
#                        legend.position='bottom') +
#     viridis::scale_fill_viridis(option='inferno', 
#                                 direction=1,
#                                 breaks=log(breaks),
#                                 labels=breaks) +
#     guides(fill=guide_colorbar(title='Probability of travel',
#                                title.position='top',
#                                label.theme=element_text(size=9),
#                                barwidth=20,
#                                barheight=0.5,
#                                frame.colour='black',
#                                ticks=TRUE))
#   return(plot_df)
# }
# 
# true_plot = plot_fun(prob_trips_total)
# adults_plot = plot_fun(prob_trips_adults)
# censored_plot = plot_fun(prob_trips_censor)
# weekly_plot = plot_fun(prob_trips_weekly)
# region_plot = plot_fun(probs_trips_region)
# prow = cowplot::plot_grid(true_plot + ggtitle("True") + theme(legend.position="none") +  theme(axis.text.x=element_blank(), axis.text.y = element_blank()
# ), 
# adults_plot + ggtitle("Adults") + theme(legend.position="none") +  theme(axis.text.x=element_blank(), axis.text.y = element_blank()
# ), 
# censored_plot + ggtitle("Censored") + theme(legend.position="none") +  theme(axis.text.x=element_blank(), axis.text.y = element_blank()
# ), 
# weekly_plot + ggtitle("Weekly")+ theme(legend.position="none")+ theme(axis.text.x=element_blank(), axis.text.y = element_blank()
# ), 
# region_plot + ggtitle("Regional")+ theme(legend.position="none")+ theme(axis.text.x=element_blank(), axis.text.y = element_blank()
# ), 
# nrow = 3)
# legend <- get_legend(
#   # create some space to the left of the legend
#   true_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
# )
# plot_grid(prow, legend, nrow = 2, rel_heights = c(10, 1))
# ggsave(filename = './figs/matrices.pdf')

####################################################
# 9. GET COEFFICIENTS FROM FITTING MOBILITY MODELS #
####################################################

named_pop = popsize
names(named_pop) = seq(1:length(named_pop))
colnames(total_trips_wdiag) = names(named_pop)
rownames(total_trips_wdiag) = names(named_pop)
colnames(dists) = names(named_pop)
rownames(dists) = names(named_pop)
# first make mobility matrices
M_true = total_trips_wdiag
diag(M_true) = NA
true_mob_matrices = list(M = M_true,
                         D = dists,
                         N = named_pop)
mod_true <- mobility(data=true_mob_matrices,
                     model='gravity',
                     type='exp',
                     n_chain=2,
                     n_burn=1000,
                     n_samp=1000,
                     n_thin=2,
                     DIC=TRUE)
# adult
colnames(adult_trips_wdiag) = names(named_pop)
rownames(adult_trips_wdiag) = names(named_pop)
named_adult = N_adult
names(named_adult) = seq(1:length(named_adult))
M_adult = adult_trips_wdiag
diag(M_adult) = NA
adult_mob_matrices = list(M = M_adult,
                          D = dists,
                          N = named_pop) ## check - or use population of adults?
mod_adult <- mobility(data=adult_mob_matrices,
                      model='gravity',
                      type='exp',
                      n_chain=2,
                      n_burn=1000,
                      n_samp=1000,
                      n_thin=2,
                      DIC=TRUE)
# censored
colnames(censored_total_trips) = names(named_pop)
rownames(censored_total_trips) = names(named_pop)
M_censored = censored_total_trips
diag(M_censored) = NA
censored_mob_matrices = list(M = M_censored,
                             D = dists,
                             N = named_pop)
mod_censored <- mobility(data=censored_mob_matrices,
                         model='gravity',
                         type='exp',
                         n_chain=2,
                         n_burn=1000,
                         n_samp=1000,
                         n_thin=2,
                         DIC=TRUE)

################################################################################
################################################################################
