################################################################################
# File Name: 04_create_figures                                                 #
#                                                                              #
# Purpose:   Create figures from mobility matrix/metapopulation simulations.   #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create simulation space figure                                 #
#            3. Create true mobility matrix epi curves                         #
#            4. Create matrix and arrival time figure                          #
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
library(cowplot)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggpubr)

# Set the seed
set.seed(123)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/mobility-commentary-project')

#####################################
# 2. CREATE SIMULATION SPACE FIGURE #
#####################################

# Load relevent data for all figures
load('./tmp/metapop_dat_probs.RData')
load('./tmp/arrival_times.RData')
load('./tmp/true_epi_curves.RData')

# Add population data to patch_df
patch_df$`Population Size` <- popsize

scatter <- ggplot(patch_df) +
  geom_point(aes(x = x, y = y, color = as.factor(cluster), 
                 size = `Population Size`), alpha = 0.80) + 
  theme_minimal() + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 26),
        panel.grid.minor = element_blank()) + 
  ylab('Y') + xlab('X') +
  scale_color_nejm()

bar <- ggplot(patch_df, aes(x = `Population Size`)) +
  geom_histogram(bins = 10, aes(), 
               alpha = 0.75, fill = 'grey', color = 'grey',
               position = "identity") + 
  theme_minimal() + xlab('Population') + ylab('Count') +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(limits = c(0, 20)) + 
  ggtitle(' ') +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Save figure
figure <- plot_grid(scatter,
                    bar,
                    nrow = 1)
ggsave('./figs/figure_1.jpg', plot = figure, height = 6, width = 12)

#############################################
# 3. CREATE TRUE MOBILITY MATRIX EPI CURVES #
#############################################

patch <- ggplot(data = avg_true_patch) +
  geom_line(aes(x = time, y = avg_incid_I, group = patch_num, 
                color = as.factor(patch_num)), alpha = 0.75, linewidth = 1) +
  theme_minimal() + xlab('Time (days)') + ylab('Average Incident Cases') +
  ggtitle('Incident Cases by Patch') +
  scale_x_continuous(limits = c(0, 250)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        legend.position = 'none')

cluster <- ggplot(data = avg_true_cluster) +
  geom_line(aes(x = time, y = avg_incid_I, group = cluster_num, 
                color = as.factor(cluster_num)), alpha = 0.75, linewidth = 1) +
  theme_minimal() + xlab('Time (days)') + ylab('Average Incident Cases') +
  ggtitle('Incident Cases by Cluster') +
  scale_x_continuous(limits = c(0, 250)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        legend.position = 'none')

# Save figure
figure <- plot_grid(patch,
                    cluster,
                    nrow = 1)
ggsave('./figs/figure_2.jpg', plot = figure, height = 6, width = 14)


############################################
# 4. CREATE MATRIX AND ARRIVAL TIME FIGURE #
############################################

# Set mobility matrix theme
matrix_theme <- theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title = element_text(size=26),
                      legend.text = element_text(size = 20),
                      legend.title = element_text(size = 20),
                      legend.position = "bottom",
                      legend.box="vertical",
                      legend.margin=margin(),
                      strip.background = element_blank(),
                      legend.spacing.y = unit(0.25, 'cm'),
                      legend.key.size = unit(1, 'cm'),
                      strip.text = element_text(size = 16),
                      plot.title = element_text(size=30, hjust = 0.5))

# Make a mobility matrix plot function
make_matrix_plot <- function(data, color, na) {
  if (na == TRUE) {
    plot <- ggplot(data=data) +
      geom_tile(aes(x=factor(Var2),
                    y=factor(Var1),
                    fill=value_cat)) +
      xlab('Destination') + ylab("Origin") +
      theme_minimal() + matrix_theme + 
      scale_fill_manual('Trip Proportion', values = c(brewer.pal(n = 5, name = color), '#FFFFFF'),
                        breaks = c("1", "2", 
                                   "3", "4", "5", "NA"),
                        labels = c("< 0.0001", "0.0001-0.001", 
                                   "0.001-0.01", "0.01-0.1", "0.1-1.0", "NA"))
    return(plot)
  }
  if (na == FALSE) {
    plot <- ggplot(data=data) +
      geom_tile(aes(x=factor(Var2),
                    y=factor(Var1),
                    fill=value_cat)) +
      xlab('Destination') + ylab("Origin") +
      theme_minimal() + matrix_theme + 
      scale_fill_manual('Trip Proportion', values = c(brewer.pal(n = 5, name = color), '#FFFFFF'),
                        breaks = c("< 0.0001", "0.0001-0.001", 
                                   "0.001-0.01", "0.01-0.1", "0.1-1.0", "NA"),
                        labels = c("< 0.0001", "0.0001-0.001", 
                                   "0.001-0.01", "0.01-0.1", "0.1-1.0", "NA"))
    return(plot)
  }
}

# Create mobility matrix plots
prob_trips_total_reshape <- reshape2::melt(prob_trips_total)
prob_trips_total_reshape$value_cat <- cut(prob_trips_total_reshape$value,
                                                breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.0001", "0.0001-0.001", 
                                                           "0.001-0.01", "0.01-0.1", "0.1-1.0"))
true_matrix <- make_matrix_plot(data = prob_trips_total_reshape, color = 'Blues', na = FALSE)

prob_trips_adults_reshape <- reshape2::melt(prob_trips_adults)
prob_trips_adults_reshape$value_cat <- cut(prob_trips_adults_reshape$value,
                                          breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.1, Inf),
                                          labels = c("< 0.0001", "0.0001-0.001", 
                                                     "0.001-0.01", "0.01-0.1", "0.1-1.0"))
adult_matrix <- make_matrix_plot(data = prob_trips_adults_reshape, color = 'Blues', na = FALSE)

prob_trips_censor_reshape <- reshape2::melt(prob_trips_censor)
prob_trips_censor_reshape$value_cat <- cut(prob_trips_censor_reshape$value,
                                           breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.1, Inf),
                                           labels = c("< 0.0001", "0.0001-0.001", 
                                                      "0.001-0.01", "0.01-0.1", "0.1-1.0"))
prob_trips_censor_reshape$value_cat <- ifelse(is.na(prob_trips_censor_reshape$value_cat), "NA", prob_trips_censor_reshape$value_cat)
censor_matrix <- make_matrix_plot(data = prob_trips_censor_reshape, color = 'Blues', na = TRUE)

prob_trips_weekly_reshape <- reshape2::melt(prob_trips_weekly)
prob_trips_weekly_reshape$value_cat <- cut(prob_trips_weekly_reshape$value,
                                           breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.1, Inf),
                                           labels = c("< 0.0001", "0.0001-0.001", 
                                                      "0.001-0.01", "0.01-0.1", "0.1-1.0"))
weekly_matrix <- make_matrix_plot(data = prob_trips_weekly_reshape, color = 'Blues', na = FALSE)

prob_trips_region_reshape <- reshape2::melt(prob_trips_region)
prob_trips_region_reshape$value_cat <- cut(prob_trips_region_reshape$value,
                                           breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.1, Inf),
                                           labels = c("< 0.0001", "0.0001-0.001", 
                                                      "0.001-0.01", "0.01-0.1", "0.1-1.0"))
region_matrix <- make_matrix_plot(data = prob_trips_region_reshape, color = 'Blues', na = FALSE)

prob_trips_total_reshape$value_cat <- ifelse(is.na(prob_trips_total_reshape$value_cat), "NA", prob_trips_total_reshape$value_cat)
prob_trips_total_reshape[1, 4] <- "NA"
legend <- get_legend(make_matrix_plot(data = prob_trips_total_reshape, color = 'Blues', na = TRUE) + guides(fill = guide_legend(nrow = 1)))

# Make arrival time comparison plots
# True Matrix
true_plot <- ggplot(data = arrival_times_true, 
       aes(y = reorder(patch_num, time_mean), x = time_mean)) + 
  geom_point(size = 3, alpha = 0.6, color = '#4292C6') + theme_minimal() +
  geom_errorbarh(aes(xmin = time_fast, xmax = time_slow,height = .5),
                 color = '#4292C6',
                 alpha = 0.5,
                 position=position_dodge(.5)) +
  ylab('Patch (ordered)') +
  xlab('Arrival Time (days)') +
  scale_x_continuous(limits = c(0, 90)) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Adult Matrix
arrival_times_true$Category <- "True"
arrival_times_true$rank <- rank(arrival_times_true$time_mean)
arrival_times_adult$Category <- "Adult"
arrival_times_adult_join <- left_join(arrival_times_adult, arrival_times_true[, c('patch_num', 'rank', 'time_mean')],
                                 by = c('patch_num' = 'patch_num'))
arrival_times_adult_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_adult_join$time_mean.y - 
                                          arrival_times_adult_join$time_mean.x > 0, 
                                        'Faster', 'Slower')
arrival_times_adult_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_adult_join$time_mean.y - 
                                          arrival_times_adult_join$time_mean.x == 0, 
                                        'Identical', arrival_times_adult_join$`Biased Mobility Matrix is`)
arrival_times_adult_append <- rbind(arrival_times_adult, arrival_times_true)
adult_plot <- ggplot(data = arrival_times_adult_join,
       aes(y = time_mean.y, x = time_mean.x)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = `Biased Mobility Matrix is`)) + theme_minimal() +
  geom_abline(slope=1, intercept = 0, linetype = 2, linewidth = 1, color = 'black', alpha = 0.6) +
  ylab('True Arrival Time (days)') +
  xlab('Estimate Arrival Time (days)') +
  scale_x_continuous(limits = c(20, 85)) +
  scale_y_continuous(limits = c(20, 85)) +
  scale_color_manual(values = c('#FCD12A', '#FF5C5C')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Censored Matrix
arrival_times_censor$Category <- "Censor"
arrival_times_censor_join <- left_join(arrival_times_censor, arrival_times_true[, c('patch_num', 'rank', 'time_mean')],
                                 by = c('patch_num' = 'patch_num'))
arrival_times_censor_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_censor_join$time_mean.y - 
                                                            arrival_times_censor_join$time_mean.x > 0, 
                                                          'Faster', 'Slower')
arrival_times_censor_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_censor_join$time_mean.y - 
                                                            arrival_times_censor_join$time_mean.x == 0, 
                                                          'Identical', arrival_times_censor_join$`Biased Mobility Matrix is`)
arrival_times_censor_append <- rbind(arrival_times_censor, arrival_times_true)
censor_plot <- ggplot(data = arrival_times_censor_join,
                     aes(y = time_mean.y, x = time_mean.x)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = `Biased Mobility Matrix is`)) + theme_minimal() +
  geom_abline(slope=1, intercept = 0, linetype = 2, linewidth = 1, color = 'black', alpha = 0.6) +
  ylab('True Arrival Time (days)') +
  xlab('Estimate Arrival Time (days)') +
  scale_x_continuous(limits = c(20, 85)) +
  scale_y_continuous(limits = c(20, 85)) +
  scale_color_manual(values = c('#FCD12A','#4292C6', '#FF5C5C')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Weekly Matrix
arrival_times_weekly$Category <- "Weekly"
arrival_times_weekly_join <- left_join(arrival_times_weekly, arrival_times_true[, c('patch_num', 'rank', 'time_mean')],
                                  by = c('patch_num' = 'patch_num'))
arrival_times_weekly_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_weekly_join$time_mean.y - 
                                                             arrival_times_weekly_join$time_mean.x > 0, 
                                                           'Faster', 'Slower')
arrival_times_weekly_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_weekly_join$time_mean.y - 
                                                             arrival_times_weekly_join$time_mean.x == 0, 
                                                           'Identical', arrival_times_weekly_join$`Biased Mobility Matrix is`)
arrival_times_weekly_append <- rbind(arrival_times_weekly, arrival_times_true)
weekly_plot <- ggplot(data = arrival_times_weekly_join,
                      aes(y = time_mean.y, x = time_mean.x)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = `Biased Mobility Matrix is`)) + theme_minimal() +
  geom_abline(slope=1, intercept = 0, linetype = 2, linewidth = 1, color = 'black', alpha = 0.6) +
  ylab('True Arrival Time (days)') +
  xlab('Estimate Arrival Time (days)') +
  scale_x_continuous(limits = c(20, 85)) +
  scale_y_continuous(limits = c(20, 85)) +
  scale_color_manual(values = c('#FCD12A', '#4292C6', '#FF5C5C')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Regional Matrix
arrival_times_true_cluster$Category <- "True"
arrival_times_region$Category <- "Region"
arrival_times_region_join <- left_join(arrival_times_region, arrival_times_true_cluster[, c('cluster_num', 'time_mean')],
                                  by = c('cluster_num' = 'cluster_num'))
arrival_times_region_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_region_join$time_mean.y - 
                                                             arrival_times_region_join$time_mean.x > 0, 
                                                           'Faster', 'Slower')
arrival_times_region_join$`Biased Mobility Matrix is` <- ifelse(arrival_times_region_join$time_mean.y - 
                                                             arrival_times_region_join$time_mean.x == 0, 
                                                           'Identical', arrival_times_region_join$`Biased Mobility Matrix is`)
arrival_times_region_append <- rbind(arrival_times_region, arrival_times_true_cluster)
region_plot <- ggplot(data = arrival_times_region_join,
                      aes(y = time_mean.y, x = time_mean.x)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = `Biased Mobility Matrix is`)) + theme_minimal() +
  geom_abline(slope=1, intercept = 0, linetype = 2, linewidth = 1, color = 'black', alpha = 0.6) +
  ylab('True Arrival Time (days)') +
  xlab('Estimate Arrival Time (days)') +
  scale_x_continuous(limits = c(20, 85)) +
  scale_y_continuous(limits = c(20, 85)) +
  scale_color_manual(values = c('#FCD12A', '#4292C6', '#FF5C5C')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Extract Legend
legend_plot <- get_legend(censor_plot)

# Combine all figures into one
figure_matrix <- plot_grid(true_matrix + ggtitle('True Mobility Matrix') + theme(legend.position = 'none'),
                    adult_matrix + ggtitle('Adult-Only Mobility Matrix') + theme(legend.position = 'none'),
                    censor_matrix + ggtitle('Censored Mobility Matrix') + theme(legend.position = 'none'),
                    weekly_matrix + ggtitle('Weekly Mobility Matrix') + theme(legend.position = 'none'),
                    region_matrix + ggtitle('Regional Mobility Matrix') + theme(legend.position = 'none'),
                    nrow = 1)

figure_plot <- plot_grid(true_plot + theme(legend.position = 'none'),
                         adult_plot + theme(legend.position = 'none'),
                         censor_plot + theme(legend.position = 'none'),
                         weekly_plot + theme(legend.position = 'none'),
                         region_plot + theme(legend.position = 'none'),
                         nrow = 1)

figure_combined <- plot_grid(figure_matrix, legend, figure_plot, legend_plot,
                             rel_heights = c(1, 0.15, 1, 0.15), nrow = 4)



ggsave('./figs/figure_3.jpg', plot = figure_combined, height = 15, width = 30)


plot_1 <- ggplot(arrival_times_adult_append, aes(x=time_mean, fill=Category)) +
  geom_density(alpha=.25) + theme_minimal() + 
  ylab('Density') +
  xlab('Arrival Time (days)') +
  scale_fill_manual(values = c('#FF5C5C', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

plot_2 <- ggplot(arrival_times_censor_append, aes(x=time_mean, fill=Category)) +
  geom_density(alpha=.25) + theme_minimal() + 
  ylab('Density') +
  xlab('Arrival Time (days)') +
  scale_fill_manual(values = c('#FF5C5C', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

plot_3 <- ggplot(arrival_times_weekly_append, aes(x=time_mean, fill=Category)) +
  geom_density(alpha=.25) + theme_minimal() + 
  ylab('Density') +
  xlab('Arrival Time (days)') +
  scale_fill_manual(values = c('#4292C6', '#FF5C5C')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

plot_4 <- ggplot(arrival_times_region_append, aes(x=time_mean, fill=Category)) +
  geom_density(alpha=.25) + theme_minimal() + 
  ylab('Density') +
  xlab('Arrival Time (days)') +
  scale_fill_manual(values = c('#FF5C5C', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

figure <- plot_grid(plot_1 + ggtitle('True vs. Adult Mobility') ,
                           plot_2 + ggtitle('True vs. Censored Mobility') ,
                           plot_3 + ggtitle('True vs. Weekly Mobility'),
                           plot_4 + ggtitle('True vs. Regional Mobility') ,
                           nrow = 1)

ggsave('./figs/figure_4.jpg', plot = figure, height = 8, width = 30)

################################################################################
################################################################################
