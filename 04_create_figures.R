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
make_matrix_plot <- function(data, color, breaks, labs) {
  plot <- ggplot(data=reshape2::melt(data)) +
    geom_tile(aes(x=factor(Var2),
                  y=factor(Var1),
                  fill=log(value))) +
    xlab('Destination') + ylab("Origin") +
    theme_minimal() + matrix_theme + 
    scale_fill_distiller(palette = color, na.value = "#FFFFFF", direction = 1,
                         breaks = log(breaks),
                         labels = labs) +
    guides(fill=guide_colorbar(title='Probability of Travel',
                               title.position='top',
                               label.theme=element_text(size=12),
                               barwidth=20,
                               barheight=0.5,
                               frame.colour='black',
                               ticks=TRUE))
  return(plot)
}

# Create mobility matrix plots
true_matrix <- make_matrix_plot(data = prob_trips_total, color = 'Blues', 
                                breaks <- c(0.001, 0.01, 0.1, 0.99),
                                labs <- c(0.001, 0.01, 0.1, 1))
adult_matrix <- make_matrix_plot(data = prob_trips_adults, color = 'Greens',
                                 breaks <- c(0.001, 0.99),
                                 labs <- c(0.001, 1))
censor_matrix <- make_matrix_plot(data = prob_trips_censor, color = 'Oranges',
                                  breaks <- c(0.001, 0.01, 0.1, 0.99),
                                  labs <- c(0.001, 0.01, 0.1, 1))
weekly_matrix <- make_matrix_plot(data = prob_trips_weekly, color = 'Purples',
                                  breaks <- c(0.001, 0.01, 0.1, 0.99),
                                  labs <- c(0.001, 0.01, 0.1, 1))
region_matrix <- make_matrix_plot(data = prob_trips_region, color = 'PuRd',
                                  breaks <- c(0.001, 0.01, 0.1, 0.99),
                                  labs <- c(0.001, 0.01, 0.1, 1))

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
arrival_times_adult$Category <- "Adult"
arrival_times_adult <- rbind(arrival_times_adult, arrival_times_true)
adult_plot <- ggplot(data = arrival_times_adult,
       aes(y = reorder(patch_num, time_mean), x = time_mean)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = Category)) + theme_minimal() +
  ylab('Patch (ordered)') +
  xlab('Arrival Time (days)') +
  scale_x_continuous(limits = c(0, 90)) +
  scale_color_manual(values = c('#74C476', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'true',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Censored Matrix
arrival_times_censor$Category <- "Censor"
arrival_times_censor <- rbind(arrival_times_censor, arrival_times_true)
censor_plot <- ggplot(data = arrival_times_censor,
                     aes(y = reorder(patch_num, time_mean), x = time_mean)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = Category)) + theme_minimal() +
  ylab('Patch (ordered)') +
  xlab('Arrival Time (days)') +
  scale_x_continuous(limits = c(0, 90)) +
  scale_color_manual(values = c('#FD8D3C', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'true',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Weekly Matrix
arrival_times_weekly$Category <- "Weekly"
arrival_times_weekly <- rbind(arrival_times_weekly, arrival_times_true)
weekly_plot <- ggplot(data = arrival_times_weekly,
                      aes(y = reorder(patch_num, time_mean), x = time_mean)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = Category)) + theme_minimal() +
  ylab('Patch (ordered)') +
  xlab('Arrival Time (days)') +
  scale_x_continuous(limits = c(0, 90)) +
  scale_color_manual(values = c('#807DBA', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'true',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Regional Matrix
arrival_times_true_cluster$Category <- "True"
arrival_times_region$Category <- "Region"
arrival_times_region <- rbind(arrival_times_region, arrival_times_true_cluster)
region_plot <- ggplot(data = arrival_times_region,
                      aes(y = reorder(cluster_num, time_mean), x = time_mean)) + 
  geom_point(size = 3, alpha = 0.6, aes(color = Category)) + theme_minimal() +
  ylab('Cluster (ordered)') +
  xlab('Arrival Time (days)') +
  scale_x_continuous(limits = c(0, 90)) +
  scale_color_manual(values = c('#DF65B0', '#4292C6')) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'true',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Combine all figures into one
figure <- plot_grid(true_matrix + ggtitle('True Mobility Matrix'),
                    adult_matrix + ggtitle('Adult-Only Mobility Matrix'),
                    censor_matrix + ggtitle('Censored Mobility Matrix'),
                    weekly_matrix + ggtitle('Weekly Mobility Matrix'),
                    region_matrix + ggtitle('Regional Mobility Matrix'),
                    true_plot,
                    adult_plot,
                    censor_plot,
                    weekly_plot,
                    region_plot,
                    nrow = 2)
ggsave('./figs/figure_3.jpg', plot = figure, height = 14, width = 30)

################################################################################
################################################################################
