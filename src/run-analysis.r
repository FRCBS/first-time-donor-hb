setwd('c:/hy-version/first-time-donor-hb')

rm(list=ls())
source('src/analysis-functions.r')
source('src/read-survival.r')
source('src/read-hb-data.r')

# source('src/analysis-functions.r') # this is sourced in various other files
source('src/analysis.r') # survival part

# Things to do
# Check the distributions
# Maybe comment if they are normal or not; even compute skewness and kurtosis
# Then the overlap in area between the emperical and estimated distributions
# Visualizations of how the distributions (month, hour, age) have changed: heatmaps with years as rows, levels in the other directions

