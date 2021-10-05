library(tidyverse)
library(data.table)
library(ggplot2)
library(RcppRoll)
library(dtwclust)
library(TSclust)
library(TSdist)
library(ggthemes)
library(maps)
library(fishualize)
library(jtools)
library(mgcv)
library(mgcViz)
library(visreg)
library(tidycensus)
library(pscl)
library(socviz)

set.seed(99)

# load parallel
library(parallel)
# create multi-process workers
workers <- makeCluster(detectCores())

# load dtwclust in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
    library(dtwclust)
    RcppParallel::setThreadOptions(1L)
}))

# register your workers
require(doParallel)
registerDoParallel(workers)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
AbsHumData <- fread('data/AbsoluteHumidity.csv')

# Convert humidity data into lists of vectors 
AbsHumList <- apply(AbsHumData,1,function(x) list(as.numeric(x[3:length(x)])))
AbsHumList <- lapply(AbsHumList,function(x) x[[1]])
names(AbsHumList) = AbsHumData$FIPS

# Compute 10-day moving average and remove NAs
AbsHumList_ma <- lapply(AbsHumList, function(x){ 
	y = roll_mean(x, 10, align="right", fill=NA)
	y = y[!is.na(y)]
	return(y)
})

# Remove counties with less than 10 days of humidity measurements
NoHumidFIPS <- which(sapply(AbsHumList_ma,function(x) length(x)) < 10)
AbsHumList_ma <- AbsHumList_ma[-NoHumidFIPS]

# Perform Dynamic Time Warping Clustering
HumCluster <- tsclust(AbsHumList_ma, type = "p", k = 18L,  distance = "dtw", seed = 999)

# Create Data Frame to Store DTW results and write names
FIPSList <- as.integer(names(AbsHumList_ma))
Hum_df <- data.frame(FIPS=FIPSList)
Hum_df$HumCluster <- HumCluster@cluster
# Rank 
Ranks <- c(
	'Low 1','Low 1','Low 1',
	'Low 2','Low 2','Low 2', 
	'Mid 1','Mid 1','Mid 1',
	'Mid 2','Mid 2','Mid 2',
	'High 1','High 1','High 1',
	'High 2','High 2', 'High 2')

AvgHumidity <- sapply(unique(Hum_df$HumCluster), function(x) {
	fips <- Hum_df %>% filter(HumCluster == x) %>% select(FIPS)
	avgHum <- median(unlist(AbsHumList_ma[as.character(fips[[1]])]))
	return(avgHum)
})
FIPSClustRank <- data.frame(HumCluster = unique(Hum_df$HumCluster), AvgHumidity = AvgHumidity)
FIPSClustRank <- cbind(FIPSClustRank[order(FIPSClustRank$AvgHumidity),], ClusterRank = Ranks)

# Create Map Data
data(fips_codes)
counties <- fips_codes %>% 
	mutate(fips = as.integer(paste0(state_code,county_code))) %>% 
	left_join(Hum_df, by=c('fips'='FIPS')) %>% left_join(FIPSClustRank, by='HumCluster')
counties$ClusterRank <- factor(counties$ClusterRank, levels = Ranks)
mapdata <- county_map  %>% 
	mutate(fips = as.integer(id)) %>%
	left_join(counties, by = 'fips')


# Write File mapping FIPS to cluster
write.csv(counties %>% select(state, state_name, county, fips, ClusterRank) %>% na.omit(), 
	file = 'FIPSHumidityCluster.csv', row.names=F)