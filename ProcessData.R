library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(RcppRoll)
library(viridis)

set.seed(99)

# Load data from data directory
AbsHumData <- fread('data/AbsoluteHumidity.csv')
UnacastData <- fread('data/NonEssentialVisits.csv') 
ConfData <- fread('data/time_series_covid19_confirmed_US.csv')
PopData <- fread('data/CountyPopulation.csv')
HumClustMember <- fread('FIPSHumidityCluster.csv')

# Select and filter out counties with less 50,000 people
selectFIPS <- AbsHumData$FIPS %>% 
	intersect(na.omit(UnacastData)$FIPS) %>% 
	intersect(ConfData$FIPS) %>% 
	intersect((PopData %>% select('FIPS','Population') %>% filter(Population >= 50000))$FIPS) %>%
	unique()

SelectCounties <- HumClustMember %>% filter(fips %in% selectFIPS)

# Create long data
mAbsHumData <- AbsHumData %>% select(-c(STATION)) %>% 
	filter(FIPS %in% selectFIPS) %>% melt(id.vars='FIPS') %>% cbind(Series = 'Humidity')
mUnacastData <- UnacastData %>% 
	filter(FIPS %in% selectFIPS) %>% melt(id.vars='FIPS') %>% cbind(Series = 'Visits')
mConfData <- ConfData %>% 
	filter(FIPS %in% selectFIPS) %>% 
	select(-c(UID, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) %>% 
	melt(id.vars='FIPS') %>% filter(value > 20) %>% cbind(Series = 'ConfirmedCases')
mdf <- rbind(mAbsHumData, mUnacastData, mConfData) %>% rename(Date = variable) %>% na.omit() %>% unique()
mdf$Date <- mdy(as.character(mdf$Date)) 

# Create wide data
widedata <- spread(mdf, Series, value) %>% group_by(FIPS) %>%
	left_join(PopData %>% select(FIPS, Population), by =  c('FIPS')) %>%
	left_join(HumClustMember,by = c('FIPS' = 'fips')) %>% 
 	mutate(NewCase = ConfirmedCases - lag(ConfirmedCases)) %>% 
 	mutate(NewCase_ma = roll_mean(NewCase, 7, align="right", fill=NA)) %>% 
 	mutate(Visits_ma = roll_mean(Visits, 7, align="right", fill=NA)) %>%
 	mutate(Visits_ma_lag = lag(Visits_ma, 14)) %>% 
 	mutate(Humidity_ma = roll_mean(Humidity, 7, align="right", fill=NA)) %>%
  	mutate(Humidity_ma_lag = lag(Humidity_ma, 14)) %>% 
	mutate(NewCasePC = NewCase / Population) %>% 
	mutate(NewCasePC_ma = roll_mean(NewCasePC, 7, align="right", fill=NA)) %>% 
	filter(ConfirmedCases > 20, NewCase >= 0, NewCase_ma >= 0)

widedata$FIPS <- as.factor(widedata$FIPS)
AllClusters <- sort(unique(HumClustMember$ClusterRank))
AllDates <- sort(unique(widedata$Date))
