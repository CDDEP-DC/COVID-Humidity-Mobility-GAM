library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(RcppRoll)

set.seed(99)
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory

# Load data from data directory
AbsHumData <- fread('data/AbsoluteHumidity.csv')
GoogleMob2020 <- fread('data/GoogleMobility2020.csv')
GoogleMob2021 <- fread('data/GoogleMobility2021.csv')
ConfData <- fread('data/time_series_covid19_confirmed_US.csv')
PopData <- fread('data/CountyPopulation.csv')
HumClustMember <- fread('FIPSHumidityCluster.csv')

# Create long data w.r.t time 
mAbsHumData <- AbsHumData %>% 
	select(-c(STATION)) %>% 
	melt(id.vars='FIPS') %>% 
	mutate(value = value) %>% 
	cbind(series = 'Humidity') %>%
	rename(c('date'='variable'))

mGoogMob <- GoogleMob2020 %>% rbind(GoogleMob2021) %>% 
	select(census_fips_code, date,
		retail_and_recreation_percent_change_from_baseline,
		grocery_and_pharmacy_percent_change_from_baseline ,
		parks_percent_change_from_baseline,
		transit_stations_percent_change_from_baseline,
		workplaces_percent_change_from_baseline,
		residential_percent_change_from_baseline) %>%
	rename(c(
		'FIPS'='census_fips_code',
		'RetailRec' = 'retail_and_recreation_percent_change_from_baseline',
		'GroceryPharmacy' = 'grocery_and_pharmacy_percent_change_from_baseline',
		'Parks' = 'parks_percent_change_from_baseline',
		'Transit' = 'transit_stations_percent_change_from_baseline',
		'Workplaces' = 'workplaces_percent_change_from_baseline',
		'Residential' = 'residential_percent_change_from_baseline')) %>%
	melt(id.vars = c('FIPS','date')) %>%
	rename('series'='variable')

mConfData <- ConfData %>% 
	select(-c(UID, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) %>% 
	melt(id.vars='FIPS') %>% 
	cbind(series = 'ConfirmedCases') %>%
	rename(c('date'='variable'))

mdf <- rbind(mAbsHumData, mGoogMob, mConfData) %>% 
	na.omit() %>%  
	mutate(date = mdy(as.character(date))) %>% 
	unique()

# Create regression data
LagTime <- 14
RollingAvgWindow <- 10

procdata <- spread(mdf, series, value) %>% 
	group_by(FIPS) %>% 
	arrange(FIPS,date) %>%
	left_join(PopData %>% select(FIPS, Population), by =  c('FIPS')) %>%
	left_join(HumClustMember,by = c('FIPS' = 'fips')) %>% 
 	mutate(NewCase = ConfirmedCases - lag(ConfirmedCases)) %>% 
 	mutate(NewCase_ma = roll_mean(NewCase, RollingAvgWindow, align="right", fill=NA)) %>% 
 	mutate(NewCase_ma_lag = lag(NewCase_ma, LagTime)) %>% 
	mutate(CaseGrowthRate = (NewCase_ma - NewCase_ma_lag)/(NewCase_ma_lag + 1)) %>% 
	mutate(CaseGrowthRate_ma = roll_mean(CaseGrowthRate, RollingAvgWindow, align="right", fill=NA)) %>% 
 	mutate(RetailRec_ma = roll_mean(RetailRec, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(RetailRec_ma_lag = lag(RetailRec_ma,LagTime)) %>% 
	mutate(RetailRec_ma_lag_scaled = RetailRec_ma_lag / 100) %>%  	
	mutate(GroceryPharmacy_ma = roll_mean(GroceryPharmacy, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(GroceryPharmacy_ma_lag = lag(GroceryPharmacy_ma,LagTime)) %>% 
	mutate(GroceryPharmacy_ma_lag_scaled = GroceryPharmacy_ma_lag / 100) %>% 	
	mutate(Parks_ma = roll_mean(Parks, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(Parks_ma_lag = lag(Parks_ma,LagTime)) %>% 
	mutate(Parks_ma_lag_scaled = Parks_ma_lag / 100) %>% 
	mutate(Transit_ma = roll_mean(Transit, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(Transit_ma_lag = lag(Transit_ma,LagTime)) %>% 
	mutate(Transit_ma_lag_scaled = Transit_ma_lag / 100) %>% 	
	mutate(Workplaces_ma = roll_mean(Workplaces, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(Workplaces_ma_lag = lag(Workplaces_ma,LagTime)) %>% 
	mutate(Workplaces_ma_lag_scaled = Workplaces_ma_lag / 100) %>% 	
 	mutate(Residential_ma = roll_mean(Residential, RollingAvgWindow, align="right", fill=NA)) %>%
 	mutate(Residential_ma_lag = lag(Residential_ma,LagTime)) %>% 
	mutate(Residential_ma_lag_scaled = Residential_ma_lag / 100) %>% 	
 	mutate(Humidity_ma = roll_mean(Humidity, RollingAvgWindow, align="right", fill=NA)) %>%
  	mutate(Humidity_ma_lag = lag(Humidity_ma, LagTime)) %>% 
  	mutate(HumidityChange2wk = Humidity_ma - lag(Humidity_ma,LagTime)) %>%
	mutate(HumidityChange2wk_ma = roll_mean(HumidityChange2wk, RollingAvgWindow, align="right", fill=NA)) %>% 
	mutate(ConfirmedCasesPC = ConfirmedCases / Population) %>% 
	mutate(NewCasePht = NewCase / Population * 100000) %>% 
	mutate(NewCasePht_ma = NewCase_ma / Population * 100000, align="right", fill=NA) %>% 
	ungroup() %>%
	filter(
		!is.na(Humidity_ma_lag),
		!is.na(RetailRec_ma_lag),
		!is.na(GroceryPharmacy_ma_lag),
		!is.na(Parks_ma_lag),
		!is.na(Transit_ma_lag),
		!is.na(Workplaces_ma_lag),
		!is.na(Residential_ma_lag),
		!is.na(NewCase_ma),
		NewCasePht >= 0)

procdata$FIPS <- as.factor(procdata$FIPS)
AllClusters <- sort(unique(HumClustMember$ClusterRank))
Alldates <- sort(unique(procdata$date))