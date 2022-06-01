library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(RcppRoll)

set.seed(99)
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
print('******* Loading and Processing Data')

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
		grocery_and_pharmacy_percent_change_from_baseline,
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
	cbind(series = 'CumCases') %>%
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
	arrange(FIPS, date) %>%
	left_join(PopData %>% select(FIPS, Population), by =  c('FIPS')) %>%
	left_join(HumClustMember, by = c('FIPS' = 'fips')) %>%
 	mutate(
		NewCase = CumCases - dplyr::lag(CumCases),
		NewCasePht = NewCase / Population * 100000,
		NewCasePht_lag = dplyr::lag(NewCasePht, LagTime),
		RetailRec_ma = roll_mean(RetailRec, RollingAvgWindow, align="right", fill=NA),
		RetailRec_ma_lag = dplyr::lag(RetailRec_ma, LagTime),
		GroceryPharmacy_ma = roll_mean(GroceryPharmacy, RollingAvgWindow, align="right", fill=NA),
		GroceryPharmacy_ma_lag = dplyr::lag(GroceryPharmacy_ma,LagTime),
		Parks_ma = roll_mean(Parks, RollingAvgWindow, align="right", fill=NA),
		Parks_ma_lag = dplyr::lag(Parks_ma, LagTime),
		Transit_ma = roll_mean(Transit, RollingAvgWindow, align="right", fill=NA),
		Transit_ma_lag = dplyr::lag(Transit_ma, LagTime),
		Workplaces_ma = roll_mean(Workplaces, RollingAvgWindow, align="right", fill=NA),
		Workplaces_ma_lag = dplyr::lag(Workplaces_ma, LagTime),
		Residential_ma = roll_mean(Residential, RollingAvgWindow, align="right", fill=NA),
		Residential_ma_lag = dplyr::lag(Residential_ma, LagTime),
		Humidity_ma = roll_mean(Humidity, RollingAvgWindow, align="right", fill=NA),
		Humidity_ma_lag = dplyr::lag(Humidity_ma, LagTime),
		CumCasesPC = CumCases / Population
	) %>%
	ungroup() %>%
	mutate(
		Humidity_ma_lag_scaled = as.numeric(scale(Humidity_ma_lag)),
		RetailRec_ma_lag_scaled = as.numeric(scale(RetailRec_ma_lag)),
		GroceryPharmacy_ma_lag_scaled = as.numeric(scale(GroceryPharmacy_ma_lag)),
		Parks_ma_lag_scaled = as.numeric(scale(Parks_ma_lag)),
		Transit_ma_lag_scaled = as.numeric(scale(Transit_ma_lag)),
		Workplaces_ma_lag_scaled = as.numeric(scale(Workplaces_ma_lag)),
		Residential_ma_lag_scaled = as.numeric(scale(Residential_ma_lag))
	) %>%
	filter(
		!is.na(Humidity_ma_lag),
		!is.na(RetailRec_ma_lag),
		!is.na(GroceryPharmacy_ma_lag),
		!is.na(Parks_ma_lag),
		!is.na(Transit_ma_lag),
		!is.na(Workplaces_ma_lag),
		!is.na(Residential_ma_lag)
	)

procdata$FIPS <- as.factor(procdata$FIPS)
AllClusters <- sort(unique(HumClustMember$ClusterRank))
Alldates <- sort(unique(procdata$date))