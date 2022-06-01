library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(lme4)

set.seed(991)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
source('ProcessData.R')
regdata <- procdata %>%
	filter(CumCases > 20, NewCasePht > 0, Population >= 50000)


BeforeOctData <- regdata %>% filter(date < as_date('2020-10-01'))

print('Fitting Spring AH + All Mobile')
mixedmod_beforeOct_fips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 
print('Fitting Spring AH')
mixedmod_beforeOct_humidfips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData)	
print('Fitting Spring RetailRec')
mixedmod_beforeOct_RetailRec <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 
print('Fitting Spring AH + GroceryPharmacy')
mixedmod_beforeOct_GroceryPharmacy <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 
print('Fitting Spring AH + Parks')
mixedmod_beforeOct_Parks <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData)
print('Fitting Spring AH + Transit')
mixedmod_beforeOct_Transit <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 
print('Fitting Spring AH + Workplaces')
mixedmod_beforeOct_Workplaces <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 
print('Fitting Spring AH + Residential')
mixedmod_beforeOct_Residential <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=BeforeOctData) 

AfterOctData <- regdata %>% filter(date >= as_date('2020-10-01'))

print('Fitting Fall AH + All Mobile')
mixedmod_afterOct_fips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH')
mixedmod_afterOct_humidfips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData)
print('Fitting Fall AH + RetailRec')
mixedmod_afterOct_RetailRec <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH + GroceryPharmacy')
mixedmod_afterOct_GroceryPharmacy <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH + Parks')
mixedmod_afterOct_Parks <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH + Transit')
mixedmod_afterOct_Transit <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH + Workplaces')
mixedmod_afterOct_Workplaces <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 
print('Fitting Fall AH + Residential')
mixedmod_afterOct_Residential <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AfterOctData) 

AllYearData <- regdata

print('Fitting AllYear AH + All Mobile')
mixedmod_allyear_fips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH')
mixedmod_allyear_humidfips <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData)
print('Fitting AllYear AH + RetailRec')
mixedmod_allyear_RetailRec <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH + GroceryPharmacy')
mixedmod_allyear_GroceryPharmacy <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH + Parks')
mixedmod_allyear_Parks <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH + Transit')
mixedmod_allyear_Transit <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH + Workplaces')
mixedmod_allyear_Workplaces <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 
print('Fitting AllYear AH + Residential')
mixedmod_allyear_Residential <- glmer(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + (1 | FIPS), family="poisson", offset = log(Population/100000), data=AllYearData) 

