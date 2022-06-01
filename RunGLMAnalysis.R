library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(performance)

# cl <- makeCluster(detectCores(), outfile="")
# registerDoParallel(cl)
set.seed(991)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
source('ProcessData.R')
regdata <- procdata %>%
	# filter(CumCases > 20, NewCase >= 0, NewCase_ma >= 0, Population >= 50000)
	filter(CumCases > 20, NewCasePht > 0, Population >= 50000)

# Run Unit Root Test for stationarity
source("UnitRootTest.R")

# Run GAMS
RunGLMs <- function(i) {
	x <- AllClusters[i]
	print(paste('******* Running Regression for', x))

	BeforeOctData <- regdata %>% filter(ClusterRank == x, date < as_date('2020-10-01'))

	print('Fitting Spring AH + All Mobile + FIPS')
	glm_beforeOct_fips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + Humidity_ma_lag + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + FIPS')
	glm_beforeOct_humidfips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData)	
	print('Fitting Spring RetailRec + FIPS')
	glm_beforeOct_RetailRec <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + GroceryPharmacy + FIPS')
	glm_beforeOct_GroceryPharmacy <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + Parks + FIPS')
	glm_beforeOct_Parks <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + Transit + FIPS')
	glm_beforeOct_Transit <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + Workplaces + FIPS')
	glm_beforeOct_Workplaces <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 
	print('Fitting Spring AH + Residential + FIPS')
	glm_beforeOct_Residential <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=BeforeOctData) 

	AfterOctData <- regdata %>% filter(ClusterRank == x, date >= as_date('2020-10-01'))

	print('Fitting Fall AH + All Mobile + FIPS')
	glm_afterOct_fips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + FIPS')
	glm_afterOct_humidfips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData)
	print('Fitting Fall AH + RetailRec + FIPS')
	glm_afterOct_RetailRec <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + GroceryPharmacy + FIPS')
	glm_afterOct_GroceryPharmacy <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + Parks + FIPS')
	glm_afterOct_Parks <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + Transit + FIPS')
	glm_afterOct_Transit <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + Workplaces + FIPS')
	glm_afterOct_Workplaces <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 
	print('Fitting Fall AH + Residential + FIPS')
	glm_afterOct_Residential <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AfterOctData) 

	AllYearData <- regdata %>% filter(ClusterRank == x)

	print('Fitting AllYear AH + All Mobile + FIPS')
	glm_allyear_fips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + GroceryPharmacy_ma_lag_scaled + Parks_ma_lag_scaled + Transit_ma_lag_scaled + Workplaces_ma_lag_scaled + Residential_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + FIPS')
	glm_allyear_humidfips <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData)
	print('Fitting AllYear AH + RetailRec + FIPS')
	glm_allyear_RetailRec <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + RetailRec_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + GroceryPharmacy + FIPS')
	glm_allyear_GroceryPharmacy <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + GroceryPharmacy_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + Parks + FIPS')
	glm_allyear_Parks <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Parks_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + Transit + FIPS')
	glm_allyear_Transit <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Transit_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + Workplaces + FIPS')
	glm_allyear_Workplaces <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Workplaces_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 
	print('Fitting AllYear AH + Residential + FIPS')
	glm_allyear_Residential <- glm(NewCase ~ NewCasePht_lag + CumCasesPC + Humidity_ma_lag + Residential_ma_lag_scaled + FIPS, family="poisson", offset = log(Population/100000), data=AllYearData) 

	return(list(
		glm_beforeOct_fips = glm_beforeOct_fips,
		glm_beforeOct_humidfips = glm_beforeOct_humidfips,
		glm_beforeOct_RetailRec = glm_beforeOct_RetailRec,
		glm_beforeOct_GroceryPharmacy = glm_beforeOct_GroceryPharmacy,
		glm_beforeOct_Parks = glm_beforeOct_Parks,
		glm_beforeOct_Transit = glm_beforeOct_Transit,
		glm_beforeOct_Workplaces = glm_beforeOct_Workplaces,
		glm_beforeOct_Residential = glm_beforeOct_Residential,

		glm_afterOct_fips = glm_afterOct_fips,
		glm_afterOct_humidfips = glm_afterOct_humidfips,
		glm_afterOct_RetailRec = glm_afterOct_RetailRec,
		glm_afterOct_GroceryPharmacy = glm_afterOct_GroceryPharmacy,
		glm_afterOct_Parks = glm_afterOct_Parks,
		glm_afterOct_Transit = glm_afterOct_Transit,
		glm_afterOct_Workplaces = glm_afterOct_Workplaces,
		glm_afterOct_Residential = glm_afterOct_Residential,

		glm_allyear_fips = glm_allyear_fips,
		glm_allyear_humidfips = glm_allyear_humidfips,
		glm_allyear_RetailRec = glm_allyear_RetailRec,
		glm_allyear_GroceryPharmacy = glm_allyear_GroceryPharmacy,
		glm_allyear_Parks = glm_allyear_Parks,
		glm_allyear_Transit = glm_allyear_Transit,
		glm_allyear_Workplaces = glm_allyear_Workplaces,
		glm_allyear_Residential = glm_allyear_Residential,

		FIPS = unique(c(as.character(BeforeOctData$FIPS), as.character(AfterOctData$FIPS), as.character(AllYearData$FIPS)))
	))
}

RegList <- lapply(1:length(AllClusters), function(i) {RunGLMs(i)})
names(RegList) <- AllClusters

# model_performance(RegList[['Low 1']]$glm_allyear_fips)
# model_performance(RegList[['Low 2']]$glm_allyear_fips)
# model_performance(RegList[['Mid 1']]$glm_allyear_fips)
# model_performance(RegList[['Mid 2']]$glm_allyear_fips)
# model_performance(RegList[['High 1']]$glm_allyear_fips)
# model_performance(RegList[['High 2']]$glm_allyear_fips)

# summary(RegList[['Low 1']]$glm_allyear_fips)
# summary(RegList[['Low 2']]$glm_allyear_fips)
# summary(RegList[['Mid 1']]$glm_allyear_fips)
# summary(RegList[['Mid 2']]$glm_allyear_fips)
# summary(RegList[['High 1']]$glm_allyear_fips)
# summary(RegList[['High 2']]$glm_allyear_fips)

# stopCluster(cl)
# generate GLM output tables
source('GenerateTables.R')

source('CalculateVIF.R')
