library(sjPlot)
library(sjmisc)
library(sjlabelled)

names(RegList) <- AllClusters

pl <- c(
  `(Intercept)` = "Intercept",
	Humidity_ma_lag = paste0('Absolute Humidity (',LagTime,'-day Lag)'),
	RetailRec_ma_lag_scaled = paste0('Retail and Recreation (',LagTime,'-day Lag)'),
	GroceryPharmacy_ma_lag_scaled = paste0('Grocery Stores and Pharmacies (',LagTime,'-day Lag)'),
	Parks_ma_lag_scaled = paste0('Parks (',LagTime,'-day Lag)'),
	Transit_ma_lag_scaled = paste0('Transit Stations (',LagTime,'-day Lag)'),
	Workplaces_ma_lag_scaled = paste0('Workplaces (',LagTime,'-day Lag)'),
	Residential_ma_lag_scaled = paste0('Residential (',LagTime,'-day Lag)'),
	CumCasesPC = 'Immunity Factor',
	NewCase = 'Daily Cases'
)
terms_inc <-c('(Intercept)',
	# 'CumCasesPC',
	# 'NewCasePht_lag',
	"Humidity_ma_lag",
	"RetailRec_ma_lag_scaled",
	"GroceryPharmacy_ma_lag_scaled",
	"Parks_ma_lag_scaled",
	"Transit_ma_lag_scaled",
	"Workplaces_ma_lag_scaled",
	"Residential_ma_lag_scaled")

mod_label <- c('Low 1','Low 2','Mid 1','Mid 2','High 1','High 2')


# Summer
print(tab_model(
	RegList[['Low 1']]$glm_beforeOct_fips,
	RegList[['Low 2']]$glm_beforeOct_fips,
	RegList[['Mid 1']]$glm_beforeOct_fips,
	RegList[['Mid 2']]$glm_beforeOct_fips,
	RegList[['High 1']]$glm_beforeOct_fips,
	RegList[['High 2']]$glm_beforeOct_fips,
	title = 'Before October',
	transform = NULL,
  	digits = 3,
  	p.style = "stars",
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = terms_inc,
	file = 'output/beforeOct.html'))

# Spring
print(tab_model(
	RegList[['Low 1']]$glm_afterOct_fips,
	RegList[['Low 2']]$glm_afterOct_fips,
	RegList[['Mid 1']]$glm_afterOct_fips,
	RegList[['Mid 2']]$glm_afterOct_fips,
	RegList[['High 1']]$glm_afterOct_fips,
	RegList[['High 2']]$glm_afterOct_fips,
	title = 'After October',
	transform = NULL,
  	digits = 3,
  	p.style = "stars",
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = terms_inc,
	file = 'output/afterOct.html'))

# Entire Year
print(tab_model(
	RegList[['Low 1']]$glm_allyear_fips,
	RegList[['Low 2']]$glm_allyear_fips,
	RegList[['Mid 1']]$glm_allyear_fips,
	RegList[['Mid 2']]$glm_allyear_fips,
	RegList[['High 1']]$glm_allyear_fips,
	RegList[['High 2']]$glm_allyear_fips,
	title = 'Entire Duration',
	transform = NULL,
  	digits = 3,
  	p.style = "stars", 	
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = terms_inc,
	file = 'output/AllYear.html'))


# Robust checks
clappendix2 <- c(
	'All',
	'Absolute Humidity',
	'Retail and Rec',
	'Grocery and Pharmacy',
	'Parks',
	'Transit',
	'Workplaces',
	'Residential'
	)

for(x in mod_label) {	
	print(tab_model(
		RegList[[as.character(x)]]$glm_beforeOct_fips,
		RegList[[as.character(x)]]$glm_beforeOct_humidfips,
		RegList[[as.character(x)]]$glm_beforeOct_RetailRec,
		RegList[[as.character(x)]]$glm_beforeOct_GroceryPharmacy,
		RegList[[as.character(x)]]$glm_beforeOct_Parks,
		RegList[[as.character(x)]]$glm_beforeOct_Transit,
		RegList[[as.character(x)]]$glm_beforeOct_Workplaces,
		RegList[[as.character(x)]]$glm_beforeOct_Residential,
		title = paste(as.character(x),'Before October'),
		transform = NULL,
	  	digits = 3,
	  	p.style = "stars", 	
		auto.label = FALSE,
		dv.labels = clappendix2,
		pred.labels = pl, 
		collapse.ci = TRUE,
		terms = terms_inc,
		file = paste0('output/BeforeOctMobility',as.character(x),'.html')))
}

for(x in mod_label) {	
	print(tab_model(
		RegList[[as.character(x)]]$glm_afterOct_fips,
		RegList[[as.character(x)]]$glm_afterOct_humidfips,
		RegList[[as.character(x)]]$glm_afterOct_RetailRec,
		RegList[[as.character(x)]]$glm_afterOct_GroceryPharmacy,
		RegList[[as.character(x)]]$glm_afterOct_Parks,
		RegList[[as.character(x)]]$glm_afterOct_Transit,
		RegList[[as.character(x)]]$glm_afterOct_Workplaces,
		RegList[[as.character(x)]]$glm_afterOct_Residential,
		title = paste(as.character(x),'After October'),
		transform = NULL,
	  	digits = 3,
	  	p.style = "stars", 	
		auto.label = FALSE,
		dv.labels = clappendix2,
		pred.labels = pl, 
		collapse.ci = TRUE,
		terms = terms_inc,
		file = paste0('output/AfterOctMobility',as.character(x),'.html')))
}

for(x in mod_label) {	
	print(tab_model(
		RegList[[as.character(x)]]$glm_allyear_fips,
		RegList[[as.character(x)]]$glm_allyear_humidfips,
		RegList[[as.character(x)]]$glm_allyear_RetailRec,
		RegList[[as.character(x)]]$glm_allyear_GroceryPharmacy,
		RegList[[as.character(x)]]$glm_allyear_Parks,
		RegList[[as.character(x)]]$glm_allyear_Transit,
		RegList[[as.character(x)]]$glm_allyear_Workplaces,
		RegList[[as.character(x)]]$glm_allyear_Residential,
		title = paste(as.character(x),'All-year'),
		transform = NULL,
	  	digits = 3,
	  	p.style = "stars", 	
		auto.label = FALSE,
		dv.labels = clappendix2,
		pred.labels = pl, 
		collapse.ci = TRUE,
		terms = terms_inc,
		file = paste0('output/AllYearMobility',as.character(x),'.html')))
}
