library(sjPlot)
library(sjmisc)
library(sjlabelled)

names(RegList) <- AllClusters

HumCoef1 <- lapply(RegList, function(x) summary(x$gam_summer_combo)$p.table[1:3,])
HumCoef2 <- lapply(RegList, function(x) summary(x$gam_spring_combo)$p.table[1:3,])
HumCoef3 <- lapply(RegList, function(x) summary(x$gam_allyear_combo)$p.table[1:3,])
HumCoef4 <- lapply(RegList, function(x) summary(x$gam_fall_combo)$p.table[1:3,])

pl <- c(
  `(Intercept)` = "Intercept",
	Humidity_ma_lag = 'Absolute Humidity (14-day Lag)',
	Visits_ma_lag = 'Non-essential Visitations (14-day Lag)',
	`s(as.numeric(Date))` = 'spline(time)',
	NewCase = 'Daily Cases'
)

mod_label <- c('Low 1','Low 2','Low 3','Mid 1','Mid 2','Mid 3','Mid 4','High 1','High 2','High 3')

# Summer
tab_model(
	RegList[['Low 1']]$gam_summer_combo,
	RegList[['Low 2']]$gam_summer_combo,
	RegList[['Low 3']]$gam_summer_combo,
	RegList[['Mid 1']]$gam_summer_combo,
	RegList[['Mid 2']]$gam_summer_combo,
	RegList[['Mid 3']]$gam_summer_combo,
	RegList[['Mid 4']]$gam_summer_combo,
	RegList[['High 1']]$gam_summer_combo,
	RegList[['High 2']]$gam_summer_combo,	
	RegList[['High 3']]$gam_summer_combo,
	title = 'July to August',
	transform = NULL,
  	digits = 3,
  	p.style = "stars", 	
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = c('(Intercept)',"Humidity_ma_lag", "Visits_ma_lag",'s(as.numeric(Date))'),
	file = 'output/JulyAug.html')

# Spring
tab_model(
	RegList[['Low 1']]$gam_spring_combo,
	RegList[['Low 2']]$gam_spring_combo,
	RegList[['Low 3']]$gam_spring_combo,
	RegList[['Mid 1']]$gam_spring_combo,
	RegList[['Mid 2']]$gam_spring_combo,
	RegList[['Mid 3']]$gam_spring_combo,
	RegList[['Mid 4']]$gam_spring_combo,
	RegList[['High 1']]$gam_spring_combo,
	RegList[['High 2']]$gam_spring_combo,
	RegList[['High 3']]$gam_spring_combo,
	title = 'Before July',
	transform = NULL,
  	digits = 3,
  	p.style = "stars", 	
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = c('(Intercept)',"Humidity_ma_lag", "Visits_ma_lag",'s(as.numeric(Date))'),
	file = 'output/BeforeJuly.html')

# Fall
tab_model(
	RegList[['Low 1']]$gam_fall_combo,
	RegList[['Low 2']]$gam_fall_combo,
	RegList[['Low 3']]$gam_fall_combo,
	RegList[['Mid 1']]$gam_fall_combo,
	RegList[['Mid 2']]$gam_fall_combo,
	RegList[['Mid 3']]$gam_fall_combo,
	RegList[['Mid 4']]$gam_fall_combo,
	RegList[['High 1']]$gam_fall_combo,
	RegList[['High 2']]$gam_fall_combo,
	RegList[['High 3']]$gam_fall_combo, 
	title = 'After August',
	transform = NULL,
  	digits = 3,
  	p.style = "stars", 	
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = c('(Intercept)',"Humidity_ma_lag", "Visits_ma_lag",'s(as.numeric(Date))'),
	file = 'output/AfterAugust.html')

# Entire Year
tab_model(
	RegList[['Low 1']]$gam_allyear_combo,
	RegList[['Low 2']]$gam_allyear_combo,
	RegList[['Low 3']]$gam_allyear_combo,
	RegList[['Mid 1']]$gam_allyear_combo,
	RegList[['Mid 2']]$gam_allyear_combo,
	RegList[['Mid 3']]$gam_allyear_combo,
	RegList[['Mid 4']]$gam_allyear_combo,
	RegList[['High 1']]$gam_allyear_combo,
	RegList[['High 2']]$gam_allyear_combo,
	RegList[['High 3']]$gam_allyear_combo, 
	title = 'Entire Duration',
	transform = NULL,
  	digits = 3,
  	p.style = "stars", 	
	auto.label = FALSE,
	dv.labels = mod_label,
	pred.labels = pl, 
	collapse.ci = TRUE,
	terms = c('(Intercept)',"Humidity_ma_lag", "Visits_ma_lag",'s(as.numeric(Date))'),
	file = 'output/AllYear.html')

clappendix <- c(
	'Both (March-Nov)',
	'Absolute Humidity Model (March-Nov)',
	'Non-essential Visitations (March-Nov)',
	'Both (March-June)',
	'Absolute Humidity Model (March-June)', 
	'Non-essential Visitations (March-June)',
	'Both (July-Aug)',
	'Absolute Humidity Model (July-Aug)', 
	'Non-essential Visitations (July-Aug)',
	'Both (Sept-Nov)',
	'Absolute Humidity Model (Sept-Nov)', 
	'Non-essential Visitations (Sept-Nov)'
	)

# appendix
lapply(mod_label, function (x){
	tab_model(
		RegList[[as.character(x)]]$gam_allyear_combo,
		RegList[[as.character(x)]]$gam_allyear_humid,
		RegList[[as.character(x)]]$gam_allyear_visit,
		RegList[[as.character(x)]]$gam_spring_combo,
		RegList[[as.character(x)]]$gam_spring_humid,
		RegList[[as.character(x)]]$gam_spring_visit,
		RegList[[as.character(x)]]$gam_summer_combo,
		RegList[[as.character(x)]]$gam_summer_humid,
		RegList[[as.character(x)]]$gam_summer_visit,
		RegList[[as.character(x)]]$gam_fall_combo,
		RegList[[as.character(x)]]$gam_fall_humid,
		RegList[[as.character(x)]]$gam_fall_visit, 
		title = as.character(x),
		transform = NULL,
	  	digits = 3,
	  	p.style = "stars", 	
		auto.label = FALSE,
		dv.labels = clappendix,
		pred.labels = pl, 
		collapse.ci = TRUE,
		terms = c('(Intercept)',"Humidity_ma_lag", "Visits_ma_lag",'s(as.numeric(Date))'),
		file = paste0('output/',as.character(x),'.html'))
})
