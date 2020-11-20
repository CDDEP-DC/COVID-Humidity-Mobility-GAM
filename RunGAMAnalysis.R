library(tidyverse)
library(data.table)
library(mgcv)
library(ggplot2)
library(lubridate)
library(RcppRoll)
library(viridis)
library(mgcViz)

set.seed(99)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/NatureMed/Code') # change directory
source('ProcessData.R') 

# Run GAMS
RegList <- lapply(AllClusters, function(x){
	print(paste('******* Running Regression for', x))
	SummerData <- widedata %>% filter(ClusterRank == x, Date >= as_date('2020-07-01'), Date < as_date('2020-09-01'))

	print('Fitting Summer AH GAM')
	gam_summer_humid <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + FIPS, family= poisson, data=SummerData, method = "REML")
	print('Fitting Summer NE GAM')
	gam_summer_visit <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Visits_ma_lag + FIPS, family= poisson, data=SummerData, method = "REML") 
	print('Fitting Summer NE + AH GAM')
	gam_summer_combo <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + Visits_ma_lag + FIPS, family= poisson, data=SummerData, method = "REML") 

	SpringData <- widedata %>% filter(ClusterRank == x, Date < as_date('2020-07-01'))

	print('Fitting Spring AH GAM')
	gam_spring_humid <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + FIPS, family= poisson, data=SpringData, method = "REML")
	print('Fitting Spring NE GAM')
	gam_spring_visit <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Visits_ma_lag + FIPS, family= poisson, data=SpringData, method = "REML") 
	print('Fitting Spring NE + AH GAM')
	gam_spring_combo <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + Visits_ma_lag + FIPS, family= poisson, data=SpringData, method = "REML") 

	AllYearData <- widedata %>% filter(ClusterRank == x)

	print('Fitting All Year AH GAM')
	gam_allyear_humid <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + FIPS, family= poisson, data=AllYearData, method = "REML")
	print('Fitting All Year NE GAM')
	gam_allyear_visit <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Visits_ma_lag + FIPS, family= poisson, data=AllYearData, method = "REML") 
	print('Fitting All Year NE + AH GAM')
	gam_allyear_combo <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + Visits_ma_lag + FIPS, family= poisson, data=AllYearData, method = "REML") 

	FallData <- widedata %>% filter(ClusterRank == x, Date >= as_date('2020-09-01'))

	print('Fitting Fall AH GAM')
	gam_fall_humid <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + FIPS, family= poisson, data=FallData, method = "REML")
	print('Fitting Fall NE GAM')
	gam_fall_visit <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Visits_ma_lag + FIPS, family= poisson, data=FallData, method = "REML") 
	print('Fitting Fall NE + AH GAM')
	gam_fall_combo <- gam(NewCase ~ offset(log(Population)) + s(as.numeric(Date)) + Humidity_ma_lag + Visits_ma_lag + FIPS, family= poisson, data=FallData, method = "REML") 



	return(list(
		gam_summer_humid = gam_summer_humid,
		gam_summer_visit = gam_summer_visit,
		gam_summer_combo = gam_summer_combo,
		gam_spring_humid = gam_spring_humid,
		gam_spring_visit = gam_spring_visit,
		gam_spring_combo = gam_spring_combo,
		gam_allyear_humid = gam_allyear_humid,
		gam_allyear_visit = gam_allyear_visit,
		gam_allyear_combo = gam_allyear_combo,
		gam_fall_humid = gam_fall_humid,
		gam_fall_visit = gam_fall_visit,
		gam_fall_combo = gam_fall_combo,
		FIPS = unique(c(as.character(SummerData$FIPS), as.character(SpringData$FIPS), as.character(AllYearData$FIPS), as.character(FallData$FIPS)))
	)) 
})

# generate GAM output tables
source('GenerateTables.R')

# plot all time-varying splines
combdata = c(x = NULL, y = NULL, ty = NULL, se = NULL, TimePeriod = NULL, Cluster = NULL)
for(x in mod_label){
	b1 <- getViz(RegList[[as.character(x)]]$gam_summer_combo)
	b2 <- getViz(RegList[[as.character(x)]]$gam_spring_combo)
	b3 <- getViz(RegList[[as.character(x)]]$gam_allyear_combo)
	b4 <- getViz(RegList[[as.character(x)]]$gam_fall_combo)

	o1 <- plot(sm(b1, 1))
	o2 <- plot(sm(b2, 1))
	o3 <- plot(sm(b3, 1))
	o4 <- plot(sm(b4, 1))

	combdata <- rbind(combdata,
		cbind(o2$data$fit, `Time Period` = 'March - June', Cluster = as.character(x)),
		cbind(o1$data$fit, `Time Period` = 'July - August', Cluster = as.character(x)),
		cbind(o4$data$fit, `Time Period` = 'September - November', Cluster = as.character(x)),
		cbind(o3$data$fit, `Time Period` = 'March - November', Cluster = as.character(x))

	)
}
combdata$x <- as_date(round(combdata$x))
combdata$Cluster <-  factor(combdata$Cluster, levels = mod_label)

splineplot <- ggplot(combdata, aes(x=x,y=y,group = `Time Period`)) + 
	scale_color_viridis(discrete=TRUE) +
	geom_line(aes(color=`Time Period`),size=.5) + 
	geom_ribbon(aes(ymin=y-se, ymax=y+se), fill = "grey70", alpha =.5) +
	facet_wrap(Cluster~.)  + ylab('f(time)') + 
	xlab('time') +
	theme_classic()

pdf('output/Spline_COMBO.pdf', height = 6, width = 11)
print(splineplot)
dev.off()

