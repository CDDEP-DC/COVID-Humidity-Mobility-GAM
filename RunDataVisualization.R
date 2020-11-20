library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(RcppRoll)
library(viridis)
library(tidycensus)
library(pscl)
library(socviz)
library(fishualize)
library(ggthemes)
library(ggpubr)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/NatureMed/Code') # change directory
source('ProcessData.R') 

# Build Plot Data
Ranks <- c('Low 1','Low 2','Low 3','Mid 1','Mid 2','Mid 3','Mid 4','High 1','High 2','High 3')
plotdata <- widedata %>% 
	mutate(`New Cases per 100,000 people` = NewCasePC_ma * 100000) %>% 
	mutate(TimePeriod = case_when(
		Date < as_date('2020-07-01') ~'March to June',
		Date >= as_date('2020-07-01') & Date < as_date('2020-09-01') ~'July to August',
		Date >= as_date('2020-09-01') ~'September to October')) %>%
	rename(
		'Absolute Humidity' = 'Humidity_ma',
		'Non-essential Visitations' = 'Visits_ma')

# Calculate average values for each county
countyMeans <-	plotdata %>% ungroup() %>% 
	group_by(FIPS, TimePeriod) %>% 
	mutate(FIPS = as.integer(as.character(FIPS))) %>%
	summarize(
		`Absolute Humidity` = mean(`Absolute Humidity`, na.rm =T),
		`Non-essential Visitations` = mean(`Non-essential Visitations`, na.rm=T),
		`New Cases per 100,000 people` = mean(`New Cases per 100,000 people`, na.rm=T)) %>%
	left_join(HumClustMember, by = c('FIPS' = 'fips')) %>%
	mutate(ClusterRank = factor(ClusterRank, levels = Ranks))
  
# Scatterplot of Cases vs. Absolute Humidity
humscatplot <- ggplot(countyMeans, 
	aes(x=`Absolute Humidity`, y=`New Cases per 100,000 people`, group = ClusterRank, color=ClusterRank)) +
	geom_point(size = .5) +
	facet_grid(ClusterRank~TimePeriod) +
	geom_smooth(method = "lm", alpha = .15,color='black') +
	ylim(0,NA) +
	scale_color_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	theme_bw() + labs(color = 'Cluster') + 
	theme(legend.title = element_text(face="bold")) + ylab('New Cases per 100,000 people') + 
	xlab('Absolute Humidity')  + 
	theme(legend.position = "none",panel.grid.minor = element_blank()) +
	theme(strip.background =element_rect(fill="white"))

# Scatterplot of Cases vs. Non-essential Visits
movescatplot <- ggplot(countyMeans, 
	aes(x=`Non-essential Visitations`, y=`New Cases per 100,000 people`, group = ClusterRank, color=ClusterRank)) +
	geom_point(size = .5) +
	facet_grid(ClusterRank~TimePeriod) +
	geom_smooth(method = "lm", alpha = .15,color='black') +
	ylim(0,NA) +
	scale_color_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	theme_bw() + labs(color = 'Cluster') + 
	theme(legend.title = element_text(face="bold")) + ylab('New Cases per 100,000 people') + 
	xlab('Non-essential Visitations') +
	theme(legend.position = "none",panel.grid.minor = element_blank()) +
	theme(strip.background =element_rect(fill="white"))

# Plot All Time Series
mplotdata <- melt(plotdata, id.vars =c('FIPS','Date','ClusterRank')) %>% mutate(ClusterRank = factor(ClusterRank, levels = Ranks))
mplotdata$value = as.numeric(as.character(mplotdata$value))
humplotpc <- ggplot(mplotdata %>% filter(variable %in% c('New Cases per 100,000 people','Absolute Humidity','Non-essential Visitations')),
	aes(x=Date, y = value, color = factor(FIPS))) + 
	geom_line(alpha = .5,size=.1) + 
	facet_grid(variable ~ ClusterRank, scale = 'free_y') +
	scale_color_viridis(discrete=TRUE) +
	theme_bw() +
	theme(legend.position = "none") + ylab('') + xlab('Date') + 
	theme(strip.background =element_rect(fill="white"))

# Plot Cluster Temporal Averages 
meanValuePlotdata <- mplotdata %>% group_by(ClusterRank,Date,variable) %>% summarize(meanValue = mean(value, na.rm =T)) %>%
	mutate(ClusterRank = factor(ClusterRank, levels = Ranks))
mean_hum <- ggplot(data=meanValuePlotdata %>% filter(variable %in% c('New Cases per 100,000 people','Absolute Humidity','Non-essential Visitations')), 
	aes(x=Date, y = meanValue, color = ClusterRank)) + 
	geom_line(alpha = 1, size=.3) + 
	scale_color_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	facet_grid(variable ~ ., scale = 'free_y') +
	theme_bw() + labs(color = 'Cluster') + 
	theme(legend.title = element_text(face="bold")) + 
	ylab('') + xlab('') + theme(strip.background =element_rect(fill="white"))

# Generate Map
mapdata <- county_map  %>% 
	mutate(fips = as.integer(id)) %>%
	left_join(HumClustMember, by = 'fips')
mapdata$ClusterRank <- factor(mapdata$ClusterRank, levels = Ranks)

HumClusterMap <-  ggplot(data = mapdata, aes(x = long, y = lat, fill = ClusterRank, group = group)) + 
	geom_polygon(color = "gray90", size = 0.05) + coord_equal() + 
	scale_fill_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	labs(fill = "Cluster") +  
	theme_map() +  guides(fill = guide_legend(nrow = 1)) + 
    theme(legend.position = "bottom",legend.title = element_text(face="bold"))

figure1 <- ggarrange(HumClusterMap,mean_hum,  
	legend = 'bottom', common.legend = T, labels = c("A", "B"),
	widths = c(2, 1))
dev.off()

# Generate Peak Maps
startdates <- ConfData %>% 
	select(-c(UID, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key))  %>% 
	melt(id.vars='FIPS') %>%  
	group_by(FIPS) %>%
	mutate(FIPS = as.integer(as.character(FIPS))) %>% 
	mutate(NewCase = value - lag(value))  %>%
	rename(Date = variable) %>%
	na.omit() 
startdates$Date = mdy(as.character(startdates$Date)) 

mapdata_spring <- mapdata %>% left_join(startdates %>% 
	filter(Date < as_date('2020-07-01')) %>% 
	summarize(PeakDate = Date[which.max(NewCase)]), by = c('fips' = 'FIPS')) # %>% 
	# filter(ClusterRank %in% c('High 1','High 2','High 3'))

SpringStartMap <- ggplot(data = mapdata_spring, aes(x = long, y = lat, fill = PeakDate, group = group)) + 
	geom_polygon(color = "gray90", size = 0.01) + coord_equal() +
    scale_fill_viridis_c(trans = "date") +
	labs(fill = "Date of Peak") +  
	theme_map() +  
    theme(legend.position = "right",legend.title = element_text(face="bold"))

mapdata_summer <- mapdata %>% left_join(startdates %>% 
	filter(Date >= as_date('2020-07-01'), Date < as_date('2020-09-01')) %>% 
	summarize(PeakDate = Date[which.max(NewCase)]), by = c('fips' = 'FIPS')) # %>% 
	# filter(ClusterRank %in% c('High 1','High 2','High 3'))

SummerStartMap <- ggplot(data = mapdata_summer, aes(x = long, y = lat, fill = PeakDate, group = group)) + 
	geom_polygon(color = "gray90", size = 0.01) + coord_equal() +
    scale_fill_viridis_c(trans = "date") +
	labs(fill = "Date of Peak") +  
	theme_map() +  
    theme(legend.position = "right",legend.title = element_text(face="bold"))

mapdata_fall <- mapdata %>% left_join(startdates %>% 
	filter(Date >= as_date('2020-09-01')) %>% 
	summarize(PeakDate = Date[which.max(NewCase)]), by = c('fips' = 'FIPS')) # %>% 
	# filter(ClusterRank %in% c('High 1','High 2','High 3'))

FallStartMap <- ggplot(data = mapdata_fall, aes(x = long, y = lat, fill = PeakDate, group = group)) + 
	geom_polygon(color = "gray90", size = 0.01) + coord_equal() +
    scale_fill_viridis_c(trans = "date") +
	labs(fill = "Date of Peak") +  
	theme_map() + 
    theme(legend.position = "right",legend.title = element_text(face="bold"))

mapdata_allyear <- mapdata %>% left_join(startdates %>% 
	summarize(PeakDate = Date[which.max(NewCase)]), by = c('fips' = 'FIPS')) # %>% 
	# filter(ClusterRank %in% c('High 1','High 2','High 3'))

AllYearStartMap <- ggplot(data = mapdata_allyear, aes(x = long, y = lat, fill = PeakDate, group = group)) + 
	geom_polygon(color = "gray90", size = 0.01) + coord_equal() +
    scale_fill_viridis_c(trans = "date") +
	labs(fill = "Date of Peak") +  
	theme_map() + 
    theme(legend.position = "right",legend.title = element_text(face="bold"))

figure3 <- ggarrange(AllYearStartMap, 
	ggarrange(SpringStartMap, SummerStartMap, FallStartMap, 
		ncol = 3, labels = c("B", "C", "D")),
	labels = c("A", ""), nrow = 2, heights = c(2,1))
dev.off()


pdf('output/TimeSeriesByClusters.pdf', height = 8, width = 16)
print(humplotpc)
dev.off()

pdf('output/HumidityClusterMean.pdf', height = 6, width = 8)
print(mean_hum)
dev.off()

pdf('output/Figure 1.pdf', height = 7, width = 14)
print(figure1)
dev.off()

pdf('output/Figure 2.pdf', height = 10, width = 10)
print(humscatplot)
dev.off()

pdf('output/Movement.pdf', height = 10, width = 10)
print(movescatplot)
dev.off()

pdf('output/Figure 3.pdf', height = 10, width = 15)
print(figure3)
dev.off()