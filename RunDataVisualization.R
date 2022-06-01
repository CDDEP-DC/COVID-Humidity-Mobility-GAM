library(tidyr)
library(dplyr)
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
setwd('C:/Users/GaryLin/Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
source('ProcessData.R') 

# Build Plot Data
Ranks <- c('Low 1','Low 2','Mid 1','Mid 2','High 1','High 2')
plotdata <- procdata  %>% 
	filter(CumCases > 20, NewCase >= 0, NewCase_ma >= 0, Population >= 50000, !is.na(ClusterRank), date < as_date('2021-03-01')) %>% 
	mutate(`New Cases per 100,000 people` = NewCasePht) %>% 
	mutate(TimePeriod = case_when(
		date < as_date('2020-10-01') ~'March 2020 to September 2020',
		date >= as_date('2020-10-01') ~'October 2020 to March 2021')) %>%
	mutate(TimePeriod =  factor(TimePeriod,
		c(	'March 2020 to September 2020',
			'October 2020 to March 2021'))) %>%
	rename(
		'Absolute Humidity (g/m^3)' = 'Humidity_ma',
		'Retail and Recreation Movement' = 'RetailRec_ma',
		'Grocery and Pharmacy Movement' = 'GroceryPharmacy_ma',
		'Parks Movement' = 'Parks_ma',
		'Transit Station Movement' = 'Transit_ma',
		'Workplace Movement' = 'Workplaces_ma',
		'Residential Movement' = 'Residential_ma'
	)

# Calculate average values for each county
countyMeans <-	plotdata %>% ungroup() %>% 
	group_by(FIPS, TimePeriod) %>% 
	mutate(FIPS = as.integer(as.character(FIPS))) %>%
	summarize(
		`Absolute Humidity (g/m^3)` = mean(`Absolute Humidity (g/m^3)`, na.rm =T),
		`Retail and Recreation Movement` = mean(`Retail and Recreation Movement`, na.rm=T),
		`Grocery and Pharmacy Movement` = mean(`Grocery and Pharmacy Movement`, na.rm=T),
		`Parks Movement` = mean(`Parks Movement`, na.rm=T),
		`Transit Station Movement` = mean(`Transit Station Movement`, na.rm=T),
		`Workplace Movement` = mean(`Workplace Movement`, na.rm=T),
		`Residential Movement` = mean(`Residential Movement`, na.rm=T),
		`New Cases per 100,000 people` = max(`New Cases per 100,000 people`, na.rm=T)) %>%
	left_join(HumClustMember, by = c('FIPS' = 'fips')) %>%
	mutate(ClusterRank = factor(ClusterRank, levels = Ranks))

# Scatterplot of Cases vs. Non-essential Visits
mcountyMeans <- countyMeans %>% 
	select(
		'FIPS',
		'TimePeriod',
		'Absolute Humidity (g/m^3)',
		"ClusterRank",
		"New Cases per 100,000 people",
		'Retail and Recreation Movement',
		'Grocery and Pharmacy Movement',
		'Parks Movement',
		'Transit Station Movement',
		'Workplace Movement',
		'Residential Movement') %>%
	reshape2::melt(id.vars = c('FIPS','TimePeriod',"ClusterRank","New Cases per 100,000 people"))

figure2 <- ggplot(mcountyMeans, 
	aes(x=value, y=`New Cases per 100,000 people`, group = ClusterRank, color=TimePeriod)) +
	geom_point(size = .5) +
	facet_grid(ClusterRank~variable, scale ='free_x') +
	geom_smooth(method = "lm", alpha = .15,color='black') +
	ylim(0,NA) +
	scale_color_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	theme_bw() + labs(color = 'Time Period') + 
	theme(legend.title = element_text(face="bold")) + ylab('New Cases per 100,000 people') + 
	xlab('') +
	theme(strip.background =element_rect(fill="white"))

# Plot All Time Series
mplotdata <- reshape2::melt(plotdata, id.vars =c('FIPS','date','ClusterRank')) %>% mutate(ClusterRank = factor(ClusterRank, levels = Ranks))
mplotdata$value = as.numeric(as.character(mplotdata$value))
figureS2 <- ggplot(mplotdata %>% filter(variable %in% 
	c(	'New Cases per 100,000 people',
		'Absolute Humidity (g/m^3)',
		'Retail and Recreation Movement',
		'Grocery and Pharmacy Movement',
		'Parks Movement',
		'Transit Station Movement',
		'Workplace Movement',
		'Residential Movement')),
	aes(x=date, y = value, color = factor(FIPS))) + 
	geom_line(alpha = .5,size=.1) + 
	facet_grid(variable ~ ClusterRank, scale = 'free_y') +
	scale_color_viridis(discrete=TRUE) +
	theme_bw() +
	theme(legend.position = "none") + ylab('') + xlab('Date') + 
	theme(strip.background =element_rect(fill="white"))

# Plot Cluster Temporal Averages 
meanValuePlotdata <- mplotdata %>% 
	group_by(ClusterRank,date,variable) %>% 
	summarize(
		meanValue = mean(value, na.rm =T),
		n = n()) %>%
	mutate(ClusterRank = factor(ClusterRank, levels = Ranks))
mean_hum <- ggplot(data=meanValuePlotdata %>% filter(variable %in% 
	c(	'New Cases per 100,000 people',
		'Absolute Humidity (g/m^3)',
		'Retail and Recreation Movement',
		'Grocery and Pharmacy Movement',
		'Parks Movement',
		'Transit Station Movement',
		'Workplace Movement',
		'Residential Movement')),
	aes(x=date, y = meanValue, color = ClusterRank)) + 
	geom_line(alpha = 1, size=.3) + 
	scale_color_fish(option = "Stethojulis_bandanensis", discrete = T, alpha = 0.8) + 
	facet_wrap(variable ~ ., scale = 'free_y',ncol=1) +
	theme_bw() + labs(color = 'Cluster') + 
	theme(legend.title = element_text(face="bold")) + 
	ylab('') + xlab('') + theme(strip.background =element_rect(fill="white"))

# Generate Map
regcounties <- plotdata %>% pull(FIPS) %>% unique()
mapdata <- county_map %>% 
	mutate(
		fips = as.integer(id), 
		regcounties = ifelse(fips %in% regcounties,'yes','no')
	) %>%
	left_join(HumClustMember, by = 'fips')
mapdata$ClusterRank <- factor(mapdata$ClusterRank, levels = Ranks)

HumClusterMap <-  ggplot() + 
	geom_polygon(data = mapdata, 
		aes(x = long, y = lat, fill = ClusterRank, group = group, alpha=regcounties),
		size = 0.05, color = 'grey90') + 
	coord_equal() + 
	scale_fill_fish(option = "Stethojulis_bandanensis", discrete = T,alpha = .8) + 
	scale_alpha_discrete(range = c(0.4, 1)) + 
	labs(fill = "Cluster", alpha = 'Included in Regression') +  
	theme_map() +  guides(fill = guide_legend(nrow = 1)) + 
    theme(legend.position = "bottom",legend.title = element_text(face="bold"))

figure1 <- ggarrange(HumClusterMap,mean_hum,  
	legend = 'bottom', 
	common.legend = T, 
	labels = c("A", "B"),
	widths = c(2, 1.3))
dev.off()


pdf('output/Figure S1.pdf', height = 17, width = 22)
print(figureS2)
dev.off()

pdf('output/Figure 1.pdf', height = 9, width = 14)
print(figure1)
dev.off()

pdf('output/Figure 2.pdf', height = 12, width = 18)
print(figure2)
dev.off()