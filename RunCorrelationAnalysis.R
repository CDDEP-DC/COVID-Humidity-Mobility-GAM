library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(RcppRoll)
library(viridis)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/NatureMed/Code') # change directory
source('ProcessData.R') 

# Correlation analysis
cormat <- lapply(AllClusters, function(x){
	widedata_march <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-03-01'), Date < as_date('2020-04-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_march <- cor(na.omit(widedata_march))
	widedata_april <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-04-01'), Date < as_date('2020-05-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_april <-cor(na.omit(widedata_april))
	widedata_may <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-05-01'), Date < as_date('2020-06-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_may <-cor(na.omit(widedata_may))
	widedata_june <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-06-01'), Date < as_date('2020-07-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_june <-cor(na.omit(widedata_june))
	widedata_july <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-07-01'), Date < as_date('2020-08-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_july <-cor(na.omit(widedata_july))
	widedata_aug <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-08-01'), Date < as_date('2020-09-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_aug <-cor(na.omit(widedata_aug))
	widedata_sept <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-09-01'), Date < as_date('2020-10-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_sept <-cor(na.omit(widedata_sept))
	widedata_oct <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-10-01'), Date < as_date('2020-11-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_oct <-cor(na.omit(widedata_oct))
	widedata_nov <- widedata %>% ungroup() %>% filter(ClusterRank == x, Date >= as_date('2020-11-01'), Date < as_date('2020-12-01')) %>%
		select(c('Visits_ma_lag','Humidity_ma_lag','NewCase'))
	cor_nov <-cor(na.omit(widedata_nov))
	return(list(
		March = cor_march, 
		April = cor_april, 
		May = cor_may, 
		June = cor_june, 
		July = cor_july, 
		August = cor_aug, 
		September = cor_sept, 
		October = cor_oct,
		November = cor_nov))
})
names(cormat) <- AllClusters

humcordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y[3,1])})
visitcordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y[3,2])})
mheatdata <- rbind(
	cbind(melt(humcordata), series = 'Absolute Humidity'),
	cbind(melt(visitcordata), series = 'Non-essential Visitations'))
mheatdata$value = as.numeric(mheatdata$value) 
mheatdata$Var2 = factor(mheatdata$Var2, levels = c('Low 1','Low 2','Low 3','Mid 1','Mid 2','Mid 3','Mid 4','High 1','High 2','High 3'))


corheatmap <- ggplot(mheatdata, aes(x=Var1,y=Var2)) + 
	geom_tile(aes(fill = value)) + 
	geom_text(aes(label = round(value,3)),size=2) +
	facet_wrap(series~.) + 
	theme_classic() +
	xlab('Month') + ylab('Cluster') + labs(fill = 'Pearson Correlation') +
	scale_fill_distiller(palette = "RdBu",limits = c(-.77, .77),direction = 1)

pdf('output/CorrelationHeat.pdf',height=6,width =14)
print(corheatmap)
dev.off()