library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(RcppRoll)

# Load data from data directory
setwd('~/../Dropbox/MobilityHumidity/GAMStudy/Manuscript/GitHub/COVID-Humidity-Mobility') # change directory
source('ProcessData.R') 
cordata <- procdata %>% filter(CumCases > 20, NewCasePht >= 0, NewCasePht_ma >= 0, Population >= 50000)

# Correlation analysis
cormat <- lapply(AllClusters, function(x){
	widedata_march <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-03-01'), date < as_date('2020-04-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_march <- cor(na.omit(widedata_march))
	widedata_april <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-04-01'), date < as_date('2020-05-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_april <-cor(na.omit(widedata_april))
	widedata_may <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-05-01'), date < as_date('2020-06-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_may <-cor(na.omit(widedata_may))
	widedata_june <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-06-01'), date < as_date('2020-07-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_june <-cor(na.omit(widedata_june))
	widedata_july <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-07-01'), date < as_date('2020-08-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_july <-cor(na.omit(widedata_july))
	widedata_aug <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-08-01'), date < as_date('2020-09-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_aug <-cor(na.omit(widedata_aug))
	widedata_sept <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-09-01'), date < as_date('2020-10-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_sept <-cor(na.omit(widedata_sept))
	widedata_oct <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-10-01'), date < as_date('2020-11-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_oct <-cor(na.omit(widedata_oct))
	widedata_nov <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-11-01'), date < as_date('2020-12-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_nov <-cor(na.omit(widedata_nov))
	widedata_dec <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2020-12-01'), date < as_date('2021-1-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_dec <-cor(na.omit(widedata_dec))
	widedata_jan <- cordata %>% ungroup() %>% filter(ClusterRank == x, date >= as_date('2021-1-01'), date < as_date('2021-2-01')) %>%
		select(c(	
			"RetailRec_ma_lag_scaled",
			"GroceryPharmacy_ma_lag_scaled",
			"Parks_ma_lag_scaled",
			"Transit_ma_lag_scaled",
			"Workplaces_ma_lag_scaled",
			"Residential_ma_lag_scaled",
			'Humidity_ma_lag',
			'NewCasePht'))
	cor_jan <-cor(na.omit(widedata_dec))
	return(list(
		`Mar 2020` = cor_march, 
		`Apr 2020` = cor_april, 
		`May 2020` = cor_may, 
		`Jun 2020` = cor_june, 
		`Jul 2020` = cor_july, 
		`Aug 2020` = cor_aug, 
		`Sep 2020` = cor_sept, 
		`Oct 2020` = cor_oct,
		`Nov 2020` = cor_nov,
		`Dec 2020` = cor_dec,
		`Jan 2021` = cor_jan))
})
names(cormat) <- AllClusters

humcordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','Humidity_ma_lag'])})
RetailRecCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','RetailRec_ma_lag_scaled'])})
GroceryPharmacyCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','GroceryPharmacy_ma_lag_scaled'])})
ParksCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','Parks_ma_lag_scaled'])})
TransitCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','Transit_ma_lag_scaled'])})
WorkplacesCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','Workplaces_ma_lag_scaled'])})
ResidentialCordata <- sapply(cormat, function(x) {df = sapply(x, function(y) y['NewCasePht','Residential_ma_lag_scaled'])})

mheatdata <- rbind(
	cbind(reshape2::melt(humcordata), series = 'Absolute Humidity'),
	cbind(reshape2::melt(RetailRecCordata), series = 'RetailRec Visitations'),
	cbind(reshape2::melt(GroceryPharmacyCordata), series = 'GroceryPharmacy Visitations'),
	cbind(reshape2::melt(ParksCordata), series = 'Parks Visitations'),
	cbind(reshape2::melt(TransitCordata), series = 'Transit Visitations'),
	cbind(reshape2::melt(WorkplacesCordata), series = 'Workplaces Visitations'),
	cbind(reshape2::melt(ResidentialCordata), series = 'Residential Visitations'))
mheatdata$value = as.numeric(mheatdata$value) 
mheatdata$Var2 = factor(mheatdata$Var2, levels = c('Low 1','Low 2','Low 3','Mid 1','Mid 2','Mid 3','Mid 4','High 1','High 2','High 3'))

corheatmap <- ggplot(mheatdata, aes(x=Var1,y=Var2)) + 
	geom_tile(aes(fill = value)) + 
	geom_text(aes(label = round(value,3)),size=2) +
	facet_wrap(series~.) + 
	theme_classic() +
	xlab('Month') + ylab('Cluster') + labs(fill = 'Pearson Correlation') +
	scale_fill_distiller(palette = "RdBu",limits = c(-.77, .77),direction = 1)

pdf('output/Figure S2.pdf',height=12,width =22)
print(corheatmap)
dev.off()