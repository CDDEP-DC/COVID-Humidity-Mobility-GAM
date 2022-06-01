library(aTSA)
library(plm)
library(reshape2)

print("Performing Unit Root Test ...")
testdata <- regdata %>%
	select(
		NewCasePht,
		CumCasesPC,
		Humidity_ma_lag,
		RetailRec_ma_lag_scaled,
		GroceryPharmacy_ma_lag_scaled,
		Parks_ma_lag_scaled,
		Transit_ma_lag_scaled,
		Workplaces_ma_lag_scaled,
		Residential_ma_lag_scaled,
		Humidity_ma_lag,
		FIPS,
		date
	) %>%
	melt(id.vars = c("date", "FIPS")) %>%
	# group_by(date, variable) %>%
	# summarize(avgValue = mean(value)) %>%
	arrange(FIPS, date) %>%
	spread(key = variable, value = value) %>%
    ungroup()

pullVarNames <- names(testdata)[3:length(names(testdata))]
pdataBeforeOct <- testdata %>% filter(date < as_date('2020-10-01')) %>% pdata.frame(index = c("FIPS", "date")) %>% select(pullVarNames)
pdataAfterOct <- testdata %>% filter(date >= as_date('2020-10-01')) %>% pdata.frame(index = c("FIPS", "date")) %>% select(pullVarNames)
pdata <- testdata %>% pdata.frame(index = c("FIPS", "date")) %>% select(pullVarNames)

# Levin-Lin-Chu Test
purtestresultsBeforeOct <- purtest(pdataBeforeOct , exo = "intercept", test = "levinlin", pmax = 14)
purtestresultsAfterOct <- purtest(pdataAfterOct, exo = "intercept", test = "levinlin", pmax = 14)
purtestresults <- purtest(pdata, exo = "intercept", test = "levinlin", pmax = 14)

summary(purtestresultsBeforeOct)
summary(purtestresultsAfterOct)
summary(purtestresults)

write.csv(summary(purtestresultsBeforeOct)$sumidres, file = "output/UnitRootTest_BeforeOct.csv")
write.csv(summary(purtestresultsAfterOct)$sumidres, file = "output/UnitRootTest_AfterOct.csv")
write.csv(summary(purtestresults)$sumidres, file = "output/UnitRootTest_AllYear.csv")
