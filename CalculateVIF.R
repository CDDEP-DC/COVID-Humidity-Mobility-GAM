library(car)

glmnames <- c(
    "glm_beforeOct_fips",
    "glm_beforeOct_humidfips",
    "glm_beforeOct_RetailRec",
    "glm_beforeOct_GroceryPharmacy",
    "glm_beforeOct_Parks",
    "glm_beforeOct_Transit",
    "glm_beforeOct_Workplaces",
    "glm_beforeOct_Residential",
    "glm_afterOct_fips",
    "glm_afterOct_humidfips",
    "glm_afterOct_RetailRec",
    "glm_afterOct_GroceryPharmacy",
    "glm_afterOct_Parks",
    "glm_afterOct_Transit",
    "glm_afterOct_Workplaces",
    "glm_afterOct_Residential",
    "glm_allyear_fips",
    "glm_allyear_humidfips",
    "glm_allyear_RetailRec",
    "glm_allyear_GroceryPharmacy",
    "glm_allyear_Parks",
    "glm_allyear_Transit",
    "glm_allyear_Workplaces",
    "glm_allyear_Residential"
)
varnames <- c(
    "Humidity_ma_lag",
    "RetailRec_ma_lag_scaled",
    "GroceryPharmacy_ma_lag_scaled",
    "Parks_ma_lag_scaled",
    "Transit_ma_lag_scaled",
    "Workplaces_ma_lag_scaled",
    "Residential_ma_lag_scaled",
    "CumCasesPC",
    "NewCasePht_lag",
    "FIPS"
)

vifs <- lapply(AllClusters, function(g) {
    glmList <- RegList[[g]]
    vifobj <- lapply(glmnames, function(x) {
        vif(glmList[[x]])
    })
    names(vifobj) <- glmnames
    return(vifobj)
})
names(vifs) <- AllClusters
tempInd <- 1
vifDF <- data.frame(matrix(data = NA, ncol = length(varnames) + 2, nrow = length(AllClusters)*length(glmnames)))
colnames(vifDF) <- c("Model", "Cluster", varnames)
for (i in AllClusters) {
    for (j in glmnames) {
        vifVect <- vifs[[i]][[j]][, "GVIF^(1/(2*Df))"]
        # vifVect <- vifs[[i]][[j]][, "GVIF"]
        vifDF[tempInd,names(vifVect)] <- vifVect
        vifDF[tempInd, "Cluster"] <- i
        vifDF[tempInd, "Model"] <- j
        tempInd <- tempInd + 1
    }
}

names(vifDF)[names(vifDF) == 'Humidity_ma_lag'] <- paste0("Absolute Humidity (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'RetailRec_ma_lag_scaled'] <- paste0("Retail and Recreation (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'GroceryPharmacy_ma_lag_scaled'] <- paste0("Grocery Stores and Pharmacies (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'Parks_ma_lag_scaled'] <- paste0("Parks (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'Transit_ma_lag_scaled'] <- paste0("Transit Stations (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'Workplaces_ma_lag_scaled'] <- paste0("Workplaces (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'Residential_ma_lag_scaled'] <- paste0("Residential (",LagTime,"-day Lag)")
names(vifDF)[names(vifDF) == 'CumCasesPC'] <- "Immunity Factor"
names(vifDF)[names(vifDF) == 'NewCasePht_lag'] <- paste0("New Cases per 100,000 (",LagTime,"-day Lag)")

vifDF$Model[vifDF$Model == "glm_beforeOct_fips"] <- "All (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_humidfips"] <- "Absolute Humidity (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_RetailRec"] <- "Retail and Rec (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_GroceryPharmacy"] <- "Grocery and Pharmacy (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_Parks"] <- "Parks (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_Transit"] <- "Transit (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_Workplaces"] <- "Workplaces (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_beforeOct_Residential"] <- "Residential (Mar 2020 - Sep 2020)"
vifDF$Model[vifDF$Model == "glm_afterOct_fips"] <- "All (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_humidfips"] <- "Absolute Humidity (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_RetailRec"] <- "Retail and Rec (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_GroceryPharmacy"] <- "Grocery and Pharmacy (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_Parks"] <- "Parks (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_Transit"] <- "Transit (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_Workplaces"] <- "Workplaces (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_afterOct_Residential"] <- "Residential (Oct 2020 - Mar 2021)"
vifDF$Model[vifDF$Model == "glm_allyear_fips"] <- "All (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_humidfips"] <- "Absolute Humidity (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_RetailRec"] <- "Retail and Rec (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_GroceryPharmacy"] <- "Grocery and Pharmacy (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_Parks"] <- "Parks (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_Transit"] <- "Transit (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_Workplaces"] <- "Workplaces (Entire Year)"
vifDF$Model[vifDF$Model == "glm_allyear_Residential"] <- "Residential (Entire Year)"

write.csv(vifDF, file = "output/VIFS.csv", row.names = F)
