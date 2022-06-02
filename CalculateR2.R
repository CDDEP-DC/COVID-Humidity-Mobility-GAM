library(DescTools)

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

R2s <- lapply(AllClusters, function(g) {
    glmList <- RegList[[g]]
    R2obj <- lapply(glmnames, function(x) {
        PseudoR2(glmList[[x]], which = 'McFadden')
    })
    names(R2obj) <- glmnames
    return(R2obj)
})
names(R2s) <- AllClusters
r2DF <- data.frame(matrix(data = NA, ncol = 2, nrow = length(AllClusters)*length(glmnames)))
colnames(r2DF) <- c("Model", "Cluster")
tempInd <- 1
for (i in AllClusters) {
    for (j in glmnames) {
        r2Vect <- R2s[[i]][[j]]
        # r2Vect <- r2s[[i]][[j]][, "Gr2"]
        r2DF[tempInd,names(r2Vect)] <- r2Vect
        r2DF[tempInd, "Cluster"] <- i
        r2DF[tempInd, "Model"] <- j
        tempInd <- tempInd + 1
    }
}

r2DF$Model[r2DF$Model == "glm_beforeOct_fips"] <- "All (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_humidfips"] <- "Absolute Humidity (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_RetailRec"] <- "Retail and Rec (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_GroceryPharmacy"] <- "Grocery and Pharmacy (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_Parks"] <- "Parks (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_Transit"] <- "Transit (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_Workplaces"] <- "Workplaces (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_beforeOct_Residential"] <- "Residential (Mar 2020 - Sep 2020)"
r2DF$Model[r2DF$Model == "glm_afterOct_fips"] <- "All (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_humidfips"] <- "Absolute Humidity (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_RetailRec"] <- "Retail and Rec (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_GroceryPharmacy"] <- "Grocery and Pharmacy (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_Parks"] <- "Parks (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_Transit"] <- "Transit (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_Workplaces"] <- "Workplaces (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_afterOct_Residential"] <- "Residential (Oct 2020 - Mar 2021)"
r2DF$Model[r2DF$Model == "glm_allyear_fips"] <- "All (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_humidfips"] <- "Absolute Humidity (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_RetailRec"] <- "Retail and Rec (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_GroceryPharmacy"] <- "Grocery and Pharmacy (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_Parks"] <- "Parks (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_Transit"] <- "Transit (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_Workplaces"] <- "Workplaces (Entire Year)"
r2DF$Model[r2DF$Model == "glm_allyear_Residential"] <- "Residential (Entire Year)"

write.csv(r2DF, file = "output/R2S.csv", row.names = F)
