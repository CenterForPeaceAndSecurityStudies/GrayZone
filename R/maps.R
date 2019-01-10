par(mfrow = c(2,1))

dcid <- readRDS(file = paste0(here::here(),"/data/grayzone_dcid.rds"))

dcid$severity_comparable <- NA
dcid[dcid$`Incident severity` < 3, "severity_comparable"] <- 1
dcid[dcid$`Incident severity` > 2 & dcid$`Incident severity` < 7, "severity_comparable"] <- 2

country <- unique(dcid$Target)
average_severity <- NULL
for( i in 1:length(country)){
  average_severity[i] <- round(mean(dcid[dcid$Target == country[i], "Incident severity"]), 2)
  #average_severity[i] <- round(mean(dcid[dcid$Target == country[i], "severity_comparable"]), 2)
}

dcid <- as.data.frame(cbind(country, average_severity), stringsAsFactors = F)
dcid$average_severity <- as.numeric(dcid$average_severity)

map <- rworldmap::joinCountryData2Map(dcid,
                                      nameJoinColumn="country",
                                      joinCode="NAME" )

#rworldmap::mapDevice()

mapParams <- rworldmap::mapCountryData(map,
                                       nameColumnToPlot='average_severity',
                                       catMethod = c(2, 2.3, 2.6, 2.9, 3.2, 3.5),
                                       mapTitle = "Russian cyber attack severity (2005-2016)\nValeriano and Maness data",
                                       addLegend = FALSE,
                                       xlim = c(-167, 50),
                                       ylim = c(30,50)
)
do.call(rworldmap::addMapLegendBoxes,
        c(mapParams,
          title = "Severity (1-10)",
          cex = 0.8
        )
)


rei <- readRDS(file = paste0(here::here(),"/data/grayzone_rei.rds"))

rei[rei$Target == "Ukraine ", "Target"] <- "Ukraine"

rei$severity_comparable <- NA
rei[rei$`Disinformation campaign` == 1 & rei$Cyberattack == 0, "severity_comparable"] <- 1
rei[rei$Cyberattack == 1, "severity_comparable"] <- 2

country <- unique(rei$Target)
average_severity <- NULL
for( i in 1:length(country)){
  #average_severity[i] <- round(mean(rei[rei$Target == country[i], "Favorable outcome"]), 2)
  average_severity[i] <- round(mean(rei[rei$Target == country[i], "severity_comparable"]), 2)
}

rei <- as.data.frame(cbind(country, average_severity), stringsAsFactors = F)
rei$average_severity <- as.numeric(rei$average_severity)

map <- rworldmap::joinCountryData2Map(rei,
                                      nameJoinColumn="country",
                                      joinCode="NAME" )


mapParams <- rworldmap::mapCountryData(map,
                                       nameColumnToPlot='average_severity',
                                       catMethod = "categorical",
                                       #catMethod = c(0, 0.4, 0.8, 1.2, 1.6, 2),
                                       mapTitle = "Russian cyber attack severity (1994-2017)\nWay and Casey data",
                                       addLegend = FALSE,
                                       xlim = c(-167, 50),
                                       ylim = c(30,50)
)
do.call(rworldmap::addMapLegendBoxes,
        c(mapParams,
          horiz = FALSE,
          title = "Severity (1-2)",
          cex = 0.8
          #legendLabels = "all"
        )
)




#### aggregate

rei_dcid <- readRDS(paste0(here::here(), "/data/grayzone_aggregate_cpass.rds"))

rei_dcid[is.na(rei_dcid[,4]), 4] <- 0
rei_dcid[is.na(rei_dcid[,5]), 5] <- 0
rei_dcid[is.na(rei_dcid[,6]), 6] <- 0
rei_dcid[is.na(rei_dcid[,7]), 7] <- 0
rei_dcid[is.na(rei_dcid[,8]), 8] <- 0

rei_dcid$resp_convmil_gro <- rei_dcid$resp_convmil_gro * 5
rei_dcid$resp_convmil_airsea <- rei_dcid$resp_convmil_airsea * 4
rei_dcid$resp_paramil <- rei_dcid$resp_paramil * 3
rei_dcid$rei_cyberdisrup <- rei_dcid$rei_cyberdisrup * 2


for( i in 1:nrow(rei_dcid)){
  rei_dcid$intensity[i] <- max(rei_dcid$resp_convmil_gro[i], rei_dcid$resp_convmil_airsea[i], rei_dcid$resp_paramil[i], rei_dcid$resp_cyberdisrup[i], rei_dcid$resp_infoops[i])
}

country <- unique(rei_dcid$target)
average_severity <- NULL
for( i in 1:length(country)){
  average_severity[i] <- median(rei_dcid[rei_dcid$target == country[i], "intensity"])
  #average_severity[i] <- round(mean(dcid[dcid$Target == country[i], "severity_comparable"]), 2)
}

rei_dcid <- as.data.frame(cbind(country, average_severity), stringsAsFactors = F)
rei_dcid$average_severity <- as.numeric(rei_dcid$average_severity)

map <- rworldmap::joinCountryData2Map(rei_dcid,
                                      nameJoinColumn="country",
                                      joinCode="NAME" )


mapParams <- rworldmap::mapCountryData(map,
                                       nameColumnToPlot='average_severity',
                                       catMethod = "categorical",
                                       #catMethod = c(0, 0.4, 0.8, 1.2, 1.6, 2),
                                       mapTitle = "\n\nRussian attack intensity (1994-2017)",
                                       addLegend = FALSE,
                                       xlim = c(-157, 50),
                                       ylim = c(30,50)
)
do.call(rworldmap::addMapLegendBoxes,
        c(mapParams,
          horiz = FALSE,
          title = "Severity",
          cex = 1
          #legendLabels = "all"
        )
)
