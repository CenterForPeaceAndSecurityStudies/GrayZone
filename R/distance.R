# Add distance covariates - table 2
library(cshapes)

## Create dataframe of distances by year that matches the nukes dataset
###DO NOT RUN###
GW <- FALSE
disttype <- "capdist"

result <- distlist(as.Date("1994-1-1"), type = disttype, useGW = GW)
result <- result[result$ccode1 < result$ccode2,]
result$year <- 1994

for (year in 1994:2016) {
  date_current <- paste(year, "1", "1", sep="-")
  result_current <- distlist(as.Date(date_current), type = disttype, useGW = GW)
  result_current$year <- year
  result <- rbind(result, result_current)
}

write.table(result, "distance.txt", row.names = FALSE)
###END DO NOT RUN###

distance <- read.table(here::here("Data/distance.txt"), header = TRUE, as.is = TRUE)
