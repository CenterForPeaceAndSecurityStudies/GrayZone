
<!-- rnb-text-begin -->

---
title: "07_FullData"
author: J Andres Gannon, Erik Gartzke, and Jon Lindsay, Center for Peace and Security
  Studies (cPASS)
date: "2019-06-16"
output:
  html_document:
    df_print: paged
    toc: yes
    toc_depth: '5'
  html_notebook:
    fig_height: 8
    fig_width: 12
    number_sections: yes
    theme: flatly
    toc: yes
    toc_depth: 5
    toc_float:
      collapsed: yes
editor_options:
  chunk_output_type: inline
---


<!-- rnb-text-end -->



<!-- rnb-text-begin -->


This document visualizes the geographic relationship between deterrence and the severity of Russian cyber operations.

Research assistance for this rmd provided by Cole Reynolds.

# Preparation
## Load packages
Pipe operators have trouble loading for individual commands

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGlicmFyeShtYWdyaXR0cilcbmxpYnJhcnkoZ2dwbG90MilcbmBgYCJ9 -->

```r
library(magrittr)
library(ggplot2)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Load data
Load the cleaned and subset versions of each dataset

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBMb2FkIGRhdGFcbnJ1c3NpYV9jeWJlciA8LSByZWFkUkRTKHBhc3RlMChoZXJlOjpoZXJlKCksICcvZGF0YS9ncmF5em9uZV9hZ2dyZWdhdGUucmRzJykpXG5gYGAifQ== -->

```r
# Load data
russia_cyber <- readRDS(paste0(here::here(), '/data/grayzone_aggregate.rds'))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Check data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyeShydXNzaWFfY3liZXIpXG5gYGAifQ== -->

```r
summary(russia_cyber)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Prior data maps
Map the data using prior variable codings from other datasets

## DCID
Map of the DCID cases showing variation in severity

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGNpZCA8LSByZWFkUkRTKGZpbGUgPSBwYXN0ZTAoaGVyZTo6aGVyZSgpLFwiL2RhdGEvZ3JheXpvbmVfZGNpZC5yZHNcIikpXG5cbmNvdW50cnkgPC0gdW5pcXVlKGRjaWQkVGFyZ2V0KVxuYXZlcmFnZV9zZXZlcml0eSA8LSBOVUxMIFxuZm9yKCBpIGluIDE6bGVuZ3RoKGNvdW50cnkpKXtcbiAgYXZlcmFnZV9zZXZlcml0eVtpXSA8LSByb3VuZChtZWFuKGRjaWRbZGNpZCRUYXJnZXQgPT0gY291bnRyeVtpXSwgXCJJbmNpZGVudCBzZXZlcml0eVwiXSksIDIpXG59XG5cbmRjaWQgPC0gYXMuZGF0YS5mcmFtZShjYmluZChjb3VudHJ5LCBhdmVyYWdlX3NldmVyaXR5KSwgc3RyaW5nc0FzRmFjdG9ycyA9IEYpXG5kY2lkJGF2ZXJhZ2Vfc2V2ZXJpdHkgPC0gYXMubnVtZXJpYyhkY2lkJGF2ZXJhZ2Vfc2V2ZXJpdHkpXG5cbm1hcCA8LSByd29ybGRtYXA6OmpvaW5Db3VudHJ5RGF0YTJNYXAoZGNpZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbmFtZUpvaW5Db2x1bW49XCJjb3VudHJ5XCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGpvaW5Db2RlPVwiTkFNRVwiIClcblxucndvcmxkbWFwOjptYXBEZXZpY2UoKVxuXG5tYXBQYXJhbXMgPC0gcndvcmxkbWFwOjptYXBDb3VudHJ5RGF0YShtYXAsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lQ29sdW1uVG9QbG90PSdhdmVyYWdlX3NldmVyaXR5JyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhdE1ldGhvZCA9IGMoMiwgMi4zLCAyLjYsIDIuOSwgMy4yLCAzLjUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFwVGl0bGUgPSBcIlxcblJ1c3NpYW4gY3liZXIgYXR0YWNrIHNldmVyaXR5ICgyMDA1LTIwMTYpIC0gVmFsZXJpYW5vIGFuZCBNYW5lc3MgZGF0YVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYWRkTGVnZW5kID0gRkFMU0UsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB4bGltID0gYygtMTY3LCA1MCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB5bGltID0gYygzMCw1MClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKVxuZG8uY2FsbChyd29ybGRtYXA6OmFkZE1hcExlZ2VuZEJveGVzLFxuICAgICAgICBjKG1hcFBhcmFtcyxcbiAgICAgICAgICB0aXRsZSA9IFwiU2V2ZXJpdHlcIlxuICAgICAgICAgIClcbiAgICAgICAgKVxuXG50ZXh0KHggPSAtMTI3LCB5ID0gLTIwLCBsYWJlbHMgPSBjKFwiRENJRCBkYXRhXCIpLCBjZXggPSAxKVxuXG5cbmBgYCJ9 -->

```r
dcid <- readRDS(file = paste0(here::here(),"/data/grayzone_dcid.rds"))

country <- unique(dcid$Target)
average_severity <- NULL 
for( i in 1:length(country)){
  average_severity[i] <- round(mean(dcid[dcid$Target == country[i], "Incident severity"]), 2)
}

dcid <- as.data.frame(cbind(country, average_severity), stringsAsFactors = F)
dcid$average_severity <- as.numeric(dcid$average_severity)

map <- rworldmap::joinCountryData2Map(dcid,
                                      nameJoinColumn="country",
                                      joinCode="NAME" )

rworldmap::mapDevice()

mapParams <- rworldmap::mapCountryData(map,
                                       nameColumnToPlot='average_severity',
                                       catMethod = c(2, 2.3, 2.6, 2.9, 3.2, 3.5),
                                       mapTitle = "\nRussian cyber attack severity (2005-2016) - Valeriano and Maness data",
                                       addLegend = FALSE,
                                       xlim = c(-167, 50),
                                       ylim = c(30,50)
                                      )
do.call(rworldmap::addMapLegendBoxes,
        c(mapParams,
          title = "Severity"
          )
        )

text(x = -127, y = -20, labels = c("DCID data"), cex = 1)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## REI
Map of the REI cases showing variation in severity. Info ops are coded as least severe, cyber disruption as moderate, and material support as most severe. Code each case by whatever is the most severe level that is present.

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucmVpIDwtIHJlYWRSRFMoZmlsZSA9IHBhc3RlMChoZXJlOjpoZXJlKCksXCIvZGF0YS9ncmF5em9uZV9yZWkucmRzXCIpKVxuXG5yZWlbcmVpJFRhcmdldCA9PSBcIlVrcmFpbmUgXCIsIFwiVGFyZ2V0XCJdIDwtIFwiVWtyYWluZVwiXG5cbmNvdW50cnkgPC0gdW5pcXVlKHJlaSRUYXJnZXQpXG5hdmVyYWdlX3NldmVyaXR5IDwtIE5VTEwgXG5mb3IoIGkgaW4gMTpsZW5ndGgoY291bnRyeSkpe1xuICBhdmVyYWdlX3NldmVyaXR5W2ldIDwtIHJvdW5kKG1lYW4ocmVpW3JlaSRUYXJnZXQgPT0gY291bnRyeVtpXSwgXCJGYXZvcmFibGUgb3V0Y29tZVwiXSksIDIpXG59XG5cbnJlaSA8LSBhcy5kYXRhLmZyYW1lKGNiaW5kKGNvdW50cnksIGF2ZXJhZ2Vfc2V2ZXJpdHkpLCBzdHJpbmdzQXNGYWN0b3JzID0gRilcbnJlaSRhdmVyYWdlX3NldmVyaXR5IDwtIGFzLm51bWVyaWMocmVpJGF2ZXJhZ2Vfc2V2ZXJpdHkpXG5cbm1hcCA8LSByd29ybGRtYXA6OmpvaW5Db3VudHJ5RGF0YTJNYXAocmVpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lSm9pbkNvbHVtbj1cImNvdW50cnlcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgam9pbkNvZGU9XCJOQU1FXCIgKVxuXG5yd29ybGRtYXA6Om1hcERldmljZSgpXG5cbm1hcFBhcmFtcyA8LSByd29ybGRtYXA6Om1hcENvdW50cnlEYXRhKG1hcCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5hbWVDb2x1bW5Ub1Bsb3Q9J2F2ZXJhZ2Vfc2V2ZXJpdHknLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY2F0TWV0aG9kID0gYygwLCAwLjQsIDAuOCwgMS4yLCAxLjYsIDIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFwVGl0bGUgPSBcIlxcblJ1c3NpYW4gY3liZXIgYXR0YWNrIHNldmVyaXR5ICgxOTk0LTIwMTcpIC0gV2F5IGFuZCBDYXNleSBkYXRhXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAjY29sb3VyUGFsZXR0ZSA9IGMoXCJsaWdodGdvbGRlbnJvZFwiLCBcInllbGxvd1wiLCBcIm9yYW5nZVwiLCBcInJlZFwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGFkZExlZ2VuZCA9IEZBTFNFLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeGxpbSA9IGMoLTE2NywgNTApLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeWxpbSA9IGMoMzAsNTApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIClcbmRvLmNhbGwocndvcmxkbWFwOjphZGRNYXBMZWdlbmRCb3hlcyxcbiAgICAgICAgYyhtYXBQYXJhbXMsXG4gICAgICAgICAgaG9yaXogPSBGQUxTRSxcbiAgICAgICAgICB0aXRsZSA9IFwiU2V2ZXJpdHlcIlxuICAgICAgICAgICNsZWdlbmRMYWJlbHMgPSBcImFsbFwiXG4gICAgICAgICAgKVxuICAgICAgICApXG5cbnRleHQoeCA9IC0xMjgsIHkgPSAtMjAsIGxhYmVscyA9IGMoXCJSRUkgZGF0YVwiKSwgY2V4ID0gMSlcblxuYGBgIn0= -->

```r
rei <- readRDS(file = paste0(here::here(),"/data/grayzone_rei.rds"))

rei[rei$Target == "Ukraine ", "Target"] <- "Ukraine"

country <- unique(rei$Target)
average_severity <- NULL 
for( i in 1:length(country)){
  average_severity[i] <- round(mean(rei[rei$Target == country[i], "Favorable outcome"]), 2)
}

rei <- as.data.frame(cbind(country, average_severity), stringsAsFactors = F)
rei$average_severity <- as.numeric(rei$average_severity)

map <- rworldmap::joinCountryData2Map(rei,
                                      nameJoinColumn="country",
                                      joinCode="NAME" )

rworldmap::mapDevice()

mapParams <- rworldmap::mapCountryData(map,
                                       nameColumnToPlot='average_severity',
                                       catMethod = c(0, 0.4, 0.8, 1.2, 1.6, 2),
                                       mapTitle = "\nRussian cyber attack severity (1994-2017) - Way and Casey data",
                                       #colourPalette = c("lightgoldenrod", "yellow", "orange", "red"),
                                       addLegend = FALSE,
                                       xlim = c(-167, 50),
                                       ylim = c(30,50)
                                      )
do.call(rworldmap::addMapLegendBoxes,
        c(mapParams,
          horiz = FALSE,
          title = "Severity"
          #legendLabels = "all"
          )
        )

text(x = -128, y = -20, labels = c("REI data"), cex = 1)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Combined
There should only be one case that is coded in both DCID and REI. For all the rest, scale their scores so we get a single score for each case in either dataset and we can compare across datasets. We know this is a rough coding since one is coding severity of the cyber attack and the other is coding the type of attack, but we'll start here.


# Current data maps

<!-- rnb-text-end -->

