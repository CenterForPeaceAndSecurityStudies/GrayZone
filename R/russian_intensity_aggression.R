gac <- readRDS("~/GrayZone/data/grayzone_aggregate_cpass.rds")
#plotting the severity of Russian attacks from 1994-2018.
library(ggplot2)

gac <- gac[c("target","year_start","resp_convmil_gro","resp_convmil_airsea",
            "resp_paramil","resp_cyberdisrup","resp_infoops")]

severity <- c()
for(i in 1:nrow(gac)){
  score <- 0
  temp <- gac[i,]
  if(!is.na(temp[3]) & temp[3]==1){ #resp_convmil_gro=5
    score <- 5
  }
  else if(!is.na(temp[4]) & temp[4]==1){ #resp_convmil_airsea=4
    score <- 4
  }
  else if(!is.na(temp[5]) & temp[5]==1){ #resp_paramil=3
    score <- 3
  }
  else if(!is.na(temp[6]) & temp[6]==1){ #resp_cyber_disrup=2
    score <- 2
  }
  else if(!is.na(temp[7]) & temp[7]==1){ #resp_infoops=1
    score <- 1
  }

  severity <- c(severity,score)
}

gac$score <- severity

years <- sort(unique(gac$year_start))
severity_avg_annual <- c()

for(yr in years){
  temp <- gac[which(gac$year_start==yr),]
  avg <- mean(temp$score)
  severity_avg_annual <- c(severity_avg_annual, avg)
}

df <- cbind(years,severity_avg_annual)
df <- as.data.frame(df)

#geom_text(label = "Info Ops", y = 1, x = 1994) +
#geom_text(label = "Cyber Disrup.", y = 2, x = 1994) +
#geom_text(label = "Paramil.", y = 3, x = 1994) +
#geom_text(label = "Mil. Air/Sea", y = 4, x = 1994) +
#geom_text(label = "Mil. Ground", y = 5, x = 1994) +


#### bar chart
g <- ggplot(df, aes(years, severity_avg_annual)) +
  geom_bar(stat="identity", width = 1, fill="gray", color = "black") +
  labs(title="Average intensity of Russian aggression (1994-2018)",
       subtitle="", x = "Year", y="Average Intensity") +
  scale_x_continuous(breaks = c(1994:2018)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("1" = "1 Info ops",
                                                           "2" = "2 Cyber Disrup.",
                                                           "3" = "3 Paramil.",
                                                           "4" = "4 Mil (Air/sea)",
                                                           "5" = "5 Mil (Gro)")) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(),
        title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 90,vjust = 0.5,
                                   hjust = 0.9),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)
        )

g

#ggsave(paste0("average_intensity_russian_aggression.jpeg"),g,"jpeg",
#       path = paste0(here::here(), '/paper/figures/'), limitsize = FALSE)


####################### by MAX intensity

years <- sort(unique(gac$year_start))
severity_max_annual <- c()

for(yr in years){
  temp <- gac[which(gac$year_start==yr),]
  max <- max(temp$score)
  severity_max_annual <- c(severity_max_annual, max)
}

df_max <- cbind(years,severity_max_annual)
df_max <- as.data.frame(df_max)


#### bar chart
g <- ggplot(df_max, aes(years, severity_max_annual)) +
  geom_bar(stat="identity", width = 1, fill="gray", color = "black") +
  labs(title="Maximum intensity of Russian aggression (1994-2018)",
       subtitle="", x = "Year", y="Maximum Intensity") +
  scale_x_continuous(breaks = c(1994:2018)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("1" = "1 Info ops",
                                                           "2" = "2 Cyber Disrup.",
                                                           "3" = "3 Paramil.",
                                                           "4" = "4 Mil (Air/sea)",
                                                           "5" = "5 Mil (Gro)")) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        title = element_text(size = 18),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(size = 18, angle = 90,vjust = 0.5,
                                   hjust = 0.9),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)
  )

g

#ggsave(paste0("maximum_intensity_russian_aggression.jpeg"),g,"jpeg",path = paste0(here::here(), '/paper/figures/'), limitsize = FALSE)

