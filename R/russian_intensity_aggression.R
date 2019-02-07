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

years <- 1994:2018
severity_avg_annual <- c()
numint <- c()

for(yr in years){
  temp <- gac[which(gac$year_start==yr),]
  if(nrow(temp)>0){
    avg <- mean(temp$score)
    numint <- c(numint,nrow(temp))
    severity_avg_annual <- c(severity_avg_annual, avg)
  }
  else{
    numint <- c(numint,0)
    severity_avg_annual <- c(severity_avg_annual, 0)
  }
}

df <- cbind(years,severity_avg_annual,numint)
df <- as.data.frame(df)

#### bar chart
g <- ggplot(df, aes(years, severity_avg_annual)) +
  geom_bar(stat="identity", width = 1, fill="gray", color = "black") +
  geom_line(aes(y = numint/4)) +
  labs(title="Average intensity of Russian aggression (1994-2018)",
       subtitle="", x = "Year", y="Average Intensity") +
  scale_x_continuous(breaks = c(1994:2018)) +
  scale_y_continuous(sec.axis = sec_axis(~.0:20,name = "Number of Russian Interventions"),
                     breaks = c(1:5), labels = c("1" = "1 Info ops",
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

years <- 1994:2018
severity_max_annual <- c()
numint_max <- c()

for(yr in years){
  temp <- gac[which(gac$year_start==yr),]
  if(nrow(temp)>0){
    max <- max(temp$score)
    numint_max <- c(numint_max,nrow(temp))
    severity_max_annual <- c(severity_max_annual, max)
  }
  else{
    numint_max <- c(numint_max,0)
    severity_max_annual <- c(severity_max_annual, 0)
  }
}

df_max <- cbind(years,severity_max_annual,numint_max)
df_max <- as.data.frame(df_max)


#### bar chart
g <- ggplot(df_max, aes(years, severity_max_annual)) +
  geom_bar(stat="identity", width = 1, fill="gray", color = "black") +
  geom_line(aes(y = numint_max/4)) +
  labs(title="Maximum intensity of Russian aggression (1994-2018)",
       subtitle="", x = "Year", y="Maximum Intensity") +
  scale_x_continuous(breaks = c(1994:2018)) +
  scale_y_continuous(sec.axis = sec_axis(~.0:20,name = "Number of Russian Interventions"),
                     breaks = c(1, 2, 3, 4, 5), labels = c("1" = "1 Info ops",
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

