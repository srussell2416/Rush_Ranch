#Rush Ranch Exploration
library(tidyverse)
library(lubridate)
library(bigleaf)
data <- read.csv("~/Documents/MS_Thesis/USSrr.csv")
names(data)

data <- data %>%
  select(-Year, - Hour) %>%
  separate(TIMESTAMP_END, into = c("Year", "Month", "Day", "Hour", "Minute"), sep = c(4, 6, 8, 10)) %>%
  mutate(datetime = paste0(Year,"-", Month, "-", Day," ", Hour, ":", Minute, ":00"),
         datetime = ymd_hms(datetime),
         Month = as.numeric(Month),
         Year = as.numeric(Year),
         DOY2 = if_else(DOY >= 91, DOY - 90, DOY + 275)
         #GPP_DT = umolCO2.to.gC(GPP_DT),
         #RECO_DT = umolCO2.to.gC(RECO_DT)
         )
data$Year[data$Month < 04 & data$Year == 2014] <- "2013-2014"
data$Year[data$Month >= 04 & data$Year == 2014] <- "2014-2015"
data$Year[data$Month < 04 & data$Year == 2015] <- "2014-2015"
data$Year[data$Month >= 04 & data$Year == 2015] <- "2015-2016"
data$Year[data$Month < 04 & data$Year == 2016] <- "2015-2016"
data$Year[data$Month >= 04 & data$Year == 2016] <- "2016-2017"
data$Year[data$Month < 04 & data$Year == 2017] <- "2016-2017"

#Comparing NEE to NEE = RECO - GPP
data %>%
  select(datetime, DOY2, GPP_DT, RECO_DT, NEE, Year, Month) %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(calc_NEE = RECO_DT - GPP_DT,
         cum_NEE = cumsum(calc_NEE),
         cum_NEE2 = cumsum(NEE)) %>%
  select(datetime, DOY2, Year, cum_NEE, cum_NEE2) %>%
  pivot_longer(c(cum_NEE, cum_NEE2), names_to = "NEE") %>%
  ggplot(aes(x = DOY2, y = value, color = NEE, linetype = Year)) +
  geom_line() +
  theme_bw()

#Yearly cumulative NEE
data %>%
  select(datetime, DOY2, GPP_NT, RECO_NT, NEE, Year, Month) %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(calc_NEE = RECO_NT - GPP_NT,
         cum_NEE = cumsum(calc_NEE)) %>%
  select(datetime, DOY2, Year, cum_NEE) %>%
  ggplot(aes(x = DOY2, y = cum_NEE, color = Year)) +
  ylab("Cumulative NEE( µmol CO2 m-2 s-1)") +
  xlab("Days since April 1st") +
  geom_line() +
  theme_bw()

#Comparing GPP to NEE
data %>%
  select(datetime, DOY2, GPP_DT, RECO_DT, Year, Month) %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(cum_GPP = cumsum(GPP_DT),
         cum_GPP = -cum_GPP,
         cum_RECO = cumsum(RECO_DT))%>%
  pivot_longer(c(cum_GPP, cum_RECO), names_to = "flux") %>%
ggplot(aes(DOY2, value, color = Year, linetype = flux)) +
  geom_line() +
  ylab("Cumulative CO2 Flux (µmol CO2 m-2 s-1)") +
  xlab("Days since April 1st") +
  theme_bw()

#

data <- read.csv("~/Documents/MS_Thesis/USSrr_v2.csv")

