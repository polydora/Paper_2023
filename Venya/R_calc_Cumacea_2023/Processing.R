library(readxl)
library(dplyr)
library(reshape2)



cum <- read_excel("Data/Cumacea_station.xlsx")


cum_2 <- 
cum %>% select(Year, Station, `Diastylis glabra`, `Brachidiastylis resima`)

cum_3 <- 
melt(cum_2, id.vars = c( "Year", "Station"), variable.name = "Species", value.name = "N")

station_means <- 
cum_3 %>% group_by(Station, Species) %>% summarise(N_meaqn = mean(N))


constant_staion <- c("St10", "St11", "St2", "St3", "St4", "St6", "St7", "St8")

library(ggplot2)

cum_2 %>% 
  filter(Station %in% constant_staion) %>%
  group_by(Year) %>% 
  summarise(N_mean_Dg = mean(`Diastylis glabra`), N_mean_Br = mean(`Brachidiastylis resima`)) %>% 
  ggplot(., aes(x = Year, y = log(N_mean_Br+1) )) +
  geom_point() +
  geom_point(aes(y = log(N_mean_Dg+1), color = "blue")) 


