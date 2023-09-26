library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(clipr)

setwd('H:/Text/Paper_2023/Sasha/r_calc_snailmovent/Data')
snail_move <- read_excel("snail_movement_2023.xlsx", na = "NA")

snail_move$Speed <- snail_move$Trek_lenght/snail_move$active_time


snail_move %>% 
  filter(Barbarity == "Barbar") %>% 
  ggplot(., aes(y = Speed, x = Species, fill = Status)) + 
  geom_boxplot() + theme_bw()


snail_move %>% 
  filter(Barbarity == "Barbar") %>% 
  ggplot(., aes(y = Squares, x = Species, fill = Status)) + 
  geom_boxplot() + theme_bw()

snail_move %>% 
  filter(Barbarity == "Barbar") %>% 
  ggplot(., aes(y = Sd, x = Species, fill = Status)) + 
  geom_boxplot() + theme_bw()


snail_move %>% 
  filter(Barbarity == "Barbar") %>% 
  ggplot(., aes(y = Trek_lenght, x = Species, fill = Status)) + 
  geom_boxplot() + theme_bw()


snail_move %>% 
  filter(Barbarity == "Barbar") %>% 
  ggplot(., aes(x = Pell, y = Squares/active_time, color = Status)) + 
  geom_point() + 
  facet_wrap(~Species) +
  theme_bw() +
  geom_smooth(method = "lm")






Litt <- snail_move[snail_move$Species == "Littorina",]
Hydr <- snail_move[snail_move$Species == "Hydrobia",]

Barbar <- snail_move[snail_move$Barbarity == "Barbar",]
Domestic <- snail_move[snail_move$Barbarity == "domestic",]

Barbar_Litt <- Litt[Litt$Barbarity == "Barbar",]
Barbar_Hydr <- Hydr[Hydr$Barbarity == "Barbar",]
Domestic_Litt <- Litt[Litt$Barbarity == "domestic",]
Domestic_Hydr <- Hydr[Hydr$Barbarity == "domestic",]

ggplot(snail_move, aes(y = Turn_time, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(snail_move, aes(y = Trek_lenght, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(snail_move, aes(y = Sd, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(snail_move, aes(y = Squares, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

#########################################################################################

ggplot(Litt, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Hydr, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Domestic, aes(y = Turn_time, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar, aes(y = Turn_time, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic, aes(y = Trek_lenght, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar, aes(y = Trek_lenght, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic, aes(y = Sd, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar, aes(y = Sd, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic, aes(y = Squares, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar, aes(y = Squares, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar_Litt, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Domestic_Litt, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Barbar_Hydr, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Domestic_Hydr, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

#########################################################################################

model_Turn_time_Species_Status <- lm(Turn_time ~ Species * Status, data = snail_move)
summary(model_Turn_time_Species_Status)

model_Trek_lenght_Species_Status <- lm(Trek_lenght ~ Species * Status, data = snail_move)
summary(model_Trek_lenght_Species_Status)

model_Sd_Species_Status <- lm(Sd ~ Species * Status, data = snail_move)
summary(model_Sd_Species_Status)

model_Squares_Species_Status <- lm(Squares ~ Species * Status, data = snail_move)
summary(model_Squares_Species_Status)

#########################################################################################

model_Squares_size_Status_Litt <- lm(Squares ~ Aperture_size * Status, data = Litt)
summary(model_Squares_size_Status_Litt)

model_Squares_size_Status_Hydr <- lm(Squares ~ Aperture_size * Status, data = Hydr)
summary(model_Squares_size_Status_Hydr)

Cf <- t.test(Turn_time ~ Species, data = snail_move)
summary(Cf)