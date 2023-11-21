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

ggplot(snail_move, aes(y = Speed, x = Species, fill = Status)) + geom_boxplot() + theme_bw()

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

ggplot(Barbar_Litt, aes(y = Trek_lenght, x = Species, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Domestic_Litt, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Barbar_Hydr, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Domestic_Hydr, aes(y = Trek_lenght, x = Aperture_size, fill = Status)) + geom_point() + theme_bw() + geom_smooth(method = "lm")

ggplot(Litt, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Hydr, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic_Litt, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic_Hydr, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar_Litt, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar_Hydr, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Barbar, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

ggplot(Domestic, aes(y = Speed, x = Aperture_size, fill = Status)) + geom_boxplot() + theme_bw()

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

model_Turn_time_Status_Litt <- lm(Turn_time ~ Aperture_size * Status, data = Litt)
summary(model_Turn_time_size_Status_Litt)

model_Turn_time_Status_Hydr <- lm(Turn_time ~ Aperture_size * Status, data = Hydr)
summary(model_Turn_time_size_Status_Hydr)

model_Sd_Status_Litt <- lm(Sd ~ Aperture_size * Status, data = Litt)
summary(model_Sd_size_Status_Litt)

model_Sd_Status_Hydr <- lm(Sd ~ Aperture_size * Status, data = Hydr)
summary(model_Sd_size_Status_Hydr)

model_Trek_lenght_Status_Litt <- lm(Trek_lenght ~ Aperture_size * Status, data = Litt)
summary(model_Trek_lenght_size_Status_Litt)

model_Trek_lenght_Status_Hydr <- lm(Trek_lenght ~ Aperture_size * Status, data = Hydr)
summary(model_Trek_lenght_size_Status_Hydr)

##########################################################################################

TurnSp <- t.test(Turn_time ~ Species, data = snail_move)
summary(TurnSp)

TrekSp <- t.test(Trek_lenght ~ Species, data = snail_move)
summary(TrekSp)

SdSp <- t.test(Sd ~ Species, data = snail_move)
summary(SdSp)

SquaresSp <- t.test(Squares ~ Species, data = snail_move)
summary(SquaresSp)

##########################################################################################

TurnSt <- t.test(Turn_time ~ Status, data = snail_move)
summary(TurnSt)

TrekSt <- t.test(Trek_lenght ~ Status, data = snail_move)
summary(TrekSt)

SdSt <- t.test(Sd ~ Status, data = snail_move)
summary(SdSt)

SquaresSt <- t.test(Squares ~ Status, data = snail_move)
summary(SquaresSt)

####################################################################
setwd('H:/Text/Paper_2023/Sasha/r_calc_snailmovent/Data')
snail_move2 <- read_excel("snail_movement_2023_lenght.xlsx", na = "NA")

Litt2 <- snail_move2[snail_move2$Species == "Littorina",]
Hydr2 <- snail_move2[snail_move2$Species == "Hydrobia",]

ggplot(Litt2, aes(y = Value, x = Status, fill = Status)) + geom_boxplot() + theme_bw() + facet_wrap(~Trait,scales = "free_y")

ggplot(Hydr2, aes(y = Value, x = Status,  fill = Status)) + geom_boxplot() + theme_bw() + facet_wrap(~Trait,scales = "free_y")

ggplot(Barbar, aes(y = Sd, x = Species, fill = Status)) + geom_boxplot() + theme_bw() 

ggplot(snail_move2, aes(y = Sd, x = Species, fill = Status)) + geom_boxplot() + theme_bw() 

model_hydr2_sd <- aov(Sd ~ Aperture_size * Status, data = Barbar_Litt)

summary(model_hydr2_sd)

ggplot(Barbar, aes(y = Speed, x = Aperture_size, fill = Status, color = Status)) + geom_point() + geom_smooth(method = 'lm') + theme_bw()

ggplot(Barbar, aes(y = Squares, x = Aperture_size, fill = Status, color = Status)) + geom_point() + theme_bw()  + geom_smooth(method = 'lm') 

