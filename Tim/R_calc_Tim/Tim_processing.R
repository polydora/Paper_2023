library(dplyr)
library(reshape2)
library(vegan)
library(ggplot2)
library(readxl)



veg <- read_excel("Data/Tim_data.xlsx", sheet = "Фитоценоз 2021")

snail <- read_excel("Data/Tim_data.xlsx", sheet = "Улитки 2021")


df <-
  dcast(Core ~ Species, data = veg, value.var = "Cover")


df[is.na(df)] <- 0


pca_df <- rda(df)

mds_df <- metaMDS(df)

plot(mds_df, type = "t")
summary(pca_df)



plot(pca_df, display = "species")

plot(pca_df, display = "sites")



MDS <- as.data.frame(mds_df$points)

ggplot(MDS, aes(MDS1, MDS2) ) + geom_point(aes(size = snail$Number))



