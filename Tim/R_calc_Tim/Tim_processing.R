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


round(vegdist(df), 2)


unfolding <- function(x, method = "euclidean") {
  n <- nrow(x)
  N <- (n^2 - n)/2
  unfold <- data.frame(i = 1:N, Object_j = NA, Object_k = NA, Distance = NA)
  pos <- 0
  for(i in 1:(n-1)) for(j in (i+1):n) {
    pos <- pos + 1
    unfold$Object_j[pos] <- i
    unfold$Object_k[pos] <- j
  }
  unfold$Distance <- as.vector(vegdist(x, method = method))
  unfold
}



distance <- unfolding(df, method = "bray")

write.table(distance, file = "clipboard", sep = "\t", row.names = F)

pca_df <- rda(df)

mds_df <- metaMDS(df)

plot(mds_df, type = "t")
summary(pca_df)



plot(pca_df, display = "species")

plot(pca_df, display = "sites")



MDS <- as.data.frame(mds_df$points)

ggplot(MDS, aes(MDS1, MDS2) ) + geom_point(aes(size = snail$Number))



