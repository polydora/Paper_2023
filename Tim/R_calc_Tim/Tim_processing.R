library(dplyr)
library(reshape2)
library(vegan)
library(ggplot2)
library(readxl)



veg <- read_excel("Data/Tim_data.xlsx", sheet = "Фитоценоз 2020 2021")

snail <- read_excel("Data/Tim_data.xlsx", sheet = "Улитки 2020 2021")


df <-
  dcast(Sample ~ Species, data = veg, value.var = "Cover")


df[is.na(df)] <- 0


df_2 <- decostand(df[, -1], method = "rank")


mds_df <- metaMDS(df_2)

# mds_df <- rda(df_2)

plot(mds_df, type = "t")


MDS <- as.data.frame(mds_df$points)

MDS_sp <- as.data.frame(scores(mds_df)$species)


MDS$Sample <- df$Sample

MDS$Year <- as.factor(sub("_.*", "", MDS$Sample))

MDS <-
merge(MDS, snail)

ggplot(MDS, aes(MDS1, MDS2) ) +
  geom_point(aes(color = Year, size = log(Abundance+1) ), position = position_jitter(width = 1))+
  geom_text(data = MDS_sp, aes(x = NMDS1, y = NMDS2, label = rownames(MDS_sp)))

df_3 <- df %>% select(-`Голая земля`)
H <- data.frame(H = diversity(df_3[,-1]), Sample = df$Sample, index = "simpson")


H_snail <-
merge(H, snail)

ggplot(H_snail, aes(x = H, y = (Abundance) )) + geom_point() + geom_smooth()


dist_df_3 <- vegdist(df_3[, -1])

plot(hclust(dist_df_3, method = "ward.D"))

clusters <- hclust(dist_df_3, method = "ward.D")

df_3$cluster <- clusters$order

write.table(as.data.frame(df_3), "clipboard", sep = "\t")
