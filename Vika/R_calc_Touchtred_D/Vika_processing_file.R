library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)



myt <- read_excel("Data/TouchTrEd_D_2023.xlsx", na = "NA")

myt <- myt %>% filter(!is.na(Supposed_Morph_A))

myt <- myt %>% mutate(Reciproc = ifelse(Reciprocal_threads == "0", "No", "Present"), Total_Bys_A =  To_Substr_A + To_mate_A, Total_Bys_B = To_Substr_B + To_mate_B)

myt %>%
  group_by(Supposed_Morph_A, Supposed_Morph_B) %>%
  summarise(Prop_Reciproc = mean(Reciproc == "Present"))

Pl_A <-
ggplot(myt, aes(x = Supposed_Morph_A, y = Total_Bys_A, fill = Supposed_Morph_B)) +
  geom_boxplot() + guides(fill = "none")

Pl_B <-
ggplot(myt, aes(x = Supposed_Morph_B, y = Total_Bys_B, fill = Supposed_Morph_A)) +
  geom_boxplot()+ guides(fill = "none")

plot_grid(Pl_A, Pl_B)

myt <- myt %>% mutate(Dif_Bys = abs(Total_Bys_A - Total_Bys_B))

ggplot(myt, aes(x = Reciproc, y = Dif_Bys)) +
  geom_boxplot(varwidth = T)+ guides(fill = "none") +
  facet_grid(Supposed_Morph_A ~ Supposed_Morph_B)



myt %>% mutate(Group = case_when(
  Supposed_Morph_A == "PT" & Supposed_Morph_B == "PT" ~ "Homo_PT",
  Supposed_Morph_A == "PE" & Supposed_Morph_B == "PE" ~ "Homo_PE",
  (Supposed_Morph_A == "PE" & Supposed_Morph_B == "PT")|
    (Supposed_Morph_A == "PT" & Supposed_Morph_B == "PE") ~ "Hetero"
))





