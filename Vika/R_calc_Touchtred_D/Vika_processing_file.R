library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)



myt <- read_excel("Data/TouchTrEd_D_2023.xlsx", na = "NA")

myt <- myt %>% filter(!is.na(Supposed_Morph_A))

myt <- myt %>% mutate(Reciproc = ifelse(Reciprocal_threads == "0", "No", "Present"), Total_Bys_A =  To_Substr_A + To_mate_A, Total_Bys_B = To_Substr_B + To_mate_B, Prop_to_Mate_A = To_mate_A/Total_Bys_A, Prop_to_Mate_B = To_mate_B/Total_Bys_B)

myt <- myt %>% filter(Experiment_Type == "Field")

nrow(myt)

myt<-
  myt %>% mutate(Group = case_when(
    Morph_A == "t" & Morph_B == "t" ~ "Homo_T",
    Morph_A == "t" & Morph_B == "e" ~ "Homo_E",
    (Morph_A == "e" & Morph_B == "t")|
      (Morph_A == "t" & Morph_B == "e") ~ "Hetero"
  ))


myt <-
  myt %>% mutate(Pair_Type = case_when(Morph_A == "t" & Morph_B == "t" ~ "TT",
                                        Morph_A == "e" & Morph_B == "e" ~ "EE",
                                        Morph_A == "t" & Morph_B == "e" |  Morph_A == "e" & Morph_B == "t"  ~ "ET") )



myt2 <-
myt %>% filter(!((Supposed_Morph_A != Morph_A)|(Supposed_Morph_B != Morph_B)) )

nrow(myt2)

myt2 %>%
  group_by(Morph_A, Morph_B) %>%
  summarise(Prop_Reciproc = mean(Reciproc == "Present"))

myt2 <-
myt2 %>% mutate(Pair_Type = case_when(Morph_A == "t" & Morph_B == "t" ~ "TT",
                                      Morph_A == "e" & Morph_B == "e" ~ "EE",
                                      Morph_A == "t" & Morph_B == "e" |  Morph_A == "e" & Morph_B == "t"  ~ "ET") )


myt2 %>%
  group_by(Pair_Type) %>%
  summarise(Prop_Reciproc = mean(Reciproc == "Present"))





Pl_A <-
ggplot(myt2, aes(x = Morph_A, y = Total_Bys_A, fill = Morph_B)) +
  geom_boxplot() + guides(fill = "none")

Pl_B <-
ggplot(myt2, aes(x = Morph_B, y = Total_Bys_B, fill = Morph_A)) +
  geom_boxplot()+ guides(fill = "none")

plot_grid(Pl_A, Pl_B)

Pl_A_prop <-
  ggplot(myt2, aes(x = Morph_A, y = Prop_to_Mate_A, fill = Morph_B)) +
  geom_boxplot() + guides(fill = "none")




Pl_B_prop <-
  ggplot(myt2, aes(x = Morph_B, y = Prop_to_Mate_B, fill = Morph_A)) +
  geom_boxplot()+ guides(fill = "none")

plot_grid(Pl_A_prop, Pl_B_prop)




myt2 <- myt2 %>% mutate(Dif_Bys = abs(Total_Bys_A - Total_Bys_B))

ggplot(myt2, aes(x = Reciproc, y = Dif_Bys)) +
  geom_boxplot(varwidth = T)+ guides(fill = "none") +
  facet_grid(Morph_A ~ Morph_B)


myt2<-
myt2 %>% mutate(Group = case_when(
  Morph_A == "t" & Morph_B == "t" ~ "Homo_T",
  Morph_A == "t" & Morph_B == "e" ~ "Homo_E",
  (Morph_A == "e" & Morph_B == "t")|
    (Morph_A == "t" & Morph_B == "e") ~ "Hetero"
))




ggplot(myt2, aes(x = Reciproc, y = Dif_Bys)) +
  geom_boxplot(varwidth = T)+ guides(fill = "none") +
  facet_wrap( ~ Group)


ggplot(myt2, aes(x = Group, y = Dif_Bys)) +
  geom_boxplot(varwidth = T)+ guides(fill = "none")




ggplot(myt2, aes(x = Group, y = (Prop_to_Mate_A + Prop_to_Mate_B)/2 )) +
  geom_boxplot(varwidth = T)+ guides(fill = "none")




myt <-
myt %>% mutate(Prop_to_Mate_Total = (To_mate_A + To_mate_B)/(To_mate_A + To_mate_B + To_Substr_A + To_Substr_B), to_Mate_Total =  To_mate_A + To_mate_B )

myt2 <-
  myt2 %>% mutate(Prop_to_Mate_Total = (To_mate_A + To_mate_B)/(To_mate_A + To_mate_B + To_Substr_A + To_Substr_B), to_Mate_Total =  To_mate_A + To_mate_B)



ggplot(myt2, aes(x = Pair_Type, y = Prop_to_Mate_Total)) +
  geom_boxplot()



ggplot(myt2, aes(x = Pair_Type, y = Prop_to_Mate_Total)) +
  geom_boxplot()

ggplot(myt2, aes(x = Pair_Type, y = to_Mate_Total)) +
  geom_boxplot()

myt <- myt %>% mutate(Out = ifelse(Reciproc == "Present", 1, 0))


M <- glm(Out ~ Pair_Type, data = myt, family = "binomial")
summary(M)
drop1(M)


M2 <- glm(Out ~ to_Mate_Total, data = myt, family = "binomial")
summary(M2)
drop1(M2)



ggplot(myt2, aes(x = Reciproc, y = to_Mate_Total)) +
  geom_boxplot()



