library(ggplot2)
library(readxl)
myt <- read_excel("Data/As_Tr_Ed_2023.xlsx")

sum(myt$Doubt)

mean(myt$Doubt == 0)

myt[10, ]
myt[ , c(1,4)]
myt[c(1,10), 1:3 ]

myt_full <- myt[myt$Doubt == 0, ]

ggplot(data = myt_full, mapping = aes(x = Morphotype, y = L, fill = Status)) +
  geom_boxplot() + 
  facet_grid(Type~ Morphotype_in_Pure)

library(dplyr)

dead <-
myt_full %>% 
  group_by(Morphotype_in_Pure, Cage, Type, Morphotype) %>% 
  summarise(Prop_dead = mean(Status == "мертв"))

ggplot(data = dead, mapping = aes(x = Morphotype, y = Prop_dead)) +
  geom_boxplot() + 
  facet_grid(Type~ Morphotype_in_Pure)



ggplot(data = dead, mapping = aes(x = Morphotype_in_Pure, y = Prop_dead, fill = Type)) +
  geom_boxplot() 






# Здесь видно процент мертвых мидий в зависимости от морфотипа и вида садка

dead1 <-
myt_full %>% 
  group_by(Cage) %>% 
  summarise(Prop_T = mean(Morphotype == "T"))


dead2 <-
  myt_full %>% 
  filter(Type == "mix") %>% 
  group_by(Cage, Morphotype) %>% 
  summarise(Prop_dead = mean(Status == "мертв"))

dead3 <-
merge(dead1, dead2)


dead4 <-
  myt_full %>% 
  group_by(Cage, Type) %>% 
  summarise(Prop_dead = mean(Status == "мертв"))


ggplot(data = dead3, mapping = aes(x = Prop_T, y = Prop_dead, color = Morphotype)) +
  geom_point()+
  geom_smooth(method = "lm")





###Распределение звезд по садкам
astr <- read_excel("Data/As_Tr_Ed_2023.xlsx", sheet = 'Data_Asterias')

astr %>% 
  group_by(Cage, Type, Morphotype_in_Pure) %>% 
  summarise(Nastr = n()) %>% 
  ggplot(aes(x = Type, y = Nastr, fill = Morphotype_in_Pure))+
  geom_boxplot()

astr_w <-
astr %>% 
  group_by(Cage, Type, Morphotype_in_Pure) %>% 
  summarise(Nastr = n())


astr_dead <- merge(dead4, astr_w)

ggplot(data = astr_dead, aes(x = Nastr, y = Prop_dead))+
  geom_point()+
  facet_wrap(~Type)



myt_full %>% 
  ggplot(aes(x = Type, y = L, fill = Status)) +
  geom_boxplot() + 
  facet_grid(Morphotype_in_Pure~Morphotype)


myt_full %>% 
  ggplot(aes(x = Type, y = L, fill = Status)) +
  geom_boxplot() + 
  facet_wrap(~Morphotype_in_Pure)


Mod <- lm(L ~ Status * Type * Morphotype, data = myt_full)

anova(Mod)

plot(Mod)

drop1(Mod)
Mod2 <- update(Mod, .~.-Status:Type:Morphotype)

drop1(Mod2)
Mod3 <- update(Mod2, .~.-Status:Type)

drop1(Mod3)
Mod4 <- update(Mod3, .~.-Type:Morphotype)

drop1(Mod4)

anova(Mod4)


myt_full %>% 
  ggplot(aes(x = Type, y = L, fill = Status)) +
  geom_boxplot() + 
  facet_wrap(~Morphotype)


myt_full$Out <- ifelse(myt_full$Status == "мертв", 1, 0)

Mod_prop_dead <- glm(Out ~ Type * Morphotype*L*Morphotype_in_Pure, data = myt_full)

anova(Mod_prop_dead, test = "Chi")

drop1(Mod_prop_dead)
Mod_prop_dead2 <- update(Mod_prop_dead, .~. -Type:Morphotype:L:Morphotype_in_Pure )

drop1(Mod_prop_dead2)
Mod_prop_dead3 <- update(Mod_prop_dead2, .~. -Type:L:Morphotype_in_Pure )

drop1(Mod_prop_dead3)
Mod_prop_dead4 <- update(Mod_prop_dead3, .~. -Type:Morphotype:L )

drop1(Mod_prop_dead4)
Mod_prop_dead5 <- update(Mod_prop_dead4, .~. -Type:L )

drop1(Mod_prop_dead5)
Mod_prop_dead6 <- update(Mod_prop_dead5, .~. -Morphotype:L:Morphotype_in_Pure  )

drop1(Mod_prop_dead6)
Mod_prop_dead7 <- update(Mod_prop_dead6, .~. -L:Morphotype_in_Pure  )

drop1(Mod_prop_dead7)
Mod_prop_dead8 <- update(Mod_prop_dead7, .~. -Type:Morphotype:Morphotype_in_Pure)

drop1(Mod_prop_dead8)
Mod_prop_dead9 <- update(Mod_prop_dead8, .~. -Morphotype:Morphotype_in_Pure)

drop1(Mod_prop_dead9)
