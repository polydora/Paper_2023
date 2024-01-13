# -------- libraries --------
# install.packages("emmeans")
library(reshape2)
library(dplyr)
library(vegan)
library(ggplot2)
library(reshape)
library(boot)
library(emmeans)
library(broom)

# ------- base ---------

myttred <- read.table("Data/Myttred 2023.csv", sep =  ";", header =  TRUE)

length(unique(myttred$N.cont))

myttred <- myttred %>% select(-N.mid)

myttred_PropT <- myttred %>% group_by(N.cont) %>% summarise(PropT = mean(morphotype == "t"))

myttred_N <- myttred %>% group_by(N.cont) %>% filter(status == "alive") %>% summarise(N = n())


myttred2 <- merge(myttred, myttred_PropT)
myttred2 <- merge(myttred2, myttred_N)



myttred2 <- 
  myttred2 %>% 
  mutate(Out = ifelse(status == "dead", 1, 0)) %>% 
  filter(PropT < 1) %>% 
  mutate(type = case_when(PropT < 0.3 ~ "Edom",
                          PropT >= 0.3 & PropT < 0.7 ~ "Mix",
                          PropT >= 0.7 ~ "Tdom")) %>% 
  mutate(pop = case_when(N < 40 ~ 20,
                          N >= 40 & N < 60 ~ 60,
                          N >= 60 ~ 120))

myttred2 %>% group_by(N.cont) %>% 

# ------ графики без обработки -------


# ggplot(myttred2, aes(N, Out)) +
#   geom_smooth(method = "glm",  method.args = list(family=binomial)) +
#   facet_wrap(~morphotype)
# 
# ggplot(myttred2, aes(PropT, Out)) +
#   geom_smooth(method = "glm",  method.args = list(family=binomial)) +
#   facet_wrap(~morphotype)
# 
# ggplot(myttred2, aes(L, Out)) +
#   geom_smooth(method = "glm",  method.args = list(family=binomial)) +
#   facet_wrap(~morphotype)


# -------- linear models & stepwise regression  ---------


Mod <- glm(Out ~ morphotype * N * type * L, data = myttred2, family = "binomial")

drop1(Mod)

Mod2 <- update(Mod, . ~ . - morphotype:N:type:L)
drop1(Mod2)

Mod3 <- update(Mod2, . ~ . - N:type:L)
drop1(Mod3)

Mod4 <- update(Mod3, . ~ . - morphotype:type:L)
drop1(Mod4)

Mod5 <- update(Mod4, . ~ . - morphotype:N:type)
drop1(Mod5)

Mod6 <- update(Mod5, . ~ . - morphotype:N:L)
drop1(Mod6)

Mod7 <- update(Mod6, . ~ . - type:L)
drop1(Mod7)

Mod8 <- update(Mod7, . ~ . - morphotype:N)
drop1(Mod8)

Mod9 <- update(Mod8, . ~ . - morphotype:L)
drop1(Mod9)

Mod10 <- update(Mod9, . ~ . - morphotype:type)
drop1(Mod10)

summary(Mod10)

EMTrend_summary <- emtrends(Mod10, ~type*morphotype, var = "N")

tidy(EMTrend_summary)[, -c(1:6)]
tidy(EMTrend_summary)[, -c(3:6)]

# ---------- linear models graphics ------------

MyData_Type <-  myttred2 %>% group_by(type, morphotype) %>% do(data.frame(N = seq(min(.$N), max(.$N), length.out = 100), L = mean(.$L, na.rm = T)))
  
predicted_Dead <- predict(Mod10, newdata = MyData_Type, type = "response", se.fit = T)

MyData_Type$Fit <- predicted_Dead$fit
MyData_Type$SE <- predicted_Dead$se.fit

Pl_MyData_Type <- ggplot(MyData_Type, aes(x = N, y = Fit, color = morphotype)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
  facet_wrap(~type) +
  scale_color_manual(values = c("blue", "yellow")) +
  labs(x = "Плотность", y = "Смертность", color = "Морфотип") +
  theme_bw()

EMTrend_summary[, -2]

iris[, -3]


# MyData_Type %>% filter(morphotype == "e") %>% ggplot(., aes(x = N, y = Fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
#   facet_wrap(~type)

# MyData_PropDead <-  myttred2 %>% do(expand.grid(N = seq(min(.$N), max(.$N), length.out = 100), L = mean(.$L, na.rm = T), type = myttred2$type, morphotype = "e"))
# 
# predicted_PropDead <- predict(Mod10, newdata = MyData_PropDead, type = "response", se.fit = T)
# 
# MyData_PropDead$Fit <- predicted_PropDead$fit
# 
# MyData_PropDead$SE <- predicted_PropDead$se.fit
# 
# ggplot(MyData_PropDead, aes(x = N, y = Fit)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
#   facet_wrap(~type)


# MyData_N <- myttred2 %>% group_by(morphotype) %>% do(expand.grid(N = c(20, 60, 120), L = mean(.$L, na.rm = T), 

# MyData_N <- myttred2 %>%  group_by(morphotype) %>% do(expand.grid(N = c(20, 60, 120), PropT = seq(min(.$PropT), max(.$PropT), length.out = 100), L = mean(.$L, na.rm = T)))
# 
# # MyData_N <- myttred2 %>% group_by(type) %>% do(expand.grid(morphotype = c("t", "e"), N = seq(min(.$N), max(.$N), length.out = 100),PropT = mean(.$PropT), L= mean(.$L, na.rm = T)))
# # 
# predicted_N <- predict(Mod0, newdata = MyData_N, type = "response", se.fit = T)
# # 
# MyData_N$Fit <- predicted_N$fit
# 
# MyData_N$SE <- predicted_N$se.fit
# 
# ggplot(MyData_N, aes(x = PropT, y = Fit, color = morphotype)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
#   facet_wrap(~N)
# # 
# MyData_L_N <- expand.grid(morphotype = c("t", "e"),N = c(20, 60, 120),  L= seq(min(myttred2$L, na.rm = T), 37), PropT = mean(myttred2$PropT))
# 
# predicted_L_N <- predict(Mod10, newdata = MyData_L_N, type = "response", se.fit = T)
# 
# MyData_L_N$Fit <- predicted_L_N$fit
# 
# MyData_L_N$SE <- predicted_L_N$se.fit
# 
# ggplot(MyData_L_N, aes(x = L, y = Fit, color = morphotype)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
#   facet_wrap(~N)
# 
# 
# 
 
# MyData_L_PropT <- expand.grid(morphotype = c("t", "e"),N = mean(myttred2$N),  L= seq(min(myttred2$L, na.rm = T), 37), type = myttred2$type)
# 
# predicted_L_PropT <- predict(Mod10, newdata = MyData_L_PropT, type = "response", se.fit = T)
# 
# MyData_L_PropT$Fit <- predicted_L_PropT$fit
# 
# MyData_L_PropT$SE <- predicted_L_PropT$se.fit
# 
# ggplot(MyData_L_PropT, aes(x = L, y = Fit, color = morphotype)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
#   facet_wrap(~type)


MyData <- expand.grid(morphotype = c("t", "e"), L= mean(myttred2$L, na.rm = T), PropT = seq(min(myttred2$PropT), max(myttred2$PropT), length.out = 100), N = seq(min(myttred2$N), max(myttred2$N), length.out = 100))

predicted <- predict(Mod10, newdata = MyData)

MyData$Fit = predicted

ggplot(MyData, aes(x = N, y = PropT )) + 
  geom_tile(aes(fill = (Fit))) +
  facet_wrap(~morphotype)  +
  scale_fill_gradient(low = "white", high = "black") +
  geom_point(data = myttred2)



  