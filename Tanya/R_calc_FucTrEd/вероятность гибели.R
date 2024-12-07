library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



myt_status <- read_excel('Data/FucTrEd.xlsx', sheet = 'Лист3')



myt_status %>% 
  pivot_longer(cols = -c(ID, Location, Status, Morphotype)  ) %>%
  uncount(value)  %>%
  as_tibble ->
  myt_outcome

myt_outcome %>%
  mutate(Out = ifelse(Status == "мертвые", 1, 0),
         Substrate = ifelse(Location == "Грунт", "Bottom", "Algae")) ->
  myt_outcome


myt_cage_param <- 
  myt %>% 
  select(ID, weight_Asc, weight_Fuc, Prop_T, N_final)

myt_outcome <- merge(myt_outcome, myt_cage_param)



mod <- glm(Out ~  Substrate*Morphotype + Prop_T*Morphotype + N_final*Morphotype, data = myt_outcome )


summary(mod)

drop1(mod)

mod2 <- update(mod, .~. - Morphotype:Prop_T)
drop1(mod2)

mod3 <- update(mod2, . ~ . - Morphotype:N_final )

drop1(mod3)

mod4 <- update(mod3, . ~ . - N_final )
drop1(mod4)


summary(mod4)

My_data <- expand.grid(Substrate = c("Bottom", "Algae"), Morphotype = c("T", "E"), Prop_T = seq(min(myt_outcome$Prop_T), max(myt_outcome$Prop_T), length.out = 20))


predicted <- predict(mod4, newdata = My_data, se.fit = T)

My_data$Predicted <- predicted$fit
My_data$SE <- predicted$se.fit


ggplot(My_data, aes(x = Prop_T, y = Predicted, color = Morphotype)) +
  geom_line() +
  geom_ribbon(aes(ymin = Predicted - 2*SE, ymax = Predicted + 2*SE), alpha = 0.2)+
  facet_wrap(~ Substrate)

