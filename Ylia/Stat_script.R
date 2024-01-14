# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("vegan")
# install.packages("dplyr")
library(dplyr)
library(readxl)
library(ggplot2)
library(vegan)

data <- read_excel("D:/Rabota_2024/Data/Mussel_Bed_N_species_2023.xlsx")

data$D <- data$NT / (data$NT + data$NE)
ggplot(data = data, aes(x = D, y = log(Polydora_quadrilobata + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Hydrobia_ulvae + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Nemertini + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Tubificoides_benedeni + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Onoba_aculeus + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Littorina_sp. + 1))) + geom_point() + geom_smooth()
ggplot(data = data, aes(x = D, y = log(Oligochaeta + 1))) + geom_point() + geom_smooth()

log(exp(1))

apply(data, MARGIN = 2, FUN = function(x) mean(x != 0))


data %>% summarise_all(.funs = function(x) mean(x != 0)) %>% t() %>% as.data.frame(.) %>% filter(V1 <= 0.1)



str(data)

exclude <- c("Year", "N", "B", "Bank", "Season", "Sample", "NT", "NE", "D", "Arenicola_marina", "Bunodactis_stella", "Cylichna_occulta_DEAD", "Harmothoe_imbricata", "Pectinaria_hyperborea", "Нитчатые_водоросли", "Fucus_sp.", "Skeneopsis_planorbis",  "Skeneopsis_planorbis_DEAD")

names(data)

trup <- c("Cylichna_occulta_DEAD", "Hydrobia_ulvae_DEAD", "Littorina_sp._DEAD", "Macoma_balthica_DEAD", "Onoba_aculeus_DEAD", "Skeneopsis_planorbis_DEAD")

data <- data %>% filter(NT !=0 & NE !=0)

mat <- data %>% filter(NT !=0 & NE !=0) %>% select(-exclude) 

vor <- data %>% filter(Bank == "VOR4" & NT !=0 & NE !=0)

vor4 <- vor %>% select(-exclude) 

dead <- data %>% filter(NT !=0 & NE !=0) %>% select(trup)
vor4_dead <- data %>% filter(NT !=0 & NE !=0)%>% filter(Bank == "VOR4") %>% select(trup) 


mat_log <- decostand(mat, method = "log")
dead_log <- decostand(dead, method = "log")

vor4_log <- decostand(vor4, method = "log")
vor4_dead_log <- decostand(vor4_dead, method = "log")

MDS_mat <- metaMDS(mat_log, autotransform = F, try = 100 )

MDS_dead <- metaMDS(dead_log, distance = "bray", autotransform = T)


stressplot(MDS_mat)
stressplot(MDS_dead)
MDS_mat$stress
MDS_dead$stress

plot(MDS_mat, type = "t")


env <- envfit(MDS_mat ~ NT + NE + B + Bank, data = data)
env
plot(MDS_mat, type = "t")
plot(env)



plot(MDS_dead, type = "t")


env <- envfit(MDS_dead ~ D + Bank, data = data)
env
plot(MDS_dead, type = "t")
plot(env)



mat_cca <- cca(mat ~   Bank + D , data = data)
plot(mat_cca, type = "t")
anova(mat_cca)

anova(mat_cca, by = "axis")

anova.cca(mat_cca, by = "terms")

anova.cca(mat_cca, by = "margin")

summary(mat_cca)


vif.cca(mat_cca)


############################################

mat_rda <- rda(dead_log ~   Bank + D , data = data)
plot(mat_rda, type = "t")
anova(mat_rda)

anova(mat_rda, by = "axis")

anova.cca(mat_rda, by = "terms")

anova.cca(mat_rda, by = "margin")

s <- summary(mat_rda)


vif.cca(mat_rda)


scores(mat_rda)


##############################################3

install.packages("remotes")
remotes::install_github("gavinsimpson/ggvegan", force = TRUE)
library(ggvegan)

gg <- fortify(mat_rda)

tail(gg)

predictors <- data %>% select(Bank)

sites <- gg %>% filter(score == "sites") %>% mutate(Bank = data$Bank)
library(dplyr)

centroids <- gg %>% filter(score == "centroids")
mat <- gg %>% filter(score == "centroids" & label == "Bankmat")
biplots <- gg %>% filter(score == "biplot" & label == "Bankmat")
k <- (mat$RDA1 / biplots$RDA1) ^-1
D <- gg %>% filter(label == "D")
species <- gg %>%  filter(score == "species") %>% mutate(N = c(1:6))
names <- species %>% filter(RDA2 < 0)
names$label


res <- summary(mat_rda)

rda_sites <- res$sites 
rda_species <- res$species


ggvegan:::arrowMul(res$biplot, rbind(rda_sites, rda_species))

ordiArrowMul(mat_rda, display = "species", fill = 1)
Pl_sites <- 
ggplot(data = sites, aes(x = RDA1, y = RDA2)) + 
  geom_point(aes(color = Bank)) +
  scale_color_manual(values = c("black", "gray", "red", "blue"))+
  geom_text(data = centroids, aes(label = label), color = "blue" ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  geom_segment(data = D, aes(x = 0, y = 0, xend = 4 * RDA1, yend = 4 * RDA2), arrow = arrow(angle = 10), color = "blue") +
  theme(legend.position = "bottom")


library(ggrepel)

Pl_species <- 
ggplot(data = species, aes(x = RDA1, y = RDA2)) + 
  geom_point() +
  scale_color_manual(values = c("black", "gray", "red", "blue"))+
  geom_text_repel(data = centroids, aes(label = label), size = 5, color = "blue" ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_text_repel(aes(label = N)) +
  theme_bw() +
  geom_segment(data = D, aes(x = 0, y = 0, xend = 4 * RDA1, yend = 4 * RDA2), arrow = arrow(angle = 10), color = "blue")


# install.packages("cowplot")

library(cowplot)

plot_grid(Pl_sites, Pl_species, nrow = 2)

###################################################


vor4_rda <- rda(vor4_log ~ D , data = vor)
plot(vor4_rda)
anova(vor4_rda)

anova(vor4_rda, by = "axis")

anova.cca(vor4_rda, by = "terms")

anova.cca(vor4_rda, by = "margin")

summary(vor4_rda)


vif.cca(vor4_rda)








MDS_vor4 <- metaMDS(vor4_log, autotransform = F, try = 100 )

MDS_vor4_dead <- metaMDS(vor4_dead_log, distance = "bray", autotransform = T)


stressplot(MDS_vor4)
stressplot(MDS_vor4_dead)
MDS_vor4$stress
MDS_vor4_dead$stress

plot(MDS_vor4, type = "t")


env <- envfit(MDS_vor4 ~ D, data = vor)
env
plot(MDS_vor4, type = "t")
plot(env)



plot(MDS_vor4_dead, type = "t")


env <- envfit(MDS_vor4_dead ~ D, data = vor)
env
plot(MDS_vor4_dead, type = "t")
plot(env)


mat_cca <- rda(vor4_log ~ D , data = vor)
plot(mat_cca, type = "t")
anova(mat_cca)

anova(mat_cca, by = "axis")

anova.cca(mat_cca, by = "terms")

anova.cca(mat_cca, by = "margin")

summary(mat_cca)



vif.cca(mat_cca)










vor$D_l <- (vor$NT / (vor$NT + vor$NE) > 0.5)



p1 <- ggplot(data = vor, aes(x = D_l, y = log(Polydora_quadrilobata + 1))) + geom_boxplot() + theme_bw() + xlab("  ") + ylab("Обилие Polydora quanrilobata") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p2 <- ggplot(data = vor, aes(x = D_l, y = log(Capitella_capitata + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие Capitella capitata") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p3 <- ggplot(data = vor, aes(x = D_l, y = log(Chironomidae_gen_sp + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие Chironomidae.gen.sp") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p4 <- ggplot(data = vor, aes(x = D_l, y = log(Fabricia_sabella + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие Fabricia sabella") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p5 <- ggplot(data = vor, aes(x = D_l, y = log(Hydrobia_ulvae_DEAD + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие мёртвых Hydrobia ulvae") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p6 <- ggplot(data = vor, aes(x = D_l, y = log(Macoma_balthica + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие Macoma baltica") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p7 <- ggplot(data = vor, aes(x = D_l, y = log(Macoma_balthica_DEAD + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие мёртвых Macoma baltica") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p8 <- ggplot(data = vor, aes(x = D_l, y = log(Onoba_aculeus_DEAD + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие мёртвых Onoba aculeus") + scale_x_discrete(labels = c("E dominated", "T dominated"))
p9 <- ggplot(data = vor, aes(x = D_l, y = log(Littorina_sp._DEAD + 1))) + geom_boxplot()+ theme_bw() + xlab("") + ylab("Обилие мёртвых Littorina sp.") + scale_x_discrete(labels = c("E dominated", "T dominated"))

p10 <- ggplot(data = vor, aes(x = D_l, y = log(Нитчатые_водоросли + 1))) + geom_boxplot()+ theme_bw() + ylab("Обилие нитчатых водорослей") + xlab("")
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) 






data$D_l <- (data$NT / (data$NT + data$NE) > 0.5)


ggplot(data = data, aes(x = D_l, y = log(Polydora_quadrilobata + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Capitella_capitata + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Chironomidae_gen_sp + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Fabricia_sabella + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Hydrobia_ulvae + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Hydrobia_ulvae_DEAD + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Jaera_sp. + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Littorina_sp. + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Macoma_balthica_DEAD + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Nemertini + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Onoba_aculeus + 1))) + geom_boxplot()
ggplot(data = data, aes(x = D_l, y = log(Tubificoides_benedeni + 1))) + geom_boxplot()


install.packages("mapproj")

