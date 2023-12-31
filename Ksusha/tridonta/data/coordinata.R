#определяем среднее значение для станций
size <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "Size")
pile <- read_excel("data/Координаты береговой линии Илистой губы.xlsx", sheet = "piles")
shore <- read_excel("data/Координаты береговой линии Илистой губы.xlsx", sheet = "Shoreline")
samples <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "sites")
abundance <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "Abundance")
arxiv <- read_excel("data/Ilistaya_inlet_1987_2019_N_B.xlsx", sheet = "Ilistaya_inlet_1987_2019_N_B")
#install.packages("reshape2")
library(cowplot)
library(dplyr)
library(reshape2)
tb <-
arxiv %>% filter(Taxa == "Tridonta borealis")

tbbrait<-
dcast(formula = Station ~ Year, value.var = "N", data = tb)

tbbrait[is.na(tbbrait)] <- 0

tb_brait_means<-data.frame(Station = tbbrait$Station, N_mean = rowMeans(tbbrait[sapply(tbbrait, is.numeric)]))

coordinates_of_stations <- read_excel("data/Coordinates_of_stations.xlsx", sheet = "Coordinates_of_stations")


N <- c(67.0944666666667, 67.0952, 67.0953333333333)
E <- c(32.67855, 32.6815, 32.67765)

A <- matrix(c(729, 2556, 1,
              2609,	1466, 1,
              269,  1038, 1), 3, 3, byrow=TRUE)

abc <- solve(A, N)
def <- solve(A, E)

a <- abc[1]
b <- abc[2]
c <- abc[3]
d <- def[1]
e <- def[2]
f <- def[3]

pixels_to_N <- function(x,y) {
  N <- a*x + b*y + c
  E <- d*x + e*y + f
  return(N)
}

pixels_to_N(729, 2556)

pixels_to_E <- function(x,y) {
  N <- a*x + b*y + c
  E <- d*x + e*y + f
  return(E)
}

coordinates_of_stations$N = pixels_to_N(coordinates_of_stations$N_paint,
                                        coordinates_of_stations$E_paint)

coordinates_of_stations$E = pixels_to_E(coordinates_of_stations$N_paint,
                                        coordinates_of_stations$E_paint)

tb_brait_means<-
merge(coordinates_of_stations, tb_brait_means)
samples_abundance<-
merge(samples, abundance)


Pl_old<-
ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = tb_brait_means,aes(x = E, y = N, size = N_mean))+
  guides(color = "none", size = "none")+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


Pl_2023<-
ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = samples_abundance %>% filter(N_alive > 0), aes(size = N_alive*40))+
  guides(color = "none", size = "none")+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


plot_grid(Pl_old, Pl_2023)

