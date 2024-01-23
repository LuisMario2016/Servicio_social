setwd("C:/serviciosocial/DATOS")
######### Esta primer parte extrae los valores de PIB de acuerdo a años especificos
######### que van desde 1895 hasta 2015 cada 15 años          
library(readxl)
paisesmad_SS <- read_excel("paisesmad_SS.xlsx", 
                             sheet = "GDP pc")

years_interes <- seq(1895, 2015, by = 15)
PIB<- as.data.frame(paisesmad_SS)

library(tidyr)
library(dplyr)
Filtro <- PIB %>% 
  filter(`GDP pc 2011 prices` %in% years_interes)

PIB_transpuesto <- t(Filtro)
colnames(PIB_transpuesto) <- unlist(PIB_transpuesto[1, ])
PIBTrans<- PIB_transpuesto[-1,]
PIBTrans <- PIBTrans[rowSums(is.na(PIBTrans)) <= 2, ]
PIB<-as.data.frame(PIBTrans)
ruta<- "C:\\serviciosocial"
write.csv(PIB, file.path(ruta, "PIB.csv"))

######### En este codigo obtenemos ahora los valores de la poblacion y los 
######### almacenamos en un cvs para su posterior consulta
paisesmad_SS <- read_excel("paisesmad_SS.xlsx", 
                           sheet = "Population")
POP<- as.data.frame(paisesmad_SS)
FiltroPOP <- POP %>% 
  filter(Population%in% years_interes)

POPTrans<- t(FiltroPOP)
colnames(POPTrans) <- unlist(POPTrans[1, ])
POPTrans<- POPTrans[-1,]
POPTrans <- POPTrans[rowSums(is.na(POPTrans)) <= 2, ]
POP<-as.data.frame(POPTrans)
ruta<- "C:\\serviciosocial"
write.csv(POP, file.path(ruta, "POP.csv"))

######################### eligiendo paises (con mayor PIB y/o poblacion)

PIB <- read.csv("C:/serviciosocial/PIB.csv")
POP <- read.csv("C:/serviciosocial/POP.csv")
resultado <- merge(POP, PIB, by = "X")# x es para pop y "y" para pib
top_filas <- resultado %>%
  slice_max(order_by = everything(), n = 10)
## Obteniendo varios resultados, se seleccionaron por interes personal 12 paises
.









