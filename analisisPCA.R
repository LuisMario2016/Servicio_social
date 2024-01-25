####### Carga de datos ##################
# Cargar las librerías
library(FactoMineR)
library(readr)
library(factoextra)

ruta_archivo <- "C:/serviciosocial/proporciones/Propxpais"
archivos_csv <- list.files(path = ruta_archivo, pattern = "\\.csv$", full.names = TRUE)

for (archivo in archivos_csv) {
  nombre_predeterminado <- gsub(".csv", "", basename(archivo))
  df <- read.csv(archivo, fileEncoding = "latin1")
  colnames(df) <- make.names(colnames(df))
  assign(nombre_predeterminado, df)}

# para cargar datos puede hacerlo manualmente desde PIB Y POP, segun el pais.
Mexico$PIB<- c(1757,2240,2640,2560,4060,6873,9785,12613,16096)
Mexico$POP<- c(1266,15000,15854,20393,32930,52775,76767,98403,118016)
Mexico<-Mexico[,-1]

AUS$PIB<-c(4285,5244,	5367,	6311,	8054,	15537,	23514,	34796,	41294)
AUS$POP<-c(3460,	4375,	5943,	7042,	9277,	12660,	15695,	18902,	23686)
AUS<-AUS[,-1]

BRAZIL$PIB<-c(1007,	990,	1298,	1610,	2675,	4635,	7862,	9834,	15826)
BRAZIL$POP<-c(15980,	22216,	30332,	41114,	61774,	95684,	137382,	176369,	206667)
BRAZIL<-BRAZIL[,-1]

CANADA$PIB<-c(3779,	6481,	6918,	8557,	13072,	19207,	28025,	36943,	43619)
CANADA$POP<-c(5169,	7188,	9549,	11688,	16050,	21750,	25942,	30823,	35883)
CANADA<-CANADA[,-1]

CHILE$PIB<-c(3304,	4485,	4903,	5177,	6341,	8195,	8024,	15212,	21589)
CHILE$POP<-C(2783,	3317,	3970,	5056,	6743,	9369,	12068,	15285,	17899)
CHILE<-CHILE[,-1]

EUA$PIB<-C(7160,	9637,	11150,	12005,	17370,	23958,	33023,	45886,	52591)
EUA$POP<-c(69851,	92767,	116284,	132637,	165931,	205052,	238466,	282738,	321397)
EUA<-EUA[,-1]

FRANCIA$PIB<-c(4095,	4726,	6641,	6443,	9881,	18187,	24755,	33410,	36827)
FRANCIA$POP<-c(40098,	41224,	40610,	41000,	44218,	51918,	56490,	60819,	66611)
FRANCIA<-FRANCIA[,-1]

INDIA$PIB<-c(920,	1111,	1113,	1093,	1078,	1384,	1720,	2753,	5794)
INDIA$POP<-c(282052,	302100,	319900,	386800,	393000,	541000,	755000,	1007310,	1252952)
INDIA<-INDIA[,-1]

JAPAN$PIB<-c(2022,	2317,	3414,	4882,	4417,	15484,	24437,	33211,	37031)
JAPAN$POP<-C(4177,	4951,	5952,	7296,	8981,	10434,	12075,	12693,	12652)
JAPAN<-JAPAN[,-1]

PORTUGAL$PIB<-c(1780,	1957,	2305,	2574,	3945,	8724,	13240,	23372,	24915)
PORTUGAL$POP<-c(5215,	5884,	6378,	7675,	8693,	9044,	9897,	10228,	10295)
PORTUGAL<-PORTUGAL[,-1]

SINGAPURE$PIB<-c(NA,	2732,	3255,	NA,	2946,	6650,	16611,	37772,	65660)
SINGAPURE$POP<-c(NA,	NA,	492,	751,	1306,	2075,	2750,	4063,	5674)
SINGAPURE<-SINGAPURE[,-1]

UK$PIB<-C(6996,	7718,	8199,	10928,	12541,	17162,	22579,	31946,	36941)
UK$POP<-c(39221,	44916,	45059,	48226,	50946,	55632,	56620,	59145,	65396)
UK<-UK[,-1]


#### PCA'S ###############

pca_sing <- PCA(SINGAPURE, graph = FALSE)
biplot_sing <- fviz_pca_biplot(pca_sin, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_sing)

pca_aus <- PCA(AUS, graph = FALSE)
biplot_aus <- fviz_pca_biplot(pca_aus, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_aus)

pca_brazil <- PCA(BRAZIL, graph = FALSE)
biplot_brazil <- fviz_pca_biplot(pca_brazil, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_brazil)

pca_canada <- PCA(CANADA, graph = FALSE)
biplot_canada <- fviz_pca_biplot(pca_canda, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_canada)

pca_chile <- PCA(CHILE, graph = FALSE)
biplot_clile <- fviz_pca_biplot(pca_chile, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_chile)

pca_eua <- PCA(EUA, graph = FALSE)
biplot_eua <- fviz_pca_biplot(pca_eua, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_eua)

pca_francia <- PCA(FRANCIA, graph = FALSE)
biplot_francia <- fviz_pca_biplot(pca_francia, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_francia)

pca_india <- PCA(INDIA, graph = FALSE)
biplot_india <- fviz_pca_biplot(pca_india, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_india)

pca_japan <- PCA(JAPAN, graph = FALSE)
biplot_japan <- fviz_pca_biplot(pca_japan, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_japan)

pca_mexico <- PCA(Mexico, graph = FALSE)
biplot_mexico <- fviz_pca_biplot(pca_mexico, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_mexico)

pca_portugal <- PCA(PORTUGAL, graph = FALSE)
biplot_portugal <- fviz_pca_biplot(pca_portugal, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_portugal)

pca_uk <- PCA(UK, graph = FALSE)
biplot_uk <- fviz_pca_biplot(pca_uk, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot_uk)

pca_sing <- PCA(SINGAPURE, graph = FALSE)
biplot <- fviz_pca_biplot(pca_sin, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
print(biplot)



















