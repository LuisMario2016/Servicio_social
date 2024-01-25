ruta_archivo <- "C:/serviciosocial/proporciones/Propxpais"
archivos_csv <- list.files(path = ruta_archivo, pattern = "\\.csv$", full.names = TRUE)

ruta_archivo <- "C:/serviciosocial/proporciones/Propxpais"
archivos_csv <- list.files(path = ruta_archivo, pattern = "\\.csv$", full.names = TRUE)

for (archivo in archivos_csv) {
  nombre_predeterminado <- gsub(".csv", "", basename(archivo))
  df <- read.csv(archivo, fileEncoding = "latin1", row.names = 1)  # 1 indica que la primera columna es el nombre de la fila
  colnames(df) <- make.names(colnames(df))
  assign(nombre_predeterminado, df)
}

Mexico1<-Mexico
Mexico <- rbind(Mexico, c(NA, NA, NA, NA))
rownames(Mexico)[nrow(Mexico)] <- "2030"

num_filas <- nrow(Mexico)
for (i in seq(num_filas, 2, by = -1)) {
  Mexico[i, ] <- Mexico[i - 1, ]
}
nueva_fila <- c(NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)
Mexico[1, ] <- nueva_fila
plot(na.omit(Mexico$primn),Mexico1$primn)
