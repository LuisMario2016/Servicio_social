# Analisis proporcional de las variables de cambio de uso de suelo
Analizar el cambio de uso de suelo a nivel global es una tarea costosa computacionalmente hablando, y ademas analizar escenarios a macro-escala pudiera ignorar variables propias del entorno, como lo es la economia, politica, poblacion, etc. Por lo que en esta seccion se aborda el estudio del cambio de uso de suelo a lo largo de 100 años, en 12 paises de importancia economica y/o aumento considerable de población.

## Obtención y filtrado
Los datos recopilados del GDP y la población fueron obtenidos de un proyecto elaborado por la Universidad de Groningen, que lleva por nombre [Maddison Project](https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020?lang=en "Maddison Project").
La base de datos fue filtrada, eliminando valores extraños, filas innecesarias, etc, todo manualmente mediante hoja de datos de Excel, el archivo puede ser revisado con el nombre [Paisesmad_SS](https://github.com/LuisMario2016/Servicio_social/blob/main/proporciones/paisesmad_SS.xlsx "Paisesmad_SS")

------------
**Importante**: El codigo completo esta disponible en esta misma carpeta, abajo solo se explican a grandes rasgos como funciona.

------------


Los datos contenidos en STATES.nc que extraimos y descargamos del tema cambio de uso de suelo, contiene las variables del cambio de uso de suelo a nivel global, por lo que necesitamos un delimitador de paises en este caso es la funcion: 

``
land<-readOGR(dsn = "https://gegp01.github.io/ServSoc/countries.geojson", verbose = F)
``

De igual forma el archivo States.nc contiene datos historicos sobre el cambio de uso de suelo, dese 1700 hasta 2015, estos años estan representados por "bands", y seleccionaremos en este caso un intervalo de 100 años, cada 15 años. El codigo completo lo puede encontrar en **PropxPais**

``
rasX.secdn_2015 <-raster("states.nc", var="secdn",band=1165)
``

Cambiaremos el nombre segun que necesitemos extraer, tanto en el nombre de la varibale como en la funcion, en este caso extamos extrayendo los datos de "secondary.mean.age" en el año 2015.

Con la siguente funcion vamos a lograr hacer una proporcion de los valores totales del pais, es decir obtener los datos a macro-escala de como se comporta el pais respecto a un cambio de uso de suelo.
``
count_country <- sapply(values_by_country, function(x) length(na.omit(x)))
proportion_country <- sapply(values_by_country, function(x) sum(na.omit(x)) / length(na.omit(x)))
``
Todos estos datos los extraemos con una funcion en R, guardamos estos datos en un dataframe temporal y luego los exportamos en un csv. Para su consulta, todos los csv estan disponibles en [Prop_mundial](https://github.com/LuisMario2016/Servicio_social/tree/main/proporciones/Prop_mundial "Prop_mundial"), ya filtrados por años y asignados a su pais correspondiente (255 paises), y los encuentra con el nombre propio de la variable, ex: secma, primf, primn, c3fnx...

``
write.csv(secdn, file.path(ruta, "secdn.csv"), row.names =TRUE)
``
## Seleccion de Paises de interes
Cargamos el archivo de Paisesmad_SS y realizamos un filtrado de datos con algunas funciones basicas de R con la paqueteria dyplr y tydr. Se eliminaron los años anteriores a nuestro interes, se filtraron los paises que les faltanban mas de 2 datos en nuestros años de interes y se realizo un filtrado con los mayores valores de PIB y POP (poblacion), estos datos para su libre consulta estan disponibles en formato csv llamados PIB y POB.
**Codigo completo disponible en [Select_paises](https://github.com/LuisMario2016/Servicio_social/blob/main/proporciones/Selec_paises.R "Select_paises") **

## Obtencion del cambio de uso de suelo por pais
Aqui nos tomaremos la generosidad de explicar mas a detalle las partes del codigo, ya que los datos ya filtrados y disponible en este repo estan disponibles para su libre consulta, sin embargo usted puede tener algun interes particular en extraer los datos de un pais en especifico.
Como primer tarea vamos a cargar la libreria y datos que necesitamos
```r
# Cargar la librería tidyverse
install.packages("tidyverse")
library(tidyverse)

directorio <- "C:/serviciosocial/proporciones/Prop_mundial"
# Lista de nombres de archivos CSV en el directorio
archivos_csv <- list.files(path = directorio, pattern = "\\.csv$", full.names = TRUE)

# Inicializar un dataframe vacío para almacenar los datos de México
datos_mexico <- data.frame() #Cambie el nombre a su interes
```
Es recomendable poner en una carpeta todos los archivos csv correspóndientes a las variables (secma,primn,primf...) que sean de su interes, tenga en cuenta el orden que tienen los arvichos en su carpeta ya que sera indispensable mas adelante.

Ahora iteraremos sobre los archivos en busqueda del pais de nuestro interes en este caso "Mexico".
```r
# Iterar sobre cada archivo CSV
for (archivo in archivos_csv) {
  # Leer el archivo CSV
  datos <- read_csv(archivo)
  
  # Filtrar las filas relacionadas con México y combinarlas en un dataframe
  datos_mexico <- bind_rows(datos_mexico, filter(datos, ...1 == "Mexico"))
}

# Verificar el dataframe con los datos de México
print(datos_mexico)
```
Recuerda que le mencionamos tener en cuenta el orden de sus archivos csv de manera ascendente-descendiente?, es hora de ocupar este recurso para poder asignar el nombre correcto a nuestras variables.
```r
## Crear un vector con los nuevos nombres de las columnas en orden en el que estan listados en su carpeta
nuevos_nombres <- c("primf", "primn", "otro_nombre", "otro_nombre_mas", ...)

# Asignar los nuevos nombres a las columnas del dataframe
colnames(datos_mexico) <- nuevos_nombres

### Al finalizar puede exportar el archivo al formato que desee para visualizacion/analisis posteriores#####
```
Como menciona esta ultima linea usted puede exportar este Dataframe a cualquier archivo que sea se su interes, ya sea csv, txt, rtf, xlsx, etc. Esto con la finalidad de poder visualizar los datos, una forma practica de hacerlo es sumar los indices de la vegetacion primaria forestal, no forestal y la vegetacion secundaria, a su vez sumar los otros tipos de uso de suelo que generalmete son cultivos, y por ultimo tomar en cuenta los asentamientos humanos, y asi poder visualizar como se comportan estas tres variables en un lapso de tiempo de 100 años. Usted podria obtener algo como esto:
[![Cambioxpais](https://raw.githubusercontent.com/LuisMario2016/Servicio_social/main/proporciones/Imagenes/Prop_pais.png?token=GHSAT0AAAAAACM3HEN3GBZEKHS26SUWJ5JWZNQHJNQ "Cambioxpais")](https://github.com/LuisMario2016/Servicio_social/blob/main/proporciones/Imagenes/Prop_pais.png "Cambioxpais")
