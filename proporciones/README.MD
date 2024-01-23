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
Todos estos datos los extraemos con una funcion en R, guardamos estos datos en un dataframe temporal y luego los exportamos en un csv. Para su consulta, todos los csv estan disponibles en [Prop_mundial](https://github.com/LuisMario2016/Servicio_social/tree/main/proporciones/Prop_mundial "Prop_mundial"), ya filtrados por años y asignados a su pais correspondiente (255 paises), y los encuentra con el nombre propio de la variable, ex: secma, primf, primn, c3fnx...

``
write.csv(secdn, file.path(ruta, "secdn.csv"), row.names =TRUE)
``
## Obtencion del cambio de uso de suelo por pais
