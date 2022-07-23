#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Problem Set 3
#
#Equipo 12

#Jorge E. García
#Ingrid Lorena Molano
#Camilo Villa Moreno

#Julio 26, 2022

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. PRELIMINARES: PREPARACIÓN ESPACIO DE TRABAJO Y LIBRERÍAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##Limpiar el entorno ----
rm(list=ls())


##Carga de librerías ----
install.packages("pacman")
library(pacman)

p_load(rio,
       sf, # Leer, escribir o manipular datos espaciales
       leaflet, #Visualizaciones dinámicas
       tmaptools, #geocode_OSM()
       osmdata,  #Get OSM´s data
       doParallel,
       gtsummary,
       GGally,
       stargazer,
       fabricatr,
       tableone,
       arsenal,
       janitor,
       tidyverse,
       gamlr,
       skimr, 
       caret,
       rvest,
       stargazer,
       smotefamily,
       MASS,
       ROCR,
       pROC,
       rpart,
       rpart.plot,
       glmnet,
       xgboost)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_PS3")

train_prop <-readRDS("./stores/train.rds") #107.567 Obs
test_prop <-readRDS("./stores/test.rds") #11.150 Obs



##1.1. Exploración incial de los datos ----


view(train_prop)
view(test_prop)

table(train_prop$l3) #Bogotá 86.211 Medellín: 21.356
table(train_prop$property_type) #Apartamento 81.577 Casa:25.990
table(train_prop$operation_type) #venta: total obs 107.567
table(train_prop$currency) #COP: total obs 107.567
table(train_prop$ad_type) #Propiedad: total obs 107.567


table(test_prop$l3) #Bogotá 793 Medellín: 10.357
table(test_prop$property_type) #Apartamento 9.658 Casa:1.492
table(test_prop$operation_type) #venta: total obs 11.150
table(test_prop$currency) #COP: total obs 11.150
table(test_prop$ad_type) #Propiedad: total obs 11.150


#Las variables operation_type, currency y as_type no dan info relevante.

#Exploración de las bases de datos:
skim(train_prop)
skim(test_prop)

#Comparación de variables (columnas) entre las dos bases de datos:
compare_df_cols(train_prop, test_prop)

#Resumen de diferencias
all_equal(train_prop, test_prop)


# Las variables son las mismas, excepto el Asking Price (Y) que no está
# en la de Test (esperable).
# 
# De acuerdo con Skim, las variables con mayor cantidad de NAs son:
#


# Var		        Complete Rate (Train)		    Complete rate (test)

# rooms  		 	      0.502				            0.515			
# bathrooms 	  	  0.720				            0.617
# surface_total 	  0.258				            0.185      
# surface_covered   0.188				            0.103
# description       0.999	(70 NA)		      	0.999 	(16 NA)
# title             1.00 	(35 NA)        		1 	    (0 NA)


#surface_total, surface covered, rooms son prácticamente inutilizables.
#Hay que revisar si en la descripción, por análisis de texto, está la info.


#Se deben completar mínimo 2 variables geográficas:

#Ej. Distancia estación Metro / Transmilenio / Parada de bus(?).
#Ej. Distancia al centro de la ciudad.


#Se deben agregar mínimo dos variables por procesamiento de texto a partir de la descripción:
# Ej: Disponibilidad ascensor
# Ej: Parqueaderos, número de parqueaderos
# Ej: Área total, superficie (ver NAs, comentarios anteriores).
# Ej: Número de piso para el caso de apartamentos (prima de altura)

##1.2. Graficas NAs----
##NAs Base Train ---- 
#Para sacar la gráfica de NAs

cantidad_na <- sapply(train_prop, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(train_prop)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 11.11% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
#Las variables sin NAs son: property_id, ad_type, start_date, end_date, 
#created_on, lat, lon, l1, l2, l3, bedrooms, price, currency, property_type, 
#operation_type

porcentaje_na <- porcentaje_na[!filtro,]
# 
orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)


# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

##NAs Base Test ---- 
#Para sacar la gráfica de NAs

cantidad_na <- sapply(test_prop, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(test_prop)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 12.91% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
#Las variables sin NAs son: property_id, ad_type, start_date, end_date, 
#created_on, lat, lon, l1, l2, l3, bedrooms, currency, property_type, 
#operation_type

porcentaje_na <- porcentaje_na[!filtro,]# 

orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)


# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. SEPARAR LAS BASES/ BOGOTÁ D.C. Y MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Base de Bogotá D.C.
train_bog <-subset(train_prop,train_prop$l3 =="Bogotá D.C")
test_bog <-subset(test_prop,test_prop$l3 =="Bogotá D.C")                   


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. VARIABLES DE TEXTO E IMPUTACIÓN DE DATOS 

##3.1. Variables de texto----

#Se cargan las bases de datos

setwd("/Users/jorgeeduardogarcia/Desktop/BIG_DATA/datos_ps3")
train_prop <- readRDS("~/Desktop/BIG_DATA/datos_ps3/dataPS3/train.Rds")
test_prop <- readRDS("~/Desktop/BIG_DATA/datos_ps3/dataPS3/test.Rds")

##Se cargan las librerías con la función P_load

install.packages("pacman")

require("pacman")
p_load(tidyverse,rio,
       sf, 
       leaflet,
       tmaptools, 
       osmdata, 
       skim,
       class)
install.packages("class")
require("class")


#Ahora se observa la base de datos
View(train_prop)
table(train_prop$l3)
skim(train_prop)

#Se obeservan los NA's de la superficie total y del área cubierta, pues son variables de interés en el precio de la vivienda
table(is.na(train_prop$surface_total)) # en el área total hay 79845 na's
table(is.na(train_prop$surface_covered)) # en la superficie cubierta hay 87368 na's

#Sin embargo, al momento de revisar la variable "descripción" se evidencia que el texto puede corroborar la información faltante reportada como na
##´Demostración con área total de la vivienda
attach(train_prop)
description
description[125]


#Se crea una sola variable que contiene la latitud y la longitud para poderlo visualizar
train_prop_sf = train_prop %>% st_as_sf(coords=c("lon", "lat"),crs=4326)

class(train_prop_sf)

#Plot mapas
leaflet() %>% addTiles() %>% addCircles(data=train_prop_sf)


##Ahora se va a extraer datos del texto


#Antes de crear el patrón, se cambian todas las letras d ela variable descripción de mayúsucla a minúscula
attach(train_prop_sf)
train_prop_sf$description <- str_to_lower(description)
train_prop_sf

##Estandarización ### Ahora se transforman las unidades de medidas, cambiándolas todas a mt2
train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "metros cuadrados" , 
                                            replacement = "mt2")

train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "mts" , 
                                            replacement = "mt2")

train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "mtrs2", 
                                            replacement = "mt2")


train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "m2" ,
                                            replacement = "mt2")
                                    

train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "mts2" ,
                                            replacement = "mt2")

train_prop_sf$description <-str_replace_all(train_prop_sf$description, pattern = "metros" ,
                                            replacement = "mt2")

##Se intentó con la sola letra "m". Sin embargo, reemplaza en todo lo que tenga la letra "m", por lo cual dessconfigura el texto de la variable descripcioón.

#Se hace la prueba con algunas filas para identificar si el cambio se realizó en la base 
train_prop_sf$description[59469]
train_prop_sf$description[59216]
train_prop_sf$description[39765]
train_prop_sf$description[14]
train_prop_sf$description[18]
                                            
                                           

#Se crea un patrón para extraer el área medida en metro cuadrado de la vivienda, de la variable descripción

x = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts" ## patrón


str_locate_all(string = train_prop$description[28] , pattern = x) ## detectar patrón

str_extract(string=train_prop$description[28] , pattern= x) ## extrac pattern

##3.2. Imputación de datos----



##3.3. Definición bases datos definitivas ----

# 4. ENSAYO: MAPA Y MEDICIÓN DE DISTANCIA ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Cargar las ciclovias (solo para ensayar)

ciclovias <- read_sf("./stores/Ciclovia/Ciclovia.shp")

ggplot()+
  geom_sf(data=ciclovias) +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


upla<-read_sf("./stores/upla/UPla.shp")


sitios_ref <- data.frame(place="Uniandes",
               lat=4.601590,
               long=-74.066391,
               nudge_y=-0.001)

sitios_ref <- sitios_ref %>% mutate(latp=lat,longp=long)

sitios_ref <- st_as_sf(sitios_ref,coords=c('longp','latp'),crs=4326)


#Graficar Bogotá con las ciclovías en azul y Uniandes como punto de referencia:

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Ejemplo de 6 sitios de la base de datos de Train (subset):

prop_subset <- head(train_prop)

prop_subset <- prop_subset %>% mutate(latp=lat,longp=lon)
prop_subset <- st_as_sf(prop_subset,coords=c('longp','latp'),crs=4326)


#Medición de distancias de las propiedades a Uniandes:

prop_subset$dist_a_Uniandes <-st_distance(prop_subset,sitios_ref)


#Se grafican las 6 propiedades en el mapa (junto con ciclovías y Uniandes, por ensayar)

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  geom_sf(data=prop_subset, col="orange") +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))



#Cargar localidades:

loc_bog <- read_sf("./stores/localidades_Bog_shp/Loca.shp")


#Se transforma toda la base de datos de Train y test:

train_prop <- train_prop %>% mutate(latp=lat,longp=lon)
train_prop <- st_as_sf(train_prop ,coords=c('longp','latp'),crs=4326)

test_prop <- test_prop %>% mutate(latp=lat,longp=lon)
test_prop <- st_as_sf(test_prop ,coords=c('longp','latp'),crs=4326)


#Validar los sistemas de coordenadas de los objetos:

st_crs(ciclovias)
st_crs(prop_subset)
st_crs(sitios_ref)
st_crs(upla)
st_crs(loc_bog)
st_crs(train_prop)

ciclovias
prop_subset
sitios_ref
upla
loc_bog
train_prop

#Transformar todos los sistemas de coordenadas a 4326
ciclovias<-st_transform(ciclovias, 4326)
prop_subset<-st_transform(prop_subset, 4326)
sitios_ref<-st_transform(sitios_ref, 4326)
upla<-st_transform(upla, 4326)
loc_bog<-st_transform(loc_bog, 4326)



#Se grafican las propiedades en el mapa (junto con ciclovías y Uniandes, por ensayar)

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=loc_bog
          %>% filter(grepl("CHAPINERO",LocNombre)==TRUE),
          fill = "gray")+
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  geom_sf(data=prop_subset, col="green") +
  geom_sf(data=test_prop
          %>% filter(grepl("Cundinamarca",l2)==TRUE),
          col="orange",
          size=1) +
    theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Solo Chapinero:

ggplot()+
  geom_sf(data=loc_bog
          %>% filter(grepl("CHAPINERO",LocNombre)==TRUE),
          fill = NA)+
  geom_sf(data=test_prop
          %>% filter(grepl("Cundinamarca",l2)==TRUE),
          col="orange",
          size=1) +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Ensayos con Leaflet:

leaflet() %>% addTiles() %>% addCircleMarkers(data=test_prop)
leaflet() %>% addTiles() %>% addCircles(data=test_prop)
leaflet() %>% addTiles() %>% addPolylines(data=ciclovias)
leaflet() %>% addTiles() %>% addPolygons(data=loc_bog)

#Solo Chapinero

leaflet() %>% addTiles() %>% addPolygons(data=loc_bog%>% filter(grepl("CHAPINERO",LocNombre)==TRUE))


#Propiedades de Test

leaflet() %>% addTiles() %>% 
  addPolygons(data=loc_bog %>% 
                filter(grepl("CHAPINERO",LocNombre)==TRUE)) %>% 
  addCircleMarkers(data=test_prop %>%
                     filter(grepl("Cundinamarca",l2)==TRUE),
                              col="red")


#Ensayos adicionales:

#Buscar sitio por nombre

uniandes <- geocode_OSM("Universidad de los Andes, Bogotá", as.sf=T)
uniandes

leaflet() %>% addTiles() %>% addCircles(data=uniandes)
uniandes <- st_transform(uniandes, 4326)


#Features:

available_features() %>% head(50)


#Estaciones de metro en Medellín:

## objeto osm
metromed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

metromed_sf <- metromed %>% osmdata_sf()
metromed_sf
metromed_station  <-  metromed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=metromed_station)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. MODELO BOGOTÁ D.C. ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. MODELO MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++