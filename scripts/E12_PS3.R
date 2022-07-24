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
       #gtsummary,
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
       xgboost,
       sfheaders,
       nngeo,
       parallel,
       lagsarlmtree,
       spdep)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_PS3")

train_prop <-readRDS("./stores/train.rds") #107.567 Obs
test_prop <-readRDS("./stores/test.rds") #11.150 Obs



##1.1. Exploración incial de los datos ----


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

##NAs Base Train 
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

##NAs Base Test
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



##1.3. Convertir las bases de datos en objetos geográficos ----

#Se transforma toda la base de datos de Train y test:

train_prop <- train_prop %>% mutate(latp=lat,longp=lon)
train_prop <- st_as_sf(train_prop ,coords=c('longp','latp'),crs=4326)

test_prop <- test_prop %>% mutate(latp=lat,longp=lon)
test_prop <- st_as_sf(test_prop ,coords=c('longp','latp'),crs=4326)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. SEPARAR LAS BASES/ BOGOTÁ D.C. Y MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Base de Bogotá D.C.
train_bog <-subset(train_prop,train_prop$l3 =="Bogotá D.C")
test_bog <-subset(test_prop,test_prop$l3 =="Bogotá D.C")                   


#Base de Medellín
train_med <-subset(train_prop,train_prop$l3 =="Medellín")
test_med <-subset(test_prop,test_prop$l3 =="Medellín")   


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. VARIABLES DE TEXTO E IMPUTACIÓN DE DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##3.1. Variables de texto----

##3.2. Imputación de datos----

##3.3. Definición bases datos definitivas ----






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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




## 5.3. FEATURES DE OSM: ----

### 5.3.1. Estaciones transporte público Bogotá: ----

## objeto osm
tpublbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

tpublbog_sf <- tpublbog %>% osmdata_sf()
tpublbog_sf
tpublbog_station  <-  tpublbog_sf$osm_points

#leaflet() %>% addTiles() %>% addCircleMarkers(data=tpublbog_station)

## Distancia de las viviendas a las estaciones
dist_tpublb_test  <-  st_distance(x=test_bog, y=tpublbog_station)
dist_tpublb_train  <-  st_distance(x=train_bog, y=tpublbog_station)

## Distancia mínima
min_dist_tpublb_test  <-  apply(dist_tpublb_test,1,min)
min_dist_tpublb_train  <-  apply(dist_tpublb_train,1,min)

test_bog$dist_tpubl <- min_dist_tpublb_test
train_bog$dist_tpubl <- min_dist_tpublb_train


### 5.3.2. Hospitales y clínicas en Bogotá: ----

## objeto osm
hospbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital")

clinbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="clinic") 

hospbog_sf <- hospbog %>% osmdata_sf()
clinbog_sf <- clinbog %>% osmdata_sf()

hospbog_sf
clinbog_sf

hosp_bog  <-  hospbog_sf$osm_points
clin_bog  <-  clinbog_sf$osm_points

colnames(hosp_bog)
colnames(clin_bog)
compare_df_cols(hosp_bog, clin_bog)

nrow(hosp_bog)
nrow(clin_bog)
nrow(hosp_bog) + nrow(clin_bog)

#Se unen las filas por columnas comunes de clínicas y hospitales
hosp_bog <- rbind(hosp_bog[intersect(colnames(hosp_bog), colnames(clin_bog))],
                  clin_bog[intersect(colnames(hosp_bog), colnames(clin_bog))])

nrow(hosp_bog)
colnames(hosp_bog)

#leaflet() %>% addTiles() %>% 
#  addCircleMarkers(data=hosp_bog,color="blue")

## Distancia de las viviendas a clínicas u hospitales
dist_hospb_test  <-  st_distance(x=test_bog, y=hosp_bog)
dist_hospb_train  <-  st_distance(x=train_bog, y=hosp_bog)

## Distancia mínima
min_dist_hospb_test  <-  apply(dist_hospb_test,1,min)
min_dist_hospb_train  <-  apply(dist_hospb_train,1,min)

test_bog$dist_hosp <- min_dist_hospb_test
train_bog$dist_hosp <- min_dist_hospb_train


### 5.3.3. Centros comerciales en Bogotá: ----

## objeto osm
ccombog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 

ccombog_sf <- ccombog %>% osmdata_sf()
ccomerc_bog  <-  ccombog_sf$osm_points

#leaflet() %>% addTiles() %>% addCircleMarkers(data=ccomerc_bog)

## Distancia de las viviendas a Centros comerciales
dist_ccomercb_test  <-  st_distance(x=test_bog, y=ccomerc_bog)
dist_ccomercb_train  <-  st_distance(x=train_bog, y=ccomerc_bog)

## Distancia mínima
min_dist_ccomercb_test  <-  apply(dist_ccomercb_test,1,min)
min_dist_ccomercb_train  <-  apply(dist_ccomercb_train,1,min)

test_bog$dist_ccomerc <- min_dist_ccomercb_test
train_bog$dist_ccomerc <- min_dist_ccomercb_train


### 5.3.4. Parques en Bogotá: ----

## objeto osm
parkbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="leisure" , value="park")

parkbog_sf <- parkbog %>% osmdata_sf()
park_bog  <-  parkbog_sf$osm_points

#leaflet() %>% addTiles() %>% addCircleMarkers(data=park_bog)

## Distancia de las viviendas a parques
dist_parkb_test  <-  st_distance(x=test_bog, y=park_bog)
dist_parkb_train  <-  st_distance(x=train_bog, y=park_bog)

## Distancia mínima
min_dist_parkb_test  <-  apply(dist_parkb_test,1,min)
min_dist_parkb_train  <-  apply(dist_parkb_train,1,min)

test_bog$dist_park <- min_dist_parkb_test
train_bog$dist_park <- min_dist_parkb_train


colSums(is.na(train_bog))
colSums(is.na(test_bog))





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. MODELO MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##6.1. PRELIMINARES Y GRÁFICOS ----

#Graficar las propiedades de Medellín:

#Todas juntas
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=train_med,color="green") %>% 
  addCircleMarkers(data=test_med,color="blue")

#Solo test
leaflet() %>% addTiles() %>% addCircleMarkers(data=test_med,color="blue")

#Solo train
leaflet() %>% addTiles() %>% addCircleMarkers(data=train_med,color="green")


#Comunas de Medellín:

El_Poblado <- getbb(place_name = "Comuna 14 - El Poblado Medellín", 
                   featuretype = "place:suburb", 
                   format_out = "sf_polygon")

class(El_Poblado)
st_crs(El_Poblado)

leaflet() %>% addTiles() %>% addPolygons(data=El_Poblado)




#Todos los barrios de Medellín
leaflet() %>% addTiles() %>% addPolygons(data=barrios_med,label=barrios_med$NOMBRE)

#Solo los barrios de El Poblado
leaflet() %>% addTiles() %>% 
  addPolygons(data=barrios_med %>% filter(grepl(14,LIMITECOMU)==TRUE),
              label=barrios_med$NOMBRE)

#####TEMP:

#Base de Medellín
train_med <-subset(train_prop,train_prop$l3 =="Medellín")
test_med <-subset(test_prop,test_prop$l3 =="Medellín")   



##6.2 INFORMACIÓN GEOGRÁFICA----

#A cada observación le quiero agregar:

#Comuna / Barrio / Estrato / Manzana / ...


#####Comuna: ----

catastro_med <- read_sf("./stores/Medellín/shp_SECTOR_CATASTRAL/SECTOR_CATASTRAL.shp")
st_crs(catastro_med)
catastro_med <- st_transform(catastro_med,4326)
colnames(catastro_med)

train_med <- st_join(train_med,catastro_med[,c('COMUNA','NOMBRE')])
test_med <- st_join(test_med,catastro_med[,c('COMUNA','NOMBRE')])

train_med$COD_COMUNA <- train_med$COMUNA
test_med$COD_COMUNA <- test_med$COMUNA

train_med$COMUNA <- train_med$NOMBRE
test_med$COMUNA <- test_med$NOMBRE

train_med$NOMBRE <- NULL
test_med$NOMBRE <- NULL

colnames(test_med)


#####Barrio: ----

barrios_med <- read_sf("./stores/Medellín/shp_BarrioVereda/BarrioVereda_2014.shp")
st_crs(barrios_med)
barrios_med <- st_transform(barrios_med,4326)
colnames(barrios_med)

train_med <- st_join(train_med,barrios_med[,c('CODIGO','NOMBRE')])
test_med <- st_join(test_med,barrios_med[,c('CODIGO','NOMBRE')])

train_med$COD_BARRIO <- train_med$CODIGO
test_med$COD_BARRIO <- test_med$CODIGO

train_med$BARRIO <- train_med$NOMBRE
test_med$BARRIO <- test_med$NOMBRE

train_med$NOMBRE <- NULL
test_med$NOMBRE <- NULL
train_med$CODIGO <- NULL
test_med$CODIGO <- NULL

colnames(test_med)

colSums(is.na(train_med))
colSums(is.na(test_med))


#### ASIGNACIÓN VARIABLES GEOGRÁFICAS: Estrato, uso del suelo.

#El siguiente código es demorado; se puede saltar a la sección
#6.2.5. en donde se cargan los datos calculados en las proóximas secciones.


# 
# #####Estrato: ----
# 
# estratos_med <- read_sf("./stores/Medellín/shp_ESTRATIFICACION/ESTRATIFICACION.shp")
# st_crs(estratos_med)
# estratos_med <- st_transform(estratos_med,4326)
# colnames(estratos_med)
# 
# 
# #Remover 346 polígonos que tienen estrato pero no manzana (calles)
# colSums(is.na(estratos_med))
# estratos_med <- estratos_med %>% filter(!is.na(estratos_med$MANZANA))
# colSums(is.na(estratos_med))
# 
# estratos_med$geom_err <- st_is_valid(estratos_med, reason = T)
# nrow(estratos_med)
# table(estratos_med$geom_err)
# 
# 31618-31618
# #301 errores en las geometrías
# 
# estratos_med <- st_make_valid(estratos_med)
# 
# estratos_med$geom_err <- st_is_valid(estratos_med, reason = T)
# nrow(estratos_med)
# table(estratos_med$geom_err)
# #Quedan solo 4 errores en las geometrías
# 
# estratos_med <- filter(estratos_med,estratos_med$geom_err == "Valid Geometry")
# nrow(estratos_med)
# #Se eliminaron las 4 geometrías malas
# 
# #Remover geometrías vacías:
# table(st_is_empty(estratos_med))
# estratos_med <- estratos_med %>% filter(!st_is_empty(.))
# 
# 
# train_med <- st_join(train_med,estratos_med[,c('MANZANA','ESTRATO')])
# test_med <- st_join(test_med,estratos_med[,c('MANZANA','ESTRATO')])

# colSums(is.na(train_med))
# colSums(is.na(test_med))
# 
# #Corrección por vecinos por los NA
# 
# #Crear la base de estratos para la Comuna de El Poblado:
# estr_med_pobl <- filter(estratos_med,COMUNA==14)
# 
# #Dividir Train entre las que sí encontró estrato/manzana y los NA (al final se unen)
# train_med_estr_ok <- filter(train_med,!(is.na(train_med$MANZANA)))
# train_med_estr_na <- filter(train_med,is.na(train_med$MANZANA))
# 
# nrow(train_med_estr_ok)+nrow(train_med_estr_na)
# nrow(train_med)
# 
# train_med_estr_na$MANZANA <- NULL
# train_med_estr_na$ESTRATO <- NULL
# 
# 
# #Dividir Test entre las que sí encontró estrato/manzana y los NA (al final se unen)
# test_med_estr_ok <- filter(test_med,!(is.na(test_med$MANZANA)))
# test_med_estr_na <- filter(test_med,is.na(test_med$MANZANA))
# 
# nrow(test_med_estr_ok)+nrow(test_med_estr_na)
# nrow(test_med)
# 
# test_med_estr_na$MANZANA <- NULL
# test_med_estr_na$ESTRATO <- NULL
# 
# #Ejecución del Join con Max Dist = 50m
# 
# #Para test:
# 
# start_test = Sys.time()
# test_med_estr_na <- st_join(test_med_estr_na,estr_med_pobl[,c('MANZANA','ESTRATO')],
#                            join = st_nn, k = 1, maxdist = 50, parallel=8)
# end_test = Sys.time()
# end_test - start_test
# 
# colSums(is.na(test_med_estr_na))
# 
# test_med_estr_na_df <- sf_to_df(test_med_estr_na, fill = TRUE, unlist = NULL)
# saveRDS(test_med_estr_na_df,"./stores/Medellín/rds_calculados/ESTRATO_test_NA.rds")
# 
# colSums(is.na(test_med))
# test_med <- rbind(test_med_estr_ok,test_med_estr_na)
# colSums(is.na(test_med))
# 
# nrow(test_med)
# 
# test_med_estrato_df <- sf_to_df(test_med, fill = TRUE, unlist = NULL)
# saveRDS(test_med_estrato_df,"./stores/Medellín/rds_calculados/ESTRATO_test.rds")
# 
# 
# 
# #Para train:
# 
# start_train = Sys.time()
# train_med_estr_na <- st_join(train_med_estr_na,estratos_med[,c('MANZANA','ESTRATO')],
#                             join = st_nn, k = 1, maxdist = 50, parallel=8)
# end_train = Sys.time()
# end_train - start_train
# 
# colSums(is.na(train_med_estr_na))
# 
# train_med_estr_na_df <- sf_to_df(train_med_estr_na, fill = TRUE, unlist = NULL)
# saveRDS(train_med_estr_na_df,"./stores/Medellín/rds_calculados/ESTRATO_train_NA.rds")
# 
# colSums(is.na(train_med))
# train_med <- rbind(train_med_estr_ok,train_med_estr_na)
# colSums(is.na(train_med))
# 
# nrow(train_med)
# 
# train_med_estrato_df <- sf_to_df(train_med, fill = TRUE, unlist = NULL)
# saveRDS(train_med_estrato_df,"./stores/Medellín/rds_calculados/ESTRATO_train.rds")
# 
# 
# #Revisión luego de imputar el Uso:
# colSums(is.na(train_med))
# colSums(is.na(test_med))
# 
# #En test quedan dos observaciones sin estrato y sin manzana.
# #Se revisan y son dos observaciones "basura", erróneas en Test.
# 
# # 89162e83a15bd128e9834fc2
# # 4133ae5fc32d14793a9968d2
# 
# leaflet() %>% addTiles() %>% 
#   addCircleMarkers(data=test_med %>% filter(is.na(test_med$MANZANA)),
#                    color="red",label=test_med$ESTRATO)
# 
# 
# 
# 
# #### Uso del suelo: ----
# 
# usos_med <- read_sf("./stores/Medellín/shp_UsosSueloUrbano/UsosGnalesSueloUrbano.shp")
# st_crs(usos_med)
# usos_med <- st_transform(usos_med,4326)
# 
# usos_med$geom_err <- st_is_valid(usos_med, reason = T)
# nrow(usos_med)
# table(usos_med$geom_err)
# 
# 25448-25343
# #105 errores en las geometrías
# 
# usos_med <- st_make_valid(usos_med)
# 
# usos_med$geom_err <- st_is_valid(usos_med, reason = T)
# nrow(usos_med)
# table(usos_med$geom_err)
# 
# usos_med <- filter(usos_med,usos_med$geom_err == "Valid Geometry")
# nrow(usos_med)
# #Se elimina la geometría mala
# 
# colnames(usos_med)
# train_med <- st_join(train_med,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")])
# test_med <- st_join(test_med,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")])
# 
# colSums(is.na(train_med))
# colSums(is.na(test_med))
# 
# 
# #Corrección por vecinos por los NA
# 
# #Crear la base de usos para la Comuna de El Poblado:
# usos_med_pobl <- st_join(usos_med,catastro_med[,c('COMUNA','NOMBRE')])
# usos_med_pobl <- filter(usos_med_pobl,usos_med_pobl$COMUNA==14)
# 
# #Dividir Train entre las que sí encontró Uso y los NA (al final se unen)
# train_med_uso_ok <- filter(train_med,!(is.na(train_med$AREAGRALUS)))
# train_med_uso_na <- filter(train_med,is.na(train_med$AREAGRALUS))
# 
# nrow(train_med_uso_ok)+nrow(train_med_uso_na)
# nrow(train_med)
# 
# #Elimino las columnas con NA para poder hacer un nuevo Join
# train_med_uso_na$COD_CAT_US <- NULL
# train_med_uso_na$COD_SUBCAT <- NULL
# train_med_uso_na$AREAGRALUS <- NULL
# train_med_uso_na$SUBCATEGOR <- NULL
# 
# #Dividir Test entre las que sí encontró Uso y los NA (al final se unen)
# test_med_uso_ok <- filter(test_med,!(is.na(test_med$AREAGRALUS)))
# test_med_uso_na <- filter(test_med,is.na(test_med$AREAGRALUS))
# 
# nrow(test_med_uso_ok)+nrow(test_med_uso_na)
# nrow(test_med)
# 
# #Elimino las columnas con NA para poder hacer un nuevo Join
# test_med_uso_na$COD_CAT_US <- NULL
# test_med_uso_na$COD_SUBCAT <- NULL
# test_med_uso_na$AREAGRALUS <- NULL
# test_med_uso_na$SUBCATEGOR <- NULL
# 
# 
# #Ejecución del Join con Max Dist = 50m
# 
# #Para test:
# 
# start_test = Sys.time()
# test_med_uso_na <- st_join(test_med_uso_na,usos_med_pobl[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")],
#                            join = st_nn, k = 1, maxdist = 50, parallel=3)
# end_test = Sys.time()
# end_test - start_test
# 
# #Reviso si quedaron NA
# colSums(is.na(test_med_uso_na))
# 
# #Guardo en un DataFrame el resultado de la búsqueda de USOS
# test_med_uso_na_df <- sf_to_df(test_med_uso_na, fill = TRUE, unlist = NULL)
# saveRDS(test_med_uso_na_df,"./stores/Medellín/rds_calculados/USOS_test_NA.rds")
# 
# colSums(is.na(test_med))
# test_med <- rbind(test_med_uso_ok,test_med_uso_na)
# colSums(is.na(test_med))
# 
# nrow(test_med)
# 
# test_med_usos_df <- sf_to_df(test_med, fill = TRUE, unlist = NULL)
# saveRDS(test_med_usos_df,"./stores/Medellín/rds_calculados/USOS_test.rds")
# 
# 
# 
# #Ejecución del Join con Max Dist = 50m
# 
# #Para Train:
# 
# start_train = Sys.time()
# train_med_uso_na <- st_join(train_med_uso_na,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")],
#                            join = st_nn, k = 1, maxdist = 50, parallel=3)
# end_train = Sys.time()
# end_train - start_train
# 
# #Reviso si quedaron NA
# colSums(is.na(train_med_uso_na))
# 
# #Guardo en un DataFrame el resultado de la búsqueda de USOS
# #(para evitar correr de nuevo el código, que se demora)
# 
# train_med_uso_na_df <- sf_to_df(train_med_uso_na, fill = TRUE, unlist = NULL)
# saveRDS(train_med_uso_na_df,"./stores/Medellín/rds_calculados/USOS_NA_train.rds")
# 
# colSums(is.na(train_med))
# train_med <- rbind(train_med_uso_ok,train_med_uso_na)
# colSums(is.na(train_med))
# 
# nrow(train_med)
# 
# train_med_usos_df <- sf_to_df(train_med, fill = TRUE, unlist = NULL)
# saveRDS(train_med_usos_df,"./stores/Medellín/rds_calculados/USOS_train.rds")
# 
# 
# 
# #Revisión luego de imputar el Uso:
# colSums(is.na(train_med))
# colSums(is.na(test_med))



#### 6.2.5. Carga de las variables geográficas calculadas: ----

#Cargo las bases de datos:
estrm_test <-readRDS("./stores/Medellín/rds_calculados/ESTRATO_test.rds")
estrm_train <-readRDS("./stores/Medellín/rds_calculados/ESTRATO_train.rds")
usosm_test <-readRDS("./stores/Medellín/rds_calculados/USOS_test.rds")
usosm_train <-readRDS("./stores/Medellín/rds_calculados/USOS_train.rds")


colnames(estrm_test)
estrm_test <- estrm_test[,c('property_id', 'MANZANA','ESTRATO')]
estrm_train <- estrm_train[,c('property_id', 'MANZANA','ESTRATO')]

colnames(usosm_test)
usosm_test <- usosm_test[,c('property_id','COD_CAT_US',
                            'COD_SUBCAT','AREAGRALUS','SUBCATEGOR')]
usosm_train <- usosm_train[,c('property_id','COD_CAT_US',
                            'COD_SUBCAT','AREAGRALUS','SUBCATEGOR')]

test_med <- left_join(test_med,estrm_test,by="property_id")
train_med <- left_join(train_med,estrm_train,by="property_id")

test_med <- left_join(test_med,usosm_test,by="property_id")
train_med <- left_join(train_med,usosm_train,by="property_id")

colSums(is.na(train_med))
colSums(is.na(test_med))



## 6.3. FEATURES DE OSM: ----

### 6.3.1. Estaciones de metro en Medellín: ----

## objeto osm
metromed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

metromed_sf <- metromed %>% osmdata_sf()
metromed_sf
metromed_station  <-  metromed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=metromed_station)

## Distancia de las viviendas a Estación metro
dist_metro_test  <-  st_distance(x=test_med, y=metromed_station)
dist_metro_train  <-  st_distance(x=train_med, y=metromed_station)

## Distancia mínima
min_dist_metro_test  <-  apply(dist_metro_test,1,min)
min_dist_metro_train  <-  apply(dist_metro_train,1,min)

test_med$dist_metro <- min_dist_metro_test
train_med$dist_metro <- min_dist_metro_train


### 6.3.2. Hospitales y clínicas en Medellín: ----

## objeto osm
hospmed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital")

clinmed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="clinic") 

hospmed_sf <- hospmed %>% osmdata_sf()
clinmed_sf <- clinmed %>% osmdata_sf()

hospmed_sf
clinmed_sf

hosp_med  <-  hospmed_sf$osm_points
clin_med  <-  clinmed_sf$osm_points

colnames(hosp_med)
colnames(clin_med)
compare_df_cols(hosp_med, clin_med)

nrow(hosp_med)
nrow(clin_med)
nrow(hosp_med) + nrow(clin_med)

#Se unen las filas por columnas comunes de clínicas y hospitales
hosp_med <- rbind(hosp_med[intersect(colnames(hosp_med), colnames(clin_med))],
             clin_med[intersect(colnames(hosp_med), colnames(clin_med))])

nrow(hosp_med)
colnames(hosp_med)

leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=hosp_med,color="blue")

## Distancia de las viviendas a clínicas u hospitales
dist_hospm_test  <-  st_distance(x=test_med, y=hosp_med)
dist_hospm_train  <-  st_distance(x=train_med, y=hosp_med)

## Distancia mínima
min_dist_hospm_test  <-  apply(dist_hospm_test,1,min)
min_dist_hospm_train  <-  apply(dist_hospm_train,1,min)

test_med$dist_hosp <- min_dist_hospm_test
train_med$dist_hosp <- min_dist_hospm_train


### 6.3.3. Centros comerciales en Medellín: ----

## objeto osm
ccommed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 

ccommed_sf <- ccommed %>% osmdata_sf()
ccomerc_med  <-  ccommed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=ccomerc_med)

## Distancia de las viviendas a Centros comerciales
dist_ccomercm_test  <-  st_distance(x=test_med, y=ccomerc_med)
dist_ccomercm_train  <-  st_distance(x=train_med, y=ccomerc_med)

## Distancia mínima
min_dist_ccomercm_test  <-  apply(dist_ccomercm_test,1,min)
min_dist_ccomercm_train  <-  apply(dist_ccomercm_train,1,min)

test_med$dist_ccomerc <- min_dist_ccomercm_test
train_med$dist_ccomerc <- min_dist_ccomercm_train


### 6.3.4. Parques en Medellín: ----

## objeto osm
parkmed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="leisure" , value="park")

parkmed_sf <- parkmed %>% osmdata_sf()
park_med  <-  parkmed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=park_med)

## Distancia de las viviendas a parques
dist_parkm_test  <-  st_distance(x=test_med, y=park_med)
dist_parkm_train  <-  st_distance(x=train_med, y=park_med)

## Distancia mínima
min_dist_parkm_test  <-  apply(dist_parkm_test,1,min)
min_dist_parkm_train  <-  apply(dist_parkm_train,1,min)

test_med$dist_park <- min_dist_parkm_test
train_med$dist_park <- min_dist_parkm_train


colSums(is.na(train_med))
colSums(is.na(test_med))

export(train_med,"./stores/Medellín/train_med.rds")
export(test_med,"./stores/Medellín/test_med.rds")


##6.4. AVALÚO A PARTIR DEL OBSERVATORIO INMOBILIARIO DE MEDELLÍN ----

train_med <- import("./stores/Medellín/train_med.rds")
test_med <- import("./stores/Medellín/train_med.rds")

#Archivos del Observatorio Inmobiliario de Medellín:
#Fuente: 
# 
# #Explorar los archivos KML
# st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2022.kml")
# st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml")
# st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2020.kml")
# st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2019.kml")
# st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2018.kml")
# 
# 
# OIME_2022_apts <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2022.kml",layer="Apartamentos")
# OIME_2022_casas <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2022.kml",layer="Casas")
# OIME_2021_apts_us <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml",layer="APARTAMENTOS USADOS.xlsx")
# OIME_2021_apts_nuev <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml",layer="APARTAMENTOS NUEVOS.xlsx")
# OIME_2021_casas <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml",layer="CASAS.xlsx")
# OIME_2020_apts <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2020.kml", layer="APARTAMENTOS.xlsx")
# OIME_2020_casas <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2020.kml", layer="CASAS.xlsx")
# OIME_2019_apts <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2019.kml",layer="APARTAMENTOS.xlsx")
# OIME_2019_casas <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2019.kml",layer="CASAS.xlsx")
# OIME_2018_apts <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2018.kml", layer="APARTAMENTOS.xlsx")
# OIME_2018_casas <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2018.kml", layer="CASAS.xlsx")
# 
# OIME_2022_apts$YEAR <- 2022
# OIME_2022_casas$YEAR <- 2022
# OIME_2021_apts_us$YEAR <- 2021
# OIME_2021_apts_nuev$YEAR <- 2021
# OIME_2021_casas$YEAR <- 2021
# OIME_2020_apts$YEAR <- 2020
# OIME_2020_casas$YEAR <- 2020
# OIME_2019_apts$YEAR <- 2019
# OIME_2019_casas$YEAR <- 2019
# OIME_2018_apts$YEAR <- 2018
# OIME_2018_casas$YEAR <- 2018
# 
# 
# OIME <- rbind(
#   OIME_2022_apts,
#   OIME_2022_casas,
#   OIME_2021_apts_us,
#   OIME_2021_apts_nuev,
#   OIME_2021_casas,
#   OIME_2020_apts,
#   OIME_2020_casas,
#   OIME_2019_apts,
#   OIME_2019_casas,
#   OIME_2018_apts,
#   OIME_2018_casas)
# 
# leaflet() %>% addTiles() %>% 
#   addCircleMarkers(data=OIME,popup = OIME$Description, color ="green")
# 
# 
# OIME <- st_join(OIME,estratos_med[,c('MANZANA','ESTRATO')])
# 
# colSums(is.na(OIME))
# 
# OIME$Descripcion <- gsub("<br>","|",OIME$Descripcion)
# 
# colnames(OIME)
# 
# OIME$Description <- NULL
# 
# OIME_info  <- data.frame(apply(OIME[ , colnames(OIME)], 1, paste, collapse = "|" ))
# 
# export(OIME_info,"./stores/Medellín/OIME_ofertas/OIME_info.xlsx")
# 
# barrios_test <- data.frame(unique(test_med$BARRIO))
# colnames(barrios_test) <- "BARRIOS"
# 
# barrios_train <- data.frame(unique(train_med$BARRIO))
# colnames(barrios_train) <- "BARRIOS"
# 
# barrios <- rbind(barrios_test,barrios_train)
# barrios <- unique(barrios)
# 
# rm(list=c('barrios_test','barrios_train'))
# 
# 
# #SE SEPARA TEXTO EN COLUMNAS EN EXCEL; EN R, FALLA Y ES COMPLICADO!!!!

OIME <- import("./stores/Medellín/OIME_ofertas/OIME_info_proc.xlsx")

colnames(OIME)

#Expresiones regulares para ajustar los barrios:

#En train: 6645 barrios NA
#En test: 1196 barrios NA

#Ideas:
#Todo mayusc
#Quitar tildes
#Quitar espacios

OIME$BARRIO <- gsub(""," NO.", OIME$Barrio)

OIME$BARRIO <- gsub(" # "," NO.", OIME$Barrio)
OIME$Barrio <- NULL

OIME$BARRIO <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(OIME$BARRIO))
test_med$BARRIO <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(test_med$BARRIO))
train_med$BARRIO <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(train_med$BARRIO))

OIME$BARRIO <- gsub("[[:space:]]", "", OIME$BARRIO)
test_med$BARRIO <- gsub("[[:space:]]", "", test_med$BARRIO)
train_med$BARRIO <- gsub("[[:space:]]", "", train_med$BARRIO)

OIME$ESTRATO <- OIME$Estrato
OIME$Estrato <- NULL


train_barrios_NA <- filter(train_med,is.na(train_med$Media_m2_barr.y))
barrios_NA <- data.frame(table(train_barrios_NA$BARRIO))

#Corrección de los nombres de barrios más representativos:

OIME$BARRIO  <-  case_when(
  OIME$BARRIO=="LOMADELOSBERNAL" ~ "LALOMADELOSBERNAL",
  OIME$BARRIO=="ZONADEEXPANSIONBELENRINCON" ~ "AREADEEXPANSIONBELENRINCON",
  OIME$BARRIO=="CABECERASANANTONIODEPRADO" ~ "SANANTONIODEPRADO",
  OIME$BARRIO=="ELESTADIO" ~ "ESTADIO",
  OIME$BARRIO=="ZONAEXPANSIONPAJARITO" ~ "AREADEEXPANSIONPAJARITO",
  OIME$BARRIO=="AEROPARQUEJUANPABLOII" ~ "PARQUEJUANPABLOII",
  OIME$BARRIO=="NUEVAVILLADEABURRA" ~ "NUEVAVILLADELABURRA",
  OIME$BARRIO=="BARRIODEJESUS" ~ "BARRIOSDEJESUS",
  OIME$BARRIO=="CATALUNA" ~ "CATALUÑA",
  OIME$BARRIO=="ZONAEXPANSIONSANANTONIO" ~ "AREADEEXPANSIONSANANTONIODEPRADO",
  OIME$BARRIO=="SUBURB.SANJOSEDELMANZANILLO" ~ "SANJOSEDELMANZANILLO",
  OIME$BARRIO=="BOLIVARIANA" ~ "U.P.B",
  OIME$BARRIO=="LAPINUELA" ~ "LAPIÑUELA",
  TRUE ~ OIME$BARRIO
)



#Mejorar el Avalúo - Ajuste por inflación?

test_med$año_anunc <- str_sub(test_med$start_date,1,4)
table(test_med$año_anunc)

train_med$año_anunc <- str_sub(train_med$start_date,1,4)
table(train_med$año_anunc)

#Del observatorio tengo observaciones entre 2018-2022.
#Cae dentro del rango de fechas, en principio no veo necesario ajustar
#por inflación.


#PENDIENTE TERMINAR SI ALGO:
# OIME_colnames <- c('ID', 'Geometry', 'Year', 'COBAMA', 'Estrato',
#                    'Fecha_ini','Tipo_oferta','Tipo_predio','Estado','Barrio','Estrato2',
#                    'Area_priv','Area_lot','Valor_com','Valor_m2','long2','lat2')
# 
# OIME_colnames
# ensayo <- data.frame(OIME_info[1:5,])
# 
# ensayo <- separate(ensayo,col=1, into=OIME_colnames, sep="|")
# warnings()
# 
# ?separate

#Comparar BARRIOS:

#Resumen de diferencias
all_equal(barrios,barrios_j)
comparedf(barrios,barrios_j)

#Comparación de variables (columnas)
compare_df_cols(barrios,barrios_j)

#Comparación detallada:
comparacion_barrios <- summary(comparedf(barrios,barrios_j))




#PENDIENTE AJUSTAR INFLACION

#Promedios

agreg_OIME_mzn <- OIME %>% 
  group_by(COBAMA) %>%
  summarize(Media_m2_mzn=mean(Valor_m2),
            Mediana_m2_mzn=median(Valor_m2))

agreg_OIME_barr <- OIME %>% 
  group_by(BARRIO) %>%
  summarize(Media_m2_barr=mean(Valor_m2),
            Mediana_m2_barr=median(Valor_m2))

agreg_OIME_estr <- OIME %>% 
  group_by(ESTRATO) %>%
  summarize(Media_m2_estr=mean(Valor_m2),
            Mediana_m2_estr=median(Valor_m2))

test_med$COBAMA <- test_med$MANZANA
train_med$COBAMA <- train_med$MANZANA

test_med <- left_join(test_med,agreg_OIME_mzn,by="COBAMA")
train_med <- left_join(train_med,agreg_OIME_mzn,by="COBAMA")

test_med <- left_join(test_med,agreg_OIME_barr,by="BARRIO")
train_med <- left_join(train_med,agreg_OIME_barr,by="BARRIO")

test_med <- left_join(test_med,agreg_OIME_estr,by="ESTRATO")
train_med <- left_join(train_med,agreg_OIME_estr,by="ESTRATO")

colSums(is.na(test_med))
colSums(is.na(train_med))




#Case: mejor avalúo disponible: Media
test_med$mejor_val_m2_mean  <-  case_when(
  !is.na(test_med$Media_m2_mzn) ~ as.double(test_med$Media_m2_mzn),
  !is.na(test_med$Media_m2_barr) ~ as.double(test_med$Media_m2_barr),
  !is.na(test_med$Media_m2_estr) ~ as.double(test_med$Media_m2_estr))

train_med$mejor_val_m2_mean  <-  case_when(
  !is.na(train_med$Media_m2_mzn) ~ as.double(train_med$Media_m2_mzn),
  !is.na(train_med$Media_m2_barr) ~ as.double(train_med$Media_m2_barr),
  !is.na(train_med$Media_m2_estr) ~ as.double(train_med$Media_m2_estr))


#Case: mejor avalúo disponible: Mediana
test_med$mejor_val_m2_median  <-  case_when(
  !is.na(test_med$Mediana_m2_mzn) ~ as.double(test_med$Mediana_m2_mzn),
  !is.na(test_med$Mediana_m2_barr) ~ as.double(test_med$Mediana_m2_barr),
  !is.na(test_med$Mediana_m2_estr) ~ as.double(test_med$Mediana_m2_estr))

train_med$mejor_val_m2_median  <-  case_when(
  !is.na(train_med$Mediana_m2_mzn) ~ as.double(train_med$Mediana_m2_mzn),
  !is.na(train_med$Mediana_m2_barr) ~ as.double(train_med$Mediana_m2_barr),
  !is.na(train_med$Mediana_m2_estr) ~ as.double(train_med$Mediana_m2_estr))


test_med$val_tot_mean <- test_med$mejor_val_m2_mean * test_med$surface_total
test_med$val_tot_median <- test_med$mejor_val_m2_median * test_med$surface_total
train_med$val_tot_mean <- train_med$mejor_val_m2_mean * train_med$surface_total
train_med$val_tot_median <- train_med$mejor_val_m2_median * train_med$surface_total


colSums(is.na(test_med))
colSums(is.na(train_med))


train_subset <- train_med[,c("description","price","val_tot_mean","val_tot_median")]
train_subset <- filter(train_subset,!(is.na(train_subset$val_tot_mean)))
train_subset$MSE_mean <- (train_subset$price - train_subset$val_tot_mean)^2
train_subset$MSE_median <- (train_subset$price - train_subset$val_tot_median)^2

colnames(train_subset)

sapply(train_subset, mean)



###Imputar área en m2, con la mediana ----

#1) Del observatorio
#2) De la BD
#3) Ambos

#Por ahora, de la OIME
agr_area_OIME_mzn <- OIME %>% 
  group_by(COBAMA) %>%
  summarize(area_mzn_median=median(Area_priv),
            num_obs_area=n())

agr_area_OIME_barr <- OIME %>% 
  group_by(BARRIO) %>%
  summarize(area_barr_median=median(Area_priv),
            num_obs_area=n())

agr_area_OIME_estr <- OIME %>% 
  group_by(ESTRATO) %>%
  summarize(area_estr_median=median(Area_priv),
            num_obs_area=n())


test_med <- left_join(test_med,agr_area_OIME_mzn,by="COBAMA")
train_med <- left_join(train_med,agr_area_OIME_mzn,by="COBAMA")

test_med <- left_join(test_med,agr_area_OIME_barr,by="BARRIO")
train_med <- left_join(train_med,agr_area_OIME_barr,by="BARRIO")

test_med <- left_join(test_med,agr_area_OIME_estr,by="ESTRATO")
train_med <- left_join(train_med,agr_area_OIME_estr,by="ESTRATO")

colSums(is.na(test_med))
colSums(is.na(train_med))


#Case: área m2: Mediana
test_med$area_OIME_median  <-  case_when(
  !is.na(test_med$surface_total) ~ test_med$surface_total,
  !is.na(test_med$area_mzn_median) ~ as.double(test_med$area_mzn_median),
  !is.na(test_med$area_barr_median) ~ as.double(test_med$area_barr_median),
  !is.na(test_med$area_estr_median) ~ as.double(test_med$area_estr_median))

train_med$area_OIME_median  <-  case_when(
  !is.na(train_med$surface_total) ~ train_med$surface_total,
  !is.na(train_med$area_mzn_median) ~ as.double(train_med$area_mzn_median),
  !is.na(train_med$area_barr_median) ~ as.double(train_med$area_barr_median),
  !is.na(train_med$area_estr_median) ~ as.double(train_med$area_estr_median))


colSums(is.na(test_med))
colSums(is.na(train_med))

test_med$val_tot_area_OIME <- test_med$mejor_val_m2_median * test_med$area_OIME_median
train_med$val_tot_area_OIME <- train_med$mejor_val_m2_median * train_med$area_OIME_median


colSums(is.na(test_med))
colSums(is.na(train_med))


train_subset <- train_med[,c("description","price","val_tot_mean","val_tot_median","val_tot_area_OIME")]
train_subset <- filter(train_subset,!(is.na(train_subset$val_tot_area_OIME)))
train_subset$MSE_median_2 <- (train_subset$price - train_subset$val_tot_area_OIME)^2

sapply(train_subset, mean)


## 6.6. REGRESIONES ----

### PARTIR LAS BASES DE DATOS: TRAIN, EVAL, TEST.

### 6.5.1. Partición de la base de datos en tres----

#La base de datos Train se divide en tres particiones:
# Tr_train: Entrenar el modelo
# Tr_eval: Evaluar, ajustar y refinar el modelo
# Tr_test: Probar el modelo


# Revisar: Generamos las particiones
set.seed(100)
split1 <- createDataPartition(train_med$price, p = .7)[[1]]
length(split1) 

other <- train_med[-split1,]
Tr_train_med <- train_med[split1,]

split2 <- createDataPartition(other$price, p = 1/3)[[1]]

Tr_eval_med <- other[ split2,]
Tr_test_med <- other[-split2,]

nrow(Tr_train_med)
nrow(Tr_eval_med)
nrow(Tr_test_med)

nrow(Tr_train_med)+nrow(Tr_eval_med)+nrow(Tr_test_med)==nrow(train_med)

rm(other)


### Matriz de desempeño de los modelos:----

resumen_modelos <- data.frame(matrix(rep(0,75),nrow=15,ncol=5))
colnames(resumen_modelos) <- c("Modelo","Dinero_gastado","Prop_compradas","Precio_prom_compr","MSE_test")
sapply(resumen_modelos, typeof)



#DF con las predicciones. Mejor omitir esto porque el número de filas 
#puede variar dependiendo del modelo

# predicciones <- Tr_test[,c("property_id","price","val_tot_area_OIME","COD_CAT_US",)]
# predicciones <- filter(predicciones,!(is.na(predicciones$COD_CAT_US)))
# predicciones$COD_CAT_US <- NULL
# predicciones <- cbind(predicciones,pred_tree_df,pred_xgb_df)



#Función para calcular la decisión de compra.
#Entrada: decis_compra(x=valores_predichos,y=error)

decision_compra <- function(x,y) case_when(y > 0 ~ x,
                                        abs(y) < 40000000 ~ x,
                                        abs(y) > 40000000 ~ 0)



###Preparación del PC, cálculos en paralelo ----

n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

# Vamos a usar n_cores - 2 procesadores para esto
cl <- makePSOCKcluster(n_cores-2) 
registerDoParallel(cl)

##Ejecutar...

# Liberamos nuestros procesadores
stopCluster(cl)




### Modelos básicos ----

colSums(is.na(Tr_train_med))


reg1 <- lm(price ~ factor(ESTRATO),
           data=Tr_train_med)

reg2 <- lm(price ~ bedrooms + factor(ESTRATO), 
           data=Tr_train_med)

reg3 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO), 
           data=Tr_train_med)

reg4 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + surface_total, 
           data=Tr_train_med)

reg5 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + factor(NOMBRE.x) + surface_total, 
           data=Tr_train_med)


reg6 <- lm(price ~ bedrooms + ESTRATO + COD_CAT_US + COD_SUBCAT, 
           data=Tr_train_med)
 
reg7 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + factor(NOMBRE.x) + surface_total + factor(COD_SUBCAT), 
           data=Tr_train_med)

reg8 <- lm(price ~ val_tot_median + surface_total + factor(ESTRATO) + bedrooms + 
             factor(COD_CAT_US) + factor(COD_SUBCAT) + dist_metro + dist_hosp + dist_ccomerc + 
             dist_park,
           data=Tr_train_med)

reg9 <- lm(price ~val_tot_area_OIME + area_OIME_median + factor(ESTRATO) + bedrooms + 
             factor(COD_CAT_US) + factor(COD_SUBCAT) + dist_metro + dist_hosp + dist_ccomerc + 
             dist_park,
           data=Tr_train_med)

reg10 <- lm(price ~ val_tot_area_OIME + area_OIME_median + factor(ESTRATO) + bedrooms + bathrooms +
             factor(COD_CAT_US) + factor(COD_SUBCAT) + dist_metro + dist_hosp + dist_ccomerc + 
             dist_park,
           data=Tr_train_med)


mean(reg8$residuals^2)
mean(reg9$residuals^2)
mean(reg10$residuals^2)

stargazer(reg8,reg9,reg10,type="text")

stargazer(reg1,reg2,reg3,type="text")
stargazer(reg4,reg5,type="text")
stargazer(reg4,reg6,type="text")
stargazer(reg6,reg7,type="text")



########Pruebas con lagsarlm----


Tr_train_sp <- as(Tr_train, "Spatial")

Tr_train_neib <- dnearneigh(coordinates(Tr_train_sp), 0, 0.1, longlat = TRUE)

listw <- nb2listw(Tr_train_neib, style="W", zero.policy = TRUE)

# Prueba 1, saca error. Empty neighbour sets found.

reg1<-lagsarlm(modelo1,data=Tr_train, listw=listw)
reg2<-lagsarlm(modelo2,data=Tr_train, listw=listw)
reg3<-lagsarlm(modelo3,data=Tr_train, listw=listw)

stargazer(reg1,reg2,reg3,type="text")

# Prueba 2, Usando eigen valores. Funciona. --- probar después y revisar con manzanas porqu eno funciona

ev <- eigenw(listw)
W <- as(listw, "CsparseMatrix")
trMatc <- trW(W, type="mult")


reg1<-lagsarlm(modelo1,data=Tr_train, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))

reg2<-lagsarlm(modelo2,data=Tr_train, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))

reg3<-lagsarlm(modelo3,data=Tr_train, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))


stargazer(reg1,reg2,reg3,type="text")




#Definición del control (a usarse en los demás modelos)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

control <- trainControl(method = "cv", number = 5,
                        #summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)


#Árbol MED:

###Modelo árbol básico (CART) ----

train_med_fact <- Tr_train_med
train_med_fact$ESTRATO <- factor(train_med_fact$ESTRATO)
train_med_fact$COD_CAT_US <- factor(train_med_fact$COD_CAT_US)
train_med_fact$COD_SUBCAT <- factor(train_med_fact$COD_SUBCAT)

colnames(train_med_fact)

form_tree <- as.formula("price ~ 
                        bedrooms + 
                        ESTRATO + 
                        COD_CAT_US + 
                        COD_SUBCAT + 
                        dist_metro + 
                        dist_hosp + 
                        dist_ccomerc + 
                        dist_park + 
                        area_OIME_median +
                        val_tot_area_OIME")


#cp_alpha<-seq(from = 0, to = 0.1, length = 10)

#Ensayo de tree con un control de internet:

tree <- train(
  form_tree,
  data = Tr_train_med, #Debería ser la de las variables con Factores?
  method = "rpart",
  trControl = control,
  parms=list(split='Gini'),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  na.action  = na.pass,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree
rpart.plot::prp(tree$finalModel)
pred_tree <- predict(tree,Tr_test_med)
pred_tree_df <- data.frame(pred_tree)



###Modelo XGBoost ----

form_xgboost <- form_tree

grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                           subsample = c(0.6))

start_xg <- Sys.time()

xgboost <- train(
  form_xgboost,
  data = Tr_train_med,
  method = "xgbTree",
  trControl = control,
  na.action  = na.pass,
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)
xgboost

#Cálculo del índice desempeño del modelo:
pred_xgb <- predict(xgboost,Tr_test_med)
pred_xgb_df <- data.frame(pred_xgb)

#Identifico la variable que tenía NAs para poder luego filtrar observaciones:
nrow(Tr_test_med) - nrow(pred_xgb_df) #Dif. entre la base y el num de predicciones
colSums(is.na(Tr_test_med)) #Reviso cuál variable tenía la cantidad de NAs de la resta anterior.

#Le pego al DF con la predicción el precio real y la variable que tenía NAs para filtrarla:
pred_xgb_df <- cbind(filter(Tr_test_med[,c("property_id","price","COD_CAT_US")],
                            !(is.na(Tr_test_med$COD_CAT_US))), #Filtra obs de la var con NAs
                     pred_xgb_df)
pred_xgb_df$geometry <- NULL #Elimino geometría
pred_xgb_df$COD_CAT_US <- NULL #Elimino la variable que tenía NAs, aquí no la necesito

pred_xgb_df$error_xgb1 <- pred_xgb_df$pred_xgb -pred_xgb_df$price
pred_xgb_df$compra_xgb1 <- decision_compra(pred_xgb_df$pred_xgb,pred_xgb_df$error_xgb1)

resumen_modelos[1,1] <- "XGBoost 1"
resumen_modelos[1,2] <- sum(predicciones$compra_xgb1)
resumen_modelos[1,3] <- sum(predicciones$compra_xgb1>0)
resumen_modelos[1,4] <- resumen_modelos[1,2] / resumen_modelos[1,3]
resumen_modelos[1,5] <- sum(predicciones$error_xgb1^2)


end_xg <- Sys.time()
start_xg-end_xg


nrow(Tr_test_med)-nrow(pred_tree_df)
colSums(is.na(Tr_test_med))





#### ENSAYO 2

form_tree2 <- as.formula("price ~ 
                        bedrooms + 
                        ESTRATO + 
                        COD_CAT_US + 
                        COD_SUBCAT + 
                        dist_metro + 
                        dist_hosp + 
                        dist_ccomerc + 
                        dist_park + 
                        area_OIME_median")


#cp_alpha<-seq(from = 0, to = 0.1, length = 10)

#Ensayo de tree con un control de internet:

tree2 <- train(
  form_tree2,
  data = Tr_train_med,
  method = "rpart",
  trControl = control,
  parms=list(split='Gini'),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  na.action  = na.pass,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree2
rpart.plot::prp(tree2$finalModel)
pred_tree2 <- predict(tree2,Tr_test_med)
pred_tree_df2 <- data.frame(pred_tree2)



###Modelo XGBoost2 ----

form_xgboost2 <- form_tree2

grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

start_xg <- Sys.time()

xgboost2 <- train(
  form_xgboost2,
  data = Tr_train_med,
  method = "xgbTree",
  trControl = control,
  na.action  = na.pass,
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)


xgboost2
pred_xgb2 <- predict(xgboost2,Tr_test_med)
pred_xgb_df2 <- data.frame(pred_xgb2)

end_xg <- Sys.time()
start_xg-end_xg


predicciones <- cbind(predicciones,pred_tree_df2,pred_xgb_df2)
predicciones$geometry <- NULL

#export(predicciones,"./views/ensayo_predicc_med.xlsx")


#Cálculo del índice de los modelos ----

#Por ahora manual, luego se generaliza.
#HAY QUE TENER CUIDADO CON EL NÚMERO DE OBSERVACIONES, 
#DEPENDIENDO DE LAS VARS USADAS

predicciones_val <- predicciones

predicciones_val$property_id <- NULL
predicciones_val$description <- NULL


#Cálculo de la  matriz de diferencias
predicciones_difs <- data.frame(lapply(predicciones[c(colnames(predicciones_val))], 
                            function(x) x - predicciones_val$price))

predicciones_difs <- data.frame(lapply(predicciones_val, 
                                       function(x) x - predicciones_val$price))


#Cálculo del valor gastado si se da la compra:

calculo_gasto_compra <- function(x) case_when(x > 0 ~ x,
                                              abs(x) < 40000000 ~ x,
                                              abs(x) < 40000000 ~ 0)

predicciones_gasto <- data.frame(lapply(predicciones_difs, 
                                       function(x) calculo_gasto_compra))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


